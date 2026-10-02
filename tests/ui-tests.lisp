;;;; ui-tests.lisp -- the real window, driven without a click.
;;;;
;;;; The window is built and never shown.  Its controls are sent the messages a
;;;; click would send, and then the model is looked at; or the model is
;;;; changed, and then the table is asked what it shows.
;;;;
;;;; Skipped where there is no window server, which the count of skips says.

(in-package #:ftp-server/tests)

(def-suite ui :in all-tests :description "The window.  Needs a window server.")
(in-suite ui)

(defun call-with-controller (function)
  (with-temporary-directory (directory)
    (let ((saved (sb-posix:getenv "FTP_SERVER_SETTINGS"))
          (controller nil))
      ;; So that nothing here writes the settings of whoever runs the tests.
      (sb-posix:setenv "FTP_SERVER_SETTINGS" (path directory "settings.lisp") 1)
      (unwind-protect
           (objc:with-autorelease-pool ()
             ;; No policy, so no Dock icon and no stolen focus.
             (objc.runloop:shared-application :activation-policy nil)
             (setf controller (fs::make-window-controller (fs:make-model)))
             (setf fs::*main-thread-target* (objc:objc-object-pointer controller)
                   fs::*bonjour-changed* nil)
             (funcall function controller directory))
        (when controller
          (fs::bonjour-unpublish)
          (fs:model-stop (fs::controller-model controller))
          (objc:invoke (fs::controller-window controller) "close"))
        (setf fs::*main-thread-target* nil)
        (if saved
            (sb-posix:setenv "FTP_SERVER_SETTINGS" saved 1)
            (sb-posix:unsetenv "FTP_SERVER_SETTINGS"))))))

(defmacro with-controller ((controller &optional (directory (gensym "DIRECTORY")))
                           &body body)
  "Run BODY with CONTROLLER a window controller on an empty model, or skip."
  `(cond ((not (ignore-errors (fs::ensure-frameworks) t))
          (skip "Objective-C runtime not available"))
         ((not (objc.runloop:window-server-p))
          (skip "no window server"))
         (t (call-with-controller (lambda (,controller ,directory)
                                    (declare (ignorable ,directory))
                                    ,@body)))))

(defun table (controller) (fs::controller-table controller))
(defun target (controller) (objc:objc-object-pointer controller))

(defun column (controller key)
  (objc:invoke (table controller) "tableColumnWithIdentifier:" key))

(defun cell-string (controller key row)
  "What the data source answers for the cell, as a string."
  (objc:invoke-into 'string (target controller)
                    "tableView:objectValueForTableColumn:row:"
                    (table controller) (column controller key) row))

(defun cell-flag (controller key row)
  (objc:invoke-bool (objc:invoke (target controller)
                                 "tableView:objectValueForTableColumn:row:"
                                 (table controller) (column controller key) row)
                    "boolValue"))

(defun set-cell (controller key row value)
  (objc:invoke (target controller) "tableView:setObjectValue:forTableColumn:row:"
               (table controller) value (column controller key) row))

(defun status (controller)
  (objc:invoke-into 'string (fs::controller-status-label controller) "stringValue"))

(defun title (button)
  (objc:invoke-into 'string button "title"))

(defun set-field (field string)
  (objc:invoke field "setStringValue:" string))

(defun free-port ()
  (let ((probe (fs::listen-on #(127 0 0 1) 0)))
    (prog1 (fs::socket-port probe)
      (sb-bsd-sockets:socket-close probe))))

(defun fill-in (controller &key (port (free-port)))
  (set-field (fs::controller-username-field controller) "ann")
  (set-field (fs::controller-password-field controller) "pw")
  (set-field (fs::controller-port-field controller) (format nil "~d" port))
  port)

(test the-window-has-a-table-with-three-columns
  (with-controller (controller)
    (is (= 3 (objc:invoke (table controller) "numberOfColumns")))
    (is (= 0 (objc:invoke (table controller) "numberOfRows")))
    (is (string= "Start" (title (fs::controller-start-button controller))))
    (is (string= "Stopped." (status controller)))
    (is-false (objc:invoke-bool (fs::controller-remove-button controller) "isEnabled"))))

(test an-added-directory-is-a-row
  (with-controller (controller)
    (is (= 0 (fs::controller-add-directory controller "/tmp")))
    (is (= 1 (objc:invoke (table controller) "numberOfRows")))
    (is (string= "tmp" (cell-string controller "name" 0)))
    (is (string= "/tmp" (cell-string controller "path" 0)))
    (is-false (cell-flag controller "writable" 0))
    ;; Something that is not a directory is not added, and the window says so.
    (is (null (fs::controller-add-directory controller "/etc/hosts")))
    (is (= 1 (objc:invoke (table controller) "numberOfRows")))
    (is (search "not a folder" (status controller)))))

(test editing-the-name-cell-renames-the-mapping
  (with-controller (controller)
    (fs::controller-add-directory controller "/tmp")
    (fs::controller-add-directory controller "/var")
    (set-cell controller "name" 0 "tempdir")
    (is (equal '("tempdir" "var") (mapping-names (fs::controller-model controller))))
    (is (string= "tempdir" (cell-string controller "name" 0)))
    ;; A name already taken is refused, in words, and nothing changes.
    (set-cell controller "name" 0 "var")
    (is (equal '("tempdir" "var") (mapping-names (fs::controller-model controller))))
    (is (search "already" (status controller)))))

(test the-checkbox-makes-a-mapping-writable
  (with-controller (controller)
    (fs::controller-add-directory controller "/tmp")
    (set-cell controller "writable" 0 (objc:invoke "NSNumber" "numberWithBool:" t))
    (is-true (fs:mapping-writable (first (fs:vfs-mappings
                                          (fs:model-vfs (fs::controller-model controller))))))
    (is-true (cell-flag controller "writable" 0))
    (set-cell controller "writable" 0 (objc:invoke "NSNumber" "numberWithBool:" nil))
    (is-false (cell-flag controller "writable" 0))))

(test remove-takes-away-the-selected-row
  (with-controller (controller)
    (fs::controller-add-directory controller "/tmp")
    (fs::controller-add-directory controller "/var")
    ;; Nothing selected, nothing removed.
    (objc:invoke (target controller) "removeMapping:" (cffi:null-pointer))
    (is (= 2 (objc:invoke (table controller) "numberOfRows")))
    (fs::select-row controller 0)
    (is-true (objc:invoke-bool (fs::controller-remove-button controller) "isEnabled"))
    (objc:invoke (fs::controller-remove-button controller) "performClick:" (cffi:null-pointer))
    (is (equal '("var") (mapping-names (fs::controller-model controller))))
    (is (= 1 (objc:invoke (table controller) "numberOfRows")))))

(test changes-are-saved-as-they-are-made
  (with-controller (controller directory)
    (fs::controller-add-directory controller "/tmp")
    (set-cell controller "name" 0 "tempdir")
    (let ((settings (fs:load-settings
                     (sb-ext:parse-native-namestring (path directory "settings.lisp")))))
      (is (equal '((:name "tempdir" :path "/tmp" :writable nil))
                 (getf settings :mappings))))))

(test start-wants-credentials-and-a-port
  (with-controller (controller)
    (objc:invoke (target controller) "toggleServer:" (cffi:null-pointer))
    (is-false (fs:model-running-p (fs::controller-model controller)))
    (is (search "password" (status controller)))
    (fill-in controller)
    (set-field (fs::controller-port-field controller) "ftp")
    (objc:invoke (target controller) "toggleServer:" (cffi:null-pointer))
    (is-false (fs:model-running-p (fs::controller-model controller)))
    (is (search "port" (status controller)))
    (is (string= "Start" (title (fs::controller-start-button controller))))))

(test the-button-starts-and-stops-a-server-that-serves-the-table
  (with-controller (controller)
    (let ((port (fill-in controller))
          (button (fs::controller-start-button controller)))
      (fs::controller-add-directory controller "/tmp")
      (set-cell controller "name" 0 "tempdir")
      (objc:invoke button "performClick:" (cffi:null-pointer))
      (is-true (fs:model-running-p (fs::controller-model controller)))
      (is (string= "Stop" (title button)))
      (is (search (format nil "port ~d" port) (status controller)))
      (is-false (objc:invoke-bool (fs::controller-port-field controller) "isEnabled"))
      ;; A loopback server is not announced.
      (is (null fs::*bonjour-status*))
      (with-client (client port)
        (is (= 230 (login client "ann" "pw")))
        (is (equal '("tempdir") (lines-of (nth-value 1 (fetch client "NLST")))))
        ;; The client's arrival reaches the status line from a server thread.
        (is-true (wait-until
                  (lambda ()
                    (objc.runloop:pump-run-loop :seconds 0.01d0)
                    (search "1 client " (status controller))))))
      (objc:invoke button "performClick:" (cffi:null-pointer))
      (is-false (fs:model-running-p (fs::controller-model controller)))
      (is (string= "Start" (title button)))
      (is (string= "Stopped." (status controller)))
      (is-true (objc:invoke-bool (fs::controller-port-field controller) "isEnabled")))))

(test a-server-other-computers-can-reach-is-announced-with-bonjour
  (with-controller (controller)
    (fill-in controller)
    (set-field (fs::controller-bonjour-field controller) "FTP Server Test")
    (objc:invoke (fs::controller-remote-checkbox controller) "setState:" 1)
    (is-true (fs::controller-start controller))
    (is-true (fs:model-allow-remote (fs::controller-model controller)))
    (is (not (null fs::*bonjour-status*)))
    (is-true (wait-until (lambda ()
                           (objc.runloop:pump-run-loop :seconds 0.05d0)
                           (consp fs::*bonjour-status*))
                         10))
    (is (eq :published (first fs::*bonjour-status*)))
    (is (search "FTP Server Test" (second fs::*bonjour-status*)))
    (fs::controller-stop controller)
    (is (null fs::*bonjour-status*))
    (is (null fs::*bonjour-service*))))
