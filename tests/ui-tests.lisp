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

(defun activity-rows (controller)
  "What the activity pane shows, asked of its data source: a list of rows,
each a list of the time, the user, the address and the message."
  (let ((table (fs::controller-activity-table controller)))
    (loop for row below (objc:invoke table "numberOfRows")
          collect (loop for key in fs::*activity-columns*
                        collect (objc:invoke-into
                                 'string (target controller)
                                 "tableView:objectValueForTableColumn:row:"
                                 table (objc:invoke table "tableColumnWithIdentifier:" key)
                                 row)))))

(defun activity-messages (controller)
  (mapcar #'fourth (activity-rows controller)))

(defun activity-text (controller)
  "The rows without their times, one to a line, as user, address and message."
  (format nil "~:{~*~a ~a ~a~%~}" (activity-rows controller)))

(defun sort-activity-by (controller key ascending)
  "What a click on a header does: change the table's sort descriptors."
  (objc:invoke (fs::controller-activity-table controller) "setSortDescriptors:"
               (vector (objc:invoke "NSSortDescriptor" "sortDescriptorWithKey:ascending:"
                                    key ascending))))

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
                    (search "1 client " (status controller)))))
        ;; And what it did reaches the activity pane, by the same road.
        (is-true (wait-until
                  (lambda ()
                    (objc.runloop:pump-run-loop :seconds 0.01d0)
                    (search "listed /" (activity-text controller)))))
        (is (search "ann 127.0.0.1 logged in" (activity-text controller)))
        ;; Before it logged in it had no name.
        (is (search "anon 127.0.0.1 connected" (activity-text controller)))
        (is (search "- - server started on port" (activity-text controller)))
        (is (null (search "pw" (activity-text controller)))))
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
    ;; A machine may refuse to announce anything -- no network, or a policy
    ;; against it -- and that is the machine's answer, not a fault here.
    (if (and (consp fs::*bonjour-status*) (eq :failed (first fs::*bonjour-status*)))
        (skip "this machine would not publish a Bonjour service (error ~d)"
              (second fs::*bonjour-status*))
        (progn
          (is (eq :published (first fs::*bonjour-status*)))
          (is (search "FTP Server Test" (second fs::*bonjour-status*)))))
    (fs::controller-stop controller)
    (is (null fs::*bonjour-status*))
    (is (null fs::*bonjour-service*))))

(test the-activity-pane-keeps-only-so-many-rows
  (with-controller (controller)
    (let ((limit fs::*activity-limit*))
      (setf fs::*activity-limit* 5)
      (unwind-protect
           (progn
             (dotimes (index 12)
               (fs::controller-add-activity controller "ann" #(127 0 0 1)
                                            (format nil "did thing ~d" index)))
             (is (equal '("did thing 7" "did thing 8" "did thing 9"
                          "did thing 10" "did thing 11")
                        (activity-messages controller)))
             ;; The oldest go, not whichever are last in the order on show.
             (sort-activity-by controller "time" nil)
             (fs::controller-add-activity controller "ann" #(127 0 0 1) "did thing 12")
             (is (equal '("did thing 12" "did thing 11" "did thing 10"
                          "did thing 9" "did thing 8")
                        (activity-messages controller))))
        (setf fs::*activity-limit* limit)))))

(test the-activity-pane-has-four-columns-with-headers
  (with-controller (controller)
    (let* ((table (fs::controller-activity-table controller))
           (columns (objc:invoke table "tableColumns")))
      (is (equal '("Time" "User" "IP Address" "Message")
                 (loop for index below (objc:invoke columns "count")
                       collect (objc:invoke-into 'string
                                                 (objc:invoke columns "objectAtIndex:" index)
                                                 "title"))))
      (is-false (cffi:null-pointer-p (objc:invoke table "headerView")))
      ;; Every header can be clicked to sort.
      (is (loop for index below (objc:invoke columns "count")
                never (cffi:null-pointer-p
                       (objc:invoke (objc:invoke columns "objectAtIndex:" index)
                                    "sortDescriptorPrototype")))))))

(test a-row-says-when-who-from-where-and-what
  (with-controller (controller)
    (fs::controller-add-activity controller "ann" #(192 168 1 20) "logged in")
    (fs::controller-add-activity controller nil #(192 168 1 21) "connected")
    (fs::controller-add-activity controller nil nil "server stopped")
    (destructuring-bind (first second third) (activity-rows controller)
      (is (= 19 (length (first first))) "a date and a time")
      (is (char= #\- (char (first first) 4)))
      (is (char= #\: (char (first first) 13)))
      (is (equal '("ann" "192.168.1.20" "logged in") (rest first)))
      (is (equal '("anon" "192.168.1.21" "connected") (rest second)))
      (is (equal '("-" "-" "server stopped") (rest third))))))

(test clicking-a-header-sorts-the-activity-pane
  (with-controller (controller)
    (fs::controller-add-activity controller "carol" #(10 0 0 10) "uploaded /a")
    (fs::controller-add-activity controller "ann" #(10 0 0 9) "listed /")
    (fs::controller-add-activity controller nil #(10 0 0 200) "connected")
    (fs::controller-add-activity controller "bob" fs::*loopback6* "downloaded /b")
    ;; As they happened, to begin with.
    (is (equal '("uploaded /a" "listed /" "connected" "downloaded /b")
               (activity-messages controller)))
    (flet ((users () (mapcar #'second (activity-rows controller)))
           (addresses () (mapcar #'third (activity-rows controller))))
      (sort-activity-by controller "user" t)
      (is (equal '("ann" "anon" "bob" "carol") (users)))
      (sort-activity-by controller "user" nil)
      (is (equal '("carol" "bob" "anon" "ann") (users)))
      ;; By number, not by spelling: 9 comes before 10.  IPv6 after IPv4.
      (sort-activity-by controller "address" t)
      (is (equal '("10.0.0.9" "10.0.0.10" "10.0.0.200" "::1") (addresses)))
      (sort-activity-by controller "message" t)
      (is (equal '("connected" "downloaded /b" "listed /" "uploaded /a")
                 (activity-messages controller)))
      (sort-activity-by controller "time" nil)
      (is (equal '("downloaded /b" "connected" "listed /" "uploaded /a")
                 (activity-messages controller)))
      ;; A row that arrives is put where the order says it goes.
      (sort-activity-by controller "user" t)
      (fs::controller-add-activity controller "ben" #(10 0 0 1) "logged in")
      (is (equal '("ann" "anon" "ben" "bob" "carol") (users))))))

(test starting-at-launch-is-a-setting-like-the-others
  (with-controller (controller directory)
    (is (= 0 (objc:invoke (fs::controller-launch-checkbox controller) "state")))
    (objc:invoke (fs::controller-launch-checkbox controller) "performClick:" (cffi:null-pointer))
    (is-true (fs:model-start-at-launch (fs::controller-model controller)))
    (is (eq t (getf (fs:load-settings
                     (sb-ext:parse-native-namestring (path directory "settings.lisp")))
                    :start-at-launch)))))

;;; The keychain ---------------------------------------------------------------------
;;;
;;; Against the real login keychain, under a service name of this test's own,
;;; which it removes.  The item is made and read by the same program, so
;;; nothing asks the user anything.  Skipped where there is no keychain to use.

(defun keychain-usable-p (service)
  (handler-case (progn (fs::keychain-set service "probe" "probe")
                       (fs::keychain-delete service "probe")
                       t)
    (error () nil)))

(defmacro with-keychain ((service) &body body)
  `(let ((,service (format nil "org.lispnik.ftp-server.test.~d.~d"
                           (sb-posix:getpid) (random 1000000))))
     (cond ((not (ignore-errors (fs::ensure-frameworks) t))
            (skip "Objective-C runtime not available"))
           ((not (keychain-usable-p ,service))
            (skip "no keychain that can be written to"))
           (t (unwind-protect (progn ,@body)
                (ignore-errors (fs::keychain-delete ,service "FTP login")))))))

(test the-keychain-keeps-replaces-and-forgets-a-password
  (with-keychain (service)
    (is (null (fs::keychain-get service "FTP login")))
    (is-true (fs::keychain-set service "FTP login" "first"))
    (is (string= "first" (fs::keychain-get service "FTP login")))
    (is-true (fs::keychain-set service "FTP login" "sécond “pass” 密码"))
    (is (string= "sécond “pass” 密码" (fs::keychain-get service "FTP login")))
    (is-true (fs::keychain-delete service "FTP login"))
    (is-false (fs::keychain-delete service "FTP login"))
    (is (null (fs::keychain-get service "FTP login")))))

(test the-model-saves-its-password-to-the-keychain
  (with-keychain (service)
    (with-temporary-directory (directory)
      (with-password-store ((fs::keychain-password-store service))
        (let ((model (fs:make-model)))
          (setf (fs:model-username model) "ann"
                (fs:model-password model) "hunter2")
          (is-true (fs:model-save model (settings-in directory)))
          (is (null (search "hunter2" (read-file (path directory "settings.lisp")))))
          (is (string= "hunter2" (fs::keychain-get service "FTP login")))))
      ;; A new store, as a new launch would have.
      (with-password-store ((fs::keychain-password-store service))
        (let ((loaded (fs:model-load (settings-in directory))))
          (is (string= "hunter2" (fs:model-password loaded)))
          ;; An empty password removes the item rather than keeping an empty one.
          (setf (fs:model-password loaded) "")
          (is-true (fs:model-save loaded (settings-in directory)))
          (is (null (fs::keychain-get service "FTP login"))))))))

(test requiring-tls-is-a-setting-like-the-others
  (with-controller (controller directory)
    (is (= 0 (objc:invoke (fs::controller-tls-checkbox controller) "state")))
    (objc:invoke (fs::controller-tls-checkbox controller) "performClick:" (cffi:null-pointer))
    (is-true (fs:model-require-tls (fs::controller-model controller)))
    (is (eq t (getf (fs:load-settings
                     (sb-ext:parse-native-namestring (path directory "settings.lisp")))
                    :require-tls)))
    ;; With no TLS to be had, as here, a server that requires it will not start.
    (fill-in controller)
    (let ((fs:*tls-maker* nil))
      (is-false (fs::controller-start controller))
      (is (search "TLS is required" (status controller))))))

;;; Copy, Clear, tooltips, and the certificate -----------------------------------------

(defun select-activity-rows (controller &rest rows)
  (let ((indexes (objc:invoke "NSMutableIndexSet" "indexSet")))
    (dolist (row rows)
      (objc:invoke indexes "addIndex:" row))
    (objc:invoke (fs::controller-activity-table controller)
                 "selectRowIndexes:byExtendingSelection:" indexes nil)))

(defun copied-messages (controller)
  "The messages in what Copy would put on the clipboard."
  (mapcar (lambda (line) (fourth (uiop:split-string line :separator '(#\Tab))))
          (remove "" (uiop:split-string (fs::controller-activity-text controller)
                                        :separator '(#\Newline))
                  :test #'string=)))

(test copy-takes-the-selected-rows-or-all-of-them
  (with-controller (controller)
    (fs::controller-add-activity controller "ann" #(10 0 0 1) "one")
    (fs::controller-add-activity controller "bob" #(10 0 0 2) "two")
    (fs::controller-add-activity controller "cat" #(10 0 0 3) "three")
    ;; Nothing selected: everything, in the order on show.
    (is (equal '("one" "two" "three") (copied-messages controller)))
    (sort-activity-by controller "time" nil)
    (is (equal '("three" "two" "one") (copied-messages controller)))
    ;; Some selected: those, as the table has them.
    (select-activity-rows controller 0 2)
    (is (equal '("three" "one") (copied-messages controller)))
    (is (search (format nil "ann~c10.0.0.1~cone" #\Tab #\Tab)
                (fs::controller-activity-text controller)))))

(test copy-puts-the-rows-on-the-clipboard
  ;; Asked for, not run by default: it overwrites the clipboard of whoever
  ;; runs the tests.
  (if (not (sb-posix:getenv "FTP_SERVER_TEST_CLIPBOARD"))
      (skip "set FTP_SERVER_TEST_CLIPBOARD to let this overwrite the clipboard")
      (with-controller (controller)
        (fs::controller-add-activity controller "ann" #(10 0 0 1) "copied row")
        (objc:invoke (target controller) "copyActivity:" (cffi:null-pointer))
        (is (search "copied row"
                    (objc:invoke-into 'string (objc:invoke "NSPasteboard" "generalPasteboard")
                                      "stringForType:" "public.utf8-plain-text"))))))

(test clear-empties-the-activity-pane
  (with-controller (controller)
    (fs::controller-add-activity controller "ann" #(10 0 0 1) "one")
    (fs::controller-add-activity controller "ann" #(10 0 0 1) "two")
    (objc:invoke (target controller) "clearActivity:" (cffi:null-pointer))
    (is (= 0 (objc:invoke (fs::controller-activity-table controller) "numberOfRows")))
    (is (null (activity-rows controller)))
    ;; And it goes on working afterwards.
    (fs::controller-add-activity controller "ann" #(10 0 0 1) "three")
    (is (equal '("three") (activity-messages controller)))))

(test a-cell-of-the-activity-pane-has-its-whole-text-as-a-tooltip
  (with-controller (controller)
    (let ((long "could not upload /a/very/long/path/that/will/not/fit/in/the/column.txt: This folder is read-only")
          (table (fs::controller-activity-table controller)))
      (fs::controller-add-activity controller "ann" #(10 0 0 1) long)
      (cffi:with-foreign-object (rect :double 4)
        (flet ((tooltip (which column row)
                 (let ((answer (objc:invoke
                                (target controller)
                                "tableView:toolTipForCell:rect:tableColumn:row:mouseLocation:"
                                which (cffi:null-pointer) rect
                                (objc:invoke which "tableColumnWithIdentifier:" column)
                                row #(0d0 0d0))))
                   (and (not (cffi:null-pointer-p answer))
                        (objc:ns-string-to-string answer)))))
          (is (string= long (tooltip table "message" 0)))
          (is (string= "ann" (tooltip table "user" 0)))
          (is (null (tooltip table "message" 5)) "no such row")
          ;; The table of folders has no tooltips of this kind.
          (fs::controller-add-directory controller "/tmp")
          (is (null (tooltip (fs::controller-table controller) "name" 0))))))))

(defun certificate-shown (controller)
  (objc:invoke-into 'string (fs::controller-certificate-label controller) "stringValue"))

(test the-window-shows-the-certificate-and-can-replace-it
  (with-controller (controller directory)
    (is (search "None yet" (certificate-shown controller)))
    (is-true (objc:invoke-bool (fs::controller-certificate-button controller) "isEnabled"))
    (is-true (fs::controller-new-certificate controller))
    (let ((first (certificate-shown controller)))
      ;; The fingerprint, on two lines.
      (is (= 95 (length first)))
      (is (= 1 (count #\Newline first)))
      (is (string= (fs::current-certificate-fingerprint)
                   (substitute #\: #\Newline first)))
      (is-true (probe-file (sb-ext:parse-native-namestring (path directory "certificate.pem"))))
      (is (search "new TLS certificate" (first (last (activity-messages controller)))))
      (is-true (fs::controller-new-certificate controller))
      (is (string/= first (certificate-shown controller))))
    ;; Not while the server is running on the one it has.
    (fill-in controller)
    (is-true (fs::controller-start controller))
    (is-false (objc:invoke-bool (fs::controller-certificate-button controller) "isEnabled"))
    (let ((shown (certificate-shown controller)))
      (objc:invoke (target controller) "newCertificate:" (cffi:null-pointer))
      (is (string= shown (certificate-shown controller))))))

(test a-lisp-mapping-shows-in-the-table-as-what-made-it
  (with-controller (controller)
    (fs::controller-add-directory controller "/tmp")
    (fs:vfs-add-lisp (fs:model-vfs (fs::controller-model controller)) "gen"
                     (fs:lisp-directory "gen" '()) :description "(made by init.lisp)")
    (objc:invoke (table controller) "reloadData")
    (is (= 2 (objc:invoke (table controller) "numberOfRows")))
    (is (string= "gen" (cell-string controller "name" 1)))
    (is (string= "(made by init.lisp)" (cell-string controller "path" 1)))
    (is (string= "/tmp" (cell-string controller "path" 0)))))
