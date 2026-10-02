;;;; model-tests.lisp -- what the window does, without the window.

(in-package #:ftp-server/tests)

(def-suite model :in all-tests :description "The model behind the window.")
(in-suite model)

(defun mapping-names (model)
  (mapcar #'fs:mapping-name (fs:vfs-mappings (fs:model-vfs model))))

(test a-directory-is-named-after-its-last-component
  (is (string= "tmp" (fs::default-mapping-name "/tmp")))
  (is (string= "tmp" (fs::default-mapping-name "/tmp/")))
  (is (string= "My Files" (fs::default-mapping-name "/Users/ann/My Files")))
  (is (string= "root" (fs::default-mapping-name "/"))))

(test adding-the-same-directory-twice-gives-two-names
  (let ((model (fs:make-model)))
    (is (string= "tmp" (fs:mapping-name (fs:model-add-directory model "/tmp"))))
    (is (string= "tmp-2" (fs:mapping-name (fs:model-add-directory model "/tmp"))))
    (is (string= "tmp-3" (fs:mapping-name (fs:model-add-directory model "/tmp/"))))
    (is (equal '("tmp" "tmp-2" "tmp-3") (mapping-names model)))
    ;; Read-only until someone says otherwise.
    (is (notany #'fs:mapping-writable (fs:vfs-mappings (fs:model-vfs model))))))

(test only-a-directory-can-be-added
  (let ((model (fs:make-model)))
    (multiple-value-bind (mapping message) (fs:model-add-directory model "/etc/hosts")
      (is (null mapping))
      (is (stringp message)))
    (is (null (fs:model-add-directory model "/nonexistent/ftp-server-test")))
    (is (null (mapping-names model)))))

(test the-example-from-the-brief
  ;; Add /tmp, call it tempdir.
  (let ((model (fs:make-model)))
    (fs:model-add-directory model "/tmp")
    (is (equal '(t nil) (multiple-value-list (fs:model-rename-mapping model 0 "tempdir"))))
    (is (equal '("tempdir") (mapping-names model)))
    (is (string= "/tmp" (fs:mapping-host-path (first (fs:vfs-mappings (fs:model-vfs model))))))))

(test a-bad-rename-says-why-and-changes-nothing
  (let ((model (fs:make-model)))
    (fs:model-add-directory model "/tmp")
    (fs:model-add-directory model "/var")
    (multiple-value-bind (ok message) (fs:model-rename-mapping model 0 "var")
      (is (null ok))
      (is (search "already" message)))
    (is (null (fs:model-rename-mapping model 0 "a/b")))
    (is (null (fs:model-rename-mapping model 0 "   ")))
    (is (null (fs:model-rename-mapping model 7 "x")))
    (is (equal '("tmp" "var") (mapping-names model)))
    ;; Spaces round a name are dropped rather than refused.
    (is-true (fs:model-rename-mapping model 0 "  temp  "))
    (is (equal '("temp" "var") (mapping-names model)))))

(test rows-are-removed-and-made-writable-by-index
  (let ((model (fs:make-model)))
    (fs:model-add-directory model "/tmp")
    (fs:model-add-directory model "/var")
    (is-true (fs:model-set-writable model 1 t))
    (is-false (fs:model-set-writable model 5 t))
    (is (equal '(nil t) (mapcar #'fs:mapping-writable
                                (fs:vfs-mappings (fs:model-vfs model)))))
    (is-true (fs:model-remove-mapping model 0))
    (is-false (fs:model-remove-mapping model 3))
    (is-false (fs:model-remove-mapping model -1))
    (is (equal '("var") (mapping-names model)))))

(test a-port-is-a-number-in-range
  (is (= 2121 (fs:parse-port "2121")))
  (is (= 21 (fs:parse-port " 21 ")))
  (is (null (fs:parse-port "0")))
  (is (null (fs:parse-port "65536")))
  (is (null (fs:parse-port "ftp")))
  (is (null (fs:parse-port "")))
  (is (null (fs:parse-port "21.5"))))

(test the-server-will-not-start-without-credentials
  (let ((model (fs:make-model)))
    (setf (fs:model-port model) 0)
    (multiple-value-bind (ok message) (fs:model-start model)
      (is (null ok))
      (is (search "password" message)))
    (setf (fs:model-username model) "ann")
    (is (null (fs:model-start model)))
    (is-false (fs:model-running-p model))
    (is (string= "Stopped." (fs:model-status-text model)))))

(test the-model-starts-and-stops-a-server-that-knows-its-user
  (let ((model (fs:make-model))
        (delay fs::*login-failure-delay*))
    (setf (fs:model-username model) "ann"
          (fs:model-password model) "pw"
          ;; Any free port; MODEL-START's own check wants 1 or more.
          (fs:model-port model) (let ((probe (fs::listen-on #(127 0 0 1) 0)))
                                  (prog1 (fs::socket-port probe)
                                    (sb-bsd-sockets:socket-close probe)))
          fs::*login-failure-delay* 0)
    (unwind-protect
         (progn
           (is (equal '(t nil) (multiple-value-list (fs:model-start model))))
           (is-true (fs:model-running-p model))
           (is (search "this computer only" (fs:model-status-text model)))
           (is (search "No clients" (fs:model-status-text model)))
           (multiple-value-bind (ok message) (fs:model-start model)
             (is (null ok))
             (is (search "already" message)))
           (with-client (client (fs:model-port model))
             (is (= 530 (login client "ann" "wrong")))
             (is (= 530 (login client "bob" "pw")))
             (is (= 230 (login client "ann" "pw")))
             (is (search "1 client " (fs:model-status-text model))))
           ;; A second server on the same port is refused in words.
           (let ((other (fs:make-model)))
             (setf (fs:model-username other) "a"
                   (fs:model-password other) "b"
                   (fs:model-port other) (fs:model-port model))
             (multiple-value-bind (ok message) (fs:model-start other)
               (is (null ok))
               (is (search "in use" message)))))
      (fs:model-stop model)
      (setf fs::*login-failure-delay* delay))
    (is-false (fs:model-running-p model))
    (is (string= "Stopped." (fs:model-status-text model)))))

(test bonjour-is-for-servers-other-computers-can-reach
  (let ((model (fs:make-model)))
    (is-false (fs:model-advertise-p model))
    (setf (fs:model-allow-remote model) t)
    (is-true (fs:model-advertise-p model))))

(test the-model-goes-to-settings-and-back
  (with-temporary-directory (directory)
    (let ((file (sb-ext:parse-native-namestring (path directory "settings.lisp")))
          (model (fs:make-model)))
      (setf (fs:model-username model) "ann"
            (fs:model-password model) "pw"
            (fs:model-port model) 2200
            (fs:model-allow-remote model) t
            (fs:model-bonjour-name model) "Files")
      (fs:model-add-directory model "/tmp")
      (fs:model-rename-mapping model 0 "tempdir")
      (fs:model-set-writable model 0 t)
      (fs:model-save model file)
      (let ((loaded (fs:make-model (fs:load-settings file))))
        (is (string= "ann" (fs:model-username loaded)))
        (is (string= "pw" (fs:model-password loaded)))
        (is (= 2200 (fs:model-port loaded)))
        (is-true (fs:model-allow-remote loaded))
        (is (string= "Files" (fs:model-bonjour-name loaded)))
        (is (equal '("tempdir") (mapping-names loaded)))
        (let ((mapping (first (fs:vfs-mappings (fs:model-vfs loaded)))))
          (is (string= "/tmp" (fs:mapping-host-path mapping)))
          (is-true (fs:mapping-writable mapping)))))))

(test settings-with-a-clashing-mapping-keep-the-first
  (let ((model (fs:make-model (list :version 1 :username "" :password "" :port 2121
                                    :allow-remote nil :bonjour-name ""
                                    :mappings '((:name "a" :path "/tmp" :writable nil)
                                                (:name "A" :path "/var" :writable nil)
                                                (:name "b/c" :path "/var" :writable nil))))))
    (is (equal '("a") (mapping-names model)))))

;;; A password kept somewhere else ----------------------------------------------------

(defun memory-store (&key (password "") fail)
  "A password store that keeps its password in a variable: (values STORE GET),
GET a function answering what it now holds.  With FAIL it refuses to store."
  (let ((kept password))
    (values (fs:make-password-store
             :fetch (lambda () kept)
             :store (lambda (password)
                      (when fail (error "refused"))
                      (setf kept password)))
            (lambda () kept))))

(defmacro with-password-store ((store) &body body)
  `(let ((fs:*password-store* ,store))
     ,@body))

(defun settings-in (directory)
  (sb-ext:parse-native-namestring (path directory "settings.lisp")))

(test with-a-store-the-password-is-not-in-the-file
  (with-temporary-directory (directory)
    (multiple-value-bind (store kept) (memory-store)
      (with-password-store (store)
        (let ((model (fs:make-model)))
          (setf (fs:model-username model) "ann"
                (fs:model-password model) "hunter2")
          (is-true (fs:model-save model (settings-in directory)))
          (is (string= "hunter2" (funcall kept)))
          (is (null (search "hunter2" (read-file (path directory "settings.lisp")))))
          (let ((loaded (fs:model-load (settings-in directory))))
            (is (string= "ann" (fs:model-username loaded)))
            (is (string= "hunter2" (fs:model-password loaded)))))))))

(test a-password-left-in-the-file-is-moved-to-the-store
  (with-temporary-directory (directory)
    ;; Written by a version with no store.
    (let ((model (fs:make-model)))
      (setf (fs:model-username model) "ann"
            (fs:model-password model) "old-secret")
      (fs:model-save model (settings-in directory)))
    (is (search "old-secret" (read-file (path directory "settings.lisp"))))
    (multiple-value-bind (store kept) (memory-store)
      (with-password-store (store)
        (let ((loaded (fs:model-load (settings-in directory))))
          (is (string= "old-secret" (fs:model-password loaded)))
          (is (string= "old-secret" (funcall kept)))
          (is (null (search "old-secret" (read-file (path directory "settings.lisp"))))
              "and it is gone from the file"))))))

(test a-store-that-refuses-does-not-put-the-password-in-the-file
  (with-temporary-directory (directory)
    (with-password-store ((memory-store :fail t))
      (let ((model (fs:make-model)))
        (setf (fs:model-username model) "ann"
              (fs:model-password model) "hunter2")
        (is-false (fs:model-save model (settings-in directory)))
        (let ((text (read-file (path directory "settings.lisp"))))
          (is (null (search "hunter2" text)))
          (is (search "ann" text) "the rest is saved all the same"))))))

(test without-a-store-the-file-has-the-password
  (with-temporary-directory (directory)
    (with-password-store (nil)
      (let ((model (fs:make-model)))
        (setf (fs:model-password model) "hunter2")
        (is-true (fs:model-save model (settings-in directory)))
        (is (string= "hunter2"
                     (fs:model-password (fs:model-load (settings-in directory)))))))))

;;; The activity log -----------------------------------------------------------------

(defun entry (sequence user address message &optional (time 0))
  (fs::make-activity-entry :sequence sequence :time time :user user
                           :address address :message message))

(test an-activity-row-names-nobody-as-anon-and-the-server-as-a-dash
  (is (string= "ann" (fs::activity-cell (entry 1 "ann" #(10 0 0 1) "x") "user")))
  (is (string= "anon" (fs::activity-cell (entry 1 nil #(10 0 0 1) "x") "user")))
  (is (string= "anon" (fs::activity-cell (entry 1 "" #(10 0 0 1) "x") "user")))
  (is (string= "-" (fs::activity-cell (entry 1 nil nil "x") "user")))
  (is (string= "10.0.0.1" (fs::activity-cell (entry 1 nil #(10 0 0 1) "x") "address")))
  (is (string= "-" (fs::activity-cell (entry 1 nil nil "x") "address")))
  (is (string= "x" (fs::activity-cell (entry 1 nil nil "x") "message")))
  (is (= 19 (length (fs::activity-cell (entry 1 nil nil "x" (get-universal-time)) "time")))))

(test addresses-sort-by-number
  (is-true (fs::address-before-p #(10 0 0 9) #(10 0 0 10)))
  (is-false (fs::address-before-p #(10 0 0 10) #(10 0 0 9)))
  (is-false (fs::address-before-p #(10 0 0 9) #(10 0 0 9)))
  (is-true (fs::address-before-p nil #(10 0 0 9)))
  (is-false (fs::address-before-p #(10 0 0 9) nil))
  (is-false (fs::address-before-p nil nil))
  (is-true (fs::address-before-p #(255 255 255 255) fs::*loopback6*)))

(test rows-alike-in-the-sorted-column-stay-in-the-order-they-happened
  (let ((entries (list (entry 3 "ann" #(10 0 0 1) "c" 100)
                       (entry 1 "ann" #(10 0 0 1) "a" 100)
                       (entry 4 "bob" #(10 0 0 1) "d" 100)
                       (entry 2 "ann" #(10 0 0 1) "b" 100))))
    (flet ((order (column ascending)
             (mapcar #'fs::activity-entry-sequence
                     (fs::sort-activity entries column ascending))))
      ;; All in the same second.
      (is (equal '(1 2 3 4) (order "time" t)))
      ;; By time, newest first is newest first even within one second.
      (is (equal '(4 3 2 1) (order "time" nil)))
      (is (equal '(1 2 3 4) (order "user" t)))
      (is (equal '(4 1 2 3) (order "user" nil)))
      (is (equal '(1 2 3 4) (order "address" nil)))
      (is (equal '(4 3 2 1) (order "message" nil)))
      ;; And the list it was given is as it was.
      (is (equal '(3 1 4 2) (mapcar #'fs::activity-entry-sequence entries))))))
