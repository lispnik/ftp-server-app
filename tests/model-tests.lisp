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
    (is (equal '("tmp" "tmp-2" "tmp-3") (mapping-names model)))))

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

(test rows-are-removed-by-index
  (let ((model (fs:make-model)))
    (fs:model-add-directory model "/tmp")
    (fs:model-add-directory model "/var")
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

;;; Users ------------------------------------------------------------------------

(test new-users-get-names-of-their-own-and-nothing-else
  (let ((model (fs:make-model)))
    (fs:model-add-directory model "/tmp")
    (let ((first (fs:model-add-user model))
          (second (fs:model-add-user model)))
      (is (equal '("user" "user2") (user-names model)))
      (is (string= "" (fs:user-password first)))
      (is (null (fs:model-access model second (first (fs:vfs-mappings (fs:model-vfs model)))))
          "no access until given some"))))

(test a-user-can-be-renamed-but-not-onto-another
  (let ((model (fs:make-model)))
    (let ((ann (add-user model "ann" "a")))
      (add-user model "bob" "b")
      (is (equal '(t nil) (multiple-value-list (fs:model-rename-user model ann "  anne "))))
      (is (equal '("anne" "bob") (user-names model)))
      (multiple-value-bind (ok message) (fs:model-rename-user model ann "BOB")
        (is (null ok))
        (is (search "already" message)))
      (is (null (fs:model-rename-user model ann "")))
      (is (equal '("anne" "bob") (user-names model))))))

(test removing-a-user-removes-them
  (let ((model (fs:make-model)))
    (let ((ann (add-user model "ann" "a")))
      (add-user model "bob" "b")
      (is-true (fs:model-remove-user model ann))
      (is-false (fs:model-remove-user model ann))
      (is (equal '("bob") (user-names model))))))

(test access-is-kept-by-mapping-and-follows-it
  (let ((model (fs:make-model)))
    (fs:model-add-directory model "/tmp")
    (fs:model-add-directory model "/var")
    (let ((ann (add-user model "ann" "a" "tmp" :read-write "var" :read))
          (tmp (fs:vfs-find (fs:model-vfs model) "tmp")))
      (is (eq :read-write (fs:model-access model ann tmp)))
      (fs:model-set-access model ann tmp :read)
      (is (eq :read (fs:model-access model ann tmp)))
      ;; Renamed, the mapping keeps who may use it.
      (fs:model-rename-mapping model 0 "tempdir")
      (is (eq :read (fs:model-access model ann tmp)))
      (is (null (access-of model ann "tmp")))
      ;; Removed, it leaves nothing behind: a new mapping with the old name
      ;; is not opened to whoever had the old one.
      (fs:model-remove-mapping model 1)
      (is (null (access-of model ann "var")))
      (fs:model-set-access model ann tmp nil)
      (is (null (fs:model-access model ann tmp))))))

(test a-bad-access-level-is-refused
  (let* ((model (fs:make-model))
         (ann (add-user model "ann" "a")))
    (signals fs:account-error
      (setf (fs:user-access (fs:model-accounts model) ann "tmp") :everything))))

(test logging-in-wants-a-user-with-that-password
  (let* ((model (fs:make-model))
         (accounts (fs:model-accounts model)))
    (add-user model "ann" "secret")
    (add-user model "nopass" "")
    (is (equal "ann" (fs:accounts-authenticate accounts "ann" "secret")))
    (is (null (fs:accounts-authenticate accounts "ann" "wrong")))
    (is (null (fs:accounts-authenticate accounts "ANN" "secret")) "names are exact")
    (is (null (fs:accounts-authenticate accounts "nobody" "")))
    (is (null (fs:accounts-authenticate accounts "nopass" "")) "no password, no login")))

(test the-server-will-not-start-without-a-user-with-a-password
  (let ((model (fs:make-model)))
    (setf (fs:model-port model) (free-local-port))
    (multiple-value-bind (ok message) (fs:model-start model)
      (is (null ok))
      (is (search "Users" message)))
    (add-user model "ann" "")
    (is (null (fs:model-start model)))
    (is-false (fs:model-running-p model))
    (is (string= "Stopped." (fs:model-status-text model)))))

(test the-model-starts-and-stops-a-server-that-knows-its-users
  (let ((model (fs:make-model))
        (delay fs::*login-failure-delay*))
    (add-user model "ann" "pw")
    (add-user model "bob" "bw")
    (setf (fs:model-port model) (free-local-port)
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
             (is (= 530 (login client "ann" "bw")) "someone else's password")
             (is (= 230 (login client "bob" "bw")))
             (is (search "1 client " (fs:model-status-text model))))
           ;; A user added while it runs can log in at once.
           (add-user model "cat" "cw")
           (with-client (client (fs:model-port model))
             (is (= 230 (login client "cat" "cw"))))
           ;; A second server on the same port is refused in words.
           (let ((other (fs:make-model)))
             (add-user other "a" "b")
             (setf (fs:model-port other) (fs:model-port model))
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

(defun settings-in (directory)
  (sb-ext:parse-native-namestring (path directory "settings.lisp")))

(test the-model-goes-to-settings-and-back
  (with-temporary-directory (directory)
    (let ((file (settings-in directory))
          (model (fs:make-model)))
      (setf (fs:model-port model) 2200
            (fs:model-allow-remote model) t
            (fs:model-bonjour-name model) "Files")
      (fs:model-add-directory model "/tmp")
      (fs:model-rename-mapping model 0 "tempdir")
      (add-user model "ann" "pw" "tempdir" :read-write)
      (add-user model "bob" "bw" "tempdir" :read "gen" :read)
      (fs:model-save model file)
      (let ((loaded (fs:make-model (fs:load-settings file))))
        (is (= 2200 (fs:model-port loaded)))
        (is-true (fs:model-allow-remote loaded))
        (is (string= "Files" (fs:model-bonjour-name loaded)))
        (is (equal '("tempdir") (mapping-names loaded)))
        (is (equal '("ann" "bob") (user-names loaded)))
        (destructuring-bind (ann bob) (fs:model-users loaded)
          (is (string= "pw" (fs:user-password ann)))
          (is (eq :read-write (access-of loaded ann "tempdir")))
          (is (eq :read (access-of loaded bob "tempdir")))
          ;; Kept for a Lisp mapping init.lisp will make again.
          (is (eq :read (access-of loaded bob "gen"))))))))

(test settings-with-a-clashing-mapping-keep-the-first
  (let ((model (fs:make-model (list :version 2 :port 2121
                                    :allow-remote nil :bonjour-name ""
                                    :mappings '((:name "a" :path "/tmp")
                                                (:name "A" :path "/var")
                                                (:name "b/c" :path "/var"))
                                    :users '((:name "ann" :password "" :access ())
                                             (:name "ANN" :password "" :access ()))))))
    (is (equal '("a") (mapping-names model)))
    (is (equal '("ann") (user-names model)))))

;;; Passwords kept somewhere else ------------------------------------------------------

(defun memory-store (&key fail)
  "A password store that keeps passwords in a table: (values STORE TABLE).
With FAIL it refuses to store."
  (let ((kept (make-hash-table :test 'equal)))
    (values (fs:make-password-store
             :fetch (lambda (name) (gethash name kept ""))
             :store (lambda (name password)
                      (when fail (error "refused"))
                      (setf (gethash name kept) password))
             :forget (lambda (name) (remhash name kept)))
            kept)))

(defmacro with-password-store ((store) &body body)
  `(let ((fs:*password-store* ,store))
     ,@body))

(test with-a-store-passwords-are-not-in-the-file
  (with-temporary-directory (directory)
    (multiple-value-bind (store kept) (memory-store)
      (with-password-store (store)
        (let ((model (fs:make-model)))
          (add-user model "ann" "hunter2")
          (add-user model "bob" "swordfish")
          (is-true (fs:model-save model (settings-in directory)))
          (is (string= "hunter2" (gethash "ann" kept)))
          (is (string= "swordfish" (gethash "bob" kept)))
          (let ((text (read-file (path directory "settings.lisp"))))
            (is (null (search "hunter2" text)))
            (is (null (search "swordfish" text))))
          (let ((loaded (fs:model-load (settings-in directory))))
            (is (equal '("hunter2" "swordfish")
                       (mapcar #'fs:user-password (fs:model-users loaded))))))))))

(test removing-or-renaming-a-user-moves-their-password
  (with-temporary-directory (directory)
    (multiple-value-bind (store kept) (memory-store)
      (with-password-store (store)
        (let* ((model (fs:make-model))
               (ann (add-user model "ann" "a"))
               (bob (add-user model "bob" "b")))
          (fs:model-save model (settings-in directory))
          (fs:model-rename-user model ann "anne")
          (fs:model-save model (settings-in directory))
          (is (null (nth-value 1 (gethash "ann" kept))) "the old name's is gone")
          (is (string= "a" (gethash "anne" kept)))
          (fs:model-remove-user model bob)
          (is (null (nth-value 1 (gethash "bob" kept)))))))))

(test a-single-user-file-becomes-a-user-with-their-access
  ;; What the version with one login wrote: a user name and password, and
  ;; which mappings were writable.
  (with-temporary-directory (directory)
    (write-file (path directory "settings.lisp")
                "(:version 1 :username \"ann\" :password \"old-secret\" :port 2121
                  :mappings ((:name \"rw\" :path \"/tmp\" :writable t)
                             (:name \"ro\" :path \"/var\" :writable nil)))")
    (multiple-value-bind (store kept) (memory-store)
      (with-password-store (store)
        (let* ((loaded (fs:model-load (settings-in directory)))
               (ann (first (fs:model-users loaded))))
          (is (equal '("ann") (user-names loaded)))
          (is (string= "old-secret" (fs:user-password ann)))
          (is (eq :read-write (access-of loaded ann "rw")))
          (is (eq :read (access-of loaded ann "ro")))
          ;; And the password has gone from the file to the store.
          (is (string= "old-secret" (gethash "ann" kept)))
          (is (null (search "old-secret" (read-file (path directory "settings.lisp"))))))))))

(test a-store-that-refuses-does-not-put-passwords-in-the-file
  (with-temporary-directory (directory)
    (with-password-store ((memory-store :fail t))
      (let ((model (fs:make-model)))
        (add-user model "ann" "hunter2")
        (is-false (fs:model-save model (settings-in directory)))
        (let ((text (read-file (path directory "settings.lisp"))))
          (is (null (search "hunter2" text)))
          (is (search "ann" text) "the rest is saved all the same"))))))

(test without-a-store-the-file-has-the-passwords
  (with-temporary-directory (directory)
    (with-password-store (nil)
      (let ((model (fs:make-model)))
        (add-user model "ann" "hunter2")
        (is-true (fs:model-save model (settings-in directory)))
        (is (string= "hunter2"
                     (fs:user-password
                      (first (fs:model-users (fs:model-load (settings-in directory)))))))))))

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

(test activity-is-copied-as-lines-of-tab-separated-columns
  (let ((text (fs::activity-text (list (entry 1 "ann" #(10 0 0 1) "logged in")
                                       (entry 2 nil nil "server stopped")))))
    (destructuring-bind (first second) (remove "" (uiop:split-string text :separator '(#\Newline))
                                               :test #'string=)
      (is (equal '("ann" "10.0.0.1" "logged in")
                 (rest (uiop:split-string first :separator '(#\Tab)))))
      (is (equal '("-" "-" "server stopped")
                 (rest (uiop:split-string second :separator '(#\Tab)))))
      (is (= 4 (length (uiop:split-string first :separator '(#\Tab)))))))
  (is (string= "" (fs::activity-text '()))))
