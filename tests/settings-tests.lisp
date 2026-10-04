;;;; settings-tests.lisp -- the settings file.

(in-package #:ftp-server/tests)

(def-suite settings :in all-tests :description "The settings file.")
(in-suite settings)

(defun settings-path (directory)
  (sb-ext:parse-native-namestring (path directory "settings.lisp")))

(test a-missing-file-gives-the-defaults
  (with-temporary-directory (directory)
    (let ((settings (fs:load-settings (settings-path directory))))
      (is (= 2121 (getf settings :port)))
      (is (null (getf settings :allow-remote)))
      (is (null (getf settings :mappings)))
      (is (null (getf settings :users))))))

(test settings-survive-being-saved-and-loaded
  (with-temporary-directory (directory)
    (let ((settings (list :version 2 :port 2200 :allow-remote t :bonjour-name "Files"
                          :start-at-launch t :require-tls t
                          :mappings (list (list :name "tempdir" :path "/tmp")
                                          (list :name "in" :path "/a b/[c]"))
                          :users (list (list :name "ann" :password "p \"q\" \\ r"
                                             :access (list (cons "tempdir" :read-write)
                                                           (cons "in" :read)))
                                       (list :name "bob" :password "" :access '()))))
          (file (settings-path directory)))
      (fs:save-settings settings file)
      (is (equal settings (fs:load-settings file))))))

(test the-file-is-private-to-its-owner
  (with-temporary-directory (directory)
    (let ((file (settings-path directory)))
      (fs:save-settings (fs::default-settings) file)
      (is (= #o600 (logand #o777 (sb-posix:stat-mode
                                  (sb-posix:stat (path directory "settings.lisp"))))))
      ;; And again over the top of an existing one.
      (fs:save-settings (fs::default-settings) file)
      (is (= #o600 (logand #o777 (sb-posix:stat-mode
                                  (sb-posix:stat (path directory "settings.lisp")))))))))

(test nonsense-in-the-file-falls-back-field-by-field
  (with-temporary-directory (directory)
    (write-file (path directory "settings.lisp")
                "(:version 2 :port 99999 :allow-remote 1
                  :mappings ((:name \"ok\" :path \"/tmp\") (:name 3) nonsense)
                  :users ((:name \"ann\" :password 7
                           :access ((\"ok\" . :read-write) (\"x\" . :everything) (3 . :read) junk))
                          (:password \"no name\")
                          nonsense))")
    (let ((settings (fs:load-settings (settings-path directory))))
      (is (= 2121 (getf settings :port)))
      (is (null (getf settings :allow-remote)) "1 is not T")
      (is (equal '((:name "ok" :path "/tmp")) (getf settings :mappings)))
      ;; A level that is not one is no access, not some.
      (is (equal '((:name "ann" :password "" :access (("ok" . :read-write))))
                 (getf settings :users))))))

(test a-file-that-is-not-lisp-gives-the-defaults-and-is-not-evaluated
  (with-temporary-directory (directory)
    (write-file (path directory "settings.lisp") "#.(error \"evaluated\") ((((")
    (is (equal (fs::default-settings)
               (fs:load-settings (settings-path directory))))))

(test a-single-user-file-becomes-one-user
  ;; Version 1 had one user name and password, and a writable flag on each
  ;; mapping.  Read in a package where a typed nil is NIL: in the keyword
  ;; package it would be :NIL, which is true, and a read-only folder would
  ;; become writable.
  (with-temporary-directory (directory)
    (write-file (path directory "settings.lisp")
                "(:version 1 :username \"u\" :password \"p\" :port 2199 :allow-remote nil
                  :mappings ((:name \"ro\" :path \"/tmp\" :writable nil)
                             (:name \"rw\" :path \"/var\" :writable t)
                             (:name \"odd\" :path \"/usr\" :writable :nil)
                             (:name \"yes\" :path \"/opt\" :writable yes)))")
    (let ((settings (fs:load-settings (settings-path directory))))
      (is (null (getf settings :allow-remote)))
      (is (equal '((:name "ro" :path "/tmp") (:name "rw" :path "/var")
                   (:name "odd" :path "/usr") (:name "yes" :path "/opt"))
                 (getf settings :mappings)))
      (is (equal '((:name "u" :password "p"
                    :access (("ro" . :read) ("rw" . :read-write)
                             ("odd" . :read) ("yes" . :read))))
                 (getf settings :users))))))

(test a-single-user-file-with-no-user-has-no-users
  (with-temporary-directory (directory)
    (write-file (path directory "settings.lisp")
                "(:version 1 :username \"\" :password \"\" :mappings ((:name \"a\" :path \"/tmp\" :writable t)))")
    (is (null (getf (fs:load-settings (settings-path directory)) :users)))))

(test the-file-is-written-in-plain-lisp
  (with-temporary-directory (directory)
    (fs:save-settings (list :version 2 :port 2121 :allow-remote t :bonjour-name ""
                            :mappings (list (list :name "a" :path "/tmp"))
                            :users (list (list :name "u" :password ""
                                               :access (list (cons "a" :read)))))
                      (settings-path directory))
    (let ((text (read-file (path directory "settings.lisp"))))
      (is (null (search "COMMON-LISP" text)))
      (is (search ":ALLOW-REMOTE T" text))
      (is (search "(\"a\" . :READ)" text)))))
