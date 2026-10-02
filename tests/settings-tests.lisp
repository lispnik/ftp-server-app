;;;; settings-tests.lisp -- the settings file.

(in-package #:ftp-server/tests)

(def-suite settings :in all-tests :description "The settings file.")
(in-suite settings)

(defun settings-path (directory)
  (sb-ext:parse-native-namestring (path directory "settings.lisp")))

(test a-missing-file-gives-the-defaults
  (with-temporary-directory (directory)
    (let ((settings (fs:load-settings (settings-path directory))))
      (is (string= "" (getf settings :username)))
      (is (= 2121 (getf settings :port)))
      (is (null (getf settings :allow-remote)))
      (is (null (getf settings :mappings))))))

(test settings-survive-being-saved-and-loaded
  (with-temporary-directory (directory)
    (let ((settings (list :version 1 :username "ann" :password "p \"q\" \\ r"
                          :port 2200 :allow-remote t :bonjour-name "Files"
                          :start-at-launch t :require-tls t
                          :mappings (list (list :name "tempdir" :path "/tmp" :writable nil)
                                          (list :name "in" :path "/a b/[c]" :writable t))))
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
                "(:username 5 :password \"pw\" :port 99999 :allow-remote 1
                  :mappings ((:name \"ok\" :path \"/tmp\") (:name 3) nonsense))")
    (let ((settings (fs:load-settings (settings-path directory))))
      (is (string= "" (getf settings :username)))
      (is (string= "pw" (getf settings :password)))
      (is (= 2121 (getf settings :port)))
      (is (null (getf settings :allow-remote)) "1 is not T")
      (is (equal '((:name "ok" :path "/tmp" :writable nil))
                 (getf settings :mappings))))))

(test a-file-that-is-not-lisp-gives-the-defaults-and-is-not-evaluated
  (with-temporary-directory (directory)
    (write-file (path directory "settings.lisp") "#.(error \"evaluated\") ((((")
    (is (equal (fs::default-settings)
               (fs:load-settings (settings-path directory))))))

(test a-file-written-by-hand-means-what-it-says
  ;; Found with the built application: read in the keyword package, a typed
  ;; nil is :NIL, which is true, and a read-only folder took an upload.
  (with-temporary-directory (directory)
    (write-file (path directory "settings.lisp")
                "(:version 1 :username \"u\" :password \"p\" :port 2199 :allow-remote nil
                  :mappings ((:name \"ro\" :path \"/tmp\" :writable nil)
                             (:name \"rw\" :path \"/var\" :writable t)
                             (:name \"odd\" :path \"/usr\" :writable :nil)
                             (:name \"yes\" :path \"/usr\" :writable yes)))")
    (let ((settings (fs:load-settings (settings-path directory))))
      (is (null (getf settings :allow-remote)))
      (is (equal '(nil t nil nil)
                 (mapcar (lambda (mapping) (getf mapping :writable))
                         (getf settings :mappings)))))))

(test the-file-is-written-in-plain-lisp
  (with-temporary-directory (directory)
    (fs:save-settings (list :version 1 :username "u" :password "p" :port 2121
                            :allow-remote t :bonjour-name ""
                            :mappings (list (list :name "a" :path "/tmp" :writable nil)))
                      (settings-path directory))
    (let ((text (read-file (path directory "settings.lisp"))))
      (is (null (search "COMMON-LISP" text)))
      (is (search ":WRITABLE NIL" text))
      (is (search ":ALLOW-REMOTE T" text)))))
