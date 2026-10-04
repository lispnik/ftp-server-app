;;;; settings.lisp -- what the window remembers between launches.
;;;;
;;;; One property list, printed to a file.  The file holds the password as it
;;;; was typed, so it is readable by its owner and nobody else.

(in-package #:ftp-server)

(defparameter *default-port* 2121
  "Above 1023, so that the server needs no privilege to listen on it.")

(defun default-settings ()
  (list :version 2 :port *default-port*
        :allow-remote nil :bonjour-name "" :start-at-launch nil :require-tls nil
        :mappings '() :users '()))

(defun settings-directory ()
  "The directory the settings file is in, where the TLS certificate is kept
beside it."
  (uiop:pathname-directory-pathname (settings-file)))

(defun settings-file ()
  "Where the settings are kept.  Worked out when asked rather than when
loaded: the application is a saved image, and the home directory it was built
in need not be the one it runs in.  FTP_SERVER_SETTINGS names another file,
which is how the tests stay away from the real one."
  (let ((override (sb-posix:getenv "FTP_SERVER_SETTINGS")))
    (if (and override (plusp (length override)))
        (sb-ext:parse-native-namestring override)
        (merge-pathnames "Library/Application Support/FTP Server/settings.lisp"
                         (user-homedir-pathname)))))

(defun read-settings-form (path)
  (with-open-file (in path :if-does-not-exist nil :external-format :utf-8)
    (when in
      ;; Read in a package that uses COMMON-LISP, so that a NIL someone typed
      ;; is NIL.  In the keyword package it would be :NIL, which is true -- and
      ;; would make a read-only folder writable.
      (let ((*read-eval* nil)
            (*package* (find-package '#:ftp-server)))
        (ignore-errors (read in nil nil))))))

(defun true-p (value)
  "Whether VALUE is T itself.  Anything else in the file -- a number, a
keyword, a misspelling -- is not a yes: the two flags kept here each widen who
can do what, so only a plain T turns one on."
  (eq value t))

(defun plist-p (object)
  (and (listp object)
       (evenp (length object))
       (loop for key in object by #'cddr always (keywordp key))))

(defun checked-mappings (object)
  "The mappings in OBJECT that are well formed, each as a property list."
  (when (listp object)
    (loop for item in object
          when (and (plist-p item)
                    (stringp (getf item :name))
                    (stringp (getf item :path)))
            collect (list :name (getf item :name)
                          :path (getf item :path)))))

(defun checked-access (object)
  "The grants in OBJECT that are well formed: (mapping-name . level), with
level :READ or :READ-WRITE.  Anything else -- a misspelt level above all -- is
no access, which is the safe way to be wrong."
  (when (listp object)
    (loop for pair in object
          when (and (consp pair)
                    (stringp (car pair))
                    (member (cdr pair) '(:read :read-write)))
            collect (cons (car pair) (cdr pair)))))

(defun checked-users (object)
  "The users in OBJECT that are well formed, each as a property list."
  (when (listp object)
    (loop for item in object
          when (and (plist-p item) (stringp (getf item :name)))
            collect (list :name (getf item :name)
                          :password (let ((password (getf item :password)))
                                      (if (stringp password) password ""))
                          :access (checked-access (getf item :access))))))

(defun migrate-single-user (form)
  "The users a version 1 file means: its one user name and password, with
read and write on the mappings it marked writable and read on the rest."
  (let ((name (getf form :username))
        (password (getf form :password)))
    (when (and (stringp name) (string/= "" name))
      (list (list :name name
                  :password (if (stringp password) password "")
                  :access (loop for item in (and (listp (getf form :mappings))
                                                 (getf form :mappings))
                                when (and (plist-p item) (stringp (getf item :name)))
                                  collect (cons (getf item :name)
                                                (if (true-p (getf item :writable))
                                                    :read-write
                                                    :read))))))))

(defun load-settings (&optional (path (settings-file)))
  "The settings in PATH, with a default for whatever is missing or malformed.
A file that cannot be read at all gives the defaults."
  (let ((form (read-settings-form path))
        (settings (default-settings)))
    (when (plist-p form)
      (flet ((take (key predicate)
               (let ((value (getf form key settings)))
                 (when (and (not (eq value settings)) (funcall predicate value))
                   (setf (getf settings key) value)))))
        (take :port (lambda (value) (typep value '(integer 1 65535))))
        (take :bonjour-name #'stringp)
        (setf (getf settings :allow-remote) (true-p (getf form :allow-remote))
              (getf settings :start-at-launch) (true-p (getf form :start-at-launch))
              (getf settings :require-tls) (true-p (getf form :require-tls))
              (getf settings :mappings) (checked-mappings (getf form :mappings))
              (getf settings :users)
              (if (getf form :users)
                  (checked-users (getf form :users))
                  (migrate-single-user form)))))
    settings))

(defun save-settings (settings &optional (path (settings-file)))
  "Write SETTINGS to PATH: to a file beside it first, then renamed over it, so
that a crash half way leaves the old settings rather than half of the new."
  (let ((temporary (make-pathname :type "tmp" :defaults path)))
    (ensure-directories-exist path)
    (with-open-file (out temporary :direction :output :if-exists :supersede
                                   :external-format :utf-8)
      ;; Before anything is in it.
      (sb-posix:chmod (sb-ext:native-namestring temporary) #o600)
      (let ((*print-readably* nil)
            (*print-pretty* t)
            (*print-length* nil)
            (*print-level* nil)
            (*package* (find-package '#:ftp-server)))
        (prin1 settings out)
        (terpri out)))
    (rename-file temporary path)
    path))
