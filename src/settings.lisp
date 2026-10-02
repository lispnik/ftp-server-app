;;;; settings.lisp -- what the window remembers between launches.
;;;;
;;;; One property list, printed to a file.  The file holds the password as it
;;;; was typed, so it is readable by its owner and nobody else.

(in-package #:ftp-server)

(defparameter *default-port* 2121
  "Above 1023, so that the server needs no privilege to listen on it.")

(defun default-settings ()
  (list :version 1 :username "" :password "" :port *default-port*
        :allow-remote nil :bonjour-name "" :mappings '()))

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
                          :path (getf item :path)
                          :writable (true-p (getf item :writable))))))

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
        (take :username #'stringp)
        (take :password #'stringp)
        (take :port (lambda (value) (typep value '(integer 1 65535))))
        (take :bonjour-name #'stringp)
        (setf (getf settings :allow-remote) (true-p (getf form :allow-remote))
              (getf settings :mappings) (checked-mappings (getf form :mappings)))))
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
