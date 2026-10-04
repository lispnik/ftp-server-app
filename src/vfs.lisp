;;;; vfs.lisp -- the virtual filesystem an FTP client sees.
;;;;
;;;; The root is not a directory on this host.  It is a list of MAPPINGS, each
;;;; a name and what that name stands for: map /tmp as "tempdir" and a client
;;;; that lists / sees tempdir, and inside it what /tmp holds.  What a mapping
;;;; holds is its BACKEND's business (backend.lisp): a directory on this host,
;;;; which is the usual thing and what everything in this file is about, or
;;;; files and directories made by Lisp as they are asked for.
;;;;
;;;; Virtual paths are lists of components, so "/tempdir/a" is ("tempdir" "a")
;;;; and the root is ().  Host paths are native namestrings -- strings, never
;;;; pathnames, because a pathname reads * and [ in a file's name as a pattern.

(in-package #:ftp-server)

;;; Conditions ------------------------------------------------------------------

(define-condition vfs-error (error)
  ((message :initarg :message :reader vfs-error-message))
  (:report (lambda (condition stream)
             (write-string (vfs-error-message condition) stream))))

(define-condition vfs-not-found (vfs-error) ()
  (:default-initargs :message "No such file or directory."))

(define-condition vfs-denied (vfs-error) ()
  (:default-initargs :message "Permission denied."))

(define-condition mapping-error (error)
  ((message :initarg :message :reader mapping-error-message))
  (:report (lambda (condition stream)
             (write-string (mapping-error-message condition) stream))))

;;; Mappings --------------------------------------------------------------------

(defstruct (mapping (:constructor %make-mapping (name host-path writable
                                                  &optional backend)))
  (name "" :type string)
  (host-path "" :type string)
  (writable nil)
  ;; NIL for a directory on this host, at HOST-PATH; otherwise the backend
  ;; that makes what the mapping holds.
  (backend nil))

(defun host-mapping-p (mapping)
  "Whether MAPPING is a directory on this host."
  (null (mapping-backend mapping)))

;;; Access ----------------------------------------------------------------------
;;;
;;; What the client being served may do with a mapping: NIL, nothing -- it is
;;; not there as far as they are concerned; :READ; or :READ-WRITE.  A session
;;; binds *ACCESS* to a function of a mapping that says, for its own user,
;;; while it runs each command.

(defun default-access (mapping)
  "Access with no users to tell apart: everyone may read every mapping, and
write those marked writable.  What a server has unless it is told otherwise."
  (if (mapping-writable mapping) :read-write :read))

(defvar *access* 'default-access
  "A function of a mapping answering NIL, :READ or :READ-WRITE for whoever is
being served.")

(defun access-to (mapping)
  (funcall *access* mapping))

(defun may-read-p (mapping)
  (and (access-to mapping) t))

(defun may-write-p (mapping)
  (eq :read-write (access-to mapping)))

(defclass vfs ()
  ((mappings :initform '() :accessor %vfs-mappings)
   (lock :initform (sb-thread:make-mutex :name "ftp-server vfs") :reader vfs-lock))
  (:documentation "The mappings, in the order they were added.  The window
changes them on the main thread while sessions read them on their own."))

(defun make-vfs () (make-instance 'vfs))

(defmacro with-vfs-lock ((vfs) &body body)
  `(sb-thread:with-recursive-lock ((vfs-lock ,vfs)) ,@body))

(defun vfs-mappings (vfs)
  "A fresh list of the mappings."
  (with-vfs-lock (vfs) (copy-list (%vfs-mappings vfs))))

(defun valid-mapping-name-p (name)
  "Whether NAME can be one component of a path: not empty, not . or .., and
with no slash, no control character and no space at either end."
  (and (stringp name)
       (<= 1 (length name) 255)
       (not (member name '("." "..") :test #'string=))
       (not (find #\/ name))
       (notany (lambda (char) (< (char-code char) 32)) name)
       (char/= #\Space (char name 0))
       (char/= #\Space (char name (1- (length name))))))

(defun vfs-find (vfs name)
  "The mapping called NAME, or NIL."
  (with-vfs-lock (vfs)
    (find name (%vfs-mappings vfs) :key #'mapping-name :test #'string=)))

(defun check-mapping-name (vfs name &optional except)
  "Signal MAPPING-ERROR unless NAME is valid and no mapping but EXCEPT has it.
Names that differ only in case count as the same: most clients on a Mac could
not tell them apart."
  (unless (valid-mapping-name-p name)
    (error 'mapping-error
           :message (format nil "~s is not a usable name." name)))
  (let ((other (find name (%vfs-mappings vfs) :key #'mapping-name :test #'string-equal)))
    (when (and other (not (eq other except)))
      (error 'mapping-error
             :message (format nil "There is already a mapping called ~a." name)))))

(defun vfs-add (vfs name host-path &key writable)
  "Map HOST-PATH as NAME.  Answers the mapping, or signals MAPPING-ERROR."
  (with-vfs-lock (vfs)
    (check-mapping-name vfs name)
    (let ((mapping (%make-mapping name host-path (and writable t))))
      (setf (%vfs-mappings vfs) (append (%vfs-mappings vfs) (list mapping)))
      mapping)))

(defun vfs-add-backend (vfs name backend &key writable)
  "Map NAME to what BACKEND makes.  Answers the mapping, or signals
MAPPING-ERROR."
  (with-vfs-lock (vfs)
    (check-mapping-name vfs name)
    (let ((mapping (%make-mapping name "" (and writable t) backend)))
      (setf (%vfs-mappings vfs) (append (%vfs-mappings vfs) (list mapping)))
      mapping)))

(defun vfs-remove (vfs mapping)
  "Remove MAPPING, a mapping or the name of one.  True if it was there."
  (with-vfs-lock (vfs)
    (let ((found (if (stringp mapping) (vfs-find vfs mapping) mapping)))
      (when (member found (%vfs-mappings vfs))
        (setf (%vfs-mappings vfs) (remove found (%vfs-mappings vfs)))
        t))))

(defun vfs-rename (vfs mapping name)
  "Call MAPPING by NAME from now on, or signal MAPPING-ERROR."
  (with-vfs-lock (vfs)
    (check-mapping-name vfs name mapping)
    (setf (mapping-name mapping) name)
    mapping))

;;; Virtual paths ---------------------------------------------------------------

(defun split-on-slash (string)
  (loop with start = 0
        for slash = (position #\/ string :start start)
        collect (subseq string start slash)
        while slash
        do (setf start (1+ slash))))

(defun parse-virtual-path (cwd string)
  "The components STRING names, read from CWD unless it starts with a slash.

. and .. are resolved here, on the names alone, and .. at the root stays at the
root.  So nothing a client types can name a place above the root; what a
symbolic link on the host can do is RESOLVE's business."
  (when (find (code-char 0) string)
    (error 'vfs-not-found))
  (let ((components (if (and (plusp (length string)) (char= #\/ (char string 0)))
                        '()
                        (reverse cwd))))
    (dolist (part (split-on-slash string))
      (cond ((or (string= part "") (string= part ".")))
            ((string= part "..") (pop components))
            (t (push part components))))
    (nreverse components)))

(defun virtual-path-string (components)
  "COMPONENTS as a client writes them: /a/b, and / for the root."
  (if (null components)
      "/"
      (format nil "~{/~a~}" components)))

;;; Host paths ------------------------------------------------------------------

(defun real-path (native)
  "NATIVE with every symbolic link followed, as realpath(3) answers, or NIL if
there is no such file."
  (sb-alien:with-alien ((buffer (sb-alien:array sb-alien:char 1025)))
    (values
     (sb-alien:alien-funcall
      (sb-alien:extern-alien
       "realpath"
       (function (sb-alien:c-string :external-format :utf-8)
                 (sb-alien:c-string :external-format :utf-8)
                 (* sb-alien:char)))
      native
      (sb-alien:cast buffer (* sb-alien:char))))))

(defun path-within-p (real root-real)
  "Whether REAL is ROOT-REAL or something beneath it.  Both are real paths.
The slash is what keeps /private/tmp2 out of /private/tmp."
  (or (string= real root-real)
      (let ((prefix (if (and (plusp (length root-real))
                             (char= #\/ (char root-real (1- (length root-real)))))
                        root-real
                        (concatenate 'string root-real "/"))))
        (and (> (length real) (length prefix))
             (string= prefix real :end2 (length prefix))))))

(defun join-host-path (directory &rest names)
  (format nil "~a~{/~a~}" (string-right-trim "/" directory) names))

(defun resolve (vfs components &key (intent :existing))
  "Where COMPONENTS is on this host: (values KIND MAPPING HOST-PATH).

KIND is :ROOT for the root, which has no mapping and no host path;
:MAPPING-ROOT for a mapping itself; and :INSIDE for anything beneath one.

INTENT says what the caller will do there.

  :EXISTING  read it.  Every link is followed, and the file that is finally
             reached has to be inside the mapped directory.
  :LEAF      create, remove or rename the last component.  The directory that
             holds it is followed and has to be inside; the last component is
             left as it is, so that removing a link removes the link.

The mapped directory is itself resolved first, each time: /tmp is a link to
/private/tmp, and it is /private/tmp that everything is measured against.

Signals VFS-NOT-FOUND or VFS-DENIED."
  (if (null components)
      (values :root nil nil)
      (let ((mapping (or (vfs-find vfs (first components))
                         (error 'vfs-not-found)))
            (rest (rest components)))
        (values (if rest :inside :mapping-root)
                mapping
                (and (host-mapping-p mapping)
                     (host-path-in mapping rest :intent intent))))))

(defun host-path-in (mapping rest &key (intent :existing))
  "Where REST, the components after MAPPING's name, is on this host, by the
rules RESOLVE describes.  Signals VFS-NOT-FOUND or VFS-DENIED."
  (let ((root-real (or (real-path (mapping-host-path mapping))
                       (error 'vfs-not-found
                              :message "The mapped directory is not available."))))
    (flet ((contained (candidate)
             (let ((real (or (real-path candidate) (error 'vfs-not-found))))
               (unless (path-within-p real root-real)
                 (error 'vfs-denied))
               real)))
      (cond ((null rest) root-real)
            ((eq intent :existing)
             (contained (apply #'join-host-path root-real rest)))
            (t
             (join-host-path (contained (apply #'join-host-path root-real (butlast rest)))
                             (first (last rest))))))))
