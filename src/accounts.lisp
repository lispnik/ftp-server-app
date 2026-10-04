;;;; accounts.lisp -- who may log in, and what each of them may do.
;;;;
;;;; A user is a name, a password, and an access level for each mapping they
;;;; have any: :READ or :READ-WRITE.  A mapping a user has no level for is one
;;;; they cannot see.  Levels are kept by the mapping's name, which is what the
;;;; settings file can say, so a mapping that is renamed has its levels moved
;;;; and one that is removed has them dropped.
;;;;
;;;; The window changes these on the main thread while sessions read them on
;;;; their own, so every reader and writer takes the lock.

(in-package #:ftp-server)

(defparameter *access-levels* '(nil :read :read-write)
  "From least to most.")

(define-condition account-error (error)
  ((message :initarg :message :reader account-error-message))
  (:report (lambda (condition stream)
             (write-string (account-error-message condition) stream))))

(defstruct (user (:constructor %make-user (name password grants)))
  (name "" :type string)
  (password "" :type string)
  (grants '() :type list))              ; an alist of mapping name to level

(defclass accounts ()
  ((users :initform '() :accessor %accounts-users)
   (lock :initform (sb-thread:make-mutex :name "ftp-server accounts")
         :reader accounts-lock)))

(defun make-accounts () (make-instance 'accounts))

(defmacro with-accounts-lock ((accounts) &body body)
  `(sb-thread:with-recursive-lock ((accounts-lock ,accounts)) ,@body))

(defun accounts-users (accounts)
  "A fresh list of the users, in the order they were added."
  (with-accounts-lock (accounts) (copy-list (%accounts-users accounts))))

(defun valid-user-name-p (name)
  "Whether NAME will do as a user name: something, of no more than 64
characters, with no control character and no space at either end."
  (and (stringp name)
       (<= 1 (length name) 64)
       (notany (lambda (char) (< (char-code char) 32)) name)
       (char/= #\Space (char name 0))
       (char/= #\Space (char name (1- (length name))))))

(defun accounts-find (accounts name)
  "The user called NAME, exactly, or NIL."
  (with-accounts-lock (accounts)
    (find name (%accounts-users accounts) :key #'user-name :test #'string=)))

(defun check-user-name (accounts name &optional except)
  (unless (valid-user-name-p name)
    (error 'account-error :message (format nil "~s is not a usable user name." name)))
  ;; Names that differ only in case are one name: a person typing theirs
  ;; would not be told which of two they had got.
  (let ((other (find name (%accounts-users accounts) :key #'user-name :test #'string-equal)))
    (when (and other (not (eq other except)))
      (error 'account-error :message (format nil "There is already a user called ~a." name)))))

(defun accounts-add (accounts name &key (password "") access)
  "Add a user called NAME.  Answers the user, or signals ACCOUNT-ERROR."
  (with-accounts-lock (accounts)
    (check-user-name accounts name)
    (let ((user (%make-user name password (copy-alist access))))
      (setf (%accounts-users accounts) (append (%accounts-users accounts) (list user)))
      user)))

(defun accounts-remove (accounts user)
  "Remove USER.  True if they were there."
  (with-accounts-lock (accounts)
    (when (member user (%accounts-users accounts))
      (setf (%accounts-users accounts) (remove user (%accounts-users accounts)))
      t)))

(defun accounts-rename (accounts user name)
  "Call USER NAME from now on, or signal ACCOUNT-ERROR."
  (with-accounts-lock (accounts)
    (check-user-name accounts name user)
    (setf (user-name user) name)
    user))

(defun accounts-set-password (accounts user password)
  (with-accounts-lock (accounts)
    (setf (user-password user) password)))

(defun user-access (accounts user mapping-name)
  "USER's level on the mapping called MAPPING-NAME: NIL, :READ or :READ-WRITE."
  (with-accounts-lock (accounts)
    (cdr (assoc mapping-name (user-grants user) :test #'string=))))

(defun (setf user-access) (level accounts user mapping-name)
  (unless (member level *access-levels*)
    (error 'account-error :message (format nil "~s is not an access level." level)))
  (with-accounts-lock (accounts)
    (setf (user-grants user)
          (if level
              (acons mapping-name level
                     (remove mapping-name (user-grants user) :key #'car :test #'string=))
              (remove mapping-name (user-grants user) :key #'car :test #'string=))))
  level)

(defun accounts-rename-mapping (accounts old new)
  "Move every user's level on the mapping called OLD to its new name, NEW."
  (with-accounts-lock (accounts)
    (dolist (user (%accounts-users accounts))
      (let ((pair (assoc old (user-grants user) :test #'string=)))
        (when pair
          (setf (car pair) new))))))

(defun accounts-forget-mapping (accounts name)
  "Drop every user's level on the mapping called NAME."
  (with-accounts-lock (accounts)
    (dolist (user (%accounts-users accounts))
      (setf (user-grants user)
            (remove name (user-grants user) :key #'car :test #'string=)))))

;;; Logging in, and being let in --------------------------------------------------------

(defun constant-time-string= (a b)
  "Whether A and B are the same, taking as long to say no as to say yes."
  (let ((a (line-to-octets a))
        (b (line-to-octets b))
        (difference 0))
    (setf difference (logxor (length a) (length b)))
    (dotimes (index (length a))
      (setf difference
            (logior difference
                    (logxor (aref a index)
                            (if (< index (length b)) (aref b index) 0)))))
    (zerop difference)))

(defun accounts-authenticate (accounts name password)
  "The name of the user NAME, if PASSWORD is theirs, or NIL.  A user with no
password cannot log in at all."
  (let ((user (accounts-find accounts name)))
    ;; Compared even for a user there is none of, so that taking longer does
    ;; not say which names exist.
    (let ((matches (constant-time-string= password (if user (user-password user) ""))))
      (and user matches (string/= "" (user-password user)) (user-name user)))))

(defun accounts-access (accounts name mapping)
  "The level of the user called NAME on MAPPING, as they are now: a user who
has since been removed has none, and one whose level has changed has the new
one, from the next thing they ask for."
  (let ((user (accounts-find accounts name)))
    (and user (user-access accounts user (mapping-name mapping)))))

(defun accounts-server-functions (accounts)
  "The :AUTHENTICATOR and :ACCESS a server serving ACCOUNTS is to be made with."
  (values (lambda (name password) (accounts-authenticate accounts name password))
          (lambda (name mapping) (accounts-access accounts name mapping))))
