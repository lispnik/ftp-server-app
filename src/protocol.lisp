;;;; protocol.lisp -- the commands that need no data connection.
;;;;
;;;; A command is a function of the session and its argument that answers a
;;;; reply: (values CODE TEXT), TEXT a string or a list of lines.  DISPATCH
;;;; finds it and calls it, and needs no socket to do so, which is how these
;;;; are tested.  The commands that move data are in session.lisp.

(in-package #:ftp-server)

(defparameter *login-failure-delay* 1
  "Seconds to wait before refusing a wrong password.")

(defclass session ()
  ((server :initarg :server :reader session-server)
   (vfs :initarg :vfs :reader session-vfs)
   (socket :initarg :socket :initform nil :reader session-socket)
   (stream :initarg :stream :initform nil :reader session-stream)
   (peer :initarg :peer :initform nil :reader session-peer)
   (state :initform :new :accessor session-state
          :documentation ":NEW, :NEED-PASSWORD or :LOGGED-IN.")
   (user :initform nil :accessor session-user)
   (cwd :initform '() :accessor session-cwd)
   (rename-from :initform nil :accessor session-rename-from
                :documentation "After RNFR: a list of the mapping and host path.")
   (rest-offset :initform 0 :accessor session-rest-offset)
   (transfer-type :initform :image :accessor session-transfer-type
                  :documentation ":IMAGE, where a file crosses as it is, or
:ASCII, where its lines end in CR LF on the wire.  Image until the client says
otherwise, although the standard's default is the other: a client that never
says is far more likely to want its file unchanged.")
   (passive :initform nil :accessor session-passive
            :documentation "The socket PASV or EPSV left listening.")
   (data :initform nil :accessor session-data
         :documentation "The data connection, while a transfer is running.")
   (quit :initform nil :accessor session-quit-p)))

;;; The table of commands ----------------------------------------------------------

(defvar *commands* (make-hash-table :test #'equal)
  "Verb to (FUNCTION . NEEDS-LOGIN).")

(defmacro define-command (verb (session argument &key (login t)) &body body)
  "Define the command VERB.  BODY answers (values CODE TEXT)."
  (let ((name (intern (format nil "COMMAND-~a" verb) '#:ftp-server)))
    `(progn
       (defun ,name (,session ,argument)
         (declare (ignorable ,session ,argument))
         ,@body)
       (setf (gethash ,verb *commands*) (cons ',name ,login))
       ',name)))

(defun parse-command-line (line)
  "LINE as (values VERB ARGUMENT): the verb in upper case, and the rest
of the line after one space, or NIL if there is none."
  (let* ((line (string-left-trim " " line))
         (space (position #\Space line)))
    (values (string-upcase (subseq line 0 space))
            (and space
                 (< (1+ space) (length line))
                 (subseq line (1+ space))))))

(defun dispatch (session verb argument)
  "Run the command VERB and answer its reply.

Whatever goes wrong with a file is the client's 550, with no more said than
that: the reply does not distinguish a file that is missing from one that a
link put out of reach."
  (let ((command (gethash verb *commands*)))
    (multiple-value-prog1
        (cond ((null command)
               (values 502 "Command not implemented."))
              ((and (cdr command) (not (eq :logged-in (session-state session))))
               (values 530 "Please log in with USER and PASS."))
              (t
               (handler-case (funcall (car command) session argument)
                 (vfs-error (condition)
                   (values 550 (vfs-error-message condition)))
                 (sb-posix:syscall-error (condition)
                   (values 550 (sb-int:strerror (sb-posix:syscall-errno condition))))
                 (file-error ()
                   (values 550 "The file could not be opened.")))))
      ;; RNFR is good for the next command only.
      (unless (string= verb "RNFR")
        (setf (session-rename-from session) nil))
      ;; REST is good for the next transfer, whatever comes between: clients
      ;; differ on whether they ask for the data port before it or after.
      (when (member verb '("RETR" "STOR" "APPE" "LIST" "NLST" "MLSD") :test #'string=)
        (setf (session-rest-offset session) 0)))))

;;; Helpers ----------------------------------------------------------------------

(defun session-path (session argument)
  "The components ARGUMENT names, from the session's directory."
  (parse-virtual-path (session-cwd session) (or argument "")))

(defun require-argument (argument)
  (unless (and argument (plusp (length argument)))
    (error 'vfs-error :message "A path is required.")))

(defun resolve-existing (session argument)
  "Resolve ARGUMENT for reading: (values KIND MAPPING HOST-PATH COMPONENTS)."
  (let ((components (session-path session argument)))
    (multiple-value-bind (kind mapping host-path)
        (resolve (session-vfs session) components :intent :existing)
      (values kind mapping host-path components))))

(defun resolve-for-change (session argument)
  "Resolve ARGUMENT as something to create, remove or rename:
(values MAPPING HOST-PATH COMPONENTS).  Refuses the root, a mapping itself,
and anything in a mapping that is not writable."
  (require-argument argument)
  (let ((components (session-path session argument)))
    (multiple-value-bind (kind mapping host-path)
        (resolve (session-vfs session) components :intent :leaf)
      (unless (eq kind :inside)
        (error 'vfs-denied))
      (unless (mapping-writable mapping)
        (error 'vfs-denied :message "This folder is read-only."))
      (values mapping host-path components))))

(defun entry-for (session argument)
  "The entry ARGUMENT names, with its components, or signal VFS-NOT-FOUND."
  (multiple-value-bind (kind mapping host-path components)
      (resolve-existing session argument)
    (values
     (ecase kind
       (:root (root-entry))
       (:mapping-root (mapping-entry mapping))
       (:inside (or (host-entry (first (last components)) host-path
                                :writable (mapping-writable mapping))
                    (error 'vfs-not-found))))
     components)))

(defun quote-path (string)
  "STRING in double quotes, with any it contains doubled, as 257 wants."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across string
          do (when (char= char #\") (write-char #\" out))
             (write-char char out))
    (write-char #\" out)))

;;; Logging in ---------------------------------------------------------------------

(define-command "USER" (session argument :login nil)
  ;; The same answer whoever it is: which names exist is not a client's to learn.
  (setf (session-state session) :need-password
        (session-user session) (or argument ""))
  (values 331 "Password required."))

(define-command "PASS" (session argument :login nil)
  (cond ((not (eq :need-password (session-state session)))
         (values 503 "Send USER first."))
        ((funcall (server-authenticator (session-server session))
                  (session-user session) (or argument ""))
         (setf (session-state session) :logged-in
               (session-cwd session) '())
         (server-log (session-server session) "~a logged in from ~a"
                     (session-user session)
                     (and (session-peer session) (address-string (session-peer session))))
         (values 230 "Logged in."))
        (t
         (setf (session-state session) :new)
         (server-log (session-server session) "login refused for ~a from ~a"
                     (session-user session)
                     (and (session-peer session) (address-string (session-peer session))))
         (sleep *login-failure-delay*)
         (values 530 "Login incorrect."))))

(define-command "QUIT" (session argument :login nil)
  (setf (session-quit-p session) t)
  (values 221 "Goodbye."))

;;; Saying what this is ---------------------------------------------------------------

(define-command "SYST" (session argument :login nil)
  (values 215 "UNIX Type: L8"))

(define-command "FEAT" (session argument :login nil)
  (values 211 '("Features:"
                " EPSV"
                " MDTM"
                " MLST type*;size*;modify*;perm*;"
                " REST STREAM"
                " SIZE"
                " UTF8"
                "End")))

(define-command "OPTS" (session argument :login nil)
  (if (and argument (string-equal "UTF8 ON" (string-trim " " argument)))
      (values 200 "UTF8 is on.")
      (values 501 "Option not understood.")))

(define-command "NOOP" (session argument :login nil)
  (values 200 "OK."))

(define-command "HELP" (session argument :login nil)
  (values 214 (list "The commands understood:"
                    (format nil " ~{~a~^ ~}"
                            (sort (loop for verb being the hash-keys of *commands*
                                        collect verb)
                                  #'string<))
                    "End")))

;;; Transfer parameters ---------------------------------------------------------------

(define-command "TYPE" (session argument)
  (let ((type (string-upcase (string-trim " " (or argument "")))))
    (cond ((member type '("A" "A N") :test #'string=)
           (setf (session-transfer-type session) :ascii)
           (values 200 "Type set to A."))
          ((member type '("I" "L 8") :test #'string=)
           (setf (session-transfer-type session) :image)
           (values 200 "Type set to I."))
          (t (values 504 "Type not supported.")))))

(define-command "MODE" (session argument)
  (if (string-equal "S" (string-trim " " (or argument "")))
      (values 200 "Mode set to S.")
      (values 504 "Only stream mode is supported.")))

(define-command "STRU" (session argument)
  (if (string-equal "F" (string-trim " " (or argument "")))
      (values 200 "Structure set to F.")
      (values 504 "Only file structure is supported.")))

(define-command "ALLO" (session argument)
  (values 202 "No storage allocation is needed."))

(define-command "REST" (session argument)
  (let ((offset (and argument
                     (ignore-errors (parse-integer argument)))))
    (cond ((and offset (>= offset 0))
           (setf (session-rest-offset session) offset)
           (values 350 (format nil "Restarting at ~d." offset)))
          (t (values 501 "REST wants a number of bytes.")))))

(define-command "ABOR" (session argument)
  ;; Commands are read one at a time, so by now there is no transfer to stop.
  (values 226 "No transfer in progress."))

(define-command "PORT" (session argument)
  (values 502 "Active mode is not supported; use PASV or EPSV."))

(define-command "EPRT" (session argument)
  (values 502 "Active mode is not supported; use PASV or EPSV."))

;;; Moving about ---------------------------------------------------------------------

(define-command "PWD" (session argument)
  (values 257 (format nil "~a is the current directory."
                      (quote-path (virtual-path-string (session-cwd session))))))

(define-command "XPWD" (session argument)
  (command-pwd session argument))

(define-command "CWD" (session argument)
  (multiple-value-bind (entry components) (entry-for session argument)
    (unless (eq :directory (entry-type entry))
      (error 'vfs-error :message "Not a directory."))
    (setf (session-cwd session) components)
    (values 250 "Directory changed.")))

(define-command "XCWD" (session argument)
  (command-cwd session argument))

(define-command "CDUP" (session argument)
  (setf (session-cwd session) (butlast (session-cwd session)))
  (values 250 "Directory changed."))

(define-command "XCUP" (session argument)
  (command-cdup session argument))

;;; Asking about one file -------------------------------------------------------------

(define-command "SIZE" (session argument)
  (require-argument argument)
  (let ((entry (entry-for session argument)))
    (unless (eq :file (entry-type entry))
      (error 'vfs-error :message "Not a plain file."))
    ;; SIZE is the number of octets a transfer would send, and in ASCII that
    ;; means reading the whole file to count its lines.  Refused instead, as
    ;; other servers do, rather than answered with a number that is wrong.
    (when (eq :ascii (session-transfer-type session))
      (error 'vfs-error :message "SIZE is not available in ASCII mode."))
    (values 213 (format nil "~d" (entry-size entry)))))

(define-command "MDTM" (session argument)
  (require-argument argument)
  (values 213 (format-timestamp (entry-mtime (entry-for session argument)))))

(define-command "MLST" (session argument)
  (multiple-value-bind (entry components) (entry-for session argument)
    (let ((path (virtual-path-string components)))
      (values 250 (list (format nil "Listing ~a" path)
                        (format nil " ~a~a" (format-mlsx-facts entry) path)
                        "End")))))

;;; Changing things ------------------------------------------------------------------

(define-command "DELE" (session argument)
  (multiple-value-bind (mapping host-path) (resolve-for-change session argument)
    (declare (ignore mapping))
    (case (host-file-type host-path)
      ((nil) (error 'vfs-not-found))
      (:directory (error 'vfs-error :message "That is a directory; use RMD.")))
    (sb-posix:unlink host-path)
    (values 250 "Deleted.")))

(define-command "MKD" (session argument)
  (multiple-value-bind (mapping host-path components)
      (resolve-for-change session argument)
    (declare (ignore mapping))
    (sb-posix:mkdir host-path #o755)
    (values 257 (format nil "~a created."
                        (quote-path (virtual-path-string components))))))

(define-command "XMKD" (session argument)
  (command-mkd session argument))

(define-command "RMD" (session argument)
  (multiple-value-bind (mapping host-path) (resolve-for-change session argument)
    (declare (ignore mapping))
    (unless (eq :directory (host-file-type host-path))
      (error 'vfs-error :message "Not a directory."))
    (sb-posix:rmdir host-path)
    (values 250 "Removed.")))

(define-command "XRMD" (session argument)
  (command-rmd session argument))

(define-command "RNFR" (session argument)
  (multiple-value-bind (mapping host-path) (resolve-for-change session argument)
    (unless (host-file-type host-path)
      (error 'vfs-not-found))
    (setf (session-rename-from session) (list mapping host-path))
    (values 350 "Ready for RNTO.")))

(define-command "RNTO" (session argument)
  (let ((from (session-rename-from session)))
    (if (null from)
        (values 503 "Send RNFR first.")
        (multiple-value-bind (mapping host-path) (resolve-for-change session argument)
          ;; A rename is one directory entry moving, and the host cannot move
          ;; one between two mapped directories that may be on two volumes.
          (unless (eq mapping (first from))
            (error 'vfs-error :message "Cannot rename from one mapped folder to another."))
          (sb-posix:rename (second from) host-path)
          (values 250 "Renamed.")))))
