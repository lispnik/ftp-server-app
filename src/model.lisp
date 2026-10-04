;;;; model.lisp -- what the window shows and does, with no window in it.
;;;;
;;;; Everything here runs without Objective-C, so it is tested without a
;;;; display.  The window is left with reading fields and calling these.

(in-package #:ftp-server)

(defclass model ()
  ((vfs :initform (make-vfs) :reader model-vfs)
   (accounts :initform (make-accounts) :reader model-accounts)
   (port :initform *default-port* :accessor model-port)
   (allow-remote :initform nil :accessor model-allow-remote
                 :documentation "Listen on every interface rather than loopback.")
   (bonjour-name :initform "" :accessor model-bonjour-name
                 :documentation "Empty for the computer's own name.")
   (start-at-launch :initform nil :accessor model-start-at-launch
                    :documentation "Start serving as soon as the window is up.")
   (require-tls :initform nil :accessor model-require-tls
                :documentation "Refuse clients that do not encrypt.")
   (tls-description :initform nil :accessor model-tls-description
                    :documentation "While running with TLS: a sentence about
the certificate in use.")
   (server :initform nil :accessor model-server)))

;;; Settings ---------------------------------------------------------------------

(defun apply-settings (model settings)
  (setf (model-port model) (getf settings :port)
        (model-allow-remote model) (getf settings :allow-remote)
        (model-bonjour-name model) (getf settings :bonjour-name)
        (model-start-at-launch model) (getf settings :start-at-launch)
        (model-require-tls model) (getf settings :require-tls))
  (dolist (item (getf settings :mappings))
    ;; A mapping the file should not have had is dropped, not fatal.
    (handler-case (vfs-add (model-vfs model) (getf item :name) (getf item :path))
      (mapping-error () nil)))
  (dolist (item (getf settings :users))
    (handler-case (accounts-add (model-accounts model) (getf item :name)
                                :password (getf item :password)
                                :access (getf item :access))
      (account-error () nil)))
  model)

(defun make-model (&optional (settings (default-settings)))
  (apply-settings (make-instance 'model) settings))

(defun model-users (model)
  (accounts-users (model-accounts model)))

(defun model-settings (model)
  (list :version 2
        :port (model-port model)
        :allow-remote (model-allow-remote model)
        :bonjour-name (model-bonjour-name model)
        :start-at-launch (model-start-at-launch model)
        :require-tls (model-require-tls model)
        ;; Only folders.  What init.lisp defines it defines again at each
        ;; launch, and a function cannot be written to a file.
        :mappings (loop for mapping in (vfs-mappings (model-vfs model))
                        when (host-mapping-p mapping)
                          collect (list :name (mapping-name mapping)
                                        :path (mapping-host-path mapping)))
        ;; Grants on a Lisp mapping are kept, by its name, for when init.lisp
        ;; makes it again.
        :users (loop for user in (model-users model)
                     collect (list :name (user-name user)
                                   :password (user-password user)
                                   :access (copy-alist (user-grants user))))))

;;; Where passwords are kept -----------------------------------------------------------

(defstruct password-store
  "Somewhere other than the settings file to keep passwords, one for each
user.  FETCH is a function of a user name answering their password, or the
empty string; STORE is a function of a user name and a password; FORGET of a
user name.  Any of them may signal an error."
  fetch store forget)

(defvar *password-store* nil
  "NIL to keep passwords in the settings file, which is what the tests and
anything without a keychain do, or a PASSWORD-STORE to keep them there instead.")

(defun model-save (model &optional (path (settings-file)))
  "Save the model's settings.  With a password store the passwords go there
and the file is written without them, whether or not the store took them: a
password the keychain refused is not then left in a file instead.  Answers
whether every password was saved."
  (let ((settings (model-settings model))
        (saved t))
    (when *password-store*
      (dolist (user (getf settings :users))
        (unless (handler-case
                    (progn (funcall (password-store-store *password-store*)
                                    (getf user :name) (getf user :password))
                           t)
                  (error () nil))
          (setf saved nil))
        (setf (getf user :password) "")))
    (save-settings settings path)
    saved))

(defun model-load (&optional (path (settings-file)))
  "A model from the settings in PATH.  With a password store the passwords
come from there -- unless the file has them, left by a version that kept them
in the file, when they are moved to the store and taken out of the file."
  (let* ((settings (load-settings path))
         (model (make-model settings)))
    (when *password-store*
      (if (some (lambda (user) (string/= "" (getf user :password)))
                (getf settings :users))
          (model-save model path)
          (dolist (user (model-users model))
            (accounts-set-password (model-accounts model) user
                                   (handler-case
                                       (funcall (password-store-fetch *password-store*)
                                                (user-name user))
                                     (error () ""))))))
    model))

(defun forget-password (name)
  "Take NAME's password out of the password store, if there is one."
  (when *password-store*
    (ignore-errors (funcall (password-store-forget *password-store*) name))))

;;; Users ------------------------------------------------------------------------

(defun model-user (model index)
  "The user in row INDEX, or NIL."
  (and (integerp index) (>= index 0)
       (nth index (model-users model))))

(defun model-add-user (model &optional (base "user"))
  "Add a user with no password and no access, named BASE or BASE with a number
after it.  Answers the user."
  (let ((accounts (model-accounts model)))
    (accounts-add accounts
                  (if (accounts-find-equal accounts base)
                      (loop for number from 2
                            for candidate = (format nil "~a~d" base number)
                            unless (accounts-find-equal accounts candidate)
                              return candidate)
                      base))))

(defun accounts-find-equal (accounts name)
  (find name (accounts-users accounts) :key #'user-name :test #'string-equal))

(defun model-remove-user (model user)
  "Remove USER, and their password from wherever it is kept."
  (when (accounts-remove (model-accounts model) user)
    (forget-password (user-name user))
    t))

(defun model-rename-user (model user name)
  "Call USER NAME.  Answers (values T NIL), or (values NIL MESSAGE) and
leaves them as they were."
  (let ((old (user-name user))
        (name (string-trim " " name)))
    (handler-case
        (progn (accounts-rename (model-accounts model) user name)
               ;; The password is kept under the name, so it moves with it.
               (unless (string= old name)
                 (forget-password old))
               (values t nil))
      (account-error (condition)
        (values nil (account-error-message condition))))))

(defun model-set-password (model user password)
  (accounts-set-password (model-accounts model) user password))

(defun model-access (model user mapping)
  "USER's level on MAPPING: NIL, :READ or :READ-WRITE."
  (user-access (model-accounts model) user (mapping-name mapping)))

(defun model-set-access (model user mapping level)
  (setf (user-access (model-accounts model) user (mapping-name mapping)) level))

;;; Mappings ---------------------------------------------------------------------

(defun model-mappings (model)
  (vfs-mappings (model-vfs model)))

(defun model-mapping (model index)
  "The mapping in row INDEX, or NIL."
  (and (integerp index) (>= index 0)
       (nth index (model-mappings model))))

(defun default-mapping-name (host-path)
  "A name for HOST-PATH: its last component, or \"root\" for /."
  (let* ((trimmed (string-right-trim "/" host-path))
         (slash (position #\/ trimmed :from-end t))
         (name (string-trim " " (subseq trimmed (if slash (1+ slash) 0)))))
    (if (valid-mapping-name-p name) name "root")))

(defun unique-mapping-name (vfs name)
  "NAME, or NAME with the first number after it that no mapping has."
  (flet ((taken-p (candidate)
           (find candidate (vfs-mappings vfs) :key #'mapping-name :test #'string-equal)))
    (if (not (taken-p name))
        name
        (loop for number from 2
              for candidate = (format nil "~a-~d" name number)
              unless (taken-p candidate) return candidate))))

(defun model-add-directory (model host-path)
  "Map the directory HOST-PATH under a name made from it.
Answers (values MAPPING NIL), or (values NIL MESSAGE)."
  (let ((real (real-path host-path)))
    (if (not (and real (eq :directory (host-file-type real))))
        (values nil (format nil "~a is not a folder." host-path))
        (let ((vfs (model-vfs model)))
          (values (vfs-add vfs (unique-mapping-name vfs (default-mapping-name host-path))
                           host-path)
                  nil)))))

(defun model-remove-mapping (model index)
  "Remove the mapping in row INDEX, and what every user had on it.  True if
there was one."
  (let ((mapping (model-mapping model index)))
    (when (and mapping (vfs-remove (model-vfs model) mapping))
      (accounts-forget-mapping (model-accounts model) (mapping-name mapping))
      t)))

(defun model-rename-mapping (model index name)
  "Call the mapping in row INDEX NAME.  Answers (values T NIL), or
(values NIL MESSAGE) and leaves it as it was."
  (let ((mapping (model-mapping model index)))
    (if (null mapping)
        (values nil "There is no such mapping.")
        (let ((old (mapping-name mapping)))
          (handler-case (progn (vfs-rename (model-vfs model) mapping
                                           (string-trim " " name))
                               ;; What users had on it, they have still.
                               (accounts-rename-mapping (model-accounts model)
                                                        old (mapping-name mapping))
                               (values t nil))
            (mapping-error (condition)
              (values nil (mapping-error-message condition))))))))

;;; The server -------------------------------------------------------------------

(defun parse-port (string)
  "The port STRING names, or NIL."
  (let ((port (ignore-errors (parse-integer (string-trim " " string)))))
    (and port (<= 1 port 65535) port)))

(defun model-running-p (model)
  (let ((server (model-server model)))
    (and server (server-running-p server) t)))

(defun model-advertise-p (model)
  "Whether the server should be announced with Bonjour: only when it can be
reached from another computer.  Announcing a loopback server would put a
service in every browser on the network that none of them could open."
  (and (model-allow-remote model) t))

(defvar *tls-maker* nil
  "NIL for a server with no TLS, or a function of no arguments that answers
what MAKE-SERVER's :TLS wants and, as a second value, a sentence about the
certificate.  It may signal an error.  The application sets this; the server
itself knows nothing of any TLS library.")

(defun model-start (model &key on-event)
  "Start the server with the model's settings.
Answers (values T NIL), or (values NIL MESSAGE)."
  (cond
    ((model-running-p model)
     (values nil "The server is already running."))
    ((notany (lambda (user) (string/= "" (user-password user))) (model-users model))
     (values nil "Add a user with a password first, in Users…"))
    ((not (typep (model-port model) '(integer 1 65535)))
     (values nil "The port must be a number from 1 to 65535."))
    (t
     ;; Users, their passwords and what they may do are asked for at each
     ;; login and each command, so changes apply to a running server.
     (let* ((tls-problem nil)
            (tls-description nil)
            (tls (and *tls-maker*
                      (handler-case
                          (multiple-value-bind (tls description) (funcall *tls-maker*)
                            (setf tls-description description)
                            tls)
                        (error (condition)
                          (setf tls-problem (princ-to-string condition))
                          nil))))
            (server (make-server
                     :tls tls
                     :require-tls (model-require-tls model)
                     :vfs (model-vfs model)
                     :authenticator (lambda (name password)
                                      (accounts-authenticate (model-accounts model)
                                                             name password))
                     :access (lambda (name mapping)
                               (accounts-access (model-accounts model) name mapping))
                     :addresses (if (model-allow-remote model)
                                    *any-addresses*
                                    *loopback-addresses*)
                     :port (model-port model)
                     :on-event on-event)))
       (when (and (model-require-tls model) (null tls))
         (return-from model-start
           (values nil (format nil "TLS is required but is not available~@[: ~a~]"
                               tls-problem))))
       (handler-case
           (progn (start-server server)
                  (setf (model-server model) server
                        (model-tls-description model)
                        (or tls-description
                            (and tls-problem
                                 (format nil "TLS is not available: ~a" tls-problem))))
                  (values t nil))
         (sb-bsd-sockets:address-in-use-error ()
           (values nil (format nil "Port ~d is already in use." (model-port model))))
         (sb-bsd-sockets:socket-error (condition)
           (values nil (format nil "Could not listen on port ~d: ~a"
                               (model-port model) condition)))
         (error (condition)
           (values nil (princ-to-string condition))))))))

(defun model-stop (model)
  "Stop the server, if it is running."
  (let ((server (shiftf (model-server model) nil)))
    (when server
      (stop-server server)
      t)))

(defun model-status-text (model)
  (let ((server (model-server model)))
    (if (not (and server (server-running-p server)))
        "Stopped."
        (let ((clients (server-session-count server)))
          (format nil "Running on ~a, port ~d.  ~[No clients~;1 client~:;~:*~d clients~] connected."
                  (if (model-allow-remote model) "all interfaces" "this computer only")
                  (server-port server)
                  clients)))))

;;; The activity log -------------------------------------------------------------------
;;;
;;; What the window's activity pane is a table of.  Here, and not with the
;;; window, because what a row says and what order the rows go in need no
;;; window to decide or to test.

(defstruct activity-entry
  (sequence 0)                          ; the order they happened in
  (time 0)                              ; a universal time
  (user nil)                            ; a string, or NIL
  (address nil)                         ; a vector of octets, or NIL for the server itself
  (message ""))

(defparameter *activity-columns* '("time" "user" "address" "message")
  "The columns of the activity pane, in the order they are shown.")

(defun format-activity-time (time)
  "TIME as the pane shows it: the date and the time of day, here."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(defun activity-cell (entry column)
  "What ENTRY shows in COLUMN, one of *ACTIVITY-COLUMNS*.

A client that has not logged in is \"anon\".  A line that is the server's own,
about no client at all, has a dash for both the user and the address."
  (let ((address (activity-entry-address entry))
        (user (activity-entry-user entry)))
    (cond ((string= column "time") (format-activity-time (activity-entry-time entry)))
          ((string= column "user")
           (cond ((and user (plusp (length user))) user)
                 (address "anon")
                 (t "-")))
          ((string= column "address")
           (if address (address-string address) "-"))
          (t (activity-entry-message entry)))))

(defun address-before-p (a b)
  "Whether address A sorts before B: none before any, IPv4 before IPv6, and
otherwise by number, so that 10.0.0.9 is before 10.0.0.10."
  (cond ((null a) (and b t))
        ((null b) nil)
        ((/= (length a) (length b)) (< (length a) (length b)))
        (t (let ((differ (mismatch a b)))
             (and differ (< (aref a differ) (aref b differ)))))))

(defun sort-activity (entries column ascending)
  "ENTRIES in order of COLUMN, as a fresh list.  Rows that are the same in
that column stay in the order they happened, whichever way the sort runs --
except by time, where the order they happened is the order: two rows in the
same second are still one before the other, and turn round with the rest."
  (let* ((before
           (cond ((string= column "time")
                  (lambda (a b)
                    (or (< (activity-entry-time a) (activity-entry-time b))
                        (and (= (activity-entry-time a) (activity-entry-time b))
                             (< (activity-entry-sequence a)
                                (activity-entry-sequence b))))))
                 ((string= column "address")
                  (lambda (a b) (address-before-p (activity-entry-address a)
                                                  (activity-entry-address b))))
                 (t
                  (lambda (a b) (string-lessp (activity-cell a column)
                                              (activity-cell b column))))))
         (ordered (if ascending
                      before
                      (lambda (a b) (funcall before b a)))))
    (stable-sort (sort (copy-list entries) #'< :key #'activity-entry-sequence)
                 ordered)))

(defun activity-text (entries)
  "ENTRIES as text to paste somewhere: a line each, the columns with tabs
between, which is what a spreadsheet wants."
  (format nil "~{~a~%~}"
          (mapcar (lambda (entry)
                    (format nil "~{~a~^~c~}"
                            (loop for (column . more) on *activity-columns*
                                  collect (activity-cell entry column)
                                  when more collect #\Tab)))
                  entries)))

;;; init.lisp ---------------------------------------------------------------------------
;;;
;;; Lisp, loaded when the application starts, from beside the settings.  It is
;;; how a mapping made by Lisp gets into the application: the window can only
;;; choose folders.  It is code, and it runs as you, with everything you can
;;; do -- the same as a shell's startup file.

(defvar *init-mappings* '()
  "What DEFINE-LISP-MAPPING has been told during this load of init.lisp,
newest first.")

(defun define-lisp-mapping (name root &key (description "(made by init.lisp)"))
  "In init.lisp: map NAME to the tree whose root is the LISP-DIRECTORY ROOT.
Who may see it, and upload to the directories in it that take uploads, is set
for each user in the Users window, as for any mapping."
  (check-type root lisp-directory)
  (push (list name root description) *init-mappings*)
  name)

(defun init-file ()
  "Where init.lisp is: beside the settings file."
  (merge-pathnames "init.lisp" (settings-directory)))

(defun error-line (text)
  "The line number SBCL's report of where a form went wrong gives, or NIL."
  (let ((at (search "starting at line " text)))
    (and at (parse-integer text :start (+ at (length "starting at line "))
                                :junk-allowed t))))

(defun load-init-file (&optional (path (init-file)))
  "Load init.lisp, if there is one.  Answers (values MAPPINGS PROBLEM):
MAPPINGS each (NAME ROOT DESCRIPTION) in the order they were defined, and
PROBLEM a sentence if loading it went wrong, saying on which line.  What was
defined before an error is kept."
  (let ((*init-mappings* '())
        (said (make-string-output-stream)))
    (if (not (probe-file path))
        (values '() nil)
        (handler-case
            (let ((*package* (find-package '#:ftp-server))
                  (*read-eval* t)
                  ;; SBCL says where a form failed here; it goes in PROBLEM.
                  (*error-output* said))
              ;; A unit of its own, so that what the compiler has to say about
              ;; it is said now and not when the program ends; and style
              ;; warnings, which are about style, not said at all.
              (with-compilation-unit (:override t)
                (handler-bind ((style-warning #'muffle-warning))
                  (load path :external-format :utf-8 :verbose nil :print nil)))
              (values (reverse *init-mappings*) nil))
          (error (condition)
            (let ((line (error-line (get-output-stream-string said))))
              (values (reverse *init-mappings*)
                      (format nil "init.lisp~@[, line ~d~]: ~a" line condition))))))))

(defun model-add-lisp-mappings (model mappings)
  "Add MAPPINGS, as LOAD-INIT-FILE answers them, to MODEL.  Answers a sentence
for each one, saying what became of it."
  (loop for (name root description) in mappings
        collect (handler-case
                    (progn (vfs-add-lisp (model-vfs model) name root
                                         :description description)
                           (format nil "init.lisp mapped ~a" name))
                  (mapping-error (condition)
                    (format nil "init.lisp could not map ~a: ~a"
                            name (mapping-error-message condition))))))
