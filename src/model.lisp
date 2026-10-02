;;;; model.lisp -- what the window shows and does, with no window in it.
;;;;
;;;; Everything here runs without Objective-C, so it is tested without a
;;;; display.  The window is left with reading fields and calling these.

(in-package #:ftp-server)

(defclass model ()
  ((vfs :initform (make-vfs) :reader model-vfs)
   (username :initform "" :accessor model-username)
   (password :initform "" :accessor model-password)
   (port :initform *default-port* :accessor model-port)
   (allow-remote :initform nil :accessor model-allow-remote
                 :documentation "Listen on every interface rather than loopback.")
   (bonjour-name :initform "" :accessor model-bonjour-name
                 :documentation "Empty for the computer's own name.")
   (start-at-launch :initform nil :accessor model-start-at-launch
                    :documentation "Start serving as soon as the window is up.")
   (server :initform nil :accessor model-server)))

;;; Settings ---------------------------------------------------------------------

(defun apply-settings (model settings)
  (setf (model-username model) (getf settings :username)
        (model-password model) (getf settings :password)
        (model-port model) (getf settings :port)
        (model-allow-remote model) (getf settings :allow-remote)
        (model-bonjour-name model) (getf settings :bonjour-name)
        (model-start-at-launch model) (getf settings :start-at-launch))
  (dolist (item (getf settings :mappings))
    ;; A mapping the file should not have had is dropped, not fatal.
    (handler-case (vfs-add (model-vfs model) (getf item :name) (getf item :path)
                           :writable (getf item :writable))
      (mapping-error () nil)))
  model)

(defun make-model (&optional (settings (default-settings)))
  (apply-settings (make-instance 'model) settings))

(defun model-settings (model)
  (list :version 1
        :username (model-username model)
        :password (model-password model)
        :port (model-port model)
        :allow-remote (model-allow-remote model)
        :bonjour-name (model-bonjour-name model)
        :start-at-launch (model-start-at-launch model)
        :mappings (mapcar (lambda (mapping)
                            (list :name (mapping-name mapping)
                                  :path (mapping-host-path mapping)
                                  :writable (mapping-writable mapping)))
                          (vfs-mappings (model-vfs model)))))

;;; Where the password is kept --------------------------------------------------------

(defstruct password-store
  "Somewhere other than the settings file to keep the password.  FETCH is a
function of no arguments answering it, or the empty string; STORE is a function
of the password.  Either may signal an error."
  fetch store)

(defvar *password-store* nil
  "NIL to keep the password in the settings file, which is what the tests and
anything without a keychain do, or a PASSWORD-STORE to keep it there instead.")

(defun model-save (model &optional (path (settings-file)))
  "Save the model's settings.  With a password store the password goes there
and the file is written without one, whether or not the store took it: a
password the keychain refused is not then left in a file instead.  Answers
whether the password was saved."
  (let ((settings (model-settings model))
        (saved t))
    (when *password-store*
      (setf (getf settings :password) ""
            saved (handler-case
                      (progn (funcall (password-store-store *password-store*)
                                      (model-password model))
                             t)
                    (error () nil))))
    (save-settings settings path)
    saved))

(defun model-load (&optional (path (settings-file)))
  "A model from the settings in PATH.  With a password store the password comes
from there -- unless the file has one, left by a version that kept it in the
file, which is then moved to the store and taken out of the file."
  (let* ((settings (load-settings path))
         (model (make-model settings)))
    (when *password-store*
      (if (string/= "" (getf settings :password))
          (model-save model path)
          (setf (model-password model)
                (handler-case (funcall (password-store-fetch *password-store*))
                  (error () "")))))
    model))

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
  "Remove the mapping in row INDEX.  True if there was one."
  (let ((mapping (model-mapping model index)))
    (and mapping (vfs-remove (model-vfs model) mapping))))

(defun model-rename-mapping (model index name)
  "Call the mapping in row INDEX NAME.  Answers (values T NIL), or
(values NIL MESSAGE) and leaves it as it was."
  (let ((mapping (model-mapping model index)))
    (if (null mapping)
        (values nil "There is no such mapping.")
        (handler-case (progn (vfs-rename (model-vfs model) mapping
                                         (string-trim " " name))
                             (values t nil))
          (mapping-error (condition)
            (values nil (mapping-error-message condition)))))))

(defun model-set-writable (model index flag)
  (let ((mapping (model-mapping model index)))
    (when mapping
      (setf (mapping-writable mapping) (and flag t))
      t)))

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

(defun model-start (model &key on-event)
  "Start the server with the model's settings.
Answers (values T NIL), or (values NIL MESSAGE)."
  (cond
    ((model-running-p model)
     (values nil "The server is already running."))
    ((or (string= "" (model-username model)) (string= "" (model-password model)))
     (values nil "Set a user name and a password first."))
    ((not (typep (model-port model) '(integer 1 65535)))
     (values nil "The port must be a number from 1 to 65535."))
    (t
     ;; What a session checks against is what was set when the server started.
     (let* ((username (model-username model))
            (password (model-password model))
            (server (make-server
                     :vfs (model-vfs model)
                     :authenticator (lambda (user pass)
                                      (let ((user-ok (constant-time-string= user username))
                                            (pass-ok (constant-time-string= pass password)))
                                        (and user-ok pass-ok)))
                     :addresses (if (model-allow-remote model)
                                    *any-addresses*
                                    *loopback-addresses*)
                     :port (model-port model)
                     :on-event on-event)))
       (handler-case
           (progn (start-server server)
                  (setf (model-server model) server)
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
