;;;; backend.lisp -- what a mapping holds, behind one set of generic functions.
;;;;
;;;; The commands ask a mapping's backend what is at a path, what a directory
;;;; holds, for a file's contents, and to make, remove or rename things.  Paths
;;;; here are the components after the mapping's own name, so () is the
;;;; mapping itself.  TLS, ASCII, REST, the activity log and the Writable flag
;;;; are all above this line, and every backend has them without asking.
;;;;
;;;; HOST-BACKEND is a directory on this host, and is everything the server did
;;;; before there were backends.  lisp-backend.lisp is the other one.

(in-package #:ftp-server)

(defgeneric backend-entry (backend mapping components)
  (:documentation "The entry for COMPONENTS, named as the client sees it.
Signals VFS-NOT-FOUND if there is nothing there."))

(defgeneric backend-list (backend mapping components)
  (:documentation "The entries of the directory at COMPONENTS."))

(defgeneric backend-open-input (backend mapping components)
  (:documentation "A stream of octets on the file at COMPONENTS, which
FILE-POSITION can move about in."))

(defgeneric backend-check-output (backend mapping components &key append offset)
  (:documentation "Signal a VFS-ERROR if a file may not be written at
COMPONENTS.  Asked before the data connection is waited for, so that a refusal
is a refusal and not a wasted transfer.")
  (:method (backend mapping components &key append offset)
    (declare (ignore backend mapping components append offset))
    nil))

(defgeneric backend-open-output (backend mapping components &key append offset)
  (:documentation "(values STREAM FINISH): a stream to write the file at
COMPONENTS to, and a function to call when the writing is over, with true if
the whole file arrived.  FINISH may signal a VFS-ERROR, which the client is
then told instead of that the transfer was complete."))

(defgeneric backend-exists-p (backend mapping components)
  (:documentation "Whether there is something at COMPONENTS, a symbolic link
counting as itself rather than what it leads to.")
  (:method (backend mapping components)
    (handler-case (and (backend-entry backend mapping components) t)
      (vfs-error () nil))))

(defun refuse-change ()
  (error 'vfs-denied :message "That cannot be changed here."))

(defgeneric backend-delete (backend mapping components)
  (:method (backend mapping components)
    (declare (ignore backend mapping components))
    (refuse-change)))

(defgeneric backend-make-directory (backend mapping components)
  (:method (backend mapping components)
    (declare (ignore backend mapping components))
    (refuse-change)))

(defgeneric backend-remove-directory (backend mapping components)
  (:method (backend mapping components)
    (declare (ignore backend mapping components))
    (refuse-change)))

(defgeneric backend-rename (backend mapping from to)
  (:method (backend mapping from to)
    (declare (ignore backend mapping from to))
    (refuse-change)))

(defgeneric backend-description (backend)
  (:documentation "A few words for the window to show in place of a folder.")
  (:method (backend)
    (declare (ignore backend))
    "(made by Lisp)"))

;;; A directory on this host ------------------------------------------------------------

(defclass host-backend () ()
  (:documentation "The directory at the mapping's HOST-PATH.  The rules about
links and staying inside it are HOST-PATH-IN's."))

(defvar *host-backend* (make-instance 'host-backend))

(defun backend-of (mapping)
  (or (mapping-backend mapping) *host-backend*))

(defun leaf-name (mapping components)
  (if components (first (last components)) (mapping-name mapping)))

(defmethod backend-entry ((backend host-backend) mapping components)
  (if (null components)
      (mapping-entry mapping)
      (or (host-entry (leaf-name mapping components)
                      (host-path-in mapping components)
                      :writable (mapping-writable mapping))
          (error 'vfs-not-found))))

(defmethod backend-list ((backend host-backend) mapping components)
  (let ((host-path (host-path-in mapping components)))
    (unless (eq :directory (host-file-type host-path))
      (error 'vfs-error :message "Not a directory."))
    (list-directory host-path
                    :root-real (real-path (mapping-host-path mapping))
                    :writable (mapping-writable mapping))))

(defun open-host-file (host-path flags &optional (mode #o644))
  "A stream of octets on HOST-PATH, opened with FLAGS and never through a
symbolic link."
  (let ((fd (sb-posix:open host-path (logior flags sb-posix:o-nofollow) mode)))
    (sb-sys:make-fd-stream fd
                           :input (not (logtest flags (logior sb-posix:o-wronly
                                                              sb-posix:o-rdwr)))
                           :output (logtest flags (logior sb-posix:o-wronly
                                                          sb-posix:o-rdwr))
                           :element-type '(unsigned-byte 8)
                           :buffering :full
                           :auto-close t)))

(defmethod backend-open-input ((backend host-backend) mapping components)
  (let ((host-path (host-path-in mapping components)))
    (unless (and components (eq :file (host-file-type host-path)))
      (error 'vfs-error :message "Not a plain file."))
    ;; HOST-PATH is already a real path, so there is no link left to follow.
    (open-host-file host-path sb-posix:o-rdonly)))

(defmethod backend-check-output ((backend host-backend) mapping components &key append offset)
  (declare (ignore append offset))
  (when (member (host-file-type (host-path-in mapping components :intent :leaf))
                '(:directory :symlink :other))
    (error 'vfs-denied)))

(defmethod backend-open-output ((backend host-backend) mapping components &key append offset)
  (backend-check-output backend mapping components)
  (let* ((host-path (host-path-in mapping components :intent :leaf))
         (offset (or offset 0))
         (file (open-host-file host-path
                               (logior sb-posix:o-wronly sb-posix:o-creat
                                       (cond (append sb-posix:o-append)
                                             ((plusp offset) 0)
                                             (t sb-posix:o-trunc))))))
    (when (and (plusp offset) (not append))
      (file-position file offset))
    ;; What arrived of an interrupted upload is kept, as it would be by cp:
    ;; REST is how a client goes on from where it stopped.
    (values file (lambda (completed) (declare (ignore completed)) (close file)))))

(defmethod backend-exists-p ((backend host-backend) mapping components)
  (and (host-file-type (host-path-in mapping components :intent :leaf)) t))

(defmethod backend-delete ((backend host-backend) mapping components)
  (let ((host-path (host-path-in mapping components :intent :leaf)))
    (case (host-file-type host-path)
      ((nil) (error 'vfs-not-found))
      (:directory (error 'vfs-error :message "That is a directory; use RMD.")))
    (sb-posix:unlink host-path)))

(defmethod backend-make-directory ((backend host-backend) mapping components)
  (sb-posix:mkdir (host-path-in mapping components :intent :leaf) #o755))

(defmethod backend-remove-directory ((backend host-backend) mapping components)
  (let ((host-path (host-path-in mapping components :intent :leaf)))
    (unless (eq :directory (host-file-type host-path))
      (error 'vfs-error :message "Not a directory."))
    (sb-posix:rmdir host-path)))

(defmethod backend-rename ((backend host-backend) mapping from to)
  (sb-posix:rename (host-path-in mapping from :intent :leaf)
                   (host-path-in mapping to :intent :leaf)))

;;; The root ---------------------------------------------------------------------------

(defun root-entries (vfs)
  "The root's entries: one directory for each mapping, whether or not what is
behind it can be reached just now."
  (mapcar (lambda (mapping)
            (handler-case (backend-entry (backend-of mapping) mapping '())
              (vfs-error ()
                (make-entry :name (mapping-name mapping) :type :directory
                            :mode #o555 :links 2 :mtime (get-universal-time)
                            :writable (mapping-writable mapping)))))
          (vfs-mappings vfs)))

;;; Streams over octets in memory ---------------------------------------------------------
;;;
;;; What a backend that makes its files gives the commands to read, and takes
;;; an upload into: the same streams of octets a file on disk would be.

(defclass octet-input-stream (sb-gray:fundamental-binary-input-stream)
  ((octets :initarg :octets :reader stream-octets)
   (position :initform 0 :accessor stream-index)))

(defun make-octet-input-stream (octets)
  (make-instance 'octet-input-stream
                 :octets (coerce octets '(simple-array (unsigned-byte 8) (*)))))

(defmethod stream-element-type ((stream octet-input-stream)) '(unsigned-byte 8))

(defmethod sb-gray:stream-read-byte ((stream octet-input-stream))
  (let ((octets (stream-octets stream))
        (index (stream-index stream)))
    (if (< index (length octets))
        (prog1 (aref octets index) (incf (stream-index stream)))
        :eof)))

(defmethod sb-gray:stream-read-sequence ((stream octet-input-stream) sequence
                                         &optional (start 0) end)
  (let* ((octets (stream-octets stream))
         (index (stream-index stream))
         (end (or end (length sequence)))
         (count (min (- end start) (- (length octets) index))))
    (replace sequence octets :start1 start :end1 (+ start count) :start2 index)
    (incf (stream-index stream) count)
    (+ start count)))

(defmethod sb-gray:stream-file-position ((stream octet-input-stream) &optional position)
  (if position
      (setf (stream-index stream)
            (min (length (stream-octets stream))
                 (case position
                   (:start 0)
                   (:end (length (stream-octets stream)))
                   (t position))))
      (stream-index stream)))

(defclass octet-output-stream (sb-gray:fundamental-binary-output-stream)
  ((octets :initform (make-array 0 :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0)
           :reader stream-octets)))

(defmethod stream-element-type ((stream octet-output-stream)) '(unsigned-byte 8))

(defmethod sb-gray:stream-write-byte ((stream octet-output-stream) octet)
  (vector-push-extend octet (stream-octets stream))
  octet)

(defmethod sb-gray:stream-write-sequence ((stream octet-output-stream) sequence
                                          &optional (start 0) end)
  (loop for index from start below (or end (length sequence))
        do (vector-push-extend (elt sequence index) (stream-octets stream)))
  sequence)

(defun output-stream-octets (stream)
  "What has been written to STREAM, as a simple vector of octets."
  (coerce (stream-octets stream) '(simple-array (unsigned-byte 8) (*))))
