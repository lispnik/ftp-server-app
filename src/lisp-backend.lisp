;;;; lisp-backend.lisp -- files and directories made by Lisp as they are asked for.
;;;;
;;;; A mapping whose backend is a tree of nodes:
;;;;
;;;;   (lisp-file NAME CONTENT)        CONTENT is a string, octets, or a function
;;;;                                   of no arguments answering either, called
;;;;                                   each time the file is listed or read
;;;;   (lisp-directory NAME CHILDREN)  CHILDREN is a list of nodes, or a function
;;;;                                   answering one, called each time the
;;;;                                   directory is looked into
;;;;
;;;; A directory with :ON-UPLOAD takes uploads, if its mapping is writable: the
;;;; function is called with the file's name and its octets once the whole of
;;;; it has arrived.  Nothing in this backend can be deleted, made or renamed.
;;;;
;;;; The functions are called on the server's threads, one for each client, so
;;;; anything they share they must lock.  What they signal is the client's 550,
;;;; with what was signalled as its reason, and the server goes on.

(in-package #:ftp-server)

(defstruct (lisp-node (:constructor nil))
  (name "" :type string)
  (mtime nil))                         ; a universal time, a function, or NIL for now

(defstruct (lisp-file (:include lisp-node)
                      (:constructor %make-lisp-file (name content mtime)))
  content)

(defstruct (lisp-directory (:include lisp-node)
                           (:constructor %make-lisp-directory
                               (name children mtime on-upload)))
  children
  on-upload)

(defun check-node-name (name)
  (unless (valid-mapping-name-p name)
    (error "~s cannot be the name of a file or directory." name))
  name)

(defun lisp-file (name content &key mtime)
  "A file called NAME whose contents are CONTENT: a string, which is sent as
UTF-8, a vector of octets, or a function of no arguments that answers one of
those each time the file is wanted.  MTIME is a universal time, or a function
answering one; by default the file is as new as the moment it is asked about."
  (%make-lisp-file (check-node-name name) content mtime))

(defun lisp-directory (name children &key mtime on-upload)
  "A directory called NAME holding CHILDREN: a list of LISP-FILE and
LISP-DIRECTORY nodes, or a function of no arguments that answers such a list
each time the directory is looked into.  ON-UPLOAD, a function of a file name
and a vector of octets, makes the directory one that can be uploaded to."
  (%make-lisp-directory (check-node-name name) children mtime on-upload))

(defclass lisp-backend ()
  ((root :initarg :root :reader lisp-backend-root
         :documentation "The LISP-DIRECTORY that is the mapping itself; its
name is not used, the mapping's is.")
   (description :initarg :description :initform "(made by Lisp)"
                :reader backend-description)))

(defun make-lisp-backend (root &key (description "(made by Lisp)"))
  (check-type root lisp-directory)
  (make-instance 'lisp-backend :root root :description description))

(defun vfs-add-lisp (vfs name root &key writable (description "(made by Lisp)"))
  "Map NAME to the tree whose root is the LISP-DIRECTORY ROOT."
  (vfs-add-backend vfs name (make-lisp-backend root :description description)
                   :writable writable))

;;; Calling what the tree was made with ----------------------------------------------------

(defmacro generating ((what) &body body)
  "Run BODY, which calls a function the tree was made with.  An error from it
is the client's: a VFS-ERROR saying so, which is 550 and not the end of the
session."
  `(handler-case (progn ,@body)
     (vfs-error (condition) (error condition))
     (error (condition)
       (error 'vfs-error :message (format nil "~a could not be made: ~a" ,what condition)))))

(defun node-children (directory)
  "DIRECTORY's children as they are now: made, checked, and with no two of
the same name, the first of them winning."
  (let ((children (lisp-directory-children directory)))
    (when (functionp children)
      (setf children (generating ((lisp-node-name directory))
                       (funcall children))))
    (let ((seen '()))
      (loop for child in children
            when (and (lisp-node-p child)
                      (valid-mapping-name-p (lisp-node-name child))
                      (not (member (lisp-node-name child) seen :test #'string=)))
              collect (progn (push (lisp-node-name child) seen) child)))))

(defun file-octets (file)
  "FILE's contents as they are now, as a vector of octets."
  (let ((content (lisp-file-content file)))
    (when (functionp content)
      (setf content (generating ((lisp-node-name file)) (funcall content))))
    (typecase content
      (string (line-to-octets content))
      ((vector (unsigned-byte 8)) (coerce content '(simple-array (unsigned-byte 8) (*))))
      (vector (handler-case (coerce content '(simple-array (unsigned-byte 8) (*)))
                (error ()
                  (error 'vfs-error
                         :message (format nil "~a is not text or octets."
                                          (lisp-node-name file))))))
      (null (make-array 0 :element-type '(unsigned-byte 8)))
      (t (error 'vfs-error
                :message (format nil "~a is not text or octets." (lisp-node-name file)))))))

(defun node-mtime (node)
  (let ((mtime (lisp-node-mtime node)))
    (or (if (functionp mtime)
            (generating ((lisp-node-name node)) (funcall mtime))
            mtime)
        (get-universal-time))))

(defun node-at (backend components)
  "The node at COMPONENTS, or signal VFS-NOT-FOUND."
  (let ((node (lisp-backend-root backend)))
    (dolist (name components node)
      (unless (lisp-directory-p node)
        (error 'vfs-not-found))
      (setf node (or (find name (node-children node) :key #'lisp-node-name :test #'string=)
                     (error 'vfs-not-found))))))

(defun node-entry (node name mapping)
  "The entry a client sees for NODE, called NAME."
  (if (lisp-directory-p node)
      (let ((writable (and (mapping-writable mapping) (lisp-directory-on-upload node) t)))
        (make-entry :name name :type :directory :size 0 :links 2
                    :mode (if writable #o755 #o555)
                    :mtime (node-mtime node) :writable writable))
      (make-entry :name name :type :file :size (length (file-octets node)) :links 1
                  :mode #o444 :mtime (node-mtime node) :writable nil)))

;;; The protocol ------------------------------------------------------------------------

(defmethod backend-entry ((backend lisp-backend) mapping components)
  (node-entry (node-at backend components) (leaf-name mapping components) mapping))

(defmethod backend-list ((backend lisp-backend) mapping components)
  (let ((node (node-at backend components)))
    (unless (lisp-directory-p node)
      (error 'vfs-error :message "Not a directory."))
    (loop for child in (node-children node)
          ;; One child that cannot be made is left out, not the whole listing.
          for entry = (handler-case (node-entry child (lisp-node-name child) mapping)
                        (vfs-error () nil))
          when entry collect entry)))

(defmethod backend-open-input ((backend lisp-backend) mapping components)
  (let ((node (node-at backend components)))
    (unless (lisp-file-p node)
      (error 'vfs-error :message "Not a plain file."))
    (make-octet-input-stream (file-octets node))))

(defun upload-directory (backend components)
  "The directory an upload to COMPONENTS goes to, if it takes uploads."
  (let ((parent (node-at backend (butlast components))))
    (unless (and (lisp-directory-p parent) (lisp-directory-on-upload parent))
      (error 'vfs-denied :message "This directory does not take uploads."))
    (let ((existing (find (first (last components)) (node-children parent)
                          :key #'lisp-node-name :test #'string=)))
      (when (lisp-directory-p existing)
        (error 'vfs-denied :message "That is a directory.")))
    parent))

(defmethod backend-check-output ((backend lisp-backend) mapping components &key append offset)
  (declare (ignore mapping))
  (upload-directory backend components)
  ;; An upload is handed over whole, so there is nothing to add to or go on with.
  (when (or append (and offset (plusp offset)))
    (error 'vfs-denied :message "Appending and resuming are not supported here.")))

(defmethod backend-open-output ((backend lisp-backend) mapping components &key append offset)
  (backend-check-output backend mapping components :append append :offset offset)
  (let ((directory (upload-directory backend components))
        (name (first (last components)))
        (stream (make-instance 'octet-output-stream)))
    (values stream
            (lambda (completed)
              ;; Only a whole upload: half a file is not handed to anybody.
              (when completed
                (generating (name)
                  (funcall (lisp-directory-on-upload directory)
                           name (output-stream-octets stream))))))))
