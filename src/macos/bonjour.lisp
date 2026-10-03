;;;; bonjour.lisp -- announcing the server on the local network.
;;;;
;;;; NSNetService hands the announcement to the system's own mDNS responder,
;;;; which is already running and already answers for this computer's name.
;;;; The service is _ftp._tcp, the type registered for FTP, which is what an
;;;; FTP client with a Bonjour browser looks for.  Finder is not one: it lists
;;;; file servers (SMB, AFP) and screens to share, and macOS 26's libraries
;;;; and Finder itself do not so much as name _ftp._tcp.
;;;;
;;;; Main thread only: the service reports back through the run loop it was
;;;; made on.

(in-package #:ftp-server)

(defparameter +bonjour-type+ "_ftp._tcp.")

(defvar *bonjour-service* nil "The NSNetService being published, or NIL.")
(defvar *bonjour-delegate* nil)

(defvar *bonjour-status* nil
  "NIL when nothing is announced; :PUBLISHING while the responder is being
asked; (:PUBLISHED name) once it has agreed, with the name it settled on; or
(:FAILED code) with NSNetService's error code.")

(defvar *bonjour-changed* nil
  "A function to call, with no arguments, when *BONJOUR-STATUS* changes.")

(defun set-bonjour-status (status)
  (setf *bonjour-status* status)
  (when *bonjour-changed*
    (funcall *bonjour-changed*)))

(objc:define-objc-class bonjour-delegate ()
  ()
  (:objc-class-name "FTPServerBonjourDelegate"))

(objc:define-objc-method ("netServiceDidPublish:" :void)
    ((self bonjour-delegate) (service objc:objc-object-pointer))
  (declare (ignorable self))
  (handler-case
      ;; The name is asked for rather than remembered: if another service on
      ;; the network had it first, the responder has chosen a different one.
      (let ((name (objc:invoke-into 'string service "name")))
        (note "Bonjour: published as ~s" name)
        (set-bonjour-status (list :published name)))
    (error (condition) (note "netServiceDidPublish: ~a" condition))))

(objc:define-objc-method ("netService:didNotPublish:" :void)
    ((self bonjour-delegate)
     (service objc:objc-object-pointer)
     (errors objc:objc-object-pointer))
  (declare (ignorable self service))
  (handler-case
      (let* ((number (and (not (null-object-p errors))
                          (objc:invoke errors "objectForKey:" "NSNetServicesErrorCode")))
             (code (if (null-object-p number) 0 (objc:invoke number "integerValue"))))
        (note "Bonjour: not published, error ~d" code)
        (set-bonjour-status (list :failed code)))
    (error (condition) (note "netService:didNotPublish: ~a" condition))))

(defun bonjour-unpublish ()
  "Withdraw the announcement, if there is one."
  (let ((service (shiftf *bonjour-service* nil)))
    (when service
      (ignore-errors
       (objc:invoke service "setDelegate:" (cffi:null-pointer))
       (objc:invoke service "stop")
       (objc:release service))
      (note "Bonjour: withdrawn")))
  (set-bonjour-status nil))

(defun bonjour-publish (port &key (name ""))
  "Announce an FTP server on PORT.  NAME empty means the computer's own name.
The answer comes later, in *BONJOUR-STATUS*."
  (objc.runloop:check-main-thread "Bonjour")
  (bonjour-unpublish)
  (unless *bonjour-delegate*
    (setf *bonjour-delegate* (make-instance 'bonjour-delegate)))
  (let ((service (objc:invoke (objc:invoke "NSNetService" "alloc")
                              "initWithDomain:type:name:port:"
                              "" +bonjour-type+ name port)))
    (when (null-object-p service)
      (error "The Bonjour service could not be made."))
    (objc:invoke service "setDelegate:" (objc:objc-object-pointer *bonjour-delegate*))
    (setf *bonjour-service* service)
    (set-bonjour-status :publishing)
    (objc:invoke service "publish")
    service))

(defun bonjour-status-text ()
  "A sentence for the status line, or NIL when there is nothing to say."
  (let ((status *bonjour-status*))
    (cond ((null status) nil)
          ((eq status :publishing) "Announcing with Bonjour…")
          ((eq (first status) :published)
           (format nil "Announced with Bonjour as “~a”." (second status)))
          (t (format nil "Bonjour could not announce it (error ~d)." (second status))))))
