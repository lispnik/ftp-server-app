;;;; main-thread.lisp -- getting from a server thread to the main one.
;;;;
;;;; The server's threads may not touch AppKit.  What they have to say is put
;;;; on a queue, and the main thread is asked to empty it.  They never wait for
;;;; that: the main thread may at that moment be waiting for them, in Stop.

(in-package #:ftp-server)

(defvar *main-thread-queue* '())
(defvar *main-thread-lock* (sb-thread:make-mutex :name "ftp-server main-thread queue"))
(defvar *main-thread-target* nil
  "The Objective-C object that answers ftpDrainQueue:, once there is one.")
(defvar *drain-scheduled* nil
  "Whether the main thread has been asked and has not yet answered, so that a
busy server asks once rather than once for each thing it says.")

(defun drain-main-thread-queue ()
  "Run everything queued, on the main thread.  Answers how many there were."
  (let ((thunks (sb-thread:with-mutex (*main-thread-lock*)
                  (setf *drain-scheduled* nil)
                  (prog1 (nreverse *main-thread-queue*)
                    (setf *main-thread-queue* '())))))
    (dolist (thunk thunks)
      (handler-case (funcall thunk)
        (error (condition) (note "main thread: ~a" condition))))
    (length thunks)))

(defun post-to-main-thread (function)
  "Have FUNCTION called on the main thread, soon, and return without waiting.
Called on the main thread, it is simply called."
  (cond
    ((objc.runloop:main-thread-p)
     (funcall function))
    (t
     (let ((ask (sb-thread:with-mutex (*main-thread-lock*)
                  (push function *main-thread-queue*)
                  (and *main-thread-target*
                       (not *drain-scheduled*)
                       (setf *drain-scheduled* t)))))
       (when ask
         ;; A pool of this thread's own: there is none here unless we make one.
         (objc:with-autorelease-pool ()
           (objc:invoke *main-thread-target*
                        "performSelectorOnMainThread:withObject:waitUntilDone:modes:"
                        (objc:coerce-to-selector "ftpDrainQueue:")
                        (cffi:null-pointer) nil +run-loop-modes+))))))
  (values))
