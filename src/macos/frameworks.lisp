;;;; frameworks.lisp -- AppKit: loading it, its constants, and the log.
;;;;
;;;; Nothing foreign is made when this is loaded.  The application is a saved
;;;; image, and a pointer made in the build would be garbage in the run.

(in-package #:ftp-server)

(defparameter +cocoa-framework+
  "/System/Library/Frameworks/Cocoa.framework/Versions/A/Cocoa")

(defparameter +security-framework+
  "/System/Library/Frameworks/Security.framework/Versions/A/Security")

(defun ensure-frameworks ()
  "Bring up the Objective-C runtime with AppKit in it, and Security for the
keychain.  Every entry point that touches AppKit calls this first; in the
bundle it is also what rebuilds the classes defined here."
  (objc:ensure-objc-initialized :modules (list +cocoa-framework+ +security-framework+)))

;;; Constants -------------------------------------------------------------------

(defconstant +ns-window-style-mask+ 15
  "Titled, closable, miniaturizable, resizable.")
(defconstant +ns-backing-store-buffered+ 2)

;;; Autoresizing: which of a view's margins and dimensions give when its
;;; superview changes size.  AppKit's y runs upwards, so "min y" is the bottom.
(defconstant +flexible-left+ 1)
(defconstant +flexible-width+ 2)
(defconstant +flexible-bottom+ 8)
(defconstant +flexible-height+ 16)
(defconstant +flexible-top+ 32)

(defconstant +ns-button-type-switch+ 3)
(defconstant +ns-modal-response-ok+ 1)
(defconstant +ns-bezel-border+ 2)
(defconstant +ns-table-view-style-full-width+ 1)

(defparameter +run-loop-modes+
  #("NSDefaultRunLoopMode" "NSEventTrackingRunLoopMode" "NSModalPanelRunLoopMode")
  "The modes a perform on the main thread is delivered in: the usual one, and
the ones that run while a window is resized or a panel is up.  Named one by
one; a perform queued for the common-modes pseudo mode from a Lisp thread has
been seen never to run.")

;;; Small things ------------------------------------------------------------------

(defun null-object-p (object)
  "Whether OBJECT is no Objective-C object at all."
  (or (null object) (cffi:null-pointer-p object)))

(defvar *log* nil
  "Where NOTE writes: the application's log, once MAIN has said so.  Written
from the main thread only -- the bundle's log stream is bound there and nowhere
else.")

(defun note (control &rest arguments)
  "Write a line to the log."
  (when *log*
    (ignore-errors
     (multiple-value-bind (second minute hour) (get-decoded-time)
       (format *log* "~&~2,'0d:~2,'0d:~2,'0d ~?~%" hour minute second control arguments))
     (finish-output *log*))))
