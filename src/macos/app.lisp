;;;; app.lisp -- the application: its delegate, its menu, and MAIN.

(in-package #:ftp-server)

(defvar *controller* nil
  "The window's controller.  Held here because nothing in AppKit retains a
target, a data source or a delegate.")
(defvar *application-delegate* nil)

(objc:define-objc-class application-delegate ()
  ()
  (:objc-class-name "FTPServerApplicationDelegate"))

;;; With no window there is nothing to stop the server with, and a server
;;; nobody can see is not one to leave running.
(objc:define-objc-method ("applicationShouldTerminateAfterLastWindowClosed:"
                          objc:objc-bool)
    ((self application-delegate) (application objc:objc-object-pointer))
  (declare (ignorable self application))
  t)

(defun shut-down ()
  "Stop announcing, stop serving, and save.  -terminate: ends the process
without unwinding anything, so this is the only chance."
  (let ((controller *controller*))
    (when controller
      (bonjour-unpublish)
      (unless (model-running-p (controller-model controller))
        (read-fields controller))
      (model-stop (controller-model controller))
      (save-model controller)
      (note "quit"))))

(objc:define-objc-method ("applicationWillTerminate:" :void)
    ((self application-delegate) (notification objc:objc-object-pointer))
  (declare (ignorable self notification))
  (handler-case (shut-down)
    (error (condition) (note "applicationWillTerminate: ~a" condition))))

;;; The menu ----------------------------------------------------------------------
;;;
;;; A program with no nib has no menu bar until it makes one, and without an
;;; Edit menu there is no Paste in the password field.  Every target is nil:
;;; the responder chain finds the field for Paste and the application for Quit.

(defun add-menu-item (menu title action key)
  (let ((item (objc:invoke (objc:invoke "NSMenuItem" "alloc")
                           "initWithTitle:action:keyEquivalent:"
                           title (objc:coerce-to-selector action) key)))
    (objc:invoke menu "addItem:" item)
    (objc:release item)
    item))

(defun add-submenu (main title items)
  "A top-level menu of ITEMS, each (TITLE ACTION [KEY]) or :SEPARATOR."
  (let ((menu (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" title))
        (holder (objc:invoke (objc:invoke "NSMenuItem" "alloc")
                             "initWithTitle:action:keyEquivalent:"
                             title (cffi:null-pointer) "")))
    (dolist (item items)
      (if (eq item :separator)
          (objc:invoke menu "addItem:" (objc:invoke "NSMenuItem" "separatorItem"))
          (destructuring-bind (item-title action &optional (key "")) item
            (add-menu-item menu item-title action key))))
    (objc:invoke holder "setSubmenu:" menu)
    (objc:invoke main "addItem:" holder)
    (objc:release holder)
    menu))

(defun install-menu ()
  (let ((main (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" "Main"))
        (application (objc.runloop:shared-application)))
    (add-submenu main "FTP Server"
                 '(("About FTP Server" "orderFrontStandardAboutPanel:")
                   :separator
                   ("Hide FTP Server" "hide:" "h")
                   ("Hide Others" "hideOtherApplications:" "H")
                   :separator
                   ("Quit FTP Server" "terminate:" "q")))
    (add-submenu main "Edit"
                 '(("Undo" "undo:" "z")
                   ("Redo" "redo:" "Z")
                   :separator
                   ("Cut" "cut:" "x")
                   ("Copy" "copy:" "c")
                   ("Paste" "paste:" "v")
                   :separator
                   ("Select All" "selectAll:" "a")))
    (objc:invoke application "setWindowsMenu:"
                 (add-submenu main "Window"
                              '(("Minimize" "performMiniaturize:" "m")
                                ("Zoom" "performZoom:")
                                ("Close" "performClose:" "w"))))
    (objc:invoke application "setMainMenu:" main)
    main))

;;; Starting ----------------------------------------------------------------------

(defun build-application (model)
  "The controller, window, delegate and menu for MODEL.  Answers the controller."
  (let ((controller (make-window-controller model))
        (delegate (make-instance 'application-delegate)))
    (setf *controller* controller
          *application-delegate* delegate
          *main-thread-target* (objc:objc-object-pointer controller)
          *bonjour-changed* (lambda () (refresh-controls controller)))
    (install-menu)
    (objc:invoke (objc.runloop:shared-application) "setDelegate:"
                 (objc:objc-object-pointer delegate))
    controller))

(defun self-test-seconds ()
  "How long FTP_SERVER_SELFTEST says to run for, or NIL.

With it set, the server starts as soon as the window is up and the application
quits by itself that many seconds later.  It is how the bundle is checked from
a script: point FTP_SERVER_SETTINGS at a prepared file, launch, and talk FTP to
it while it lasts."
  (let ((value (sb-posix:getenv "FTP_SERVER_SELFTEST")))
    (and value
         (let ((seconds (ignore-errors (parse-integer value))))
           (and seconds (plusp seconds) seconds)))))

(defun main ()
  "The application's entry point.  Does not return."
  (setf *log* *error-output*)
  (handler-case
      (progn
        (ensure-frameworks)
        (objc.runloop:shared-application)
        (setf *password-store* (choose-password-store))
        (let ((controller (build-application (model-load)))
              (seconds (self-test-seconds)))
          (show-window controller)
          (note "ready")
          (when (and (model-start-at-launch (controller-model controller))
                     (not seconds))
            (controller-start controller))
          (when seconds
            (note "self test: ~:[did not start~;started~], quitting in ~d seconds"
                  (controller-start controller) seconds)
            (objc:invoke (objc.runloop:shared-application)
                         "performSelector:withObject:afterDelay:"
                         (objc:coerce-to-selector "terminate:")
                         (cffi:null-pointer) (float seconds 1d0)))))
    (error (condition)
      (note "could not start: ~a" condition)
      (sb-ext:exit :code 1 :abort t)))
  (objc.runloop:run-cocoa-application))
