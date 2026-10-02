;;;; window.lisp -- the window: settings, the table of mapped folders, Start.
;;;;
;;;; One controller is the target of every control and the table's data
;;;; source.  It holds no state of its own beyond the controls: what it shows
;;;; is the model's, and what it does is a call on the model.

(in-package #:ftp-server)

(objc:define-objc-class window-controller ()
  ((model :initarg :model :accessor controller-model)
   (window :initform nil :accessor controller-window)
   (table :initform nil :accessor controller-table)
   (username-field :initform nil :accessor controller-username-field)
   (password-field :initform nil :accessor controller-password-field)
   (port-field :initform nil :accessor controller-port-field)
   (bonjour-field :initform nil :accessor controller-bonjour-field)
   (remote-checkbox :initform nil :accessor controller-remote-checkbox)
   (remove-button :initform nil :accessor controller-remove-button)
   (start-button :initform nil :accessor controller-start-button)
   (status-label :initform nil :accessor controller-status-label)
   (message :initform nil :accessor controller-message
            :documentation "Something that went wrong, shown until the next
thing that goes right."))
  (:objc-class-name "FTPServerWindowController"))

(defmacro define-controller-method ((selector result-type &key on-error)
                                    (&rest arguments) &body body)
  "A method of the controller.  Nothing may unwind into AppKit, so whatever
BODY signals is logged and ON-ERROR is answered instead."
  `(objc:define-objc-method (,selector ,result-type)
       ((self window-controller) ,@arguments)
     (declare (ignorable self ,@(mapcar #'first arguments)))
     (handler-case (progn ,@body)
       (error (condition)
         (note "~a: ~a" ,selector condition)
         ,on-error))))

;;; Showing the model -----------------------------------------------------------------

(defun field-string (field)
  (or (objc:invoke-into 'string field "stringValue") ""))

(defun controller-status-text (controller)
  (or (controller-message controller)
      (format nil "~a~@[  ~a~]"
              (model-status-text (controller-model controller))
              (and (model-running-p (controller-model controller))
                   (bonjour-status-text)))))

(defun refresh-controls (controller)
  "Make the controls agree with the model: the status line, the button's
title, and which fields can be changed.  What the server listens on and who
may log in are fixed while it runs; the folders are not."
  (let* ((model (controller-model controller))
         (running (model-running-p model))
         (table (controller-table controller)))
    (objc:invoke (controller-status-label controller) "setStringValue:"
                 (controller-status-text controller))
    (objc:invoke (controller-start-button controller) "setTitle:"
                 (if running "Stop" "Start"))
    (dolist (control (list (controller-username-field controller)
                           (controller-password-field controller)
                           (controller-port-field controller)
                           (controller-bonjour-field controller)
                           (controller-remote-checkbox controller)))
      (objc:invoke control "setEnabled:" (not running)))
    (objc:invoke (controller-remove-button controller) "setEnabled:"
                 (>= (objc:invoke table "selectedRow") 0))))

(defun show-model (controller)
  "Put the model's settings into the fields."
  (let ((model (controller-model controller)))
    (objc:invoke (controller-username-field controller) "setStringValue:"
                 (model-username model))
    (objc:invoke (controller-password-field controller) "setStringValue:"
                 (model-password model))
    (objc:invoke (controller-port-field controller) "setStringValue:"
                 (format nil "~d" (model-port model)))
    (objc:invoke (controller-bonjour-field controller) "setStringValue:"
                 (model-bonjour-name model))
    (objc:invoke (controller-remote-checkbox controller) "setState:"
                 (if (model-allow-remote model) 1 0))
    (objc:invoke (controller-table controller) "reloadData")
    (refresh-controls controller)))

(defun read-fields (controller)
  "Put the fields into the model.  Answers NIL, and a message, if the port is
not one; the other fields are taken as they are."
  (let ((model (controller-model controller))
        (port (parse-port (field-string (controller-port-field controller)))))
    (setf (model-username model) (field-string (controller-username-field controller))
          (model-password model) (field-string (controller-password-field controller))
          (model-bonjour-name model)
          (string-trim " " (field-string (controller-bonjour-field controller)))
          (model-allow-remote model)
          (= 1 (objc:invoke (controller-remote-checkbox controller) "state")))
    (cond (port
           (setf (model-port model) port)
           (values t nil))
          (t
           (values nil "The port must be a number from 1 to 65535.")))))

(defun save-model (controller)
  (handler-case (model-save (controller-model controller))
    (error (condition) (note "saving settings: ~a" condition))))

(defun controller-changed (controller &optional message)
  "After anything changes: remember MESSAGE, redraw, and save."
  (setf (controller-message controller) message)
  (objc:invoke (controller-table controller) "reloadData")
  (refresh-controls controller)
  (save-model controller))

;;; Starting and stopping -------------------------------------------------------------

(defun controller-server-event (controller event arguments)
  "On the main thread, for something a server thread said."
  (when (eq event :log)
    (note "~a" (first arguments)))
  (when (member event '(:client-connected :client-disconnected))
    (note "~(~a~) ~a" event (and (first arguments) (address-string (first arguments)))))
  (refresh-controls controller))

(defun controller-start (controller)
  "Start the server from what the fields say.  True if it started."
  (let ((model (controller-model controller)))
    (multiple-value-bind (ok message) (read-fields controller)
      (when ok
        (multiple-value-setq (ok message)
          (model-start model
                       :on-event (lambda (event &rest arguments)
                                   (post-to-main-thread
                                    (lambda ()
                                      (controller-server-event controller event
                                                               arguments)))))))
      (when ok
        (note "started on port ~d" (server-port (model-server model)))
        (when (model-advertise-p model)
          (handler-case
              (bonjour-publish (server-port (model-server model))
                               :name (model-bonjour-name model))
            (error (condition) (note "Bonjour: ~a" condition)))))
      (controller-changed controller message)
      ok)))

(defun controller-stop (controller)
  (bonjour-unpublish)
  (when (model-stop (controller-model controller))
    (note "stopped"))
  (controller-changed controller))

(define-controller-method ("toggleServer:" :void) ((sender objc:objc-object-pointer))
  ;; So that a field still being typed in is finished before it is read.
  (objc:invoke (controller-window self) "makeFirstResponder:" (cffi:null-pointer))
  (if (model-running-p (controller-model self))
      (controller-stop self)
      (controller-start self)))

(define-controller-method ("settingsChanged:" :void) ((sender objc:objc-object-pointer))
  (unless (model-running-p (controller-model self))
    (multiple-value-bind (ok message) (read-fields self)
      (declare (ignore ok))
      (controller-changed self message))))

(define-controller-method ("ftpDrainQueue:" :void) ((argument objc:objc-object-pointer))
  (drain-main-thread-queue))

;;; Folders -----------------------------------------------------------------------

(defun controller-add-directory (controller host-path)
  "Map HOST-PATH and show it.  Answers the row it is in, or NIL."
  (multiple-value-bind (mapping message)
      (model-add-directory (controller-model controller) host-path)
    (controller-changed controller message)
    (and mapping
         (position mapping (model-mappings (controller-model controller))))))

(defun select-row (controller row)
  (objc:invoke (controller-table controller)
               "selectRowIndexes:byExtendingSelection:"
               (objc:invoke "NSIndexSet" "indexSetWithIndex:" row) nil)
  (refresh-controls controller))

(defun choose-directories (window)
  "Ask for folders with an open panel.  Answers their paths."
  (declare (ignorable window))
  (let ((panel (objc:invoke "NSOpenPanel" "openPanel")))
    (objc:invoke panel "setCanChooseDirectories:" t)
    (objc:invoke panel "setCanChooseFiles:" nil)
    (objc:invoke panel "setAllowsMultipleSelection:" t)
    (objc:invoke panel "setPrompt:" "Add")
    (objc:invoke panel "setMessage:" "Choose the folders to share over FTP.")
    (when (= +ns-modal-response-ok+ (objc:invoke panel "runModal"))
      (let ((urls (objc:invoke panel "URLs")))
        (loop for index below (objc:invoke urls "count")
              for path = (objc:invoke-into 'string (objc:invoke urls "objectAtIndex:" index)
                                           "path")
              when path collect path)))))

(define-controller-method ("addMapping:" :void) ((sender objc:objc-object-pointer))
  (let ((row nil))
    (dolist (path (choose-directories (controller-window self)))
      (setf row (or (controller-add-directory self path) row)))
    (when row
      (select-row self row)
      ;; Straight into its name, which is the thing most likely to be wanted
      ;; different: /tmp arrives as "tmp" and may be meant as "tempdir".
      (ignore-errors
       (objc:invoke (controller-table self) "editColumn:row:withEvent:select:"
                    0 row (cffi:null-pointer) t)))))

(define-controller-method ("removeMapping:" :void) ((sender objc:objc-object-pointer))
  (let ((row (objc:invoke (controller-table self) "selectedRow")))
    (when (>= row 0)
      (model-remove-mapping (controller-model self) row)
      (controller-changed self))))

;;; The table's data ------------------------------------------------------------------

(defun column-key (column)
  (if (null-object-p column)
      ""
      (or (objc:invoke-into 'string column "identifier") "")))

(define-controller-method ("numberOfRowsInTableView:" :long :on-error 0)
    ((table objc:objc-object-pointer))
  (length (model-mappings (controller-model self))))

(define-controller-method ("tableView:objectValueForTableColumn:row:"
                           objc:objc-object-pointer
                           :on-error (cffi:null-pointer))
    ((table objc:objc-object-pointer)
     (column objc:objc-object-pointer)
     (row :long))
  (let ((mapping (model-mapping (controller-model self) row))
        (key (column-key column)))
    ;; Autoreleased: an object a Lisp method answers is the caller's to release,
    ;; and a table releases nothing it is given here.
    (cond ((null mapping) (cffi:null-pointer))
          ((string= key "writable")
           (objc:invoke "NSNumber" "numberWithBool:" (mapping-writable mapping)))
          ((string= key "path")
           (objc:string-to-ns-string (mapping-host-path mapping) t))
          (t
           (objc:string-to-ns-string (mapping-name mapping) t)))))

(define-controller-method ("tableView:setObjectValue:forTableColumn:row:" :void)
    ((table objc:objc-object-pointer)
     (value objc:objc-object-pointer)
     (column objc:objc-object-pointer)
     (row :long))
  (let ((model (controller-model self))
        (key (column-key column)))
    (cond ((null-object-p value))
          ((string= key "writable")
           (model-set-writable model row (objc:invoke-bool value "boolValue"))
           (controller-changed self))
          ((string= key "name")
           ;; A refused name leaves the mapping as it was; the table redraws
           ;; the old one and the status line says why.
           (multiple-value-bind (ok message)
               (model-rename-mapping model row
                                     (or (objc:invoke-into 'string value "description") ""))
             (declare (ignore ok))
             (controller-changed self message))))))

(define-controller-method ("tableViewSelectionDidChange:" :void)
    ((notification objc:objc-object-pointer))
  (refresh-controls self))

;;; Building it -----------------------------------------------------------------------

(defun add-label (content text frame mask)
  (let ((label (objc:invoke "NSTextField" "labelWithString:" text)))
    (objc:invoke label "setFrame:" frame)
    (objc:invoke label "setAutoresizingMask:" mask)
    (objc:invoke content "addSubview:" label)
    label))

(defun add-field (content class frame mask target)
  "A text field of CLASS that tells TARGET when it has been changed."
  (let ((field (objc:invoke (objc:invoke class "alloc") "initWithFrame:" frame)))
    (objc:invoke field "setAutoresizingMask:" mask)
    (objc:invoke field "setTarget:" target)
    (objc:invoke field "setAction:" (objc:coerce-to-selector "settingsChanged:"))
    (objc:invoke content "addSubview:" field)
    (objc:release field)
    field))

(defun add-button (content title action frame mask target)
  (let ((button (objc:invoke "NSButton" "buttonWithTitle:target:action:"
                             title target (objc:coerce-to-selector action))))
    (objc:invoke button "setFrame:" frame)
    (objc:invoke button "setAutoresizingMask:" mask)
    (objc:invoke content "addSubview:" button)
    button))

(defun add-column (table key title width &key editable checkbox (resizing 2))
  (let ((column (objc:invoke (objc:invoke "NSTableColumn" "alloc")
                             "initWithIdentifier:" key)))
    (objc:invoke column "setTitle:" title)
    (objc:invoke column "setWidth:" width)
    (objc:invoke column "setResizingMask:" resizing)
    (objc:invoke column "setEditable:" editable)
    (when checkbox
      (let ((cell (objc:invoke (objc:invoke "NSButtonCell" "alloc") "init")))
        (objc:invoke cell "setButtonType:" +ns-button-type-switch+)
        (objc:invoke cell "setTitle:" "")
        (objc:invoke column "setDataCell:" cell)
        (objc:release cell)))
    (objc:invoke table "addTableColumn:" column)
    (objc:release column)
    column))

(defun add-table (content frame mask target)
  "The table of mappings in its scroll view.  Answers the table."
  (let ((scroll (objc:invoke (objc:invoke "NSScrollView" "alloc") "initWithFrame:" frame))
        ;; At the scroll view's own origin: FRAME's is a place in the window,
        ;; and a table given it draws its rows that far from its headers.
        (table (objc:invoke (objc:invoke "NSTableView" "alloc") "initWithFrame:"
                            (vector 0d0 0d0 (aref frame 2) (aref frame 3)))))
    ;; Edge to edge, so that the three widths below are the whole of it.
    (objc:invoke table "setStyle:" +ns-table-view-style-full-width+)
    (add-column table "name" "Name" 130d0 :editable t)
    ;; The host directory takes whatever width is going; the others keep theirs.
    (add-column table "path" "Folder on This Mac" 280d0 :resizing 3)
    (add-column table "writable" "Writable" 70d0 :editable t :checkbox t)
    (objc:invoke table "setUsesAlternatingRowBackgroundColors:" t)
    (objc:invoke table "setAllowsMultipleSelection:" nil)
    (objc:invoke table "setColumnAutoresizingStyle:" 1)
    (objc:invoke table "setDataSource:" target)
    (objc:invoke table "setDelegate:" target)
    (objc:invoke scroll "setHasVerticalScroller:" t)
    (objc:invoke scroll "setAutohidesScrollers:" t)
    (objc:invoke scroll "setBorderType:" +ns-bezel-border+)
    (objc:invoke scroll "setAutoresizingMask:" mask)
    (objc:invoke scroll "setDocumentView:" table)
    (objc:invoke content "addSubview:" scroll)
    (objc:invoke table "sizeToFit")
    (objc:release table)
    (objc:release scroll)
    table))

(defparameter +window-width+ 560d0)
(defparameter +window-height+ 490d0)

(defun make-window-controller (model)
  "A controller for MODEL with its window built and not yet shown."
  (let* ((controller (make-instance 'window-controller :model model))
         (target (objc:objc-object-pointer controller))
         (window (objc:invoke (objc:invoke "NSWindow" "alloc")
                              "initWithContentRect:styleMask:backing:defer:"
                              (vector 0d0 0d0 +window-width+ +window-height+)
                              +ns-window-style-mask+ +ns-backing-store-buffered+ t))
         (content (objc:invoke window "contentView"))
         ;; Pinned to the top, to the bottom, and stretching between.
         (top +flexible-bottom+)
         (bottom +flexible-top+))
    ;; Or the close button frees a window this still points at.
    (objc:invoke window "setReleasedWhenClosed:" nil)
    (objc:invoke window "setTitle:" "FTP Server")
    (objc:invoke window "setContentMinSize:" #(480d0 380d0))
    (setf (controller-window controller) window)

    (add-label content "User name:" #(20d0 448d0 100d0 17d0) top)
    (setf (controller-username-field controller)
          (add-field content "NSTextField" #(126d0 445d0 200d0 22d0) top target))
    (add-label content "Password:" #(20d0 418d0 100d0 17d0) top)
    (setf (controller-password-field controller)
          (add-field content "NSSecureTextField" #(126d0 415d0 200d0 22d0) top target))
    (add-label content "Port:" #(20d0 388d0 100d0 17d0) top)
    (setf (controller-port-field controller)
          (add-field content "NSTextField" #(126d0 385d0 70d0 22d0) top target))
    (let ((checkbox (objc:invoke "NSButton" "checkboxWithTitle:target:action:"
                                 "Allow connections from other computers"
                                 target (objc:coerce-to-selector "settingsChanged:"))))
      (objc:invoke checkbox "setFrame:" #(210d0 386d0 330d0 18d0))
      (objc:invoke checkbox "setAutoresizingMask:" top)
      (objc:invoke content "addSubview:" checkbox)
      (setf (controller-remote-checkbox controller) checkbox))
    (add-label content "Bonjour name:" #(20d0 358d0 100d0 17d0) top)
    (let ((field (add-field content "NSTextField" #(126d0 355d0 200d0 22d0) top target)))
      (objc:invoke field "setPlaceholderString:" "This computer’s name")
      (setf (controller-bonjour-field controller) field))

    (add-label content "Shared folders:" #(20d0 326d0 300d0 17d0) top)
    (setf (controller-table controller)
          (add-table content #(20d0 96d0 520d0 224d0)
                     (logior +flexible-width+ +flexible-height+) target))

    (add-button content "Add…" "addMapping:" #(14d0 56d0 90d0 32d0) bottom target)
    (setf (controller-remove-button controller)
          (add-button content "Remove" "removeMapping:" #(106d0 56d0 90d0 32d0)
                      bottom target))

    (setf (controller-status-label controller)
          (add-label content "" #(20d0 22d0 410d0 17d0)
                     (logior +flexible-width+ bottom)))
    (setf (controller-start-button controller)
          (add-button content "Start" "toggleServer:" #(436d0 14d0 110d0 32d0)
                      (logior +flexible-left+ bottom) target))

    (objc:invoke window "center")
    (show-model controller)
    controller))

(defun show-window (controller)
  (objc:invoke (controller-window controller) "makeKeyAndOrderFront:" (cffi:null-pointer))
  (objc:invoke (objc.runloop:shared-application) "activateIgnoringOtherApps:" t)
  controller)
