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
   (launch-checkbox :initform nil :accessor controller-launch-checkbox)
   (tls-checkbox :initform nil :accessor controller-tls-checkbox)
   (certificate-label :initform nil :accessor controller-certificate-label)
   (certificate-button :initform nil :accessor controller-certificate-button)
   (activity-table :initform nil :accessor controller-activity-table)
   (activity :initform '() :accessor controller-activity
             :documentation "The activity pane's entries, newest first.")
   (activity-rows :initform #() :accessor controller-activity-rows
                  :documentation "The same entries in the order the table shows.")
   (activity-count :initform 0 :accessor controller-activity-count)
   (activity-sort :initform "time" :accessor controller-activity-sort
                  :documentation "The column the activity table is sorted on.")
   (activity-ascending :initform t :accessor controller-activity-ascending)
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
                           (controller-remote-checkbox controller)
                           (controller-tls-checkbox controller)))
      (objc:invoke control "setEnabled:" (not running)))
    ;; A server that is running is serving the certificate it started with.
    (objc:invoke (controller-certificate-button controller) "setEnabled:" (not running))
    (objc:invoke (controller-certificate-label controller) "setStringValue:"
                 (certificate-text))
    (objc:invoke (controller-remove-button controller) "setEnabled:"
                 (>= (objc:invoke table "selectedRow") 0))))

;;; The certificate ---------------------------------------------------------------

(defun split-fingerprint (fingerprint)
  "FINGERPRINT on two lines, broken between two of its octets, so that it fits
beside its button."
  (let ((middle (position #\: fingerprint :start (floor (length fingerprint) 2))))
    (if middle
        (format nil "~a~%~a" (subseq fingerprint 0 middle) (subseq fingerprint (1+ middle)))
        fingerprint)))

(defun certificate-text ()
  "What the window says of the certificate: its fingerprint, which is what a
client shows when it asks whether to trust the server."
  (let ((fingerprint (current-certificate-fingerprint)))
    (if fingerprint
        (split-fingerprint fingerprint)
        (format nil "None yet.~%One is made the first time the server starts."))))

(defun controller-new-certificate (controller)
  "Replace the certificate, unasked.  Answers true if it was replaced."
  (handler-case
      (progn (regenerate-certificate)
             (controller-add-activity controller nil nil
                                      (format nil "new TLS certificate, SHA-256 fingerprint ~a"
                                              (current-certificate-fingerprint)))
             (controller-changed controller)
             t)
    (error (condition)
      (note "new certificate: ~a" condition)
      (controller-changed controller "A new certificate could not be made.")
      nil)))

(defun confirm (message detail button)
  "Ask with an alert.  True if BUTTON, rather than Cancel, was pressed."
  (let ((alert (objc:invoke (objc:invoke "NSAlert" "alloc") "init")))
    (unwind-protect
         (progn
           (objc:invoke alert "setMessageText:" message)
           (objc:invoke alert "setInformativeText:" detail)
           (objc:invoke alert "addButtonWithTitle:" button)
           (objc:invoke alert "addButtonWithTitle:" "Cancel")
           ;; NSAlertFirstButtonReturn.
           (= 1000 (objc:invoke alert "runModal")))
      (objc:release alert))))

(define-controller-method ("newCertificate:" :void) ((sender objc:objc-object-pointer))
  (unless (model-running-p (controller-model self))
    (when (or (null (current-certificate-fingerprint))
              (confirm "Make a new TLS certificate?"
                       "Every client that trusted the present certificate will be asked to trust the new one."
                       "New Certificate"))
      (controller-new-certificate self))))

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
    (objc:invoke (controller-launch-checkbox controller) "setState:"
                 (if (model-start-at-launch model) 1 0))
    (objc:invoke (controller-tls-checkbox controller) "setState:"
                 (if (model-require-tls model) 1 0))
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
          (= 1 (objc:invoke (controller-remote-checkbox controller) "state"))
          (model-start-at-launch model)
          (= 1 (objc:invoke (controller-launch-checkbox controller) "state"))
          (model-require-tls model)
          (= 1 (objc:invoke (controller-tls-checkbox controller) "state")))
    (cond (port
           (setf (model-port model) port)
           (values t nil))
          (t
           (values nil "The port must be a number from 1 to 65535.")))))

(defun save-model (controller)
  "Save the settings.  Answers a message if something about that went wrong."
  (handler-case
      (if (model-save (controller-model controller))
          nil
          (progn (note "the keychain did not take the password")
                 "The password could not be saved to the keychain."))
    (error (condition)
      (note "saving settings: ~a" condition)
      "The settings could not be saved.")))

(defun controller-changed (controller &optional message)
  "After anything changes: remember MESSAGE, redraw, and save."
  (let ((trouble (save-model controller)))
    (setf (controller-message controller) (or message trouble)))
  (objc:invoke (controller-table controller) "reloadData")
  (refresh-controls controller))

;;; Starting and stopping -------------------------------------------------------------

(defparameter *activity-limit* 500
  "How many rows the activity pane keeps.")

(defun show-activity (controller)
  "Put the activity table's rows in the order its header asks for, and show
them."
  (let ((table (controller-activity-table controller)))
    (setf (controller-activity-rows controller)
          (coerce (sort-activity (controller-activity controller)
                                 (controller-activity-sort controller)
                                 (controller-activity-ascending controller))
                  'vector))
    (objc:invoke table "reloadData")
    ;; In order of time, keep the newest row in view, whichever end it is at.
    ;; In any other order there is no such end, and the table is left alone.
    (when (and (string= "time" (controller-activity-sort controller))
               (plusp (length (controller-activity-rows controller))))
      (objc:invoke table "scrollRowToVisible:"
                   (if (controller-activity-ascending controller)
                       (1- (length (controller-activity-rows controller)))
                       0)))))

(defun controller-add-activity (controller user address message)
  "Add a row to the activity pane, and a line to the log.  USER is a name or
NIL; ADDRESS is where the client is, or NIL for something the server itself did."
  (let ((entry (make-activity-entry
                :sequence (incf (controller-activity-count controller))
                :time (get-universal-time)
                :user user :address address :message message)))
    (note "~@[~a  ~]~a"
          (and address
               (format nil "~@[~a@~]~a" (and user (plusp (length user)) user)
                       (address-string address)))
          message)
    (push entry (controller-activity controller))
    (when (> (length (controller-activity controller)) *activity-limit*)
      ;; The oldest go, whatever order is on show.
      (setf (controller-activity controller)
            (subseq (controller-activity controller) 0 *activity-limit*)))
    (show-activity controller)
    entry))

(defun controller-clear-activity (controller)
  (setf (controller-activity controller) '())
  (show-activity controller))

(defun selected-activity (controller)
  "The entries of the selected rows of the activity pane, in the order shown;
or of every row, if none is selected."
  (let* ((table (controller-activity-table controller))
         (rows (controller-activity-rows controller))
         (indexes (objc:invoke table "selectedRowIndexes"))
         (selected (loop for index = (objc:invoke indexes "firstIndex")
                           then (objc:invoke indexes "indexGreaterThanIndex:" index)
                         until (or (= index cocoa:ns-not-found) (>= index (length rows)))
                         collect (aref rows index))))
    (or selected (coerce rows 'list))))

(defun controller-activity-text (controller)
  "What Copy puts on the clipboard."
  (activity-text (selected-activity controller)))

(defun copy-to-clipboard (string)
  (let ((pasteboard (objc:invoke "NSPasteboard" "generalPasteboard")))
    (objc:invoke pasteboard "clearContents")
    (objc:invoke pasteboard "setString:forType:" string "public.utf8-plain-text")))

(define-controller-method ("clearActivity:" :void) ((sender objc:objc-object-pointer))
  (controller-clear-activity self))

(define-controller-method ("copyActivity:" :void) ((sender objc:objc-object-pointer))
  (copy-to-clipboard (controller-activity-text self)))

;;; Copy from the Edit menu, or Command-C.  The window's delegate is asked
;;; after the window itself, so this is reached only when nothing with a
;;; selection of its own -- a field being typed in -- has taken it.
(define-controller-method ("copy:" :void) ((sender objc:objc-object-pointer))
  (copy-to-clipboard (controller-activity-text self)))

;;; A message too long for its column is cut short with an ellipsis; resting
;;; the pointer on it shows the whole of it.
(define-controller-method ("tableView:toolTipForCell:rect:tableColumn:row:mouseLocation:"
                           objc:objc-object-pointer
                           :on-error (cffi:null-pointer))
    ((table objc:objc-object-pointer)
     (cell objc:objc-object-pointer)
     (rect (:pointer :void))
     (column objc:objc-object-pointer)
     (row :long)
     (location cocoa:ns-point))
  (let ((rows (controller-activity-rows self)))
    (if (and (activity-table-p self table) (< -1 row (length rows)))
        (objc:string-to-ns-string (activity-cell (aref rows row) (column-key column)) t)
        (cffi:null-pointer))))

(defun controller-server-event (controller event arguments)
  "On the main thread, for something a server thread said."
  (ecase event
    (:activity
     (destructuring-bind (user address text) arguments
       (controller-add-activity controller user address text)))
    (:client-connected
     (controller-add-activity controller nil (first arguments) "connected"))
    (:client-disconnected
     (controller-add-activity controller nil (first arguments) "disconnected"))
    (:started
     (controller-add-activity controller nil nil
                              (format nil "server started on port ~d" (first arguments))))
    (:stopped
     (controller-add-activity controller nil nil "server stopped"))
    (:log
     (controller-add-activity controller nil nil (first arguments))))
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
        (when (model-tls-description model)
          (controller-add-activity controller nil nil (model-tls-description model)))
        (when (model-advertise-p model)
          (handler-case
              (bonjour-publish (server-port (model-server model))
                               :name (model-bonjour-name model))
            (error (condition) (note "Bonjour: ~a" condition)))))
      (controller-changed controller message)
      ok)))

(defun controller-stop (controller)
  (bonjour-unpublish)
  (model-stop (controller-model controller))
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

(defun activity-table-p (controller table)
  "Whether TABLE is the activity pane rather than the table of folders.  The
controller is the data source of both."
  (let ((activity (controller-activity-table controller)))
    (and activity (cffi:pointer-eq table activity))))

(define-controller-method ("numberOfRowsInTableView:" :long :on-error 0)
    ((table objc:objc-object-pointer))
  (if (activity-table-p self table)
      (length (controller-activity-rows self))
      (length (model-mappings (controller-model self)))))

(define-controller-method ("tableView:objectValueForTableColumn:row:"
                           objc:objc-object-pointer
                           :on-error (cffi:null-pointer))
    ((table objc:objc-object-pointer)
     (column objc:objc-object-pointer)
     (row :long))
  (let ((mapping (and (not (activity-table-p self table))
                      (model-mapping (controller-model self) row)))
        (key (column-key column)))
    ;; Autoreleased: an object a Lisp method answers is the caller's to release,
    ;; and a table releases nothing it is given here.
    (cond ((activity-table-p self table)
           (let ((rows (controller-activity-rows self)))
             (if (< -1 row (length rows))
                 (objc:string-to-ns-string (activity-cell (aref rows row) key) t)
                 (cffi:null-pointer))))
          ((null mapping) (cffi:null-pointer))
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
          ;; Nothing in the activity pane is to be changed.
          ((activity-table-p self table))
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

;;; A click on a column's header changes the table's sort descriptors, and
;;; this is how the table says so.  The first descriptor is the column just
;;; clicked and which way; the sorting itself is done here, in Lisp.
(define-controller-method ("tableView:sortDescriptorsDidChange:" :void)
    ((table objc:objc-object-pointer) (old objc:objc-object-pointer))
  (when (activity-table-p self table)
    (let ((descriptors (objc:invoke table "sortDescriptors")))
      (when (and (not (null-object-p descriptors))
                 (plusp (objc:invoke descriptors "count")))
        (let* ((first (objc:invoke descriptors "objectAtIndex:" 0))
               (key (objc:invoke-into 'string first "key")))
          (when (member key *activity-columns* :test #'equal)
            (setf (controller-activity-sort self) key
                  (controller-activity-ascending self)
                  (objc:invoke-bool first "ascending"))
            (show-activity self)))))))

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

(defun add-column (table key title width &key editable checkbox (resizing 2) sortable)
  (let ((column (objc:invoke (objc:invoke "NSTableColumn" "alloc")
                             "initWithIdentifier:" key)))
    (objc:invoke column "setTitle:" title)
    (objc:invoke column "setWidth:" width)
    (objc:invoke column "setResizingMask:" resizing)
    (objc:invoke column "setEditable:" editable)
    (when sortable
      ;; What a click on this column's header asks for: its key, ascending
      ;; first.  The header draws the arrow.
      (objc:invoke column "setSortDescriptorPrototype:"
                   (objc:invoke "NSSortDescriptor" "sortDescriptorWithKey:ascending:" key t)))
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
    ;; Several rows at once, for Copy.
    (objc:invoke table "setAllowsMultipleSelection:" t)
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

(defun add-small-button (content title action frame mask target)
  "A button of the small size, for beside a label."
  (let ((button (add-button content title action frame mask target)))
    (objc:invoke button "setControlSize:" 1)
    (objc:invoke button "setFont:"
                 (objc:invoke "NSFont" "systemFontOfSize:"
                              (objc:invoke "NSFont" "smallSystemFontSize")))
    button))

(defparameter +window-width+ 680d0)
(defparameter +window-height+ 782d0)
(defparameter +activity-height+ 156d0)

(defun add-checkbox (content title frame mask target)
  (let ((checkbox (objc:invoke "NSButton" "checkboxWithTitle:target:action:"
                               title target (objc:coerce-to-selector "settingsChanged:"))))
    (objc:invoke checkbox "setFrame:" frame)
    (objc:invoke checkbox "setAutoresizingMask:" mask)
    (objc:invoke content "addSubview:" checkbox)
    checkbox))

(defun add-activity-table (content frame mask target)
  "The activity pane: a table of when, who, from where and what, sorted by a
click on any of its headers.  Answers the table."
  (let ((scroll (objc:invoke (objc:invoke "NSScrollView" "alloc") "initWithFrame:" frame))
        (table (objc:invoke (objc:invoke "NSTableView" "alloc") "initWithFrame:"
                            (vector 0d0 0d0 (aref frame 2) (aref frame 3))))
        (font (objc:invoke "NSFont" "systemFontOfSize:" 11d0)))
    (objc:invoke table "setStyle:" +ns-table-view-style-full-width+)
    (objc:invoke table "setRowHeight:" 16d0)
    (loop for (key title width resizing) in '(("time" "Time" 136d0 2)
                                              ("user" "User" 90d0 2)
                                              ("address" "IP Address" 116d0 2)
                                              ;; The message has what is left.
                                              ("message" "Message" 280d0 3))
          do (let ((column (add-column table key title width
                                       :resizing resizing :sortable t)))
               (objc:invoke (objc:invoke column "dataCell") "setFont:" font)
               ;; A message too long for its column ends in an ellipsis.
               (objc:invoke (objc:invoke column "dataCell") "setLineBreakMode:" 4)))
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

(defun make-window-controller (model)
  "A controller for MODEL with its window built and not yet shown.

Laid out from both ends: the settings hang from the top, the activity pane and
the Start button stand on the bottom, and the table has what is between, which
is what grows when the window does."
  (let* ((controller (make-instance 'window-controller :model model))
         (target (objc:objc-object-pointer controller))
         (window (objc:invoke (objc:invoke "NSWindow" "alloc")
                              "initWithContentRect:styleMask:backing:defer:"
                              (vector 0d0 0d0 +window-width+ +window-height+)
                              +ns-window-style-mask+ +ns-backing-store-buffered+ t))
         (content (objc:invoke window "contentView"))
         ;; Pinned to the top, to the bottom, and stretching between.
         (top +flexible-bottom+)
         (bottom +flexible-top+)
         (wide (- +window-width+ 40d0))
         ;; Where the next row of settings goes.
         (y (- +window-height+ 42d0)))
    ;; Or the close button frees a window this still points at.
    (objc:invoke window "setReleasedWhenClosed:" nil)
    (objc:invoke window "setTitle:" "FTP Server")
    (objc:invoke window "setContentMinSize:" #(600d0 640d0))
    ;; For Copy, which reaches the window's delegate by the responder chain.
    (objc:invoke window "setDelegate:" target)
    (setf (controller-window controller) window)
    (flet ((labelled-field (label class width)
             (add-label content label (vector 20d0 (+ y 3d0) 100d0 17d0) top)
             (prog1 (add-field content class (vector 126d0 y width 22d0) top target)
               (decf y 30d0)))
           (checkbox (title)
             (prog1 (add-checkbox content title (vector 126d0 (+ y 4d0) 400d0 18d0)
                                  top target)
               (decf y 24d0))))
      (setf (controller-username-field controller)
            (labelled-field "User name:" "NSTextField" 200d0)
            (controller-password-field controller)
            (labelled-field "Password:" "NSSecureTextField" 200d0)
            (controller-port-field controller)
            (labelled-field "Port:" "NSTextField" 70d0)
            (controller-bonjour-field controller)
            (labelled-field "Bonjour name:" "NSTextField" 200d0))
      (objc:invoke (controller-bonjour-field controller) "setPlaceholderString:"
                   "This computer’s name")
      (setf (controller-remote-checkbox controller)
            (checkbox "Allow connections from other computers")
            (controller-tls-checkbox controller)
            (checkbox "Require TLS: refuse clients that do not encrypt")
            (controller-launch-checkbox controller)
            (checkbox "Start serving when FTP Server opens"))
      ;; The certificate: its fingerprint on two lines, which can be selected
      ;; and copied, and the button that replaces it.
      (decf y 14d0)
      (add-label content "TLS certificate:" (vector 20d0 (+ y 10d0) 100d0 17d0) top)
      (let ((label (objc:invoke "NSTextField" "wrappingLabelWithString:" "")))
        (objc:invoke label "setFrame:" (vector 126d0 (- y 4d0) 360d0 30d0))
        (objc:invoke label "setAutoresizingMask:" top)
        (objc:invoke label "setFont:"
                     (objc:invoke "NSFont" "monospacedSystemFontOfSize:weight:" 10d0 0d0))
        (objc:invoke content "addSubview:" label)
        (setf (controller-certificate-label controller) label))
      (setf (controller-certificate-button controller)
            (add-button content "New Certificate…" "newCertificate:"
                        (vector (- +window-width+ 174d0) y 160d0 32d0)
                        (logior +flexible-left+ top) target))
      (decf y 28d0))

    ;; From the bottom up.
    (setf (controller-status-label controller)
          (add-label content "" (vector 20d0 22d0 (- +window-width+ 150d0) 17d0)
                     (logior +flexible-width+ bottom))
          (controller-start-button controller)
          (add-button content "Start" "toggleServer:"
                      (vector (- +window-width+ 124d0) 14d0 110d0 32d0)
                      (logior +flexible-left+ bottom) target)
          (controller-activity-table controller)
          (add-activity-table content (vector 20d0 56d0 wide +activity-height+)
                              (logior +flexible-width+ bottom) target))
    ;; Oldest first, to begin with, and the header says so with its arrow.
    (objc:invoke (controller-activity-table controller) "setSortDescriptors:"
                 (vector (objc:invoke "NSSortDescriptor" "sortDescriptorWithKey:ascending:"
                                      "time" t)))
    (let ((above-activity (+ 56d0 +activity-height+)))
      (add-label content "Activity:" (vector 20d0 (+ above-activity 6d0) 300d0 17d0) bottom)
      (add-small-button content "Copy" "copyActivity:"
                        (vector (- +window-width+ 162d0) (+ above-activity 1d0) 70d0 26d0)
                        (logior +flexible-left+ bottom) target)
      (add-small-button content "Clear" "clearActivity:"
                        (vector (- +window-width+ 88d0) (+ above-activity 1d0) 70d0 26d0)
                        (logior +flexible-left+ bottom) target)
      (add-button content "Add…" "addMapping:"
                  (vector 14d0 (+ above-activity 28d0) 90d0 32d0) bottom target)
      (setf (controller-remove-button controller)
            (add-button content "Remove" "removeMapping:"
                        (vector 106d0 (+ above-activity 28d0) 90d0 32d0) bottom target))
      ;; And the table between the two.
      (let ((table-bottom (+ above-activity 68d0)))
        (add-label content "Shared folders:" (vector 20d0 (- y 2d0) 300d0 17d0) top)
        (setf (controller-table controller)
              (add-table content (vector 20d0 table-bottom wide (- y 8d0 table-bottom))
                         (logior +flexible-width+ +flexible-height+) target))))

    (objc:invoke window "center")
    (show-model controller)
    controller))

(defun show-window (controller)
  (objc:invoke (controller-window controller) "makeKeyAndOrderFront:" (cffi:null-pointer))
  (objc:invoke (objc.runloop:shared-application) "activateIgnoringOtherApps:" t)
  controller)
