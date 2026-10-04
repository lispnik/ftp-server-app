;;;; users-window.lisp -- who may log in, and what each of them may do.
;;;;
;;;; On the left, the users: added, removed, and renamed in place.  On the
;;;; right, for the one selected, their password and a table of every mapping
;;;; with what they may do there: nothing, read, or read and write.  A change
;;;; is saved at once, and applies to a running server from each client's next
;;;; command.

(in-package #:ftp-server)

(defvar *users-controller* nil "The Users window's controller, once it is made.")

(defparameter +access-titles+ #("No Access" "Read" "Read & Write")
  "What the access popup offers, in the order of *ACCESS-LEVELS*.")

(objc:define-objc-class users-controller ()
  ((main :initarg :main :reader users-main
         :documentation "The main window's controller: it saves, and has the model.")
   (window :initform nil :accessor users-window)
   (users-table :initform nil :accessor users-users-table)
   (access-table :initform nil :accessor users-access-table)
   (password-field :initform nil :accessor users-password-field)
   (remove-button :initform nil :accessor users-remove-button)
   (message-label :initform nil :accessor users-message-label)
   (selected :initform nil :accessor users-selected
             :documentation "The user the right-hand side is about, or NIL."))
  (:objc-class-name "FTPServerUsersController"))

(defmacro define-users-method ((selector result-type &key on-error) (&rest arguments)
                               &body body)
  "A method of the Users controller.  Nothing may unwind into AppKit."
  `(objc:define-objc-method (,selector ,result-type)
       ((self users-controller) ,@arguments)
     (declare (ignorable self ,@(mapcar #'first arguments)))
     (handler-case (progn ,@body)
       (error (condition)
         (note "users ~a: ~a" ,selector condition)
         ,on-error))))

(defun users-model (controller)
  (controller-model (users-main controller)))

(defparameter +users-hint+
  "A folder a user has no access to is hidden from them.")

(defun users-say (controller &optional message)
  (objc:invoke (users-message-label controller) "setStringValue:"
               (or message +users-hint+)))

;;; Keeping it up to date ---------------------------------------------------------------

(defun selected-user-row (controller)
  (objc:invoke (users-users-table controller) "selectedRow"))

(defun show-selected-user (controller)
  "Make the right-hand side about the selected user."
  (let* ((user (model-user (users-model controller) (selected-user-row controller)))
         (field (users-password-field controller)))
    (setf (users-selected controller) user)
    (objc:invoke field "setStringValue:" (if user (user-password user) ""))
    (objc:invoke field "setEnabled:" (and user t))
    (objc:invoke (users-access-table controller) "setEnabled:" (and user t))
    (objc:invoke (users-access-table controller) "reloadData")
    (objc:invoke (users-remove-button controller) "setEnabled:" (and user t))))

(defun refresh-users-window ()
  "Show the users and the mappings as they are now, if the window is made."
  (let ((controller *users-controller*))
    (when controller
      (objc:invoke (users-users-table controller) "reloadData")
      (show-selected-user controller))))

(defun users-changed (controller &optional message)
  "After anything changes: save, and say MESSAGE if there is one."
  (let ((trouble (save-model (users-main controller))))
    (users-say controller (or message trouble)))
  (refresh-controls (users-main controller))
  (objc:invoke (users-users-table controller) "reloadData")
  (show-selected-user controller))

(defun commit-password (controller)
  "Give the selected user what the password field says, if that is new."
  (let ((user (users-selected controller)))
    (when user
      (let ((password (field-string (users-password-field controller))))
        (unless (string= password (user-password user))
          (model-set-password (users-model controller) user password)
          (users-changed controller))))))

;;; Users -----------------------------------------------------------------------------

(defun users-select-row (controller row)
  (objc:invoke (users-users-table controller) "selectRowIndexes:byExtendingSelection:"
               (objc:invoke "NSIndexSet" "indexSetWithIndex:" row) nil)
  (show-selected-user controller))

(defun users-add-user (controller)
  "Add a user, select them, and answer their row."
  (commit-password controller)
  (let* ((model (users-model controller))
         (user (model-add-user model))
         (row (position user (model-users model))))
    (users-changed controller "Name them, then give them a password and access.")
    (users-select-row controller row)
    row))

(define-users-method ("addUser:" :void) ((sender objc:objc-object-pointer))
  (let ((row (users-add-user self)))
    ;; Straight into the name, which is the first thing to change.
    (ignore-errors
     (objc:invoke (users-users-table self) "editColumn:row:withEvent:select:"
                  0 row (cffi:null-pointer) t))))

(define-users-method ("removeUser:" :void) ((sender objc:objc-object-pointer))
  (let ((user (users-selected self)))
    (when user
      (model-remove-user (users-model self) user)
      (setf (users-selected self) nil)
      (objc:invoke (users-users-table self) "deselectAll:" (cffi:null-pointer))
      (users-changed self (format nil "Removed ~a." (user-name user))))))

(define-users-method ("passwordChanged:" :void) ((sender objc:objc-object-pointer))
  (commit-password self))

;;; The password field's delegate: editing that ends by any means -- Tab, a
;;; click elsewhere -- and not only by Return.
(define-users-method ("controlTextDidEndEditing:" :void)
    ((notification objc:objc-object-pointer))
  (commit-password self))

;;; The tables' data -------------------------------------------------------------------

(defun users-table-p (controller table)
  (cffi:pointer-eq table (users-users-table controller)))

(defun access-index (level)
  (position level *access-levels*))

(define-users-method ("numberOfRowsInTableView:" :long :on-error 0)
    ((table objc:objc-object-pointer))
  (if (users-table-p self table)
      (length (model-users (users-model self)))
      (length (model-mappings (users-model self)))))

(define-users-method ("tableView:objectValueForTableColumn:row:" objc:objc-object-pointer
                      :on-error (cffi:null-pointer))
    ((table objc:objc-object-pointer)
     (column objc:objc-object-pointer)
     (row :long))
  (let ((model (users-model self)))
    (if (users-table-p self table)
        (let ((user (model-user model row)))
          (if user
              (objc:string-to-ns-string (user-name user) t)
              (cffi:null-pointer)))
        (let ((mapping (model-mapping model row))
              (user (users-selected self)))
          (cond ((null mapping) (cffi:null-pointer))
                ((string= "access" (column-key column))
                 (objc:invoke "NSNumber" "numberWithInteger:"
                              (if user (access-index (model-access model user mapping)) 0)))
                (t (objc:string-to-ns-string (mapping-name mapping) t)))))))

(define-users-method ("tableView:setObjectValue:forTableColumn:row:" :void)
    ((table objc:objc-object-pointer)
     (value objc:objc-object-pointer)
     (column objc:objc-object-pointer)
     (row :long))
  (let ((model (users-model self)))
    (cond
      ((null-object-p value))
      ((users-table-p self table)
       (let ((user (model-user model row)))
         (when user
           (multiple-value-bind (ok message)
               (model-rename-user model user
                                  (or (objc:invoke-into 'string value "description") ""))
             (declare (ignore ok))
             (users-changed self message)))))
      ((string= "access" (column-key column))
       (let ((mapping (model-mapping model row))
             (user (users-selected self))
             (index (objc:invoke value "integerValue")))
         (when (and mapping user (< -1 index (length *access-levels*)))
           (model-set-access model user mapping (nth index *access-levels*))
           (users-changed self)))))))

(define-users-method ("tableViewSelectionDidChange:" :void)
    ((notification objc:objc-object-pointer))
  (when (users-table-p self (objc:invoke notification "object"))
    ;; What was typed for the user who was selected goes to them, not the next.
    (commit-password self)
    (show-selected-user self)))

;;; Building it -------------------------------------------------------------------------

(defun add-access-column (table)
  "The column of popups, one for each mapping, saying what the user may do."
  (let ((column (add-column table "access" "Access" 130d0 :editable t))
        (cell (objc:invoke (objc:invoke "NSPopUpButtonCell" "alloc")
                           "initTextCell:pullsDown:" "" nil)))
    (objc:invoke cell "addItemsWithTitles:" +access-titles+)
    (objc:invoke cell "setBordered:" nil)
    (objc:invoke column "setDataCell:" cell)
    (objc:release cell)
    column))

(defun add-simple-table (content frame mask target columns)
  "A table in a scroll view, with COLUMNS added by the function COLUMNS.
Answers the table."
  (let ((scroll (objc:invoke (objc:invoke "NSScrollView" "alloc") "initWithFrame:" frame))
        (table (objc:invoke (objc:invoke "NSTableView" "alloc") "initWithFrame:"
                            (vector 0d0 0d0 (aref frame 2) (aref frame 3)))))
    (objc:invoke table "setStyle:" +ns-table-view-style-full-width+)
    (funcall columns table)
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

(defparameter +users-width+ 620d0)
(defparameter +users-height+ 420d0)

(defun make-users-controller (main)
  "The Users window for the main window's controller MAIN, built and not shown."
  (let* ((controller (make-instance 'users-controller :main main))
         (target (objc:objc-object-pointer controller))
         (window (objc:invoke (objc:invoke "NSWindow" "alloc")
                              "initWithContentRect:styleMask:backing:defer:"
                              (vector 0d0 0d0 +users-width+ +users-height+)
                              +ns-window-style-mask+ +ns-backing-store-buffered+ t))
         (content (objc:invoke window "contentView"))
         (top +flexible-bottom+)
         (bottom +flexible-top+)
         (right-x 250d0)
         (right-width (- +users-width+ 250d0 20d0)))
    (objc:invoke window "setReleasedWhenClosed:" nil)
    (objc:invoke window "setTitle:" "Users")
    (objc:invoke window "setContentMinSize:" #(520d0 320d0))
    (setf (users-window controller) window)

    ;; The users.
    (add-label content "Users:" (vector 20d0 (- +users-height+ 36d0) 200d0 17d0) top)
    (setf (users-users-table controller)
          (add-simple-table content (vector 20d0 60d0 210d0 (- +users-height+ 104d0))
                            (logior +flexible-height+) target
                            (lambda (table)
                              (add-column table "name" "Name" 190d0 :editable t :resizing 3))))
    (add-button content "Add" "addUser:" #(14d0 18d0 100d0 32d0) bottom target)
    (setf (users-remove-button controller)
          (add-button content "Remove" "removeUser:" #(116d0 18d0 100d0 32d0) bottom target))

    ;; The one selected.
    (add-label content "Password:" (vector right-x (- +users-height+ 36d0) 80d0 17d0) top)
    (let ((field (add-field content "NSSecureTextField"
                            (vector (+ right-x 80d0) (- +users-height+ 39d0)
                                    (- right-width 80d0) 22d0)
                            (logior top +flexible-width+) target)))
      (objc:invoke field "setAction:" (objc:coerce-to-selector "passwordChanged:"))
      (objc:invoke field "setDelegate:" target)
      (setf (users-password-field controller) field))
    (add-label content "Access to shared folders:"
               (vector right-x (- +users-height+ 68d0) right-width 17d0) top)
    (setf (users-access-table controller)
          (add-simple-table content (vector right-x 60d0 right-width (- +users-height+ 136d0))
                            (logior +flexible-width+ +flexible-height+) target
                            (lambda (table)
                              (add-column table "mapping" "Shared Folder" 200d0 :resizing 3)
                              (add-access-column table))))
    (setf (users-message-label controller)
          (add-label content "" (vector right-x 26d0 right-width 17d0)
                     (logior +flexible-width+ bottom)))
    (objc:invoke (users-message-label controller) "setFont:"
                 (objc:invoke "NSFont" "systemFontOfSize:"
                              (objc:invoke "NSFont" "smallSystemFontSize")))
    ;; A long message ends in an ellipsis rather than off the edge.
    (objc:invoke (objc:invoke (users-message-label controller) "cell") "setLineBreakMode:" 4)
    (users-say controller)

    (objc:invoke window "center")
    (objc:invoke (users-users-table controller) "reloadData")
    (show-selected-user controller)
    controller))

(defun show-users-window (main)
  "Show the Users window, making it the first time."
  (unless *users-controller*
    (setf *users-controller* (make-users-controller main)))
  (refresh-users-window)
  (objc:invoke (users-window *users-controller*) "makeKeyAndOrderFront:" (cffi:null-pointer))
  *users-controller*)

(define-controller-method ("showUsers:" :void) ((sender objc:objc-object-pointer))
  (show-users-window self))
