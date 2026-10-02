;;;; keychain.lisp -- the password, kept in the login keychain.
;;;;
;;;; The settings file is a file: anything running as this user can read it.
;;;; A keychain item can be read only by the application that made it, unless
;;;; the user says otherwise when asked.  So the password goes there, and the
;;;; settings file is left without one.
;;;;
;;;; This is the SecItem interface of Security.framework, which is C: a query
;;;; is a dictionary whose keys are constants the framework exports.

(in-package #:ftp-server)

(defparameter +keychain-service+ "org.lispnik.ftp-server"
  "The service the item is filed under, which is what Keychain Access shows as
its name.")
(defparameter +keychain-account+ "FTP login")

(defconstant +err-sec-success+ 0)
(defconstant +err-sec-item-not-found+ -25300)
(defconstant +utf-8+ 4 "NSUTF8StringEncoding.")

(define-condition keychain-error (error)
  ((status :initarg :status :reader keychain-error-status)
   (operation :initarg :operation :reader keychain-error-operation))
  (:report (lambda (condition stream)
             (format stream "The keychain refused to ~a the password (error ~d)."
                     (keychain-error-operation condition)
                     (keychain-error-status condition)))))

(defun security-constant (name)
  "The value of the constant NAME that Security.framework exports.  Looked up
when asked: in the application this is a saved image, and an address found at
build time means nothing at run time."
  (cffi:mem-ref (cffi:foreign-symbol-pointer name) :pointer))

(defun keychain-query (service account &rest more)
  "A dictionary naming the item for SERVICE and ACCOUNT, with the keys and
values in MORE, which alternate, added to it."
  (let ((query (objc:invoke "NSMutableDictionary" "dictionary")))
    (objc:invoke query "setObject:forKey:"
                 (security-constant "kSecClassGenericPassword")
                 (security-constant "kSecClass"))
    (objc:invoke query "setObject:forKey:" service (security-constant "kSecAttrService"))
    (objc:invoke query "setObject:forKey:" account (security-constant "kSecAttrAccount"))
    (loop for (key value) on more by #'cddr
          do (objc:invoke query "setObject:forKey:" value (security-constant key)))
    query))

(defun password-data (password)
  "PASSWORD as an autoreleased NSData of UTF-8."
  (let ((string (objc:string-to-ns-string password t)))
    (objc:invoke string "dataUsingEncoding:" +utf-8+)))

(defun keychain-get (service account)
  "The password kept for SERVICE and ACCOUNT, or NIL if there is none.
Signals KEYCHAIN-ERROR if there is one and it may not be read."
  (objc:with-autorelease-pool ()
    (cffi:with-foreign-object (result :pointer)
      (setf (cffi:mem-ref result :pointer) (cffi:null-pointer))
      (let ((status (cffi:foreign-funcall
                     "SecItemCopyMatching"
                     :pointer (keychain-query service account
                                              "kSecReturnData"
                                              (objc:invoke "NSNumber" "numberWithBool:" t)
                                              "kSecMatchLimit"
                                              (security-constant "kSecMatchLimitOne"))
                     :pointer result
                     :int32)))
        (cond ((= status +err-sec-item-not-found+) nil)
              ((/= status +err-sec-success+)
               (error 'keychain-error :status status :operation "read"))
              (t
               ;; The data is ours to release, and so is the string made of it.
               (let* ((data (cffi:mem-ref result :pointer))
                      (string (objc:invoke (objc:invoke "NSString" "alloc")
                                           "initWithData:encoding:" data +utf-8+)))
                 (unwind-protect
                      (if (null-object-p string)
                          ""
                          (or (objc:ns-string-to-string string) ""))
                   (unless (null-object-p string) (objc:release string))
                   (objc:release data)))))))))

(defun keychain-delete (service account)
  "Forget the password for SERVICE and ACCOUNT.  True if there was one."
  (objc:with-autorelease-pool ()
    (let ((status (cffi:foreign-funcall "SecItemDelete"
                                        :pointer (keychain-query service account)
                                        :int32)))
      (cond ((= status +err-sec-success+) t)
            ((= status +err-sec-item-not-found+) nil)
            (t (error 'keychain-error :status status :operation "remove"))))))

(defun keychain-set (service account password)
  "Keep PASSWORD for SERVICE and ACCOUNT, in place of any that is there."
  (objc:with-autorelease-pool ()
    (let* ((changes (objc:invoke "NSMutableDictionary" "dictionary")))
      (objc:invoke changes "setObject:forKey:" (password-data password)
                   (security-constant "kSecValueData"))
      (let ((status (cffi:foreign-funcall "SecItemUpdate"
                                          :pointer (keychain-query service account)
                                          :pointer changes
                                          :int32)))
        (when (= status +err-sec-item-not-found+)
          (setf status (cffi:foreign-funcall
                        "SecItemAdd"
                        :pointer (keychain-query service account
                                                 "kSecValueData" (password-data password)
                                                 "kSecAttrLabel" "FTP Server")
                        :pointer (cffi:null-pointer)
                        :int32)))
        (unless (= status +err-sec-success+)
          (error 'keychain-error :status status :operation "save"))
        t))))

;;; The store the model saves through -------------------------------------------------

(defun keychain-password-store (&optional (service +keychain-service+)
                                          (account +keychain-account+))
  "A password store on the keychain item for SERVICE and ACCOUNT.

It remembers what it last read or wrote, and saving the same password again
does nothing: the window saves its settings at every change, and the keychain
is not something to be asked that often."
  (let ((known nil))
    (make-password-store
     :fetch (lambda ()
              (setf known (or (keychain-get service account) "")))
     :store (lambda (password)
              (unless (equal password known)
                (if (string= password "")
                    (keychain-delete service account)
                    (keychain-set service account password))
                (setf known password))
              t))))

(defun choose-password-store ()
  "Where the application keeps its password: the keychain, unless the settings
are somewhere other than the usual place, which is how a test or a script
keeps away from the real ones.  FTP_SERVER_KEYCHAIN names a keychain service to
use even then."
  (let ((service (sb-posix:getenv "FTP_SERVER_KEYCHAIN"))
        (elsewhere (sb-posix:getenv "FTP_SERVER_SETTINGS")))
    (cond ((and service (plusp (length service)))
           (keychain-password-store service))
          ((and elsewhere (plusp (length elsewhere))) nil)
          (t (keychain-password-store)))))
