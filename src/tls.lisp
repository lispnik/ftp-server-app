;;;; tls.lisp -- TLS for the server: the certificate, and the handshake.
;;;;
;;;; A system of its own, so that the server proper depends on nothing but
;;;; SBCL.  What the server is given is a function from a plain stream to an
;;;; encrypted one; that this one is made with OpenSSL is known only here.

(in-package #:ftp-server)

(defparameter *openssl-program* "/usr/bin/openssl"
  "The command that makes the certificate.  macOS ships it.")

(defparameter *certificate-days* 3650)

(defun certificate-files (&optional (directory (settings-directory)))
  "The certificate and its private key, as two pathnames in DIRECTORY."
  (values (merge-pathnames "certificate.pem" directory)
          (merge-pathnames "private-key.pem" directory)))

(defun run-openssl (&rest arguments)
  "Run the openssl command and answer what it printed.  Signals an error with
what it complained of if it failed."
  (let* ((output (make-string-output-stream))
         (errors (make-string-output-stream))
         (process (sb-ext:run-program *openssl-program* arguments
                                      :output output :error errors :input nil)))
    (unless (zerop (sb-ext:process-exit-code process))
      (error "openssl ~a failed: ~a" (first arguments)
             (string-trim '(#\Newline #\Space) (get-output-stream-string errors))))
    (get-output-stream-string output)))

(defun certificate-common-name ()
  "A name for the certificate: this computer's, as far as it can be written
in one."
  (let ((host (remove-if-not (lambda (char)
                               (or (alphanumericp char) (find char ".-")))
                             (or (ignore-errors (machine-instance)) ""))))
    ;; A common name may be 64 characters and no more.
    (let ((name (format nil "FTP Server on ~a" (if (string= host "") "this Mac" host))))
      (subseq name 0 (min 64 (length name))))))

(defun generate-certificate (certificate key)
  "Make a self-signed certificate and its key.  Nobody has vouched for it: a
client is shown its fingerprint, or told to trust it, the first time."
  (ensure-directories-exist certificate)
  (run-openssl "req" "-x509" "-newkey" "rsa:2048" "-nodes" "-sha256"
               "-days" (format nil "~d" *certificate-days*)
               "-subj" (format nil "/CN=~a" (certificate-common-name))
               "-keyout" (sb-ext:native-namestring key)
               "-out" (sb-ext:native-namestring certificate))
  (sb-posix:chmod (sb-ext:native-namestring key) #o600)
  (values certificate key))

(defun ensure-certificate (&optional (directory (settings-directory)))
  "The certificate and key in DIRECTORY, made now if either is missing."
  (multiple-value-bind (certificate key) (certificate-files directory)
    (unless (and (probe-file certificate) (probe-file key))
      (generate-certificate certificate key))
    (values certificate key)))

(defun certificate-fingerprint (certificate)
  "The SHA-256 fingerprint of CERTIFICATE, as pairs of hex digits with colons
between, which is how clients show it."
  (let* ((output (run-openssl "x509" "-noout" "-fingerprint" "-sha256"
                              "-in" (sb-ext:native-namestring certificate)))
         (equals (position #\= output)))
    (string-trim '(#\Newline #\Space) (subseq output (if equals (1+ equals) 0)))))

;;; The library, in an application ----------------------------------------------------
;;;
;;; The application is a saved image, and SBCL opens a saved image's libraries
;;; again on the way up, from the paths they had in the build: Homebrew's, on
;;; the machine that built it.  A Mac with no Homebrew would not get as far as
;;; a window.  The bundle has its own copy of the libraries, which
;;; asdf-macos-app puts in Contents/Frameworks, so as the image is saved each
;;; library's path is changed to its place there, relative to the executable,
;;; which is a form dlopen understands.

(defun bundle-build-p ()
  "Whether this image is being saved as an application bundle: it is the build
of one that has asdf-macos-app in it."
  (and (find-package '#:asdf-macos-app) t))

(defun point-tls-libraries-at-bundle ()
  "A save hook.  In a bundle build, have OpenSSL opened from the bundle."
  (when (bundle-build-p)
    (dolist (object sb-alien::*shared-objects*)
      (let ((name (sb-alien::shared-object-namestring object)))
        (when (or (search "libssl" name) (search "libcrypto" name))
          ;; By the name the file really has, which is the name it is copied
          ;; under: Homebrew's libssl.dylib is a link to libssl.4.dylib.
          (let ((leaf (file-namestring (or (ignore-errors (truename name)) name))))
            (setf (sb-alien::shared-object-namestring object)
                  (format nil "@executable_path/../Frameworks/~a" leaf))))))))

(pushnew 'point-tls-libraries-at-bundle sb-ext:*save-hooks*)

(defvar *tls-ready-in-process* nil
  "The process cl+ssl's foreign state was last made in.")

(defun ensure-tls-state ()
  "In a process other than the one cl+ssl was loaded in -- a saved image,
started -- forget what it made in the library then: contexts, methods and
strings at addresses in a process that has ended."
  (let ((pid (sb-posix:getpid)))
    (unless (eql pid *tls-ready-in-process*)
      (when *tls-ready-in-process*
        (setf cl+ssl::*ssl-global-context* nil
              cl+ssl::*ssl-global-method* nil
              cl+ssl::*bio-lisp-method* nil
              cl+ssl::*file-name* (cffi:foreign-string-alloc "cl+ssl/src/bio.lisp")
              cl+ssl::*lib-num-for-errors* (cl+ssl::err-get-next-error-library)))
      (setf *tls-ready-in-process* pid)))
  t)

;; The process this is loaded in is the one cl+ssl's state belongs to.
(setf *tls-ready-in-process* (sb-posix:getpid))

(defun make-tls-wrapper (certificate key)
  "A function for MAKE-SERVER's :TLS, serving CERTIFICATE with KEY: given a
stream on a connection, it does the server's side of the handshake and answers
the encrypted stream.  TLS 1.2 and later."
  (ensure-tls-state)
  (let ((context (cl+ssl:make-context
                  :certificate-chain-file (sb-ext:native-namestring certificate)
                  :private-key-file (sb-ext:native-namestring key)
                  ;; Clients are known by their password, not by a certificate.
                  :verify-mode cl+ssl:+ssl-verify-none+
                  :min-proto-version cl+ssl::+tls1-2-version+)))
    ;; No session tickets.  In TLS 1.3 a server sends them after the
    ;; handshake, unasked; a client that is only uploading never reads them,
    ;; and a socket closed with something unread in it is reset rather than
    ;; finished, which throws away whatever of the upload had yet to arrive.
    ;; Large uploads lost their last part until this was here.
    (cffi:foreign-funcall "SSL_CTX_set_num_tickets" :pointer context :size 0 :int)
    (lambda (stream)
      (cl+ssl:with-global-context (context)
        ;; Over the Lisp stream rather than its descriptor, so that the
        ;; stream's timeouts still apply and shutting the socket down still
        ;; wakes whoever is reading.
        (cl+ssl:make-ssl-server-stream stream :unwrap-stream-p nil)))))

(defun make-default-tls (&optional (directory (settings-directory)))
  "TLS with the certificate kept in DIRECTORY, beside the settings: the
wrapper, and a sentence about the certificate for whoever starts the server."
  (multiple-value-bind (certificate key) (ensure-certificate directory)
    (values (make-tls-wrapper certificate key)
            (format nil "TLS is available.  Certificate SHA-256 fingerprint ~a"
                    (certificate-fingerprint certificate)))))
