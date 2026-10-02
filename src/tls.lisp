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

(defun regenerate-certificate (&optional (directory (settings-directory)))
  "Throw the certificate and its key away and make new ones.  Every client
that trusted the old certificate will be asked about the new one."
  (multiple-value-bind (certificate key) (certificate-files directory)
    (dolist (file (list certificate key))
      (when (probe-file file)
        (delete-file file)))
    (generate-certificate certificate key)))

(defun current-certificate-fingerprint (&optional (directory (settings-directory)))
  "The fingerprint of the certificate in DIRECTORY, or NIL if there is none
yet or it cannot be read."
  (let ((certificate (certificate-files directory)))
    (and (probe-file certificate)
         (ignore-errors (certificate-fingerprint certificate)))))

(defun certificate-fingerprint (certificate)
  "The SHA-256 fingerprint of CERTIFICATE, as pairs of hex digits with colons
between, which is how clients show it."
  (let* ((output (run-openssl "x509" "-noout" "-fingerprint" "-sha256"
                              "-in" (sb-ext:native-namestring certificate)))
         (equals (position #\= output)))
    (string-trim '(#\Newline #\Space) (subseq output (if equals (1+ equals) 0)))))

;;; The library, in an application ----------------------------------------------------
;;;
;;; The application is a saved image.  Which copy of OpenSSL it opens when it
;;; starts is asdf-macos-app's business: as it saves the image it points each
;;; bundled library at the bundle's own copy, in Contents/Frameworks.  What is
;;; left for this file is what cl+ssl itself remembers from the process that
;;; was saved.

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

(defun tls-accept (context stream &key (tickets t))
  "The server's side of a handshake over STREAM, in CONTEXT: an encrypted
stream.  Without TICKETS the server sends no session tickets on it.

This is cl+ssl's MAKE-SSL-SERVER-STREAM, taken apart for the one thing it has
no argument for: something done to the connection between its being made and
the handshake.  It is over the Lisp stream rather than its descriptor, so that
the stream's timeouts still apply and shutting the socket down still wakes
whoever is reading."
  (cl+ssl:with-global-context (context)
    (cl+ssl::ensure-initialized)
    (let ((tls (make-instance 'cl+ssl::ssl-server-stream
                              :socket stream
                              :close-callback nil
                              :certificate nil
                              :key nil
                              :input-buffer-size cl+ssl::*default-buffer-size*
                              :output-buffer-size cl+ssl::*default-buffer-size*)))
      (cl+ssl::with-new-ssl (handle)
        (cl+ssl::install-handle-and-bio tls handle stream nil)
        (cl+ssl::ssl-set-accept-state handle)
        (unless tickets
          (cffi:foreign-funcall "SSL_set_num_tickets" :pointer handle :size 0 :int))
        (cl+ssl::collecting-verify-error (handle)
          (cl+ssl::ensure-ssl-funcall tls #'plusp #'cl+ssl::ssl-accept handle))
        (cl+ssl::handle-external-format tls nil)))))

(defun make-tls-wrapper (certificate key)
  "A function for MAKE-SERVER's :TLS, serving CERTIFICATE with KEY: given a
stream on a connection, it does the server's side of the handshake and answers
the encrypted stream.  With :DATA true the connection is a data connection.
TLS 1.2 and later.

Session tickets go out on the control connection and not on data connections,
and both halves of that matter.

A client proves that a data connection is its own by resuming, on it, the
session of its control connection; a careful client -- FileZilla -- warns when
it cannot.  In TLS 1.3 a session is resumed with a ticket and with nothing
else, so the control connection has to be given some.

But in TLS 1.3 a server sends tickets after the handshake, unasked.  A client
that is only uploading never reads them; a socket closed with something unread
in it is reset rather than finished; and a reset throws away whatever of the
upload had yet to arrive.  Large uploads lost their last part.  So a data
connection, which has no use for a ticket of its own, is sent none."
  (ensure-tls-state)
  (let ((context (cl+ssl:make-context
                  :certificate-chain-file (sb-ext:native-namestring certificate)
                  :private-key-file (sb-ext:native-namestring key)
                  ;; Clients are known by their password, not by a certificate.
                  :verify-mode cl+ssl:+ssl-verify-none+
                  :min-proto-version cl+ssl::+tls1-2-version+)))
    ;; What sessions made here are sessions of, which a server has to have
    ;; said before it will resume one.
    (cffi:with-foreign-string ((name length) "ftp-server")
      (cffi:foreign-funcall "SSL_CTX_set_session_id_context"
                            :pointer context :pointer name
                            :unsigned-int (1- length) :int))
    (lambda (stream &key data)
      (tls-accept context stream :tickets (not data)))))

(defun make-default-tls (&optional (directory (settings-directory)))
  "TLS with the certificate kept in DIRECTORY, beside the settings: the
wrapper, and a sentence about the certificate for whoever starts the server."
  (multiple-value-bind (certificate key) (ensure-certificate directory)
    (values (make-tls-wrapper certificate key)
            (format nil "TLS is available.  Certificate SHA-256 fingerprint ~a"
                    (certificate-fingerprint certificate)))))
