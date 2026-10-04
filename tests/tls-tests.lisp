;;;; tls-tests.lisp -- FTP over TLS: the commands, the certificate, and a real
;;;; handshake against a real server.

(in-package #:ftp-server/tests)

(def-suite tls :in all-tests :description "Explicit FTP over TLS.")
(in-suite tls)

;;; The commands, with nothing encrypted ---------------------------------------------

(defun pretend-tls (stream &key data)
  "TLS that encrypts nothing, for the commands that only ask whether there is any."
  (declare (ignore data))
  stream)

(defun make-tls-test-session (&key (tls #'pretend-tls) require-tls (logged-in nil))
  "A session with no connection on a server whose TLS is the function TLS."
  (let* ((vfs (fs:make-vfs))
         (server (fs:make-server :vfs vfs :tls tls :require-tls require-tls
                                 :authenticator (constantly t)))
         (session (make-instance 'fs::session :server server :vfs vfs)))
    (when logged-in
      (setf (fs::session-state session) :logged-in))
    session))

(test auth-is-refused-where-there-is-no-tls
  (let ((session (make-tls-test-session :tls nil)))
    (is (= 502 (code session "AUTH TLS")))
    (is-false (fs::session-pending-tls session))
    (is (notany (lambda (line) (search "AUTH" line)) (second (reply session "FEAT"))))))

(test auth-is-agreed-to-and-the-handshake-is-left-for-after-the-reply
  (let ((session (make-tls-test-session)))
    (is (= 504 (code session "AUTH KERBEROS")))
    (is-false (fs::session-pending-tls session))
    (is (= 234 (code session "AUTH TLS")))
    (is-true (fs::session-pending-tls session))
    (is-false (fs::session-secure-p session) "not until the reply has gone")
    (is (member " AUTH TLS" (second (reply session "FEAT")) :test #'string=))
    (is (member " PROT" (second (reply session "FEAT")) :test #'string=))))

(test auth-makes-whoever-was-logging-in-start-again
  (let ((session (make-tls-test-session)))
    (is (= 331 (code session "USER ann")))
    (is (= 234 (code session "AUTH TLS")))
    (is (= 503 (code session "PASS anything")))))

(test protection-is-set-only-on-an-encrypted-connection
  (let ((session (make-tls-test-session)))
    (is (= 503 (code session "PBSZ 0")))
    (is (= 503 (code session "PROT P")))
    (setf (fs::session-secure-p session) t)
    (is (= 503 (code session "AUTH TLS")) "twice is once too many")
    (is (equal '(200 "PBSZ=0") (reply session "PBSZ 0")))
    (is (= 200 (code session "PROT P")))
    (is-true (fs::session-protect-data session))
    (is (= 200 (code session "PROT C")))
    (is-false (fs::session-protect-data session))
    (is (= 504 (code session "PROT S")))))

(test a-server-that-requires-tls-takes-no-name-in-the-clear
  (let ((session (make-tls-test-session :require-tls t)))
    (is (= 530 (code session "USER ann")))
    (is (eq :new (fs::session-state session)))
    (setf (fs::session-secure-p session) t)
    (is (= 331 (code session "USER ann")))
    (is (= 230 (code session "PASS x")))
    ;; Nor does it move data in the clear, or agree to.
    (is (= 534 (code session "PROT C")))
    (is (= 522 (code session "LIST")))
    (is (= 200 (code session "PROT P")))
    (is (= 425 (code session "LIST")) "now it is only the data port that is missing")))

;;; The certificate ------------------------------------------------------------------

(test a-certificate-is-made-once-and-its-key-is-private
  (with-temporary-directory (directory)
    (let ((pathname (sb-ext:parse-native-namestring (concatenate 'string directory "/"))))
      (multiple-value-bind (certificate key) (fs::ensure-certificate pathname)
        (is-true (probe-file certificate))
        (is-true (probe-file key))
        (is (= #o600 (logand #o777 (sb-posix:stat-mode
                                    (sb-posix:stat (sb-ext:native-namestring key))))))
        (is (search "BEGIN CERTIFICATE" (read-file (sb-ext:native-namestring certificate))))
        (let ((fingerprint (fs::certificate-fingerprint certificate)))
          ;; Thirty-two octets: sixty-four digits and thirty-one colons.
          (is (= 95 (length fingerprint)))
          (is (every (lambda (char) (or (digit-char-p char 16) (char= char #\:))) fingerprint))
          ;; Asked for again, it is the same one.
          (fs::ensure-certificate pathname)
          (is (string= fingerprint (fs::certificate-fingerprint certificate))))))))

;;; Over the wire --------------------------------------------------------------------

(defvar *client-context* nil
  "The TLS context the test client makes its connections in.")

(defun client-context ()
  "A context that keeps no cache of sessions.  cl+ssl's own does, and an
OpenSSL client that caches its sessions drops a TLS 1.3 session the first time
it is resumed -- so the control connection's session would be good for one
data connection and no more, by the client's choice and not the server's."
  (or *client-context*
      (setf *client-context*
            (cl+ssl:make-context :verify-mode cl+ssl:+ssl-verify-none+
                                 :session-cache-mode cl+ssl:+ssl-sess-cache-off+))))

(defun client-tls (stream)
  "The client's side of a handshake over STREAM.  The certificate is one the
server made for itself, so there is nothing to check it against."
  (cl+ssl:with-global-context ((client-context))
    (cl+ssl:make-ssl-client-stream stream :unwrap-stream-p nil :verify nil)))

(defun secure-client (client)
  "AUTH TLS, and the handshake.  Answers the reply's code."
  (let ((code (send client "AUTH TLS")))
    (when (eql code 234)
      (setf (client-stream client) (client-tls (client-stream client))))
    code))

(defun tls-fetch (client command)
  "Run COMMAND, which sends data over an encrypted connection: (values CODE DATA)."
  (multiple-value-bind (socket stream) (connect-to (passive-port client :extended t))
    (unwind-protect
         (let ((code (send client command)))
           (if (eql code 150)
               (let* ((tls (client-tls stream))
                      (data (read-all tls)))
                 (ignore-errors (close tls))
                 (values (read-reply client) data))
               (values code nil)))
      (ignore-errors (sb-bsd-sockets:socket-close socket :abort t)))))

(defun tls-store (client command contents)
  (multiple-value-bind (socket stream) (connect-to (passive-port client :extended t))
    (unwind-protect
         (let ((code (send client command)))
           (cond ((eql code 150)
                  (let ((tls (client-tls stream)))
                    (write-sequence (sb-ext:string-to-octets contents :external-format :utf-8)
                                    tls)
                    (finish-output tls)
                    ;; Which says goodbye properly, and closes the socket.
                    (close tls))
                  (read-reply client))
                 (t code)))
      (ignore-errors (sb-bsd-sockets:socket-close socket :abort t)))))

(defun call-with-tls-server (vfs require-tls function)
  (with-temporary-directory (directory)
    (let* ((tls (fs::make-default-tls
                 (sb-ext:parse-native-namestring (concatenate 'string directory "/"))))
           (events (list))
           (lock (sb-thread:make-mutex))
           (server (fs:make-server
                    :vfs vfs :port 0 :tls tls :require-tls require-tls
                    :authenticator (lambda (user password)
                                     (and (string= user "user") (string= password "secret")))
                    :on-event (lambda (&rest event)
                                (sb-thread:with-mutex (lock) (push event events)))))
           (delay fs::*login-failure-delay*))
      (setf fs::*login-failure-delay* 0)
      (fs:start-server server)
      (unwind-protect
           (funcall function (fs:server-port server)
                    (lambda () (sb-thread:with-mutex (lock) (reverse events))))
        (fs:stop-server server)
        (setf fs::*login-failure-delay* delay)))))

(defmacro with-tls-server ((port vfs &key require-tls (events (gensym "EVENTS")))
                           &body body)
  `(call-with-tls-server ,vfs ,require-tls
                         (lambda (,port ,events)
                           (declare (ignorable ,events))
                           ,@body)))

(test a-session-can-be-encrypted-from-login-to-data
  (with-temporary-directory (directory)
    (write-file (path directory "hello.txt") "hello over tls")
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "m" directory :writable t)
      (with-tls-server (port vfs)
        (with-client (client port)
          (is (= 234 (secure-client client)))
          (is (= 230 (login client)))
          (is (= 200 (send client "PBSZ 0")))
          (is (= 200 (send client "PROT P")))
          (multiple-value-bind (code data) (tls-fetch client "NLST /m")
            (is (= 226 code))
            (is (equal '("hello.txt") (lines-of data))))
          (multiple-value-bind (code data) (tls-fetch client "RETR /m/hello.txt")
            (is (= 226 code))
            (is (string= "hello over tls" data)))
          (is (= 226 (tls-store client "STOR /m/up.txt" "sent over tls")))
          (is (string= "sent over tls" (read-file (path directory "up.txt"))))
          (is (= 221 (send client "QUIT"))))))))

(test a-large-file-survives-tls-both-ways
  (with-temporary-directory (directory)
    (let* ((vfs (fs:make-vfs))
           (text (with-output-to-string (out)
                   (dotimes (line 40000) (format out "line ~d of a large file~%" line)))))
      (write-file (path directory "big.txt") text)
      (fs:vfs-add vfs "m" directory :writable t)
      (with-tls-server (port vfs)
        (with-client (client port)
          (secure-client client)
          (login client)
          (send client "PBSZ 0")
          (send client "PROT P")
          (multiple-value-bind (code data) (tls-fetch client "RETR /m/big.txt")
            (is (= 226 code))
            (is (string= text data)))
          (is (= 226 (tls-store client "STOR /m/copy.txt" text)))
          (is (string= text (read-file (path directory "copy.txt")))))))))

(test tls-is-offered-not-imposed-unless-required
  (with-temporary-directory (directory)
    (write-file (path directory "a.txt") "plain")
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "m" directory)
      (with-tls-server (port vfs)
        ;; In the clear throughout.
        (with-client (client port)
          (is (= 230 (login client)))
          (is (string= "plain" (nth-value 1 (fetch client "RETR /m/a.txt")))))
        ;; Encrypted commands, data in the clear: PROT C, which is the default.
        (with-client (client port)
          (is (= 234 (secure-client client)))
          (is (= 230 (login client)))
          (is (string= "plain" (nth-value 1 (fetch client "RETR /m/a.txt")))))))))

(test a-server-that-requires-tls-turns-away-the-clear
  (with-temporary-directory (directory)
    (write-file (path directory "a.txt") "private")
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "m" directory)
      (with-tls-server (port vfs :require-tls t :events events)
        (with-client (client port)
          (is (= 530 (send client "USER user")))
          (is (= 530 (send client "PWD"))))
        (with-client (client port)
          (is (= 234 (secure-client client)))
          (is (= 230 (login client)))
          ;; Data in the clear is refused before it is opened.
          (is (= 522 (fetch client "RETR /m/a.txt")))
          (is (= 534 (send client "PROT C")))
          (is (= 200 (send client "PROT P")))
          (is (string= "private" (nth-value 1 (tls-fetch client "RETR /m/a.txt")))))
        (is-true (wait-until
                  (lambda ()
                    (find "was refused: this server requires TLS"
                          (activity (funcall events)) :key #'second :test #'equal))))))))

(test a-client-that-botches-the-handshake-does-not-hurt-the-server
  (with-tls-server (port (fs:make-vfs) :events events)
    (with-client (client port)
      (is (= 234 (send client "AUTH TLS")))
      ;; Not a handshake.
      (fs::write-line-crlf (client-stream client) "USER user")
      (finish-output (client-stream client)))
    (with-client (client port)
      (is (= 234 (secure-client client)))
      (is (= 230 (login client))))
    (is (null (find :log (funcall events) :key #'first))
        "and it is not reported as something wrong with the server")))

(test the-model-starts-a-server-with-tls-when-it-is-given-the-means
  (with-temporary-directory (directory)
    (let ((model (fs:make-model))
          (pathname (sb-ext:parse-native-namestring (concatenate 'string directory "/")))
          (delay fs::*login-failure-delay*))
      (add-user model "user" "secret")
      (setf (fs:model-require-tls model) t
            (fs:model-port model) (let ((probe (fs::listen-on #(127 0 0 1) 0)))
                                    (prog1 (fs::socket-port probe)
                                      (sb-bsd-sockets:socket-close probe)))
            fs::*login-failure-delay* 0)
      (unwind-protect
           (progn
             ;; Required, and no way to provide it.
             (let ((fs:*tls-maker* nil))
               (multiple-value-bind (ok message) (fs:model-start model)
                 (is (null ok))
                 (is (search "TLS is required" message))))
             (let ((fs:*tls-maker* (lambda () (error "no library"))))
               (multiple-value-bind (ok message) (fs:model-start model)
                 (is (null ok))
                 (is (search "no library" message))))
             (let ((fs:*tls-maker* (lambda () (fs::make-default-tls pathname))))
               (is-true (fs:model-start model))
               (is (search "fingerprint" (fs:model-tls-description model)))
               (with-client (client (fs:model-port model))
                 (is (= 530 (send client "USER user")))
                 (is (= 234 (secure-client client)))
                 (is (= 230 (login client))))))
        (fs:model-stop model)
        (setf fs::*login-failure-delay* delay)))))

(test a-certificate-can-be-replaced
  (with-temporary-directory (directory)
    (let ((pathname (sb-ext:parse-native-namestring (concatenate 'string directory "/"))))
      (is (null (fs::current-certificate-fingerprint pathname)) "none to begin with")
      (fs::ensure-certificate pathname)
      (let ((before (fs::current-certificate-fingerprint pathname)))
        (is (= 95 (length before)))
        (fs::regenerate-certificate pathname)
        (let ((after (fs::current-certificate-fingerprint pathname)))
          (is (= 95 (length after)))
          (is (string/= before after))
          ;; And the new key is as private as the old.
          (is (= #o600 (logand #o777 (sb-posix:stat-mode
                                      (sb-posix:stat (path directory "private-key.pem")))))))))))

;;; Resuming the control connection's session on a data connection -----------------------
;;;
;;; What a careful client does, so that nobody else can take its data
;;; connection: FileZilla warns when a server will not let it.  cl+ssl has no
;;; argument for the session to resume, so the client's handshake is taken
;;; apart here as the server's is in tls.lisp.

(defun client-tls-resuming (stream session)
  "The client's side of a handshake over STREAM, offering to resume SESSION."
  (cl+ssl:with-global-context ((client-context))
    (cl+ssl::ensure-initialized)
    (let ((tls (make-instance 'cl+ssl::ssl-stream
                              :socket stream :close-callback nil
                              :input-buffer-size cl+ssl::*default-buffer-size*
                              :output-buffer-size cl+ssl::*default-buffer-size*)))
      (cl+ssl::with-new-ssl (handle)
        (cl+ssl::install-handle-and-bio tls handle stream nil)
        (cl+ssl::ssl-set-connect-state handle)
        (cffi:foreign-funcall "SSL_set_session" :pointer handle :pointer session :int)
        (cl+ssl::ensure-ssl-funcall tls #'plusp #'cl+ssl::ssl-connect handle)
        (cl+ssl::handle-external-format tls nil)))))

(defun tls-handle (stream)
  (cl+ssl::ssl-stream-handle stream))

(defun session-of (stream)
  "The session of the TLS stream STREAM, to be freed by the caller."
  (cffi:foreign-funcall "SSL_get1_session" :pointer (tls-handle stream) :pointer))

(defun resumed-p (stream)
  (= 1 (cffi:foreign-funcall "SSL_session_reused" :pointer (tls-handle stream) :int)))

(defun tls-version (stream)
  (cffi:foreign-funcall "SSL_get_version" :pointer (tls-handle stream) :string))

(defun close-keeping-session (tls)
  "Close TLS so that its session can be resumed again.

OpenSSL gives up on a session whose connection ends badly, and by then the
server has closed this one: saying goodbye to it fails, and a connection freed
without having said goodbye counts as ended badly too.  Either way the
session, which is the control connection's, would be good for one data
connection and no more -- which is the client's library being careful, not the
server refusing.  So the connection is marked as properly finished, which it
was, and then let go."
  (cffi:foreign-funcall "SSL_set_shutdown" :pointer (tls-handle tls) :int 3 :void)
  (ignore-errors (close tls :abort t)))

(defun fetch-resuming (client command)
  "Run COMMAND over a data connection that resumes the control connection's
session: (values CODE DATA RESUMED-P)."
  (let ((session (session-of (client-stream client))))
    (unwind-protect
         (multiple-value-bind (socket stream) (connect-to (passive-port client :extended t))
           (unwind-protect
                (let ((code (send client command)))
                  (if (eql code 150)
                      (let* ((tls (client-tls-resuming stream session))
                             (resumed (resumed-p tls))
                             (data (read-all tls)))
                        (close-keeping-session tls)
                        (values (read-reply client) data resumed))
                      (values code nil nil)))
             (ignore-errors (sb-bsd-sockets:socket-close socket :abort t))))
      (cffi:foreign-funcall "SSL_SESSION_free" :pointer session :void))))

(test a-data-connection-can-resume-the-control-connections-session
  (with-temporary-directory (directory)
    (write-file (path directory "hello.txt") "resumed")
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "m" directory)
      (with-tls-server (port vfs)
        (with-client (client port)
          (secure-client client)
          ;; The replies to these are read after the handshake, and in TLS 1.3
          ;; it is then that the client is given what it resumes with.
          (login client)
          (send client "PBSZ 0")
          (send client "PROT P")
          (is (string= "TLSv1.3" (tls-version (client-stream client))))
          (is-false (resumed-p (client-stream client)) "the control connection is new")
          ;; More than once: one ticket has to do for every data connection.
          (dotimes (attempt 3)
            (multiple-value-bind (code data resumed) (fetch-resuming client "RETR /m/hello.txt")
              (is (= 226 code))
              (is (string= "resumed" data))
              (is-true resumed "the data connection resumed the control session"))))))))

(test resuming-does-not-bring-back-the-truncated-upload
  ;; Tickets are what resumption needs and what cut uploads short, so both at
  ;; once: a control connection that hands them out, and a large upload over a
  ;; data connection that must not be sent any.
  (with-temporary-directory (directory)
    (let ((vfs (fs:make-vfs))
          (text (make-string 1500000 :initial-element #\x)))
      (fs:vfs-add vfs "m" directory :writable t)
      (with-tls-server (port vfs)
        (with-client (client port)
          (secure-client client)
          (login client)
          (send client "PBSZ 0")
          (send client "PROT P")
          (is (= 226 (tls-store client "STOR /m/big.txt" text)))
          (is (= 1500000 (length (read-file (path directory "big.txt")))))
          (multiple-value-bind (code data resumed) (fetch-resuming client "RETR /m/big.txt")
            (is (= 226 code))
            (is (= 1500000 (length data)))
            (is-true resumed)))))))
