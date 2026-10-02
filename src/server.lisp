;;;; server.lisp -- the listening socket and the threads behind it.
;;;;
;;;; One thread accepts; each connection gets a thread of its own, running
;;;; RUN-SESSION.  None of them may touch AppKit or write to a stream of the
;;;; application's: what they have to say goes to the server's ON-EVENT
;;;; function, which must return promptly and must not wait for the thread
;;;; that will call STOP-SERVER.

(in-package #:ftp-server)

(defparameter *max-sessions* 32
  "How many clients may be connected at once.")

(defparameter *loopback* #(127 0 0 1))
(defparameter *any-address* #(0 0 0 0))
(defparameter *loopback6* #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
(defparameter *any-address6* #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(defparameter *loopback-addresses* (list *loopback* *loopback6*)
  "This computer only, over IPv4 and IPv6.")
(defparameter *any-addresses* (list *any-address* *any-address6*)
  "Every interface, over IPv4 and IPv6.")

(defclass server ()
  ((vfs :initarg :vfs :reader server-vfs)
   (authenticator :initarg :authenticator :reader server-authenticator
                  :documentation "A function of a user name and a password.")
   (addresses :initarg :addresses :reader server-addresses
              :documentation "What to listen on.  The first has to work; the
rest are listened on if they can be.")
   (requested-port :initarg :port :reader server-requested-port)
   (port :initform nil :accessor server-port
         :documentation "The port being listened on, once started.")
   (on-event :initarg :on-event :reader server-on-event)
   (tls :initarg :tls :initform nil :reader server-tls
        :documentation "NIL, or a function that takes a stream of octets on a
connection that has just been accepted, does the server's half of a TLS
handshake over it, and answers the encrypted stream.  It is called with
:DATA T for a data connection.")
   (require-tls :initarg :require-tls :initform nil :reader server-require-tls
                :documentation "Refuse to take a password, or move data, in the clear.")
   (listeners :initform '() :accessor server-listeners)
   (accept-threads :initform '() :accessor server-accept-threads)
   (stopping :initform nil :accessor server-stopping-p)
   (running :initform nil :accessor server-running-p)
   (sessions :initform '() :accessor server-sessions)
   (threads :initform '() :accessor server-threads)
   (lock :initform (sb-thread:make-mutex :name "ftp-server sessions")
         :reader server-lock)))

(defun make-server (&key vfs authenticator (addresses *loopback-addresses*) (port 2121)
                      on-event tls require-tls)
  "A server that is not yet listening.

ON-EVENT, if given, is called on the server's threads with a keyword and its
arguments:

  :STARTED port                 :STOPPED
  :CLIENT-CONNECTED address     :CLIENT-DISCONNECTED address
  :ACTIVITY user address text   what a client did, in words
  :LOG string                   something that went wrong"
  (make-instance 'server :vfs vfs :authenticator authenticator
                         :addresses addresses :port port :on-event on-event
                         :tls tls :require-tls require-tls))

(defun server-emit (server event &rest arguments)
  (let ((function (server-on-event server)))
    (when function
      (ignore-errors (apply function event arguments)))))

(defun server-log (server control &rest arguments)
  (server-emit server :log (apply #'format nil control arguments)))

(defun server-session-count (server)
  (sb-thread:with-mutex ((server-lock server))
    (length (server-sessions server))))

(defmacro guarding-thread ((server what) &body body)
  "Run BODY so that nothing it signals leaves the thread: the application is
built with the debugger disabled, where an unhandled error ends the process."
  `(handler-case (progn ,@body)
     (serious-condition (condition)
       (server-log ,server "~a: ~a" ,what condition))))

(defun serve-connection (server socket)
  "Run a session on SOCKET, on this thread, and close it afterwards."
  (let ((session nil)
        (address (peer-address socket)))
    (unwind-protect
         (guarding-thread (server "session")
           (setf session (make-session server socket))
           (let ((admitted
                   (sb-thread:with-mutex ((server-lock server))
                     (when (and (not (server-stopping-p server))
                                (< (length (server-sessions server)) *max-sessions*))
                       (push session (server-sessions server))
                       t))))
             (cond (admitted
                    (server-emit server :client-connected address)
                    (unwind-protect (run-session session)
                      (sb-thread:with-mutex ((server-lock server))
                        (setf (server-sessions server)
                              (remove session (server-sessions server))))
                      (server-emit server :client-disconnected address)))
                   (t
                    (ignore-errors
                     (write-reply (session-stream session) 421
                                  "Too many connections; try again later."))))))
      (when session
        (session-close-data session))
      (close-quietly socket)
      (sb-thread:with-mutex ((server-lock server))
        (setf (server-threads server)
              (remove sb-thread:*current-thread* (server-threads server)))))))

(defun accept-loop (server listener)
  (progn
    (unwind-protect
         (guarding-thread (server "accept")
           (loop
             (let ((socket (accept-with-timeout
                            listener nil (lambda () (server-stopping-p server)))))
               (when (or (null socket) (server-stopping-p server))
                 (close-quietly socket)
                 (return))
               (sb-thread:with-mutex ((server-lock server))
                 (push (sb-thread:make-thread #'serve-connection
                                              :name "ftp-server session"
                                              :arguments (list server socket))
                       (server-threads server))))))
      (close-quietly listener))))

(defun start-server (server)
  "Start listening.  The socket is bound here, on the caller's thread, so that
a port already in use is the caller's error to report."
  (when (server-running-p server)
    (error "The server is already running."))
  (let* ((addresses (server-addresses server))
         (first (listen-on (first addresses) (server-requested-port server)))
         (port (socket-port first))
         ;; The same port on each of the others.  One that cannot be had -- a
         ;; machine with no IPv6, say -- is done without.
         (listeners (cons first
                          (loop for address in (rest addresses)
                                for listener = (handler-case (listen-on address port)
                                                 (error (condition)
                                                   (server-log server
                                                               "not listening on ~a: ~a"
                                                               (address-string address)
                                                               condition)
                                                   nil))
                                when listener collect listener))))
    (setf (server-listeners server) listeners
          (server-port server) port
          (server-stopping-p server) nil
          (server-running-p server) t
          (server-accept-threads server)
          (loop for listener in listeners
                collect (sb-thread:make-thread #'accept-loop :name "ftp-server accept"
                                                             :arguments (list server listener)))))
  (server-emit server :started (server-port server))
  server)

(defun stop-server (server &key (timeout 2))
  "Stop listening and end every session, waiting at most TIMEOUT seconds for
the threads to finish.  A thread that has not finished by then is left to
finish on its own: its sockets are already shut down."
  (when (server-running-p server)
    (setf (server-stopping-p server) t)
    (let ((sessions (sb-thread:with-mutex ((server-lock server))
                      (copy-list (server-sessions server)))))
      (dolist (session sessions)
        (session-interrupt session)))
    (let ((deadline (+ (get-internal-real-time)
                       (* timeout internal-time-units-per-second)))
          (threads (append (server-accept-threads server)
                           (sb-thread:with-mutex ((server-lock server))
                             (copy-list (server-threads server))))))
      (dolist (thread threads)
        (let ((remaining (/ (- deadline (get-internal-real-time))
                            internal-time-units-per-second)))
          (sb-thread:join-thread thread :default nil
                                        :timeout (max 0.01 (float remaining))))))
    (setf (server-running-p server) nil
          (server-accept-threads server) '()
          (server-listeners server) '())
    (server-emit server :stopped))
  server)
