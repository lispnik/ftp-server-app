;;;; session.lisp -- one client: its control connection, and the commands that
;;;; open a data connection.
;;;;
;;;; Passive mode only.  The client asks with PASV or EPSV, the session listens
;;;; on a port of its own and says which, and the client connects to it for the
;;;; one listing or file that follows.

(in-package #:ftp-server)

(defparameter *control-timeout* 300
  "Seconds a client may say nothing before it is dropped.")

(defparameter *data-timeout* 60
  "Seconds a data connection may stall before the transfer is abandoned.")

(defparameter *data-accept-timeout* 30
  "Seconds to wait for the client to connect for its data.")

(defparameter *transfer-buffer-size* 65536)

(defun make-session (server socket)
  (make-instance 'session :server server
                          :vfs (server-vfs server)
                          :socket socket
                          :stream (octet-stream socket :timeout *control-timeout*)
                          :peer (peer-address socket)))

(defun session-stopping-p (session)
  (server-stopping-p (session-server session)))

(defun session-reply (session code text)
  (write-reply (session-stream session) code text))

(defun session-close-passive (session)
  (close-quietly (shiftf (session-passive session) nil)))

(defun session-close-data (session)
  "Close whatever data sockets the session has.  Its own thread calls this."
  (session-close-passive session)
  (close-quietly (shiftf (session-data session) nil)))

(defun session-interrupt (session)
  "Make the session's thread stop waiting, from another thread: its reads see
the end of the stream, and it closes its own sockets on the way out."
  (shutdown-quietly (session-socket session))
  (shutdown-quietly (session-data session)))

(defun activity-path (session verb argument cwd)
  "The path the command was about, as the client knows it, from the directory
CWD it was sent in."
  (or (ignore-errors
       (virtual-path-string
        (cond ((member verb '("CDUP" "XCUP") :test #'string=)
               (session-cwd session))
              ((member verb '("LIST" "NLST") :test #'string=)
               (parse-virtual-path cwd (strip-list-options argument)))
              (t (parse-virtual-path cwd (or argument ""))))))
      (or argument "")))

(defun run-command (session verb argument)
  "Run one command, answer the client, and say what it did."
  (let ((cwd (session-cwd session))
        (from (third (session-rename-from session))))
    (setf (session-transferred session) 0)
    (multiple-value-bind (code text) (dispatch session verb argument)
      ;; Said before the reply is sent: the reply can fail, if the client has
      ;; gone, and what happened to the file is true either way.
      (let ((sentence (describe-activity verb (activity-path session verb argument cwd)
                                         code text
                                         :size (session-transferred session)
                                         :from from)))
        (when sentence
          (server-emit (session-server session) :activity
                       (session-user session) (session-peer session) sentence)))
      (session-reply session code text)
      ;; AUTH was agreed to, and its reply has gone: the handshake, and from
      ;; here on an encrypted stream in place of the plain one.
      (when (shiftf (session-pending-tls session) nil)
        (setf (session-stream session)
              (funcall (server-tls (session-server session)) (session-stream session))
              (session-secure-p session) t)))))

(defun run-session (session)
  "Greet the client, then read and answer commands until it leaves."
  (handler-case
      (progn
        (session-reply session 220 "FTP Server ready.")
        (loop until (or (session-quit-p session) (session-stopping-p session))
              do (let ((line (read-crlf-line (session-stream session))))
                   (cond ((null line) (return))
                         ((eq line :too-long)
                          (session-reply session 500 "Line too long."))
                         ((string= "" (string-trim " " line)))
                         (t
                          (multiple-value-bind (verb argument) (parse-command-line line)
                            (run-command session verb argument)))))))
    ;; The client went away, or said nothing for too long.
    (stream-error () nil)
    (sb-sys:io-timeout () nil)
    (sb-bsd-sockets:socket-error () nil)
    ;; Anything else is worth a line, unless TLS is in it: a handshake that
    ;; fails and a client that hangs up without saying goodbye are both
    ;; signalled by the TLS library in conditions of its own, and both are
    ;; only a client going away.
    (error (condition)
      (unless (or (session-secure-p session)
                  (server-tls (session-server session)))
        (server-log (session-server session) "session: ~a" condition)))))

;;; Opening a data connection ---------------------------------------------------------

(defun session-counter (session)
  "A function that adds to the session's count of octets transferred."
  (lambda (octets) (incf (session-transferred session) octets)))

(defun open-passive (session)
  "Listen for a data connection on the address the client reached us at, on any
free port, and answer the socket."
  (session-close-passive session)
  (let ((local (local-address (session-socket session))))
    (setf (session-passive session)
          (handler-case (listen-on local 0 :backlog 1)
            ;; A link-local IPv6 address cannot be bound without its interface,
            ;; which SOCKET-NAME does not say.  Every address of that family,
            ;; then; who may connect is checked when someone does.
            (sb-bsd-sockets:socket-error ()
              (listen-on (if (ipv6-address-p local) *any-address6* *any-address*)
                         0 :backlog 1))))))

(define-command "PASV" (session argument)
  (if (ipv6-address-p (local-address (session-socket session)))
      ;; Its reply has room for four octets of address and no more.
      (values 522 "PASV cannot describe an IPv6 address; use EPSV.")
      (let* ((listener (open-passive session))
             (port (socket-port listener)))
        (values 227 (format nil "Entering Passive Mode (~{~d~^,~},~d,~d)."
                            (coerce (local-address (session-socket session)) 'list)
                            (ash port -8) (logand port 255))))))

(define-command "EPSV" (session argument)
  (if (and argument (string-equal "ALL" (string-trim " " argument)))
      (values 200 "EPSV ALL accepted.")
      (values 229 (format nil "Entering Extended Passive Mode (|||~d|)."
                          (socket-port (open-passive session))))))

(define-condition transfer-failed (error) ())

(defun call-with-data-connection (session function)
  "Call FUNCTION with a stream on the data connection the client was promised,
and answer the reply that ends the transfer."
  (let ((listener (session-passive session))
        (protect (session-protect-data session)))
    (cond
      ((and (server-require-tls (session-server session)) (not protect))
       (values 522 "Data connections must be encrypted; send PROT P."))
      ((null listener)
       (values 425 "Use PASV or EPSV first."))
      (t
       (session-reply session 150 "Opening the data connection.")
       (let ((data (accept-with-timeout listener *data-accept-timeout*
                                        (lambda () (session-stopping-p session)))))
         (session-close-passive session)
         (cond
           ((null data)
            (values 425 "The data connection was never opened."))
           ;; Whoever connects to the port gets the file, so it has to be the
           ;; client that asked.
           ((not (equalp (peer-address data) (session-peer session)))
            (close-quietly data)
            (values 425 "The data connection came from somewhere else."))
           (t
            (setf (session-data session) data)
            (unwind-protect
                 (handler-case
                     (let ((stream (octet-stream data :timeout *data-timeout*)))
                       (when protect
                         (setf stream (funcall (server-tls (session-server session))
                                               stream)))
                       (funcall function stream)
                       (finish-output stream)
                       ;; Closing an encrypted stream is what tells the client
                       ;; that the data ended rather than was cut off.
                       (when protect
                         (ignore-errors (close stream)))
                       (values 226 "Transfer complete."))
                   ((or stream-error sb-sys:io-timeout sb-bsd-sockets:socket-error
                     transfer-failed) ()
                     (values 426 "The connection closed; transfer aborted."))
                   ;; The TLS library's own conditions, which this file cannot name.
                   (error (condition)
                     (if protect
                         (values 426 "The connection closed; transfer aborted.")
                         (error condition))))
              (close-quietly (shiftf (session-data session) nil))))))))))

;;; Listings ------------------------------------------------------------------------

(defun strip-list-options (argument)
  "ARGUMENT without the ls options some clients put first, as in LIST -la."
  (let ((argument (string-left-trim " " (or argument ""))))
    (loop while (and (plusp (length argument)) (char= #\- (char argument 0)))
          do (let ((space (position #\Space argument)))
               (setf argument (if space
                                  (string-left-trim " " (subseq argument space))
                                  ""))))
    argument))

(defun entries-for-listing (session argument)
  "The entries a listing of ARGUMENT shows: a directory's, or a file's own."
  (multiple-value-bind (kind mapping host-path components)
      (resolve-existing session argument)
    (ecase kind
      (:root (root-entries (session-vfs session)))
      ((:mapping-root :inside)
       (if (eq :directory (host-file-type host-path))
           (list-directory host-path
                           :root-real (real-path (mapping-host-path mapping))
                           :writable (mapping-writable mapping))
           (list (or (host-entry (first (last components)) host-path
                                 :writable (mapping-writable mapping))
                     (error 'vfs-not-found))))))))

(defun send-listing (session argument line-function)
  (let ((entries (entries-for-listing session argument)))
    (call-with-data-connection
     session
     (lambda (stream)
       (dolist (entry entries)
         (write-line-crlf stream (funcall line-function entry)))))))

(define-command "LIST" (session argument)
  (send-listing session (strip-list-options argument) #'format-list-line))

(define-command "NLST" (session argument)
  (send-listing session (strip-list-options argument) #'entry-name))

(define-command "MLSD" (session argument)
  (send-listing session argument
                (lambda (entry)
                  (concatenate 'string (format-mlsx-facts entry) (entry-name entry)))))

;;; Files -------------------------------------------------------------------------

(defun copy-octets (from to count)
  "Copy FROM to TO, calling COUNT with the size of each piece as it goes."
  (let ((buffer (make-array *transfer-buffer-size* :element-type '(unsigned-byte 8))))
    (loop for end = (read-sequence buffer from)
          while (plusp end)
          do (write-sequence buffer to :end end)
             (funcall count end))))

;;; ASCII transfers ---------------------------------------------------------------
;;;
;;; In TYPE A a line ends in CR LF on the wire, whatever it ends in on either
;;; host.  Here it ends in LF, so a file going out gains a CR before each LF and
;;; a file coming in loses it.  Both are done a buffer at a time, and both have
;;; to remember one octet across the join between two buffers.

(defconstant +cr+ 13)
(defconstant +lf+ 10)

(defun ascii-encode (in end out previous)
  "Copy IN, up to END, into OUT with a CR put before each LF that has none.
PREVIOUS is the octet before IN, or NIL.  OUT must be twice the size of IN.
Answers how much of OUT was filled and the last octet of IN.

An LF that already follows a CR is left alone, so that a file kept with CR LF
endings goes out as it is rather than with two CRs to a line."
  (let ((fill 0))
    (dotimes (index end)
      (let ((octet (aref in index)))
        (when (and (= octet +lf+) (not (eql previous +cr+)))
          (setf (aref out fill) +cr+)
          (incf fill))
        (setf (aref out fill) octet
              previous octet)
        (incf fill)))
    (values fill previous)))

(defun ascii-decode (in end out pending-cr)
  "Copy IN, up to END, into OUT with each CR LF made an LF.  PENDING-CR says
the buffer before this one ended in a CR that has not been written yet.  OUT
must be one octet larger than IN.  Answers how much of OUT was filled and
whether this buffer in turn ends in a CR held back.

A CR that no LF follows is not a line ending and is kept."
  (let ((fill 0))
    (dotimes (index end)
      (let ((octet (aref in index)))
        (when (and pending-cr (/= octet +lf+))
          (setf (aref out fill) +cr+)
          (incf fill))
        (setf pending-cr (= octet +cr+))
        (unless pending-cr
          (setf (aref out fill) octet)
          (incf fill))))
    (values fill pending-cr)))

(defun copy-octets-to-ascii (from to count)
  "Send the file FROM down the data connection TO, in ASCII.  COUNT is called
with how much of the file each piece was."
  (let ((in (make-array *transfer-buffer-size* :element-type '(unsigned-byte 8)))
        (out (make-array (* 2 *transfer-buffer-size*) :element-type '(unsigned-byte 8)))
        (previous nil))
    (loop for end = (read-sequence in from)
          while (plusp end)
          do (multiple-value-bind (fill last) (ascii-encode in end out previous)
               (setf previous last)
               (write-sequence out to :end fill)
               (funcall count end)))))

(defun copy-octets-from-ascii (from to count)
  "Receive the data connection FROM, in ASCII, into the file TO.  COUNT is
called with how much each piece put in the file."
  (let ((in (make-array *transfer-buffer-size* :element-type '(unsigned-byte 8)))
        (out (make-array (1+ *transfer-buffer-size*) :element-type '(unsigned-byte 8)))
        (pending-cr nil))
    (loop for end = (read-sequence in from)
          while (plusp end)
          do (multiple-value-bind (fill pending) (ascii-decode in end out pending-cr)
               (setf pending-cr pending)
               (write-sequence out to :end fill)
               (funcall count fill)))
    ;; The very last octet was a CR, with nothing after it to decide by.
    (when pending-cr
      (write-byte +cr+ to)
      (funcall count 1))))

(defun open-host-file (host-path flags &optional (mode #o644))
  "A stream of octets on HOST-PATH, opened with FLAGS and never through a
symbolic link."
  (let ((fd (sb-posix:open host-path (logior flags sb-posix:o-nofollow) mode)))
    (sb-sys:make-fd-stream fd
                           :input (not (logtest flags (logior sb-posix:o-wronly
                                                              sb-posix:o-rdwr)))
                           :output (logtest flags (logior sb-posix:o-wronly
                                                          sb-posix:o-rdwr))
                           :element-type '(unsigned-byte 8)
                           :buffering :full
                           :auto-close t)))

(define-command "RETR" (session argument)
  (require-argument argument)
  (multiple-value-bind (kind mapping host-path) (resolve-existing session argument)
    (declare (ignore mapping))
    (unless (and (eq kind :inside) (eq :file (host-file-type host-path)))
      (error 'vfs-error :message "Not a plain file."))
    (let ((offset (session-rest-offset session)))
      ;; HOST-PATH is already a real path, so there is no link left to follow.
      (with-open-stream (file (open-host-file host-path sb-posix:o-rdonly))
        (when (plusp offset)
          (file-position file offset))
        (call-with-data-connection
         session
         (lambda (stream)
           (funcall (if (eq :ascii (session-transfer-type session))
                        #'copy-octets-to-ascii
                        #'copy-octets)
                    file stream (session-counter session))))))))

(defun store-file (session argument append)
  (multiple-value-bind (mapping host-path) (resolve-for-change session argument)
    (declare (ignore mapping))
    (when (member (host-file-type host-path) '(:directory :symlink :other))
      (error 'vfs-denied))
    ;; Before the file is opened, which is when it is emptied.
    (unless (session-passive session)
      (return-from store-file (values 425 "Use PASV or EPSV first.")))
    (let* ((offset (session-rest-offset session))
           (flags (logior sb-posix:o-wronly sb-posix:o-creat
                          (cond (append sb-posix:o-append)
                                ((plusp offset) 0)
                                (t sb-posix:o-trunc))))
           (file (open-host-file host-path flags)))
      (unwind-protect
           (progn
             (when (and (plusp offset) (not append))
               (file-position file offset))
             (call-with-data-connection
              session
              (lambda (stream)
                (handler-case (progn
                                (funcall (if (eq :ascii (session-transfer-type session))
                                             #'copy-octets-from-ascii
                                             #'copy-octets)
                                         stream file (session-counter session))
                                (finish-output file))
                  ;; The disk, not the connection: still an aborted transfer.
                  (file-error () (error 'transfer-failed))))))
        (ignore-errors (close file))))))

(define-command "STOR" (session argument)
  (store-file session argument nil))

(define-command "APPE" (session argument)
  (store-file session argument t))
