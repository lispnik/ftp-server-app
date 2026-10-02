;;;; net.lisp -- sockets, and the lines that go over them.
;;;;
;;;; SB-BSD-SOCKETS rather than a portable layer: the server wants an accept
;;;; that gives up, the address a connection arrived at, and a way to wake a
;;;; thread that is blocked reading, and all three are here directly.

(in-package #:ftp-server)

(defparameter *poll-interval* 0.25
  "How long a blocked accept goes without looking at its stop flag, in seconds.")

(defun listen-on (address port &key (backlog 16))
  "A socket listening on ADDRESS, a vector of four octets, and PORT.  Port 0
asks for any free port; SOCKET-PORT says which was given."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (done nil))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
           (sb-bsd-sockets:socket-bind socket address port)
           (sb-bsd-sockets:socket-listen socket backlog)
           (setf done t)
           socket)
      (unless done
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

(defun socket-port (socket)
  "The local port of SOCKET."
  (nth-value 1 (sb-bsd-sockets:socket-name socket)))

(defun local-address (socket)
  "The local address of SOCKET, a vector of four octets."
  (values (sb-bsd-sockets:socket-name socket)))

(defun peer-address (socket)
  "The address at the far end of SOCKET, or NIL if it has gone."
  (ignore-errors (values (sb-bsd-sockets:socket-peername socket))))

(defun address-string (address)
  (format nil "~{~d~^.~}" (coerce address 'list)))

(defun accept-with-timeout (socket seconds stop-p)
  "The next connection to SOCKET, or NIL after SECONDS or once STOP-P answers
true.  SECONDS NIL waits for as long as STOP-P allows.

Polls rather than blocking in accept: nothing another thread can do to a
listening socket wakes a thread that is blocked accepting on it."
  (let ((deadline (and seconds
                       (+ (get-internal-real-time)
                          (* seconds internal-time-units-per-second)))))
    (loop
      (when (funcall stop-p)
        (return nil))
      (when (and deadline (> (get-internal-real-time) deadline))
        (return nil))
      (when (sb-sys:wait-until-fd-usable (sb-bsd-sockets:socket-file-descriptor socket)
                                         :input *poll-interval* nil)
        (return (handler-case (sb-bsd-sockets:socket-accept socket)
                  (sb-bsd-sockets:socket-error () nil)))))))

(defun octet-stream (socket &key timeout)
  "A buffered stream of octets on SOCKET.  TIMEOUT, in seconds, is how long a
read or write may wait before SB-SYS:IO-TIMEOUT is signalled."
  (sb-bsd-sockets:socket-make-stream socket :input t :output t
                                            :element-type '(unsigned-byte 8)
                                            :buffering :full
                                            :timeout timeout))

(defun shutdown-quietly (socket)
  "Stop SOCKET in both directions, so that a thread blocked reading it sees the
end of the stream.  The descriptor stays open and is still that thread's to
close: closing it from here could hand its number to someone else."
  (when socket
    (ignore-errors (sb-bsd-sockets:socket-shutdown socket :direction :io))))

(defun close-quietly (socket)
  (when socket
    (ignore-errors (sb-bsd-sockets:socket-close socket :abort t))))

;;; Lines -----------------------------------------------------------------------

(defconstant +telnet-iac+ 255)

(defun octets-to-line (octets)
  (sb-ext:octets-to-string octets :external-format '(:utf-8 :replacement #\?)))

(defun line-to-octets (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun read-crlf-line (stream &optional (max 4096))
  "The next line from STREAM without its CR LF: a string, NIL at the end of the
stream, or :TOO-LONG for a line of more than MAX octets, which is read to its
end and thrown away.

Telnet commands -- IAC and the octet after it, which some clients send ahead
of ABOR -- are dropped."
  (let ((octets (make-array 128 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0))
        (too-long nil))
    (loop
      (let ((octet (read-byte stream nil nil)))
        (cond ((null octet)
               (return (cond (too-long :too-long)
                             ((zerop (length octets)) nil)
                             (t (octets-to-line octets)))))
              ((= octet 10)
               (when (and (plusp (length octets))
                          (= 13 (aref octets (1- (length octets)))))
                 (decf (fill-pointer octets)))
               (return (if too-long :too-long (octets-to-line octets))))
              ((= octet +telnet-iac+)
               (read-byte stream nil nil))
              ((>= (length octets) max)
               (setf too-long t))
              (t (vector-push-extend octet octets)))))))

(defun write-line-crlf (stream string)
  (write-sequence (line-to-octets string) stream)
  (write-byte 13 stream)
  (write-byte 10 stream))

(defun reply-lines (code text)
  "The lines of a reply: one, or for a list of lines TEXT the first with a
hyphen after the code, the last with a space, and the ones between as given."
  (if (listp text)
      (loop for (line . more) on text
            for first = t then nil
            collect (cond ((and first more) (format nil "~d-~a" code line))
                          (more line)
                          (t (format nil "~d ~a" code line))))
      (list (format nil "~d ~a" code text))))

(defun write-reply (stream code text)
  "Send a reply and flush it."
  (dolist (line (reply-lines code text))
    (write-line-crlf stream line))
  (finish-output stream))
