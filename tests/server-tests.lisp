;;;; server-tests.lisp -- a real server on a loopback port, and a client.

(in-package #:ftp-server/tests)

(def-suite server :in all-tests :description "The server, over real sockets.")
(in-suite server)

;;; A client just large enough to test with -----------------------------------------

(defstruct client socket stream)

(defun connect-to (port)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket #(127 0 0 1) port)
    (values socket (fs::octet-stream socket :timeout 10))))

(defun read-reply (client)
  "The next reply: (values CODE LINES)."
  (let ((lines '()))
    (loop
      (let ((line (fs::read-crlf-line (client-stream client))))
        (when (null line)
          (return (values nil (nreverse lines))))
        (push line lines)
        (when (and (>= (length line) 4)
                   (every #'digit-char-p (subseq line 0 3))
                   (char= #\Space (char line 3)))
          (return (values (parse-integer line :end 3) (nreverse lines))))))))

(defun send (client line)
  "Send LINE and answer its reply."
  (fs::write-line-crlf (client-stream client) line)
  (finish-output (client-stream client))
  (read-reply client))

(defun open-client (port)
  (multiple-value-bind (socket stream) (connect-to port)
    (let ((client (make-client :socket socket :stream stream)))
      (assert (eql 220 (read-reply client)))
      client)))

(defun close-client (client)
  (ignore-errors (sb-bsd-sockets:socket-close (client-socket client) :abort t)))

(defun login (client &optional (user "user") (password "secret"))
  (send client (format nil "USER ~a" user))
  (send client (format nil "PASS ~a" password)))

(defun passive-port (client &key extended)
  "Ask for a data port, with PASV or with EPSV, and answer it."
  (multiple-value-bind (code lines) (send client (if extended "EPSV" "PASV"))
    (let* ((line (first lines))
           (open (position #\( line))
           (close (position #\) line))
           (inside (subseq line (1+ open) close)))
      (cond (extended
             (assert (eql 229 code))
             (parse-integer (string-trim "|" inside)))
            (t
             (assert (eql 227 code))
             (let ((numbers (mapcar #'parse-integer (uiop:split-string inside :separator ","))))
               (+ (* 256 (fifth numbers)) (sixth numbers))))))))

(defun read-all (stream)
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (loop for octet = (read-byte stream nil nil)
          while octet do (vector-push-extend octet octets))
    (sb-ext:octets-to-string octets :external-format :utf-8)))

(defun fetch (client command &key extended)
  "Run COMMAND, which sends data, and answer (values CODE DATA)."
  (multiple-value-bind (socket stream) (connect-to (passive-port client :extended extended))
    (unwind-protect
         (let ((code (send client command)))
           (if (eql code 150)
               (let ((data (read-all stream)))
                 (values (read-reply client) data))
               (values code nil)))
      (ignore-errors (sb-bsd-sockets:socket-close socket :abort t)))))

(defun store (client command contents)
  "Run COMMAND, which receives data, sending CONTENTS.  Answers the last code."
  (multiple-value-bind (socket stream) (connect-to (passive-port client))
    (unwind-protect
         (let ((code (send client command)))
           (cond ((eql code 150)
                  (write-sequence (sb-ext:string-to-octets contents :external-format :utf-8)
                                  stream)
                  (finish-output stream)
                  (sb-bsd-sockets:socket-close socket)
                  (read-reply client))
                 (t code)))
      (ignore-errors (sb-bsd-sockets:socket-close socket :abort t)))))

(defun lines-of (data)
  (remove "" (uiop:split-string (remove #\Return data) :separator '(#\Newline))
          :test #'string=))

;;; A server to test against ----------------------------------------------------------

(defun call-with-server (vfs function)
  (let* ((events (list))
         (lock (sb-thread:make-mutex))
         (server (fs:make-server
                  :vfs vfs
                  :authenticator (lambda (user password)
                                   (and (string= user "user") (string= password "secret")))
                  :port 0
                  :on-event (lambda (&rest event)
                              (sb-thread:with-mutex (lock) (push event events)))))
         (delay fs::*login-failure-delay*))
    ;; Set, not bound: the session threads read the global value.
    (setf fs::*login-failure-delay* 0)
    (fs:start-server server)
    (unwind-protect
         (funcall function server
                  (lambda () (sb-thread:with-mutex (lock) (reverse events))))
      (fs:stop-server server)
      (setf fs::*login-failure-delay* delay))))

(defmacro with-server ((server port vfs &optional (events (gensym "EVENTS"))) &body body)
  "Run BODY with SERVER listening on PORT over VFS.  EVENTS is a function
answering the events so far."
  `(call-with-server ,vfs
                     (lambda (,server ,events)
                       (declare (ignorable ,events))
                       (let ((,port (fs:server-port ,server)))
                         ,@body))))

(defmacro with-client ((client port) &body body)
  `(let ((,client (open-client ,port)))
     (unwind-protect (progn ,@body)
       (close-client ,client))))

(defun wait-until (predicate &optional (seconds 3))
  (loop repeat (* seconds 100)
        when (funcall predicate) return t
        do (sleep 0.01)))

;;; The tests ------------------------------------------------------------------------

(test the-server-takes-a-free-port-and-greets
  (with-server (server port (fs:make-vfs))
    (is (plusp port))
    (is-true (fs:server-running-p server))
    (with-client (client port)
      (is (= 215 (send client "SYST"))))))

(test a-wrong-password-is-refused
  (with-server (server port (fs:make-vfs))
    (with-client (client port)
      (is (= 530 (login client "user" "wrong")))
      (is (= 530 (send client "PWD")))
      (is (= 230 (login client)))
      (is (= 257 (send client "PWD"))))))

(test the-mapped-name-is-what-the-root-lists
  ;; The whole point: map a directory as "tempdir", log in, ls.
  (with-temporary-directory (directory)
    (write-file (path directory "hello.txt") "hello")
    (sb-posix:mkdir (path directory "sub") #o755)
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "tempdir" directory)
      (with-server (server port vfs)
        (with-client (client port)
          (login client)
          (multiple-value-bind (code data) (fetch client "LIST")
            (is (= 226 code))
            (let ((lines (lines-of data)))
              (is (= 1 (length lines)))
              (is (char= #\d (char (first lines) 0)))
              (is (search " tempdir" (first lines)))))
          (multiple-value-bind (code data) (fetch client "NLST")
            (is (= 226 code))
            (is (equal '("tempdir") (lines-of data))))
          (is (= 250 (send client "CWD tempdir")))
          (multiple-value-bind (code data) (fetch client "NLST" :extended t)
            (is (= 226 code))
            (is (equal '("hello.txt" "sub") (lines-of data))))
          (multiple-value-bind (code data) (fetch client "LIST -la")
            (is (= 226 code))
            (is (= 2 (length (lines-of data)))))
          (multiple-value-bind (code data) (fetch client "MLSD /tempdir")
            (is (= 226 code))
            (is (search "type=file;size=5;" (first (lines-of data))))
            (is (search "type=dir;" (second (lines-of data))))))))))

(test adding-and-removing-a-mapping-shows-at-once
  (let ((vfs (fs:make-vfs)))
    (with-server (server port vfs)
      (with-client (client port)
        (login client)
        (is (equal '() (lines-of (nth-value 1 (fetch client "NLST")))))
        (fs:vfs-add vfs "tempdir" "/tmp")
        (is (equal '("tempdir") (lines-of (nth-value 1 (fetch client "NLST")))))
        (fs:vfs-remove vfs "tempdir")
        (is (equal '() (lines-of (nth-value 1 (fetch client "NLST")))))
        (is (= 550 (send client "CWD tempdir")))))))

(test a-file-comes-back-as-it-is
  (with-temporary-directory (directory)
    (write-file (path directory "hello.txt") (format nil "hello~%world~%"))
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "m" directory)
      (with-server (server port vfs)
        (with-client (client port)
          (login client)
          (multiple-value-bind (code data) (fetch client "RETR /m/hello.txt")
            (is (= 226 code))
            (is (string= (format nil "hello~%world~%") data)))
          (is (= 350 (send client "REST 6")))
          (multiple-value-bind (code data) (fetch client "RETR /m/hello.txt")
            (is (= 226 code))
            (is (string= (format nil "world~%") data)))
          ;; REST was for that one transfer.
          (is (= 12 (length (nth-value 1 (fetch client "RETR /m/hello.txt")))))
          (is (= 550 (fetch client "RETR /m/missing.txt")))
          (is (= 550 (fetch client "RETR /m"))))))))

(test storing-needs-a-writable-mapping
  (with-temporary-directory (directory)
    (sb-posix:mkdir (path directory "ro") #o755)
    (sb-posix:mkdir (path directory "rw") #o755)
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "ro" (path directory "ro"))
      (fs:vfs-add vfs "rw" (path directory "rw") :writable t)
      (with-server (server port vfs)
        (with-client (client port)
          (login client)
          (is (= 550 (store client "STOR /ro/new.txt" "no")))
          (is (null (fs::host-file-type (path directory "ro" "new.txt"))))
          (is (= 226 (store client "STOR /rw/new.txt" "first")))
          (is (string= "first" (read-file (path directory "rw" "new.txt"))))
          (is (= 226 (store client "STOR /rw/new.txt" "2nd")) "replaced, not overlaid")
          (is (string= "2nd" (read-file (path directory "rw" "new.txt"))))
          (is (= 226 (store client "APPE /rw/new.txt" " and more")))
          (is (string= "2nd and more" (read-file (path directory "rw" "new.txt"))))
          ;; Making the mapping read-only takes effect on the next command.
          (setf (fs:mapping-writable (fs:vfs-find vfs "rw")) nil)
          (is (= 550 (store client "STOR /rw/new.txt" "no")))
          (is (string= "2nd and more" (read-file (path directory "rw" "new.txt")))))))))

(test a-link-out-of-the-mapping-cannot-be-fetched
  (with-temporary-directory (directory)
    (sb-posix:mkdir (path directory "m") #o755)
    (write-file (path directory "secret.txt") "secret")
    (sb-posix:symlink (path directory "secret.txt") (path directory "m" "link"))
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "m" (path directory "m"))
      (with-server (server port vfs)
        (with-client (client port)
          (login client)
          (is (= 550 (fetch client "RETR /m/link")))
          (is (= 550 (fetch client "RETR /m/../secret.txt")))
          (is (equal '() (lines-of (nth-value 1 (fetch client "NLST /m"))))))))))

(test the-server-says-who-came-and-went
  (with-server (server port (fs:make-vfs) events)
    (with-client (client port)
      (login client)
      (is-true (wait-until (lambda () (= 1 (fs:server-session-count server)))))
      (is (= 221 (send client "QUIT"))))
    (is-true (wait-until (lambda () (zerop (fs:server-session-count server)))))
    (is-true (wait-until (lambda () (find :client-disconnected (funcall events)
                                          :key #'first))))
    (let ((kinds (mapcar #'first (funcall events))))
      (is (eq :started (first kinds)))
      (is (member :client-connected kinds))
      (is (member :log kinds)))))

(test stopping-ends-a-session-that-is-waiting
  (let* ((vfs (fs:make-vfs))
         (server (fs:make-server :vfs vfs :port 0
                                 :authenticator (constantly t))))
    (fs:start-server server)
    (let ((client (open-client (fs:server-port server)))
          (start (get-internal-real-time)))
      (unwind-protect
           (progn
             (is-true (wait-until (lambda () (= 1 (fs:server-session-count server)))))
             (fs:stop-server server :timeout 3)
             (is (< (/ (- (get-internal-real-time) start) internal-time-units-per-second)
                    2.5)
                 "the session did not have to time out")
             (is-false (fs:server-running-p server))
             (is (zerop (fs:server-session-count server)))
             ;; The client sees the connection close.
             (is (null (ignore-errors (send client "NOOP")))))
        (close-client client)))))

(test the-port-is-free-again-after-stopping
  (let ((server (fs:make-server :vfs (fs:make-vfs) :port 0 :authenticator (constantly t))))
    (fs:start-server server)
    (let ((port (fs:server-port server)))
      (fs:stop-server server)
      (let ((again (fs:make-server :vfs (fs:make-vfs) :port port
                                   :authenticator (constantly t))))
        (fs:start-server again)
        (unwind-protect
             (with-client (client port)
               (is (= 200 (send client "NOOP"))))
          (fs:stop-server again))))))

(test a-port-in-use-is-the-callers-error
  (with-server (server port (fs:make-vfs))
    (let ((second (fs:make-server :vfs (fs:make-vfs) :port port
                                  :authenticator (constantly t))))
      (signals sb-bsd-sockets:address-in-use-error (fs:start-server second))
      (is-false (fs:server-running-p second)))))

(test a-client-that-leaves-mid-transfer-does-not-hurt-the-server
  (with-temporary-directory (directory)
    (with-open-file (out (sb-ext:parse-native-namestring (path directory "big.bin"))
                         :direction :output :element-type '(unsigned-byte 8))
      (write-sequence (make-array (* 8 1024 1024) :element-type '(unsigned-byte 8)
                                                  :initial-element 65)
                      out))
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "m" directory)
      (with-server (server port vfs)
        (with-client (client port)
          (login client)
          (multiple-value-bind (socket stream) (connect-to (passive-port client))
            (is (= 150 (send client "RETR /m/big.bin")))
            (read-byte stream)
            (sb-bsd-sockets:socket-close socket :abort t))
          (is (= 426 (read-reply client)))
          (is (= 200 (send client "NOOP"))))
        ;; And it still takes new clients.
        (with-client (client port)
          (is (= 230 (login client))))))))

(test a-data-port-nobody-connects-to-is-given-up
  (let ((timeout fs::*data-accept-timeout*))
    (setf fs::*data-accept-timeout* 0.3)
    (unwind-protect
         (with-server (server port (fs:make-vfs))
           (with-client (client port)
             (login client)
             (passive-port client)
             (is (= 150 (send client "LIST")))
             (is (= 425 (read-reply client)))
             (is (= 200 (send client "NOOP")))))
      (setf fs::*data-accept-timeout* timeout))))

(test an-overlong-line-is-refused-and-the-session-goes-on
  (with-server (server port (fs:make-vfs))
    (with-client (client port)
      (is (= 500 (send client (make-string 5000 :initial-element #\A))))
      (is (= 200 (send client "NOOP"))))))

;;; ASCII transfers, over the wire ------------------------------------------------------

(defun write-octets (host-path octets)
  (with-open-file (out (sb-ext:parse-native-namestring host-path)
                       :direction :output :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (write-sequence octets out)))

(defun read-octets (host-path)
  (with-open-file (in (sb-ext:parse-native-namestring host-path)
                      :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun fetch-octets (client command)
  "Run COMMAND and answer (values CODE OCTETS), the data untouched."
  (multiple-value-bind (socket stream) (connect-to (passive-port client))
    (unwind-protect
         (let ((code (send client command)))
           (if (eql code 150)
               (let ((octets (make-array 0 :element-type '(unsigned-byte 8)
                                           :adjustable t :fill-pointer 0)))
                 (loop for octet = (read-byte stream nil nil)
                       while octet do (vector-push-extend octet octets))
                 (values (read-reply client)
                         (coerce octets '(simple-array (unsigned-byte 8) (*)))))
               (values code nil)))
      (ignore-errors (sb-bsd-sockets:socket-close socket :abort t)))))

(defun store-octets (client command octets)
  (multiple-value-bind (socket stream) (connect-to (passive-port client))
    (unwind-protect
         (let ((code (send client command)))
           (cond ((eql code 150)
                  (write-sequence octets stream)
                  (finish-output stream)
                  (sb-bsd-sockets:socket-close socket)
                  (read-reply client))
                 (t code)))
      (ignore-errors (sb-bsd-sockets:socket-close socket :abort t)))))

(test an-ascii-upload-is-stored-with-this-hosts-line-endings
  (with-temporary-directory (directory)
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "m" directory :writable t)
      (with-server (server port vfs)
        (with-client (client port)
          (login client)
          (is (= 200 (send client "TYPE A")))
          (is (= 226 (store-octets client "STOR /m/text.txt"
                                   (octets "one" 13 10 "two" 13 10 "three" 13 10))))
          (is (equalp (octets "one" 10 "two" 10 "three" 10)
                      (read-octets (path directory "text.txt"))))
          ;; Appended in the same way.
          (is (= 226 (store-octets client "APPE /m/text.txt" (octets "four" 13 10))))
          (is (equalp (octets "one" 10 "two" 10 "three" 10 "four" 10)
                      (read-octets (path directory "text.txt")))))))))

(test an-ascii-download-arrives-with-cr-lf
  (with-temporary-directory (directory)
    (write-octets (path directory "text.txt") (octets "one" 10 "two" 13 10 "three"))
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "m" directory)
      (with-server (server port vfs)
        (with-client (client port)
          (login client)
          (is (= 200 (send client "TYPE A")))
          (multiple-value-bind (code data) (fetch-octets client "RETR /m/text.txt")
            (is (= 226 code))
            (is (equalp (octets "one" 13 10 "two" 13 10 "three") data))))))))

(test an-image-transfer-is-untouched-before-and-after-ascii
  (with-temporary-directory (directory)
    (let ((vfs (fs:make-vfs))
          (data (octets "a" 13 10 "b" 10 "c" 13 0 255 13 10)))
      (fs:vfs-add vfs "m" directory :writable t)
      (with-server (server port vfs)
        (with-client (client port)
          (login client)
          ;; With no TYPE sent at all.
          (is (= 226 (store-octets client "STOR /m/first.bin" data)))
          (is (equalp data (read-octets (path directory "first.bin"))))
          (is (equalp data (nth-value 1 (fetch-octets client "RETR /m/first.bin"))))
          ;; And after going to ASCII and coming back.
          (is (= 200 (send client "TYPE A")))
          (is (= 200 (send client "TYPE I")))
          (is (= 226 (store-octets client "STOR /m/second.bin" data)))
          (is (equalp data (read-octets (path directory "second.bin"))))
          (is (equalp data (nth-value 1 (fetch-octets client "RETR /m/second.bin")))))))))

(test a-large-ascii-file-comes-back-as-it-went
  ;; Larger than the transfer buffer, so that line endings fall on its joins.
  (with-temporary-directory (directory)
    (let* ((vfs (fs:make-vfs))
           (lines 30000)
           (host (apply #'concatenate '(vector (unsigned-byte 8))
                        (loop for line below lines
                              collect (octets (format nil "line ~d" line) 10)))))
      (write-octets (path directory "big.txt") host)
      (fs:vfs-add vfs "m" directory :writable t)
      (with-server (server port vfs)
        (with-client (client port)
          (login client)
          (is (= 200 (send client "TYPE A")))
          (multiple-value-bind (code wire) (fetch-octets client "RETR /m/big.txt")
            (is (= 226 code))
            (is (= (+ (length host) lines) (length wire)))
            (is (= 226 (store-octets client "STOR /m/copy.txt" wire)))
            (is (equalp host (read-octets (path directory "copy.txt"))))))))))
