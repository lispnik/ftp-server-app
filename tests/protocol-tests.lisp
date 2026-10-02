;;;; protocol-tests.lisp -- the commands, called with no socket under them.

(in-package #:ftp-server/tests)

(def-suite protocol :in all-tests :description "Commands that move no data.")
(in-suite protocol)

(defun make-test-session (vfs &key (logged-in t))
  "A session on VFS with no connection, which user/secret can log in to."
  (let* ((server (fs:make-server :vfs vfs
                                 :authenticator (lambda (user password)
                                                  (and (string= user "user")
                                                       (string= password "secret")))))
         (session (make-instance 'fs::session :server server :vfs vfs)))
    (when logged-in
      (setf (fs::session-state session) :logged-in))
    session))

(defun reply (session line)
  "The reply to LINE, as a list of its code and text."
  (multiple-value-bind (verb argument) (fs::parse-command-line line)
    (multiple-value-list (fs::dispatch session verb argument))))

(defun code (session line)
  (first (reply session line)))

(defmacro with-mapped-directory ((session directory &key writable) &body body)
  "Run BODY with DIRECTORY a new directory mapped as \"m\" and SESSION logged in."
  (let ((vfs (gensym "VFS")))
    `(with-temporary-directory (,directory)
       (let ((,vfs (fs:make-vfs)))
         (fs:vfs-add ,vfs "m" ,directory :writable ,writable)
         (let ((,session (make-test-session ,vfs)))
           ,@body)))))

(test a-command-line-is-a-verb-and-the-rest
  (is (equal '("USER" "ann") (multiple-value-list (fs::parse-command-line "user ann"))))
  (is (equal '("PWD" nil) (multiple-value-list (fs::parse-command-line "PWD"))))
  (is (equal '("PWD" nil) (multiple-value-list (fs::parse-command-line "PWD "))))
  (is (equal '("CWD" "a b  c") (multiple-value-list (fs::parse-command-line "CWD a b  c")))))

(test replies-are-one-line-or-bracketed-by-the-code
  (is (equal '("200 OK.") (fs::reply-lines 200 "OK.")))
  (is (equal '("211-Features:" " SIZE" "211 End")
             (fs::reply-lines 211 '("Features:" " SIZE" "End")))))

(test nothing-much-is-allowed-before-logging-in
  (let ((session (make-test-session (fs:make-vfs) :logged-in nil)))
    (is (= 530 (code session "PWD")))
    (is (= 530 (code session "CWD /")))
    (is (= 530 (code session "PASV")))
    (is (= 530 (code session "RETR x")))
    (is (= 215 (code session "SYST")))
    (is (= 211 (code session "FEAT")))
    (is (= 502 (code session "NONSENSE")))))

(test logging-in-wants-the-right-user-and-password
  (let ((fs::*login-failure-delay* 0)
        (session (make-test-session (fs:make-vfs) :logged-in nil)))
    (is (= 503 (code session "PASS secret")))
    (is (= 331 (code session "USER user")))
    (is (= 530 (code session "PASS wrong")))
    ;; The password alone, after a failure, is not enough.
    (is (= 503 (code session "PASS secret")))
    (is (= 331 (code session "USER nobody")))
    (is (= 530 (code session "PASS secret")))
    (is (= 331 (code session "USER user")))
    (is (= 230 (code session "PASS secret")))
    (is (= 257 (code session "PWD")))))

(test the-working-directory-moves-and-stops-at-the-root
  (with-mapped-directory (session directory)
    (sb-posix:mkdir (path directory "sub") #o755)
    (write-file (path directory "file.txt") "x")
    (is (equal '(257 "\"/\" is the current directory.") (reply session "PWD")))
    (is (= 250 (code session "CWD m")))
    (is (= 250 (code session "CWD sub")))
    (is (equal '(257 "\"/m/sub\" is the current directory.") (reply session "PWD")))
    (is (= 250 (code session "CDUP")))
    (is (= 250 (code session "CDUP")))
    (is (= 250 (code session "CDUP")))
    (is (equal '(257 "\"/\" is the current directory.") (reply session "PWD")))
    (is (= 550 (code session "CWD nowhere")))
    (is (= 550 (code session "CWD m/file.txt")))
    (is (= 250 (code session "CWD /m/sub/../../m/./sub")))
    (is (= 250 (code session "CWD ../../../..")))
    (is (equal '(257 "\"/\" is the current directory.") (reply session "PWD")))))

(test a-quote-in-a-directory-name-is-doubled
  (with-mapped-directory (session directory)
    (sb-posix:mkdir (path directory "a\"b") #o755)
    (is (= 250 (code session "CWD /m/a\"b")))
    (is (equal '(257 "\"/m/a\"\"b\" is the current directory.") (reply session "PWD")))))

(test size-and-time-are-answered-for-files
  (with-mapped-directory (session directory)
    (write-file (path directory "five.txt") "12345")
    (is (equal '(213 "5") (reply session "SIZE /m/five.txt")))
    (is (= 550 (code session "SIZE /m")))
    (is (= 550 (code session "SIZE /m/missing")))
    (destructuring-bind (code text) (reply session "MDTM /m/five.txt")
      (is (= 213 code))
      (is (= 14 (length text)))
      (is (every #'digit-char-p text)))))

(test mlst-describes-one-thing
  (with-mapped-directory (session directory)
    (write-file (path directory "five.txt") "12345")
    (destructuring-bind (code lines) (reply session "MLST /m/five.txt")
      (is (= 250 code))
      (is (= 3 (length lines)))
      (is (search "type=file;size=5;" (second lines)))
      (is (search "; /m/five.txt" (second lines))))
    (destructuring-bind (code lines) (reply session "MLST /")
      (is (= 250 code))
      (is (search "type=dir;" (second lines))))))

(test a-read-only-mapping-refuses-every-change
  (with-mapped-directory (session directory)
    (write-file (path directory "keep.txt") "keep")
    (sb-posix:mkdir (path directory "sub") #o755)
    (is (= 550 (code session "DELE /m/keep.txt")))
    (is (= 550 (code session "MKD /m/new")))
    (is (= 550 (code session "RMD /m/sub")))
    (is (= 550 (code session "RNFR /m/keep.txt")))
    (is (= 550 (code session "STOR /m/new.txt")))
    (is (= 550 (code session "APPE /m/keep.txt")))
    (is (string= "keep" (read-file (path directory "keep.txt"))))
    (is (eq :directory (fs::host-file-type (path directory "sub"))))
    (is (null (fs::host-file-type (path directory "new"))))))

(test a-writable-mapping-allows-them
  (with-mapped-directory (session directory :writable t)
    (write-file (path directory "a.txt") "a")
    (is (equal '(257 "\"/m/new\" created.") (reply session "MKD /m/new")))
    (is (eq :directory (fs::host-file-type (path directory "new"))))
    (is (= 550 (code session "MKD /m/new")))
    (is (= 350 (code session "RNFR /m/a.txt")))
    (is (= 250 (code session "RNTO /m/new/b.txt")))
    (is (string= "a" (read-file (path directory "new" "b.txt"))))
    (is (= 550 (code session "RMD /m/new")) "it is not empty")
    (is (= 550 (code session "DELE /m/new")) "DELE is for files")
    (is (= 250 (code session "DELE /m/new/b.txt")))
    (is (= 250 (code session "RMD /m/new")))
    (is (null (fs::host-file-type (path directory "new"))))))

(test rnto-needs-the-rnfr-just-before-it
  (with-mapped-directory (session directory :writable t)
    (write-file (path directory "a.txt") "a")
    (is (= 503 (code session "RNTO /m/b.txt")))
    (is (= 350 (code session "RNFR /m/a.txt")))
    (is (= 200 (code session "NOOP")))
    (is (= 503 (code session "RNTO /m/b.txt")))
    (is (= 550 (code session "RNFR /m/missing")))))

(test the-root-and-the-mappings-themselves-cannot-be-changed
  (with-mapped-directory (session directory :writable t)
    (is (= 550 (code session "MKD /new")))
    (is (= 550 (code session "RMD /m")))
    (is (= 550 (code session "DELE /m")))
    (is (= 550 (code session "RNFR /m")))
    (is (eq :directory (fs::host-file-type directory)))))

(test nothing-moves-from-one-mapping-to-another
  (with-temporary-directory (directory)
    (sb-posix:mkdir (path directory "one") #o755)
    (sb-posix:mkdir (path directory "two") #o755)
    (write-file (path directory "one" "a.txt") "a")
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "one" (path directory "one") :writable t)
      (fs:vfs-add vfs "two" (path directory "two") :writable t)
      (let ((session (make-test-session vfs)))
        (is (= 350 (code session "RNFR /one/a.txt")))
        (is (= 550 (code session "RNTO /two/a.txt")))
        (is (eq :file (fs::host-file-type (path directory "one" "a.txt"))))))))

(test deleting-a-link-deletes-the-link
  (with-temporary-directory (directory)
    (sb-posix:mkdir (path directory "m") #o755)
    (write-file (path directory "outside.txt") "outside")
    (sb-posix:symlink (path directory "outside.txt") (path directory "m" "link"))
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "m" (path directory "m") :writable t)
      (let ((session (make-test-session vfs)))
        (is (= 550 (code session "SIZE /m/link")) "it cannot be read")
        (is (= 250 (code session "DELE /m/link")))
        (is (null (fs::host-file-type (path directory "m" "link"))))
        (is (string= "outside" (read-file (path directory "outside.txt"))))))))

(test nothing-is-written-through-a-link
  (with-temporary-directory (directory)
    (sb-posix:mkdir (path directory "m") #o755)
    (write-file (path directory "outside.txt") "outside")
    (sb-posix:symlink (path directory "outside.txt") (path directory "m" "link"))
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "m" (path directory "m") :writable t)
      (let ((session (make-test-session vfs)))
        (is (= 550 (code session "STOR /m/link")))
        (is (= 550 (code session "APPE /m/link")))
        (is (string= "outside" (read-file (path directory "outside.txt"))))))))

(test storing-without-a-data-connection-leaves-the-file-alone
  (with-mapped-directory (session directory :writable t)
    (write-file (path directory "a.txt") "contents")
    (is (= 425 (code session "STOR /m/a.txt")))
    (is (string= "contents" (read-file (path directory "a.txt"))))
    (is (= 425 (code session "LIST")))))

(test the-small-commands-answer
  (let ((session (make-test-session (fs:make-vfs))))
    (is (= 200 (code session "TYPE I")))
    (is (= 200 (code session "TYPE A")))
    (is (= 504 (code session "TYPE E")))
    (is (= 200 (code session "MODE S")))
    (is (= 504 (code session "MODE B")))
    (is (= 200 (code session "STRU F")))
    (is (= 200 (code session "OPTS UTF8 ON")))
    (is (= 501 (code session "OPTS MLST type;")))
    (is (= 350 (code session "REST 10")))
    (is (= 501 (code session "REST ten")))
    (is (= 502 (code session "PORT 127,0,0,1,4,1")))
    (is (= 214 (code session "HELP")))
    (is (= 221 (code session "QUIT")))
    (is-true (fs::session-quit-p session))))

;;; ASCII transfers ---------------------------------------------------------------

(defun octets (&rest parts)
  "PARTS, strings and octets, as one vector of octets: 13 is CR and 10 is LF."
  (let ((vector (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (dolist (part parts)
      (if (integerp part)
          (vector-push-extend part vector)
          (loop for char across part do (vector-push-extend (char-code char) vector))))
    (coerce vector '(simple-array (unsigned-byte 8) (*)))))

(defun encode-in-pieces (octets size)
  "OCTETS through ASCII-ENCODE, SIZE octets at a time."
  (let ((result (list))
        (previous nil)
        (out (make-array (* 2 size) :element-type '(unsigned-byte 8))))
    (loop for start from 0 below (length octets) by size
          do (let ((piece (subseq octets start (min (length octets) (+ start size)))))
               (multiple-value-bind (fill last)
                   (fs::ascii-encode piece (length piece) out previous)
                 (setf previous last)
                 (push (subseq out 0 fill) result))))
    (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse result))))

(defun decode-in-pieces (octets size)
  "OCTETS through ASCII-DECODE, SIZE octets at a time, with the CR that may be
left at the end."
  (let ((result (list))
        (pending nil)
        (out (make-array (1+ size) :element-type '(unsigned-byte 8))))
    (loop for start from 0 below (length octets) by size
          do (let ((piece (subseq octets start (min (length octets) (+ start size)))))
               (multiple-value-bind (fill held)
                   (fs::ascii-decode piece (length piece) out pending)
                 (setf pending held)
                 (push (subseq out 0 fill) result))))
    (when pending
      (push (octets 13) result))
    (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse result))))

(test ascii-going-out-puts-a-cr-before-each-bare-lf
  (is (equalp (octets "a" 13 10 "b" 13 10) (encode-in-pieces (octets "a" 10 "b" 10) 64)))
  (is (equalp (octets 13 10 13 10) (encode-in-pieces (octets 10 10) 64)))
  (is (equalp (octets "no newline") (encode-in-pieces (octets "no newline") 64)))
  (is (equalp (octets) (encode-in-pieces (octets) 64)))
  ;; A line that already ends CR LF is left as it is, and a lone CR is kept.
  (is (equalp (octets "a" 13 10 "b" 13 "c" 13 10)
              (encode-in-pieces (octets "a" 13 10 "b" 13 "c" 10) 64))))

(test ascii-coming-in-makes-cr-lf-an-lf
  (is (equalp (octets "a" 10 "b" 10) (decode-in-pieces (octets "a" 13 10 "b" 13 10) 64)))
  (is (equalp (octets 10 10) (decode-in-pieces (octets 13 10 13 10) 64)))
  ;; Only CR LF is a line ending: a CR alone, a bare LF and CR CR LF are kept.
  (is (equalp (octets "a" 13 "b") (decode-in-pieces (octets "a" 13 "b") 64)))
  (is (equalp (octets "a" 10 "b") (decode-in-pieces (octets "a" 10 "b") 64)))
  (is (equalp (octets "a" 13 10) (decode-in-pieces (octets "a" 13 13 10) 64)))
  (is (equalp (octets "end" 13) (decode-in-pieces (octets "end" 13) 64))))

(test ascii-conversion-does-not-care-where-the-buffers-join
  ;; Every piece size, so that each CR LF is at some point cut in two.
  (let ((host (octets "one" 10 "two" 13 10 10 "three" 13 "x" 13 13 10 "end" 10))
        (wire (octets "one" 13 10 "two" 13 10 13 10 13 "x" 13 13 10 "end" 13)))
    (let ((encoded (encode-in-pieces host 64))
          (decoded (decode-in-pieces wire 64)))
      (loop for size from 1 to 8
            do (is (equalp encoded (encode-in-pieces host size)))
               (is (equalp decoded (decode-in-pieces wire size)))))))

(test a-text-file-survives-going-out-and-coming-back
  (let ((text (octets "first" 10 10 "second line" 10 "third with no newline")))
    (loop for size in '(1 2 3 7 64)
          do (is (equalp text (decode-in-pieces (encode-in-pieces text size) size))))))

(test type-is-remembered-and-starts-as-image
  (let ((session (make-test-session (fs:make-vfs))))
    (is (eq :image (fs::session-transfer-type session)))
    (is (equal '(200 "Type set to A.") (reply session "TYPE A")))
    (is (eq :ascii (fs::session-transfer-type session)))
    (is (= 504 (code session "TYPE E")))
    (is (eq :ascii (fs::session-transfer-type session)) "a refused type changes nothing")
    (is (equal '(200 "Type set to I.") (reply session "TYPE L 8")))
    (is (eq :image (fs::session-transfer-type session)))
    (is (= 200 (code session "type a n")))
    (is (eq :ascii (fs::session-transfer-type session)))))

(test size-is-refused-in-ascii
  (with-mapped-directory (session directory)
    (write-file (path directory "five.txt") "12345")
    (is (= 200 (code session "TYPE A")))
    (is (= 550 (code session "SIZE /m/five.txt")))
    (is (= 200 (code session "TYPE I")))
    (is (equal '(213 "5") (reply session "SIZE /m/five.txt")))))
