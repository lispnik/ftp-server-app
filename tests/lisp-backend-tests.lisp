;;;; lisp-backend-tests.lisp -- files and directories made by Lisp.

(in-package #:ftp-server/tests)

(def-suite lisp-backend :in all-tests :description "Mappings made by Lisp.")
(in-suite lisp-backend)

(defun counting-file (name)
  "A file whose contents say how many times it has been read."
  (let ((count 0)
        (lock (sb-thread:make-mutex)))
    (fs:lisp-file name (lambda ()
                         (sb-thread:with-mutex (lock)
                           (format nil "read ~d~%" (incf count)))))))

(defun sample-tree (&key uploads)
  "A tree with a fixed file, a counting one, octets, an empty one, and a
directory made on each look."
  (fs:lisp-directory
   "root"
   (list (fs:lisp-file "hello.txt" (format nil "hello~%world~%")
                       :mtime (encode-universal-time 5 4 3 2 1 2026 0))
         (counting-file "count.txt")
         (fs:lisp-file "bytes.bin" (make-array 3 :element-type '(unsigned-byte 8)
                                                 :initial-contents '(0 255 10)))
         (fs:lisp-file "empty.txt" nil)
         (fs:lisp-directory "numbers"
                            (lambda ()
                              (loop for n from 1 to 3
                                    collect (fs:lisp-file (format nil "~d.txt" n)
                                                          (format nil "~r~%" n)))))
         (fs:lisp-directory "inbox" '()
                            :on-upload (and uploads
                                            (lambda (name octets)
                                              (funcall uploads name octets)))))))

(defmacro with-lisp-mapping ((session &key writable uploads tree) &body body)
  "Run BODY with SESSION logged in to a VFS with the tree mapped as \"gen\"."
  (let ((vfs (gensym "VFS")))
    `(let ((,vfs (fs:make-vfs)))
       (fs:vfs-add-lisp ,vfs "gen" ,(or tree `(sample-tree :uploads ,uploads))
                        :writable ,writable)
       (let ((,session (make-test-session ,vfs)))
         ,@body))))

;;; The tree itself ------------------------------------------------------------------

(test a-node-is-named-as-a-path-component-is
  (signals error (fs:lisp-file "a/b" "x"))
  (signals error (fs:lisp-file ".." "x"))
  (signals error (fs:lisp-directory "" '()))
  (is (string= "ok" (fs::lisp-node-name (fs:lisp-file "ok" "x")))))

(test octets-in-memory-read-as-a-file-would
  (let ((stream (fs::make-octet-input-stream #(1 2 3 4 5))))
    (is (= 1 (read-byte stream)))
    (let ((buffer (make-array 3 :element-type '(unsigned-byte 8))))
      (is (= 3 (read-sequence buffer stream)))
      (is (equalp #(2 3 4) buffer)))
    (is (= 4 (file-position stream)))
    (file-position stream 1)
    (is (= 2 (read-byte stream)))
    (file-position stream 5)
    (is (null (read-byte stream nil nil))))
  (let ((stream (make-instance 'fs::octet-output-stream)))
    (write-byte 7 stream)
    (write-sequence #(8 9) stream)
    (is (equalp #(7 8 9) (fs::output-stream-octets stream)))))

;;; Looking at it --------------------------------------------------------------------

(test a-lisp-mapping-is-a-directory-at-the-root-like-any-other
  (with-lisp-mapping (session)
    (is (= 250 (code session "CWD /gen")))
    (is (= 250 (code session "CWD numbers")))
    (is (equal '(257 "\"/gen/numbers\" is the current directory.") (reply session "PWD")))
    (is (= 550 (code session "CWD 1.txt")) "a file is not a directory")
    (is (= 550 (code session "CWD /gen/nothing")))
    (is (equal '("gen") (mapcar #'fs::entry-name (fs::root-entries (fs::session-vfs session)))))))

(test what-a-file-is-is-asked-of-its-function
  (with-lisp-mapping (session)
    (is (equal '(213 "12") (reply session "SIZE /gen/hello.txt")))
    (is (equal '(213 "3") (reply session "SIZE /gen/bytes.bin")))
    (is (equal '(213 "0") (reply session "SIZE /gen/empty.txt")))
    (is (equal '(213 "20260102030405") (reply session "MDTM /gen/hello.txt")))
    (is (= 550 (code session "SIZE /gen/numbers")))
    (destructuring-bind (code lines) (reply session "MLST /gen/numbers/2.txt")
      (is (= 250 code))
      (is (search "type=file;size=4;" (second lines))))))

(test nothing-in-a-lisp-mapping-can-be-deleted-made-or-renamed
  (with-lisp-mapping (session :writable t)
    (is (= 550 (code session "DELE /gen/hello.txt")))
    (is (= 550 (code session "MKD /gen/new")))
    (is (= 550 (code session "RMD /gen/numbers")))
    (is (= 350 (code session "RNFR /gen/hello.txt")))
    (is (= 550 (code session "RNTO /gen/renamed.txt")))
    (is (= 550 (code session "RNFR /gen/nothing")))))

;;; Over the wire --------------------------------------------------------------------

(defmacro with-lisp-server ((port &key writable uploads tree events) &body body)
  (let ((vfs (gensym "VFS")) (server (gensym "SERVER")))
    `(let ((,vfs (fs:make-vfs)))
       (fs:vfs-add-lisp ,vfs "gen" ,(or tree `(sample-tree :uploads ,uploads))
                        :writable ,writable)
       (with-server (,server ,port ,vfs ,@(and events (list events)))
         ,@body))))

(test a-generated-tree-lists-and-reads-like-a-folder
  (with-lisp-server (port)
    (with-client (client port)
      (login client)
      (is (equal '("gen") (lines-of (nth-value 1 (fetch client "NLST")))))
      (is (equal '("hello.txt" "count.txt" "bytes.bin" "empty.txt" "numbers" "inbox")
                 (lines-of (nth-value 1 (fetch client "NLST /gen")))))
      (is (equal '("1.txt" "2.txt" "3.txt")
                 (lines-of (nth-value 1 (fetch client "NLST /gen/numbers")))))
      (let ((listing (lines-of (nth-value 1 (fetch client "LIST /gen")))))
        (is (char= #\- (char (first listing) 0)))
        (is (search " hello.txt" (first listing)))
        (is (char= #\d (char (fifth listing) 0))))
      (is (string= (format nil "hello~%world~%")
                   (nth-value 1 (fetch client "RETR /gen/hello.txt"))))
      (is (string= (format nil "three~%")
                   (nth-value 1 (fetch client "RETR /gen/numbers/3.txt"))))
      (is (equalp #(0 255 10)
                  (nth-value 1 (fetch-octets client "RETR /gen/bytes.bin")))))))

(test a-file-is-made-again-each-time-it-is-read
  (with-lisp-server (port)
    (with-client (client port)
      (login client)
      (let ((first (nth-value 1 (fetch client "RETR /gen/count.txt")))
            (second (nth-value 1 (fetch client "RETR /gen/count.txt"))))
        (is (string/= first second))
        (is (search "read " first))))))

(test a-directory-made-by-a-function-is-made-each-time-it-is-looked-into
  (let ((names (list "a.txt")))
    (with-lisp-server (port :tree (fs:lisp-directory
                                   "root" (lambda ()
                                            (mapcar (lambda (name) (fs:lisp-file name name))
                                                    names))))
      (with-client (client port)
        (login client)
        (is (equal '("a.txt") (lines-of (nth-value 1 (fetch client "NLST /gen")))))
        (setf names (list "a.txt" "b.txt"))
        (is (equal '("a.txt" "b.txt") (lines-of (nth-value 1 (fetch client "NLST /gen")))))))))

(test rest-and-ascii-work-on-a-generated-file
  (with-lisp-server (port)
    (with-client (client port)
      (login client)
      (is (= 350 (send client "REST 6")))
      (is (string= (format nil "world~%") (nth-value 1 (fetch client "RETR /gen/hello.txt"))))
      (is (= 200 (send client "TYPE A")))
      (is (equalp (octets "hello" 13 10 "world" 13 10)
                  (nth-value 1 (fetch-octets client "RETR /gen/hello.txt")))))))

(test what-goes-wrong-in-a-function-is-the-clients-550-and-no-more
  (with-lisp-server (port :tree (fs:lisp-directory
                                 "root"
                                 (list (fs:lisp-file "fine.txt" "fine")
                                       (fs:lisp-file "broken.txt" (lambda () (error "it broke")))
                                       (fs:lisp-file "wrong.txt" (lambda () 42))
                                       (fs:lisp-directory "bad" (lambda () (error "no children"))))))
    (with-client (client port)
      (login client)
      (multiple-value-bind (code lines) (send client "SIZE /gen/broken.txt")
        (is (= 550 code))
        (is (search "it broke" (first lines)) "and it says what"))
      (is (= 550 (fetch client "RETR /gen/broken.txt")))
      (is (= 550 (fetch client "RETR /gen/wrong.txt")))
      (is (= 550 (fetch client "NLST /gen/bad")))
      ;; A listing leaves out what cannot be made, rather than failing.
      (is (equal '("fine.txt" "bad") (lines-of (nth-value 1 (fetch client "NLST /gen")))))
      (is (string= "fine" (nth-value 1 (fetch client "RETR /gen/fine.txt")))))))

(test an-upload-is-handed-whole-to-its-directorys-function
  (let ((received '())
        (lock (sb-thread:make-mutex)))
    (with-lisp-server (port :writable t
                            :uploads (lambda (name octets)
                                       (sb-thread:with-mutex (lock)
                                         (push (list name (sb-ext:octets-to-string
                                                           octets :external-format :utf-8))
                                               received))))
      (with-client (client port)
        (login client)
        (is (= 226 (store client "STOR /gen/inbox/note.txt" "dear server")))
        (is (= 226 (store client "STOR /gen/inbox/two.txt" (make-string 200000 :initial-element #\z))))
        ;; Only to a directory that takes them.
        (is (= 550 (store client "STOR /gen/numbers/4.txt" "no")))
        (is (= 550 (store client "STOR /gen/new.txt" "no")))
        ;; And whole: there is nothing to append to or go on with.
        (is (= 550 (store client "APPE /gen/inbox/note.txt" "more")))
        (is (= 350 (send client "REST 5")))
        (is (= 550 (store client "STOR /gen/inbox/note.txt" "rest"))))
      (is (equal '(("note.txt" "dear server")) (last received)))
      (is (= 200000 (length (second (first received))))))))

(test a-read-only-lisp-mapping-takes-no-uploads
  (let ((called nil))
    (with-lisp-server (port :writable nil :uploads (lambda (name octets)
                                                     (declare (ignore name octets))
                                                     (setf called t)))
      (with-client (client port)
        (login client)
        (is (= 550 (store client "STOR /gen/inbox/note.txt" "no")))))
    (is-false called)))

(test an-upload-function-that-fails-is-reported-and-the-session-goes-on
  (with-lisp-server (port :writable t
                          :uploads (lambda (name octets)
                                     (declare (ignore octets))
                                     (error "rejected ~a" name))
                          :events events)
    (with-client (client port)
      (login client)
      (multiple-value-bind (socket stream) (connect-to (passive-port client))
        (unwind-protect
             (progn
               (is (= 150 (send client "STOR /gen/inbox/x.txt")))
               (write-sequence (octets "data") stream)
               (finish-output stream)
               (sb-bsd-sockets:socket-close socket)
               (multiple-value-bind (code lines) (read-reply client)
                 (is (= 550 code) "not 226: the upload did not land")
                 (is (search "rejected x.txt" (first lines)))))
          (ignore-errors (sb-bsd-sockets:socket-close socket :abort t))))
      (is (= 200 (send client "NOOP"))))
    (is-true (wait-until
              (lambda ()
                (find-if (lambda (row) (search "could not upload /gen/inbox/x.txt" (second row)))
                         (activity (funcall events))))))))

(test the-activity-log-describes-generated-files-as-it-does-others
  (with-lisp-server (port :events events)
    (with-client (client port)
      (login client)
      (fetch client "RETR /gen/hello.txt")
      (send client "QUIT"))
    (is-true (wait-until
              (lambda ()
                (find "downloaded /gen/hello.txt (12 bytes)"
                      (activity (funcall events)) :key #'second :test #'equal))))))

;;; init.lisp and the settings -------------------------------------------------------

(test lisp-mappings-are-not-saved-with-the-folders
  (with-temporary-directory (directory)
    (let ((model (fs:make-model))
          (file (sb-ext:parse-native-namestring (path directory "settings.lisp"))))
      (fs:model-add-directory model "/tmp")
      (fs:vfs-add-lisp (fs:model-vfs model) "gen" (sample-tree))
      (fs:model-save model file)
      (is (equal '("tmp") (mapcar (lambda (m) (getf m :name))
                                  (getf (fs:load-settings file) :mappings)))))))

(test init-lisp-defines-lisp-mappings
  (with-temporary-directory (directory)
    (let ((init (path directory "init.lisp")))
      (write-file init "
(define-lisp-mapping \"status\"
  (lisp-directory \"status\"
    (list (lisp-file \"version.txt\" \"1.0\")
          (lisp-file \"now.txt\" (lambda () (princ-to-string (get-universal-time)))))))

(define-lisp-mapping \"drop\"
  (lisp-directory \"drop\" '() :on-upload (lambda (name octets) (list name octets)))
  :description \"(a drop box)\")
")
      (multiple-value-bind (mappings problem) (fs:load-init-file (sb-ext:parse-native-namestring init))
        (is (null problem))
        (is (equal '("status" "drop") (mapcar #'first mappings)))
        (is (equal '("(made by init.lisp)" "(a drop box)") (mapcar #'third mappings)))
        (let ((model (fs:make-model)))
          (fs:model-add-directory model "/tmp")
          (is (equal '("init.lisp mapped status" "init.lisp mapped drop")
                     (fs::model-add-lisp-mappings model mappings)))
          (is (equal '("tmp" "status" "drop") (mapping-names model)))
          (let ((drop (fs:vfs-find (fs:model-vfs model) "drop")))
            (is-false (fs:host-mapping-p drop))
            (is (string= "(a drop box)" (fs::backend-description (fs:mapping-backend drop)))))
          ;; Again, as a second launch would: the names are taken.
          (is (search "already" (first (fs::model-add-lisp-mappings model mappings)))))))))

(test an-error-in-init-lisp-keeps-what-came-before-it
  (with-temporary-directory (directory)
    (let ((init (path directory "init.lisp")))
      (write-file init "
(define-lisp-mapping \"first\" (lisp-directory \"first\" '()))
(error \"something is wrong\")
(define-lisp-mapping \"never\" (lisp-directory \"never\" '()))
")
      (multiple-value-bind (mappings problem) (fs:load-init-file (sb-ext:parse-native-namestring init))
        (is (equal '("first") (mapcar #'first mappings)))
        (is (search "something is wrong" problem))
        (is (search "line 3" problem) "and where")))))

(test without-init-lisp-there-is-nothing-to-do
  (with-temporary-directory (directory)
    (is (equal '(nil nil)
               (multiple-value-list
                (fs:load-init-file (sb-ext:parse-native-namestring
                                    (path directory "init.lisp"))))))))
