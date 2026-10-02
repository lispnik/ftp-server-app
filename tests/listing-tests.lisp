;;;; listing-tests.lisp -- what a directory holds and how it is written down.

(in-package #:ftp-server/tests)

(def-suite listing :in all-tests :description "Directory listings.")
(in-suite listing)

(defparameter *then* (encode-universal-time 5 4 3 2 1 2026 0)
  "03:04:05 UTC on the second of January 2026.")

(test a-mode-is-written-as-ls-writes-it
  (is (string= "-rw-r--r--" (fs::mode-string #o644 :file)))
  (is (string= "drwxr-xr-x" (fs::mode-string #o755 :directory)))
  (is (string= "----------" (fs::mode-string 0 :file))))

(test a-list-line-has-the-hour-for-a-recent-file
  (let ((entry (fs::make-entry :name "a.txt" :type :file :size 1234 :mode #o644
                               :links 1 :mtime *then*)))
    (is (string= "-rw-r--r--   1 ftp      ftp              1234 Jan  2 03:04 a.txt"
                 (fs::format-list-line entry :now (+ *then* 60) :time-zone 0)))))

(test a-list-line-has-the-year-for-an-old-file
  (let ((entry (fs::make-entry :name "old" :type :directory :size 64 :mode #o755
                               :links 2 :mtime *then*)))
    (is (string= "drwxr-xr-x   2 ftp      ftp                64 Jan  2  2026 old"
                 (fs::format-list-line entry
                                       :now (+ *then* (* 400 24 60 60))
                                       :time-zone 0)))))

(test timestamps-are-utc
  (is (string= "20260102030405" (fs::format-timestamp *then*))))

(test facts-say-what-may-be-done
  (flet ((facts (type writable)
           (fs::format-mlsx-facts
            (fs::make-entry :name "x" :type type :size 7 :mtime *then*
                            :writable writable))))
    (is (string= "type=file;size=7;modify=20260102030405;perm=r; " (facts :file nil)))
    (is (string= "type=file;size=7;modify=20260102030405;perm=radfw; " (facts :file t)))
    (is (string= "type=dir;modify=20260102030405;perm=el; " (facts :directory nil)))
    (is (string= "type=dir;modify=20260102030405;perm=elcmpdf; " (facts :directory t)))))

(test a-directory-is-listed-in-order-without-dot-entries
  (with-temporary-directory (directory)
    (write-file (path directory "b.txt") "bb")
    (write-file (path directory "a.txt") "a")
    (sb-posix:mkdir (path directory "sub") #o755)
    (let ((entries (fs::list-directory directory)))
      (is (equal '("a.txt" "b.txt" "sub") (mapcar #'fs::entry-name entries)))
      (is (equal '(:file :file :directory) (mapcar #'fs::entry-type entries)))
      (is (equal '(1 2) (mapcar #'fs::entry-size (subseq entries 0 2)))))))

(test links-are-listed-as-what-they-reach-or-not-at-all
  (with-temporary-directory (directory)
    (write-file (path directory "real.txt") "four")
    (sb-posix:symlink (path directory "real.txt") (path directory "inside"))
    (sb-posix:symlink "/etc/hosts" (path directory "outside"))
    (sb-posix:symlink (path directory "nothing") (path directory "dangling"))
    (let ((entries (fs::list-directory directory
                                       :root-real (fs:real-path directory))))
      (is (equal '("inside" "real.txt") (mapcar #'fs::entry-name entries)))
      (is (eq :file (fs::entry-type (first entries))))
      (is (= 4 (fs::entry-size (first entries)))))))

(test the-root-lists-a-directory-for-each-mapping
  (let ((vfs (fs:make-vfs)))
    (fs:vfs-add vfs "tempdir" "/tmp")
    (fs:vfs-add vfs "gone" "/nonexistent/ftp-server-test" :writable t)
    (let ((entries (fs::root-entries vfs)))
      (is (equal '("tempdir" "gone") (mapcar #'fs::entry-name entries)))
      (is (every (lambda (entry) (eq :directory (fs::entry-type entry))) entries))
      (is (equal '(nil t) (mapcar #'fs::entry-writable entries))))))

(test what-cannot-be-written-shows-no-write-bits
  (with-temporary-directory (directory)
    (write-file (path directory "a.txt") "a")
    (sb-posix:chmod (path directory "a.txt") #o666)
    (is (= #o444 (fs::entry-mode (first (fs::list-directory directory)))))
    (is (= #o666 (fs::entry-mode (first (fs::list-directory directory :writable t)))))))
