;;;; vfs-tests.lisp -- names, virtual paths, and staying inside a mapping.

(in-package #:ftp-server/tests)

(def-suite vfs :in all-tests :description "The virtual filesystem.")
(in-suite vfs)

(test mapping-names-are-single-path-components
  (is-true (fs:valid-mapping-name-p "tempdir"))
  (is-true (fs:valid-mapping-name-p "My Files"))
  (is-true (fs:valid-mapping-name-p "a[1]*"))
  (is-false (fs:valid-mapping-name-p ""))
  (is-false (fs:valid-mapping-name-p "."))
  (is-false (fs:valid-mapping-name-p ".."))
  (is-false (fs:valid-mapping-name-p "a/b"))
  (is-false (fs:valid-mapping-name-p " lead"))
  (is-false (fs:valid-mapping-name-p "trail "))
  (is-false (fs:valid-mapping-name-p (format nil "new~%line")))
  (is-false (fs:valid-mapping-name-p nil)))

(test mappings-are-added-in-order-and-removed
  (let ((vfs (fs:make-vfs)))
    (fs:vfs-add vfs "one" "/tmp")
    (fs:vfs-add vfs "two" "/var" :writable t)
    (is (equal '("one" "two") (mapcar #'fs:mapping-name (fs:vfs-mappings vfs))))
    (is-false (fs:mapping-writable (fs:vfs-find vfs "one")))
    (is-true (fs:mapping-writable (fs:vfs-find vfs "two")))
    (is-true (fs:vfs-remove vfs "one"))
    (is-false (fs:vfs-remove vfs "one"))
    (is (equal '("two") (mapcar #'fs:mapping-name (fs:vfs-mappings vfs))))))

(test a-name-is-used-once-whatever-its-case
  (let ((vfs (fs:make-vfs)))
    (fs:vfs-add vfs "tempdir" "/tmp")
    (signals fs:mapping-error (fs:vfs-add vfs "tempdir" "/var"))
    (signals fs:mapping-error (fs:vfs-add vfs "TempDir" "/var"))
    (signals fs:mapping-error (fs:vfs-add vfs "a/b" "/var"))
    (is (= 1 (length (fs:vfs-mappings vfs))))))

(test a-mapping-can-be-renamed-but-not-onto-another
  (let* ((vfs (fs:make-vfs))
         (one (fs:vfs-add vfs "one" "/tmp")))
    (fs:vfs-add vfs "two" "/var")
    (fs:vfs-rename vfs one "uno")
    (is (eq one (fs:vfs-find vfs "uno")))
    (is (null (fs:vfs-find vfs "one")))
    ;; To its own name, in another case, is no clash.
    (fs:vfs-rename vfs one "UNO")
    (signals fs:mapping-error (fs:vfs-rename vfs one "two"))
    (signals fs:mapping-error (fs:vfs-rename vfs one ""))
    (is (string= "UNO" (fs:mapping-name one)))))

(test virtual-paths-are-normalised-on-names-alone
  (is (equal '() (fs:parse-virtual-path '() "/")))
  (is (equal '() (fs:parse-virtual-path '("a") "/")))
  (is (equal '("a") (fs:parse-virtual-path '("a") "")))
  (is (equal '("a" "b") (fs:parse-virtual-path '("a") "b")))
  (is (equal '("b") (fs:parse-virtual-path '("a") "/b")))
  (is (equal '("a" "c") (fs:parse-virtual-path '("a") "b/../c")))
  (is (equal '("a" "b") (fs:parse-virtual-path '() "//a/./b//"))))

(test dot-dot-stops-at-the-root
  (is (equal '() (fs:parse-virtual-path '() "..")))
  (is (equal '() (fs:parse-virtual-path '("a") "../../../..")))
  (is (equal '("etc" "passwd") (fs:parse-virtual-path '("a") "../../../etc/passwd")))
  (signals fs:vfs-not-found
    (fs:parse-virtual-path '() (format nil "a~cb" (code-char 0)))))

(test virtual-paths-print-as-a-client-writes-them
  (is (string= "/" (fs:virtual-path-string '())))
  (is (string= "/a/b" (fs:virtual-path-string '("a" "b")))))

(test containment-wants-a-whole-component
  (is-true (fs::path-within-p "/private/tmp" "/private/tmp"))
  (is-true (fs::path-within-p "/private/tmp/a" "/private/tmp"))
  (is-false (fs::path-within-p "/private/tmp2" "/private/tmp"))
  (is-false (fs::path-within-p "/private" "/private/tmp"))
  (is-true (fs::path-within-p "/etc" "/")))

(test the-root-resolves-to-nothing-on-the-host
  (let ((vfs (fs:make-vfs)))
    (is (equal '(:root nil nil) (multiple-value-list (fs:resolve vfs '()))))
    (signals fs:vfs-not-found (fs:resolve vfs '("nothing")))))

(test a-mapping-reached-through-a-link-resolves-to-the-real-directory
  ;; The user's own example: /tmp is a link to /private/tmp.
  (with-temporary-directory (directory)
    (is (string/= directory (fs:real-path directory)))
    (write-file (path directory "a.txt") "a")
    (let* ((vfs (fs:make-vfs))
           (mapping (fs:vfs-add vfs "tempdir" directory)))
      (multiple-value-bind (kind found host) (fs:resolve vfs '("tempdir"))
        (is (eq :mapping-root kind))
        (is (eq mapping found))
        (is (string= (fs:real-path directory) host)))
      (multiple-value-bind (kind found host) (fs:resolve vfs '("tempdir" "a.txt"))
        (is (eq :inside kind))
        (is (eq mapping found))
        (is (string= (path (fs:real-path directory) "a.txt") host)))
      (signals fs:vfs-not-found (fs:resolve vfs '("tempdir" "missing"))))))

(test a-link-that-leaves-the-mapping-is-refused
  (with-temporary-directory (directory)
    (sb-posix:mkdir (path directory "shared") #o755)
    (sb-posix:mkdir (path directory "private") #o755)
    (write-file (path directory "private" "secret.txt") "secret")
    (sb-posix:symlink (path directory "private") (path directory "shared" "out"))
    (sb-posix:symlink "/etc/hosts" (path directory "shared" "hosts"))
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "s" (path directory "shared"))
      (signals fs:vfs-denied (fs:resolve vfs '("s" "out")))
      (signals fs:vfs-denied (fs:resolve vfs '("s" "out" "secret.txt")))
      (signals fs:vfs-denied (fs:resolve vfs '("s" "hosts")))
      ;; Nor may something be created through it.
      (signals fs:vfs-denied (fs:resolve vfs '("s" "out" "new.txt") :intent :leaf)))))

(test a-link-that-stays-inside-the-mapping-is-followed
  (with-temporary-directory (directory)
    (sb-posix:mkdir (path directory "real") #o755)
    (write-file (path directory "real" "a.txt") "a")
    (sb-posix:symlink (path directory "real") (path directory "alias"))
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "m" directory)
      (is (string= (path (fs:real-path directory) "real" "a.txt")
                   (nth-value 2 (fs:resolve vfs '("m" "alias" "a.txt"))))))))

(test a-neighbour-with-the-mapping-as-a-prefix-is-outside
  (with-temporary-directory (directory)
    (sb-posix:mkdir (path directory "data") #o755)
    (sb-posix:mkdir (path directory "data2") #o755)
    (sb-posix:symlink (path directory "data2") (path directory "data" "next"))
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "d" (path directory "data"))
      (signals fs:vfs-denied (fs:resolve vfs '("d" "next"))))))

(test a-leaf-is-named-without-being-followed
  (with-temporary-directory (directory)
    (sb-posix:symlink "/etc/hosts" (path directory "hosts"))
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "m" directory)
      ;; Something that is not there yet, in a directory that is.
      (is (string= (path (fs:real-path directory) "new.txt")
                   (nth-value 2 (fs:resolve vfs '("m" "new.txt") :intent :leaf))))
      ;; The link itself, so that deleting it deletes the link.
      (is (string= (path (fs:real-path directory) "hosts")
                   (nth-value 2 (fs:resolve vfs '("m" "hosts") :intent :leaf))))
      ;; But not in a directory that is not there.
      (signals fs:vfs-not-found
        (fs:resolve vfs '("m" "missing" "new.txt") :intent :leaf)))))

(test a-mapping-whose-directory-has-gone-is-not-found
  (let ((vfs (fs:make-vfs)))
    (fs:vfs-add vfs "gone" "/nonexistent/ftp-server-test")
    (signals fs:vfs-not-found (fs:resolve vfs '("gone")))))

(test names-with-pattern-characters-are-just-names
  (with-temporary-directory (directory)
    (write-file (path directory "a[1]*?.txt") "x")
    (let ((vfs (fs:make-vfs)))
      (fs:vfs-add vfs "m" directory)
      (is (string= (path (fs:real-path directory) "a[1]*?.txt")
                   (nth-value 2 (fs:resolve vfs '("m" "a[1]*?.txt"))))))))
