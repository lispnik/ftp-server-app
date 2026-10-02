;;;; package.lisp -- the test package, the root suite, and what tests share.

(defpackage #:ftp-server/tests
  (:use #:common-lisp #:fiveam)
  (:local-nicknames (#:fs #:ftp-server))
  (:export #:all-tests #:run-tests))

(in-package #:ftp-server/tests)

(def-suite all-tests :description "Everything.")

(defun run-tests ()
  "Run every test and answer whether they all passed."
  (let ((results (run 'all-tests)))
    (explain! results)
    (results-status results)))

;;; Directories to test against -----------------------------------------------------

(defun call-with-temporary-directory (function)
  ;; Under /tmp on purpose: /tmp is a link to /private/tmp, so every test that
  ;; maps one of these maps a directory reached through a link.
  (let ((directory (sb-posix:mkdtemp "/tmp/ftp-server-test-XXXXXX")))
    (unwind-protect (funcall function directory)
      (uiop:delete-directory-tree
       (sb-ext:parse-native-namestring (concatenate 'string directory "/"))
       :validate (lambda (path)
                   (search "ftp-server-test-" (sb-ext:native-namestring path)))
       :if-does-not-exist :ignore))))

(defmacro with-temporary-directory ((directory) &body body)
  "Run BODY with DIRECTORY bound to the native name of a new, empty directory
that is removed afterwards."
  `(call-with-temporary-directory (lambda (,directory) ,@body)))

(defun path (directory &rest names)
  (format nil "~a~{/~a~}" directory names))

(defun write-file (host-path contents)
  (with-open-file (out (sb-ext:parse-native-namestring host-path)
                       :direction :output :if-exists :supersede
                       :external-format :utf-8)
    (write-string contents out))
  host-path)

(defun read-file (host-path)
  (with-open-file (in (sb-ext:parse-native-namestring host-path)
                      :external-format :utf-8)
    (let ((string (make-string (file-length in))))
      (subseq string 0 (read-sequence string in)))))
