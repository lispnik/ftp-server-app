;;;; access-tests.lisp -- several users, each seeing and doing only what they may.

(in-package #:ftp-server/tests)

(def-suite access :in all-tests :description "Users and what each may do.")
(in-suite access)

(defun call-with-accounts-server (function)
  "Two folders, shared and private, and a server whose users are ACCOUNTS:
ann may read and write shared and read private; bob may read shared and has
no access to private.  FUNCTION gets the port, the accounts and the directory."
  (with-temporary-directory (directory)
    (sb-posix:mkdir (path directory "shared") #o755)
    (sb-posix:mkdir (path directory "private") #o755)
    (write-file (path directory "shared" "notice.txt") "for everyone")
    (write-file (path directory "private" "secret.txt") "for ann")
    (let* ((vfs (fs:make-vfs))
           (accounts (fs:make-accounts))
           (ann (fs:accounts-add accounts "ann" :password "a"))
           (bob (fs:accounts-add accounts "bob" :password "b")))
      (fs:vfs-add vfs "shared" (path directory "shared"))
      (fs:vfs-add vfs "private" (path directory "private"))
      (setf (fs:user-access accounts ann "shared") :read-write
            (fs:user-access accounts ann "private") :read
            (fs:user-access accounts bob "shared") :read)
      (multiple-value-bind (authenticator access) (fs::accounts-server-functions accounts)
        (let ((server (fs:make-server :vfs vfs :port 0
                                      :authenticator authenticator :access access))
              (delay fs::*login-failure-delay*))
          (setf fs::*login-failure-delay* 0)
          (fs:start-server server)
          (unwind-protect
               (funcall function (fs:server-port server) accounts directory)
            (fs:stop-server server)
            (setf fs::*login-failure-delay* delay)))))))

(defmacro with-accounts-server ((port accounts directory) &body body)
  `(call-with-accounts-server (lambda (,port ,accounts ,directory)
                                (declare (ignorable ,accounts ,directory))
                                ,@body)))

(test each-user-logs-in-with-their-own-password
  (with-accounts-server (port accounts directory)
    (with-client (client port)
      (is (= 530 (login client "ann" "b")))
      (is (= 230 (login client "ann" "a"))))
    (with-client (client port)
      (is (= 230 (login client "bob" "b"))))))

(test the-root-lists-only-what-a-user-may-see
  (with-accounts-server (port accounts directory)
    (with-client (client port)
      (login client "ann" "a")
      (is (equal '("shared" "private") (lines-of (nth-value 1 (fetch client "NLST"))))))
    (with-client (client port)
      (login client "bob" "b")
      (is (equal '("shared") (lines-of (nth-value 1 (fetch client "NLST"))))))))

(test a-mapping-a-user-may-not-see-is-not-there-for-them
  (with-accounts-server (port accounts directory)
    (with-client (client port)
      (login client "bob" "b")
      ;; Not "permission denied": that would say it exists.
      (multiple-value-bind (code lines) (send client "CWD /private")
        (is (= 550 code))
        (is (search "No such" (first lines))))
      (is (= 550 (send client "SIZE /private/secret.txt")))
      (is (= 550 (fetch client "RETR /private/secret.txt")))
      (is (= 550 (fetch client "NLST /private"))))))

(test reading-is-not-writing
  (with-accounts-server (port accounts directory)
    (with-client (client port)
      (login client "bob" "b")
      (is (string= "for everyone" (nth-value 1 (fetch client "RETR /shared/notice.txt"))))
      (is (= 550 (store client "STOR /shared/new.txt" "no")))
      (is (= 550 (send client "DELE /shared/notice.txt")))
      (is (= 550 (send client "MKD /shared/dir")))
      (is (null (fs::host-file-type (path directory "shared" "new.txt")))))
    (with-client (client port)
      (login client "ann" "a")
      (is (= 226 (store client "STOR /shared/new.txt" "yes")))
      (is (string= "yes" (read-file (path directory "shared" "new.txt"))))
      ;; ann may read private, and that is all.
      (is (string= "for ann" (nth-value 1 (fetch client "RETR /private/secret.txt"))))
      (is (= 550 (store client "STOR /private/new.txt" "no"))))))

(test listings-show-what-a-user-may-do
  (with-accounts-server (port accounts directory)
    (with-client (client port)
      (login client "ann" "a")
      (is (search "perm=elcmpdf" (first (lines-of (nth-value 1 (fetch client "MLSD /")))))))
    (with-client (client port)
      (login client "bob" "b")
      (is (search "perm=el;" (first (lines-of (nth-value 1 (fetch client "MLSD /"))))))
      (is (search "perm=r;" (first (lines-of (nth-value 1 (fetch client "MLSD /shared")))))))))

(test a-change-of-access-applies-from-the-next-command
  (with-accounts-server (port accounts directory)
    (with-client (client port)
      (login client "bob" "b")
      (is (= 550 (send client "CWD /private")))
      (setf (fs:user-access accounts (fs:accounts-find accounts "bob") "private") :read)
      (is (= 250 (send client "CWD /private")) "the same session, no new login")
      (is (= 550 (store client "STOR /private/x.txt" "no")))
      (setf (fs:user-access accounts (fs:accounts-find accounts "bob") "private") :read-write)
      (is (= 226 (store client "STOR /private/x.txt" "yes")))
      ;; Taken away again, it goes again.
      (setf (fs:user-access accounts (fs:accounts-find accounts "bob") "private") nil)
      (is (= 550 (fetch client "RETR /private/x.txt"))))))

(test a-removed-user-keeps-their-connection-and-loses-everything-else
  (with-accounts-server (port accounts directory)
    (with-client (client port)
      (login client "bob" "b")
      (is (equal '("shared") (lines-of (nth-value 1 (fetch client "NLST")))))
      (fs::accounts-remove accounts (fs:accounts-find accounts "bob"))
      (is (equal '() (lines-of (nth-value 1 (fetch client "NLST")))))
      (is (= 550 (fetch client "RETR /shared/notice.txt"))))
    (with-client (client port)
      (is (= 530 (login client "bob" "b"))))))

(test a-renamed-mapping-keeps-its-users
  (with-accounts-server (port accounts directory)
    (fs::accounts-rename-mapping accounts "shared" "common")
    (with-client (client port)
      (login client "bob" "b")
      ;; The VFS still calls it shared, so bob sees nothing: the model renames
      ;; both at once, and this is the accounts' half on its own.
      (is (equal '() (lines-of (nth-value 1 (fetch client "NLST"))))))))

(test a-lisp-mapping-is-shared-by-the-same-rules
  (let* ((vfs (fs:make-vfs))
         (accounts (fs:make-accounts))
         (received '())
         (ann (fs:accounts-add accounts "ann" :password "a"))
         (bob (fs:accounts-add accounts "bob" :password "b")))
    (fs:vfs-add-lisp vfs "gen"
                     (fs:lisp-directory "gen"
                                        (list (fs:lisp-file "hello.txt" "hello")
                                              (fs:lisp-directory
                                               "inbox" '()
                                               :on-upload (lambda (name octets)
                                                            (declare (ignore octets))
                                                            (push name received))))))
    (setf (fs:user-access accounts ann "gen") :read-write
          (fs:user-access accounts bob "gen") :read)
    (multiple-value-bind (authenticator access) (fs::accounts-server-functions accounts)
      (let ((server (fs:make-server :vfs vfs :port 0
                                    :authenticator authenticator :access access)))
        (fs:start-server server)
        (unwind-protect
             (progn
               (with-client (client (fs:server-port server))
                 (login client "bob" "b")
                 (is (string= "hello" (nth-value 1 (fetch client "RETR /gen/hello.txt"))))
                 (is (= 550 (store client "STOR /gen/inbox/b.txt" "no"))))
               (with-client (client (fs:server-port server))
                 (login client "ann" "a")
                 (is (= 226 (store client "STOR /gen/inbox/a.txt" "yes")))))
          (fs:stop-server server))))
    (is (equal '("a.txt") received))))
