;;;; listing.lisp -- what a directory holds, and how FTP writes it down.

(in-package #:ftp-server)

(defconstant +unix-to-universal+ (encode-universal-time 0 0 0 1 1 1970 0)
  "Seconds between the Lisp epoch and the Unix one.")

(defstruct entry
  (name "" :type string)
  (type :file)                          ; :file, :directory or :other
  (size 0)
  (mode 0)
  (links 1)
  (mtime 0)                             ; a universal time
  (writable nil))

;;; Reading the host --------------------------------------------------------------

(defun stat-type (stat)
  (let ((format (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt)))
    (cond ((= format sb-posix:s-ifdir) :directory)
          ((= format sb-posix:s-ifreg) :file)
          ((= format sb-posix:s-iflnk) :symlink)
          (t :other))))

(defun stat-or-nil (function host-path)
  (handler-case (funcall function host-path)
    (sb-posix:syscall-error () nil)))

(defun host-file-type (host-path)
  "What HOST-PATH is, without following a link: :FILE, :DIRECTORY, :SYMLINK,
:OTHER, or NIL if nothing is there."
  (let ((stat (stat-or-nil #'sb-posix:lstat host-path)))
    (and stat (stat-type stat))))

(defun host-entry (name host-path &key root-real writable)
  "The entry for HOST-PATH under NAME, or NIL if it should not be shown.

A symbolic link is shown as what it leads to, and not at all if that is
outside ROOT-REAL or is nothing: a client could not open it anyway."
  (let ((stat (stat-or-nil #'sb-posix:lstat host-path)))
    (when (and stat (eq :symlink (stat-type stat)))
      (let ((real (real-path host-path)))
        (setf stat (and real
                        (or (null root-real) (path-within-p real root-real))
                        (stat-or-nil #'sb-posix:stat real)))))
    (when stat
      (make-entry :name name
                  :type (let ((type (stat-type stat)))
                          (if (member type '(:file :directory)) type :other))
                  :size (sb-posix:stat-size stat)
                  ;; No write bits on what cannot be written through here,
                  ;; whatever the host allows its own users.
                  :mode (logand (sb-posix:stat-mode stat)
                                (if writable #o777 #o555))
                  :links (sb-posix:stat-nlink stat)
                  :mtime (+ (sb-posix:stat-mtime stat) +unix-to-universal+)
                  :writable writable))))

(defun directory-names (host-path)
  "The names in the directory HOST-PATH, without . and .., sorted."
  (let ((directory (sb-posix:opendir host-path)))
    (unwind-protect
         (sort (loop for dirent = (sb-posix:readdir directory)
                     until (sb-alien:null-alien dirent)
                     for name = (sb-posix:dirent-name dirent)
                     unless (member name '("." "..") :test #'string=)
                       collect name)
               #'string<)
      (sb-posix:closedir directory))))

(defun list-directory (host-path &key root-real writable)
  "The entries of the directory HOST-PATH."
  (loop for name in (directory-names host-path)
        for entry = (host-entry name (join-host-path host-path name)
                                :root-real root-real :writable writable)
        when entry collect entry))

(defun mapping-entry (mapping)
  "MAPPING as an entry of the root: a directory, whether or not the host
directory behind it can be reached just now."
  (let ((entry (host-entry (mapping-name mapping) (mapping-host-path mapping)
                           :writable (mapping-writable mapping))))
    (if (and entry (eq :directory (entry-type entry)))
        entry
        (make-entry :name (mapping-name mapping) :type :directory
                    :mode #o555 :links 2 :mtime (get-universal-time)
                    :writable (mapping-writable mapping)))))

(defun root-entry ()
  "The root itself."
  (make-entry :name "/" :type :directory :mode #o555 :links 2
              :mtime (get-universal-time)))

;;; Writing it down ----------------------------------------------------------------

(defun mode-string (mode type)
  "MODE as ls -l writes it: a type character and three rwx triples."
  (let ((string (make-string 10 :initial-element #\-)))
    (when (eq type :directory)
      (setf (char string 0) #\d))
    (loop for index from 1 to 9
          for bit = (ash 1 (- 9 index))
          when (logtest mode bit)
            do (setf (char string index) (char "rwx" (mod (1- index) 3))))
    string))

(defparameter +months+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun format-list-time (time now time-zone)
  "TIME as ls -l writes it: the hour for anything from the last six months,
the year for anything else."
  (multiple-value-bind (second minute hour day month year)
      (if time-zone (decode-universal-time time time-zone) (decode-universal-time time))
    (declare (ignore second))
    (if (< -1 (- now time) (* 183 24 60 60))
        (format nil "~a ~2d ~2,'0d:~2,'0d" (aref +months+ (1- month)) day hour minute)
        (format nil "~a ~2d  ~4d" (aref +months+ (1- month)) day year))))

(defun format-list-line (entry &key (now (get-universal-time)) time-zone)
  "ENTRY as a line of LIST.  The owner and group are constant: who owns a file
on this machine is not a client's business.  TIME-ZONE is for tests; without it
the time is local, which is what clients expect of LIST."
  (format nil "~a ~3d ftp      ftp      ~12d ~a ~a"
          (mode-string (entry-mode entry) (entry-type entry))
          (entry-links entry)
          (entry-size entry)
          (format-list-time (entry-mtime entry) now time-zone)
          (entry-name entry)))

(defun format-timestamp (time)
  "TIME as MDTM and MLSD write it: YYYYMMDDHHMMSS, in UTC."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time time 0)
    (format nil "~4,'0d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d"
            year month day hour minute second)))

(defun format-mlsx-facts (entry)
  "ENTRY's facts as MLSD and MLST write them, up to and including the space
that comes before the name."
  (let ((directory (eq :directory (entry-type entry)))
        (writable (entry-writable entry)))
    (format nil "type=~a;~@[size=~d;~]modify=~a;perm=~a; "
            (if directory "dir" "file")
            (and (not directory) (entry-size entry))
            (format-timestamp (entry-mtime entry))
            (cond ((and directory writable) "elcmpdf")
                  (directory "el")
                  (writable "radfw")
                  (t "r")))))
