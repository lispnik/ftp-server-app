;;;; ftp-server.asd -- the FTP server, its window, and their tests.
;;;;
;;;; The bundle is ftp-server-app.asd.  It is a file of its own because
;;;; :DEFSYSTEM-DEPENDS-ON is resolved when a .asd is read, and loading the
;;;; server should not need asdf-macos-app.

(defsystem "ftp-server/core"
  :description "An FTP server over a virtual filesystem of mapped directories."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ((:require :sb-posix) (:require :sb-bsd-sockets))
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "vfs")
                             (:file "listing")
                             (:file "net")
                             (:file "server")
                             (:file "protocol")
                             (:file "session")
                             (:file "settings")
                             (:file "model"))))
  :in-order-to ((test-op (test-op "ftp-server/tests"))))

(defsystem "ftp-server"
  :description "The FTP server in a Cocoa window."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("ftp-server/core" "objc")
  :components ((:module "src/macos"
                :pathname "src/macos/"
                :serial t
                :components ((:file "frameworks")
                             (:file "main-thread")
                             (:file "bonjour")
                             (:file "window")
                             (:file "app"))))
  :in-order-to ((test-op (test-op "ftp-server/tests"))))

(defsystem "ftp-server/tests"
  :description "The FiveAM suite."
  :depends-on ("ftp-server" "fiveam")
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "vfs-tests")
                             (:file "listing-tests")
                             (:file "settings-tests")
                             (:file "protocol-tests")
                             (:file "server-tests")
                             (:file "model-tests")
                             (:file "ui-tests"))))
  ;; FIVEAM:RUN! prints failures but returns NIL, and ASDF discards what a
  ;; TEST-OP returns, so the failure has to be an error.
  :perform (test-op (o c)
             (unless (uiop:symbol-call :ftp-server/tests :run-tests)
               (error "The test suite failed."))))
