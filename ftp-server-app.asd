;;;; ftp-server-app.asd -- "FTP Server.app", built with asdf-macos-app.
;;;;
;;;;   make app        (or: sbcl --eval '(asdf:make "ftp-server-app")' --quit)

(defsystem "ftp-server-app"
  :defsystem-depends-on ("asdf-macos-app")
  :class :macos-app-system
  :build-operation "macos-app-op"
  :entry-point "ftp-server:main"
  :description "An FTP server in a Cocoa window, as an application."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("ftp-server")
  :bundle-identifier "org.lispnik.ftp-server"
  :bundle-name "FTP Server"
  :bundle-executable "ftp-server"
  ;; Drawn by tools/icon.lisp; asdf-macos-app makes the .icns from the PNG.
  :bundle-icon "res/icon.png"
  :bundle-principal-class "NSApplication"
  :bundle-category "public.app-category.utilities"
  :bundle-copyright "MIT"
  :bundle-log t
  ;; The server reads mapped folders on a relaunch with no open panel in
  ;; between, and advertises itself on the local network, so macOS wants a
  ;; reason to show for each.
  :bundle-info-plist
  (("NSBonjourServices" . (:array "_ftp._tcp"))
   ("NSLocalNetworkUsageDescription"
    . "FTP Server advertises itself with Bonjour and accepts connections from the local network.")
   ("NSDocumentsFolderUsageDescription"
    . "FTP Server serves the folders you map, which may be in Documents.")
   ("NSDesktopFolderUsageDescription"
    . "FTP Server serves the folders you map, which may be on the Desktop.")
   ("NSDownloadsFolderUsageDescription"
    . "FTP Server serves the folders you map, which may be in Downloads.")
   ("NSRemovableVolumesUsageDescription"
    . "FTP Server serves the folders you map, which may be on a removable volume."))
  ;; Merged against this file rather than the current directory, which is what
  ;; a bare "build/" would resolve against.
  :bundle-output-directory
  #.(merge-pathnames "build/"
                     (uiop:pathname-directory-pathname
                      (or *load-truename* *default-pathname-defaults*)))
  ;; Ad hoc unless told otherwise; an empty variable counts as unset.
  :code-signing-identity #.(let ((identity (uiop:getenv "MACOS_SIGNING_IDENTITY")))
                             (if (and identity (plusp (length identity)))
                                 identity
                                 "-")))
