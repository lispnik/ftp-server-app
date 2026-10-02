;;;; package.lisp -- one package for the server and its window.
;;;;
;;;; OBJC is deliberately not used: it exports INVOKE, DESCRIPTION, RELEASE and
;;;; RETAIN, so its symbols are written out in full.

(defpackage #:ftp-server
  (:use #:cl)
  (:export
   ;; The virtual filesystem.
   #:vfs #:make-vfs #:mapping #:mapping-name #:mapping-host-path #:mapping-writable
   #:vfs-mappings #:vfs-add #:vfs-remove #:vfs-rename #:vfs-find
   #:valid-mapping-name-p #:mapping-error
   #:vfs-error #:vfs-not-found #:vfs-denied
   #:parse-virtual-path #:virtual-path-string #:resolve #:real-path
   ;; The server.
   #:server #:make-server #:start-server #:stop-server
   #:server-port #:server-running-p #:server-session-count
   ;; Settings and the model behind the window.
   #:settings-file #:load-settings #:save-settings
   #:model #:make-model #:model-load #:model-save #:model-vfs
   #:password-store #:make-password-store #:*password-store*
   #:model-username #:model-password #:model-port #:model-allow-remote
   #:model-bonjour-name #:model-start-at-launch #:model-require-tls
   #:model-tls-description #:*tls-maker* #:model-running-p #:model-advertise-p
   #:model-add-directory #:model-remove-mapping #:model-rename-mapping
   #:model-set-writable #:model-start #:model-stop #:model-status-text
   #:parse-port
   ;; The application.
   #:main))
