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
   #:host-mapping-p #:mapping-backend #:vfs-add-backend
   ;; Files and directories made by Lisp, for init.lisp.
   #:lisp-file #:lisp-directory #:vfs-add-lisp #:define-lisp-mapping
   #:init-file #:load-init-file
   ;; The server.
   #:server #:make-server #:start-server #:stop-server
   #:server-port #:server-running-p #:server-session-count
   ;; Settings and the model behind the window.
   #:settings-file #:load-settings #:save-settings
   #:model #:make-model #:model-load #:model-save #:model-vfs
   #:password-store #:make-password-store #:*password-store*
   #:model-accounts #:model-users #:model-user #:model-add-user #:model-remove-user
   #:model-rename-user #:model-set-password #:model-access #:model-set-access
   #:accounts #:make-accounts #:accounts-add #:accounts-find #:accounts-users
   #:user #:user-name #:user-password #:user-access #:account-error
   #:accounts-authenticate #:accounts-access
   #:model-port #:model-allow-remote
   #:model-bonjour-name #:model-start-at-launch #:model-require-tls
   #:model-tls-description #:*tls-maker* #:model-running-p #:model-advertise-p
   #:model-add-directory #:model-remove-mapping #:model-rename-mapping
   #:model-start #:model-stop #:model-status-text
   #:parse-port
   ;; The application.
   #:main))
