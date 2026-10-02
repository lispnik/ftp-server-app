# FTP Server.
#
#   make deps    restore the dependencies ocicl.csv pins, into ./ocicl/
#   make test    run the FiveAM suite
#   make run     run the application from source, unbundled
#   make app     build "build/FTP Server.app"
#   make clean   remove the bundle and the fasls
#
# Build with a safepoint SBCL: asdf-macos-app ships the runtime of whichever
# SBCL performs the build, and the server's threads share the image with AppKit.

SBCL ?= sbcl

# Signing, which is personal: local.mk, never committed.
-include local.mk

# Optional sibling checkouts that shadow the pinned objc and asdf-macos-app.
OBJC_DIR ?=
MACOS_APP_DIR ?=

# :IGNORE-INHERITED-CONFIGURATION so that a dependency missing from ocicl.csv
# fails here, rather than resolving to whatever the developer's own source
# registry happens to reach.
REGISTRY = (asdf:initialize-source-registry \
              (list :source-registry \
                    $(if $(OBJC_DIR),(list :tree (truename "$(OBJC_DIR)/"))) \
                    $(if $(MACOS_APP_DIR),(list :tree (truename "$(MACOS_APP_DIR)/"))) \
                    (list :tree (truename "./")) \
                    :ignore-inherited-configuration))

LISP = $(SBCL) --non-interactive --no-userinit --no-sysinit \
         --eval '(require :asdf)' --eval '$(REGISTRY)'

.PHONY: deps test run app clean

deps:
	ocicl install

test:
	$(LISP) --eval '(asdf:load-system "ftp-server/tests")' \
	        --eval '(uiop:quit (if (ftp-server/tests:run-tests) 0 1))'

run:
	$(LISP) --eval '(asdf:load-system "ftp-server")' \
	        --eval '(ftp-server:main)'

app:
	$(LISP) --eval '(asdf:make "ftp-server-app")'

clean:
	rm -rf build
	find . -name '*.fasl' -not -path './ocicl/*' -delete
