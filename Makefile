# FTP Server.
#
#   make deps    restore the dependencies ocicl.csv pins, into ./ocicl/
#   make test    run the FiveAM suite
#   make run     run the application from source, unbundled
#   make app     build "build/FTP Server.app"
#   make icon    draw res/icon.png again, from tools/icon.lisp
#   make dmg     the bundle in a disk image, dist/FTP-Server-<version>-<arch>.dmg
#   make notarize-dmg   submit that disk image to Apple, and staple the ticket
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

# What the disk image is called: the version the bundle says, and the machine
# it was built on, which is the machine it runs on.
VERSION := $(shell sed -n 's/.*:version "\(.*\)".*/\1/p' ftp-server-app.asd | head -1)
ARCH := $(shell uname -m)
DIST = dist
DMG = $(DIST)/FTP-Server-$(VERSION)-$(ARCH).dmg
APP = build/FTP Server.app

.PHONY: deps test run app icon dmg notarize-dmg clean

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

# The icon is drawn by a program, with the same bindings the application uses.
# Its PNG is checked in, so this is only for when tools/icon.lisp changes.
icon:
	$(LISP) --eval '(asdf:load-system "objc")' \
	        --load tools/icon.lisp \
	        --eval '(ftp-server-icon:main)'

# A disk image, which is what people expect to download: the application and
# a link to /Applications beside it, to drag it onto.  Signed when
# MACOS_SIGNING_IDENTITY names a Developer ID, as the application inside is.
dmg: app
	@mkdir -p "$(DIST)"
	rm -rf "$(DIST)/stage" "$(DMG)"
	mkdir -p "$(DIST)/stage"
	ditto "$(APP)" "$(DIST)/stage/FTP Server.app"
	ln -s /Applications "$(DIST)/stage/Applications"
	hdiutil create -volname "FTP Server $(VERSION)" -srcfolder "$(DIST)/stage" \
	  -fs HFS+ -format UDZO -ov "$(DMG)"
	rm -rf "$(DIST)/stage"
	@if [ -n "$(MACOS_SIGNING_IDENTITY)" ] && [ "$(MACOS_SIGNING_IDENTITY)" != "-" ]; then \
	  codesign --force --sign "$(MACOS_SIGNING_IDENTITY)" --timestamp "$(DMG)"; \
	else \
	  echo "note: the disk image is unsigned, and the application in it signed ad hoc"; \
	fi
	@echo "built $(DMG)"

# Notarise the disk image and staple the ticket to it, so that the first thing
# a downloader opens is recognised.  Needs a Developer ID build, and either a
# keychain profile (NOTARY_PROFILE, made once with `xcrun notarytool
# store-credentials') or an App Store Connect API key (NOTARY_KEY,
# NOTARY_KEY_ID, NOTARY_ISSUER), which is what CI has.
notarize-dmg:
	@test -f "$(DMG)" || { echo "error: no $(DMG); make dmg first" >&2; exit 1; }
	@codesign -dvv "$(APP)" 2>&1 | grep -q 'flags=.*adhoc' && { \
	  echo "error: $(APP) is signed ad hoc, and Apple will refuse it." >&2; exit 1; } || true
	@if [ -n "$(NOTARY_KEY)" ]; then \
	  xcrun notarytool submit "$(DMG)" --key "$(NOTARY_KEY)" --key-id "$(NOTARY_KEY_ID)" \
	    --issuer "$(NOTARY_ISSUER)" --wait; \
	else \
	  xcrun notarytool submit "$(DMG)" --keychain-profile "$(NOTARY_PROFILE)" --wait; \
	fi
	xcrun stapler staple "$(DMG)"
	spctl -a -vvv -t open --context context:primary-signature "$(DMG)"

clean:
	rm -rf build dist
	find . -name '*.fasl' -not -path './ocicl/*' -delete
