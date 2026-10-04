# FTP Server

A macOS application, written in Common Lisp, that serves folders you choose
over FTP. The window lists *mappings*: each one is a folder on this Mac and the
name an FTP client sees it under.

Map `/tmp` as `tempdir`, press Start, and a client that logs in and lists `/`
sees `tempdir`; inside it is whatever `/tmp` holds.

Built with SBCL, [objc](https://github.com/lispnik/objc) for AppKit,
[asdf-macos-app](https://github.com/lispnik/asdf-macos-app) for the bundle,
FiveAM for the tests and ocicl for the dependencies.

## Building

```sh
make deps    # restore the dependencies ocicl.csv pins, into ./ocicl/
make test    # the FiveAM suite
make app     # build/FTP Server.app
make run     # run from source, unbundled
make icon    # draw res/icon.png again
```

Use an SBCL built `--with-sb-safepoint`: the bundle ships the runtime of
whichever SBCL built it, and the server's threads share the image with AppKit.

The Makefile runs SBCL without your init file and with a source registry of
this tree alone, so a dependency missing from `ocicl.csv` fails the build
instead of resolving from somewhere else on your machine. To build against
checkouts of `objc` or `asdf-macos-app` instead of the pinned ones:

```sh
make app OBJC_DIR=~/Projects/common-lisp/objc MACOS_APP_DIR=~/Projects/common-lisp/asdf-macos-app
```

The bundle is signed ad hoc unless `MACOS_SIGNING_IDENTITY` names a
certificate. Put that in `local.mk`, which is not committed.

## Using it

- **Users…** opens the Users window: the people who may log in, each with a
  password and, for every shared folder, No Access, Read, or Read & Write.
  A user sees only the folders they have some access to: to them the others
  are not there, rather than forbidden. Read lets them list and download;
  Read & Write adds uploading, deleting, renaming and making directories.
  Changes apply to a running server, from each client's next command; a user
  who is removed keeps their connection and loses everything they could see.
  There is no anonymous login, and the server will not start until some user
  has a password. Names are unique whatever their case, and are typed exactly
  when logging in.
- **Port.** 2121 by default. Ports below 1024 need root.
- **Allow connections from other computers.** Off, the server listens on
  127.0.0.1 only. On, it listens on every interface and is announced with
  Bonjour as `_ftp._tcp`, for FTP clients that browse for one. Finder does
  not: its Network list shows file servers and screens to share, not FTP
  servers. In Finder, use Go → Connect to Server with `ftp://` and the
  address; Finder's FTP is read-only and cannot use TLS.
- **Bonjour name.** The name the server is announced under; empty means this
  computer's name.
- **Require TLS.** Refuses any client that does not encrypt, before it has
  sent a name or a password. Without it TLS is offered and plain FTP is still
  accepted.
- **Start serving when FTP Server opens.** Starts the server as soon as the
  window is up.
- **Shared folders.** Add… chooses folders; double-click a name to change it.
  Who may use each one is set in the Users window. A folder that is renamed
  keeps its users; one that is removed takes their access with it. Folders can
  be added, removed and changed while the server runs, and clients see the
  change on their next command.

- **Activity.** A table of what each connected client is doing, with a row for
  each thing: the time, the user (`anon` until someone has logged in), the IP
  address, and a message saying what they opened, listed, downloaded or
  uploaded and how large it was, what they deleted or renamed, and what they
  were refused. Click a column's header to sort by it, and again to reverse;
  addresses sort by number. Rest the pointer on a message that has been cut
  short to see all of it. Copy puts the selected rows on the clipboard, or all
  of them if none is selected, with tabs between the columns; Command-C does
  the same. Clear empties the table. Passwords are never shown. The same lines
  go to the log.
- **TLS certificate.** The SHA-256 fingerprint of the certificate the server
  presents, which is what a client shows when it asks whether to trust the
  server; it can be selected and copied. New Certificate… replaces the
  certificate, after asking, while the server is stopped.

The other settings are fixed while the server runs: stop it to change them.

The server listens on IPv4 and IPv6. Over IPv6 a client has to use `EPSV`,
which every current client does; `PASV` has no room for the address.

```sh
curl --user name:password ftp://127.0.0.1:2121/
curl --user name:password ftp://127.0.0.1:2121/tempdir/
curl --user name:password -T file.txt ftp://127.0.0.1:2121/tempdir/   # needs Read & Write
curl --ssl-reqd -k --user name:password ftp://127.0.0.1:2121/         # over TLS; -k trusts the certificate
dns-sd -B _ftp._tcp                                                   # the Bonjour announcement
```

Settings are kept in `~/Library/Application Support/FTP Server/settings.lisp`
and the log in `~/Library/Logs/FTP Server.log`.

## Files and directories made by Lisp

A mapping need not be a folder. `init.lisp`, beside the settings in
`~/Library/Application Support/FTP Server/`, is loaded each time the
application starts, and can map names to files and directories that Lisp
makes as they are asked for:

```lisp
(define-lisp-mapping "status"
  (lisp-directory "status"
    (list
     ;; Made again each time it is listed or read.
     (lisp-file "uptime.txt"
                (lambda () (format nil "up ~d seconds~%"
                                   (floor (get-internal-real-time)
                                          internal-time-units-per-second))))
     ;; A string is sent as UTF-8; a vector of octets as it is.
     (lisp-file "motd.txt" (format nil "Welcome.~%"))
     ;; A directory whose children are made each time it is looked into.
     (lisp-directory "squares"
                     (lambda ()
                       (loop for n from 1 to 5
                             collect (lisp-file (format nil "~d.txt" n)
                                                (format nil "~d~%" (* n n)))))))))

(define-lisp-mapping "dropbox"
  ;; Uploads here are handed to the function, whole, once they have arrived.
  (lisp-directory "dropbox" '()
                  :on-upload (lambda (name octets)
                               (format t "~a: ~d octets~%" name (length octets)))))
```

- `(lisp-file name content &key mtime)` — `content` is a string, a vector of
  octets, or a function of no arguments returning one, called each time the
  file is listed or read. A listing shows a file's size, so its function runs
  for a listing too.
- `(lisp-directory name children &key mtime on-upload)` — `children` is a list
  of nodes, or a function returning one, called each time the directory is
  looked into. With `on-upload`, a function of the file's name and its octets,
  the directory takes uploads from users with Read & Write on its mapping.
  Uploads are whole:
  `APPE` and resuming are refused.
- `(define-lisp-mapping name root &key description)` — `description` is what
  the window shows in place of a folder. Who may use the mapping is set in the
  Users window, as for any other, and kept by its name between launches.

Nothing in a Lisp mapping can be deleted, made or renamed. Downloads, `REST`,
ASCII mode, TLS and the activity log work as they do for folders. What a
function signals is the client's 550, with the error as its reason, and the
server goes on; in a listing, a file that cannot be made is left out.
Functions run on the server's threads, one per client, so anything they share
they must lock.

Lisp mappings appear in the window's table and are not saved with the folders:
`init.lisp` makes them again at the next launch. What it did, and any error in
it, is in the activity pane. `init.lisp` is code and runs as you, as a shell's
startup file does.

## What to know before using it

- **Plain FTP is not encrypted**, and unless "Require TLS" is ticked the server
  accepts it: the password and every file then cross the network in the clear.
  Tick it before allowing connections from other computers on a network you do
  not trust.
- **A data connection can resume the control connection's TLS session**, which
  is how a client knows the data connection is its own; FileZilla warns of a
  server that will not allow it. It is allowed, not demanded: a client that
  does not resume is still served.
- **TLS is explicit FTP over TLS** (`AUTH TLS`, then `PROT P` for the data),
  TLS 1.2 and later, which is what clients call "FTPS (explicit)" or "FTP with
  TLS/SSL". The certificate is one the server makes for itself the first time,
  kept beside the settings as `certificate.pem` and `private-key.pem`. Nobody
  has vouched for it, so a client will ask you to trust it; its SHA-256
  fingerprint is in the window, and in the activity pane each time the server
  starts, to compare against. New Certificate… in the window makes another.
- **TLS uses OpenSSL**, through cl+ssl. The build needs Homebrew's
  (`brew install openssl`); the bundle carries its own copy and uses that one,
  which asdf-macos-app arranges as it saves the image.
- **Passwords are kept in your login keychain**, one item for each user, under
  the service `org.lispnik.ftp-server` and the user's name, and not in the
  settings file. A keychain item belongs to the application that made it, and
  an ad hoc signature is different on each build, so after rebuilding macOS
  asks once for each user whether the new build may read it; a Developer ID
  signature avoids that. When `FTP_SERVER_SETTINGS` names another settings file
  the passwords stay in that file instead, unless `FTP_SERVER_KEYCHAIN` names a
  keychain service to use.
- **Settings from the version with one login are converted** the first time
  they are read: that login becomes a user with Read & Write on the folders
  that were Writable and Read on the rest, and its password moves to that
  user's keychain item.
- **Passive mode only** (`PASV` and `EPSV`); `PORT` is refused.
- **Transfers are binary unless the client asks for ASCII.** After `TYPE A`,
  an upload's CR LF line endings are stored as LF and a download's LF goes out
  as CR LF. A binary file sent in ASCII mode is damaged by that, as it is on
  any FTP server. A client that sends no `TYPE` gets binary, although the
  standard's default is ASCII, and `SIZE` is refused in ASCII mode.
- **A client cannot leave a mapped folder.** `..` stops at the root, and a
  symbolic link that leads outside the folder is not listed and cannot be
  opened, written through or entered. A link that stays inside is followed.
- **Rebuilding may bring back the macOS prompts** for the firewall and for
  folder access: an ad hoc signature is different on each build.

## Layout

```
ftp-server.asd       ftp-server/core, ftp-server/tls, ftp-server, ftp-server/tests
ftp-server-app.asd   the bundle
src/                 the server; no Objective-C, depends only on SBCL's contribs
  vfs.lisp           mappings, virtual paths, staying inside a mapping
  backend.lisp       what a mapping holds, as generic functions; folders on this host
  lisp-backend.lisp  files and directories made by Lisp
  listing.lisp       LIST and MLSD lines
  net.lisp           sockets and CRLF lines
  server.lisp        the listener and its threads
  protocol.lisp      commands that move no data
  session.lisp       one client; PASV, listings and transfers
  accounts.lisp      users, their passwords, and what each may do with each mapping
  settings.lisp      the settings file
  tls.lisp           the certificate and the handshake; the only file that knows OpenSSL
  model.lisp         what the window does, with no window in it
src/macos/           the windows, the keychain, the Bonjour announcement, the application
tests/               FiveAM; ui-tests drive the real window without showing it
```

## The icon

`res/icon.png` is drawn by `tools/icon.lisp`, with the same Objective-C
bindings the application uses; `make icon` draws it again after a change.

## Checking the bundle from a script

With `FTP_SERVER_SELFTEST=N` the application starts the server as soon as its
window is up and quits by itself N seconds later. `FTP_SERVER_SETTINGS` names
a settings file to use instead of the real one, and `MACOS_APP_LOG` a log file.

```sh
env -u SBCL_HOME FTP_SERVER_SETTINGS=/path/to/settings.lisp FTP_SERVER_SELFTEST=15 \
    "build/FTP Server.app/Contents/MacOS/ftp-server" &
curl --retry 10 --retry-connrefused --user u:p ftp://127.0.0.1:2121/
```
