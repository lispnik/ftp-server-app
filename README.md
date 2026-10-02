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

- **User name and password.** One account; there is no anonymous login. The
  server will not start without both.
- **Port.** 2121 by default. Ports below 1024 need root.
- **Allow connections from other computers.** Off, the server listens on
  127.0.0.1 only. On, it listens on every interface and is announced with
  Bonjour.
- **Bonjour name.** The name the server is announced under; empty means this
  computer's name.
- **Shared folders.** Add… chooses folders; double-click a name to change it;
  tick Writable to allow uploads, deletes, renames and new directories in that
  folder. Folders can be added, removed and changed while the server runs, and
  clients see the change on their next command.

The other settings are fixed while the server runs: stop it to change them.

```sh
curl --user name:password ftp://127.0.0.1:2121/
curl --user name:password ftp://127.0.0.1:2121/tempdir/
curl --user name:password -T file.txt ftp://127.0.0.1:2121/tempdir/   # needs Writable
dns-sd -B _ftp._tcp                                                   # the Bonjour announcement
```

Settings are kept in `~/Library/Application Support/FTP Server/settings.lisp`
and the log in `~/Library/Logs/FTP Server.log`.

## What to know before using it

- **FTP is not encrypted.** The password and every file cross the network in
  the clear. Leave "Allow connections from other computers" off unless you
  trust the network.
- **The password is stored as typed** in the settings file, which is readable
  only by you (mode 600).
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
ftp-server.asd       ftp-server/core, ftp-server, ftp-server/tests
ftp-server-app.asd   the bundle
src/                 the server; no Objective-C, depends only on SBCL's contribs
  vfs.lisp           mappings, virtual paths, staying inside a mapping
  listing.lisp       LIST and MLSD lines
  net.lisp           sockets and CRLF lines
  server.lisp        the listener and its threads
  protocol.lisp      commands that move no data
  session.lisp       one client; PASV, listings and transfers
  settings.lisp      the settings file
  model.lisp         what the window does, with no window in it
src/macos/           the window, the Bonjour announcement, the application
tests/               FiveAM; ui-tests drive the real window without showing it
```

## Checking the bundle from a script

With `FTP_SERVER_SELFTEST=N` the application starts the server as soon as its
window is up and quits by itself N seconds later. `FTP_SERVER_SETTINGS` names
a settings file to use instead of the real one, and `MACOS_APP_LOG` a log file.

```sh
env -u SBCL_HOME FTP_SERVER_SETTINGS=/path/to/settings.lisp FTP_SERVER_SELFTEST=15 \
    "build/FTP Server.app/Contents/MacOS/ftp-server" &
curl --retry 10 --retry-connrefused --user u:p ftp://127.0.0.1:2121/
```
