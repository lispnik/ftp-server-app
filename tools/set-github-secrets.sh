#!/bin/bash
# tools/set-github-secrets.sh -- give CI what it needs to sign and notarise.
#
# Sets the six repository secrets .github/workflows/macos.yml reads.  With them,
# releases are signed with your Developer ID and their disk images notarised by
# Apple; without them, builds are signed ad hoc.
#
# Fill in the placeholders below, then:
#
#   tools/set-github-secrets.sh --dry-run   # check everything, set nothing
#   tools/set-github-secrets.sh             # set the secrets
#
# Nothing below is itself a secret -- two file paths and three identifiers --
# so the filled-in script is safe to keep.  The .p12's password is asked for
# when the script runs (or read from P12_PASSWORD), and the two files are read
# and encoded then, and sent to GitHub without being written anywhere.
#
# WHERE THESE COME FROM
#
#   The certificate: Keychain Access, My Certificates, your "Developer ID
#   Application" certificate with its private key beneath it -- select both,
#   File > Export Items..., as a .p12 with a password.  No such certificate?
#   developer.apple.com > Certificates, Identifiers & Profiles, +, "Developer ID
#   Application" (an Account Holder has to make it).
#
#   The identity: exactly as `security find-identity -v -p codesigning` prints
#   it, e.g. "Developer ID Application: Jane Doe (ABCDE12345)".
#
#   The notary key: App Store Connect > Users and Access > Integrations > App
#   Store Connect API > Team Keys, +, access "Developer".  Download the .p8 --
#   Apple lets you do that once -- and note its Key ID and the Issuer ID shown
#   above the list.

set -euo pipefail

# ---- placeholders -------------------------------------------------------------

CERTIFICATE_P12="${CERTIFICATE_P12:-/path/to/DeveloperID.p12}"
SIGNING_IDENTITY="${SIGNING_IDENTITY:-Developer ID Application: Your Name (TEAMID)}"
NOTARY_KEY_P8="${NOTARY_KEY_P8:-/path/to/AuthKey_KEYID.p8}"
NOTARY_KEY_ID="${NOTARY_KEY_ID:-KEYID}"
NOTARY_ISSUER="${NOTARY_ISSUER:-00000000-0000-0000-0000-000000000000}"

# The repository, as owner/name.  By default, the one this checkout pushes to.
REPOSITORY="${REPOSITORY:-}"

# ---- nothing to change below ------------------------------------------------------

dry_run=""
case "${1:-}" in
  --dry-run|-n) dry_run=1 ;;
  "") ;;
  *) echo "usage: $0 [--dry-run]" >&2; exit 2 ;;
esac

problems=0
problem() { echo "error: $*" >&2; problems=$((problems + 1)); }

# Placeholders left as they were.
[[ "$CERTIFICATE_P12" == /path/to/* ]] && problem "CERTIFICATE_P12 is still the placeholder"
[[ "$SIGNING_IDENTITY" == *"Your Name (TEAMID)"* ]] && problem "SIGNING_IDENTITY is still the placeholder"
[[ "$NOTARY_KEY_P8" == /path/to/* ]] && problem "NOTARY_KEY_P8 is still the placeholder"
[[ "$NOTARY_KEY_ID" == KEYID ]] && problem "NOTARY_KEY_ID is still the placeholder"
[[ "$NOTARY_ISSUER" == 00000000-0000-0000-0000-000000000000 ]] && problem "NOTARY_ISSUER is still the placeholder"

# What they say has to be there, and of the right shape.
[[ -f "$CERTIFICATE_P12" ]] || problem "no certificate at $CERTIFICATE_P12"
[[ -f "$NOTARY_KEY_P8" ]] || problem "no notary key at $NOTARY_KEY_P8"
[[ "$SIGNING_IDENTITY" =~ ^Developer\ ID\ Application:\ .+\ \([A-Z0-9]{10}\)$ ]] \
  || problem "SIGNING_IDENTITY should look like \"Developer ID Application: Name (ABCDE12345)\""
[[ "$NOTARY_KEY_ID" =~ ^[A-Z0-9]{10}$ ]] || problem "NOTARY_KEY_ID should be ten letters and digits"
[[ "$NOTARY_ISSUER" =~ ^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$ ]] \
  || problem "NOTARY_ISSUER should be a UUID, in lower case"
if [[ -f "$NOTARY_KEY_P8" ]] && ! grep -q "BEGIN PRIVATE KEY" "$NOTARY_KEY_P8"; then
  problem "$NOTARY_KEY_P8 does not look like a .p8 key"
fi

if (( problems > 0 )); then
  echo "$problems thing(s) to put right; nothing was set." >&2
  exit 1
fi

# Logged in first: the lookup below fails without it, and less helpfully.
gh auth status > /dev/null 2>&1 || { echo "error: gh is not logged in; run gh auth login" >&2; exit 1; }
if [[ -z "$REPOSITORY" ]]; then
  REPOSITORY="$(gh repo view --json nameWithOwner -q .nameWithOwner)"
fi

# The .p12's password: asked for, not kept.
if [[ -z "${P12_PASSWORD:-}" ]]; then
  read -r -s -p "Password for $(basename "$CERTIFICATE_P12"): " P12_PASSWORD
  echo
fi
export P12_PASSWORD

# That the password opens the .p12, and that the certificate in it is the
# identity named above -- or CI would fail at the first codesign, an hour in.
# macOS's own openssl, which reads the older encryption Keychain Access uses.
#
# The name is printed a field to a line and in UTF-8, so that it can be taken
# whole: on one line, a comma in it ("Acme, Inc.") looks like the end of it,
# and a letter outside ASCII ("Müller") comes out escaped.
subject="$(/usr/bin/openssl pkcs12 -in "$CERTIFICATE_P12" -nokeys -passin env:P12_PASSWORD 2>/dev/null \
           | /usr/bin/openssl x509 -noout -subject -nameopt utf8,sep_multiline 2>/dev/null)" \
  || { echo "error: could not open $CERTIFICATE_P12 with that password" >&2; exit 1; }
[[ -n "$subject" ]] \
  || { echo "error: could not open $CERTIFICATE_P12 with that password" >&2; exit 1; }
common_name="$(sed -n 's/^ *CN=//p' <<< "$subject" | head -1)"
if [[ "$common_name" != "$SIGNING_IDENTITY" ]]; then
  echo "error: the certificate in $CERTIFICATE_P12 is \"$common_name\"," >&2
  echo "       but SIGNING_IDENTITY is \"$SIGNING_IDENTITY\"" >&2
  exit 1
fi
if ! /usr/bin/openssl pkcs12 -in "$CERTIFICATE_P12" -nocerts -nodes -passin env:P12_PASSWORD 2>/dev/null \
     | grep -q "PRIVATE KEY"; then
  echo "error: $CERTIFICATE_P12 has the certificate but not its private key;" >&2
  echo "       export both together from Keychain Access" >&2
  exit 1
fi
echo "ok: $CERTIFICATE_P12 opens, and holds \"$common_name\" with its key"

# name, then value: sent on standard input, so that it is never in an argument
# list for anything else on this machine to read.
set_secret() {
  local name="$1" value="$2"
  if [[ -n "$dry_run" ]]; then
    printf '  would set %-28s (%d characters)\n' "$name" "${#value}"
  else
    printf '%s' "$value" | gh secret set "$name" --repo "$REPOSITORY"
  fi
}

[[ -n "$dry_run" ]] && echo "dry run: checking only; nothing is sent to $REPOSITORY" \
                    || echo "setting the secrets of $REPOSITORY"
set_secret MACOS_CERTIFICATE_P12      "$(base64 < "$CERTIFICATE_P12" | tr -d '\n')"
set_secret MACOS_CERTIFICATE_PASSWORD "$P12_PASSWORD"
set_secret MACOS_SIGNING_IDENTITY     "$SIGNING_IDENTITY"
set_secret NOTARY_KEY_P8              "$(base64 < "$NOTARY_KEY_P8" | tr -d '\n')"
set_secret NOTARY_KEY_ID              "$NOTARY_KEY_ID"
set_secret NOTARY_ISSUER              "$NOTARY_ISSUER"

if [[ -z "$dry_run" ]]; then
  echo
  gh secret list --repo "$REPOSITORY"
  echo
  echo "Done.  The next push builds a signed application; the next v<version> tag"
  echo "releases signed, notarised disk images."
fi
