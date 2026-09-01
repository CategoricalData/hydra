#!/usr/bin/env bash
# gpg wrapper that pins the signing identity for release tooling.
#
# sbt-pgp runs with useGpg=true, which shells out to `gpg` WITHOUT passing
# --local-user, so its `pgpSigningKey` setting is inert and gpg falls back to
# the operator's DEFAULT key. For Hydra that is the maintainer's personal key,
# not the release key declared in the repo-root KEYS file — the mismatch that
# shipped wrong-key Maven signatures at 0.17.5.
#
# Point sbt-pgp's `gpgCommand` at this script and set HYDRA_PGP_KEY; every
# signing call then carries --local-user. With HYDRA_PGP_KEY unset this is a
# transparent passthrough, so it is safe as a default.
set -euo pipefail
GPG_BIN="${HYDRA_GPG_BIN:-gpg}"
if [ -n "${HYDRA_PGP_KEY:-}" ]; then
    exec "$GPG_BIN" --local-user "$HYDRA_PGP_KEY" "$@"
fi
exec "$GPG_BIN" "$@"
