#!/usr/bin/env bash
# bin/check-release-env.sh — probe the PUBLISH-time toolchain before the first upload.
#
# Sibling of bin/check-env.sh (contributor setup), but scoped to what a release actually
# needs: newer/stricter tool versions than day-to-day development, plus registry credential
# presence. Run this BEFORE bin/prepare-release.sh's upload steps — the two 0.17.5 traps
# (JDK 11 default vs the nmcp plugin's JDK 17+ requirement; twine 6.2.0 silently capped by
# an old system Python) each surfaced mid-publish-run instead of up front. See #704.
#
# Usage:
#   bin/check-release-env.sh
#
# Exits 0 if every check passes, 1 otherwise. Prints a summary either way.
# See docs/release-workflow.md ("Access prerequisites") for the credential setup this
# script checks the presence of.

set -uo pipefail

case "${1:-}" in
    -h|--help)
        sed -n '2,16p' "$0" | sed 's/^# \{0,1\}//'
        exit 0
        ;;
    "") ;;
    *)
        echo "Unknown argument: $1 (try --help)" >&2
        exit 2
        ;;
esac

HYDRA_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

if [ -t 1 ]; then
    GREEN=$'\033[0;32m'
    RED=$'\033[0;31m'
    DIM=$'\033[2m'
    RESET=$'\033[0m'
else
    GREEN=""; RED=""; DIM=""; RESET=""
fi

hard_fail=0
declare -a FAILED=()

ok() {
    printf "  %sOK %s %-58s %s%s%s\n" "${GREEN}" "${RESET}" "$1" "${DIM}" "$2" "${RESET}"
}
fail() {
    printf "  %sX  %s %-58s %sFAILED%s\n" "${RED}" "${RESET}" "$1" "${RED}" "${RESET}"
    [ -n "${2:-}" ] && printf "       %s\n" "$2"
    hard_fail=1
    FAILED+=("$1")
}

printf "Hydra release environment check\n"
printf "================================\n\n"

# --- JDK 17+ (Maven Central nmcp plugin; blocks both publish-maven.sh and
# publish-sbt.sh) — verbatim port of publish-maven.sh's guard. ------------------
JAVA_BIN="${JAVA_HOME:+$JAVA_HOME/bin/}java"
if command -v "$JAVA_BIN" >/dev/null 2>&1; then
    JAVA_VERSION_LINE="$("$JAVA_BIN" -version 2>&1 | head -1)"
    JAVA_MAJOR="$(printf '%s' "$JAVA_VERSION_LINE" | sed -E 's/.*version "([0-9]+).*/\1/')"
    if [ -n "$JAVA_MAJOR" ] && [ "$JAVA_MAJOR" -ge 17 ] 2>/dev/null; then
        ok "JDK 17+ (nmcp Central Portal plugin)" "$JAVA_VERSION_LINE"
    else
        fail "JDK 17+ (nmcp Central Portal plugin)" \
            "Current: $JAVA_VERSION_LINE — export JAVA_HOME=\$(/usr/libexec/java_home -v 19)  # or 17/18 (macOS), or point JAVA_HOME at a JDK 17+ install"
    fi
else
    fail "JDK 17+ (nmcp Central Portal plugin)" "java not found (JAVA_HOME=${JAVA_HOME:-<unset>})"
fi

# --- twine >= 7.0.0, under Python >= 3.10 -------------------------------------
# twine 7 requires Python >=3.10; `pip install --upgrade twine` against an older
# system Python silently caps at 6.2.0, which then rejects hatchling's
# Metadata-Version 2.5 output. Report the Python-floor and the twine-version
# checks as two separate lines so that failure mode is legible instead of a
# generic "twine missing".
if command -v twine >/dev/null 2>&1; then
    TWINE_PY_VERSION="$(twine --version 2>&1 | grep -oE 'Python [0-9]+\.[0-9]+' | head -n1 | awk '{print $2}')"
    TWINE_PY_NUM="$(printf '%s' "$TWINE_PY_VERSION" | awk -F. '{print $1*100+$2}')"
    if [ -n "$TWINE_PY_NUM" ] && [ "$TWINE_PY_NUM" -ge 310 ] 2>/dev/null; then
        ok "twine runs under Python >=3.10" "Python $TWINE_PY_VERSION"
    else
        fail "twine runs under Python >=3.10" \
            "twine is running under Python ${TWINE_PY_VERSION:-<unknown>} — twine>=7 needs >=3.10, so an upgrade attempt SILENTLY caps at 6.2.0 here. Install twine into a 3.10+ environment (e.g. uv tool install twine, or pipx install --python python3.12 twine)."
    fi

    TWINE_VERSION="$(twine --version 2>&1 | grep -oE 'twine version [0-9]+\.[0-9]+(\.[0-9]+)?' | awk '{print $3}')"
    TWINE_MAJOR="$(printf '%s' "$TWINE_VERSION" | awk -F. '{print $1}')"
    if [ -n "$TWINE_MAJOR" ] && [ "$TWINE_MAJOR" -ge 7 ] 2>/dev/null; then
        ok "twine >= 7.0.0" "twine $TWINE_VERSION"
    else
        fail "twine >= 7.0.0" \
            "Installed: twine ${TWINE_VERSION:-<unknown>} — twine<7's bundled packaging rejects hatchling's Metadata-Version 2.5 with InvalidDistribution. Upgrade: pip install --upgrade 'twine>=7' (must run under Python >=3.10)."
    fi
else
    fail "twine >= 7.0.0" "twine not found on PATH. Install into a Python >=3.10 environment: pip install 'twine>=7'"
fi

# --- sbt present ---------------------------------------------------------------
if command -v sbt >/dev/null 2>&1; then
    ok "sbt (Scala publish)" "$(sbt --numeric-version 2>&1 | tail -n1)"
else
    fail "sbt (Scala publish)" "sbt not found. brew install sbt  |  see https://www.scala-sbt.org/download.html"
fi

# --- gpg present AND agent produces a working signature -----------------------
# Verbatim port of publish-sbt.sh's preflight: command -v alone doesn't catch a
# dead/misconfigured gpg-agent, which would otherwise fail partway through a
# multi-package signed bundle upload. Honors HYDRA_PGP_KEY like the original.
if command -v gpg >/dev/null 2>&1; then
    _sign_probe="$(mktemp)"; printf 'hydra-release-signing-probe\n' > "$_sign_probe"
    _sign_key_flag=()
    [ -n "${HYDRA_PGP_KEY:-}" ] && _sign_key_flag=(--local-user "$HYDRA_PGP_KEY")
    if gpg --batch --yes ${_sign_key_flag[@]+"${_sign_key_flag[@]}"} --detach-sign --armor \
           -o "$_sign_probe.asc" "$_sign_probe" 2>"$_sign_probe.err" \
       && gpg --verify "$_sign_probe.asc" "$_sign_probe" 2>/dev/null; then
        ok "gpg-agent produces a working signature" "$(command -v gpg)"
    else
        fail "gpg-agent produces a working signature" \
            "gpg could not sign (see $_sign_probe.err). Common fix: gpgconf --kill gpg-agent, or use a stable gpg if a dev build (2.5.x) is active. Set HYDRA_PGP_KEY to pin a signing key."
    fi
    rm -f "$_sign_probe" "$_sign_probe.asc"
else
    fail "gpg-agent produces a working signature" "gpg not found on PATH — required for Scala publishSigned and source-archive signing."
fi

# --- node satisfies heads/typescript's declared engines.node floor -------------
# Read the range dynamically (never hardcoded) so this tracks package.json as
# the single source of truth. #708 ratified the floor as `^18.0.0 || >=20.0.0`
# (the true vitest constraint — node 19 is explicitly unsupported); validated via
# `npx semver`, the same range engine npm itself uses, rather than a hand-rolled
# parser.
TS_PKG_JSON="$HYDRA_ROOT/heads/typescript/package.json"
if command -v node >/dev/null 2>&1 && [ -f "$TS_PKG_JSON" ]; then
    NODE_VERSION="$(node --version 2>&1 | sed 's/^v//')"
    ENGINES_NODE="$(python3 -c "import json,sys; print(json.load(open(sys.argv[1])).get('engines',{}).get('node','<none>'))" "$TS_PKG_JSON" 2>/dev/null)"
    if [ "$ENGINES_NODE" != "<none>" ] && [ -n "$ENGINES_NODE" ] \
       && npx --yes semver -r "$ENGINES_NODE" "$NODE_VERSION" >/dev/null 2>&1; then
        ok "node satisfies heads/typescript engines.node" "installed: v$NODE_VERSION, declared: $ENGINES_NODE"
    else
        fail "node satisfies heads/typescript engines.node" \
            "installed: v$NODE_VERSION does not satisfy declared range '$ENGINES_NODE'. Install a matching node (nvm install 20, or 18.x)."
    fi
else
    fail "node satisfies heads/typescript engines.node" "node not found, or $TS_PKG_JSON missing"
fi

# --- Registry credential presence (presence-only; never print secret values) --
# Each check mirrors the exact fallback logic the corresponding publish-*.sh
# already uses, so a pass here means that script's own guard will also pass.
if [ -n "${HACKAGE_TOKEN:-}" ] || [ -f "$HOME/.config/cabal/config" ]; then
    ok "Hackage credentials" "HACKAGE_TOKEN or ~/.config/cabal/config"
else
    fail "Hackage credentials" "Set HACKAGE_TOKEN, or configure cabal auth (~/.config/cabal/config)."
fi

GRADLE_PROPS="${GRADLE_USER_HOME:-$HOME/.gradle}/gradle.properties"
if [ -f "$GRADLE_PROPS" ] && grep -qiE "sonatype|signing" "$GRADLE_PROPS"; then
    ok "Maven Central credentials (Java)" "$GRADLE_PROPS"
else
    fail "Maven Central credentials (Java)" "No sonatype/signing keys in $GRADLE_PROPS. See docs/release-workflow.md (Java releases)."
fi

SBT_SONATYPE_CONF="${SBT_USER_HOME:-$HOME/.sbt}/1.0/sonatype.sbt"
if { [ -n "${SONATYPE_USERNAME:-}" ] && [ -n "${SONATYPE_PASSWORD:-}" ]; } || [ -f "$SBT_SONATYPE_CONF" ]; then
    ok "Maven Central credentials (Scala)" "SONATYPE_USERNAME/PASSWORD or $SBT_SONATYPE_CONF"
else
    fail "Maven Central credentials (Scala)" "Set SONATYPE_USERNAME + SONATYPE_PASSWORD, or add $SBT_SONATYPE_CONF."
fi

if [ -f "$HOME/.pypirc" ]; then
    ok "PyPI credentials" "~/.pypirc"
else
    fail "PyPI credentials" "No ~/.pypirc found. See docs/release-workflow.md (Python releases) for token setup."
fi

if [ -n "${NPM_TOKEN:-}" ]; then
    ok "npm credentials" "NPM_TOKEN set"
elif command -v npm >/dev/null 2>&1 && npm whoami >/dev/null 2>&1; then
    ok "npm credentials" "logged in as $(npm whoami 2>/dev/null)"
else
    fail "npm credentials" "Set NPM_TOKEN, or run npm login."
fi

printf "\n"
if [ "$hard_fail" -eq 0 ]; then
    printf "%sAll release-environment checks passed.%s\n" "${GREEN}" "${RESET}"
    exit 0
else
    printf "%s%d check(s) failed:%s %s\n" "${RED}" "${#FAILED[@]}" "${RESET}" "${FAILED[*]}"
    printf "Fix the above before starting a publish run — each failure mirrors a guard\n"
    printf "in the corresponding publish-*.sh that would otherwise fire mid-release.\n"
    exit 1
fi
