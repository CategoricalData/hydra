#!/usr/bin/env bash
# bin/check-env.sh — probe contributor environment for the tools required by a given scope.
#
# Usage:
#   bin/check-env.sh                # default scope: --triad
#   bin/check-env.sh --kernel       # Haskell + Python (minimum kernel work)
#   bin/check-env.sh --triad        # adds JDK (haskell/java/python sync + bootstrap)
#   bin/check-env.sh --full         # adds Scala + four Lisp dialects (full 8 x 8 matrix)
#   bin/check-env.sh --go           # just the Go toolchain (head bud)
#   bin/check-env.sh --authoring    # just the GitHub CLI
#   bin/check-env.sh --all          # union of every scope above
#
# Exits 0 if every required tool in scope is present, 1 otherwise.
# See docs/contributor-setup.md for the underlying tier model.

set -uo pipefail

SCOPE="${1:-}"
case "${SCOPE}" in
    ""|--triad)   SCOPE="triad"   ;;
    --kernel)     SCOPE="kernel"  ;;
    --full)       SCOPE="full"    ;;
    --go)         SCOPE="go"      ;;
    --authoring)  SCOPE="authoring" ;;
    --all)        SCOPE="all"     ;;
    -h|--help)
        sed -n '2,14p' "$0" | sed 's/^# \{0,1\}//'
        exit 0
        ;;
    *)
        echo "Unknown scope: ${SCOPE}" >&2
        echo "Run '$0 --help' for usage." >&2
        exit 2
        ;;
esac

# Color output only when stdout is a TTY.
if [ -t 1 ]; then
    GREEN=$'\033[0;32m'
    RED=$'\033[0;31m'
    DIM=$'\033[2m'
    RESET=$'\033[0m'
else
    GREEN=""; RED=""; DIM=""; RESET=""
fi

# Each tool entry is: SCOPES|FRIENDLY_NAME|BIN|VERSION_CMD|INSTALL_HINT
# - SCOPES is a comma-separated list of scope names that include this tool.
# - VERSION_CMD writes to stdout *or* stderr; we capture both and take the first line.
# - INSTALL_HINT is a single suggestion suitable for both macOS and Linux.
TOOLS=(
    "kernel,triad,full,go|Haskell Stack|stack|stack --numeric-version|brew install haskell-stack  |  curl -sSL https://get.haskellstack.org/ | sh"
    "kernel,triad,full,go|Python 3|python3|python3 --version|brew install python@3.12  |  apt install python3"
    "triad,full,go|JDK (>=11)|java|java -version|brew install --cask temurin@17  |  apt install openjdk-17-jdk"
    "triad,full|uv (Python tool; required when Python is host)|uv|uv --version|curl -LsSf https://astral.sh/uv/install.sh | sh"
    "full|Scala sbt|sbt|sbt --numeric-version|brew install sbt  |  see https://www.scala-sbt.org/download.html"
    "full|Clojure CLI|clojure|clojure --version|brew install clojure/tools/clojure  |  see https://clojure.org/guides/install_clojure"
    "full|SBCL + cl-ppcre|__sbcl_clppcre__|__sbcl_clppcre__|brew install sbcl  |  apt install sbcl; then install Quicklisp + cl-ppcre per docs/contributor-setup.md"
    "full|Emacs|emacs|emacs --version|brew install emacs  |  apt install emacs-nox"
    "full|Scheme (Guile or chibi-scheme)|__scheme__|__scheme__|brew install guile  |  apt install guile-3.0"
    "go|Go|go|go version|brew install go  |  see https://go.dev/doc/install"
    "authoring|GitHub CLI|gh|gh --version|brew install gh  |  see https://github.com/cli/cli#installation"
)

scope_match() {
    local entry_scopes="$1"
    case "${SCOPE}" in
        all) return 0 ;;
    esac
    IFS=',' read -ra arr <<< "${entry_scopes}"
    for s in "${arr[@]}"; do
        [ "$s" = "${SCOPE}" ] && return 0
    done
    return 1
}

# Resolve a tool's version. Returns "missing" if the binary is not on PATH.
# Special-case Scheme: try guile first, then chibi-scheme.
# Special-case SBCL+cl-ppcre: require sbcl on PATH AND (asdf:load-system :cl-ppcre)
#   to succeed silently — the CL target tests load cl-ppcre via ASDF, and a bare
#   sbcl without Quicklisp/cl-ppcre will crash mid-test with
#   "Component :CL-PPCRE not found". See docs/contributor-setup.md Tier 3.
resolve_tool() {
    local bin="$1" version_cmd="$2"
    if [ "$bin" = "__scheme__" ]; then
        if command -v guile >/dev/null 2>&1; then
            REAL_BIN="$(command -v guile)"
            REAL_VERSION="$(guile --version 2>&1 | head -n1)"
            return 0
        fi
        if command -v chibi-scheme >/dev/null 2>&1; then
            REAL_BIN="$(command -v chibi-scheme)"
            REAL_VERSION="$(chibi-scheme -V 2>&1 | head -n1)"
            return 0
        fi
        REAL_BIN=""; REAL_VERSION=""
        return 1
    fi
    if [ "$bin" = "__sbcl_clppcre__" ]; then
        if ! command -v sbcl >/dev/null 2>&1; then
            REAL_BIN=""; REAL_VERSION=""
            return 1
        fi
        REAL_BIN="$(command -v sbcl)"
        local sbcl_version
        sbcl_version="$(sbcl --version 2>&1 | head -n1)"
        if sbcl --non-interactive --eval '(asdf:load-system :cl-ppcre)' >/dev/null 2>&1; then
            REAL_VERSION="$sbcl_version  (cl-ppcre loadable)"
            return 0
        fi
        REAL_VERSION="$sbcl_version  (cl-ppcre NOT loadable — Quicklisp/cl-ppcre missing)"
        return 1
    fi
    if ! command -v "$bin" >/dev/null 2>&1; then
        REAL_BIN=""; REAL_VERSION=""
        return 1
    fi
    REAL_BIN="$(command -v "$bin")"
    REAL_VERSION="$(eval "$version_cmd" 2>&1 | head -n1)"
    return 0
}

printf "Hydra contributor environment check (scope: %s)\n" "${SCOPE}"
printf "%s\n\n" "================================================="

present=0
missing=0
declare -a MISSING_HINTS=()

for entry in "${TOOLS[@]}"; do
    IFS='|' read -r scopes name bin version_cmd hint <<< "$entry"
    scope_match "$scopes" || continue

    if resolve_tool "$bin" "$version_cmd"; then
        printf "  %s%s%s %-30s %s%s%s  %s%s%s\n" \
            "${GREEN}" "OK " "${RESET}" \
            "$name" \
            "" "$REAL_VERSION" "" \
            "${DIM}" "$REAL_BIN" "${RESET}"
        present=$((present + 1))
    else
        printf "  %s%s%s %-30s %sNOT FOUND%s\n" \
            "${RED}" "X  " "${RESET}" \
            "$name" \
            "${RED}" "${RESET}"
        printf "       %sInstall:%s %s\n" "${DIM}" "${RESET}" "$hint"
        missing=$((missing + 1))
        MISSING_HINTS+=("$name")
    fi
done

total=$((present + missing))
printf "\nResult: %d of %d tools present in scope '%s'.\n" "$present" "$total" "$SCOPE"

if [ "$missing" -eq 0 ]; then
    printf "%sAll required tools for scope '%s' are installed.%s\n" "${GREEN}" "$SCOPE" "${RESET}"
    exit 0
else
    printf "%s%d missing:%s %s\n" "${RED}" "$missing" "${RESET}" "${MISSING_HINTS[*]}"
    printf "See docs/contributor-setup.md for tier definitions and install pointers.\n"
    exit 1
fi
