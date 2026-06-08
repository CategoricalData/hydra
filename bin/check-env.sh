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
    "kernel,triad,full|Python 3.12+ on PATH (required for generated PEP 695 syntax in test_json.py)|__python_312__|__python_312__|brew install python@3.12  |  apt install python3.12, or 'uv python install 3.12 && ln -s ~/.local/share/uv/python/cpython-3.12-*/bin/python3.12 ~/.local/bin/python3'"
    "triad,full,go|JDK (>=11)|java|java -version|brew install --cask temurin@17  |  apt install openjdk-17-jdk"
    "triad,full,go|JAVA_HOME set + valid (bin/sync.sh requires it)|__java_home__|__java_home__|export JAVA_HOME=\$(brew --prefix temurin)/Contents/Home  |  export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64 (or your distro's path)"
    "triad,full|uv (Python tool; required when Python is host)|uv|uv --version|curl -LsSf https://astral.sh/uv/install.sh | sh"
    "triad,full|pytest on PATH running on Python 3.12+ (demo runner uses bare \`pytest\`)|__pytest_312__|__pytest_312__|pip install pytest (into a Python 3.12+ env)  |  source heads/python/.venv/bin/activate after a first 'uv run pytest' from heads/python/"
    "full|Scala sbt|sbt|sbt --numeric-version|brew install sbt  |  see https://www.scala-sbt.org/download.html"
    "full|Clojure CLI|clojure|clojure --version|brew install clojure/tools/clojure  |  see https://clojure.org/guides/install_clojure"
    "full|SBCL (Common Lisp)|sbcl|sbcl --version|brew install sbcl  |  apt install sbcl"
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
# Special-case __java_home__: validate JAVA_HOME is exported and $JAVA_HOME/bin/java runs.
# Special-case __python_312__: validate `python3` is Python 3.12+.
# Special-case __pytest_312__: validate the `pytest` on PATH runs against Python 3.12+.
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
    if [ "$bin" = "__java_home__" ]; then
        if [ -z "${JAVA_HOME:-}" ]; then
            REAL_BIN=""; REAL_VERSION="JAVA_HOME is not set"
            return 1
        fi
        if [ ! -x "$JAVA_HOME/bin/java" ]; then
            REAL_BIN="$JAVA_HOME"; REAL_VERSION="JAVA_HOME/bin/java is not executable"
            return 1
        fi
        REAL_BIN="$JAVA_HOME"
        REAL_VERSION="$("$JAVA_HOME/bin/java" -version 2>&1 | head -n1)"
        return 0
    fi
    if [ "$bin" = "__python_312__" ]; then
        if ! command -v python3 >/dev/null 2>&1; then
            REAL_BIN=""; REAL_VERSION="python3 not on PATH"
            return 1
        fi
        REAL_BIN="$(command -v python3)"
        REAL_VERSION="$(python3 --version 2>&1 | head -n1)"
        local v
        v="$(python3 -c 'import sys; print(sys.version_info[0]*100+sys.version_info[1])' 2>/dev/null)"
        if [ -z "$v" ] || [ "$v" -lt 312 ]; then
            return 1
        fi
        return 0
    fi
    if [ "$bin" = "__pytest_312__" ]; then
        if ! command -v pytest >/dev/null 2>&1; then
            REAL_BIN=""; REAL_VERSION="pytest not on PATH"
            return 1
        fi
        REAL_BIN="$(command -v pytest)"
        REAL_VERSION="$(pytest --version 2>&1 | head -n1)"
        local v
        v="$(pytest --version 2>&1 | grep -oE 'Python [0-9]+\.[0-9]+' | head -n1 | awk '{print $2}' | awk -F. '{print $1*100+$2}')"
        if [ -z "$v" ]; then
            v="$(pytest -c /dev/null --co -q 2>&1 | grep -oE 'Python [0-9]+\.[0-9]+' | head -n1 | awk '{print $2}' | awk -F. '{print $1*100+$2}')"
        fi
        if [ -z "$v" ]; then
            v="$(python3 -c 'import pytest, sys; print(sys.version_info[0]*100+sys.version_info[1])' 2>/dev/null)"
        fi
        if [ -z "$v" ] || [ "$v" -lt 312 ]; then
            REAL_VERSION="$REAL_VERSION (running on Python <3.12)"
            return 1
        fi
        return 0
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
