#!/bin/bash
# Activation script for the pixi haskell environment.
#
# Uses the official GHCup installer to manage GHC, Stack, and Cabal.
# All tools are installed into $PIXI_PROJECT_ROOT/.ghcup (project-local).
#
# Versions can be overridden:
#   HYDRA_GHC_VERSION   (default: 9.10.2, matching lts-24.7)
#   HYDRA_STACK_VERSION (default: recommended)
#   HYDRA_CABAL_VERSION (default: recommended)

GHC_VERSION="${HYDRA_GHC_VERSION:-9.10.2}"
STACK_VERSION="${HYDRA_STACK_VERSION:-recommended}"
CABAL_VERSION="${HYDRA_CABAL_VERSION:-recommended}"

export GHCUP_INSTALL_BASE_PREFIX="${PIXI_PROJECT_ROOT}"
GHCUP="${PIXI_PROJECT_ROOT}/.ghcup/bin/ghcup"

# Ensure a C++ compiler and headers are available for GHC's configure step.
# Homebrew's clang++ lacks its own header search paths; prefer Homebrew g++ if available.
BREW_PREFIX="${HOMEBREW_PREFIX:-/home/linuxbrew/.linuxbrew}"
if [ -d "$BREW_PREFIX" ]; then
    # Find the newest g++-* version from Homebrew
    BREW_GPP=$(ls "$BREW_PREFIX/bin"/g++-* 2>/dev/null | sort -V | tail -1)
    if [ -n "$BREW_GPP" ]; then
        # Create a temporary bin dir with a g++ symlink so configure finds it
        GHCUP_SHIM_DIR="${PIXI_PROJECT_ROOT}/.ghcup/shims"
        mkdir -p "$GHCUP_SHIM_DIR"
        if [ ! -e "$GHCUP_SHIM_DIR/g++" ]; then
            ln -sf "$BREW_GPP" "$GHCUP_SHIM_DIR/g++"
        fi
        export PATH="$GHCUP_SHIM_DIR:$PATH"
    fi

    # Also export C++ include paths for compilers that need them
    for d in "$BREW_PREFIX"/include/c++/*/; do
        if [ -d "$d" ]; then
            export CPLUS_INCLUDE_PATH="${d}:${d}x86_64-pc-linux-gnu:${CPLUS_INCLUDE_PATH:-}"
            break
        fi
    done
fi

# Install GHCup itself if not present
if [ ! -x "$GHCUP" ]; then
    echo "Installing GHCup into ${PIXI_PROJECT_ROOT}/.ghcup ..."
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
        BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
        BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
        BOOTSTRAP_HASKELL_INSTALL_NO_STACK_HOOK=1 \
        BOOTSTRAP_HASKELL_GHC_VERSION="$GHC_VERSION" \
        BOOTSTRAP_HASKELL_ADJUST_BASHRC=0 \
        GHCUP_INSTALL_BASE_PREFIX="${PIXI_PROJECT_ROOT}" \
        sh
fi

# Install and set GHC
if ! "$GHCUP" list -t ghc -r -c installed 2>/dev/null | grep -q "$GHC_VERSION"; then
    echo "Installing GHC $GHC_VERSION via GHCup..."
    "$GHCUP" install ghc "$GHC_VERSION"
fi
"$GHCUP" set ghc "$GHC_VERSION" 2>/dev/null

# Install Stack
if ! "$GHCUP" list -t stack -r -c installed 2>/dev/null | grep -q "stack"; then
    echo "Installing Stack via GHCup..."
    "$GHCUP" install stack "$STACK_VERSION"
fi

# Install Cabal
if ! "$GHCUP" list -t cabal -r -c installed 2>/dev/null | grep -q "cabal"; then
    echo "Installing Cabal via GHCup..."
    "$GHCUP" install cabal "$CABAL_VERSION"
fi
