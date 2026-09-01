#!/usr/bin/env bash
set -u
LOG="$1"; shift
export PATH="$HOME/.nvm/versions/node/v22.16.0/bin:/opt/homebrew/bin:$PATH"
export JAVA_HOME="$(/usr/libexec/java_home -v 19)"
export HYDRA_RELEASE_SIGNING_KEY=FC93F19114D72013
cd /Users/josh/projects/github/CategoricalData/hydra/worktrees/release-0.17.6
{ echo "=== launched $(date +%Y-%m-%dT%H:%M:%S) ==="; echo "=== cmd: $* ==="
  echo "=== gpg: $(command -v gpg) | java: $JAVA_HOME | node: $(node --version) ==="; } >> "$LOG"
"$@" >> "$LOG" 2>&1
code=$?
echo "$code" > "$LOG.exitcode"
echo "=== finished $(date +%Y-%m-%dT%H:%M:%S) exit=$code ===" >> "$LOG"
