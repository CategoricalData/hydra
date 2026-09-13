#!/usr/bin/env bash
# with-stack-slot.sh — serialize shared-~/.stack GHC builds across the Hydra fleet.
#
# PROBLEM (#730): every worktree shares one ~/.stack (STACK_ROOT is unset), so two
# concurrent `stack build`s race on the global package DB and fail at link with no
# clear error, or drive the machine into memory pressure and get OOM-SIGKILL'd. The
# fleet historically serialized these by hand — the coordinator would `pgrep` for
# heavy builds before telling an agent "the slot is clear." That monitor is racy
# (two agents can both read "clear" at once) and cannot free a slot when a holder
# dies. This wrapper replaces it with a real mutex.
#
# THIS WRAPPER: acquires an exclusive flock() on a single shared lockfile, runs the
# wrapped command while holding it, and releases on exit. Any build that ultimately
# spawns a shared-~/.stack GHC compile must run *under* this wrapper. Because the lock
# is held for the whole child process tree (fd 9 is inherited), it does not matter
# whether the child runs `stack build` directly or three wrappers deep — the slot is
# serialized end to end, and it auto-releases the moment the holder tree exits or dies.
#
# USAGE
#   bin/with-stack-slot.sh [--timeout SECONDS] [--label TEXT] -- <command> [args...]
#   bin/with-stack-slot.sh <command> [args...]           # '--' optional if unambiguous
#
# EXAMPLES
#   bin/with-stack-slot.sh --label "staging /sync" -- bin/sync.sh --hosts all --targets all
#   bin/with-stack-slot.sh -- heads/haskell/bin/test-distribution.sh hydra-kernel
#
# WRAPPING A BUILD ENTRY POINT
#   Entry points self-re-exec through this wrapper with a guard at the top:
#     if [ -z "${HYDRA_STACK_SLOT_HELD:-}" ]; then
#       exec "$SCRIPT_DIR/with-stack-slot.sh" --label "<script>" -- "$0" "$@"
#     fi
#   Internal scripts (transform-json-to-target.sh, assemble-*, cold-seed-*, verify-*,
#   update-json-*) need NO guard — they inherit the slot from the wrapped parent.
#
# ENVIRONMENT
#   HYDRA_STACK_SLOT_LOCK   lockfile path        (default: ~/.hydra-stack-slot.lock)
#   HYDRA_STACK_SLOT_WAIT   default -w seconds   (default: 0 = block indefinitely)
#   HYDRA_STACK_SLOT_STALE  stale-holder age (s) (default: 21600 = 6h; 0 disables)
#   HYDRA_STACK_SLOT_BYPASS =1 skips the lock entirely (escape hatch; logs a warning)
#   HYDRA_STACK_SLOT_HELD   set by this wrapper for its child tree; presence => reentrant
#                           pass-through (do not set by hand).
#
# COORDINATOR OVERRIDE
#   A wedged holder still owns a live flock (that is the point — a dead holder frees it
#   automatically). To force-break a genuinely stuck holder, a coordinator kills it:
#       fuser -k ~/.hydra-stack-slot.lock     # kills whoever holds the lock — use with care
#
# EXIT STATUS
#   The wrapped command's own exit status, EXCEPT:
#     124  timed out waiting for the slot (mirrors coreutils `timeout`)
#     125  usage / internal error before the command ran

set -euo pipefail

LOCKFILE="${HYDRA_STACK_SLOT_LOCK:-$HOME/.hydra-stack-slot.lock}"
WAIT="${HYDRA_STACK_SLOT_WAIT:-0}"        # 0 => block until acquired
STALE="${HYDRA_STACK_SLOT_STALE:-21600}"  # 6h; 0 disables stale recovery
LABEL=""

# ---- arg parsing ----------------------------------------------------------
while [ $# -gt 0 ]; do
  case "$1" in
    --timeout) WAIT="${2:?--timeout needs a value}"; shift 2 ;;
    --timeout=*) WAIT="${1#*=}"; shift ;;
    --label)   LABEL="${2:-}"; shift 2 ;;
    --label=*) LABEL="${1#*=}"; shift ;;
    --lockfile) LOCKFILE="${2:?--lockfile needs a value}"; shift 2 ;;
    --lockfile=*) LOCKFILE="${1#*=}"; shift ;;
    --) shift; break ;;
    -*) echo "with-stack-slot: unknown option: $1" >&2; exit 125 ;;
    *) break ;;   # first non-option token starts the command
  esac
done

if [ $# -eq 0 ]; then
  echo "with-stack-slot: no command given (usage: with-stack-slot.sh [--timeout N] -- <cmd>...)" >&2
  exit 125
fi

# ---- bypass escape hatch --------------------------------------------------
if [ "${HYDRA_STACK_SLOT_BYPASS:-0}" = "1" ]; then
  echo "with-stack-slot: WARNING — HYDRA_STACK_SLOT_BYPASS=1, running WITHOUT the slot lock: $*" >&2
  exec "$@"
fi

# ---- reentrancy guard -----------------------------------------------------
# The lock is NOT reentrant: flock() on a second fd from a descendant would
# block on the ancestor that already holds it -> self-deadlock. If we are
# already inside a held slot (env marker inherited from the outer wrapper),
# just run the command directly. This makes it safe to wrap BOTH a top-level
# entry point (e.g. sync.sh) and an inner step it calls (e.g. test-distribution.sh)
# without deadlock — the outermost wrapper owns the slot for the whole tree.
if [ -n "${HYDRA_STACK_SLOT_HELD:-}" ]; then
  echo "with-stack-slot: already inside slot ${HYDRA_STACK_SLOT_HELD}; running nested command directly: $*" >&2
  exec "$@"
fi

# ---- portable flock(1) shim ------------------------------------------------
# flock(1) is util-linux and DOES NOT EXIST on macOS/BSD, where the entire Hydra
# fleet's laptop tier runs. Without a shim, every entry point that re-execs through
# this wrapper (sync.sh, test.sh, run-bootstrapping-demo.sh, prepare-release.sh,
# regenerate-lexicon.sh, ...) dies with "flock: command not found" — and worse, a
# `flock -n 9` that fails with 127 is indistinguishable from "lock is held", so the
# script silently fell through to the blocking path and failed there too.
#
# We keep real flock(1) where it exists (Linux CI) because kernel-held locks release
# automatically when the holder dies. Where it does not, we emulate just the three
# forms this script uses (-n, blocking, -w SECONDS) with Perl's flock(), which binds
# the same advisory-lock syscall and is present in the macOS base system. Perl holds
# the lock on the inherited fd for as long as it runs, so we must NOT let it exit
# while we need the lock; instead we test acquisition and rely on our own fd 9.
if command -v flock >/dev/null 2>&1; then
  _slot_flock() { flock "$@"; }
elif command -v perl >/dev/null 2>&1; then
  # _slot_flock [-n | -w SECS] FD  — mirrors the flock(1) exit contract:
  #   0 = acquired, non-zero = not acquired (1 for -n, 1 for -w timeout).
  _slot_flock() {
    local mode="block" secs=0 fd
    case "${1:-}" in
      -n) mode="nb"; fd="${2:?}" ;;
      -w) mode="timed"; secs="${2:?}"; fd="${3:?}" ;;
      *)  fd="${1:?}" ;;
    esac
    HYDRA_SLOT_FD="$fd" HYDRA_SLOT_MODE="$mode" HYDRA_SLOT_SECS="$secs" perl -e '
      my $fd   = $ENV{HYDRA_SLOT_FD};
      my $mode = $ENV{HYDRA_SLOT_MODE};
      my $secs = $ENV{HYDRA_SLOT_SECS} || 0;
      open(my $fh, ">&=", $fd) or exit 2;   # reuse the ALREADY-OPEN inherited fd
      my $LOCK_EX = 2; my $LOCK_NB = 4;
      if ($mode eq "nb") {
        exit(flock($fh, $LOCK_EX | $LOCK_NB) ? 0 : 1);
      } elsif ($mode eq "timed") {
        my $deadline = time + $secs;
        while (1) {
          exit 0 if flock($fh, $LOCK_EX | $LOCK_NB);
          exit 1 if time >= $deadline;
          select(undef, undef, undef, 0.2);
        }
      } else {
        exit(flock($fh, $LOCK_EX) ? 0 : 1);  # blocks in-kernel until acquired
      }
    '
  }
else
  echo "with-stack-slot: neither flock(1) nor perl is available; cannot serialize builds." >&2
  echo "with-stack-slot: install util-linux (Linux) or use HYDRA_STACK_SLOT_BYPASS=1 to skip the lock." >&2
  exit 125
fi

# Identify this holder. Prefer the git worktree name for human readability.
WORKTREE="$(git rev-parse --show-toplevel 2>/dev/null | sed 's#.*/worktrees/##' || echo "$PWD")"
# started= is recorded as BOTH a human-readable local timestamp and epoch seconds.
# startedEpoch is what stale detection parses: `date -d` (GNU-only) is unavailable on
# BSD/macOS, and `date -Is` is likewise GNU-only, so neither may be used here.
HOLDER_DESC="pid=$$ worktree=${WORKTREE} label=${LABEL:-none} started=$(date '+%Y-%m-%dT%H:%M:%S%z') startedEpoch=$(date +%s) cmd=[$*]"

# The metadata file sits next to the lock and records the current holder. It is
# advisory (for humans + stale detection); the flock() on $LOCKFILE is the real mutex.
METAFILE="${LOCKFILE}.holder"

acquire_epoch=$(date +%s)

# fd 9 carries the lock for the life of this shell (and its children, which
# inherit it). flock releases automatically when fd 9 closes — i.e. when this
# process and every child that inherited fd 9 have exited. That is exactly the
# "held for the whole build tree" semantics we want.
exec 9>"$LOCKFILE"

# ---- who-holds-it reporting + stale note ----------------------------------
# flock() itself is auto-released if the holder dies, so a crashed holder never
# actually keeps the mutex. The stale check below only inspects the *advisory*
# METAFILE and warns; it never force-breaks a live flock (which would defeat the
# whole point). We warn loudly if someone appears wedged.
report_holder() {
  if [ -r "$METAFILE" ]; then
    echo "with-stack-slot: slot currently held by:" >&2
    sed 's/^/    /' "$METAFILE" >&2
  fi
}

# Try a non-blocking grab first so we can report who holds it and enforce -w.
if ! _slot_flock -n 9; then
  report_holder
  # Stale-metadata note (informational only; flock liveness is authoritative).
  if [ "$STALE" != "0" ] && [ -r "$METAFILE" ]; then
    held_epoch=$(sed -n 's/.*startedEpoch=\([0-9]*\).*/\1/p' "$METAFILE" | head -1)
    if [ -n "$held_epoch" ] && [ "$held_epoch" != "0" ]; then
      age=$(( acquire_epoch - held_epoch ))
      if [ "$age" -gt "$STALE" ]; then
        echo "with-stack-slot: WARNING — recorded holder is ${age}s old (> ${STALE}s stale threshold)." >&2
        echo "with-stack-slot:   flock is still live, so the process is alive but wedged, OR" >&2
        echo "with-stack-slot:   the metafile is stale. Inspect the pid above; a coordinator may" >&2
        echo "with-stack-slot:   override with:  fuser -k ${LOCKFILE}   (kills the holder — use with care)." >&2
      fi
    fi
  fi

  if [ "$WAIT" = "0" ]; then
    echo "with-stack-slot: waiting for the build slot (blocking; set --timeout N to bound the wait)..." >&2
    _slot_flock 9   # block until acquired
  else
    echo "with-stack-slot: waiting up to ${WAIT}s for the build slot..." >&2
    if ! _slot_flock -w "$WAIT" 9; then
      echo "with-stack-slot: timed out after ${WAIT}s waiting for the slot; giving up." >&2
      exit 124
    fi
  fi
fi

# ---- we hold the slot -----------------------------------------------------
printf '%s\n' "$HOLDER_DESC" > "$METAFILE"
waited=$(( $(date +%s) - acquire_epoch ))
echo "with-stack-slot: acquired build slot after ${waited}s — ${HOLDER_DESC}" >&2

# Clear the advisory metafile when we release, whatever the outcome.
cleanup() { : > "$METAFILE" 2>/dev/null || true; }
trap cleanup EXIT

# Run the wrapped command while holding fd 9. Children inherit fd 9, so the lock
# stays held until the entire tree exits. Capture status to return it verbatim.
# Export the reentrancy marker so nested wrappers pass through instead of deadlocking.
export HYDRA_STACK_SLOT_HELD="$HOLDER_DESC"
set +e
"$@"
status=$?
set -e

exit "$status"
