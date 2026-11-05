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
#   bin/with-stack-slot.sh --priority [...] -- <command> [args...]  # go ahead of ordinary waiters
#   bin/with-stack-slot.sh --boost <worktree>            # coordinator: promote that worktree's waiter
#   bin/with-stack-slot.sh --unboost <worktree>          # coordinator: withdraw a boost
#   bin/with-stack-slot.sh --pin <worktree>              # coordinator: boost that persists across acquisitions
#   bin/with-stack-slot.sh --unpin <worktree>            # coordinator: withdraw a pin
#   bin/with-stack-slot.sh --assign <worktree> A|B       # coordinator: pin a worktree to a build lane
#   bin/with-stack-slot.sh --lanes                       # show lane assignments, holders, free memory
#
# BUILD LANES
#   There are two slots ("lanes"), so two Haskell builds can run at once. Lane A
#   is the original slot on the shared ~/.stack. Lane B uses its own STACK_ROOT
#   (~/.stack-laneB; programs/ is a symlink to ~/.stack/programs, so GHC itself is
#   shared read-only), so the two lanes never write the same package database --
#   the race this wrapper exists to prevent (#730). Each worktree is pinned to one
#   lane (default A; see --assign) because its heads/haskell/.stack-work is built
#   against that lane's snapshot database: moving a worktree to the other lane
#   costs one full rebuild. Lane B is the yielding lane: it needs more free memory
#   to start (HYDRA_STACK_SLOT_MEM_B, default 9216 MB, vs 6144 MB for lane A), and
#   while it runs a guard pauses its build (SIGSTOP) when available memory drops
#   below HYDRA_STACK_SLOT_PAUSE_MB (2560), resumes it above
#   HYDRA_STACK_SLOT_RESUME_MB (5120), and kills it if memory falls below
#   HYDRA_STACK_SLOT_KILL_MB (1024) while paused -- a failed build is better than a
#   livelocked machine.
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
#   HYDRA_STACK_SLOT_PRIORITY =1 is the same as --priority
#   HYDRA_STACK_SLOT_LOCK_B lane B lockfile     (default: ~/.hydra-stack-slot-b.lock)
#   HYDRA_STACK_ROOT_B      lane B STACK_ROOT   (default: ~/.stack-laneB)
#   HYDRA_STACK_LANES       lane assignments    (default: ~/.hydra-stack-lanes; "<worktree> B" lines)
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
LOCKFILE_A="$HOME/.hydra-stack-slot.lock"
LOCKFILE_B="${HYDRA_STACK_SLOT_LOCK_B:-$HOME/.hydra-stack-slot-b.lock}"
STACK_ROOT_B="${HYDRA_STACK_ROOT_B:-$HOME/.stack-laneB}"
LANES_FILE="${HYDRA_STACK_LANES:-$HOME/.hydra-stack-lanes}"
LOCK_OVERRIDE=0
[ -n "${HYDRA_STACK_SLOT_LOCK:-}" ] && LOCK_OVERRIDE=1
WAIT="${HYDRA_STACK_SLOT_WAIT:-0}"        # 0 => block until acquired
STALE="${HYDRA_STACK_SLOT_STALE:-21600}"  # 6h; 0 disables stale recovery
LABEL=""
PRIORITY="${HYDRA_STACK_SLOT_PRIORITY:-0}"
BOOST=""
UNBOOST=""
PIN=""
UNPIN=""
ASSIGN_WT=""
ASSIGN_LANE=""
SHOW_LANES=0

# lane_of <worktree>: prints A or B.
lane_of() {
  if [ -r "$LANES_FILE" ] && grep -qx "${1} B" "$LANES_FILE"; then echo B; else echo A; fi
}
lockfile_of_lane() { if [ "$1" = B ]; then echo "$LOCKFILE_B"; else echo "$LOCKFILE_A"; fi; }

# ---- arg parsing ----------------------------------------------------------
while [ $# -gt 0 ]; do
  case "$1" in
    --timeout) WAIT="${2:?--timeout needs a value}"; shift 2 ;;
    --timeout=*) WAIT="${1#*=}"; shift ;;
    --label)   LABEL="${2:-}"; shift 2 ;;
    --label=*) LABEL="${1#*=}"; shift ;;
    --priority) PRIORITY=1; shift ;;
    --boost)   BOOST="${2:?--boost needs a worktree name}"; shift 2 ;;
    --unboost) UNBOOST="${2:?--unboost needs a worktree name}"; shift 2 ;;
    --pin)     PIN="${2:?--pin needs a worktree name}"; shift 2 ;;
    --unpin)   UNPIN="${2:?--unpin needs a worktree name}"; shift 2 ;;
    --assign)  ASSIGN_WT="${2:?--assign needs a worktree name}"; ASSIGN_LANE="${3:?--assign needs A or B}"; shift 3 ;;
    --lanes)   SHOW_LANES=1; shift ;;
    --lockfile) LOCKFILE="${2:?--lockfile needs a value}"; LOCK_OVERRIDE=1; shift 2 ;;
    --lockfile=*) LOCKFILE="${1#*=}"; LOCK_OVERRIDE=1; shift ;;
    --) shift; break ;;
    -*) echo "with-stack-slot: unknown option: $1" >&2; exit 125 ;;
    *) break ;;   # first non-option token starts the command
  esac
done

# ---- coordinator boost (no command) ----------------------------------------
# A boost marks a worktree so its next (or current) waiter counts as a priority
# waiter. The marker is consumed when that worktree acquires the slot. A pin is a boost that is
# never consumed: every waiter from that worktree is a priority waiter until --unpin (for
# release-priority work that builds repeatedly).
if [ -n "$BOOST$UNBOOST$PIN$UNPIN" ]; then
  [ "$LOCK_OVERRIDE" = 1 ] || LOCKFILE="$(lockfile_of_lane "$(lane_of "${BOOST:-${UNBOOST:-${PIN:-$UNPIN}}}")")"
  mkdir -p "${LOCKFILE}.priority.d"
  if [ -n "$PIN" ]; then
    : > "${LOCKFILE}.priority.d/pin-${PIN//\//_}"
    echo "with-stack-slot: pinned ${PIN}; its waiters go ahead of ordinary waiters until --unpin." >&2
  elif [ -n "$UNPIN" ]; then
    rm -f "${LOCKFILE}.priority.d/pin-${UNPIN//\//_}"
    echo "with-stack-slot: removed pin for ${UNPIN}." >&2
  elif [ -n "$BOOST" ]; then
    : > "${LOCKFILE}.priority.d/wt-${BOOST//\//_}"
    echo "with-stack-slot: boosted ${BOOST}; its waiter goes ahead of ordinary waiters (within ~30s)." >&2
  else
    rm -f "${LOCKFILE}.priority.d/wt-${UNBOOST//\//_}"
    echo "with-stack-slot: removed boost for ${UNBOOST}." >&2
  fi
  exit 0
fi

# ---- coordinator lane commands (no command) ---------------------------------
if [ -n "$ASSIGN_WT" ]; then
  case "$ASSIGN_LANE" in A|B) ;; *) echo "with-stack-slot: --assign lane must be A or B" >&2; exit 125 ;; esac
  touch "$LANES_FILE"
  grep -vx "${ASSIGN_WT} B" "$LANES_FILE" > "${LANES_FILE}.tmp" || true
  [ "$ASSIGN_LANE" = B ] && echo "${ASSIGN_WT} B" >> "${LANES_FILE}.tmp"
  mv "${LANES_FILE}.tmp" "$LANES_FILE"
  echo "with-stack-slot: ${ASSIGN_WT} -> lane ${ASSIGN_LANE} (its next Haskell build in the new lane is a full rebuild)." >&2
  exit 0
fi
if [ "$SHOW_LANES" = 1 ]; then
  for l in A B; do
    lf="$(lockfile_of_lane "$l")"
    h="$(sed -n 's/.*worktree=\([^ ]*\).*started=\([^ ]*\).*/\1 since \2/p' "${lf}.holder" 2>/dev/null | head -1 || true)"
    echo "lane $l: ${h:-free}"
  done
  echo "lane B worktrees: $(awk '$2=="B"{printf "%s ", $1}' "$LANES_FILE" 2>/dev/null || true)"
  awk '/^MemAvailable:/{printf "memory available: %d MB\n", $2/1024}' /proc/meminfo 2>/dev/null || true
  exit 0
fi

if [ $# -eq 0 ]; then
  echo "with-stack-slot: no command given (usage: with-stack-slot.sh [--timeout N] -- <cmd>...)" >&2
  exit 125
fi

# ---- bypass escape hatch --------------------------------------------------
if [ "${HYDRA_STACK_SLOT_BYPASS:-0}" = "1" ]; then
  echo "with-stack-slot: WARNING — HYDRA_STACK_SLOT_BYPASS=1, running WITHOUT the slot lock: $*" >&2
  # Mark the slot as "held" for the child tree even though we took no lock. Entry-point
  # guards re-exec through this wrapper whenever HYDRA_STACK_SLOT_HELD is unset, so
  # exec'ing without setting it makes wrapper -> script -> wrapper loop forever (observed:
  # 55k iterations). The marker is what terminates that recursion; bypass still takes no lock.
  export HYDRA_STACK_SLOT_HELD="bypass (HYDRA_STACK_SLOT_BYPASS=1; no lock held)"
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
      -u) mode="un"; fd="${2:?}" ;;
      -w) mode="timed"; secs="${2:?}"; fd="${3:?}" ;;
      *)  fd="${1:?}" ;;
    esac
    HYDRA_SLOT_FD="$fd" HYDRA_SLOT_MODE="$mode" HYDRA_SLOT_SECS="$secs" perl -e '
      my $fd   = $ENV{HYDRA_SLOT_FD};
      my $mode = $ENV{HYDRA_SLOT_MODE};
      my $secs = $ENV{HYDRA_SLOT_SECS} || 0;
      open(my $fh, ">&=", $fd) or exit 2;   # reuse the ALREADY-OPEN inherited fd
      my $LOCK_EX = 2; my $LOCK_NB = 4;
      if ($mode eq "un") {
        exit(flock($fh, 8) ? 0 : 1);        # LOCK_UN
      } elsif ($mode eq "nb") {
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

# Pick this worktree's lane (an explicit --lockfile / HYDRA_STACK_SLOT_LOCK wins).
LANE=A
if [ "$LOCK_OVERRIDE" = 0 ] && [ "$(lane_of "$WORKTREE")" = B ]; then
  LANE=B
  LOCKFILE="$LOCKFILE_B"
  if [ ! -d "$STACK_ROOT_B" ]; then
    echo "with-stack-slot: lane B STACK_ROOT $STACK_ROOT_B does not exist; set it up (see BUILD LANES) or --assign ${WORKTREE} A." >&2
    exit 125
  fi
fi

# The metadata file sits next to the lock and records the current holder. It is
# advisory (for humans + stale detection); the flock() on $LOCKFILE is the real mutex.
METAFILE="${LOCKFILE}.holder"

acquire_epoch=$(date +%s)

# fd 9 carries the lock for the life of this shell (and its children, which
# inherit it). flock releases automatically when fd 9 closes — i.e. when this
# process and every child that inherited fd 9 have exited. That is exactly the
# "held for the whole build tree" semantics we want.
exec 9>"$LOCKFILE"

# ---- priority lane --------------------------------------------------------
# Waiters started with --priority, or from a worktree a coordinator boosted with
# `with-stack-slot.sh --boost <worktree>`, go ahead of ordinary waiters. See the
# wait loop below. Waiters running an older copy of this script ignore the lane
# and still compete normally.
PRIODIR="${LOCKFILE}.priority.d"
mkdir -p "$PRIODIR"

is_priority_waiter() {
  [ "$PRIORITY" = "1" ] || [ -e "$PRIODIR/wt-${WORKTREE//\//_}" ] || [ -e "$PRIODIR/pin-${WORKTREE//\//_}" ]
}

# True if a live priority waiter other than this process exists. Removes pid
# files whose process has gone (a waiter killed with SIGKILL leaves one behind).
other_priority_waiters() {
  local f pid found=1
  for f in "$PRIODIR"/pid-*; do
    [ -e "$f" ] || continue
    pid="${f##*/pid-}"
    [ "$pid" = "$$" ] && continue
    if kill -0 "$pid" 2>/dev/null; then found=0; else rm -f "$f"; fi
  done
  return $found
}

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

# A priority waiter's pid file must not outlive it.
trap 'rm -f "$PRIODIR/pid-$$"' EXIT

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
  else
    echo "with-stack-slot: waiting up to ${WAIT}s for the build slot..." >&2
  fi
  # flock() wakes waiters in no particular order, so without help a release goes to
  # an arbitrary waiter. Priority waiters (--priority, or a coordinator --boost of
  # this worktree) advertise themselves with a pid file in $PRIODIR; ordinary
  # waiters stand back while any live one exists, and hand the slot straight back
  # if they win a race against one. Waits are short and re-checked, so a boost
  # applied mid-wait takes effect within ~30s.
  while :; do
    step=30
    if [ "$WAIT" != "0" ]; then
      left=$(( WAIT - ($(date +%s) - acquire_epoch) ))
      if [ "$left" -le 0 ]; then
        echo "with-stack-slot: timed out after ${WAIT}s waiting for the slot; giving up." >&2
        exit 124
      fi
      [ "$left" -lt "$step" ] && step="$left"
    fi
    if is_priority_waiter; then
      : > "$PRIODIR/pid-$$"
      _slot_flock -w "$step" 9 && break
    elif other_priority_waiters; then
      sleep $(( step < 10 ? step : 10 ))
    elif _slot_flock -w $(( step < 10 ? step : 10 )) 9; then
      other_priority_waiters || break
      _slot_flock -u 9
      sleep 2
    fi
  done
fi

# ---- we hold the slot -----------------------------------------------------
# HOLDER_DESC's started/startedEpoch are stamped HERE, after acquisition, not at
# script entry — a holder that waited hours in queue and just started building must
# report an age of ~0s, not the full wait time. Stamping this before the blocking
# _slot_flock call (the original bug) made the stale-holder warning above measure
# queue-wait time instead of hold time, firing false "may be wedged" alerts on any
# build that simply queued for a long time — exactly what a busy fleet does daily.
# startedEpoch is what stale detection parses: `date -d` (GNU-only) is unavailable on
# BSD/macOS, and `date -Is` is likewise GNU-only, so neither may be used here.
HOLDER_DESC="pid=$$ lane=${LANE} worktree=${WORKTREE} label=${LABEL:-none} started=$(date '+%Y-%m-%dT%H:%M:%S%z') startedEpoch=$(date +%s) cmd=[$*]"
printf '%s\n' "$HOLDER_DESC" > "$METAFILE"
waited=$(( $(date +%s) - acquire_epoch ))
echo "with-stack-slot: acquired build slot after ${waited}s — ${HOLDER_DESC}" >&2

# Leave the priority lane: drop this waiter's pid file and consume any boost.
rm -f "$PRIODIR/pid-$$" "$PRIODIR/wt-${WORKTREE//\//_}"

# Clear the advisory metafile when we release, whatever the outcome.
cleanup() { : > "$METAFILE" 2>/dev/null || true; }
trap cleanup EXIT

# ---- disk-headroom guard (hydra-agents#3) ---------------------------------
# Checked HERE, after acquiring the slot, not before: a build that queued for
# hours could see disk free up or fill up during the wait, so the only check
# that matters is the one taken right before we actually let a build proceed.
# ~6GB is this fleet's measured .stack-work footprint (dist/ + install/) for
# a single worktree's full Haskell build (see docs/history/haskell-as-host-cost.md);
# two incidents of root disk hitting 0 while a build was already in flight,
# dying mid-write with ENOSPC, motivated this fail-fast guard over silent
# corruption. Sourced from the hydra-agents checkout (generic, project-agnostic
# function); only the 6144 MB threshold here is Hydra-specific.
DISK_CHECK_LIB="$(git rev-parse --show-toplevel 2>/dev/null)/../../external/agents/bin/lib-disk-check.sh"
if [ -r "$DISK_CHECK_LIB" ]; then
  # shellcheck disable=SC1090
  . "$DISK_CHECK_LIB"
  if ! ha_check_disk_headroom 6144; then
    exit 1
  fi
else
  echo "with-stack-slot: hydra-agents disk-check lib not found at $DISK_CHECK_LIB; skipping the guard (not fatal)." >&2
fi

# ---- memory-headroom guard (hydra-agents lib-mem-check.sh) ----------------
# Also checked after acquiring the slot, but this one WAITS instead of failing:
# memory pressure usually passes (a build finishes, a session exits). We keep
# the slot while waiting, so no other heavy build starts on a box that's out of
# room. On 2026-10-30 alpha froze (livelock, no OOM kill) when a slot was granted
# with swap ~93% full: a haskell sync's ghc (2.7 GB) and then update-json-main
# (3.5 GB and climbing) exhausted RAM + swap. 6144 MB covers one such sync's
# peak with a margin. Waits up to 1 h, re-checking every minute.
# HA_MEM_CHECK_SKIP=1 bypasses it for a deliberate human override.
MEM_CHECK_LIB="$(git rev-parse --show-toplevel 2>/dev/null)/../../external/agents/bin/lib-mem-check.sh"
if [ -r "$MEM_CHECK_LIB" ]; then
  # shellcheck disable=SC1090
  . "$MEM_CHECK_LIB"
  if [ "$LANE" = B ]; then mem_needed="${HYDRA_STACK_SLOT_MEM_B:-9216}"; else mem_needed=6144; fi
  if ! ha_wait_mem_headroom "$mem_needed" 3600 60; then
    exit 1
  fi
else
  echo "with-stack-slot: hydra-agents mem-check lib not found at $MEM_CHECK_LIB; skipping the guard (not fatal)." >&2
fi

# Run the wrapped command while holding fd 9. Children inherit fd 9, so the lock
# stays held until the entire tree exits. Capture status to return it verbatim.
# Export the reentrancy marker so nested wrappers pass through instead of deadlocking.
export HYDRA_STACK_SLOT_HELD="$HOLDER_DESC"

# ---- lane B: own STACK_ROOT + runtime memory guard -------------------------
GUARD_PID=""
if [ "$LANE" = B ]; then
  export STACK_ROOT="$STACK_ROOT_B"
  if [ -r /proc/meminfo ]; then
    PAUSE_MB="${HYDRA_STACK_SLOT_PAUSE_MB:-2560}"
    RESUME_MB="${HYDRA_STACK_SLOT_RESUME_MB:-5120}"
    KILL_MB="${HYDRA_STACK_SLOT_KILL_MB:-1024}"
    # descendants <root> <skip>: all PIDs under <root>, excluding the <skip> subtree.
    descendants() {
      ps -eo pid=,ppid= | awk -v r="$1" -v s="$2" '
        { par[$1] = $2; pids[n++] = $1 }
        END {
          changed = 1; in_[r] = 1
          while (changed) {
            changed = 0
            for (i = 0; i < n; i++) {
              p = pids[i]
              if (!(p in in_) && (par[p] in in_) && p != s) { in_[p] = 1; changed = 1 }
            }
          }
          for (p in in_) if (p != r) print p
        }'
    }
    (
      me="$BASHPID"; paused=0
      log() { echo "with-stack-slot[lane B guard] $(date -u +%H:%M:%S): $*" >&2; }
      while kill -0 "$$" 2>/dev/null; do
        avail=$(awk '/^MemAvailable:/{print int($2/1024)}' /proc/meminfo)
        if [ "$paused" = 0 ] && [ "$avail" -lt "$PAUSE_MB" ]; then
          log "only ${avail} MB available (< ${PAUSE_MB}); pausing this lane-B build"
          for p in $(descendants "$$" "$me"); do kill -STOP "$p" 2>/dev/null; done
          paused=1
        elif [ "$paused" = 1 ] && [ "$avail" -lt "$KILL_MB" ]; then
          log "only ${avail} MB available while paused (< ${KILL_MB}); killing this lane-B build to protect the machine"
          for p in $(descendants "$$" "$me"); do kill -TERM "$p" 2>/dev/null; kill -CONT "$p" 2>/dev/null; done
          sleep 10
          for p in $(descendants "$$" "$me"); do kill -KILL "$p" 2>/dev/null; done
          exit 0
        elif [ "$paused" = 1 ] && [ "$avail" -ge "$RESUME_MB" ]; then
          log "${avail} MB available again; resuming"
          for p in $(descendants "$$" "$me"); do kill -CONT "$p" 2>/dev/null; done
          paused=0
        fi
        sleep 10
      done
    ) &
    GUARD_PID=$!
  fi
fi

set +e
"$@"
status=$?
set -e
[ -n "$GUARD_PID" ] && kill "$GUARD_PID" 2>/dev/null

exit "$status"
