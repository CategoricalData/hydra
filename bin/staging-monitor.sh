#!/usr/bin/env bash
# staging-monitor.sh — event source for the staging coordinator loop.
# Emits one line per actionable event: new inbox message, origin/main advance,
# and a periodic FLEET-AUDIT idle-capacity nudge (the phase-2 forcing function).
# Each stdout line becomes a chat notification. Runs until stopped.
set -uo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.." || exit 1
inbox="claude-hydra-messages/inbox"

# Seed baselines so we only report NEW state, not everything already present.
seen_main="$(git rev-parse origin/main 2>/dev/null || echo none)"
# Snapshot current inbox filenames.
ls "$inbox"/*.md 2>/dev/null | sort > /tmp/staging-mon-inbox-seen.txt || : > /tmp/staging-mon-inbox-seen.txt

# FLEET-AUDIT cadence: emit every 10th 60s loop (~10 min). Start the counter at 9
# so the first audit fires one tick in, not the instant the monitor starts.
audit_period=10
audit_ctr=9

# De-dup state for fleet_audit: only re-emit when (idle,ahead) changes, with a
# keep-alive every audit_quiet_heartbeat suppressed audits so a persistent idle
# still resurfaces periodically instead of pinging identically every ~10 min.
last_audit_key=""
audit_quiet_ctr=0

# De-dup state for fleet_shepherd (per-worker neglect audit). Keyed by the set of
# stuck-worker names so it re-fires only when the stuck set CHANGES, with a
# keep-alive every shepherd_quiet_heartbeat suppressed audits.
last_shepherd_key=""
shepherd_quiet_ctr=0
shepherd_quiet_heartbeat=4
# A live worker session idle-at-prompt longer than this (seconds) with unlanded
# work is a SHEPHERD-DUE event: it is holding for the coordinator and starving.
shepherd_idle_threshold=5400        # 90 min: neglected long enough to flag
# ...but a worker idle beyond this ceiling is ABANDONED (dead session), not
# neglected — that is fleet-hygiene's cleanup target, not a live shepherding
# obligation. Flag only the [threshold, ceiling] window so the SHEPHERD-DUE list
# stays a small set of ACTIONABLE neglected workers, not a graveyard of dead ones.
# EXCEPTION: P0 has NO ceiling — a release-blocker/kernel-bug that has been stuck
# for weeks is the WORST case, not exempt; it must surface regardless of age.
shepherd_stale_ceiling=432000       # 5 days
audit_quiet_heartbeat=6

# De-dup state for the release-hold heartbeat (see release_hold_active). While a
# hold is declared, fleet_audit stops nagging about idle cores (throttling is the
# WHOLE POINT during a release) and instead emits an occasional HELD heartbeat so
# the deliberate throttle stays visible and can't be silently forgotten.
last_held_key=""
held_quiet_ctr=0
held_quiet_heartbeat=6

# De-dup state for fleet_stalled_design (the #497-class detector). Design-complete
# worktrees change slowly, so de-dup hard and heartbeat rarely.
last_stalled_key=""
stalled_quiet_ctr=0
stalled_quiet_heartbeat=6

# De-dup state for fleet_attention (the #416-class detector: a worker blocked on a
# permission/idle prompt). The notification hook already drops a marker in
# ~/.cache/claude-attention/<wt>.txt; this audit is the missing coordinator half.
last_attention_key=""
attention_quiet_ctr=0
attention_quiet_heartbeat=4

# release_hold_active — echo the hold REASON if the coordinator has declared a
# deliberate machine-throttle (release gate in flight), else echo nothing.
# Release-gate throttling is the ONLY legitimate reason to sit idle capacity: any
# other idle is a dispatch failure. The coordinator declares a hold by writing
# claude-hydra-messages/release-hold (first line = reason, e.g. "0.17.6 gate: #702
# CI + bootstrap"); `rm` it the moment both gates resolve. Gitignored, like the
# rest of claude-hydra-messages/.
#
# HARD INVARIANT (learned the hard way, 2026-08-26): a release hold is only VALID
# when ZERO P0 release-blockers are open. An open P0 means we are NOT in the
# pre-release stage — release prep should never have begun. So a hold declared
# while a P0 is open is a CONTRADICTION, not a throttle: it must SHOUT (see the
# release_hold_invalid checks in fleet_audit / fleet_shepherd), never go quiet.
# This helper only reports the declared reason; validity is enforced at the call
# sites, which cross-check count_open_p0.
release_hold_active() {
  local f="claude-hydra-messages/release-hold"
  [ -r "$f" ] || return 1
  head -1 "$f" 2>/dev/null | cut -c1-100
  return 0
}

# count_open_p0 — count live worker worktrees that are (a) ahead of origin/main
# (unlanded work) and (b) marked P0 in claude-hydra-messages/priority/<base>. A
# non-zero count means a release-blocker is OPEN, which makes any release hold
# invalid. Cheap: one rev-list + one priority-file read per sibling worktree.
count_open_p0() {
  local c=0 wt base n prio pf
  for wt in ../*/; do
    [ -e "$wt/.git" ] || continue
    base="$(basename "$wt")"
    case "$base" in task_*|bug_*|feature_*) : ;; *) continue ;; esac
    n="$(git -C "$wt" rev-list --count origin/main..HEAD 2>/dev/null || echo 0)"
    [ "${n:-0}" -gt 0 ] 2>/dev/null || continue
    pf="claude-hydra-messages/priority/${base}"
    [ -r "$pf" ] || continue
    prio="$(head -1 "$pf" 2>/dev/null | tr -d ' ' | cut -c1-2)"
    [ "$prio" = "P0" ] && c=$((c + 1))
  done
  echo "$c"
}

# fleet_audit — emit a single FLEET-AUDIT line ONLY when there is idle capacity
# worth acting on. The CPU/disk half is designated-build-machine-only (gated on
# ~/.hydra-sandbox; see claude/sandbox-permissions.md); the landable-worktree
# sweep runs everywhere. Fully defensive: any probe failure degrades to a quiet
# tick rather than killing the loop.
fleet_audit() {
  # Ready-to-land count: sibling worktrees ahead of origin/main AND recently active.
  # Only count worktrees whose HEAD commit is within the last 7 days — a worktree
  # ahead-of-main but untouched for weeks is a stale/dead branch, not landable work,
  # and counting it inflates the audit into crying wolf every tick. Cheap — one
  # rev-list + one committer-date read per worktree.
  local ahead=0 wt n age now cutoff
  now="$(date +%s 2>/dev/null || echo 0)"
  cutoff=$((now - 7*24*3600))
  for wt in ../*/; do
    [ -e "$wt/.git" ] || continue
    n="$(git -C "$wt" rev-list --count origin/main..HEAD 2>/dev/null || echo 0)"
    [ "${n:-0}" -gt 0 ] 2>/dev/null || continue
    # skip stale: HEAD commit older than the cutoff
    age="$(git -C "$wt" log -1 --format=%ct 2>/dev/null || echo 0)"
    [ "${age:-0}" -ge "$cutoff" ] 2>/dev/null && ahead=$((ahead + 1))
  done

  if [ -f "$HOME/.hydra-sandbox" ]; then
    # Designated build machine: also surface idle cores + disk headroom.
    local cores load idle disk
    cores="$(nproc 2>/dev/null || echo 0)"
    # 1-min load average; integer part is enough for an idle-core estimate.
    load="$(cut -d' ' -f1 /proc/loadavg 2>/dev/null || echo 0)"
    load="${load%.*}"; load="${load:-0}"
    idle=$((cores - load))
    disk="$(df -h / 2>/dev/null | awk 'NR==2 {print $4}' || echo '?')"
    # Only actionable when >= 2 cores are genuinely idle — else stay quiet.
    if [ "${idle:-0}" -ge 2 ] 2>/dev/null; then
      # Release-hold gate: if the coordinator has DECLARED a deliberate throttle
      # (release gate in flight), idle cores are EXPECTED, not a dispatch failure.
      # Suppress the "dispatch or justify idle" nag entirely — the hold IS the
      # justification — but emit a deduped HELD heartbeat so the throttle stays
      # visible (keep-alive every held_quiet_heartbeat ticks). This is the ONLY
      # circumstance in which sitting idle capacity is correct; everywhere else the
      # nag below must fire so idle cores convert to high-priority ready work.
      local hold_reason; hold_reason="$(release_hold_active)"
      if [ -n "$hold_reason" ]; then
        # INVARIANT CHECK: a hold is only valid with ZERO open P0 blockers. If a P0
        # is open, release prep should never have started — the hold is a
        # contradiction. SHOUT every tick (no de-dup, no quiet) until it's resolved.
        local openp0; openp0="$(count_open_p0)"
        if [ "${openp0:-0}" -gt 0 ] 2>/dev/null; then
          echo "FLEET-AUDIT: *** INVALID RELEASE HOLD *** '${hold_reason}' declared while ${openp0} P0 release-blocker(s) OPEN. A P0 open means we are NOT in pre-release — release prep should not have begun. CLEAR THE P0 (land/close it) or DROP THE HOLD, then dispatch the $idle idle cores. Do NOT sit idle behind an invalid hold."
          return 0
        fi
        local hkey="held:${ahead}"
        if [ "$hkey" != "${last_held_key:-}" ] || [ "${held_quiet_ctr:-0}" -ge "${held_quiet_heartbeat:-6}" ]; then
          echo "FLEET-AUDIT: HELD (valid release throttle, 0 P0 open) — $idle idle cores intentionally idle: ${hold_reason}. Idle is CORRECT until the gate clears; ${ahead} worktrees ahead-of-main wait."
          last_held_key="$hkey"; held_quiet_ctr=0
        else
          held_quiet_ctr=$((${held_quiet_ctr:-0} + 1))
        fi
        return 0
      fi
      # DON'T cry wolf: during a long CI wait the (idle, ahead) state is identical
      # tick after tick, which trains the coordinator to rubber-stamp "gated, idle".
      # Emit only when the state CHANGES from the last emission, or once every
      # audit_quiet_heartbeat audits as a keep-alive so a persistent idle still
      # resurfaces (just not every ~10 min). State key = COARSE idle bucket + ahead.
      # The exact idle count is NOT actionable (dispatch is a discrete decision, not
      # proportional to core count), and load-average jitter from the coordinator's
      # own brief bursts oscillates the count by +/-1 CONSTANTLY. Earlier 2-3/4-7/8+
      # buckets put a boundary at 8, right where idle naturally flaps (seen 7<->8
      # re-firing every tick). Collapse to a single "capacity available" bucket with
      # a wide hysteresis gap: once >=2 cores are idle it's "idle", and only a change
      # in the ahead-count (genuinely new landable work) or the heartbeat re-fires.
      # The DISPLAYED number stays exact; only the KEY is coarse.
      local ibucket="idle"   # single bucket: any actionable idle (>=2) is the same state
      local key="${ibucket}:${ahead}"
      if [ "$key" != "${last_audit_key:-}" ] || [ "${audit_quiet_ctr:-0}" -ge "${audit_quiet_heartbeat:-6}" ]; then
        # No release hold is active (checked above), so idle cores are a DISPATCH
        # FAILURE, not a benign wait. Point the coordinator at the closed loop:
        # idle capacity should be pulling the HIGHEST-PRIORITY ready work off the
        # SHEPHERD-DUE list (P0/P1 first). Only a declared release hold justifies
        # idle — and none is declared here.
        echo "FLEET-AUDIT: $idle idle cores, disk $disk free, $ahead worktrees ahead-of-main — NO release hold declared, so idle = dispatch gap. Pull the top SHEPHERD-DUE item (P0/P1 first) onto a free core, or declare a hold (claude-hydra-messages/release-hold) if throttling for a release."
        last_audit_key="$key"; audit_quiet_ctr=0
      else
        audit_quiet_ctr=$((${audit_quiet_ctr:-0} + 1))
      fi
    fi
  else
    # Non-designated machine: no CPU/util part; nudge only if there is landable work,
    # and only when the landable count CHANGES (same de-dup rationale as above).
    if [ "$ahead" -gt 0 ] && [ "$ahead" != "${last_audit_key:-}" ]; then
      echo "FLEET-AUDIT: $ahead worktrees ahead-of-main (landable?) — land ready work or justify idle."
      last_audit_key="$ahead"
    fi
  fi
}

# fleet_hygiene — detect HEAVY build processes (>15% CPU) whose owning worktree's
# branch is ALREADY landed on origin/main (its commits are ancestors of main), or
# whose worktree is a subagent worktree (agent-*). Those are CANDIDATE ORPHANS —
# builds that outlived their work and keep pegging cores while masking the machine's
# true state. Detection ONLY: we never kill anything (a landed worktree may have a
# legitimately-running follow-up; the agent decides). Sandbox-gated + fully
# defensive: any probe failure degrades to a quiet tick, never crashes the loop.
fleet_hygiene() {
  [ -f "$HOME/.hydra-sandbox" ] || return 0

  local pid pcpu comm cwd wt br n=0 orphans=""
  # Top CPU consumers whose runtime is a heavy build tool; ignore pcpu<=15.
  while read -r pid pcpu comm; do
    [ -n "${pid:-}" ] || continue
    case "$comm" in
      ghc|ghc-*|java|python|python3|node|stack) : ;;
      *) continue ;;
    esac
    # pcpu is a float (e.g. 42.3); compare on the integer part.
    awk -v c="${pcpu%.*}" 'BEGIN{exit !(c+0 > 15)}' 2>/dev/null || continue

    # Attribute the process to a worktree by walking /proc/<pid>/cwd upward a few
    # levels until we hit a dir with a .git entry (a worktree root).
    cwd="$(readlink -f "/proc/$pid/cwd" 2>/dev/null || echo '')"
    [ -n "$cwd" ] || continue
    wt=""
    local probe="$cwd" i=0
    while [ "$i" -lt 6 ] && [ "$probe" != "/" ] && [ -n "$probe" ]; do
      if [ -e "$probe/.git" ]; then wt="$probe"; break; fi
      probe="$(dirname "$probe" 2>/dev/null || echo /)"
      i=$((i + 1))
    done
    [ -n "$wt" ] || continue

    # Candidate orphan if: worktree name matches agent-* (subagent worktree), OR
    # its HEAD is already an ancestor of origin/main (its work has landed).
    local base; base="$(basename "$wt" 2>/dev/null || echo '')"
    # EXCEPTION: staging's own gate + diagnostic worktrees (bootstrap-clean-*,
    # batch-land-*, *-gate-*, verify*) are deliberately at-main / detached AND run a
    # legitimate long build (bootstrap / land verification / a clean-room repro like
    # verify702-clean). A running build there is the INTENDED state, not an orphan —
    # the "HEAD ancestor of main" heuristic false-positives on them. Skip by name.
    # (verify* added 2026-08-26 after FLEET-HYGIENE flagged my own live #702 repro.)
    # (post-release-*-bump added 2026-09-01: coordinator-driven post-release version
    # bump + shim-retirement build; HEAD==main-based, no worker session, live sync — the
    # exact intended-build-in-a-non-worker-worktree false positive.)
    # (wt-* added 2026-09-03: coordinator scratchpad fix worktrees, e.g. wt-decimal-fix
    # running a full regen for a fix-forward. HEAD==main-based (fix not yet committed) +
    # the regen is a detached bash/ghc, not a claude session, so the LIVE-SESSION GUARD
    # below misses it — same intended-build-in-a-non-worker-worktree false positive.)
    case "$base" in bootstrap-clean-*|batch-land-*|*-gate-*|verify*|post-release-*-bump|wt-*) continue ;; esac
    # LIVE-SESSION GUARD: never flag a worktree that has a live `claude` session cwd'd
    # under it — that's an ACTIVELY ENGAGED worker (e.g. task_702 mid-fix, HEAD==main
    # because it hasn't committed yet), not an orphan. The old check said "check no
    # live session" in its message but never actually tested it. (Added 2026-08-27
    # after it flagged the just-engaged task_702 #702-fix worker.)
    local has_session=""
    for cp in $(pgrep -x claude 2>/dev/null); do
      local ccwd; ccwd="$(readlink -f "/proc/$cp/cwd" 2>/dev/null || echo '')"
      case "$ccwd" in "$wt"|"$wt"/*) has_session=1; break ;; esac
    done
    [ -n "$has_session" ] && continue
    if [ "${base#agent-}" != "$base" ]; then
      orphans="${orphans:+$orphans,}$base"; n=$((n + 1)); continue
    fi
    if git -C "$wt" merge-base --is-ancestor HEAD origin/main 2>/dev/null; then
      orphans="${orphans:+$orphans,}$base"; n=$((n + 1))
    fi
  done < <(ps -eo pid,pcpu,comm --sort=-pcpu 2>/dev/null || true)

  [ "$n" -ge 1 ] 2>/dev/null && \
    echo "FLEET-HYGIENE: $n heavy build(s) in landed/agent worktrees (candidate orphans: $orphans) — reap if truly done (check no live session)."
  return 0
}

# fleet_shepherd — the WORKER-NEGLECT audit (companion to fleet_audit's machine
# audit). fleet_audit answers "is the MACHINE busy / is there landable work" — it
# stays quiet while a busy critical path runs, so a worker parked idle-at-prompt
# for hours waiting on the coordinator STARVES INVISIBLY (this is exactly how #702
# sat for 2+ days: the un-red kept cores busy, every machine-forcing-function was
# satisfied, and nothing watched the workers). This audit instead names each
# STUCK WORKER: a live claude session cwd'd in a task_/bug_/feature_ worktree that
# is idle-at-prompt (low CPU) with unlanded work (ahead of origin/main) — i.e. it
# is holding for a coordinator instruction that never came. Naming a specific
# worker ("task_702 idle 7h") is undismissable in a way an aggregate count
# ("24 ahead") is not. De-duped on the stuck-set so it doesn't cry wolf, with a
# keep-alive so a persistent stuck worker resurfaces. Fully defensive.
fleet_shepherd() {
  local stuck="" stuckkey="" nstuck=0 pid cwd wt base br cpu ahead lastc now nowc idle_s subj
  local np0=0 np1=0 nabandoned=0 nheld_p2=0   # per-run counters (reset each call, not global)
  now="$(date +%s 2>/dev/null || echo 0)"
  # Release-hold awareness: during a DECLARED release throttle, P2 workers holding
  # is EXPECTED (the coordinator is deliberately not dispatching), so suppress the
  # P2 tail to keep the list actionable. But P0/P1 ALWAYS surface — a starving
  # kernel bug or in-cycle task must never be silenced by a release hold (that is
  # the exact #702 failure this whole audit exists to prevent).
  local hold_reason; hold_reason="$(release_hold_active)"
  # Enumerate live claude worker processes (interactive sessions), map each to its
  # worktree, and flag the neglected ones.
  for pid in $(pgrep -x claude 2>/dev/null); do
    cwd="$(readlink -f "/proc/$pid/cwd" 2>/dev/null || echo '')"
    [ -n "$cwd" ] || continue
    # Only worker worktrees (task_/bug_/feature_), not the coordinator seat or wiki.
    case "$cwd" in
      */worktrees/task_*|*/worktrees/bug_*|*/worktrees/feature_*) : ;;
      *) continue ;;
    esac
    # Walk to the worktree root (dir with a .git entry).
    wt="$cwd"; local i=0
    while [ "$i" -lt 6 ] && [ "$wt" != "/" ] && [ -n "$wt" ]; do
      [ -e "$wt/.git" ] && break
      wt="$(dirname "$wt" 2>/dev/null || echo /)"; i=$((i + 1))
    done
    [ -e "$wt/.git" ] || continue
    base="$(basename "$wt" 2>/dev/null || echo '')"
    # DO-NOT-SHEPHERD exclusions (Josh-directed holds): these sessions are meant to
    # be idle and must not be flagged/woken.
    case "$base" in
      task_674*|task_410*|task_701*|task_685*) continue ;;   # Fable / held / freed
    esac
    # 0.18 work (children of #509) is exempt from shepherding pressure.
    # (Cheap heuristic: skip the known 0.18 design worktrees by name if needed.)

    # Idle check: low CPU = not actively working. Sample once (cheap); the pane
    # holding at a prompt is the real signal but ps is the fast gate.
    cpu="$(ps -o pcpu= -p "$pid" 2>/dev/null | tr -d ' ')"; cpu="${cpu%.*}"; cpu="${cpu:-0}"
    awk -v c="$cpu" 'BEGIN{exit !(c+0 <= 3)}' 2>/dev/null || continue   # >3% CPU = working, skip

    # Unlanded work: ahead of origin/main (nothing to shepherd if fully landed).
    ahead="$(git -C "$wt" rev-list --count origin/main..HEAD 2>/dev/null || echo 0)"
    [ "${ahead:-0}" -gt 0 ] 2>/dev/null || continue

    # How long idle? Use the claude process's own last-active proxy: the mtime of
    # /proc/<pid>/stat is unreliable, so approximate with elapsed-since-last-CPU via
    # the process start is wrong; instead use the worktree HEAD age as a floor AND
    # accept that a low-CPU claude in an ahead worktree is "holding". Report HEAD age.
    lastc="$(git -C "$wt" log -1 --format=%ct 2>/dev/null || echo 0)"
    idle_s=$(( now - lastc ))
    # Only flag if HEAD (last commit) is older than the idle threshold — a worker
    # that committed recently is mid-work, not parked.
    [ "$idle_s" -ge "$shepherd_idle_threshold" ] 2>/dev/null || continue

    subj="$(git -C "$wt" log -1 --format=%s 2>/dev/null | cut -c1-48)"
    # PRIORITY (data-driven, not hard-coded logic): read a per-worktree priority
    # from claude-hydra-messages/priority/<base> (a file whose first line is P0/P1/
    # P2/P3), else default P2. P0 = release-blocker / kernel bug (must not starve);
    # P1 = in-cycle; P2 = normal; P3 = background. This is the missing "priority"
    # dimension — a high-priority worker idle must SHOUT, not blend into N names.
    local prio pf; prio="P2"
    pf="claude-hydra-messages/priority/${base}"
    [ -r "$pf" ] && { prio="$(head -1 "$pf" 2>/dev/null | tr -d ' ' | cut -c1-2)"; case "$prio" in P0|P1|P2|P3) : ;; *) prio="P2";; esac; }
    # Staleness ceiling: a worker idle beyond the ceiling is ABANDONED (dead
    # session) → fleet-hygiene's cleanup target, not a live shepherding obligation.
    # Skip it here to keep the SHEPHERD-DUE list a small ACTIONABLE set. EXCEPTION:
    # P0 has NO ceiling — a release-blocker stuck for weeks is the worst case.
    if [ "$prio" != "P0" ] && [ "$idle_s" -ge "$shepherd_stale_ceiling" ] 2>/dev/null; then
      nabandoned=$((${nabandoned:-0} + 1)); continue
    fi
    # During a declared release hold, suppress the P2/P3 tail (holding is expected)
    # but keep P0/P1 (must never be silenced by a hold). Counted, not silent, so the
    # emission line can say "+N P2 held" — the throttle is visible, not hidden.
    if [ -n "$hold_reason" ]; then
      case "$prio" in P2|P3) nheld_p2=$((nheld_p2 + 1)); continue ;; esac
    fi
    # Prefix with priority so a sort surfaces P0 first; keep idle-hours for ranking.
    stuck="${stuck:+$stuck
}${prio} ${base} (idle ~$(( idle_s / 3600 ))h, ${ahead} unlanded, last: ${subj})"
    # STABLE de-dup key fragment: priority + base ONLY — NOT idle-hours (which
    # increments every hour and would defeat de-dup, re-firing the identical list
    # every tick as the ~Nh value ticks up) and NOT ahead-count/subj (cosmetic).
    # The DISPLAYED line above keeps idle-hours for ranking; the KEY stays stable so
    # an unchanged stuck-SET stays quiet between heartbeats.
    stuckkey="${stuckkey:+$stuckkey,}${prio}:${base}"
    nstuck=$((nstuck + 1))
    case "$prio" in P0) np0=$((${np0:-0} + 1));; P1) np1=$((${np1:-0} + 1));; esac
  done

  # Nothing actionable surfaced. If a release hold suppressed only-P2 workers, stay
  # SILENT (the FLEET-AUDIT HELD heartbeat already reports the throttle) — don't
  # double-report. Reset de-dup so the next real P0/P1 fires immediately.
  [ "$nstuck" -ge 1 ] 2>/dev/null || { last_shepherd_key=""; return 0; }

  # Sort stuck workers by priority (P0 first), then present. A P0 idle worker is
  # the loudest possible line — it is a release-blocker/kernel bug being starved.
  local sorted; sorted="$(printf '%s\n' "$stuck" | sort)"
  local hi=""; [ "${np0:-0}" -gt 0 ] && hi=" *** ${np0} P0 (release-blocker/kernel-bug) STARVING ***"
  [ "${np1:-0}" -gt 0 ] && hi="${hi} [${np1} P1]"
  # During a hold, note how many P2/P3 were suppressed so the throttle is visible,
  # not hidden — "still holding N P2 (release throttle)".
  local heldnote=""
  [ -n "$hold_reason" ] && [ "${nheld_p2:-0}" -gt 0 ] && heldnote=" (+${nheld_p2} P2 held: release throttle)"

  # De-dup: re-emit only when the stuck-set changes, or every heartbeat. P0-present
  # SHORTENS the effective de-dup (always re-fire P0 every tick — a starving kernel
  # bug must not be silenced by de-dup).
  # Key on the STABLE fragment (priority:base per worker, sorted), NOT the displayed
  # text — so the identical stuck-set doesn't re-fire every tick as idle-hours tick up.
  local key; key="$nstuck:$(printf '%s\n' "$stuckkey" | tr ',' '\n' | sort | tr '\n' ',')"
  if [ "${np0:-0}" -gt 0 ] || [ "$key" != "${last_shepherd_key:-}" ] || [ "${shepherd_quiet_ctr:-0}" -ge "${shepherd_quiet_heartbeat:-4}" ]; then
    printf 'SHEPHERD-DUE%s%s — %s worker(s) idle-at-prompt with unlanded work, holding for coordinator. HIGHEST-PRIORITY FIRST:\n%s\nEngage the top item now (send its next action to inbox+tmux) or reassign; NEVER let a P0/P1 worker starve while the critical path runs.\n' "$hi" "$heldnote" "$nstuck" "$sorted"
    last_shepherd_key="$key"; shepherd_quiet_ctr=0
  else
    shepherd_quiet_ctr=$((shepherd_quiet_ctr + 1))
  fi
  return 0
}

# fleet_stalled_design — the #497-class detector (added 2026-09-01 after feature_497
# sat idle for THREE release cycles, invisible to every other audit).
#
# The gap it closes: fleet_shepherd only walks LIVE claude sessions with commits
# AHEAD of main. A worktree whose design is complete, whose implementation-authorizing
# work already LANDED (so it is AT main, 0 ahead), and which has NO live session —
# because nobody re-launched it to start implementation — is invisible to BOTH the
# machine audit (it uses no cores) and the worker audit (no session, 0 ahead). And
# every `git merge origin/main --ff` resets its HEAD-age staleness clock, so even a
# time heuristic reads it as "recently active." That is EXACTLY how #497 parked
# "awaiting go-ahead to implement" and drained through 0.17.2→0.17.6 untouched.
#
# This audit instead walks ALL feature_* worktrees ON DISK (not just live sessions),
# and flags any that is: (a) at or behind origin/main (0 commits ahead — its design
# landed or never diverged), (b) has NO live claude session cwd'd under it, AND (c)
# carries a durable design-complete marker its plan doc opts into: a line matching
# `AWAITING-IMPLEMENT` (case-insensitive) anywhere in <worktree>/<branch>-plan.md.
# The marker is REQUIRED — the monitor cannot infer "planning-mode complete" from
# git state, so a worker/coordinator must write it when design finishes. No marker =
# not flagged (avoids nagging about genuinely-not-started or abandoned branches).
# Fully defensive; any probe failure degrades to a quiet tick.
fleet_stalled_design() {
  local stalled="" stalledkey="" n=0 wt base plan ahead behind has_session cp ccwd
  while read -r wt _; do
    [ -n "$wt" ] || continue
    base="$(basename "$wt" 2>/dev/null || echo '')"
    case "$base" in feature_*) : ;; *) continue ;; esac
    # (c) design-complete marker in the plan doc — REQUIRED to flag.
    plan="$wt/${base}-plan.md"
    [ -r "$plan" ] || continue
    grep -qi 'AWAITING-IMPLEMENT' "$plan" 2>/dev/null || continue
    # (a) at/behind main: 0 commits ahead (design landed or never diverged).
    ahead="$(git -C "$wt" rev-list --count origin/main..HEAD 2>/dev/null || echo 0)"
    [ "${ahead:-0}" -eq 0 ] 2>/dev/null || continue
    # (b) no live claude session cwd'd under this worktree.
    has_session=""
    for cp in $(pgrep -x claude 2>/dev/null); do
      ccwd="$(readlink -f "/proc/$cp/cwd" 2>/dev/null || echo '')"
      case "$ccwd" in "$wt"|"$wt"/*) has_session=1; break ;; esac
    done
    [ -n "$has_session" ] && continue
    behind="$(git -C "$wt" rev-list --count HEAD..origin/main 2>/dev/null || echo 0)"
    stalled="${stalled:+$stalled
}${base} (design-complete, no session, ${behind} behind main — awaiting implement authorization)"
    stalledkey="${stalledkey:+$stalledkey,}${base}"
    n=$((n + 1))
  done < <(git worktree list 2>/dev/null || true)

  [ "$n" -ge 1 ] 2>/dev/null || { last_stalled_key=""; return 0; }

  local key; key="$n:$(printf '%s\n' "$stalledkey" | tr ',' '\n' | sort | tr '\n' ',')"
  if [ "$key" != "${last_stalled_key:-}" ] || [ "${stalled_quiet_ctr:-0}" -ge "${stalled_quiet_heartbeat:-6}" ]; then
    printf 'STALLED-DESIGN: %s design-complete worktree(s) with NO live session, awaiting implement-authorization (the #497 class — a coordinator obligation, NOT idle-by-throttle):\n%s\nFor each: authorize implementation (relaunch + go), schedule it, or record an explicit deferral-with-reason. A design-complete issue with no coordinator decision across a release cycle is a coordinator failure.\n' "$n" "$(printf '%s\n' "$stalled" | sort)"
    last_stalled_key="$key"; stalled_quiet_ctr=0
  else
    stalled_quiet_ctr=$((stalled_quiet_ctr + 1))
  fi
  return 0
}

# fleet_attention — the #416-class detector: a worker BLOCKED on a permission or
# idle prompt (added 2026-09-01 after feature_416 sat blocked on a permission
# prompt for hours, invisible to every audit).
#
# The gap it closes: a worker blocked on an interactive prompt is invisible to
# fleet_shepherd — it sits at low CPU (looks like normal holding), it is often 0
# commits ahead (in-tree WIP), and NOTHING inspected the pane or the notification
# signal. Even the sandbox bypass (--dangerously-skip-permissions) does not suppress
# every prompt (idle "waiting for input", trust prompts, etc.), so a prompt CAN
# still block on a sandbox.
#
# The signal ALREADY EXISTS: bin/claude-hooks/notification-hook.sh drops a marker at
# ~/.cache/claude-attention/<worktree>.txt (leading `type:` line = idle_prompt /
# permission / etc.) whenever a session's Notification fires, and stop-hook.sh clears
# it when the session resumes. The spawn briefing even PROMISED "the coordinator
# monitors ~/.cache/claude-attention/" — but the coordinator half was never wired up.
# THIS is that half: read the markers and surface a recent, uncleared one as a
# BLOCKED-ON-PROMPT event naming the worktree + wait type + how long. Recency-gated
# (only markers touched within the window — a live wait, not a stale leftover the
# stop-hook missed) and skips `*-completed.txt`. Fully defensive.
fleet_attention() {
  local dir="$HOME/.cache/claude-attention" f base typ ts_line age_min mt now blocked="" blockedkey="" n=0
  [ -d "$dir" ] || return 0
  now="$(date +%s 2>/dev/null || echo 0)"
  # Only markers modified within the last ~20 min = an ACTIVE wait (the hook rewrites
  # the marker each Notification; a much older mtime is a wait the stop-hook likely
  # already cleared or a dead session — fleet_hygiene / stalled-design territory).
  for f in "$dir"/*.txt; do
    [ -e "$f" ] || continue
    base="$(basename "$f" .txt 2>/dev/null || echo '')"
    case "$base" in *-completed) continue ;; esac
    # WATCHDOG GUARD (2026-09-03): the resource watchdog drops watchdog-alpha-*.txt
    # attention markers (memory-pressure / liveness) that are NOT worker idle_prompts —
    # they can't be "unblocked" by an inbox nudge. The coordinator handles memory/liveness
    # via its own free-mem check (act only <5Gi avail) + owned-build liveness. Skip them
    # here so a watchdog alert (which re-fires ~every 5 min) doesn't whack-a-mole
    # BLOCKED-ON-PROMPT. (Watchdog's own 'stalled/lapsed' text is a separate false-positive
    # when it can't see an owned CI run or a scratchpad regen — banked for its tuning.)
    case "$base" in watchdog-*) continue ;; esac
    mt="$(stat -c %Y "$f" 2>/dev/null || echo 0)"
    age_min=$(( (now - mt) / 60 ))
    [ "$age_min" -le 20 ] 2>/dev/null || continue
    # FALSE-POSITIVE GUARD: a marker's mtime only updates on a NEW Notification, so a
    # session that was blocked, got a marker, then RESUMED work keeps a stale (<20m)
    # marker until its stop-hook clears it. CPU is unreliable (a session mid-turn
    # waiting on the model API sits at low CPU too). The reliable signal is the pane's
    # turn-state: an ACTIVE turn shows "esc to interrupt". If the worktree's tmux pane
    # is mid-turn, it is WORKING, not blocked — skip. (Caught 2026-09-01 when feature_416
    # showed "Fermenting…/esc to interrupt" but still had a 5-min-old marker.)
    if tmux capture-pane -t "${base}:0.0" -p 2>/dev/null | grep -q 'esc to interrupt'; then
      continue
    fi
    # HELD-FOR-SLOT GUARD: a worker the coordinator has deliberately parked to wait for
    # the serialized ~/.stack build slot is EXPECTED to be idle — it is standing by for a
    # GRANT, not stranded. The coordinator registers it in claude-hydra-messages/slot-queue/
    # <base> when telling it to hold, and removes the file on GRANT. Skip flagging these so
    # a queue of slot-waiting workers doesn't re-fire BLOCKED-ON-PROMPT every tick. (Added
    # 2026-09-01 when the fleet went mostly slot-queued and every holding worker re-alarmed.)
    [ -e "claude-hydra-messages/slot-queue/${base}" ] && continue
    # DONE-WORKER GUARD: a worker whose work has fully LANDED (HEAD is an ancestor of
    # origin/main, i.e. 0 commits ahead) idling is NOT a strand needing a next-action —
    # it is finished and awaiting finalize/reap (fleet_hygiene's job), not shepherding.
    # Skip it here so a completed worker's lingering marker (its stop-hook clears on
    # turn-end, but that can lag) doesn't re-fire BLOCKED-ON-PROMPT forever. (Caught
    # 2026-09-01 when bug_719, fully landed, kept flagging while going quiet.)
    local wtpath="worktrees/${base}"
    if [ -d "../${base}/.git" ] || [ -e "../${base}/.git" ]; then
      if git -C "../${base}" merge-base --is-ancestor HEAD origin/main 2>/dev/null; then
        continue
      fi
    fi
    typ="$(sed -n 's/^type:[[:space:]]*//p' "$f" 2>/dev/null | head -1)"; typ="${typ:-unknown}"
    blocked="${blocked:+$blocked
}${base} (${typ}, waiting ~${age_min}m)"
    blockedkey="${blockedkey:+$blockedkey,}${base}:${typ}"
    n=$((n + 1))
  done

  [ "$n" -ge 1 ] 2>/dev/null || { last_attention_key=""; return 0; }

  local key; key="$n:$(printf '%s\n' "$blockedkey" | tr ',' '\n' | sort | tr '\n' ',')"
  if [ "$key" != "${last_attention_key:-}" ] || [ "${attention_quiet_ctr:-0}" -ge "${attention_quiet_heartbeat:-4}" ]; then
    printf 'BLOCKED-ON-PROMPT: %s worker session(s) waiting on a permission/idle prompt (the #416 class — invisible to CPU/ahead heuristics; from ~/.cache/claude-attention/ notification markers):\n%s\nFor each: read the marker (tail ~/.cache/claude-attention/<wt>.txt), then unblock — send the answer/next-action to its inbox + tmux-nudge (idle_prompt = it needs its next instruction; a permission type = a gated action even sandbox bypass did not cover, may need Josh).\n' "$n" "$(printf '%s\n' "$blocked" | sort)"
    last_attention_key="$key"; attention_quiet_ctr=0
  else
    attention_quiet_ctr=$((attention_quiet_ctr + 1))
  fi
  return 0
}

while true; do
  git fetch origin main -q 2>/dev/null || true
  cur_main="$(git rev-parse origin/main 2>/dev/null || echo none)"
  [ "$cur_main" != "$seen_main" ] && { echo "MAIN-ADVANCED: origin/main → $cur_main"; seen_main="$cur_main"; }

  ls "$inbox"/*.md 2>/dev/null | sort > /tmp/staging-mon-inbox-now.txt || : > /tmp/staging-mon-inbox-now.txt
  comm -13 /tmp/staging-mon-inbox-seen.txt /tmp/staging-mon-inbox-now.txt | sed 's|.*/|INBOX-NEW: |'
  cp /tmp/staging-mon-inbox-now.txt /tmp/staging-mon-inbox-seen.txt

  audit_ctr=$((audit_ctr + 1))
  if [ "$audit_ctr" -ge "$audit_period" ]; then
    audit_ctr=0
    fleet_audit || true
    fleet_hygiene || true
    fleet_shepherd || true
    fleet_stalled_design || true
  fi
  # fleet_attention runs EVERY tick (not just the periodic audit): a worker blocked
  # on a prompt is time-sensitive (it makes zero progress until unblocked), and its
  # own de-dup keeps an unchanged blocked-set quiet, so per-tick polling is cheap.
  fleet_attention || true

  sleep 60
done
