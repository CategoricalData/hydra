#!/usr/bin/env bash
#
# pre-push-bootstrap-gate.sh — mechanical land gate for origin/main.
#
# Refuses any push to refs/heads/main unless a passing /bootstrap artifact
# exists whose validated TREE matches the tree of the commit being pushed.
# This enforces the staging charter (claude/branch-flow.md §Cadence: "Push to
# origin/main once /sync, /test, and /bootstrap are all green") as a machine
# check that cannot be rationalized past, rather than prose an agent must
# remember to honor.
#
# SCOPE — binds the AGENT, not the human. The staging agent lands EXCLUSIVELY
# from its staging worktree (name varies per machine — see
# GATED_WORKTREE_PATTERN below), so the gate enforces ONLY when the push
# originates there. Pushes from every OTHER worktree pass through untouched,
# so the user can manually push to main from any of them with zero friction.
# (If the user ever pushes from the staging worktree itself, the logged
# bypass below lets them through: HYDRA_BOOTSTRAP_GATE_BYPASS="<reason>".)
#
# Installed as .git/hooks/pre-push via bin/install-git-hooks.sh. The shared
# bare repo means the hook fires for every worktree's pushes — but it self-
# limits to (a) pushes to main AND (b) origin == the staging worktree.
# Feature-branch pushes and non-staging pushes pass through untouched.
#
# Keying rationale (see project memory + branch-flow.md):
#   - A bootstrap validates a TREE, not a commit label. Rebases / cherry-picks
#     preserve trees, so we match on `git rev-parse <sha>^{tree}`, falling back
#     to exact commit match. metadata.json records `commit`; we derive its tree.
#   - We scan ALL sibling worktrees' bootstrap/runs/, since a bootstrap may have
#     run in the worktree that produced the SHA (459, 566, ...), not in staging.
#   - We require the run to cover at least the {haskell,java,python} triad —
#     the minimum bar the staging cycle runs. A partial/single-host run does
#     not clear the gate.
#
# Legitimate bypass (logged): doc-only rides and emergency revert-to-a-
# previously-green-tree do not need a fresh bootstrap. Bypass with:
#   HYDRA_BOOTSTRAP_GATE_BYPASS="<reason>" git push origin <sha>:main
# The reason is required and is appended to bin/.bootstrap-gate-bypass.log.
#
set -euo pipefail

remote_name="${1:-}"
GATED_REF="refs/heads/main"
TRIAD=(haskell java python)
# Staging worktrees are named differently per machine (staging here,
# hydra-staging on marvin7). Match any name CONTAINING
# "staging" (case-insensitive) so every machine's staging worktree is gated
# automatically — fail-safe: a new staging worktree gates itself by its name,
# with no per-machine allowlist to forget. Human worktrees (feature_/bug_/
# task_/main) don't contain "staging", so manual pushes pass free.
GATED_WORKTREE_PATTERN="staging"

# Resolve repo roots. git-common-dir → shared bare repo (object store for tree
# resolution of any sha). We scan worktrees relative to it.
common_dir="$(git rev-parse --git-common-dir)"
# hydra.git/  →  its parent is hydra/ ; worktrees live at hydra/worktrees/*
repo_root="$(cd "$(dirname "$common_dir")" && pwd)"
worktrees_root="$repo_root/worktrees"

# Scope: only gate pushes ORIGINATING FROM a staging worktree (where the agent
# lands). pre-push runs in the pushing worktree's context, so --show-toplevel
# names it. Any other worktree — the user's manual-push origin — passes free.
# Case-insensitive substring match so staging / hydra-staging all
# gate; feature_/bug_/task_/main worktrees don't match and pass free.
origin_worktree="$(basename "$(git rev-parse --show-toplevel 2>/dev/null || echo '?')")"
case "${origin_worktree,,}" in
  *"$GATED_WORKTREE_PATTERN"*) : ;;   # a staging worktree — enforce the gate
  *) exit 0 ;;                        # not staging — pass free
esac

log_bypass() {
  local sha="$1" reason="$2" script_dir
  script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  local logf="$script_dir/.bootstrap-gate-bypass.log"
  printf '%s\t%s\t%s\t%s\n' "$(git rev-parse HEAD 2>/dev/null || echo '?')" \
    "$sha" "$remote_name" "$reason" >> "$logf" 2>/dev/null || true
}

# tree_of <sha> — the tree hash a bootstrap would have validated.
tree_of() { git rev-parse "$1^{tree}" 2>/dev/null || true; }

# Does any bootstrap run in any worktree validate this tree with a triad-covering
# status:ok? Prints the matching run dir on success.
find_passing_run() {
  local want_tree="$1" want_commit="$2"
  local md
  # newest-first so the reported match is the most recent
  while IFS= read -r md; do
    [ -f "$md" ] || continue
    python3 - "$md" "$want_tree" "$want_commit" "$common_dir" <<'PY'
import json, subprocess, sys
md, want_tree, want_commit, common_dir = sys.argv[1:5]
try:
    d = json.load(open(md))
except Exception:
    sys.exit(1)
if d.get("status") != "ok":
    sys.exit(1)
hosts = set(d.get("hosts") or [])
targets = set(d.get("targets") or [])
triad = {"haskell", "java", "python"}
if not (triad <= hosts and triad <= targets):
    sys.exit(1)
c = d.get("commit")
if not c:
    sys.exit(1)
# derive the run's tree from its recorded commit, via the shared object store
try:
    t = subprocess.check_output(
        ["git", "--git-dir", common_dir, "rev-parse", f"{c}^{{tree}}"],
        stderr=subprocess.DEVNULL).decode().strip()
except Exception:
    t = None
if (t and t == want_tree) or (c == want_commit) or (want_commit.startswith(c)) or (c.startswith(want_commit)):
    print(md)
    sys.exit(0)
sys.exit(1)
PY
    if [ $? -eq 0 ]; then return 0; fi
  done < <(ls -t "$worktrees_root"/*/bootstrap/runs/*/metadata.json 2>/dev/null)
  return 1
}

exit_code=0
while read -r local_ref local_sha remote_ref remote_sha; do
  # Only gate pushes to main. Everything else (feature branches, tags) passes.
  [ "$remote_ref" = "$GATED_REF" ] || continue
  # Branch deletion (local_sha all-zero) — allow.
  case "$local_sha" in *[!0]*) : ;; *) continue ;; esac

  want_tree="$(tree_of "$local_sha")"
  short="$(git rev-parse --short "$local_sha" 2>/dev/null || echo "$local_sha")"

  if [ -n "${HYDRA_BOOTSTRAP_GATE_BYPASS:-}" ]; then
    log_bypass "$local_sha" "$HYDRA_BOOTSTRAP_GATE_BYPASS"
    echo "pre-push: BOOTSTRAP GATE BYPASSED for $short → main" >&2
    echo "          reason: $HYDRA_BOOTSTRAP_GATE_BYPASS (logged)" >&2
    continue
  fi

  if match="$(find_passing_run "$want_tree" "$local_sha")"; then
    run_dir="$(dirname "$match")"
    echo "pre-push: bootstrap gate OK — $short (tree ${want_tree:0:12}) validated by" >&2
    echo "          $run_dir" >&2
  else
    echo "" >&2
    echo "pre-push: ✗ BOOTSTRAP GATE — refusing to push $short to origin/main" >&2
    echo "          No passing /bootstrap run validates this commit's tree" >&2
    echo "          (${want_tree:0:12}) across the {haskell,java,python} triad." >&2
    echo "" >&2
    echo "          Run /bootstrap on THIS commit and let it pass, then re-push." >&2
    echo "          Legitimate exception (doc-only ride, revert-to-green)? Re-push with:" >&2
    echo "            HYDRA_BOOTSTRAP_GATE_BYPASS=\"<reason>\" git push origin $short:main" >&2
    echo "" >&2
    exit_code=1
  fi
done

exit "$exit_code"
