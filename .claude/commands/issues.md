---
description: Drain the pending issue-proposal queue with the user — present each draft, discuss, then file (+ usually spawn) or discard per the user's verdict. Filing and spawning are per-item user-gated; this command never acts on its own recommendations.
allowed-tools:
  - Read
  - Write
  - Edit
  - Bash(gh issue *)
  - Bash(gh api *)
  - Bash(git worktree list*)
  - Bash(ls *)
  - Bash(mv *)
  - Bash(cp *)
  - Bash(bin/spawn-issue-worktree.sh *)
---

# Present, discuss, and disposition proposed issues

## When to run

User-invoked, typically on a coordinator session, when proposals have
accumulated — from the proposals channel
(`claude-hydra-messages/proposals/pending/`), the branch plan's findings /
decision-register sections, or drafts held inline in recent messages.
The point of the command is a single structured pass: everything pending gets
a verdict, nothing lingers half-proposed.

## Procedure

1. **Gather.** Collect every pending proposal:
   - `claude-hydra-messages/proposals/pending/*.md` (the formal queue);
   - the branch plan doc's findings / register / decision-batch sections;
   - anything flagged "for the next batch" in recent inbox archives.
   Dedupe (the same finding often arrives via two routes). Note each item's
   provenance (who found it, where the evidence lives).

2. **Present.** Show the user each draft in full — title plus complete body,
   ready to file verbatim — ordered by your severity assessment. For each,
   state: proposed parent issue, proposed type (bug/feature/task), a
   recommended disposition (file+spawn / file-only / fold-into-existing /
   defer / discard), and one sentence of why. Enrich bodies with all context
   held (repro accounts, run IDs, commit SHAs, severity logic) — the issue
   should stand alone for a cold reader.

3. **Discuss.** Iterate per the user's feedback; amend drafts inline. The
   user may re-parent, merge drafts, downgrade, or redirect an item into a
   checklist line on an existing issue instead (a common outcome — offer it
   when a pending event, e.g. a release-gated task, would dissolve the
   symptom anyway).

4. **Execute each verdict — only on explicit per-item approval** (hard rule
   5: "file 1, 3; discard 2" in this conversation is the gate; a
   recommendation is never approval):
   - **File:** `gh issue create --title ... --body-file ...`, then link the
     parent as a NATIVE sub-issue (a body mention is not a parent link):
     fetch node IDs, then
     `gh api graphql -f query='mutation{addSubIssue(input:{issueId:"<parent>",subIssueId:"<child>"}){subIssue{number}}}'`.
     Verify the link landed.
   - **Spawn** (usually wanted with file — confirm if the user didn't say):
     check `worktrees/closed/` for a prior worktree on the same issue first;
     then `COORDINATOR=<current-coordinator> TYPE=<bug|feature|task>
     PARENT=<parent-NNN> bin/spawn-issue-worktree.sh <NNN> <slug> "<title>"`.
     COORDINATOR must be the coordinator CURRENT at spawn time — a stale
     value misroutes the worker's briefing (observed failure). If the new
     issue's parent belongs to another coordinator's subtree, spawn under
     THAT coordinator and notify them (verify-after-copy).
   - **Fold:** apply the content where directed (existing issue's checklist,
     a doc, a parked worker's inbox for pickup at wake) and record the
     disposition so the original proposer's finding isn't orphaned.
   - **Discard:** record why in the plan doc; the reasoning is the artifact.

5. **Bookkeeping.** Move dispositioned proposal files
   `pending/ → approved/` or `rejected/` with the verdict recorded inside;
   update the plan doc's register; notify affected coordinators and (if the
   proposer still runs) the proposing worker. All sends verify-after-copy.

## Hard rules in force

- Never file, close, comment, label, re-parent, or spawn without the user's
  explicit per-item approval given in this conversation.
- Draft-and-show means show the FULL text that will be filed, not a summary.
- Check `worktrees/closed/` before any spawn (`--force-respawn` to override).
- One issue → one active worker branch; don't spawn onto an issue that
  already has one.
