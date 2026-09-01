# Save, then compact the conversation

## When to run

The user invokes this **between major tasks**, as a context-switching measure:
the conversation is about to be summarized into a fresh window, and anything
that lives only in the conversation will be lost.

Note the two different things being compacted, which the old name for this
command (`/compact-plan`) confused:

- **the branch plan** — a file on disk, pruned by `/save`
- **the conversation** — this session's context, summarized by the built-in
  `/compact`

This command is the second wrapped around the first, in that order.

## Who can invoke what

**`/compact` is a Claude Code CLI built-in, not a skill.** There is no tool
call that triggers it — it is not in the skill registry, and `Skill({skill:
"compact"})` will fail. **Only the user can run it, by typing it.**

That asymmetry is the whole reason this section exists. Step 1 is yours; step 2
is the user's. Do not narrate step 2 as though you were about to perform it,
and do not treat your inability to perform it as a reason to ask whether to
proceed — there is nothing to decide. Hand off, plainly and once.

## Procedure

1. **Run `/save`.** Follow that skill in full: record this session's state into
   the plan, move any durable lessons to memory files or docs, then prune the
   plan to its durable core and write it in a single `Write`.

   This must complete BEFORE compacting. `/compact` is lossy — the plan has to
   be a complete handoff first, because after compaction the plan may be the
   only surviving record.

2. **Hand off to the user for `/compact`.** End your reply with this line, on
   its own, as the very last thing you write:

   > **Plan saved (N → M lines). Type `/compact` now to finish `/save-and-compact`.**

   Substitute the real before/after line counts. This is not optional and not
   conditional: emit it on every run, even when the plan barely changed, and
   even when you are mid-task. It is the only thing standing between the user
   and a forgotten compaction, so nothing may follow it — no caveats, no
   "meanwhile", no summary of open work. Put anything else you need to say
   ABOVE it.

   Never ask permission to compact. The user already asked, by invoking this
   command; a question here just costs a round-trip and risks the step being
   dropped.

3. **Re-read the trimmed plan** — after the user's `/compact` lands, in the
   fresh window. Immediately `Read` the plan document back in. Do not skip
   this: the compaction summary may reference the plan as it was *before* the
   prune, so re-reading is what guarantees the new window holds the current
   lean version rather than the bloat you just removed.

## Notes

- If background tasks or monitors are still running, they MUST appear in the
  plan's "Current state" (with their IDs) before compacting, so the next window
  knows to keep watching them. `/save` step 3 covers this.
- Use plain `/save` when you want the handoff recorded without clearing context
  — e.g. at the end of a session, or before a risky operation.
