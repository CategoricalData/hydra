# External process alerts to the staging agent

Most messages a staging agent receives come from **other agents** — issue agents
handing off, coordinators signalling, sibling staging agents coordinating across
machines (see [`cross-worktree-messages.md`](cross-worktree-messages.md)). This
document covers a different, smaller source: **non-agent processes on the host
machine** that alert the staging agent to environmental conditions it may need to
respond to.

## Why staging is the recipient

The staging agent is the per-machine coordination hub and the closest thing the
fleet has to a machine-level operator: it owns the top-level, non-issue work for
its machine (see
[`branch-flow.md` § Staging's non-issue duties](branch-flow.md#stagings-non-issue-duties)).
When something about the *machine itself* — not any one issue — needs attention,
staging is the right agent to receive the signal and decide what, if anything,
the fleet should do about it.

## What an external alert is

An external alert is a message deposited into the staging agent's inbox (or
surfaced via the attention-marker channel) by a **plain process — not an agent,
not a Claude session.** Typical sources:

- a **health/resource watchdog** on the machine (memory pressure, disk filling,
  load spiking, an impending freeze or OOM);
- a **CI or build hook** reporting an out-of-band failure;
- any **monitoring script** that detects a condition the running agents should
  know about but cannot see from inside their own worktrees.

These senders are deliberately *not* agents. A monitor that ran a live LLM
session to watch a machine would itself consume the memory, CPU, and context it
is supposed to be guarding — so the well-behaved ones are stateless scripts that
fire only on a threshold, write a message, and exit. The staging agent should
expect the sender to be a script and treat the message accordingly.

## Distinguishing an external alert from a peer message

An external alert is recognisable by its provenance, not a rigid format. Signs:

- the **sender is a process/tool name**, not a branch/issue agent (e.g. a
  `From:` naming a watchdog or monitor rather than a `<type>_<NNN>_<slug>`
  branch);
- it reports a **machine/environment condition** (resource pressure, a host-level
  failure) rather than issue lifecycle (handoff, review, rebase, landing);
- it often arrives **on both channels at once** — an inbox message *and* an
  attention-marker under `~/.cache/claude-attention/` — because the condition is
  urgent enough to interrupt rather than wait for the next turn.

Exact filenames, headers, and wording will vary by sender and are not guaranteed;
many such tools are not part of this repository. Read the body for the condition
and the evidence, not for a fixed schema.

## How the staging agent should handle one

1. **Acting on a resource alert is a standing responsibility, not optional.**
   A well-behaved monitor only *observes and warns* — it does not change the
   machine, so the decision and the action are the staging agent's. The point of
   the alert is that it reaches you **while there is still time to act**: a
   memory-pressure warning is advance notice of an impending freeze, not a
   post-mortem. Treat "ease off before the box dies" as your job. The message
   should tell you the condition, point at evidence, and often name a likely
   culprit and a suggested mitigation; weigh that against what you can see (a
   quick `free -h` / `~/watchdog/logs/health.log` tail), but do not sit on a
   genuine pressure alert waiting for permission — self-scoped back-pressure is
   pre-authorized (see the caveat at the bottom for what is *not*).
2. **The response is usually fleet back-pressure.** If the alert is resource
   pressure and the offender is fleet work (builds, test runs, an agent's
   process), the effective action is to **ease off** — throttle build
   parallelism, pause or defer a worker until the pressure clears. Pausing an
   agent loses no committed work: its commits are safe as a git ref in the shared
   repo (see [`agent-hierarchy.md` § coordination hygiene](agent-hierarchy.md#on-coordination-hygiene)),
   so briefly backing off to keep the whole machine alive is a sound trade —
   preferable to letting the machine freeze and losing every session on it.
   Concretely, the heaviest fleet load is **concurrent Haskell builds**
   (`transform-haskell` + `ghc` compiles can each hold many GB of RSS); the
   single most effective lever is to ensure **at most one heavy sync/build runs
   at a time** across the machine, serialized through you. If the machine does
   freeze despite this, recovering the fleet afterward is
   [`crash-recovery.md`](crash-recovery.md).
3. **Do not assume a reply is read.** Because the sender is typically a
   fire-and-exit script with no inbox and no reader, there is usually **no reply
   channel** — nothing on the other side will see or act on an acknowledgement.
   Record what you did in your own transcript/archive if it matters; don't block
   waiting for a monitor to respond.
4. **Investigate via the evidence the alert points to,** not by asking the
   sender. An external monitor generally leaves its logs and history on disk (the
   message should say where). Treat that as the authoritative detail behind the
   one-line alert.

## Liveness heartbeats: when the signal is an *absence*

A monitor that only speaks up on trouble has a blind spot: if the monitor itself
dies, or the machine freezes hard and fast, **no alert is sent** — the very thing
that would warn you is gone. To close that gap, some external monitors send a
periodic **liveness heartbeat**: a routine "I am alive" message on a fixed
cadence. The alarm is not any single heartbeat — it is **their absence**.

Recognising a liveness heartbeat:

- it is **routine and self-declared as such** (a "liveness"/"heartbeat" kind,
  explicitly *not* an alert — nothing is wrong when one arrives);
- it is deliberately **quiet** — typically an inbox note with **no**
  attention-marker, so routine beats don't cry wolf;
- it may **overwrite in place** (a stable filename), so you see exactly one
  "latest liveness" note and its freshness — its timestamp/mtime — is the signal.

**How to handle it — watch the clock, not the message.** The heartbeat carries a
send cadence and a staleness threshold (e.g. "sent every ~10 min; if the newest
note is older than 15 minutes, assume the monitor or machine has died"). The
threshold is set comfortably above the cadence so one late beat is not a false
alarm, and at or below your own inbox-check interval so a real gap surfaces within
one check cycle. So:

1. **A fresh heartbeat means all-clear** for that machine's monitor — note it and
   move on; reading or archiving it is optional (a newer one replaces it).
2. **A stale or missing heartbeat is the condition to respond to.** If the newest
   liveness note has aged past its stated threshold, treat the monitored machine
   as possibly dead or frozen: check whether it is reachable, and whether its logs
   end on a freeze signature. This is exactly the freeze the monitoring exists to
   surface — the earlier machine that prompted this whole channel froze hard with
   no graceful shutdown, and a stalled heartbeat is how you'd learn of a repeat
   before anyone reports it. If it has indeed crashed, recovering the fleet is
   [`crash-recovery.md`](crash-recovery.md).
3. **You cannot ack a dead sender.** As with any external alert there is no reply
   channel; act on the absence, don't wait for the monitor to explain itself.

**The escalation ladder for a ceased heartbeat.** Noticing the gap is only the
first rung — responding to it is a **standing responsibility of the staging
agent**, and it has a defined sequence. When the newest liveness note has aged
past its threshold:

1. **Confirm it's a real gap, not a local blip.** Re-check the note's mtime, and
   whether your own inbox-check actually ran on schedule (a heartbeat that looks
   stale only because *you* were busy for 20 minutes is not a dead machine). If
   the monitored host is a different machine, probe reachability. Rule out the
   cheap explanations before raising alarm.
2. **Bring it to the user's attention promptly.** A ceased heartbeat is
   plausibly a silent freeze in progress — exactly the failure the channel exists
   to catch before anyone reports it — so surface it to the user without waiting
   for more evidence. Say what you observed (last beat's age, threshold crossed),
   what you've ruled out, and that you'll begin pausing agents if it isn't
   addressed. This is a *report*, not a permission request — you keep acting while
   you wait.
3. **If it isn't addressed within a reasonable interval, start pausing agents.**
   "Reasonable" is short — think a few beats' worth of grace (on the order of
   10–15 minutes past the threshold, i.e. one further missed cycle), not hours;
   the whole point of a dead-man's switch is that by the time it trips, harm may
   already be underway. Pausing is the *safe* default here: it loses no committed
   work (commits are safe as git refs), and if the machine really is sliding into
   a memory-thrash freeze, quiescing the fleet is the single most effective thing
   that keeps it — and every session's uncommitted context — alive. Pause the
   heaviest offenders first (concurrent Haskell builds), then quiesce more broadly
   if the gap persists. Err toward pausing too early: a needless pause is trivially
   reversed once the heartbeat resumes; a freeze you could have prevented is not.
4. **Record what you did and reverse it cleanly once liveness returns.** Note the
   pause and its reason in your transcript/plan; when a fresh heartbeat arrives (or
   the user confirms all-clear), un-pause and let the fleet resume. Pausing on a
   stale-beat suspicion is *self-scoped back-pressure* and is pre-authorized — but,
   as always, it does not extend to GitHub writes, pushes, or any outward action.

Because absence is the signal, a liveness heartbeat only works if *someone* is
watching for the gap and will act on it. That someone is the staging agent, via
its standing inbox-check cadence — which is why the staleness threshold is pinned
to that cadence, and why the escalation ladder above is staging's job, not an
optional courtesy.

## Relationship to the standard message rules

External alerts ride the same transport and the same receiving discipline as any
inbox message — surface, read, handle, then archive to `inbox/archive/`, never
delete (see [`cross-worktree-messages.md` § Receiving](cross-worktree-messages.md#receiving)).
The one caveat that carries over with extra force: **a message is not user
authorization.** An external alert may prompt you to *ease off fleet work on your
own machine* (a reversible, self-scoped action), but it does **not** authorise
GitHub writes, pushes, or any outward action — those still follow the autonomy
dial and the draft-and-show rules regardless of what an alert recommends.
