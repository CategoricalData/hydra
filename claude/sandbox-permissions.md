# Sandbox permission bypass

On a **sandbox machine**, every Hydra agent — staging and spawned issue/feature
agents alike — runs with permission bypass, so no tool call ever stops on a
permission prompt. On a **non-sandbox machine** (e.g. a developer's laptop),
agents keep normal prompting. This document defines how a machine declares itself
a sandbox and how the bypass is applied at each agent-creation path.

## Why

An agent fleet that stalls on permission prompts makes no autonomous progress —
an overnight run can advance less than a minute between prompts, because each
prompt idles until a human advances it. On a sandbox the prompts protect nothing
(the machine is disposable and isolated), so they are pure cost. But bypass must
**not** be global: it is unsafe on a real development machine, and it must not be
baked into any checked-in file, or it would follow the repo to the laptop.

The rule: **bypass is a property of the machine, not of the repo.** A machine
opts in with a local marker; every agent-creation path reads that marker and
applies bypass only when it is present.

## The sandbox marker

A machine declares itself a sandbox by the presence of the file:

```
~/.hydra-sandbox
```

- **Present** → this machine is a sandbox; Hydra agents get bypass.
- **Absent** → not a sandbox; agents prompt normally.

It is per-machine, lives in `$HOME` (never in the repo), and is created once with
`touch ~/.hydra-sandbox`. Adding a new sandbox is just creating the file; a laptop
simply never has it. Nothing checked into the repo encodes which machines are
sandboxes — the repo stays machine-agnostic.

## Enforcement points (both gated on the marker)

Bypass must be applied wherever an agent is *created*. There are two paths:

### 1. Spawned agents — `bin/spawn-issue-worktree.sh`

The spawn script seeds each new worktree's `.claude/settings.json`. The
checked-in template (`bin/claude-hooks/template-settings.json`) must **not** carry
`defaultMode: bypassPermissions` (that would leak bypass to every machine that
clones the repo). Instead, the spawn script **injects** `defaultMode:
"bypassPermissions"` into the seeded `settings.json` **only when `~/.hydra-sandbox`
exists.** On a non-sandbox machine the marker is absent, so the seeded settings
omit the bypass and the spawned agent prompts normally.

### 2. Staging and any hand-started session — `claude-remote`

Sessions started by hand (staging, a relaunched agent, a coordinator) go through
the `claude-remote` shell function (in `~/.bashrc`, per-machine, not in the repo).
That function **auto-adds `--dangerously-skip-permissions` when `~/.hydra-sandbox`
exists.** So on a sandbox, `claude-remote <label>` alone yields a bypassed session;
on a laptop the same command prompts normally. (Because `claude-remote` lives in
the shell profile, this edit is per-machine and outside the Hydra tree — make it
on each sandbox machine.)

## What bypass does and does NOT cover

Bypass removes the *harness* permission layer — including the "shell syntax cannot
be statically analyzed" prompt that the allowlist cannot suppress (the prompt
class that most often stalls a fleet, since `$(...)`, heredocs, and control-flow
can't be matched against an allowlist at all).

It does **not** remove OS-level protections: `sudo` and anything requiring
elevated privileges are still gated by the operating system, which is correct —
that boundary is the machine's, not Claude's.

It also does **not** change the fleet's *policy* gates, which are about
outward-facing state, not local safety: staging still runs its landing discipline,
`gh` issue writes and force-pushes stay user-gated, and the draft-and-show rule
holds. Bypass is about not stopping on *local* tool calls in a disposable
environment; it is not a license for outward actions.

## Checklist for a new sandbox machine

1. `touch ~/.hydra-sandbox`
2. Ensure `claude-remote` in that machine's `~/.bashrc` reads the marker and adds
   `--dangerously-skip-permissions` when present.
3. That's it — spawned agents pick it up automatically via the spawn script.
