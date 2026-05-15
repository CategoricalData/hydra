---
description: Run bin/run-bootstrapping-demo.sh to exercise cross-host code generation plus tests. Default is the haskell/java/python triad (NOT the full matrix — note this is opposite to /sync's default). Several invocation modes — see below.
argument-hint: [lang1,lang2,... | all | others]
allowed-tools:
  - Bash(bin/run-bootstrapping-demo.sh*)
  - Read
  - Bash(git status*)
---

# Cross-host bootstrap demo

## When to run

User-invoked. Typical triggers:

- Validating cross-host code generation end-to-end
- Pre-release verification
- Investigating a CI failure that's not reproducible from sync alone
  (since this demo exercises the *full* generation pipeline including
  tests, where sync alone runs only Haskell `stack test`)

## Invocation modes

Dispatch on `$ARGUMENTS`:

### No argument — bootstrapping triad (default)

```bash
bin/run-bootstrapping-demo.sh
```

Default hosts and targets: `haskell,java,python` × `haskell,java,python`
— the bootstrapping triad, **NOT** the full matrix. Note the asymmetry
with `/sync`: `/sync` defaults to `all × all`, this command
defaults to the triad.

### `lang1,lang2[,...]` — scoped

```bash
bin/run-bootstrapping-demo.sh --hosts $ARGUMENTS --targets $ARGUMENTS --tag $(echo "$ARGUMENTS" | tr ',' '_')
```

Same languages on both sides; the `--tag` lets the dashboard show
multiple runs side by side.

### `all` — full matrix

```bash
bin/run-bootstrapping-demo.sh --hosts all --targets all --tag all
```

Full all-hosts × all-targets matrix. **Long-running** — an hour or
more — typically reserved for pre-release verification or overnight
runs.

### `others` — narrow "other hosts → python" check

```bash
bin/run-bootstrapping-demo.sh --hosts scala,lisp --targets python
```

Narrow bootstrapping pass that checks whether the "other" hosts (Scala
and Lisp dialects) can bootstrap Python — useful for catching
host-specific emit regressions without paying the full matrix cost.

## Output handling

**Critical:** capture full stdout+stderr to a temp file (do NOT pipe
through `grep`/`tail` — the dashboard table will be lost). When done,
show the script's dashboard output **verbatim**:

- The NxM results matrix
- Per-path timings
- Total time

Do not reformat the table. The dashboard is meaningful as-printed; any
reflow destroys column alignment.

## On long-running invocation

For `/bootstrap all` or any pass that takes >10 minutes, give the user a
brief status update approximately every 10 minutes. Watch for hangs
(especially around the 18-22 minute mark in step 5 of a full bootstrap
on a memory-constrained host).

## On failure

If a bootstrap fails, investigate and fix the issue, then re-run the
failing step and all subsequent steps. Don't re-run steps that
succeeded. The dashboard table will show which cells are red.
