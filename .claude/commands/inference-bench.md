---
description: Run the cross-host inference benchmark via bin/run-inference-bench.sh. The runner invokes sync-bench.sh automatically before measuring so the bench dist is always current. Measures inference cost across hosts on a controlled hydra-bench workload.
argument-hint: [--hosts <list>]
allowed-tools:
  - Bash(bin/run-inference-bench.sh*)
  - Bash(bin/sync-bench.sh*)
  - Read
---

# Cross-host inference benchmark

## When to run

User-invoked. Typical triggers:

- Validating a perf-sensitive kernel change
- Pre-release performance review
- Investigating an inference-cost regression

## Procedure

Run:

```bash
bin/run-inference-bench.sh $ARGUMENTS
```

The runner invokes `bin/sync-bench.sh` itself, so the hydra-bench package
is current before measurement. Don't run sync-bench separately first.

Pass any host-scoping arguments through to the underlying runner:

```bash
bin/run-inference-bench.sh --hosts haskell,python
```

Capture full stdout+stderr to a file so the dashboard output remains
intact. Do not pipe through `grep` or `tail` — the dashboard table will
be lost.

## After completion

Show the dashboard output verbatim, then summarize:

- Wall-clock per host
- Comparison to prior recorded baselines (if mentioned in
  `docs/history/inference-bench-complexity-analysis.md` or similar)
- Anything that looks anomalous (regression, spike, ordering inversion)

Do not reformat the dashboard table.
