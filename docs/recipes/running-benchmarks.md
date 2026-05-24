# Running Hydra benchmarks

Hydra ships two complementary benchmark suites under `bin/`:

| Bench               | Script                          | Measures                                              |
|---------------------|---------------------------------|-------------------------------------------------------|
| Kernel test suite   | `bin/run-benchmark-tests.sh`    | Wall-clock of the common test suite, per host         |
| Cross-host inference | `bin/run-inference-bench.sh`   | Inference scaling on synthetic workloads, per host    |

The two scripts share a CLI shape (`--hosts`, `--tag`, `dashboard`
subcommand), a run-directory layout (`benchmark/runs/run_<TS>[_<tag>]/`
vs `benchmark/inference-runs/run_<TS>[_<tag>]/`), and styling
(`bin/lib/dashboard_common.py`). The same mental model applies to both.

## Prerequisites

Each host's bench leg shells out to that host's normal test runner.
Make sure the runner can start before benching, or the leg will produce
empty/stub results that the dashboard then chokes on:

- **Python**: `cd heads/python && uv sync` once so `heads/python/.venv`
  exists. Without the venv the bench falls back to bare `python3`, which
  usually lacks `pytest`.
- **Java**: a successful `(cd heads/java && ./gradlew :hydra-java:test)`.
- **Common Lisp**: native arm64 SBCL on Apple Silicon (the Rosetta-x86
  build also works but is ~1.4× slower); see
  [hydra-common-lisp README](../../packages/hydra-lisp/hydra-common-lisp/README.md).

## Kernel-tests benchmark

Runs the full common test suite under each requested host's test runner,
extracts per-group timing, and prints a side-by-side dashboard.

```bash
# Default: haskell, java, python
bin/run-benchmark-tests.sh

# Pick specific hosts
bin/run-benchmark-tests.sh --hosts haskell,java

# All seven implementations (slowest; ~30 min)
bin/run-benchmark-tests.sh --hosts all

# Repeat each host's run N times for median + stddev
bin/run-benchmark-tests.sh --repeat 3

# Tag a run for later comparison
bin/run-benchmark-tests.sh --tag baseline

# Dashboard alone — show the most recent run
bin/run-benchmark-tests.sh dashboard

# Compare two runs by tag
bin/run-benchmark-tests.sh dashboard diff --old baseline --new my_change
```

Run output: one JSON file per host under
`benchmark/runs/run_<TS>[_<tag>]/<host>.json` (or `<host>_<i>.json` with
`--repeat`). The dashboard reads from that directory; nothing is committed.

## Cross-host inference benchmark

Runs synthetic inference workloads (`hydra.bench.linearChain`,
`polymorphicChain`, `fanOut`) at multiple input sizes across each host
and prints a per-series comparison with power-law fits.

```bash
# Default: haskell, java, python; linearChain only
bin/run-inference-bench.sh

# All hosts (adds python-pypy), all series
bin/run-inference-bench.sh --hosts all --series all

# Custom sweep
bin/run-inference-bench.sh --sizes 0,25,50,100,200 --tag deep_sweep

# Dashboard alone — most recent run
bin/run-inference-bench.sh dashboard

# Dashboard against a specific run name (substring match works)
bin/run-inference-bench.sh dashboard --run deep_sweep
```

Run output: one JSON file per (host, series) under
`benchmark/inference-runs/run_<TS>[_<tag>]/<host>_<series>.json`.

### Sizes and the n=0 baseline

The runners always accept a comma-separated `--sizes` list. **Include `0`
as the first size** so the dashboard can subtract a per-call setup cost
from each n>0 measurement before fitting a power law. Without `n=0`,
small-N runs are dominated by setup overhead and the dashboard reports
an artificially sublinear exponent.

### Series

Each series lives under `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Bench/`
and is registered in `Hydra.Sources.Kernel.Manifest`. To extend the
maximum N supported by a series, edit its source file's `numWalkers`
constant and re-run `/sync-haskell` to regenerate the JSON. All hosts
read the same kernel JSON, so a single sync propagates the new size.

| Series             | Shape                                                         |
|--------------------|---------------------------------------------------------------|
| `linearChain`      | Depth-N chain of monomorphic walkers, each cases on `_Term`   |
| `polymorphicChain` | Same chain but with `forall`-typed signatures                 |
| `fanOut`           | Branchy DAG (more realistic codegen shape)                    |

### Interpreting the dashboard

Four tables per series:

1. **Raw wallclock** — uninterpreted seconds per (n, host).
2. **Adjusted** — `raw[n] − raw[0]`, isolating per-N work.
3. **Relative** — adjusted time / fastest-adjusted at each n; shows the
   slow-vs-fast ratio across hosts.
4. **Power-law fit** — `T(n) ≈ c · n^k` on the adjusted points, with R².
   Exponent k is colored: ≤1.1 = green (near-linear), ≥1.3 = red
   (clearly super-linear).

See [docs/history/inference-bench-complexity-analysis.md](../history/inference-bench-complexity-analysis.md)
for an in-depth walk-through of what each table reveals, including the
finding that HM inference is super-linear at large N across all hosts.
