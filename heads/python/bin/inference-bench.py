#!/usr/bin/env python3
"""Cross-host inference benchmark — Python runner.

Loads the synthetic ``hydra.bench.inferenceScaling`` workload from the kernel
JSON, takes prefixes of the chained walker definitions, and times
``hydra.codegen.infer_modules_given`` on each prefix. Emits a JSON line per
size to stdout describing ``{host, n, elapsed_seconds, ok}``, plus a final
summary on stderr.

Usage:
    inference-bench.py [--sizes 10,25,50,100] [--out path/to/result.json]

This script is meant to be invoked by ``bin/run-inference-bench.sh``, which
dispatches to per-host runners and aggregates results. It can also be run
standalone for development.
"""
from __future__ import annotations

import argparse
import json
import os
import sys
import time

# Resolve the worktree root from this script's location:
# heads/python/bin/inference-bench.py -> ../../..
_HERE = os.path.dirname(os.path.abspath(__file__))
_ROOT = os.path.abspath(os.path.join(_HERE, "..", "..", ".."))

# Wire up sys.path so this script can run via `python` directly without uv.
for sub in (
    "heads/python/src/main/python",
    "dist/python/hydra-kernel/src/main/python",
):
    p = os.path.join(_ROOT, sub)
    if p not in sys.path:
        sys.path.insert(0, p)

import hydra.codegen as codegen
from hydra.context import Context
from hydra.core import Name
from hydra.dsl.python import FrozenDict, Left, Right
from hydra.generation import (
    bootstrap_graph,
    load_modules_from_json,
    read_manifest_field,
)
from hydra.packaging import DefinitionTerm, Module, Namespace, TermDefinition

KERNEL_JSON = os.path.join(_ROOT, "dist/json/hydra-kernel/src/main/json")
BENCH_JSON = os.path.join(_ROOT, "dist/json/hydra-bench/src/main/json")
DEFAULT_BENCH_NS = "hydra.bench.linearChain"


def _parse_sizes(s: str) -> list[int]:
    return sorted({int(x.strip()) for x in s.split(",") if x.strip()})


def _load_universe() -> tuple[Module, ...]:
    """Load mainModules from the kernel JSON manifest plus the hydra-bench package.

    hydra-bench is opt-in: its modules live under dist/json/hydra-bench/ and are
    populated by bin/sync-bench.sh, not by the default sync. If the package
    directory is absent, the benchmark won't find its workload — that's the
    intended signal to run sync-bench first.
    """
    kernel_nss = read_manifest_field(KERNEL_JSON, "mainModules")
    universe = list(load_modules_from_json(KERNEL_JSON, kernel_nss))
    if os.path.isdir(BENCH_JSON):
        bench_nss = read_manifest_field(BENCH_JSON, "mainModules")
        universe.extend(load_modules_from_json(BENCH_JSON, bench_nss))
    return tuple(universe)


def _find_bench_module(universe: tuple[Module, ...], ns: str) -> Module:
    for m in universe:
        if m.namespace.value == ns:
            return m
    raise RuntimeError(
        f"bench module {ns} not found in kernel JSON. "
        "Run bin/sync-haskell.sh to regenerate."
    )


def _make_synthetic_module(bench: Module, n: int) -> Module:
    """Build a synthetic module containing the first n walker defs.

    The defs are renamed into a private namespace (``z.bench.scaling``) to
    avoid clashing with the original definitions in the universe — inference
    needs the original chain to resolve walker(k-1) lookups, AND a fresh
    target module to actually time.

    Each renamed def keeps the same body, which still references the
    bench-module's defs by absolute name. So the synthetic module pulls types
    from the bench module without competing with it for namespace ownership.

    n=0 produces an empty target module — used to measure per-inference-call
    setup cost, which the dashboard subtracts before fitting complexity curves.
    """
    defs = [d for d in bench.definitions if isinstance(d, DefinitionTerm)]
    # Defs come in order walker0, walker1, ...; take first n.
    take = defs[:n]
    target_ns = Namespace("z.bench.scaling")
    renamed: list[DefinitionTerm] = []
    for td in take:
        local = td.value.name.value.split(".")[-1]
        renamed.append(
            DefinitionTerm(
                TermDefinition(
                    Name(f"{target_ns.value}.{local}"),
                    td.value.term,
                    td.value.type_scheme,
                )
            )
        )
    deps = (bench.namespace,) + tuple(bench.dependencies)
    return Module(
        bench.description,
        target_ns,
        deps,
        tuple(renamed),
    )


def _time_inference(universe: tuple[Module, ...], target: Module) -> tuple[float, bool, str]:
    ctx = Context((), (), FrozenDict({}))
    t0 = time.perf_counter()
    result = codegen.infer_modules_given(
        ctx, bootstrap_graph(), tuple(universe) + (target,), (target,)
    )
    elapsed = time.perf_counter() - t0
    match result:
        case Right(value=_):
            return elapsed, True, ""
        case Left(value=err):
            return elapsed, False, str(err)[:200]
        case _:
            return elapsed, False, f"unexpected: {type(result).__name__}"


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--sizes", default="0,10,25,50,100",
                    help="Comma-separated prefix sizes (default: 0,10,25,50,100). "
                         "n=0 measures per-call inference setup cost (empty target module).")
    ap.add_argument("--namespace", default=DEFAULT_BENCH_NS,
                    help=f"Hydra namespace of the bench workload (default: {DEFAULT_BENCH_NS})")
    ap.add_argument("--out", default=None,
                    help="Write JSON results to this file (default: stdout)")
    ap.add_argument("--host-tag", default=None,
                    help="Override host tag in output JSON (default: detected)")
    args = ap.parse_args()

    # Detect interpreter for the host tag.
    if args.host_tag:
        host = args.host_tag
    else:
        impl = sys.implementation.name  # "cpython" / "pypy"
        host = f"python-{impl}"

    sizes = _parse_sizes(args.sizes)

    print(f"Loading universe from {KERNEL_JSON} ...", file=sys.stderr, flush=True)
    t0 = time.perf_counter()
    universe = _load_universe()
    print(f"  loaded {len(universe)} modules ({time.perf_counter()-t0:.1f}s)",
          file=sys.stderr, flush=True)

    bench = _find_bench_module(universe, args.namespace)
    available = sum(1 for d in bench.definitions if isinstance(d, DefinitionTerm))
    print(f"Bench workload {args.namespace}: {available} definitions available",
          file=sys.stderr, flush=True)

    results = []
    for n in sizes:
        if n > available:
            print(f"  skipping n={n} (only {available} defs available)",
                  file=sys.stderr, flush=True)
            continue
        target = _make_synthetic_module(bench, n)
        elapsed, ok, err = _time_inference(universe, target)
        status = "OK" if ok else f"FAIL: {err}"
        print(f"  n={n:>3}: {elapsed:>6.2f}s {status}", file=sys.stderr, flush=True)
        results.append({
            "host": host,
            "namespace": args.namespace,
            "n": n,
            "elapsed_seconds": elapsed,
            "ok": ok,
            "error": err if not ok else None,
        })

    payload = json.dumps(results, indent=2)
    if args.out:
        with open(args.out, "w") as f:
            f.write(payload)
        print(f"Wrote {args.out}", file=sys.stderr)
    else:
        print(payload)

    return 0 if all(r["ok"] for r in results) else 1


if __name__ == "__main__":
    sys.exit(main())
