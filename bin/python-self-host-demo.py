#!/usr/bin/env python3
"""Generate dist/json/hydra-python from the Python DSL sources.

This is the self-hosting demo for issue #344: the same dist/json output
that's normally produced by the Haskell pipeline (update-json-main reading
packages/hydra-python/src/main/haskell/Hydra/Sources/Python/*.hs) is
instead produced by the Python pipeline reading the parallel
packages/hydra-python/src/main/python/hydra/sources/python/*.py.

The driver:
  1. Loads the kernel universe from dist/json/hydra-kernel/.
  2. Imports each hydra.sources.python.* module and pulls its module_.
  3. Runs hydra.codegen.infer_modules_given over (universe + python_sources).
  4. Builds a schemaMap from the inferred graph.
  5. Encodes each python source module to JSON via codegen.module_to_json.
  6. Writes each output to dist/json/hydra-python/<namespace-path>.json.

Use --out-root to redirect output; default is dist/json/hydra-python.

For comparison vs the Haskell path, run with --out-root=/tmp/hp-from-python
and diff against the existing dist/json/hydra-python (which was generated
from the Haskell sources).
"""
from __future__ import annotations

import argparse
import importlib
import sys
import time
from pathlib import Path

# Anchor PYTHONPATH to the worktree root.
_HERE = Path(__file__).resolve().parent
_ROOT = _HERE.parent
for sub in (
    "packages/hydra-python/src/main/python",
    "dist/python/hydra-kernel/src/main/python",
    "dist/python/hydra-python/src/main/python",
    "heads/python/src/main/python",
):
    p = str(_ROOT / sub)
    if p not in sys.path:
        sys.path.insert(0, p)

import hydra.codegen as codegen
from hydra.dsl.python import Left, Right
from hydra.generation import (
    bootstrap_graph,
    infer_and_write_by_package,
    load_modules_from_json,
    read_manifest_field,
)

# Python source module names under hydra.sources.python.* (matches Haskell
# Hydra.Sources.Python.* set; alphabetical to match Haskell manifest order).
SOURCE_MODULE_NAMES = [
    "coder",
    "environment",
    "language",
    "names",
    "serde",
    "syntax",
    "testing",
    "utils",
]


def _namespace_to_path(ns: str) -> str:
    """hydra.python.language -> hydra/python/language."""
    return ns.replace(".", "/")


def _load_python_sources():
    """Import each hydra.sources.python.<m> module and return its module_."""
    mods = []
    for name in SOURCE_MODULE_NAMES:
        full = f"hydra.sources.python.{name}"
        m = importlib.import_module(full)
        mods.append(m.module_)
    return mods


def _build_universe(quiet: bool = True):
    """Load the kernel mainModules. Returns a list."""
    import contextlib, io
    kernel_json = str(_ROOT / "dist/json/hydra-kernel/src/main/json")
    nss = read_manifest_field(kernel_json, "mainModules")
    if quiet:
        # load_modules_from_json prints "Loaded: hydra.X" per module — silence.
        with contextlib.redirect_stdout(io.StringIO()):
            return list(load_modules_from_json(kernel_json, nss))
    return list(load_modules_from_json(kernel_json, nss))


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--out-root",
                    default=str(_ROOT / "dist/json/hydra-python/src/main/json"),
                    help="Output directory root (default: dist/json/hydra-python/src/main/json)")
    ap.add_argument("--no-infer", action="store_true",
                    help="Skip inference (assumes type schemes are pre-set on every term). "
                         "Useful only when the DSL produces typed terms directly.")
    args = ap.parse_args()

    print(f"Loading universe...", flush=True)
    t0 = time.perf_counter()
    universe = _build_universe()
    t_universe = time.perf_counter() - t0
    print(f"  loaded {len(universe)} kernel modules ({t_universe:.1f}s)", flush=True)

    print(f"Importing Python source modules...", flush=True)
    t0 = time.perf_counter()
    py_sources = _load_python_sources()
    t_import = time.perf_counter() - t0
    print(f"  imported {len(py_sources)} python source modules ({t_import:.1f}s)", flush=True)
    for m in py_sources:
        ndefs = len(m.definitions) if m.definitions else 0
        print(f"    {m.name.value}: {ndefs} definitions", flush=True)

    # Resolve where the demo expects to write. --out-root is the legacy
    # contract: a single per-package json directory ending in
    # ".../hydra-python/src/main/json". The per-package driver works in
    # terms of the dist/json root and routes each package into its own
    # <pkg>/src/main/json/ subdir. Strip the four-segment per-package tail
    # when present; otherwise treat --out-root as the dist-json root
    # verbatim (non-canonical layout, e.g. /tmp output for --compare).
    out_root = Path(args.out_root).resolve()
    tail = ("hydra-python", "src", "main", "json")
    if out_root.parts[-len(tail):] == tail:
        dist_json_root = out_root.parents[len(tail) - 1]
    else:
        dist_json_root = out_root
    dist_json_root = str(dist_json_root)
    print(f"Output dist/json root: {dist_json_root}", flush=True)

    if args.no_infer:
        # Direct write path: no inference, no per-package iteration.
        # Useful only when the DSL produces typed terms directly.
        print("Skipping inference (--no-infer); direct write...", flush=True)
        t0 = time.perf_counter()
        graph = codegen.modules_to_graph(bootstrap_graph(),
                                         tuple(universe) + tuple(py_sources),
                                         tuple(py_sources))
        schema_map = codegen.build_schema_map(graph)
        n_written = 0
        for m in py_sources:
            result = codegen.module_to_json(schema_map, m)
            match result:
                case Right(value=json_str):
                    file_path = Path(dist_json_root) / "hydra-python" / "src" / "main" / "json" / \
                        (_namespace_to_path(m.name.value) + ".json")
                    file_path.parent.mkdir(parents=True, exist_ok=True)
                    new_content = json_str + "\n"
                    if file_path.exists() and file_path.read_text() == new_content:
                        continue
                    file_path.write_text(new_content)
                    n_written += 1
                case Left(value=err):
                    print(f"  ENCODE FAILED for {m.name.value}: {err}", flush=True)
                    return 2
        t_pkg = time.perf_counter() - t0
        print(f"  wrote {n_written} files ({t_pkg:.1f}s)", flush=True)
    else:
        # Per-package iterative inference + JSON write (mirrors the
        # Haskell-side inferAndWriteByPackage). For today's single-package
        # demo (hydra-python sources only) this is effectively a one-iteration
        # loop, but the driver shape is in place for future multi-package
        # self-hosts.
        print(f"Per-package inference + write...", flush=True)
        t0 = time.perf_counter()
        universe_all = tuple(universe) + tuple(py_sources)
        infer_and_write_by_package(
            hydra_root=str(_ROOT),
            dist_json_root=dist_json_root,
            universe_mods=universe_all,
            mods=tuple(py_sources),
            seed_acc=tuple(universe),
        )
        t_pkg = time.perf_counter() - t0
        print(f"  done ({t_pkg:.1f}s)", flush=True)

    t_total = t_universe + t_import + t_pkg
    print(f"\nSummary:", flush=True)
    print(f"  universe load:        {t_universe:>6.1f}s", flush=True)
    print(f"  source import:        {t_import:>6.1f}s", flush=True)
    print(f"  per-package + write:  {t_pkg:>6.1f}s", flush=True)
    print(f"  total:                {t_total:>6.1f}s", flush=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
