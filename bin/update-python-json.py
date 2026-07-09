#!/usr/bin/env python3
"""Update dist/json/hydra-python from the Python DSL sources.

Originally introduced for issue #344 as the "Python self-host demo"; now
the canonical Python DSL → JSON step in the regular sync pipeline
(Phase 5). The same dist/json output that's normally produced by the
Haskell pipeline (update-json-main reading
packages/hydra-python/src/main/haskell/Hydra/Sources/Python/*.hs) is
instead produced by this driver, reading the parallel
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
import os
import sys
import time
from pathlib import Path

# Anchor PYTHONPATH to the worktree root.
#
# #370: the hydra-python coder RUNTIME comes from one of two places.
#   - published-host (default): the wrapper (generate-hydra-python-from-python.sh)
#     puts the published hydra-python wheels' site-packages on PYTHONPATH and sets
#     HYDRA_PYTHON_HOST_MODE=published. We then must NOT prepend the local
#     dist/python/* trees, or they would shadow the published wheels.
#   - local-host (bootstrap shim) or a direct run: we prepend the local
#     dist/python/hydra-{kernel,python} trees as before.
# The DSL sources (packages/hydra-python) and the driver (heads/python) are always
# local in both modes.
_HERE = Path(__file__).resolve().parent
_ROOT = _HERE.parent
_PUBLISHED = os.environ.get("HYDRA_PYTHON_HOST_MODE") == "published"
_subs = ["packages/hydra-python/src/main/python"]
if not _PUBLISHED:
    _subs += [
        "dist/python/hydra-kernel/src/main/python",
        "dist/python/hydra-python/src/main/python",
    ]
_subs += ["heads/python/src/main/python"]
for sub in _subs:
    p = str(_ROOT / sub)
    if p not in sys.path:
        sys.path.insert(0, p)

# The `hydra` package is an extend_path namespace package, but in local-host mode
# it may already be imported (and its __path__ fixed) via the venv's editable
# install of heads/python before these sys.path inserts take effect — so the
# namespace never merges in the dist/python/hydra-{kernel,python} trees and
# `import hydra.codegen` (a generated kernel module that lives ONLY under
# dist/python/hydra-kernel) fails. This bit CI under `uv run`, where the editable
# .pth resolves `hydra` to heads/python alone (#472 local-host shim). Re-extend
# hydra.__path__ over every sys.path entry that carries a hydra/ dir so the merge
# is deterministic regardless of import order or installer (uv/pip/editable).
if not _PUBLISHED and "hydra" in sys.modules:
    import pkgutil
    sys.modules["hydra"].__path__ = pkgutil.extend_path(
        sys.modules["hydra"].__path__, "hydra")

import hydra.codegen as codegen
from hydra.overlay.python.dsl.python import Left, Right
from hydra.generation import (
    bootstrap_graph,
    infer_and_write_by_package,
    load_modules_from_json,
    read_manifest_field,
    reload_term_signature_sources,
    synthesize_and_write_dsl_modules,
    write_package_manifests,
)

# Python source module names under hydra.sources.python.* (matches Haskell
# Hydra.Sources.Python.* set; alphabetical to match Haskell manifest order).
SOURCE_MODULE_NAMES = [
    "coder",
    "environment",
    "language",
    "names",
    "pyproject",
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
        # run (hydra-python sources only) this is effectively a one-iteration
        # loop, but the driver shape is in place for future multi-package
        # native-coder updates.
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
        # #370/#346: also synthesize the DSL-wrapper modules
        # (hydra.dsl.python.{environment,syntax}) that the legacy Haskell
        # update-json-main DSL pass used to write. Now that hydra-python is
        # single-writer (no Haskell DSL fallback), the native driver owns its
        # full emission set. The DSL-type source modules are the type-defining
        # ones: hydra.python.environment and hydra.python.syntax.
        #
        # #556: hydra.python.utils and hydra.python.names are demand-curated in
        # too (mirroring the kernel's dslTermModules, #467) — they're the source
        # modules behind the _kernel_refs.py var("hydra.python.utils..."/
        # "hydra.python.names...") string refs this issue retires. Unlike the
        # type-only modules above, these are TERM-only, so their term
        # definitions need a TermSignature before generate_ref_bindings will
        # emit anything for them — see reload_term_signature_sources below.
        dsl_type_mods = [
            m for m in py_sources
            if m.name.value in (
                "hydra.python.environment", "hydra.python.syntax",
                "hydra.python.utils", "hydra.python.names")
        ]
        # #467/#556: the raw dsl_type_mods entries have
        # termDefinitionSignature = None (inference never runs on derived
        # modules); reload the term-bearing ones from their just-written
        # dist/json counterparts, which carry infer_and_write_by_package's
        # inferred signatures.
        dsl_sources_for_synthesis = reload_term_signature_sources(
            dist_json_root, universe_all, dsl_type_mods)
        written_dsl = synthesize_and_write_dsl_modules(
            dist_json_root, universe_all, dsl_sources_for_synthesis)
        print(f"  DSL wrappers: {len(written_dsl)} module(s) written", flush=True)

        # #556: write the native package's manifest.json source-driven (rich
        # schema, partitioned by Generation.namespace_to_package), mirroring
        # Java's writePackageManifests. Previously this manifest was a static
        # leftover from the retired Haskell DSL pass (legacy dslModules/
        # mainModules-only schema, #511), so it never picked up hydra.python.
        # utils/names once they became DSL-generated here — silently starving
        # the Haskell-side assembler of hydra/dsl/python/{utils,names}.py.
        # mainModules = the loaded source modules; mainDslModules = the
        # DSL-type source modules; mainEncodingModules is empty (this driver
        # doesn't yet synthesize encode/decode wrappers for hydra-python).
        write_package_manifests(dist_json_root, py_sources, dsl_type_mods, [])

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
