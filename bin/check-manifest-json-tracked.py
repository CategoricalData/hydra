#!/usr/bin/env python3
"""Verify every module JSON declared by a dist/json/<pkg> manifest.json is
tracked in git, and (WARN-only) flag tracked module JSONs no manifest declares.

Background (#541): during #416, two independent .gitignore patterns silently
untracked the new hydra.build.* family's kernel JSON while the files remained
present on disk in every worktree. Every runtime test suite passed (the files
were there locally); only a fresh clone would have failed to bootstrap. This
check catches that class of defect directly: declared-but-untracked is a hard
failure, independent of whether the file happens to exist on the machine
running the check.

Path resolution (matches hydra.codegen.moduleNameToPath, the single DSL-defined
source of truth generated into every host — see
packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Generation.hs):
  namespace "a.b.c" -> "a/b/c" (dot-to-slash, no stripping) -> "<dir>/a/b/c.json"

Manifest module-list keys, all resolved under <pkg>/src/main/json/ unless noted:
  mainModules       -> the namespace's own JSON file (literal path)
  testModules       -> same namespaces, but under <pkg>/src/test/json/ instead
                       (declared inside the src/main/json/manifest.json -- there
                       is no separate src/test/json/manifest.json; see
                       heads/python/src/main/python/hydra/bootstrap.py:362-372)
  mainDslModules
      -> each entry is a SOURCE namespace; the generated file is the DSL WRAPPER,
         at a DERIVED namespace, per Hydra.Sources.Kernel.Terms.Dsls.dslModuleName:
           hydra.X.Y   -> hydra.dsl.X.Y   (strip leading "hydra.", insert "dsl.")
           other.X.Y   -> hydra.dsl.other.X.Y  (non-hydra.* namespaces keep full path)
  dslModules
      -> a distinct LEGACY key (predates the #474 mainDslModules migration,
         kept for backward-compat) that stores ALREADY-DERIVED hydra.dsl.*
         names directly -- resolved as a literal path, no re-derivation.
  mainEncodingModules
      -> each entry is a SOURCE namespace; TWO generated wrapper files exist, per
         Hydra.Sources.Kernel.Terms.{Encoding.encodeModuleName,Decoding.decodeModuleName}:
           hydra.X.Y -> hydra.encode.X.Y  (drop first segment, insert "encode")
           hydra.X.Y -> hydra.decode.X.Y  (drop first segment, insert "decode")
These derivation functions are DSL-defined once and generated identically into
every host; this script reimplements them in Python to check the committed
dist/json tree without invoking any host toolchain.

Existence is NOT required for derived DSL/encoding wrapper paths: the real
generator (Hydra.Generation.writeDslJson et al., heads/haskell/.../Generation.hs)
skips writing a wrapper whose generated module has zero definitions -- e.g. a
type module made only of function-typed fields (not encodable) or forall-only
phantom-type definitions produces an empty encoder/DSL module and is silently
omitted. Replicating that emptiness predicate in Python would mean
reimplementing the encoder/DSL generator itself, so this script only enforces
trackedness for derived paths that DO exist on disk -- a derived wrapper that
exists locally but is untracked is still exactly the #416 failure mode (checked
out of a fresh clone, the file is gone). Only mainModules/testModules entries
(literal, unconditional, one file per declared namespace) are required to
exist at all.

Usage:
  bin/check-manifest-json-tracked.py [--root DIR]

Exit code is nonzero iff any mainModules/testModules entry is missing or
untracked, or any derived DSL/encoding wrapper that exists on disk is
untracked. Orphan JSONs (tracked but undeclared) are reported but do not
affect the exit code.
"""
from __future__ import annotations

import argparse
import json
import subprocess
import sys
from pathlib import Path

LITERAL_KEY = "mainModules"
TEST_KEY = "testModules"
# mainDslModules lists SOURCE namespaces needing derivation (dslModuleName).
# dslModules is a distinct legacy key (kept for backward-compat through the
# #474 manifest migration, see Generation.hs) that stores ALREADY-DERIVED
# hydra.dsl.* names directly -- resolve those as literal paths, not through
# dslModuleName again.
DSL_SOURCE_KEY = "mainDslModules"
DSL_LITERAL_KEY = "dslModules"
ENCODING_KEY = "mainEncodingModules"


def namespace_to_relpath(ns: str) -> str:
    return ns.replace(".", "/") + ".json"


def dsl_module_name(ns: str) -> str:
    """Mirrors Hydra.Sources.Kernel.Terms.Dsls.dslModuleName."""
    parts = ns.split(".")
    if parts and parts[0] == "hydra":
        return "hydra.dsl." + ".".join(parts[1:])
    return "hydra.dsl." + ns


def encode_module_name(ns: str) -> str:
    """Mirrors Hydra.Sources.Kernel.Terms.Encoding.encodeModuleName."""
    parts = ns.split(".")
    return "hydra.encode." + ".".join(parts[1:])


def decode_module_name(ns: str) -> str:
    """Mirrors Hydra.Sources.Kernel.Terms.Decoding.decodeModuleName."""
    parts = ns.split(".")
    return "hydra.decode." + ".".join(parts[1:])


def is_tracked(repo_root: Path, path: Path) -> bool:
    result = subprocess.run(
        ["git", "ls-files", "--error-unmatch", str(path.relative_to(repo_root))],
        cwd=repo_root, capture_output=True, text=True,
    )
    return result.returncode == 0


def list_tracked_json(repo_root: Path, json_dir: Path) -> set[Path]:
    if not json_dir.is_dir():
        return set()
    result = subprocess.run(
        ["git", "ls-files", "--", str(json_dir.relative_to(repo_root))],
        cwd=repo_root, capture_output=True, text=True, check=True,
    )
    return {repo_root / line for line in result.stdout.splitlines() if line.endswith(".json")}


def check_manifest(repo_root: Path, manifest_path: Path) -> tuple[list[str], set[Path]]:
    """Returns (failure messages, set of declared absolute JSON paths)."""
    pkg_main_dir = manifest_path.parent  # .../<pkg>/src/main/json
    pkg_root = pkg_main_dir.parent.parent.parent  # .../<pkg>
    test_dir = pkg_root / "src" / "test" / "json"

    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    pkg_name = manifest.get("package", pkg_root.name)

    failures = []
    declared: set[Path] = set()

    def check_one(ns: str, base_dir: Path, field_name: str, require_exists: bool):
        rel = namespace_to_relpath(ns)
        full = base_dir / rel
        if not full.is_file():
            if require_exists:
                declared.add(full)
                failures.append(f"{pkg_name}: {field_name} declares '{ns}' but "
                                 f"{full.relative_to(repo_root)} does not exist on disk")
            return
        declared.add(full)
        if not is_tracked(repo_root, full):
            failures.append(f"{pkg_name}: {field_name} declares '{ns}' but "
                             f"{full.relative_to(repo_root)} is NOT TRACKED by git")

    for ns in manifest.get(LITERAL_KEY, []):
        check_one(ns, pkg_main_dir, LITERAL_KEY, require_exists=True)
    for ns in manifest.get(TEST_KEY, []):
        check_one(ns, test_dir, TEST_KEY, require_exists=True)
    for ns in manifest.get(DSL_SOURCE_KEY, []):
        check_one(dsl_module_name(ns), pkg_main_dir, DSL_SOURCE_KEY, require_exists=False)
    for ns in manifest.get(DSL_LITERAL_KEY, []):
        check_one(ns, pkg_main_dir, DSL_LITERAL_KEY, require_exists=False)
    for ns in manifest.get(ENCODING_KEY, []):
        check_one(encode_module_name(ns), pkg_main_dir, ENCODING_KEY, require_exists=False)
        check_one(decode_module_name(ns), pkg_main_dir, ENCODING_KEY, require_exists=False)

    return failures, declared


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    here = Path(__file__).resolve().parent.parent
    ap.add_argument("--root", default=str(here), help="Repository root (default: repo containing this script)")
    args = ap.parse_args()

    repo_root = Path(args.root).resolve()
    manifests = sorted((repo_root / "dist" / "json").glob("*/src/main/json/manifest.json"))

    if not manifests:
        print(f"No manifest.json files found under {repo_root / 'dist' / 'json'}", file=sys.stderr)
        sys.exit(1)

    all_failures = []
    all_declared: set[Path] = set()
    all_tracked: set[Path] = set()

    for manifest_path in manifests:
        failures, declared = check_manifest(repo_root, manifest_path)
        all_failures.extend(failures)
        all_declared |= declared
        pkg_main_dir = manifest_path.parent
        pkg_root = pkg_main_dir.parent.parent.parent
        all_tracked |= list_tracked_json(repo_root, pkg_main_dir)
        all_tracked |= list_tracked_json(repo_root, pkg_root / "src" / "test" / "json")

    # manifest.json itself is tracked infrastructure, not a declared module -- exclude.
    all_tracked = {p for p in all_tracked if p.name != "manifest.json"}
    orphans = sorted(all_tracked - all_declared)

    print(f"Checked {len(manifests)} manifest.json files under {repo_root / 'dist' / 'json'}")
    print()

    if all_failures:
        print(f"FAIL: {len(all_failures)} manifest-declared module(s) missing or untracked:")
        for msg in all_failures:
            print(f"  {msg}")
        print()
    else:
        print("OK: every manifest-declared module JSON is present and tracked.")
        print()

    if orphans:
        print(f"WARN: {len(orphans)} tracked module JSON(s) not declared by any manifest (orphans; #405 owns cleanup):")
        for p in orphans:
            print(f"  {p.relative_to(repo_root)}")
        print()

    sys.exit(1 if all_failures else 0)


if __name__ == "__main__":
    main()
