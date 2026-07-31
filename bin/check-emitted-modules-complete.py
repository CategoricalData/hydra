#!/usr/bin/env python3
"""Verify every module a dist/json/<pkg> manifest.json declares in mainModules
is actually EMITTED as a generated source file in the assembled Haskell
distribution (dist/haskell/<pkg>).

Background (0.17.2 hydra-build defect): the published hydra-build sdist exposed
only 3 of its 8 declared modules (missing Hydra.Build.{Format,Libraries,
ManifestWriter} + Hydra.{Decode,Encode}.Build.Format). The manifest.json was
complete and every module JSON was present and tracked (so #541's
check-manifest-json-tracked.py passed), but the assembled distribution the sdist
was built from was stale/truncated — a build-time staleness that no existing
check caught, because they verify JSON trackedness and regeneration determinism,
not that the manifest's declared modules reach the emitted output.

This check closes that gap: it is the "did every declared module actually get
emitted?" gate. Run it against a freshly-assembled dist/haskell tree (CI does
this in the Haskell job after assembling packages). A manifest namespace with no
corresponding generated .hs file is a hard failure — exactly the shape of the
hydra-build defect, and it would have blocked that release.

Scope: mainModules for the Haskell target — one generated .hs per declared
namespace, required unconditionally. This is the exact set whose truncation
caused the hydra-build defect. Derived DSL/encode/decode wrappers are NOT
hard-checked here (the generator omits empty ones — see Generation.writeDslJson;
see check-manifest-json-tracked.py for the trackedness side): a whole-package
truncation still surfaces as a missing mainModules file.

Haskell path derivation mirrors hydra.codegen + the Haskell coder:
  namespace "a.b.cD" -> "A/B/CD.hs" — each dot-separated segment has its FIRST
  character upper-cased with the remainder preserved (camelCase kept, e.g.
  manifestWriter -> ManifestWriter), NOT Python str.capitalize() (which would
  lower-case the tail -> Manifestwriter).

Usage:
  check-emitted-modules-complete.py [--dist-json-root <dir>] [--dist-haskell-root <dir>]

Exit code is nonzero iff any package's mainModules namespace has no emitted
Haskell source file.
"""

import argparse
import json
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent


def seg_pascal(seg: str) -> str:
    """First char upper, rest preserved (camelCase kept). manifestWriter -> ManifestWriter."""
    return seg[:1].upper() + seg[1:] if seg else seg


def namespace_to_haskell_relpath(ns: str) -> str:
    """hydra.build.manifestWriter -> Hydra/Build/ManifestWriter.hs"""
    return "/".join(seg_pascal(p) for p in ns.split(".")) + ".hs"


def check_package(dist_haskell_root: Path, manifest_path: Path) -> list[str]:
    """Return a list of error strings for one package's manifest."""
    errors: list[str] = []
    manifest = json.loads(manifest_path.read_text())
    # dist/json/<pkg>/src/main/json/manifest.json -> <pkg>
    pkg = manifest_path.parent.parent.parent.parent.name
    hs_main = dist_haskell_root / pkg / "src" / "main" / "haskell"

    main_modules = manifest.get("mainModules") or []
    if not main_modules:
        return errors  # nothing declared (e.g. the hydra umbrella) -> nothing to check

    present = 0
    for ns in main_modules:
        rel = namespace_to_haskell_relpath(ns)
        if (hs_main / rel).is_file():
            present += 1
        else:
            errors.append(
                f"{pkg}: manifest mainModules declares '{ns}' but no emitted "
                f"Haskell source at {hs_main / rel}"
            )

    # Derived DSL / encode / decode wrappers: check only when present on disk
    # (the generator omits empty wrappers), mirroring check-manifest-json-tracked.py.
    # We don't hard-require these; they are informational and would show up as a
    # mainModules miss if the whole package were truncated.

    if not errors:
        print(f"  OK {pkg}: {present}/{len(main_modules)} mainModules emitted")
    return errors


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--dist-json-root", default=str(REPO_ROOT / "dist" / "json"))
    ap.add_argument("--dist-haskell-root", default=str(REPO_ROOT / "dist" / "haskell"))
    args = ap.parse_args()

    dist_json_root = Path(args.dist_json_root)
    dist_haskell_root = Path(args.dist_haskell_root)

    manifests = sorted(dist_json_root.glob("*/src/main/json/manifest.json"))
    if not manifests:
        print(f"ERROR: no manifests found under {dist_json_root}", file=sys.stderr)
        return 2

    # Packages the sync step is expected to assemble into dist/haskell (Phase 3:
    # hydra-kernel/hydra-build/hydra-pg/hydra-rdf). If one of these has NO
    # dist/haskell/<pkg> tree, that is itself a failure — otherwise the guard
    # would silently skip the very package (hydra-build) it exists to protect.
    # Other packages (coq/wasm/ext, coder packages) are checked when present but
    # not required, since a given CI job may not assemble them for Haskell.
    required = {"hydra-kernel", "hydra-build", "hydra-pg", "hydra-rdf"}

    print("=== Checking manifest mainModules are emitted into dist/haskell ===")
    all_errors: list[str] = []
    for mf in manifests:
        pkg = mf.parent.parent.parent.parent.name
        if not (dist_haskell_root / pkg).is_dir():
            if pkg in required:
                all_errors.append(
                    f"{pkg}: REQUIRED package has no assembled dist/haskell/{pkg} tree "
                    f"(expected from the sync step) — cannot verify its modules were emitted"
                )
            else:
                print(f"  -- {pkg}: not assembled for Haskell (skipped)")
            continue
        all_errors.extend(check_package(dist_haskell_root, mf))

    if all_errors:
        print("\nFAIL: manifest-declared modules missing from the emitted distribution:", file=sys.stderr)
        for e in all_errors:
            print(f"  {e}", file=sys.stderr)
        print(
            "\nThis is the 0.17.2 hydra-build defect shape: the manifest declares "
            "modules that never reached the assembled output. Re-assemble the "
            "package from a clean tree (bust the per-package digest) and re-check.",
            file=sys.stderr,
        )
        return 1

    print("\nOK: every package's manifest mainModules are all emitted into dist/haskell.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
