#!/usr/bin/env python3
"""Temporary characterization test for #416 Phase 2 pilot: compare the
generated hydra.build.routing module against the legacy _PACKAGE_PREFIXES
table in hydra.generation, over every namespace declared in the current
package manifests.

Not part of the permanent test suite. Run manually:
    uv run heads/python/bin/characterize-build-routing.py
"""

from __future__ import annotations

import glob
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]

# hydra.build.routing moved to the hydra-build package (#546); its dependencies
# (Dsls, Encoding, Decoding, Packaging, Errors, ...) remain in hydra-kernel. Both
# generated trees are needed on the path.
sys.path.insert(0, str(ROOT / "dist" / "python" / "hydra-kernel" / "src" / "main" / "python"))
sys.path.insert(0, str(ROOT / "dist" / "python" / "hydra-build" / "src" / "main" / "python"))
# The legacy _PACKAGE_PREFIXES table lives in the hand-written Python driver.
sys.path.insert(0, str(ROOT / "heads" / "python" / "src" / "main" / "python"))

import hydra.packaging
from hydra.build.routing import build_routing_map, namespace_to_package_in
from hydra.overlay.python.dsl.python import Left, Right
from hydra.generation import namespace_to_package as legacy_namespace_to_package

# Known, expected divergences between the legacy _PACKAGE_PREFIXES table and the
# manifest-derived generated module, confirmed 2026-07-02: the legacy table is
# missing prefix entries for these namespaces and silently falls back to
# "hydra-kernel", while the generated module correctly routes them per the
# manifest. Verified latent (not live): both native drivers are hard-scoped to
# their own package's sources, so these namespaces never actually reach
# namespace_to_package in production. New entries here should be scrutinized,
# not just added — they represent a real gap in the legacy table.
EXPECTED_DIVERGENCES = {
    "hydra.decode.neo4j.model": "hydra-pg",
    "hydra.encode.neo4j.model": "hydra-pg",
    "hydra.error.neo4j": "hydra-pg",
    "hydra.neo4j.model": "hydra-pg",
    "hydra.neo4j.pg": "hydra-pg",
    "hydra.validate.neo4j": "hydra-pg",
    "hydra.gradle": "hydra-java",
    "hydra.jvm.serde": "hydra-jvm",
    # #546: build modules extracted from hydra-kernel into hydra-build; the legacy
    # table has no hydra.build. prefix so it falls back to hydra-kernel, while the
    # manifest routes them to hydra-build.
    "hydra.build.modules": "hydra-build",
    "hydra.build.reconcile": "hydra-build",
    "hydra.build.routing": "hydra-build",
    "hydra.test.build.modules": "hydra-build",
    "hydra.test.build.reconcile": "hydra-build",
    "hydra.test.build.routing": "hydra-build",
}


def load_manifests():
    pkgs = []
    for path in sorted(glob.glob(str(ROOT / "dist" / "json" / "*" / "src" / "main" / "json" / "manifest.json"))):
        with open(path) as f:
            m = json.load(f)
        # derivedMainModules is not a manifest JSON field; derived names (hydra.dsl.*,
        # hydra.encode.*, hydra.decode.*, hydra.sources.*) are synthesized by
        # build_routing_map itself via derived_names, so mainModules ++ testModules is
        # the correct (and complete) declared-module input here.
        declared = list(dict.fromkeys(m["mainModules"] + m["testModules"]))
        pkgs.append((m["package"], [hydra.packaging.ModuleName(n) for n in declared]))
    return pkgs


def main():
    pkgs = load_manifests()
    all_namespaces = sorted({ns.value for _, mods in pkgs for ns in mods})
    print(f"Loaded {len(pkgs)} packages, {len(all_namespaces)} distinct declared namespaces.")

    rm = build_routing_map(pkgs)

    agree = 0
    expected_divergences_seen = []
    unexpected_disagreements = []
    fail_loud_hits = []
    for ns in all_namespaces:
        module_name = hydra.packaging.ModuleName(ns)
        legacy_pkg = legacy_namespace_to_package(module_name)
        generated = namespace_to_package_in(rm, module_name)
        if isinstance(generated, Right):
            generated_pkg = generated.value
            if generated_pkg == legacy_pkg:
                agree += 1
            elif EXPECTED_DIVERGENCES.get(ns) == generated_pkg:
                expected_divergences_seen.append((ns, legacy_pkg, generated_pkg))
            else:
                unexpected_disagreements.append((ns, legacy_pkg, generated_pkg))
        elif isinstance(generated, Left):
            # A declared namespace that the generated module fails loud on is always a
            # bug (every declared namespace must route somewhere) -- never expected.
            fail_loud_hits.append((ns, legacy_pkg))
        else:
            raise AssertionError(f"unexpected Either variant: {generated!r}")

    print(f"\nAgreement: {agree}/{len(all_namespaces)}")
    print(f"Expected divergences seen: {len(expected_divergences_seen)}/{len(EXPECTED_DIVERGENCES)}")
    for ns, legacy_pkg, generated_pkg in expected_divergences_seen:
        print(f"  {ns}: legacy={legacy_pkg} generated={generated_pkg} (expected)")

    missing_expected = sorted(set(EXPECTED_DIVERGENCES) - {ns for ns, _, _ in expected_divergences_seen})
    if missing_expected:
        print(f"\n{len(missing_expected)} previously-known divergences NOT reproduced this run "
              f"(legacy table may have been fixed, or namespace removed from manifests -- update "
              f"EXPECTED_DIVERGENCES if so):")
        for ns in missing_expected:
            print(f"  {ns}")

    ok = not unexpected_disagreements and not fail_loud_hits and not missing_expected

    if unexpected_disagreements:
        print(f"\n{len(unexpected_disagreements)} UNEXPECTED DISAGREEMENTS (new divergence, not in "
              f"EXPECTED_DIVERGENCES -- investigate before adding to the known set):")
        for ns, legacy_pkg, generated_pkg in unexpected_disagreements:
            print(f"  {ns}: legacy={legacy_pkg} generated={generated_pkg}")

    if fail_loud_hits:
        print(f"\n{len(fail_loud_hits)} namespaces where the generated module fails loud on a "
              f"DECLARED namespace -- always a bug, never expected:")
        for ns, legacy_pkg in fail_loud_hits:
            print(f"  {ns}: legacy fallback answer={legacy_pkg}")

    if ok:
        print("\nAll divergences from the legacy table are accounted for by the known set; "
              "no unexpected disagreements, no fail-loud hits on declared namespaces.")
    else:
        sys.exit(1)


if __name__ == "__main__":
    main()
