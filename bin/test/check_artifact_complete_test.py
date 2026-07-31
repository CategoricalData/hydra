#!/usr/bin/env python3
"""Regression tests for bin/check-artifact-complete.py.

These reproduce the two 0.17.2 publish defects as synthetic archives and assert
the artifact-completeness gate FAILS on each, plus a good archive that passes:

  1. GOOD haskell-sdist       -> all declared modules present     -> exit 0
  2. hydra-build 3/8 sdist    -> 5 declared modules truncated     -> exit 1 (names the 5)
  3. Java Cat-not-Concat jar  -> pre-#417 class names only        -> exit 1 (names the #417 symbols)

Fixture archives are built programmatically (tarfile/zipfile writing tiny dummy
members) in a temp dir -- no binary artifacts are committed. The gate is invoked
as a subprocess so the test exercises the real CLI (argv, exit codes, stderr),
which is how the publish path will call it.

Run:  python3 bin/test/check_artifact_complete_test.py
Exit code is 0 iff all three cases behave as expected.
"""
from __future__ import annotations

import io
import json
import subprocess
import sys
import tarfile
import tempfile
import zipfile
from pathlib import Path

SCRIPT = Path(__file__).resolve().parent.parent / "check-artifact-complete.py"

# The real hydra-build manifest mainModules (the package the 0.17.2 sdist truncated).
HYDRA_BUILD_MAIN_MODULES = [
    "hydra.build.format",
    "hydra.build.libraries",
    "hydra.build.manifestWriter",
    "hydra.build.modules",
    "hydra.build.reconcile",
    "hydra.build.routing",
    "hydra.decode.build.format",
    "hydra.encode.build.format",
]


def write_manifest(path: Path, pkg: str, main_modules: list[str]) -> None:
    path.write_text(json.dumps({"package": pkg, "mainModules": main_modules}), encoding="utf-8")


def make_tar_gz(path: Path, member_suffixes: list[str], top_prefix: str) -> None:
    """Write a .tar.gz with a top-level <top_prefix>/ dir containing tiny dummy members."""
    with tarfile.open(path, mode="w:gz") as tf:
        for suffix in member_suffixes:
            data = b"-- dummy generated source\n"
            info = tarfile.TarInfo(name=f"{top_prefix}/{suffix}")
            info.size = len(data)
            tf.addfile(info, io.BytesIO(data))


def make_jar(path: Path, class_members: list[str]) -> None:
    """Write a .jar (zip) containing tiny dummy .class members."""
    with zipfile.ZipFile(path, mode="w") as zf:
        for member in class_members:
            zf.writestr(member, b"\xca\xfe\xba\xbe")  # dummy class magic


def run_check(*args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(SCRIPT), *args], capture_output=True, text=True
    )


def haskell_suffix(ns: str) -> str:
    """Mirror the script: hydra.build.format -> src/main/haskell/Hydra/Build/Format.hs"""
    segs = [s[:1].upper() + s[1:] for s in ns.split(".")]
    return "src/main/haskell/" + "/".join(segs) + ".hs"


FAILURES: list[str] = []


def expect(cond: bool, msg: str) -> None:
    status = "PASS" if cond else "FAIL"
    print(f"  [{status}] {msg}")
    if not cond:
        FAILURES.append(msg)


def case_good_sdist(tmp: Path) -> None:
    print("Case 1: GOOD haskell-sdist (all modules present) -> exit 0")
    manifest = tmp / "good-manifest.json"
    write_manifest(manifest, "hydra-build", HYDRA_BUILD_MAIN_MODULES)
    artifact = tmp / "hydra-build-0.17.2.tar.gz"
    make_tar_gz(
        artifact,
        [haskell_suffix(ns) for ns in HYDRA_BUILD_MAIN_MODULES],
        top_prefix="hydra-build-0.17.2",
    )
    r = run_check("--manifest", str(manifest), "--artifact", str(artifact))
    expect(r.returncode == 0, f"exit 0 (got {r.returncode})")
    expect("every declared module" in r.stdout, "reports completeness on stdout")
    if r.returncode != 0:
        print(r.stdout, r.stderr)


def case_hydra_build_3of8(tmp: Path) -> None:
    print("Case 2: hydra-build 3/8 truncated sdist -> exit 1, names the 5 missing")
    manifest = tmp / "build-manifest.json"
    write_manifest(manifest, "hydra-build", HYDRA_BUILD_MAIN_MODULES)
    # The actual 0.17.2 defect: only Modules, Reconcile, Routing shipped.
    present = ["hydra.build.modules", "hydra.build.reconcile", "hydra.build.routing"]
    artifact = tmp / "hydra-build-truncated.tar.gz"
    make_tar_gz(
        artifact, [haskell_suffix(ns) for ns in present], top_prefix="hydra-build-0.17.2"
    )
    r = run_check("--manifest", str(manifest), "--artifact", str(artifact))
    expect(r.returncode == 1, f"exit 1 (got {r.returncode})")
    missing = [ns for ns in HYDRA_BUILD_MAIN_MODULES if ns not in present]
    for ns in missing:
        expect(ns in r.stderr, f"names missing module '{ns}'")
    expect(len(missing) == 5, f"exactly 5 modules missing (got {len(missing)})")


def case_java_cat_not_concat(tmp: Path) -> None:
    print("Case 3: Java Cat-not-Concat jar -> exit 1, names missing #417 symbols")
    # A minimal kernel-ish manifest whose namespaces the stale jar DOES cover at
    # the directory level (so only the required-symbols check catches the defect).
    manifest = tmp / "kernel-manifest.json"
    write_manifest(
        manifest,
        "hydra-kernel",
        ["hydra.overlay.java.lib.strings", "hydra.dsl.lib", "hydra.lib.optionals"],
    )
    # The stale 0.17.2 jar: pre-#417 class names present, #417-renamed ones absent.
    artifact = tmp / "hydra-kernel-0.17.2.jar"
    make_jar(
        artifact,
        [
            "hydra/overlay/java/lib/strings/Cat.class",
            "hydra/overlay/java/lib/strings/Cat2.class",
            "hydra/overlay/java/lib/strings/Intercalate.class",
            "hydra/lib/optionals/Map.class",
        ],
    )
    # Post-#417 required class basenames the correct jar must carry.
    required = tmp / "required-symbols.json"
    required.write_text(
        json.dumps(
            {
                "hydra.overlay.java.lib.strings": ["Concat", "Join"],
                "hydra.dsl.lib": ["Ordering"],
                "hydra.lib.optionals": ["WithDefault"],
            }
        ),
        encoding="utf-8",
    )
    r = run_check(
        "--manifest",
        str(manifest),
        "--artifact",
        str(artifact),
        "--kind",
        "java-jar",
        "--required-symbols",
        str(required),
    )
    expect(r.returncode == 1, f"exit 1 (got {r.returncode})")
    for sym in ["Concat", "Join", "Ordering", "WithDefault"]:
        expect(sym in r.stderr, f"names missing #417 symbol '{sym}'")


def make_wheel(path: Path, module_relpaths: list[str]) -> None:
    """A .whl is a zip; write hydra/<ns>.py members directly at the root."""
    with zipfile.ZipFile(path, mode="w") as zf:
        for rel in module_relpaths:
            zf.writestr(rel, "# dummy\n")


def make_npm_tgz(path: Path, module_relpaths: list[str]) -> None:
    """An npm tarball prefixes with package/ and TS emits to dist/."""
    with tarfile.open(path, mode="w:gz") as tf:
        for rel in module_relpaths:
            data = b"// dummy\n"
            info = tarfile.TarInfo(name=f"package/dist/{rel}")
            info.size = len(data)
            tf.addfile(info, io.BytesIO(data))


def _camel_to_lower_snake(seg: str) -> str:
    # Mirror the Python coder (hydra.formatting CAMEL -> LOWER_SNAKE): decapitalize,
    # start a new word at each uppercase char, lowercase, join with '_'. So the real
    # wheel names 'manifestWriter' as manifest_writer.py, NOT manifestwriter.py.
    if not seg:
        return seg
    s = seg[0].lower() + seg[1:]
    out = []
    for c in s:
        if c.isupper():
            out.append("_" + c.lower())
        else:
            out.append(c)
    return "".join(out)


def python_relpath(ns: str) -> str:
    # Snake-case each segment exactly as the coder does. Using the real transform
    # here makes case_python_good a regression test for the camelCase gate bug:
    # a wheel with manifest_writer.py must satisfy a manifest declaring manifestWriter.
    return "/".join(_camel_to_lower_snake(p) for p in ns.split(".")) + ".py"


def npm_relpath(ns: str) -> str:
    return "/".join(ns.split(".")) + ".js"


def case_python_truncated(tmp: Path) -> None:
    print("Case 4: truncated python-wheel -> exit 1, names missing module")
    manifest = tmp / "py-manifest.json"
    write_manifest(manifest, "hydra-build", HYDRA_BUILD_MAIN_MODULES)
    present = ["hydra.build.modules", "hydra.build.reconcile", "hydra.build.routing"]
    artifact = tmp / "hydra_build-0.17.3-py3-none-any.whl"
    make_wheel(artifact, [python_relpath(ns) for ns in present])
    r = run_check("--manifest", str(manifest), "--artifact", str(artifact), "--kind", "python-wheel")
    expect(r.returncode == 1, f"exit 1 (got {r.returncode})")
    expect("hydra.build.format" in r.stderr, "names missing module 'hydra.build.format'")


def case_python_good(tmp: Path) -> None:
    print("Case 5: complete python-wheel -> exit 0")
    manifest = tmp / "py-good-manifest.json"
    write_manifest(manifest, "hydra-build", HYDRA_BUILD_MAIN_MODULES)
    artifact = tmp / "hydra_build_good-0.17.3-py3-none-any.whl"
    make_wheel(artifact, [python_relpath(ns) for ns in HYDRA_BUILD_MAIN_MODULES])
    r = run_check("--manifest", str(manifest), "--artifact", str(artifact), "--kind", "python-wheel")
    expect(r.returncode == 0, f"exit 0 (got {r.returncode})")


def case_npm_truncated(tmp: Path) -> None:
    print("Case 6: truncated npm-tgz -> exit 1, names missing module")
    manifest = tmp / "npm-manifest.json"
    write_manifest(manifest, "hydra-build", HYDRA_BUILD_MAIN_MODULES)
    present = ["hydra.build.modules", "hydra.build.reconcile", "hydra.build.routing"]
    artifact = tmp / "hydra-build-0.17.3.tgz"
    make_npm_tgz(artifact, [npm_relpath(ns) for ns in present])
    r = run_check("--manifest", str(manifest), "--artifact", str(artifact), "--kind", "npm-tgz")
    expect(r.returncode == 1, f"exit 1 (got {r.returncode})")
    expect("hydra.build.format" in r.stderr, "names missing module 'hydra.build.format'")


def case_npm_lib_overlay(tmp: Path) -> None:
    print("Case 7: npm hydra.lib.* satisfied by TS overlay path -> exit 0")
    # The TS host ships hydra.lib.X primitives as overlay modules at
    # hydra/overlay/typescript/lib/<leaf>.js, NOT hydra/lib/<leaf>.js. The gate
    # must accept the overlay location, or every kernel npm tarball false-fails.
    manifest = tmp / "npm-lib-manifest.json"
    main_modules = ["hydra.lib.strings", "hydra.lib.math", "hydra.lib.defaults"]
    write_manifest(manifest, "hydra-kernel", main_modules)
    artifact = tmp / "hydra-kernel-0.17.3.tgz"
    # strings + math only at the overlay path; defaults at the direct hydra/lib/ path.
    members = [
        "hydra/overlay/typescript/lib/strings.js",
        "hydra/overlay/typescript/lib/math.js",
        "hydra/lib/defaults.js",
    ]
    make_npm_tgz(artifact, members)
    r = run_check("--manifest", str(manifest), "--artifact", str(artifact), "--kind", "npm-tgz")
    expect(r.returncode == 0, f"exit 0 (got {r.returncode})")


def main() -> int:
    with tempfile.TemporaryDirectory() as td:
        tmp = Path(td)
        case_good_sdist(tmp)
        case_hydra_build_3of8(tmp)
        case_java_cat_not_concat(tmp)
        case_python_truncated(tmp)
        case_python_good(tmp)
        case_npm_truncated(tmp)
        case_npm_lib_overlay(tmp)

    print()
    if FAILURES:
        print(f"TEST FAILED: {len(FAILURES)} assertion(s) failed")
        return 1
    print("ALL TESTS PASSED (7/7 cases)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
