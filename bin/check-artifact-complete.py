#!/usr/bin/env python3
"""Verify that a PACKAGED ARTIFACT (a built sdist .tar.gz or a .jar) actually
contains a representation of every module its dist/json manifest declares in
mainModules -- inspecting the archive that gets UPLOADED, not the generated
dist/ tree it was (supposedly) built from.

Background (0.17.2 double defect). The 0.17.2 release shipped TWO truncated
artifacts because the publishing machine's local dist/ tree was stale, and no
existing gate inspects the packaged archive -- every check (/sync, /test,
/bootstrap, and the CI guard check-emitted-modules-complete.py) validates the
generated dist/ TREE, never the built archive:

  1. The hydra-build sdist exposed 3 of its 8 declared modules -- it carried
     only Hydra.Build.{Modules,Reconcile,Routing}, missing Hydra.Build.{Format,
     Libraries,ManifestWriter} and Hydra.{Decode,Encode}.Build.Format. The
     manifest was complete; the archive was not.
  2. The published Java hydra-kernel-0.17.2.jar carried PRE-#417 class names
     (hydra/overlay/java/lib/strings/{Cat,Cat2,Intercalate}.class) and was
     MISSING the #417-renamed classes (strings/Concat, strings/Join,
     dsl/lib/Ordering, optionals/WithDefault). The dist/ tree on a fresh clone
     was correct; the jar built on the stale machine was a whole release behind.

check-emitted-modules-complete.py closes the gap for the dist/ TREE. THIS script
closes it for the ARCHIVE: given a manifest and a built artifact, it opens the
archive (WITHOUT extracting to disk -- membership is read from the tar/zip index
in-memory), and asserts each manifest mainModules namespace is represented inside
the archive. A declared namespace with no member in the archive is a hard failure
-- exactly the shape of both 0.17.2 defects, and it would have blocked that
release at publish time.

Two artifact kinds, two path derivations:

  * Haskell sdist (.tar.gz). One generated .hs per declared namespace. Namespace
    "hydra.build.manifestWriter" -> a member whose path ends with
    "src/main/haskell/Hydra/Build/ManifestWriter.hs". Segments are PascalCased
    via first-char-upper / rest-preserved (camelCase kept: manifestWriter ->
    ManifestWriter, NOT Manifestwriter), mirroring the Haskell coder and
    check-emitted-modules-complete.py. The sdist has a top-level
    "hydra-<version>/" prefix; we match on the path SUFFIX and never hardcode the
    prefix.

  * Java jar (.jar). A namespace does NOT map 1:1 to a class -- Java emits one
    .class per TYPE, many per module, under hydra/<segments>/. So for a namespace
    we assert the archive contains AT LEAST ONE .class whose path places it in
    that namespace's directory: either hydra/<segs>/<Anything>.class (the
    namespace is a package directory) OR hydra/<parent-segs>/<Leaf><...>.class
    (the leaf segment is a class-name prefix, e.g. hydra.build.format ->
    hydra/build/Format*.class). Segments are lowercased as-is for the directory.
    This is lenient on the exact class name but strict that the namespace's
    directory is represented.

    The jar also supports a REQUIRED-SYMBOLS mode (--required-symbols), which is
    what catches the #417 Cat-not-Concat defect: the manifest alone cannot encode
    post-#417 class NAMES (a module truncation and a stale-rename look identical
    at the namespace-directory level -- the stale jar still had a strings/
    directory, just with the old Cat/Cat2/Intercalate classes). The
    required-symbols file is a JSON object {namespace: [required-class-basenames]}
    asserting specific class basenames (e.g. Concat, Join, Ordering, WithDefault)
    exist as <basename>.class somewhere in that namespace's directory. A required
    basename with no matching .class member is a hard failure naming it.

Usage:
  check-artifact-complete.py --manifest <manifest.json> --artifact <archive>
      [--kind {haskell-sdist,java-jar}]        (inferred from extension if omitted)
      [--required-symbols <symbols.json>]      (java-jar only; {ns: [basenames]})

Exit code is 1 iff any manifest mainModules namespace (or any required symbol)
has no representation in the archive; 0 if complete.
"""
from __future__ import annotations

import argparse
import json
import sys
import tarfile
import zipfile
from pathlib import Path


def seg_pascal(seg: str) -> str:
    """First char upper, rest preserved (camelCase kept). manifestWriter -> ManifestWriter.

    Mirrors check-emitted-modules-complete.py.seg_pascal and the Haskell coder;
    NOT str.capitalize(), which would lower-case the tail (-> Manifestwriter).
    """
    return seg[:1].upper() + seg[1:] if seg else seg


def namespace_to_haskell_suffix(ns: str) -> str:
    """hydra.build.manifestWriter -> src/main/haskell/Hydra/Build/ManifestWriter.hs

    Returned as a path SUFFIX (no leading archive prefix) so it matches regardless
    of the sdist's top-level hydra-<version>/ directory.
    """
    return "src/main/haskell/" + "/".join(seg_pascal(p) for p in ns.split(".")) + ".hs"


def namespace_to_java_dir(ns: str) -> str:
    """hydra.build.format -> hydra/build/format ; hydra.typeScript.coder -> hydra/typeScript/coder

    Java package segments preserve the namespace's OWN casing verbatim — a
    camelCase segment like 'typeScript', 'pathAlgebra', or 'openGql' becomes a
    same-cased package directory in the jar (hydra/typeScript/...). Do NOT
    lower-case: an earlier version did, which made the gate miss real classes
    under hydra/typeScript/ and mis-locate leaf classes like
    hydra/build/ManifestWriter.class (from the 'manifestWriter' leaf). The
    directory match here is case-sensitive against the jar's actual paths; the
    leaf-as-class-prefix form in _class_members_in_namespace handles the leaf
    being a class file rather than a subdirectory.
    """
    return "/".join(ns.split("."))


def _camel_to_lower_snake(seg: str) -> str:
    """Mirror hydra.formatting.convert_case CAMEL -> LOWER_SNAKE for one segment.

    The Python coder names each module file by snake-casing its namespace segment
    (dist/python .../hydra/python/names.py: each DottedName part is CAMEL ->
    LOWER_SNAKE). The kernel algorithm: decapitalize, then start a new word at each
    uppercase char, lowercase each word, join with '_'. So 'manifestWriter' ->
    'manifest_writer', 'openCypher' -> 'open_cypher', 'termsToElements' ->
    'terms_to_elements', 'openGql' -> 'open_gql', 'pathAlgebra' -> 'path_algebra'.
    An all-lowercase segment ('format', 'build') is unchanged. Matching the coder
    here (rather than a naive lower()) is what keeps the gate from raising false
    MISSes on camelCase namespaces — the modules ARE present, just snake-cased.
    """
    if not seg:
        return seg
    # decapitalize: first char to lower (the coder's `decapitalize`)
    s = seg[0].lower() + seg[1:]
    out = []
    for c in s:
        if c.isupper():
            out.append("_")
            out.append(c.lower())
        else:
            out.append(c)
    return "".join(out)


def namespace_to_python_suffix(ns: str) -> str:
    """hydra.build.manifestWriter -> hydra/build/manifest_writer.py

    One generated .py per declared namespace in the wheel/sdist, with each segment
    snake-cased exactly as the Python coder does (see _camel_to_lower_snake).
    Matched as a path SUFFIX so the wheel's package root or the sdist's
    <pkg>-<version>/... prefix doesn't matter.
    """
    return "/".join(_camel_to_lower_snake(p) for p in ns.split(".")) + ".py"


def namespace_to_npm_suffix(ns: str) -> str:
    """hydra.build.format -> hydra/build/format.js

    npm tarballs prefix everything with package/ and TS compiles to dist/, so we
    match the trailing hydra/<segs>.js only. Segments lowercased-as-is; only the
    leaf's first character may be recased by the TS coder, so match case-insensitively
    at the caller (see check_suffix_ci).
    """
    return "/".join(p for p in ns.split(".")) + ".js"


def list_archive_members(artifact: Path, kind: str) -> list[str]:
    """Return archive member paths WITHOUT extracting to disk.

    tarfile/zipfile both read the central index in-memory; no member content is
    read and nothing is written to disk.
    """
    # zip-based artifacts: .jar (java + scala) and .whl
    if kind in ("java-jar", "scala-jar", "python-wheel"):
        with zipfile.ZipFile(artifact, mode="r") as zf:
            return zf.namelist()
    # tar-based artifacts: haskell sdist, python sdist, npm tarball
    if kind in ("haskell-sdist", "python-sdist", "npm-tgz"):
        with tarfile.open(artifact, mode="r:*") as tf:
            return tf.getnames()
    raise ValueError(f"unknown kind: {kind}")


def infer_kind(artifact: Path) -> str:
    name = artifact.name.lower()
    if name.endswith(".jar"):
        return "java-jar"
    if name.endswith(".whl"):
        return "python-wheel"
    # .tar.gz / .tgz / .tar default to the haskell sdist (the original artifact
    # kind). python-sdist and npm-tgz ALSO use these extensions, so a caller
    # verifying those MUST pass --kind explicitly — every publish script does.
    if name.endswith(".tar.gz") or name.endswith(".tgz") or name.endswith(".tar"):
        return "haskell-sdist"
    raise ValueError(
        f"cannot infer artifact kind from '{artifact.name}'; pass --kind explicitly "
        f"(haskell-sdist / python-wheel / python-sdist / npm-tgz / java-jar)"
    )


def check_haskell_sdist(members: list[str], main_modules: list[str]) -> list[str]:
    """One .hs per declared namespace must appear as a member-path suffix."""
    errors: list[str] = []
    for ns in main_modules:
        suffix = namespace_to_haskell_suffix(ns)
        if any(m.endswith(suffix) for m in members):
            print(f"  OK   {ns} -> */{suffix}")
        else:
            errors.append(
                f"manifest mainModules declares '{ns}' but no member ending in "
                f"'{suffix}' is present in the sdist"
            )
            print(f"  MISS {ns} -> (no */{suffix})")
    return errors


def check_python(members: list[str], main_modules: list[str], label: str) -> list[str]:
    """One .py per declared namespace must appear as a member-path suffix.

    Same shape as the Haskell sdist check (one generated source file per namespace)
    but with the Python path derivation (lowercase segments, .py). Covers both the
    wheel (.whl zip) and the sdist (.tar.gz) — the member set is what differs, not
    the derivation.
    """
    errors: list[str] = []
    for ns in main_modules:
        suffix = namespace_to_python_suffix(ns)
        if any(m.endswith(suffix) for m in members):
            print(f"  OK   {ns} -> */{suffix}")
        else:
            errors.append(
                f"manifest mainModules declares '{ns}' but no member ending in "
                f"'{suffix}' is present in the {label}"
            )
            print(f"  MISS {ns} -> (no */{suffix})")
    return errors


def _npm_candidate_suffixes(ns: str) -> list[str]:
    """Member-path suffixes that satisfy a declared namespace in an npm tarball.

    Normally one compiled .js at hydra/<segs>.js. But the translingual primitive
    libraries (hydra.lib.*) are provided host-natively: in the TypeScript host they
    ship as OVERLAY modules at hydra/overlay/typescript/lib/<leaf>.js, not at
    hydra/lib/<leaf>.js (only hydra.lib.defaults is a generated hydra/lib/ file).
    So for a hydra.lib.X namespace, accept EITHER the direct path or the TS overlay
    path — the primitive is present either way. This mirrors the Java jar, where
    hydra.lib.strings is satisfied by hydra/lib/Strings.class AND/OR the
    hydra/overlay/java/lib/strings/ package.
    """
    direct = namespace_to_npm_suffix(ns)  # hydra/<segs>.js
    cands = [direct]
    if ns.startswith("hydra.lib."):
        leaf = ns.split(".")[-1]
        cands.append(f"hydra/overlay/typescript/lib/{leaf}.js")
    return cands


def check_npm(members: list[str], main_modules: list[str]) -> list[str]:
    """One compiled .js per declared namespace must appear as a member-path suffix.

    npm tarballs prefix with package/ and TS emits to dist/, so we match the
    trailing hydra/<segs>.js. The TS coder may recase the leaf file, so match the
    suffix case-insensitively. hydra.lib.* primitives are accepted at their TS
    overlay path too (see _npm_candidate_suffixes).
    """
    errors: list[str] = []
    lowered = [m.lower() for m in members]
    for ns in main_modules:
        cands = _npm_candidate_suffixes(ns)
        matched = next(
            (c for c in cands if any(m.endswith(c.lower()) for m in lowered)),
            None,
        )
        if matched is not None:
            print(f"  OK   {ns} -> */{matched}")
        else:
            errors.append(
                f"manifest mainModules declares '{ns}' but no member ending in "
                f"any of {cands} is present in the npm tarball"
            )
            print(f"  MISS {ns} -> (no {' , '.join('*/'+c for c in cands)})")
    return errors


def _class_members_in_namespace(members: list[str], java_dir: str) -> list[str]:
    """.class members that place a type in namespace directory `java_dir`.

    Accepts either the namespace as a package directory (hydra/<dir>/X.class) or
    the leaf segment as a class-name prefix in the parent directory
    (hydra/<parent>/Leaf*.class).
    """
    hits = []
    dir_prefix = f"{java_dir}/"
    # Leaf-as-class-prefix form: parent dir + PascalCased leaf as filename prefix.
    segs = java_dir.split("/")
    parent = "/".join(segs[:-1])
    leaf_pascal = seg_pascal(segs[-1])
    leaf_prefix = (f"{parent}/" if parent else "") + leaf_pascal
    for m in members:
        if not m.endswith(".class"):
            continue
        if m.startswith(dir_prefix):
            hits.append(m)
        elif m.startswith(leaf_prefix):
            # e.g. hydra/build/Format.class or hydra/build/Format$Inner.class
            hits.append(m)
    return hits


def check_java_jar(
    members: list[str],
    main_modules: list[str],
    required_symbols: dict[str, list[str]] | None,
) -> list[str]:
    """Each namespace's directory must contain >=1 .class; required symbols by name."""
    errors: list[str] = []
    for ns in main_modules:
        java_dir = namespace_to_java_dir(ns)
        hits = _class_members_in_namespace(members, java_dir)
        if hits:
            print(f"  OK   {ns} -> {len(hits)} class(es) under {java_dir}/")
        else:
            errors.append(
                f"manifest mainModules declares '{ns}' but the jar has no .class "
                f"under 'hydra' directory '{java_dir}/' (nor a '{java_dir}*.class' leaf)"
            )
            print(f"  MISS {ns} -> (no .class under {java_dir}/)")

    if required_symbols:
        # A basename is satisfied by any member <basename>.class (or
        # <basename>$Inner.class) inside the namespace's directory subtree. This
        # is what distinguishes a correct jar from the stale-rename #417 defect,
        # where the namespace directory existed but held pre-rename class names.
        present_basenames = {
            Path(m).name.split("$", 1)[0][: -len(".class")]
            for m in members
            if m.endswith(".class")
        }
        for ns, basenames in sorted(required_symbols.items()):
            # Keys beginning with '_' are documentation (JSON has no comments),
            # e.g. a top-level "__doc__" describing the file — skip them.
            if ns.startswith("_"):
                continue
            for basename in basenames:
                if basename in present_basenames:
                    print(f"  OK   required symbol {ns}::{basename}")
                else:
                    errors.append(
                        f"required symbol '{basename}' for namespace '{ns}' is "
                        f"missing: no '{basename}.class' member in the jar "
                        f"(the #417 stale-rename defect shape)"
                    )
                    print(f"  MISS required symbol {ns}::{basename}")
    return errors


def check_scala_jar(members: list[str], main_modules: list[str]) -> list[str]:
    """Each declared namespace must have >=1 .class under its hydra/<dir>/ directory.

    Scala maps every namespace to a package directory of .class files, exactly like
    Java, so the directory-presence check is shared with check_java_jar. The Scala
    specifics the check tolerates automatically: companion objects (X$.class),
    Scala-3 top-level-definition holders (hydra/lib/strings/strings$package.class),
    and .tasty sidecars (ignored — only .class counts). No leaf-as-class-prefix case
    (Scala never hoists the leaf to a PascalCase class in the parent dir) and no
    #417 required-symbols machinery (that is Java-host-specific). hydra.lib.* land
    directly at hydra/lib/<leaf>/ here (NOT an overlay path as in the TS host).
    """
    errors: list[str] = []
    for ns in main_modules:
        java_dir = namespace_to_java_dir(ns)  # dotted -> slashed, casing preserved
        hits = [
            m for m in members
            if m.endswith(".class") and m.startswith(f"{java_dir}/")
        ]
        if hits:
            print(f"  OK   {ns} -> {len(hits)} class(es) under {java_dir}/")
        else:
            errors.append(
                f"manifest mainModules declares '{ns}' but the scala jar has no "
                f".class under directory '{java_dir}/'"
            )
            print(f"  MISS {ns} -> (no .class under {java_dir}/)")
    return errors


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    ap.add_argument("--manifest", required=True, help="dist/json/<pkg>/src/main/json/manifest.json")
    ap.add_argument("--artifact", required=True, help="built .tar.gz sdist or .jar")
    ap.add_argument(
        "--kind",
        choices=["haskell-sdist", "java-jar", "scala-jar", "python-wheel", "python-sdist", "npm-tgz"],
        default=None,
        help="artifact kind; inferred from extension for .jar/.whl, else required "
        "(.tar.gz/.tgz are ambiguous across haskell/python/npm)",
    )
    ap.add_argument(
        "--required-symbols",
        default=None,
        help="java-jar only: JSON {namespace: [required-class-basenames]} "
        "asserting post-#417 class names exist (catches the stale-rename defect)",
    )
    args = ap.parse_args()

    manifest_path = Path(args.manifest)
    artifact = Path(args.artifact)

    if not manifest_path.is_file():
        print(f"ERROR: manifest not found: {manifest_path}", file=sys.stderr)
        return 2
    if not artifact.is_file():
        print(f"ERROR: artifact not found: {artifact}", file=sys.stderr)
        return 2

    kind = args.kind or infer_kind(artifact)

    if args.required_symbols and kind != "java-jar":
        print("ERROR: --required-symbols is only valid with --kind java-jar", file=sys.stderr)
        return 2

    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    pkg = manifest.get("package", manifest_path.parent.parent.parent.parent.name)
    main_modules = manifest.get("mainModules") or []

    required_symbols: dict[str, list[str]] | None = None
    if args.required_symbols:
        required_symbols = json.loads(Path(args.required_symbols).read_text(encoding="utf-8"))

    members = list_archive_members(artifact, kind)

    print(f"=== Checking {kind} artifact {artifact.name} against {pkg} manifest ===")
    print(f"    {len(main_modules)} declared mainModules, {len(members)} archive members")

    if not main_modules and not required_symbols:
        print(f"  -- {pkg}: no mainModules declared and no required symbols; nothing to check")
        return 0

    if kind == "haskell-sdist":
        errors = check_haskell_sdist(members, main_modules)
    elif kind == "python-wheel":
        errors = check_python(members, main_modules, "wheel")
    elif kind == "python-sdist":
        errors = check_python(members, main_modules, "sdist")
    elif kind == "npm-tgz":
        errors = check_npm(members, main_modules)
    elif kind == "scala-jar":
        errors = check_scala_jar(members, main_modules)
    else:  # java-jar
        errors = check_java_jar(members, main_modules, required_symbols)

    if errors:
        print(
            f"\nFAIL: {len(errors)} declared module(s)/symbol(s) missing from the "
            f"{kind} artifact:",
            file=sys.stderr,
        )
        for e in errors:
            print(f"  {pkg}: {e}", file=sys.stderr)
        print(
            "\nThis is the 0.17.2 shape: the manifest declares content that never "
            "reached the UPLOADED archive (stale local dist/ at publish time). "
            "Re-assemble the package from a clean tree and rebuild the artifact.",
            file=sys.stderr,
        )
        return 1

    print(f"\nOK: every declared module (and required symbol) is present in the {kind} artifact.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
