#!/usr/bin/env python3
"""Guard the "oil and water" invariant for the published-host drivers (#608, #617).

The invariant (docs/build-system.md, "The published host is oil and water with
the working tree"): a driver that LINKS a published Hydra package (from Hackage)
must NOT also source-dir the matching package's LOCAL HEAD authoring tree under
packages/<pkg>/src/main/haskell. The two are different kinds of the same thing —
published generated code vs. HEAD DSL source — and mixing them in one compile
breaks the moment HEAD adds a kernel type the published package lacks.

This script guards TWO related-but-distinct violations of that invariant, both
first-line, toolchain-free (python3 + git only) CI gates:

## Check 1: source-dir oil-and-water (#608)

Background (#608): the #376 cold-seeder (heads/haskell/json-driver) linked the
published hydra-kernel-0.17.1 while ALSO source-dirring
packages/hydra-kernel/src/main/haskell (for Hydra.Sources.All, its JSON
decode-universe context). That authoring tree contains Term-printers phantom-
annotated with generated kernel types (e.g. MissingCaseBranchesError, added on
main by #598) that do not exist in the published 0.17.1 kernel — so the driver
failed to compile ("Not in scope") on every new-kernel-type PR, reddening main.

What is and is NOT a violation:
  - VIOLATION: a heads/**/package.yaml lists `- hydra-<pkg>` under a
    `dependencies:` block AND has a `source-dirs:` entry pointing into
    `packages/hydra-<pkg>/src/main/haskell` (the full HEAD authoring tree,
    which hpack expands to include the Terms modules).
  - OK: source-dirring a BUILD-TIME COPY that lives under the driver's own
    directory (e.g. heads/haskell/json-driver/typesmods/, a Terms-free copy of
    just the DSL Types subtree). Such copies are not under packages/, so they are
    not flagged. The #608 fix relies on exactly this: it compiles only Terms-free
    Type DEFINITIONS (copied into typesmods/), never the Terms authoring tree.
  - OK: a driver that source-dirs packages/<pkg> but does NOT depend on the
    published hydra-<pkg> (a genuine local-host / --local-host build compiles the
    package from source and lists no published dep — the oil, no water).

## Check 2: cold-seeder unpublished Hydra.Build.* import (#617)

Background (#617, follow-up to #608): the cold-seeder's own generation modules
(heads/haskell/json-driver/app/ColdSeedMain.hs, plus the four "headmods" files
copied verbatim from heads/haskell/src/main/haskell/Hydra/ by
cold-seed-dist-haskell.sh: Generation.hs, PackageRouting.hs, TargetFilePaths.hs,
Digest.hs) compile against the PUBLISHED hydra-build (stack.yaml extra-dep,
pinned at hostVersion). If any of those files' import Hydra.Build.<Mod> where
<Mod> is not yet part of the published hydra-build, the cold-clone CI job fails
with "Could not find module" — invisible to local `stack test`, which builds
against a tree that already has the module. This reverted main TWICE:
  - #560: heads/**/PackageRouting.hs started importing Hydra.Build.Routing
    before hydra-build (with Routing) was published.
  - #607: Hydra/Generation.hs started importing Hydra.Build.ManifestWriter
    before hydra-build (with ManifestWriter) was published.

Ground truth for "is <Mod> published": since dist/haskell/hydra-build/ (the
compiled Hydra.Build.* Haskell sources) is entirely gitignored (#376 untrack),
there is no tracked file to check module presence against directly. Instead this
check uses `git merge-base HEAD origin/main` as a toolchain-free, no-network
proxy for "the last known-good state", and flags a Hydra.Build.<Mod> import in
the cold-seeder's closure if EITHER:
  (a) module-absent: dist/json/hydra-build/src/main/json/manifest.json's
      mainModules/testModules at the merge-base does not list the module
      (hydra.build.<mod>, camelCased from <Mod>), or
  (b) import-new: the module is listed at the merge-base, but the specific
      importing file's import list did NOT reference Hydra.Build.<Mod> at the
      merge-base (i.e. the import itself is new-on-branch, even though the
      target module predates the branch — this is exactly the #607 shape:
      ManifestWriter already existed in hydra-build's DSL source before this
      import was added to Generation.hs).
Known limitation (accepted — see #617 design discussion): this proxy can
false-negative if a module was added AND published in a single upstream cycle
between the merge-base and HEAD (a real Hackage release lookup would catch
that, at the cost of a network dependency this first-line lint intentionally
avoids). It can also, in principle, false-positive if hostVersion itself has
not advanced past the merge-base by the time this runs — accepted trade-off
for staying toolchain-free.

Usage:
  bin/check-oil-and-water.py [--root DIR] [--merge-base-ref REF]

Exit code is nonzero iff either check finds a violation.
"""
from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path

# A source-dirs entry that reaches into the HEAD authoring tree of some package.
# Matches ".../packages/<pkg>/src/main/haskell" anywhere in the (possibly
# relative, possibly ../-prefixed) path value.
PACKAGES_SRC_RE = re.compile(r"packages/(?P<pkg>[A-Za-z0-9_-]+)/src/main/haskell")


def parse_blocks(text: str) -> tuple[set[str], set[str]]:
    """Return (published hydra-* deps, packages/<pkg> source-dir'd packages).

    A hand-rolled scanner (no PyYAML, to keep this a toolchain-free CI gate).
    package.yaml here is regular: top-level `dependencies:` and per-executable
    `source-dirs:` blocks, each item on its own `  - value` line. We track which
    kind of list we are currently inside by the most recent `<key>:` header and
    stop collecting when indentation returns to a non-list line.
    """
    deps: set[str] = set()
    src_pkgs: set[str] = set()

    current: str | None = None  # "dependencies" | "source-dirs" | None
    for raw in text.splitlines():
        line = raw.rstrip("\n")
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue

        # A list item under the current block?
        m_item = re.match(r"^(\s*)-\s+(.*)$", line)
        if m_item and current is not None:
            value = m_item.group(2).strip().strip('"').strip("'")
            # Drop inline comments / version bounds: first whitespace-delimited token.
            token = value.split()[0] if value else ""
            if current == "dependencies":
                if token.startswith("hydra-"):
                    deps.add(token)
            elif current == "source-dirs":
                m_pkg = PACKAGES_SRC_RE.search(value)
                if m_pkg:
                    src_pkgs.add(m_pkg.group("pkg"))
            continue

        # A `key:` header (re)sets the current block context.
        m_key = re.match(r"^\s*([A-Za-z0-9_-]+):\s*(\S.*)?$", line)
        if m_key:
            key = m_key.group(1)
            if key == "dependencies":
                current = "dependencies"
            elif key == "source-dirs":
                current = "source-dirs"
            else:
                # Any other key ends the current list (e.g. `main:`, `executables:`).
                current = None
            continue

        # Any other non-list, non-key line ends the current list.
        current = None

    return deps, src_pkgs


def check_file(repo_root: Path, path: Path) -> list[str]:
    deps, src_pkgs = parse_blocks(path.read_text(encoding="utf-8"))
    violations = []
    for pkg in sorted(src_pkgs):
        if pkg in deps:
            rel = path.relative_to(repo_root)
            violations.append(
                f"{rel}: depends on published '{pkg}' AND source-dirs its HEAD "
                f"authoring tree (packages/{pkg}/src/main/haskell) — oil-and-water "
                f"violation (#608). Either drop the published dep (local-host "
                f"build) or source-dir only a Terms-free copy under the driver dir."
            )
    return violations


# --- Check 2: cold-seeder unpublished Hydra.Build.* import (#617) ----------------

# The cold-seeder's own closure: ColdSeedMain.hs plus the four "headmods" files
# copied verbatim from heads/haskell/src/main/haskell/Hydra/ by
# cold-seed-dist-haskell.sh. Kept in sync BY HAND with that script's copy list
# (Generation, PackageRouting, TargetFilePaths, Digest) — if that list ever
# changes, update both places; a mismatch here fails safe (misses a file) rather
# than unsafe (false violation), so drift is a coverage gap, not a false alarm.
COLDSEED_CLOSURE_ROOTS = [
    "heads/haskell/json-driver/app/ColdSeedMain.hs",
    "heads/haskell/src/main/haskell/Hydra/Generation.hs",
    "heads/haskell/src/main/haskell/Hydra/PackageRouting.hs",
    "heads/haskell/src/main/haskell/Hydra/TargetFilePaths.hs",
    "heads/haskell/src/main/haskell/Hydra/Digest.hs",
]

# `import [qualified] Hydra.Build.<Mod> [(...)] [as ...]`. Captures the dotted
# module suffix after "Hydra.Build.".
BUILD_IMPORT_RE = re.compile(
    r"^import\s+(?:qualified\s+)?Hydra\.Build\.(?P<mod>[A-Za-z0-9_.]+)\b"
)


def find_build_imports(text: str) -> set[str]:
    """Return the set of Hydra.Build.<Mod> module suffixes imported by this text."""
    return {
        m.group("mod")
        for line in text.splitlines()
        if (m := BUILD_IMPORT_RE.match(line.strip()))
    }


def module_suffix_to_namespace(mod: str) -> str:
    """Hydra.Build.ManifestWriter -> hydra.build.manifestWriter (manifest.json's
    module-namespace convention: dot-separated, first segment after the package
    lowercased-initial)."""
    first, *rest = mod.split(".")
    camel = first[:1].lower() + first[1:]
    return ".".join(["hydra", "build", camel] + rest)


def git_show(repo_root: Path, ref: str, rel_path: str) -> str | None:
    """Return file contents at `ref`, or None if absent at that ref (or ref/repo
    doesn't resolve — e.g. a synthetic fixture root with no git history)."""
    try:
        result = subprocess.run(
            ["git", "show", f"{ref}:{rel_path}"],
            cwd=repo_root, capture_output=True, text=True, timeout=30,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    return result.stdout if result.returncode == 0 else None


def resolve_merge_base(repo_root: Path, ref: str) -> str | None:
    """git merge-base HEAD `ref`, or None if it can't be resolved.

    Requires full history (the CI job's checkout step uses fetch-depth: 0 for
    exactly this reason — see .github/workflows/ci.yml). A shallow clone with
    no local origin/main ref returns None here; the caller treats that as
    "nothing to check against" and skips Check 2 rather than false-flagging
    every import, so a misconfigured checkout fails safe (silently permissive)
    rather than unsafe (spurious violations)."""
    try:
        result = subprocess.run(
            ["git", "merge-base", "HEAD", ref],
            cwd=repo_root, capture_output=True, text=True, timeout=30,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    return result.stdout.strip() if result.returncode == 0 else None


def published_modules_at(repo_root: Path, merge_base: str) -> set[str] | None:
    """The hydra-build namespace set (mainModules + testModules) at `merge_base`,
    read from the tracked manifest.json. None if unavailable (no such ref/path —
    treated as "can't verify", not "nothing published", by the caller)."""
    text = git_show(
        repo_root, merge_base,
        "dist/json/hydra-build/src/main/json/manifest.json",
    )
    if text is None:
        return None
    try:
        manifest = json.loads(text)
    except json.JSONDecodeError:
        return None
    modules = set(manifest.get("mainModules", [])) | set(manifest.get("testModules", []))
    return modules


def check_coldseed_build_imports(repo_root: Path, merge_base_ref: str) -> list[str]:
    """Flag Hydra.Build.<Mod> imports in the cold-seeder's closure that are not
    provably published at `merge_base_ref` (see module docstring, Check 2)."""
    merge_base = resolve_merge_base(repo_root, merge_base_ref)
    if merge_base is None:
        # No git history to compare against (e.g. a shallow clone with no
        # origin/main, or a synthetic fixture missing the ref) — nothing to
        # check against, so skip rather than false-flag every import.
        return []

    published_at_merge_base = published_modules_at(repo_root, merge_base)
    if published_at_merge_base is None:
        return []

    violations = []
    for rel in COLDSEED_CLOSURE_ROOTS:
        path = repo_root / rel
        if not path.is_file():
            continue
        current_text = path.read_text(encoding="utf-8")
        current_imports = find_build_imports(current_text)
        if not current_imports:
            continue

        merge_base_text = git_show(repo_root, merge_base, rel) or ""
        merge_base_imports = find_build_imports(merge_base_text)

        for mod in sorted(current_imports):
            ns = module_suffix_to_namespace(mod)
            module_absent = ns not in published_at_merge_base
            import_is_new = mod not in merge_base_imports
            if module_absent:
                violations.append(
                    f"{rel}: imports Hydra.Build.{mod} ({ns}), which is not in "
                    f"hydra-build's published module set as of merge-base "
                    f"{merge_base[:12]} — cold-seeder oil-and-water violation "
                    f"(#617, #560/#607 shape). The cold-seeder links the "
                    f"PUBLISHED hydra-build; a HEAD-new Hydra.Build.* module in "
                    f"its import closure breaks the cold-clone build."
                )
            elif import_is_new:
                violations.append(
                    f"{rel}: newly imports Hydra.Build.{mod} ({ns}) — the module "
                    f"predates merge-base {merge_base[:12]}, but this import is "
                    f"new-on-branch (#617, #607 shape: a pre-existing hydra-build "
                    f"module gaining a new cold-seeder call site can still be "
                    f"unpublished at the currently pinned hostVersion)."
                )
    return violations


def main():
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    here = Path(__file__).resolve().parent.parent
    ap.add_argument("--root", default=str(here),
                    help="Repository root (default: repo containing this script)")
    ap.add_argument("--merge-base-ref", default="origin/main",
                    help="Ref to compute the Check-2 merge-base against "
                         "(default: origin/main)")
    args = ap.parse_args()

    repo_root = Path(args.root).resolve()
    heads = repo_root / "heads"
    package_yamls = sorted(heads.glob("**/package.yaml")) if heads.is_dir() else []

    if not package_yamls:
        print(f"No heads/**/package.yaml found under {heads}", file=sys.stderr)
        sys.exit(1)

    all_violations = []
    for path in package_yamls:
        all_violations.extend(check_file(repo_root, path))

    print(f"Check 1: checked {len(package_yamls)} heads/**/package.yaml file(s) "
          f"for the source-dir oil-and-water invariant (#608)")

    coldseed_violations = check_coldseed_build_imports(repo_root, args.merge_base_ref)
    all_violations.extend(coldseed_violations)

    print(f"Check 2: checked {len(COLDSEED_CLOSURE_ROOTS)} cold-seeder closure "
          f"file(s) for unpublished Hydra.Build.* imports (#617)")
    print()

    if all_violations:
        print(f"FAIL: {len(all_violations)} oil-and-water violation(s):")
        for msg in all_violations:
            print(f"  {msg}")
        print()
        sys.exit(1)

    print("OK: no driver links a published hydra-* package while source-dirring "
          "its local HEAD authoring tree, and the cold-seeder's closure imports "
          "no unpublished Hydra.Build.* module.")
    sys.exit(0)


if __name__ == "__main__":
    main()
