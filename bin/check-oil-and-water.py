#!/usr/bin/env python3
"""Guard the "oil and water" invariant for the published-host drivers (#608).

The invariant (docs/build-system.md, "The published host is oil and water with
the working tree"): a driver that LINKS a published Hydra package (from Hackage)
must NOT also source-dir the matching package's LOCAL HEAD authoring tree under
packages/<pkg>/src/main/haskell. The two are different kinds of the same thing —
published generated code vs. HEAD DSL source — and mixing them in one compile
breaks the moment HEAD adds a kernel type the published package lacks.

Background (#608): the #376 cold-seeder (heads/haskell/json-driver) linked the
published hydra-kernel-0.17.1 while ALSO source-dirring
packages/hydra-kernel/src/main/haskell (for Hydra.Sources.All, its JSON
decode-universe context). That authoring tree contains Term-printers phantom-
annotated with generated kernel types (e.g. MissingCaseBranchesError, added on
main by #598) that do not exist in the published 0.17.1 kernel — so the driver
failed to compile ("Not in scope") on every new-kernel-type PR, reddening main.

This check catches that class of defect directly and cheaply (python3 + git only,
no host toolchain), so it can run as a first-line CI gate.

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

Usage:
  bin/check-oil-and-water.py [--root DIR]

Exit code is nonzero iff any driver both depends on a published hydra-<pkg> and
source-dirs the matching packages/hydra-<pkg>/src/main/haskell tree.
"""
from __future__ import annotations

import argparse
import re
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


def main():
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    here = Path(__file__).resolve().parent.parent
    ap.add_argument("--root", default=str(here),
                    help="Repository root (default: repo containing this script)")
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

    print(f"Checked {len(package_yamls)} heads/**/package.yaml file(s) for the "
          f"oil-and-water invariant (#608)")
    print()

    if all_violations:
        print(f"FAIL: {len(all_violations)} oil-and-water violation(s):")
        for msg in all_violations:
            print(f"  {msg}")
        print()
        sys.exit(1)

    print("OK: no driver links a published hydra-* package while source-dirring "
          "its local HEAD authoring tree.")
    sys.exit(0)


if __name__ == "__main__":
    main()
