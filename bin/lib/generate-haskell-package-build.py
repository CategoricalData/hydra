#!/usr/bin/env python3
"""Generate a standalone hpack build (package.yaml + stack.yaml) for one Hydra
Haskell distribution package under dist/haskell/<pkg>/.

Each emitted build is self-contained: from `dist/haskell/<pkg>/`, running
`stack sdist` produces a Hackage-ready tarball, and `stack build` compiles the
package. The package.yaml declares `<pkg>` at the worktree VERSION with
exact-version dependencies on every Hydra package listed in
`packages/<pkg>/package.json`'s `dependencies` array, so the published
distribution is version-locked across the per-package family (an external
consumer that adds e.g. `hydra-haskell == <version>` transitively pulls
`hydra-kernel == <version>`).

This is the Haskell analog of bin/lib/generate-java-package-build.py. Unlike the
Java/Maven side, the generated dist trees are pure Haskell source with no
hand-written runtime to copy in — EXCEPT the kernel, which needs the two
hand-written kernel modules (Hydra.Settings, Hydra.Kernel) staged in by the
assembler. This generator only writes the build metadata; source staging is the
assembler's job (heads/haskell/bin/assemble-haskell-distribution.sh).

Inputs:
  packages/<pkg>/package.json  (read for name, description, dependencies)
  VERSION                       (read for the version string)

Outputs:
  dist/haskell/<pkg>/package.yaml
  dist/haskell/<pkg>/stack.yaml   (unless --no-stack-yaml)
"""

from __future__ import annotations

import argparse
import json
import os
import sys


AUTHOR = "Joshua Shinavier <josh@fortytwo.net>"
LICENSE = "Apache-2.0"
CATEGORY = "Data"
GITHUB = "CategoricalData/hydra"
RESOLVER = "lts-24.7"

# Per-package external (non-Hydra) Hackage dependencies, derived from the
# imports actually present in each dist tree. The monolith `hydra` package
# declared 13 deps, but most (aeson, SHA, regex-tdfa, directory, filepath,
# split, vector, base64-bytestring) are used only by the HEAD machinery
# (exec drivers, JSON I/O, generation), not by the published library content.
# The per-package libraries need only the small set below.
#
# Verified via: grep of `^import ... Data.X` across each dist/haskell/<pkg>
# main tree (see feature_418 plan, session-2 findings).
EXTERNAL_DEPS: dict[str, list[str]] = {
    # hydra-kernel ships the generated kernel runtime PLUS the hand-written
    # Haskell primitive library (Hydra.Haskell.Lib.*) + DSL term support
    # (Hydra.Dsl.{Terms,Literals,Meta.Common}) it calls — the Haskell analog of
    # Java's copied kernel runtime. Those add base64-bytestring/split/text/
    # regex-tdfa beyond the generated tree's base/bytestring/containers/
    # scientific. Verified by transitive import closure of the kernel dist.
    "hydra-kernel": [
        "base                          >= 4.19.0 && < 4.22",
        "base64-bytestring             >= 1.2.1 && < 1.3",     # Lib.Literals (Data.ByteString.Base64)
        "bytestring                    >= 0.11.5 && < 0.13",   # Data.ByteString
        "containers                    >= 0.6.7 && < 0.8",     # Data.Map, Data.Set
        "regex-tdfa                    >= 1.3.2 && < 1.4",     # Lib.Regex (Text.Regex.TDFA)
        "scientific                    >= 0.3.7 && < 0.4",     # Data.Scientific
        "split                         >= 0.2.3 && < 0.3",     # Lib.Lists (Data.List.Split)
        "text                          >= 2.0.2 && < 2.2",     # Lib.Strings (Data.Text)
    ],
    "hydra-haskell": [
        "base                          >= 4.19.0 && < 4.22",
        "containers                    >= 0.6.7 && < 0.8",     # Data.Map, Data.Set
        "scientific                    >= 0.3.7 && < 0.4",     # Data.Scientific
    ],
    # The umbrella ships only re-export modules; it needs `base` and whatever
    # its re-export imports pull (transitively via the Hydra deps). base only.
    "hydra": [
        "base                          >= 4.19.0 && < 4.22",
    ],
}

# Short synopsis per package (one line). Keeps the Hackage package page legible.
SYNOPSIS: dict[str, str] = {
    "hydra-kernel": "The Hydra kernel: core types, terms, inference, and DSL runtime",
    "hydra-haskell": "Hydra's Haskell coder: emit Haskell source from Hydra modules",
    "hydra": "Hydra: graphs are programs, and programs are graphs (umbrella package)",
}

# Longer description per package. Hackage/`cabal check` warns when 'description'
# is not longer than 'synopsis', so for packages whose package.json description
# is terse we supply a fuller one here. The Hydra context line is shared.
HYDRA_BLURB = (
    "Hydra is an implementation of the LambdaGraph data model, which takes advantage of an\n"
    "isomorphism between labeled hypergraphs and typed lambda calculus: in Hydra, \"graphs are\n"
    "programs, and programs are graphs\"."
)
LONG_DESCRIPTION: dict[str, str] = {
    "hydra-kernel": (
        HYDRA_BLURB + "\n"
        "This package is the Hydra kernel: the core type and term model, type inference, the term\n"
        "rewriting and reduction engine, the primitive library, and the DSL runtime — the code that\n"
        "must be present in every Hydra implementation. The convenient entry point is the Hydra.Kernel\n"
        "module, which re-exports the kernel's collision-free surface."
    ),
    "hydra-haskell": (
        HYDRA_BLURB + "\n"
        "This package is Hydra's Haskell coder: it translates Hydra modules into Haskell source.\n"
        "The top-level entry point is moduleToHaskell (and moduleToHaskellModule for the structured\n"
        "AST). It builds on hydra-kernel."
    ),
}

# The umbrella `hydra` is NOT a Hydra source package (no packages/hydra/), it is
# a hand-written Haskell-head publishing artifact. Its metadata is specified
# here rather than read from a package.json. Inter-package deps are the curated
# 0.16.0 publish subset (see feature_418 plan).
UMBRELLA_META: dict = {
    "name": "hydra",
    "description": (
        "Hydra is an implementation of the LambdaGraph data model, which takes advantage of an\n"
        "isomorphism between labeled hypergraphs and typed lambda calculus: in Hydra, \"graphs are\n"
        "programs, and programs are graphs\". This umbrella package provides a single convenient\n"
        "entry point (the Hydra module) re-exporting the Hydra kernel plus the Haskell coder's\n"
        "moduleToHaskell. Depend on hydra-kernel / hydra-haskell directly for finer-grained APIs."
    ),
    "dependencies": ["hydra-kernel", "hydra-haskell"],
}


def render_package_yaml(name: str, description: str, version: str,
                        hydra_deps: list[str]) -> str:
    synopsis = SYNOPSIS.get(name, description)
    ext = EXTERNAL_DEPS.get(name, ["base >= 4.19.0 && < 4.22"])
    # Hydra inter-package deps are EXACTLY version-locked: published in synchrony
    # from one repo, so == <version> guarantees the Cabal solver unifies and the
    # umbrella can never be satisfied against a mismatched sibling.
    dep_lines = list(ext) + [f"{d} == {version}" for d in hydra_deps]
    deps_block = "\n".join(f"  - {d}" for d in dep_lines)

    # Folded description block: package.json description, wrapped as a single
    # YAML folded scalar.
    desc_indented = "\n  ".join(description.split("\n"))

    if name == "hydra":
        meta_source = "the built-in umbrella spec in bin/lib/generate-haskell-package-build.py"
    else:
        meta_source = f"packages/{name}/package.json"
    return f"""# Generated file. Do not edit.
# Regenerated by bin/lib/generate-haskell-package-build.py from
# {meta_source} and the worktree's VERSION file.
# From this directory, `stack sdist` produces a Hackage-ready tarball.

name:         {name}
version:      {version}
author:       {AUTHOR}
license:      {LICENSE}
license-file: LICENSE
category:     {CATEGORY}
synopsis:     {json.dumps(synopsis)}
description:  >
  {desc_indented}
github:       {GITHUB}

extra-source-files:
  - LICENSE

dependencies:
{deps_block}

library:
  source-dirs:
    - src/main/haskell
"""


def render_stack_yaml() -> str:
    return f"""# Generated file. Do not edit.
resolver: {RESOLVER}

system-ghc: true
install-ghc: false

packages:
  - .

extra-deps:
"""


def main() -> int:
    p = argparse.ArgumentParser(
        description=__doc__.splitlines()[0] if __doc__ else None)
    p.add_argument("package", help="Package name (e.g. hydra-kernel)")
    p.add_argument(
        "--repo-root",
        default=os.environ.get("HYDRA_ROOT_DIR"),
        help="Hydra worktree root (default: $HYDRA_ROOT_DIR)",
    )
    p.add_argument(
        "--out-dir",
        help="Override output directory (default: <repo-root>/dist/haskell/<package>)",
    )
    p.add_argument(
        "--no-stack-yaml",
        action="store_true",
        help="Do not emit stack.yaml (e.g. when an outer stack project supplies one)",
    )
    args = p.parse_args()

    if not args.repo_root:
        print("error: --repo-root or $HYDRA_ROOT_DIR is required", file=sys.stderr)
        return 2

    if args.package == "hydra":
        # Umbrella: metadata is built in, not read from packages/.
        meta = UMBRELLA_META
    else:
        pkg_json_path = os.path.join(
            args.repo_root, "packages", args.package, "package.json")
        if not os.path.isfile(pkg_json_path):
            print(f"error: no such package.json: {pkg_json_path}", file=sys.stderr)
            return 1
        with open(pkg_json_path) as f:
            meta = json.load(f)

    pkg_name = meta.get("name") or args.package
    # Prefer a fuller hand-written description (keeps `cabal check` happy: it
    # warns when description is not longer than synopsis) and fall back to the
    # package.json description.
    description = LONG_DESCRIPTION.get(pkg_name) or meta.get("description") or pkg_name
    hydra_deps = list(meta.get("dependencies") or [])

    version_path = os.path.join(args.repo_root, "VERSION")
    with open(version_path) as f:
        version = f.read().strip()

    out_dir = args.out_dir or os.path.join(
        args.repo_root, "dist", "haskell", args.package)
    os.makedirs(out_dir, exist_ok=True)

    pkg_path = os.path.join(out_dir, "package.yaml")
    with open(pkg_path, "w") as f:
        f.write(render_package_yaml(pkg_name, description, version, hydra_deps))
    print(f"  wrote {pkg_path}")

    if not args.no_stack_yaml:
        stack_path = os.path.join(out_dir, "stack.yaml")
        with open(stack_path, "w") as f:
            f.write(render_stack_yaml())
        print(f"  wrote {stack_path}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
