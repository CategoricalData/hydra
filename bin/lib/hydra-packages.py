#!/usr/bin/env python3
"""Read the Hydra package registry and per-package metadata.

Subcommands:
  list                          Print every package from hydra.json, space-separated.
  deps <pkg>                    Print the dependencies of <pkg>, space-separated.
  topo <pkg> [<pkg> ...]        Topologically sort the given packages (deps first).
  reverse-closure <pkg>         Print every registered package that transitively
                                depends on <pkg> (including <pkg> itself).
                                Output is alphabetically sorted; pipe through
                                `topo` if a build-order traversal is needed.
  supports-target <pkg> <tgt>   Exit 0 if <pkg> declares <tgt> in its
                                targetLanguages field (or has no field, meaning
                                every target). Exit 1 otherwise.

Roots the registry at HYDRA_ROOT_DIR (env var) if set, else at the repo root
inferred from this script's location (../..).
"""

import json
import os
import sys
from pathlib import Path


def repo_root() -> Path:
    env = os.environ.get("HYDRA_ROOT_DIR")
    if env:
        return Path(env)
    return Path(__file__).resolve().parent.parent.parent


def load_all_packages(root: Path) -> list[str]:
    with open(root / "hydra.json") as f:
        return list(json.load(f)["packages"])


def load_package_meta(root: Path, pkg: str) -> dict:
    path = root / "packages" / pkg / "package.json"
    if not path.exists():
        return {}
    with open(path) as f:
        return json.load(f)


def load_deps(root: Path, pkg: str) -> list[str]:
    return list(load_package_meta(root, pkg).get("dependencies", []))


def cmd_list(root: Path, args: list[str]) -> int:
    if args:
        print("Usage: hydra-packages.py list", file=sys.stderr)
        return 2
    print(" ".join(load_all_packages(root)))
    return 0


def cmd_deps(root: Path, args: list[str]) -> int:
    if len(args) != 1:
        print("Usage: hydra-packages.py deps <pkg>", file=sys.stderr)
        return 2
    print(" ".join(load_deps(root, args[0])))
    return 0


def cmd_topo(root: Path, args: list[str]) -> int:
    # DFS topo sort over the given package set; deps come before dependents.
    # Edges that point to packages outside the input set are ignored, matching
    # the prior bash behavior (sort within the slice the user asked for).
    packages = list(args)
    deps = {pkg: load_deps(root, pkg) for pkg in packages}
    in_set = set(packages)

    visited: set[str] = set()
    order: list[str] = []

    def visit(pkg: str) -> None:
        if pkg in visited:
            return
        visited.add(pkg)
        for d in deps.get(pkg, []):
            if d in in_set:
                visit(d)
        order.append(pkg)

    for pkg in packages:
        visit(pkg)

    print(" ".join(order))
    return 0


def cmd_reverse_closure(root: Path, args: list[str]) -> int:
    if len(args) != 1:
        print("Usage: hydra-packages.py reverse-closure <pkg>", file=sys.stderr)
        return 2
    root_pkg = args[0]
    all_packages = load_all_packages(root)
    deps = {pkg: load_deps(root, pkg) for pkg in all_packages}

    # Reverse adjacency: who depends on each pkg?
    rdeps: dict[str, list[str]] = {p: [] for p in all_packages}
    for p, ds in deps.items():
        for d in ds:
            if d in rdeps:
                rdeps[d].append(p)

    # BFS outward from root_pkg.
    closure = {root_pkg}
    frontier = [root_pkg]
    while frontier:
        next_frontier: list[str] = []
        for p in frontier:
            for dependent in rdeps.get(p, []):
                if dependent not in closure:
                    closure.add(dependent)
                    next_frontier.append(dependent)
        frontier = next_frontier

    print(" ".join(sorted(closure)))
    return 0


def cmd_supports_target(root: Path, args: list[str]) -> int:
    if len(args) != 2:
        print("Usage: hydra-packages.py supports-target <pkg> <target>", file=sys.stderr)
        return 2
    pkg, target = args
    meta = load_package_meta(root, pkg)
    if not meta:
        # Unknown package: match prior bash behavior (no package.json => support every target).
        return 0
    tls = meta.get("targetLanguages")
    if tls is None:
        return 0
    return 0 if target in tls else 1


COMMANDS = {
    "list": cmd_list,
    "deps": cmd_deps,
    "topo": cmd_topo,
    "reverse-closure": cmd_reverse_closure,
    "supports-target": cmd_supports_target,
}


def main(argv: list[str]) -> int:
    if len(argv) >= 2 and argv[1] in ("-h", "--help"):
        print(__doc__, end="")
        return 0
    if len(argv) < 2:
        print(__doc__, end="", file=sys.stderr)
        return 2
    cmd = argv[1]
    handler = COMMANDS.get(cmd)
    if handler is None:
        print(f"Unknown subcommand: {cmd}", file=sys.stderr)
        print("Subcommands: " + ", ".join(COMMANDS), file=sys.stderr)
        return 2
    return handler(repo_root(), argv[2:])


if __name__ == "__main__":
    sys.exit(main(sys.argv))
