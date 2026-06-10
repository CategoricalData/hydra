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

Version accessors (single source of truth is hydra.json; the standalone VERSION
file has been retired):
  current-version               Print hydra.json:currentVersion (this repo's
                                release/dev version).
  set-current-version <X.Y.Z>   Write currentVersion. Leaves hostVersion and
                                hostVersionOverrides untouched.
  host-version <pkg>            Print the published-host version the build/sync
                                should depend on for <pkg>: hostVersionOverrides[pkg]
                                if present, else hostVersion if <pkg> is a published
                                host. Exits 1 (no output) when <pkg> is not a
                                consumed published host, so the caller falls back to
                                a local content hash (the migration-shim path).
  set-host-version <X.Y.Z>      Write the global hostVersion. Per-host overrides in
                                hostVersionOverrides are hand-edited (uncommon).
  is-published <pkg>            Exit 0 if host-version <pkg> would resolve, else 1.

Roots the registry at HYDRA_ROOT_DIR (env var) if set, else at the repo root
inferred from this script's location (../..).
"""

import json
import os
import sys
from pathlib import Path
from typing import Optional


# Host packages that (a) have published artifacts and (b) participate in the
# per-target generator stamp via component_identity (bin/lib/assemble-common.sh).
# These are the only packages for which `host-version` resolves to a version
# string; any other package falls back to the local-content-hash shim. Restricting
# the set here is the single knob that decides "consume the published host" vs
# "build locally" per package. Lisp dialects all share the hydra-lisp coder, so the
# one entry covers clojure/scheme/common-lisp/emacs-lisp.
PUBLISHED_HOSTS = frozenset({
    "hydra-kernel",
    "hydra-haskell",
    "hydra-java",
    "hydra-python",
    "hydra-scala",
    "hydra-lisp",
})


def repo_root() -> Path:
    env = os.environ.get("HYDRA_ROOT_DIR")
    if env:
        return Path(env)
    return Path(__file__).resolve().parent.parent.parent


def config_path(root: Path) -> Path:
    return root / "hydra.json"


def load_config(root: Path) -> dict:
    with open(config_path(root)) as f:
        return json.load(f)


def save_config(root: Path, config: dict) -> None:
    # Round-trip preserving insertion order and the file's 2-space indent style.
    path = config_path(root)
    with open(path, "w") as f:
        json.dump(config, f, indent=2)
        f.write("\n")


def load_all_packages(root: Path) -> list[str]:
    return list(load_config(root)["packages"])


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


_VERSION_RE = None


def _valid_version(s: str) -> bool:
    global _VERSION_RE
    if _VERSION_RE is None:
        import re
        _VERSION_RE = re.compile(r"^[0-9]+\.[0-9]+\.[0-9]+$")
    return bool(_VERSION_RE.match(s))


def resolve_host_version(root: Path, pkg: str) -> Optional[str]:
    """The published-host version the build should depend on for <pkg>, or None.

    Resolution: hostVersionOverrides[pkg] → hostVersion (if pkg is a published
    host) → None (build pkg locally; caller uses a local content hash)."""
    config = load_config(root)
    overrides = config.get("hostVersionOverrides") or {}
    if pkg in overrides:
        return overrides[pkg]
    if pkg in PUBLISHED_HOSTS:
        return config.get("hostVersion")
    return None


def cmd_current_version(root: Path, args: list[str]) -> int:
    if args:
        print("Usage: hydra-packages.py current-version", file=sys.stderr)
        return 2
    print(load_config(root)["currentVersion"])
    return 0


def cmd_set_current_version(root: Path, args: list[str]) -> int:
    if len(args) != 1 or not _valid_version(args[0]):
        print("Usage: hydra-packages.py set-current-version <X.Y.Z>", file=sys.stderr)
        return 2
    config = load_config(root)
    config["currentVersion"] = args[0]
    save_config(root, config)
    return 0


def cmd_host_version(root: Path, args: list[str]) -> int:
    if len(args) != 1:
        print("Usage: hydra-packages.py host-version <pkg>", file=sys.stderr)
        return 2
    ver = resolve_host_version(root, args[0])
    if not ver:
        # Not a consumed published host: print nothing, exit 1 so the caller
        # falls back to the local content hash (the shim path).
        return 1
    print(ver)
    return 0


def cmd_set_host_version(root: Path, args: list[str]) -> int:
    if len(args) != 1 or not _valid_version(args[0]):
        print("Usage: hydra-packages.py set-host-version <X.Y.Z>", file=sys.stderr)
        return 2
    config = load_config(root)
    config["hostVersion"] = args[0]
    save_config(root, config)
    return 0


def cmd_is_published(root: Path, args: list[str]) -> int:
    if len(args) != 1:
        print("Usage: hydra-packages.py is-published <pkg>", file=sys.stderr)
        return 2
    return 0 if resolve_host_version(root, args[0]) else 1


COMMANDS = {
    "list": cmd_list,
    "deps": cmd_deps,
    "topo": cmd_topo,
    "reverse-closure": cmd_reverse_closure,
    "supports-target": cmd_supports_target,
    "current-version": cmd_current_version,
    "set-current-version": cmd_set_current_version,
    "host-version": cmd_host_version,
    "set-host-version": cmd_set_host_version,
    "is-published": cmd_is_published,
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
