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
                                hostOverrides untouched.
  host-version <pkg>            Print the published-host version the build/sync
                                should depend on for <pkg>: the hostOverrides entry
                                for <pkg>'s host if present (host-keyed — e.g.
                                hostOverrides["java"] applies to hydra-java), else
                                hostVersion if <pkg> is a published host. Exits 1
                                (no output) when <pkg> is not a consumed published
                                host, so the caller falls back to a local content
                                hash (the migration-shim path).
  set-host-version <X.Y.Z>      Write the global hostVersion. Per-host overrides in
                                hostOverrides are hand-edited (uncommon).
  is-published <pkg>            Exit 0 if host-version <pkg> would resolve, else 1.
  haskell-hackage <pkg>         Print the version iff <pkg> is consumable from
                                Hackage at its resolved host version (probes the
                                actual artifact: local Stack index, then a live
                                HEAD request). Exit 1 otherwise — the #370 Haskell
                                host then keeps <pkg>'s source-dirs local. A
                                hostOverrides value of "local" for <pkg>'s host
                                forces local (exit 1).

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
    "hydra-jvm",
    "hydra-python",
    "hydra-scala",
    "hydra-lisp",
    "hydra-typescript",
})


# Host-mode overrides (hydra.json `hostOverrides`) are keyed by HOST — a language
# name like "java" — not by the coder *package* (`hydra-java`). The language, the
# coder package, and the host runtime are three distinct things that happen to
# correspond 1:1 today; they diverge once hosts become independently-versioned
# artifacts (post-0.17). The override answers a question about the *host* ("consume
# the published Java host, or build it locally?"), so it is keyed on the host's
# stable identity — its language. The build-cache / component-identity path stays
# PACKAGE-keyed (it is genuinely about packages), so resolve_host_version bridges
# host→package via these maps. Lisp dialects are distinct hosts that share the one
# hydra-lisp coder package, so several hosts map to it.
# Note hydra-kernel maps to the "haskell" host, not a host of its own: the kernel
# is the data model the Haskell host *consumes* (alongside the hydra-haskell coder
# runtime), so the two move together — routing the Haskell host local pulls both
# hydra-kernel and hydra-haskell to the local compile path. A kernel-only override
# would leave the head linking a local kernel against a published coder (or vice
# versa), a mismatch; so "haskell" is the single knob for the whole host.
PACKAGE_FOR_HOST = {
    "haskell":     "hydra-haskell",   # also covers hydra-kernel (see HOST_FOR_PACKAGE)
    "java":        "hydra-java",
    "python":      "hydra-python",
    "scala":       "hydra-scala",
    "typescript":  "hydra-typescript",
    "clojure":     "hydra-lisp",
    "scheme":      "hydra-lisp",
    "common-lisp": "hydra-lisp",
    "emacs-lisp":  "hydra-lisp",
}
# Inverse: package → the canonical host name. hydra-kernel → "haskell" (it is part
# of the Haskell host's consumed set, not a separate host). For hydra-lisp (shared
# by four dialects) the canonical host is "lisp"; the four dialect keys in
# PACKAGE_FOR_HOST still resolve to hydra-lisp, so an override can target either
# "lisp" (all dialects) or a single dialect.
HOST_FOR_PACKAGE = {
    "hydra-kernel":     "haskell",
    "hydra-haskell":    "haskell",
    "hydra-java":       "java",
    "hydra-python":     "python",
    "hydra-scala":      "scala",
    "hydra-typescript": "typescript",
    "hydra-lisp":       "lisp",
}


def host_override_for_package(root: Path, pkg: str) -> Optional[str]:
    """The hostOverrides value (version string or "local") that applies to <pkg>,
    or None if no override applies. Bridges the package-keyed caller to the
    host-keyed `hostOverrides` map: looks up the package's canonical host, and —
    for the shared hydra-lisp coder — also honours a more specific per-dialect
    override key if present."""
    overrides = load_config(root).get("hostOverrides") or {}
    if not overrides:
        return None
    host = HOST_FOR_PACKAGE.get(pkg)
    if host is not None and host in overrides:
        return overrides[host]
    # hydra-lisp: a single-dialect override (e.g. "clojure") also applies, since
    # every dialect host maps to the one hydra-lisp coder package.
    if pkg == "hydra-lisp":
        for dialect in ("clojure", "scheme", "common-lisp", "emacs-lisp"):
            if dialect in overrides:
                return overrides[dialect]
    return None


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

    Resolution: hostOverrides[host_for(pkg)] → hostVersion (if pkg is a published
    host) → None (build pkg locally; caller uses a local content hash).

    The override is HOST-keyed (a language name like "java"); this function bridges
    from the package-keyed caller via host_override_for_package. An override value
    of "local" is the explicit "build this host from local source" signal (#370):
    it resolves to None, exactly like an unconsumed package, so component_identity
    content-hashes it and edits rebuild."""
    ov = host_override_for_package(root, pkg)
    if ov is not None:
        return None if ov == "local" else ov
    if pkg in PUBLISHED_HOSTS:
        return load_config(root).get("hostVersion")
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


def _hackage_has(pkg: str, version: str) -> bool:
    """True if <pkg>-<version> is resolvable as a Hackage package.

    #370 Haskell-host gate: the host consumes a Haskell coder package from
    Hackage iff its artifact actually exists there — we probe the artifact, not a
    declared list, so a coder published in a future cycle flips local→consumed
    with no config edit. Checks the local Stack Hackage index first (offline-safe,
    and exactly what `stack build` resolves against); falls back to a live HEAD
    request to hackage.haskell.org."""
    name = f"{pkg}-{version}"
    # 1. Local Stack index (no network). The pantry index lists entries as
    #    "<pkg>/<version>/<pkg>.cabal".
    idx = Path.home() / ".stack" / "pantry" / "hackage" / "00-index.tar"
    if idx.is_file():
        import tarfile
        try:
            prefix = f"{pkg}/{version}/"
            with tarfile.open(idx, "r") as tar:
                for member in tar:
                    if member.name.startswith(prefix):
                        return True
        except (tarfile.TarError, OSError):
            pass  # fall through to the live check
    # 2. Live Hackage check.
    try:
        import urllib.request
        req = urllib.request.Request(
            f"https://hackage.haskell.org/package/{name}", method="HEAD")
        with urllib.request.urlopen(req, timeout=10) as resp:
            return 200 <= resp.status < 300
    except Exception:
        return False


def cmd_haskell_hackage(root: Path, args: list[str]) -> int:
    """Print the version iff <pkg> is consumable from Hackage at its resolved
    host version (and not overridden "local"); exit 1 otherwise so the caller
    keeps that package's source-dirs on the local compile path."""
    if len(args) != 1:
        print("Usage: hydra-packages.py haskell-hackage <pkg>", file=sys.stderr)
        return 2
    pkg = args[0]
    ver = resolve_host_version(root, pkg)  # respects "local" override (→ None)
    if not ver:
        return 1
    if _hackage_has(pkg, ver):
        print(ver)
        return 0
    return 1


COMMANDS = {
    "haskell-hackage": cmd_haskell_hackage,
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
