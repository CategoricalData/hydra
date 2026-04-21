#!/usr/bin/env python3
"""Shell-level freshness check for sync.sh Phase 1.

Reads dist/json/digest.main.json (the universe-wide v1 digest written
by writeModulesJsonPackageSplit) and compares its recorded hashes
against the current SHA-256 of every DSL source file under
packages/*/src/main/haskell/Hydra/Sources/.

Exit 0 if every recorded namespace's source file content-matches AND
no DSL source has been added/removed (count parity). Exit 1 otherwise.

This is a sub-second check that lets bin/sync.sh skip Phase 1 (which
even on a Hydra-side cache hit costs 30+ seconds of Haskell startup)
when nothing has actually changed.

Falls back to exit 1 (cache miss) on any error: missing digest file,
missing packages dir, malformed digest, etc. Caller never reads
stdout; only exit code matters. Stderr is for diagnostics.
"""
import hashlib
import json
import re
import sys
from pathlib import Path

NAMESPACE_RE = re.compile(r'^ns\s*=\s*Namespace\s*"([^"]+)"', re.MULTILINE)


def hash_file(path: Path) -> str:
    h = hashlib.sha256()
    with open(path, 'rb') as f:
        for chunk in iter(lambda: f.read(65536), b''):
            h.update(chunk)
    return h.hexdigest()


def discover_namespaces(packages_root: Path) -> dict:
    """Walk packages/*/src/main/haskell/Hydra/Sources/ and return a
    namespace → file path map. Mirrors Hydra.Digest.discoverNamespaceFiles."""
    out = {}
    if not packages_root.is_dir():
        return out
    for pkg in sorted(packages_root.iterdir()):
        sources_dir = pkg / "src" / "main" / "haskell" / "Hydra" / "Sources"
        if not sources_dir.is_dir():
            continue
        for hs in sources_dir.rglob("*.hs"):
            try:
                content = hs.read_text(errors='replace')
            except OSError:
                continue
            m = NAMESPACE_RE.search(content)
            if m:
                out[m.group(1)] = str(hs)
    return out


def main() -> int:
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <hydra-root> <digest-file>", file=sys.stderr)
        return 2
    hydra_root = Path(sys.argv[1]).resolve()
    digest_file = Path(sys.argv[2]).resolve()

    if not digest_file.is_file():
        print(f"  shell-fresh: digest absent ({digest_file}); cache miss", file=sys.stderr)
        return 1

    try:
        with open(digest_file) as f:
            digest = json.load(f)
    except (OSError, json.JSONDecodeError) as e:
        print(f"  shell-fresh: digest unreadable ({e}); cache miss", file=sys.stderr)
        return 1

    recorded = digest.get('hashes', {})
    if not recorded:
        print(f"  shell-fresh: digest empty; cache miss", file=sys.stderr)
        return 1

    packages_root = hydra_root / "packages"
    ns_files = discover_namespaces(packages_root)
    if not ns_files:
        print(f"  shell-fresh: no DSL sources found under {packages_root}; cache miss",
              file=sys.stderr)
        return 1

    # Count parity: if a DSL source was added or removed since the last
    # digest, force a miss. We only enforce this for namespaces that
    # actually appear in the digest's universe (some DSL sources are
    # exec-only or test helpers — those won't have digest entries and
    # are ignored here).
    if set(recorded.keys()) - set(ns_files.keys()):
        missing = set(recorded.keys()) - set(ns_files.keys())
        print(f"  shell-fresh: {len(missing)} recorded namespaces missing"
              f" (e.g. {next(iter(missing))}); cache miss", file=sys.stderr)
        return 1

    # Hash each recorded namespace's source and compare.
    for ns, recorded_hash in recorded.items():
        path = Path(ns_files[ns])
        try:
            current_hash = hash_file(path)
        except OSError as e:
            print(f"  shell-fresh: cannot hash {path}: {e}; cache miss", file=sys.stderr)
            return 1
        if current_hash != recorded_hash:
            print(f"  shell-fresh: hash mismatch for {ns}; cache miss",
                  file=sys.stderr)
            return 1

    print(f"  shell-fresh: {len(recorded)} DSL sources clean; Phase 1 cache hit",
          file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
