#!/usr/bin/env python3
"""Shell-level freshness check for sync.sh Phase 1.

Compares a recorded hash of every input that affects Phase 1's outputs
(DSL sources + hand-written runtime + test infra + Stack/package config)
against the current state.

Inputs hashed:
  - packages/*/src/main/haskell/Hydra/Sources/**.hs           (Haskell DSL sources)
  - packages/hydra-java/src/main/java/hydra/sources/**/*.java (Java DSL sources, #344)
  - packages/hydra-python/src/main/python/hydra/sources/**/*.py (Python DSL sources, #344)
  - heads/haskell/src/main/haskell/**.hs                      (hand-written runtime)
  - heads/haskell/src/test/haskell/**.hs                      (hand-written test infra)
  - heads/haskell/src/exec/**/*.hs                            (executables: transform-haskell-dsl-to-json, etc.)
  - heads/haskell/package.yaml                                (build config)
  - heads/haskell/stack.yaml                                  (build config)
  - heads/haskell/bin/sync-haskell.sh                         (the runner itself)

Exit 0 if the current hash matches the recorded one in
heads/haskell/.stack-work/phase1-input-cache.txt — every Phase 1
artifact (JSON kernel, Haskell kernel, hydra-test result) is
guaranteed unchanged. Exit 1 otherwise (cache miss / no record).

Sub-second on a populated tree.

Usage: check-phase1-fresh.py <hydra-root>
"""
import hashlib
import sys
from pathlib import Path

CACHE_FILE_REL = "heads/haskell/.stack-work/phase1-input-cache.txt"


def hash_file(path: Path) -> str:
    h = hashlib.sha256()
    try:
        with open(path, 'rb') as f:
            for chunk in iter(lambda: f.read(65536), b''):
                h.update(chunk)
    except OSError:
        return "MISSING"
    return h.hexdigest()


def hash_committed(hydra_root: Path, rel: str) -> str:
    """SHA-256 of the COMMITTED (HEAD) form of a tracked file.

    #370: heads/haskell/{package.yaml,stack.yaml} are transiently rewritten into
    published-host mode during a sync (generate-head-haskell-build.py) and restored
    after. Their committed form is the mode-invariant local-mode source of truth.
    Hashing the committed form keeps the Phase-1 freshness stamp stable across host
    modes — a published-mode rewrite must not poison the cache, since the host
    produces byte-identical DSL→JSON output regardless of where its kernel came
    from. Falls back to the working-tree file if git is unavailable."""
    import subprocess
    r = subprocess.run(
        ["git", "-C", str(hydra_root), "show", f"HEAD:{rel}"],
        capture_output=True)
    if r.returncode != 0:
        return hash_file(hydra_root / rel)
    return hashlib.sha256(r.stdout).hexdigest()


def collect_inputs(hydra_root: Path) -> list:
    """Return a sorted list of (relpath, sha256) covering every Phase 1
    input. Determinism matters — bytes-on-disk reproducibility is the
    contract."""
    paths = set()
    packages = hydra_root / "packages"
    if packages.is_dir():
        for pkg in packages.iterdir():
            sources = pkg / "src" / "main" / "haskell" / "Hydra" / "Sources"
            if sources.is_dir():
                for hs in sources.rglob("*.hs"):
                    paths.add(hs)
    # Host-native DSL source trees (#344). The Java and Python coders are
    # now authored host-native, so edits there change Phase 1 outputs even
    # though no .hs file moved. Future host-native trees (Scala, TypeScript)
    # should be added here when they exist.
    host_native_dsl_dirs = (
        ("hydra-java", "src/main/java/hydra/sources", "*.java"),
        ("hydra-python", "src/main/python/hydra/sources", "*.py"),
    )
    for pkg_name, subdir, glob in host_native_dsl_dirs:
        d = packages / pkg_name / subdir if packages.is_dir() else None
        if d is not None and d.is_dir():
            for src in d.rglob(glob):
                paths.add(src)
    heads_haskell = hydra_root / "heads" / "haskell"
    for sub in ("src/main/haskell", "src/test/haskell", "src/exec"):
        d = heads_haskell / sub
        if d.is_dir():
            for hs in d.rglob("*.hs"):
                paths.add(hs)
    runner = heads_haskell / "bin" / "sync-haskell.sh"
    if runner.is_file():
        paths.add(runner)
    result = [(str(p.relative_to(hydra_root)), hash_file(p)) for p in paths]
    # #370: hash the COMMITTED (mode-invariant) form of the head build files so a
    # transient published-host rewrite doesn't poison the Phase-1 cache.
    for cfg in ("heads/haskell/package.yaml", "heads/haskell/stack.yaml"):
        result.append((cfg, hash_committed(hydra_root, cfg)))
    return sorted(result)


def hash_inputs(hydra_root: Path) -> str:
    """Single hash over every input."""
    h = hashlib.sha256()
    for rel, content_hash in collect_inputs(hydra_root):
        h.update(rel.encode('utf-8'))
        h.update(b'\0')
        h.update(content_hash.encode('utf-8'))
        h.update(b'\n')
    return h.hexdigest()


def main() -> int:
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <hydra-root> [--record]", file=sys.stderr)
        return 2
    hydra_root = Path(sys.argv[1]).resolve()
    record_mode = "--record" in sys.argv[2:]

    cache_path = hydra_root / CACHE_FILE_REL
    current = hash_inputs(hydra_root)

    if record_mode:
        cache_path.parent.mkdir(parents=True, exist_ok=True)
        cache_path.write_text(current + "\n")
        return 0

    if not cache_path.is_file():
        print("  phase1-fresh: no recorded hash; cache miss", file=sys.stderr)
        return 1
    recorded = cache_path.read_text().strip()
    if recorded != current:
        print("  phase1-fresh: input hash changed; cache miss", file=sys.stderr)
        return 1
    print("  phase1-fresh: every Phase 1 input clean; skipping Phase 1",
          file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
