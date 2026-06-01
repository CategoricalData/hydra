#!/usr/bin/env python3
"""Post-Phase-1 staleness gate for the hydra-java / hydra-python coder JSON (#406).

Why this exists
---------------
Per #344, the canonical hydra.java.* / hydra.python.* JSON under
dist/json/hydra-{java,python}/ is "owned" by the native self-host generators,
so a warm `bin/sync.sh` Phase 1 deliberately skips re-exporting it
(update-json-main writes those namespaces only with --include-java-python,
which sync.sh sets only on cold-start — a file-existence test).

dist/json/hydra-{java,python}/.../coder.json is translated in Phase 2 into
dist/haskell/hydra-{java,python}/.../Coder.hs, which is compiled into the core
`hydra` library (heads/haskell/package.yaml). When a kernel rename ripples into
the coders, the coder JSON must be re-exported against the *new* kernel or
Phase 2 produces a Coder.hs referencing a renamed-away field and the next
`stack build` dies (#406). The cold-start gate keys on file existence only, so
it never catches this on a warm tree.

This check closes that gap. It is evaluated AFTER Phase 1 (so the kernel JSON on
disk already reflects any in-flight rename) and BEFORE Phase 2 consumes the
coder JSON. A miss means sync.sh must re-export the java/python coder JSON
(via update-json-main --include-java-python) before Phase 2.

Inputs hashed
-------------
  - dist/json/hydra-kernel/src/main/json/**/*.json   (the translated kernel;
        reflects a rename once Phase 1 has run — the #406 trigger)
  - packages/hydra-java/src/main/haskell/Hydra/Sources/**.hs    (legacy Java coder DSL)
  - packages/hydra-python/src/main/haskell/Hydra/Sources/**.hs  (legacy Python coder DSL)

The kernel JSON is the load-bearing input: it is what makes a transitive kernel
rename observable here even when the coder source files are byte-identical, and
it is the input that survives the imminent deletion of the legacy Haskell coder
DSL (post-deletion the native generators are the sole writers, but the kernel
JSON they translate is unchanged as a staleness signal). The legacy coder DSL
files are hashed too while they still exist so a direct edit there also re-exports;
once they are deleted, rglob simply finds nothing there and the kernel-JSON
signal stands alone — no code change needed.

Exit 0 if the recorded hash matches (coder JSON is fresh w.r.t. the current
kernel — sync.sh may skip the re-export). Exit 1 on any miss (no record / hash
changed / sentinel JSON missing), forcing a re-export.

Usage:
  check-java-python-json-fresh.py <hydra-root>            # check
  check-java-python-json-fresh.py <hydra-root> --record   # stamp current state
"""
import hashlib
import sys
from pathlib import Path

CACHE_FILE_REL = "heads/haskell/.stack-work/java-python-json-cache.txt"

# Coder JSON outputs. A missing sentinel is a cold-start: report a miss so the
# flag is forced (sync.sh seeds the bootstrap in that case anyway).
SENTINELS_REL = (
    "dist/json/hydra-java/src/main/json/hydra/java/coder.json",
    "dist/json/hydra-python/src/main/json/hydra/python/coder.json",
)


def hash_file(path: Path) -> str:
    h = hashlib.sha256()
    try:
        with open(path, 'rb') as f:
            for chunk in iter(lambda: f.read(65536), b''):
                h.update(chunk)
    except OSError:
        return "MISSING"
    return h.hexdigest()


def collect_inputs(hydra_root: Path) -> list:
    """Return a sorted list of (relpath, sha256) covering every input that
    determines the freshness of the hydra-java / hydra-python coder JSON
    relative to the current kernel. Sorted by relpath for determinism."""
    paths = set()

    # (a) The translated kernel JSON. After Phase 1 this reflects any in-flight
    # kernel rename, so a change here is the #406 trigger.
    kernel_json = hydra_root / "dist" / "json" / "hydra-kernel" / "src" / "main" / "json"
    if kernel_json.is_dir():
        for f in kernel_json.rglob("*.json"):
            paths.add(f)

    # (b) Legacy Haskell DSL coder sources for the two affected packages. Hashed
    # while they exist so a direct edit also re-exports; harmless (finds nothing)
    # once they are deleted, leaving the kernel-JSON signal in (a).
    packages = hydra_root / "packages"
    if packages.is_dir():
        for pkg_name in ("hydra-java", "hydra-python"):
            sources = packages / pkg_name / "src" / "main" / "haskell" / "Hydra" / "Sources"
            if sources.is_dir():
                for hs in sources.rglob("*.hs"):
                    paths.add(hs)

    return sorted((str(p.relative_to(hydra_root)), hash_file(p))
                  for p in paths)


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

    # Cold-start: a missing sentinel means the JSON was never written. Report a
    # miss so the flag is forced (sync.sh seeds the bootstrap in that case too).
    for rel in SENTINELS_REL:
        if not (hydra_root / rel).is_file():
            print(f"  java-python-json-fresh: sentinel missing ({rel}); miss",
                  file=sys.stderr)
            return 1

    if not cache_path.is_file():
        print("  java-python-json-fresh: no recorded hash; miss", file=sys.stderr)
        return 1
    recorded = cache_path.read_text().strip()
    if recorded != current:
        print("  java-python-json-fresh: kernel/coder DSL changed since last "
              "coder-JSON export; re-export needed (#406)", file=sys.stderr)
        return 1
    print("  java-python-json-fresh: coder JSON fresh w.r.t. kernel; skip re-export",
          file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
