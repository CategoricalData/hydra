#!/usr/bin/env python3
"""Post-Phase-1 staleness gate for the hydra-java / hydra-python / hydra-scala
coder JSON (#406, extended to native DSL edits + Scala by #632).

Why this exists
---------------
Per #344, the canonical hydra.java.* / hydra.python.* / hydra.scala.* JSON under
dist/json/hydra-{java,python,scala}/ is "owned" by the native self-host generators,
so a warm `bin/sync.sh` Phase 1 deliberately skips re-exporting it
(update-json-main writes the java/python namespaces only with --include-java-python,
which sync.sh sets only on cold-start — a file-existence test).

dist/json/hydra-{java,python,scala}/.../coder.json is translated in Phase 2 into
dist/haskell/hydra-{java,python,scala}/.../Coder.hs, which is compiled into the
core `hydra` library (heads/haskell/package.yaml). Two distinct events can make
that translated JSON stale:
  - A kernel rename ripples into the coders (#406): the coder JSON must be
    re-exported against the *new* kernel or Phase 2 emits a Coder.hs referencing
    a renamed-away field and the next `stack build` dies.
  - A coder-signature change is authored directly in the native DSL source
    (packages/hydra-{java,python,scala}/src/main/{java,python,scala}/...) with
    no kernel rename involved (#632): dist/json/hydra-<lang>/ must be re-derived
    from the edited source, or Phase 2 builds Coder.hs from the JSON as it stood
    before the edit.
The cold-start gate keys on file existence only, so it catches neither case on a
warm tree.

This check closes both gaps. It is evaluated AFTER Phase 1 (so the kernel JSON on
disk already reflects any in-flight rename) and BEFORE Phase 2 consumes the coder
JSON. A miss means sync.sh must re-export the java/python/scala coder JSON (via
the native drivers) before Phase 2.

Inputs hashed
-------------
  - dist/json/hydra-kernel/src/main/json/**/*.json                (the translated
        kernel; reflects a rename once Phase 1 has run — the #406 trigger)
  - packages/hydra-java/src/main/java/**/*.java                   (native Java coder DSL)
  - packages/hydra-python/src/main/python/**/*.py                 (native Python coder DSL)
  - packages/hydra-scala/src/main/scala/**/*.scala                (native Scala coder DSL)

The kernel JSON is the load-bearing input for the #406 case: it is what makes a
transitive kernel rename observable here even when the coder source files are
byte-identical. The native DSL sources are the load-bearing input for the #632
case: a direct signature edit there, with the kernel untouched, must still flip
this gate. (The legacy Haskell DSL copies these paths replaced were deleted in
#346 — hashing paths under a since-deleted `src/main/haskell/Hydra/Sources` tree
silently matched nothing and was the root cause of #632's defect C.)

Exit 0 if the recorded hash matches (coder JSON is fresh w.r.t. the current
kernel and native DSL sources — sync.sh may skip the re-export). Exit 1 on any
miss (no record / hash changed / sentinel JSON missing), forcing a re-export.

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
    "dist/json/hydra-scala/src/main/json/hydra/scala/coder.json",
)

# Native DSL source trees, one per (package, source-extension) pair.
NATIVE_SOURCE_DIRS = (
    ("hydra-java", ("src", "main", "java"), "*.java"),
    ("hydra-python", ("src", "main", "python"), "*.py"),
    ("hydra-scala", ("src", "main", "scala"), "*.scala"),
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
    determines the freshness of the hydra-java / hydra-python / hydra-scala
    coder JSON relative to the current kernel and native DSL sources. Sorted
    by relpath for determinism."""
    paths = set()

    # (a) The translated kernel JSON. After Phase 1 this reflects any in-flight
    # kernel rename, so a change here is the #406 trigger.
    kernel_json = hydra_root / "dist" / "json" / "hydra-kernel" / "src" / "main" / "json"
    if kernel_json.is_dir():
        for f in kernel_json.rglob("*.json"):
            paths.add(f)

    # (b) Native DSL coder sources for the three affected packages — the #632
    # trigger. A direct signature edit here, with the kernel untouched, must
    # still flip this gate.
    packages = hydra_root / "packages"
    if packages.is_dir():
        for pkg_name, rel_parts, glob_pattern in NATIVE_SOURCE_DIRS:
            sources = packages / pkg_name
            for part in rel_parts:
                sources = sources / part
            if sources.is_dir():
                for src in sources.rglob(glob_pattern):
                    paths.add(src)

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
        print("  java-python-json-fresh: kernel or native coder DSL changed since "
              "last coder-JSON export; re-export needed (#406/#632)", file=sys.stderr)
        return 1
    print("  java-python-json-fresh: coder JSON fresh w.r.t. kernel and native DSL "
          "sources; skip re-export", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
