#!/usr/bin/env python3
"""Shell-level freshness check for sync.sh Phase 5 (Native DSL→JSON).

Mirror of bin/lib/check-phase1-fresh.py for the self-hosted coder demos
(JavaSelfHostDemo / python-self-host-demo.py). Phase 5 was previously
unconditional — every sync re-ran the ~30-minute JavaSelfHostDemo even
when no input could affect its output. This check lets sync.sh skip the
demo when every input is byte-identical to the last successful run.

Usage:
  check-phase5-fresh.py <hydra-root> <kind>            # check
  check-phase5-fresh.py <hydra-root> <kind> --record   # record after success

  kind ∈ {java, python}

Exit 0 if the recorded hash matches the current input hash; the demo can
be skipped. Exit 1 otherwise (no record, or input changed).

Inputs hashed per kind:

  java:
    packages/hydra-java/src/main/java/**/*.java
    heads/java/src/main/java/hydra/{BenchInference,Bootstrap,Generation,
        HydraTestBase,JavaSelfHostDemo,ProfileJavaCoder}.java
    heads/java/src/main/java/hydra/json/*.java
    dist/json/hydra-kernel/src/main/json/**/*.json
    bin/generate-hydra-java-from-java.sh
    packages/hydra-java/build.gradle

  python:
    packages/hydra-python/src/main/python/**/*.py
    heads/python/src/main/python/**/*.py
    dist/json/hydra-kernel/src/main/json/**/*.json
    bin/generate-hydra-python-from-python.sh
    bin/python-self-host-demo.py

Cache file per kind: dist/json/hydra-<kind>/build/phase5-input-cache.txt
"""
import hashlib
import sys
from pathlib import Path


# headsExtras include list from packages/hydra-java/build.gradle:54-65.
# Keep in sync.
JAVA_HEADS_EXTRAS = (
    "hydra/BenchInference.java",
    "hydra/Bootstrap.java",
    "hydra/Generation.java",
    "hydra/HydraTestBase.java",
    "hydra/JavaSelfHostDemo.java",
    "hydra/ProfileJavaCoder.java",
    "hydra/json/JsonIoCoder.java",
    "hydra/json/JsonSerde.java",
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


def collect_java_inputs(hydra_root: Path) -> set:
    paths = set()
    pkg_java_src = hydra_root / "packages" / "hydra-java" / "src" / "main" / "java"
    if pkg_java_src.is_dir():
        for f in pkg_java_src.rglob("*.java"):
            paths.add(f)
    heads_java_src = hydra_root / "heads" / "java" / "src" / "main" / "java"
    for rel in JAVA_HEADS_EXTRAS:
        p = heads_java_src / rel
        if p.is_file():
            paths.add(p)
    kernel_json = hydra_root / "dist" / "json" / "hydra-kernel" / "src" / "main" / "json"
    if kernel_json.is_dir():
        for f in kernel_json.rglob("*.json"):
            paths.add(f)
    for extra in (
        hydra_root / "bin" / "generate-hydra-java-from-java.sh",
        hydra_root / "packages" / "hydra-java" / "build.gradle",
    ):
        if extra.is_file():
            paths.add(extra)
    return paths


def collect_python_inputs(hydra_root: Path) -> set:
    paths = set()
    pkg_py = hydra_root / "packages" / "hydra-python" / "src" / "main" / "python"
    if pkg_py.is_dir():
        for f in pkg_py.rglob("*.py"):
            paths.add(f)
    heads_py = hydra_root / "heads" / "python" / "src" / "main" / "python"
    if heads_py.is_dir():
        for f in heads_py.rglob("*.py"):
            paths.add(f)
    kernel_json = hydra_root / "dist" / "json" / "hydra-kernel" / "src" / "main" / "json"
    if kernel_json.is_dir():
        for f in kernel_json.rglob("*.json"):
            paths.add(f)
    for extra in (
        hydra_root / "bin" / "generate-hydra-python-from-python.sh",
        hydra_root / "bin" / "python-self-host-demo.py",
    ):
        if extra.is_file():
            paths.add(extra)
    return paths


def collect_inputs(hydra_root: Path, kind: str) -> list:
    if kind == "java":
        paths = collect_java_inputs(hydra_root)
    elif kind == "python":
        paths = collect_python_inputs(hydra_root)
    else:
        raise ValueError(f"unknown kind: {kind!r}")
    return sorted((str(p.relative_to(hydra_root)), hash_file(p)) for p in paths)


def hash_inputs(hydra_root: Path, kind: str) -> str:
    h = hashlib.sha256()
    for rel, content_hash in collect_inputs(hydra_root, kind):
        h.update(rel.encode('utf-8'))
        h.update(b'\0')
        h.update(content_hash.encode('utf-8'))
        h.update(b'\n')
    return h.hexdigest()


def cache_path_for(hydra_root: Path, kind: str) -> Path:
    return hydra_root / "dist" / "json" / f"hydra-{kind}" / "build" / "phase5-input-cache.txt"


def main() -> int:
    if len(sys.argv) < 3 or sys.argv[2] not in ("java", "python"):
        print(f"Usage: {sys.argv[0]} <hydra-root> <java|python> [--record]",
              file=sys.stderr)
        return 2
    hydra_root = Path(sys.argv[1]).resolve()
    kind = sys.argv[2]
    record_mode = "--record" in sys.argv[3:]

    cache_path = cache_path_for(hydra_root, kind)
    current = hash_inputs(hydra_root, kind)

    if record_mode:
        cache_path.parent.mkdir(parents=True, exist_ok=True)
        cache_path.write_text(current + "\n")
        return 0

    if not cache_path.is_file():
        print(f"  phase5-fresh ({kind}): no recorded hash; cache miss",
              file=sys.stderr)
        return 1
    recorded = cache_path.read_text().strip()
    if recorded != current:
        print(f"  phase5-fresh ({kind}): input hash changed; cache miss",
              file=sys.stderr)
        return 1
    print(f"  phase5-fresh ({kind}): every Phase 5 input clean; skipping demo",
          file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
