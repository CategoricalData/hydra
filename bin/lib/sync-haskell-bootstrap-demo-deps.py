#!/usr/bin/env python3
"""#670 — keep the bootstrap demo's Haskell external (Hackage) dependency list
in sync with heads/haskell/package.yaml's `dependencies:` block.

The bootstrap demo's Haskell target (demos/bootstrapping/resources/haskell/
package.yaml, copied verbatim by demos/bootstrapping/bin/setup-haskell-target.sh)
compiles a flattened tree of the head's hand-written runtime plus the kernel,
Haskell-coder, and hydra-build DSL sources. Its external dep footprint has
always been a straight subset of the head's own `dependencies:` block — never
a curated list in its own right. Before #670 it was a separate hand-maintained
copy that silently drifted (#666 added `unix` to the head but missed this
file, breaking every `*-to-haskell` bootstrap cell).

This script makes heads/haskell/package.yaml the single hand-maintained source
of truth: it reads that file's top-level `dependencies:` block and rewrites
the demo resource's `dependencies:` block to match exactly. Nothing else in
the demo file (synopsis, description, tests stanza) is touched — those are
legitimately demo-specific.

Usage:
  sync-haskell-bootstrap-demo-deps.py [--check]

--check: exit 1 if the demo file's dependencies block is NOT already in sync
(no write). Used as a fast CI-runnable drift guard.
"""

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent.parent
HEAD_PKG_YAML = ROOT / "heads" / "haskell" / "package.yaml"
DEMO_PKG_YAML = ROOT / "demos" / "bootstrapping" / "resources" / "haskell" / "package.yaml"

HEAD_DEPS_BLOCK_RE = re.compile(r"\ndependencies:\n((?:  - .*\n)+)")

GEN_MARKER_START = "  # --- BEGIN generated from heads/haskell/package.yaml (#670). Do not edit by hand. ---\n"
GEN_MARKER_END = "  # --- END generated ---\n"

# Matches the demo's dependencies: block whether it is still in bare
# (pre-#670) form or already wrapped in the generated markers, so re-running
# this script is idempotent.
DEMO_DEPS_BLOCK_RE = re.compile(
    r"\ndependencies:\n(?:" + re.escape(GEN_MARKER_START) + r")?"
    r"((?:  - .*\n)+)" + r"(?:" + re.escape(GEN_MARKER_END) + r")?")


def extract_deps_block(text: str, path: Path) -> str:
    m = HEAD_DEPS_BLOCK_RE.search(text)
    if not m:
        raise SystemExit(f"sync-haskell-bootstrap-demo-deps.py: no top-level "
                          f"'dependencies:' block found in {path}")
    return m.group(1)


def rewrite_demo(demo_text: str, head_deps_block: str) -> str:
    generated = GEN_MARKER_START + head_deps_block + GEN_MARKER_END
    new_text, count = DEMO_DEPS_BLOCK_RE.subn("\ndependencies:\n" + generated, demo_text, count=1)
    if count != 1:
        raise SystemExit("sync-haskell-bootstrap-demo-deps.py: failed to locate "
                          f"'dependencies:' block in {DEMO_PKG_YAML}")
    return new_text


def main(argv: list[str]) -> int:
    check_only = "--check" in argv

    head_text = HEAD_PKG_YAML.read_text()
    demo_text = DEMO_PKG_YAML.read_text()

    head_deps_block = extract_deps_block(head_text, HEAD_PKG_YAML)
    new_demo_text = rewrite_demo(demo_text, head_deps_block)

    if check_only:
        if new_demo_text == demo_text:
            print("sync-haskell-bootstrap-demo-deps.py: in sync")
            return 0
        print(f"sync-haskell-bootstrap-demo-deps.py: {DEMO_PKG_YAML} dependencies "
              "are OUT OF SYNC with heads/haskell/package.yaml. "
              "Run bin/lib/sync-haskell-bootstrap-demo-deps.py to fix.",
              file=sys.stderr)
        return 1

    if new_demo_text != demo_text:
        DEMO_PKG_YAML.write_text(new_demo_text)
        print(f"Wrote {DEMO_PKG_YAML} (dependencies synced from {HEAD_PKG_YAML})")
    else:
        print(f"{DEMO_PKG_YAML}: already in sync")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
