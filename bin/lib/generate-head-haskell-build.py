#!/usr/bin/env python3
"""#370 — emit heads/haskell/{package.yaml,stack.yaml} for the chosen host mode.

The Haskell HOST (heads/haskell's exec drivers) can either compile the kernel +
Haskell-coder runtime from the local dist trees (local mode, today's behavior) or
link them as PUBLISHED Hackage dependencies (published mode, the #370 default).
Only the host changes; dist/haskell/hydra-kernel is still generated as a *target*.

This script does NOT re-template the whole package.yaml. The committed
heads/haskell/{package.yaml,stack.yaml} ARE the local-mode source of truth
(hand-maintained). For published mode we apply a small, well-defined transform to
them in place:

  - drop the library source-dirs for any coder package consumable from Hackage
    (probed via `hydra-packages.py haskell-hackage <pkg>` — today hydra-kernel +
    hydra-haskell; a coder published in a future cycle flips automatically),
  - add those packages to the library `dependencies`,
  - write `extra-deps: [hydra-<pkg>-<ver>, ...]` into stack.yaml.

`--mode local` restores the committed files verbatim (the foolproof escape /
release-cutting / migration-shim path). A package with hostOverrides[pkg]
== "local" is treated as not-consumable (kept local) even in published mode,
because `haskell-hackage` returns non-zero for it.

Usage:
  generate-head-haskell-build.py --mode {published,local} [--print]

--print writes nothing; it prints the computed (consumed, local) partition.
"""

import subprocess
import sys
from pathlib import Path

HEAD = Path(__file__).resolve().parent.parent.parent / "heads" / "haskell"
ROOT = Path(__file__).resolve().parent.parent.parent
PKG_YAML = HEAD / "package.yaml"
STACK_YAML = HEAD / "stack.yaml"
HYDRA_PACKAGES = ROOT / "bin" / "lib" / "hydra-packages.py"

# The Haskell coder packages whose dist/haskell/<pkg>/src/main/haskell tree the
# host compiles, and which COULD be consumed from Hackage instead. (The data
# domains rdf/pg/ext/wasm/bench and the native-driver java/python coders are not
# candidates here.) Probed per-package; only those actually on Hackage are
# consumed.
HACKAGE_CANDIDATES = [
    "hydra-kernel",
    "hydra-haskell",
    "hydra-coq",
    "hydra-typescript",
    "hydra-scala",
    "hydra-lisp",
    "hydra-go",
]

GEN_HEADER = (
    "# Note: this is an automatically generated file (bin/lib/"
    "generate-head-haskell-build.py, #370). Do not edit.\n"
    "# Regenerate via heads/haskell/bin/sync-haskell.sh "
    "[--published-host|--local-host].\n"
    "# The committed (local-mode) form is the hand-maintained source of truth.\n"
)


def consumed_from_hackage() -> dict[str, str]:
    """{pkg: version} for each candidate actually consumable from Hackage now."""
    out = {}
    for pkg in HACKAGE_CANDIDATES:
        r = subprocess.run(
            ["python3", str(HYDRA_PACKAGES), "haskell-hackage", pkg],
            capture_output=True, text=True, env={"HYDRA_ROOT_DIR": str(ROOT)})
        if r.returncode == 0 and r.stdout.strip():
            out[pkg] = r.stdout.strip()
    return out


def git_show_committed(path: Path) -> str:
    """The committed (HEAD) contents of a tracked file = the local-mode form."""
    rel = path.relative_to(ROOT)
    r = subprocess.run(
        ["git", "-C", str(ROOT), "show", f"HEAD:{rel.as_posix()}"],
        capture_output=True, text=True)
    if r.returncode != 0:
        # Fall back to on-disk (e.g. uncommitted local-mode edits); strip a
        # previously-generated header if present.
        text = path.read_text()
        return _strip_header(text)
    return r.stdout


def _strip_header(text: str) -> str:
    lines = text.splitlines(keepends=True)
    while lines and lines[0].startswith("# Note: this is an automatically generated") \
            or (lines and lines[0].startswith("# Regenerate via")) \
            or (lines and lines[0].startswith("# The committed (local-mode)")):
        lines.pop(0)
    return "".join(lines)


def transform_package_yaml(local_text: str, consumed: dict[str, str]) -> str:
    """Published-mode package.yaml: drop each consumed coder's dist main
    source-dir from the library, and add the consumed packages to the top-level
    `dependencies` block (which the library + execs + tests all inherit)."""
    drop_dirs = {
        f"- ../../dist/haskell/{pkg}/src/main/haskell" for pkg in consumed
    }
    dep_lines = "".join(
        f"  - {pkg:<29} == {ver}\n" for pkg, ver in sorted(consumed.items()))

    out = []
    injected = False
    for line in local_text.splitlines(keepends=True):
        if line.strip() in drop_dirs:
            continue
        out.append(line)
        # Inject the consumed Hackage deps once, right after the top-level
        # `dependencies:` header (column 0 — the library's own block is indented).
        if not injected and line.rstrip() == "dependencies:":
            out.append(dep_lines)
            injected = True
    return GEN_HEADER + "\n" + "".join(out)


def transform_stack_yaml(local_text: str, consumed: dict[str, str]) -> str:
    """Published-mode stack.yaml: extra-deps with the consumed Hackage pins."""
    extra = "".join(f"  - {pkg}-{ver}\n" for pkg, ver in sorted(consumed.items()))
    lines = local_text.splitlines(keepends=True)
    out = []
    replaced = False
    for line in lines:
        if line.rstrip() == "extra-deps:":
            out.append("extra-deps:\n")
            out.append(extra)
            replaced = True
        else:
            out.append(line)
    text = "".join(out)
    if not replaced:
        text += "\nextra-deps:\n" + extra
    return GEN_HEADER + "\n" + text


def main(argv: list[str]) -> int:
    mode = None
    do_print = False
    i = 0
    while i < len(argv):
        a = argv[i]
        if a == "--mode" and i + 1 < len(argv):
            mode = argv[i + 1]; i += 2
        elif a == "--print":
            do_print = True; i += 1
        elif a in ("-h", "--help"):
            print(__doc__, end=""); return 0
        else:
            print(f"unknown arg: {a}", file=sys.stderr); return 2
    if mode not in ("published", "local"):
        print("--mode {published,local} required", file=sys.stderr); return 2

    consumed = consumed_from_hackage() if mode == "published" else {}
    local_pkg = git_show_committed(PKG_YAML)
    local_stack = git_show_committed(STACK_YAML)

    if do_print:
        print(f"mode: {mode}")
        print(f"consumed from Hackage: {consumed}")
        print(f"local (compiled): {[p for p in HACKAGE_CANDIDATES if p not in consumed]}")
        return 0

    if mode == "local":
        # Restore the committed (hand-maintained) form verbatim.
        PKG_YAML.write_text(local_pkg)
        STACK_YAML.write_text(local_stack)
    else:
        PKG_YAML.write_text(transform_package_yaml(local_pkg, consumed))
        STACK_YAML.write_text(transform_stack_yaml(local_stack, consumed))
    print(f"Wrote heads/haskell/{{package.yaml,stack.yaml}} ({mode}; "
          f"consumed: {','.join(sorted(consumed)) or 'none'})", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
