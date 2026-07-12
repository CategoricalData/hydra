#!/usr/bin/env python3
"""Remove compiled output for symlinked (non-own) TypeScript sources from a
publish-ready dist/typescript/<pkg>/ package, and rewrite the retained
modules' relative imports of those foreign sources to point at the sibling
npm package's public subpath export instead (#584).

Background: heads/typescript/bin/assemble-distribution.sh symlinks the
kernel (and, for hydra-pg, hydra-rdf; for hydra-java/hydra-scala, hydra-jvm)
`.ts` tree into each downstream package's own source tree, so `tsc` can
compile/typecheck it self-contained. `tsc` has no `include`/`exclude` option
that stops it from emitting a file that's transitively imported by an
included root (confirmed: both only prune the initial root-file set, not
the full program), so the compile step in publish-npm.sh unavoidably emits
JS/d.ts for those symlinked sources too, alongside the package's own
modules — this is the duplication #584 reports.

Since all files (own and symlinked) are compiled as one `tsc` program
rooted at <pkg>/src/main/typescript/, cross-file imports between them are
emitted as plain relative specifiers (e.g. `import * as X from
"../annotations.js"`) with no notion that some of those files are
"logically" a separate npm package. So simply deleting the compiled output
for symlinked sources (pruning) leaves those relative imports dangling.
This script therefore does both, in one pass:

  1. Prune: delete .js/.js.map/.d.ts/.d.ts.map compiled from a symlinked
     source (individual file or whole symlinked subdirectory).
  2. Rewrite: in every *retained* .js/.d.ts file, rewrite `from "<relative
     specifier>"` where the specifier resolves to a pruned path, to
     `<owning-sibling-package>/dist/<path>.js` — the same public subpath
     each package's package.json already exports (`"./dist/*.js"`).

Ownership of a symlinked source is derived from the symlink's target path:
resolving through dist/typescript/<owner>/src/main/typescript/... names the
owner unambiguously (assemble-distribution.sh only ever symlinks from one
sibling package's own tree into another's).

Usage:
  prune-typescript-foreign-modules.py <pkgdir>

  <pkgdir>  A dist/typescript/<pkg>/ directory that has already been
            compiled (i.e. <pkgdir>/dist/ exists, from `tsc --project
            tsconfig.build.json`).
"""

from __future__ import annotations

import os
import re
import shutil
import sys


SRC_SUBPATH = os.path.join("src", "main", "typescript")
FROM_SPECIFIER_RE = re.compile(r'(from\s+["\'])(\.\.?/[^"\']+)(["\'])')


def find_symlinks(src_root: str) -> list[str]:
    """Top-level-first walk that finds symlinks (file or directory) under
    src_root without descending into symlinked directories (a symlinked dir
    is reported once, as itself; its contents are not independently listed,
    since a plain, non-`-L` walk doesn't follow directory symlinks)."""
    found = []
    for dirpath, dirnames, filenames in os.walk(src_root):
        # Don't descend into symlinked directories; record them and prune
        # them from dirnames so os.walk doesn't follow them.
        real_dirnames = []
        for d in dirnames:
            full = os.path.join(dirpath, d)
            if os.path.islink(full):
                found.append(full)
            else:
                real_dirnames.append(d)
        dirnames[:] = real_dirnames
        for f in filenames:
            full = os.path.join(dirpath, f)
            if os.path.islink(full):
                found.append(full)
    return found


def owning_package(symlink_target: str, dist_typescript_root: str) -> str | None:
    """Given a resolved symlink target, name the sibling package (under
    dist/typescript/) whose source tree it lives in, or None if it doesn't
    match the expected dist/typescript/<pkg>/src/main/typescript/ shape."""
    target = os.path.realpath(symlink_target)
    root = os.path.realpath(dist_typescript_root)
    if not target.startswith(root + os.sep):
        return None
    rel = target[len(root) + 1 :]
    parts = rel.split(os.sep)
    if len(parts) < 4 or parts[1:4] != ["src", "main", "typescript"]:
        return None
    return parts[0]


def relpath_under_hydra(symlink_path: str, src_root: str) -> str:
    """Path of a symlink relative to <pkgdir>/src/main/typescript/, without
    a trailing .ts extension (used to map to the compiled dist/ path)."""
    rel = os.path.relpath(symlink_path, src_root)
    if rel.endswith(".ts"):
        rel = rel[: -len(".ts")]
    return rel


def prune_and_map(pkgdir: str) -> dict[str, str]:
    """Delete compiled output for symlinked sources; return a map of
    {compiled-relpath-without-extension -> owning-package} for every pruned
    path, for use by the rewrite pass."""
    src_root = os.path.join(pkgdir, SRC_SUBPATH)
    out_root = os.path.join(pkgdir, "dist")
    # pkgdir is dist/typescript/<pkg>/, so its parent is dist/typescript/.
    dist_typescript_root = os.path.dirname(os.path.normpath(pkgdir))

    pruned_map: dict[str, str] = {}
    for symlink in find_symlinks(src_root):
        owner = owning_package(symlink, dist_typescript_root)
        rel = relpath_under_hydra(symlink, src_root)
        if owner is not None:
            pruned_map[rel] = owner

        out_path_dir = os.path.join(out_root, rel)
        if os.path.isdir(symlink):
            if os.path.isdir(out_path_dir):
                shutil.rmtree(out_path_dir)
        else:
            for ext in (".js", ".js.map", ".d.ts", ".d.ts.map"):
                f = out_path_dir + ext
                if os.path.isfile(f):
                    os.remove(f)
    return pruned_map


def rewrite_imports(pkgdir: str, pruned_map: dict[str, str]) -> int:
    """Rewrite relative import/export specifiers in retained .js/.d.ts files
    that point at a pruned path, to the owning sibling package's public
    dist/*.js subpath. Returns the number of specifiers rewritten."""
    out_root = os.path.join(pkgdir, "dist")
    rewritten = 0

    for dirpath, _, filenames in os.walk(out_root):
        for fname in filenames:
            if not (fname.endswith(".js") or fname.endswith(".d.ts")):
                continue
            fpath = os.path.join(dirpath, fname)
            with open(fpath, encoding="utf-8") as f:
                text = f.read()

            def replace(m: re.Match) -> str:
                nonlocal rewritten
                prefix, specifier, suffix = m.group(1), m.group(2), m.group(3)
                # specifier is relative to fpath's directory; resolve it to a
                # path relative to out_root, matching pruned_map's keys.
                resolved = os.path.normpath(os.path.join(dirpath, specifier))
                rel_to_out = os.path.relpath(resolved, out_root)
                rel_no_ext = rel_to_out[: -len(".js")] if rel_to_out.endswith(".js") else rel_to_out
                owner = pruned_map.get(rel_no_ext)
                if owner is None:
                    return m.group(0)
                rewritten += 1
                new_specifier = f"{owner}/dist/{rel_no_ext}.js"
                return f"{prefix}{new_specifier}{suffix}"

            new_text = FROM_SPECIFIER_RE.sub(replace, text)
            if new_text != text:
                with open(fpath, "w", encoding="utf-8") as f:
                    f.write(new_text)
    return rewritten


def main() -> int:
    if len(sys.argv) != 2:
        print("usage: prune-typescript-foreign-modules.py <pkgdir>", file=sys.stderr)
        return 2
    pkgdir = sys.argv[1]
    if not os.path.isdir(os.path.join(pkgdir, "dist")):
        print(f"error: {pkgdir}/dist does not exist (compile first)", file=sys.stderr)
        return 1

    pruned_map = prune_and_map(pkgdir)
    rewritten = rewrite_imports(pkgdir, pruned_map)
    print(f"  Pruned {len(pruned_map)} symlinked-foreign source path(s)")
    print(f"  Rewrote {rewritten} import specifier(s) to sibling package(s)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
