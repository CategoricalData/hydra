#!/usr/bin/env python3
"""Inject cross-package hyperlinks into per-package Scaladoc trees.

Scala 3 scaladoc cannot cross-link separate per-package doc trees: a reference to
a type from a binary-package dependency renders as <span data-unresolved-link>.
This post-processor repairs those spans into real <a href> links, giving us
per-package trees (clean per-package left-nav) AND working cross-package links.

Disambiguation is safe: a name is only linked when it resolves to a UNIQUE symbol
owned by one of the package's DEPENDENCY packages. Within-package names are left
untouched (scaladoc already links those); ambiguous names are skipped.

Inputs:
  --search-data  path to the UNIFIED build's scripts/searchData.js (the FQN index)
  --tree         per-package api/ tree to rewrite in place
  --package      that tree's package name (e.g. hydra-jvm)
  --deps         space-separated dependency packages (e.g. "hydra-kernel")
  --pages-base   published base URL for sibling per-package trees
  --dist-root    dist/scala root; per-package top-level hydra.<ns> ownership is
                 derived from dist/scala/<pkg>/src/main/scala/hydra (tracks the
                 kernel as it evolves)
  [--dry-run]    report counts, do not write
"""
import argparse, json, re, collections, pathlib, sys

SPAN_RE = re.compile(r'<span data-unresolved-link="" t="t">([A-Za-z][A-Za-z0-9_]*)</span>')
TYPELIKE = {"type", "class", "trait", "object", "enum", "case class"}


def derive_ns_map(dist_root):
    """Map each package -> the top-level hydra.<ns> namespaces it owns, read from
    dist/scala/<pkg>/src/main/scala/hydra (a .scala file `foo.scala` -> namespace
    `foo`; a subdirectory `bar/` -> namespace `bar`)."""
    dist = pathlib.Path(dist_root)
    m = {}
    for pkgdir in sorted(dist.glob("hydra-*")):
        base = pkgdir / "src" / "main" / "scala" / "hydra"
        ns = set()
        if base.is_dir():
            for p in base.iterdir():
                if p.suffix == ".scala":
                    ns.add(p.stem)
                elif p.is_dir():
                    ns.add(p.name)
        m[pkgdir.name] = sorted(ns)
    return m


def top_ns(d):
    p = d.split(".")
    return p[1] if p and p[0] == "hydra" and len(p) > 1 else (p[0] if p else "")


def load_index(search_data):
    raw = open(search_data, encoding="utf-8").read()
    m = re.search(r"pages\s*=\s*(\[.*\])", raw, re.S)
    if not m:
        sys.exit("ERROR: could not parse searchData.js")
    return json.loads(m.group(1))


def build_linkmap(arr, owner, depset):
    cand = collections.defaultdict(set)
    for e in arr:
        n, d, l, k = e.get("n", ""), e.get("d", ""), e.get("l", ""), e.get("k", "")
        if not n or k not in TYPELIKE:
            continue
        op = owner.get(top_ns(d))
        if op in depset:
            cand[n].add((op, l))
    # unique-among-dependencies only
    return {n: next(iter(s)) for n, s in cand.items() if len(s) == 1}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--search-data", required=True)
    ap.add_argument("--tree", required=True)
    ap.add_argument("--package", required=True)
    ap.add_argument("--deps", default="")
    ap.add_argument("--pages-base", required=True)
    ap.add_argument("--dist-root", required=True)
    ap.add_argument("--dry-run", action="store_true")
    a = ap.parse_args()

    ns_map = derive_ns_map(a.dist_root)
    owner = {}
    for pk, nss in ns_map.items():
        for ns in nss:
            owner.setdefault(ns, pk)

    depset = set(a.deps.split())
    arr = load_index(a.search_data)
    linkmap = build_linkmap(arr, owner, depset)

    rewrites, files = 0, 0
    for hp in pathlib.Path(a.tree).rglob("*.html"):
        txt = hp.read_text(encoding="utf-8")

        def repl(m):
            nonlocal rewrites
            name = m.group(1)
            if name in linkmap:
                dep, url = linkmap[name]
                rewrites += 1
                return f'<a href="{a.pages_base}/{dep}/{url}">{name}</a>'
            return m.group(0)

        new = SPAN_RE.sub(repl, txt)
        if new != txt:
            files += 1
            if not a.dry_run:
                hp.write_text(new, encoding="utf-8")

    print(f"[{a.package}] linkable dep symbols={len(linkmap)} "
          f"rewrote={rewrites} across {files} files"
          f"{' (dry-run)' if a.dry_run else ''}")


if __name__ == "__main__":
    main()
