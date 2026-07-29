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
# The top-bar project-name title. Each page's title is static HTML (no JS ever
# rewrites it — verified) and scaladoc does a full browser navigation on link
# clicks (no client-side page swap), so every freshly-loaded page shows its own
# correct package name. We keep the title but reword it to "<pkg> package" for
# clarity. The project name inside varies (it's the sbt module name, which equals
# our --package), so capture and replace just the inner text. Depth-varying href
# is preserved untouched.
TITLE_RE = re.compile(
    r'(<a href="[^"]*" class="logo-container">'
    r'<span class="project-name[^"]*">)[^<]*(</span></a>)')
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

    rewrites, files, labels = 0, 0, 0
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

        # Reword the top-bar title to "<pkg> package" for clarity. Each page's
        # title is static HTML naming its own package; the SPA-disable script
        # below guarantees a full page load on cross-tree clicks, so the title
        # (and search index, and sidebar) always match the page you land on.
        new, n = TITLE_RE.subn(rf'\g<1>{a.package} package\g<2>', new)
        labels += n

        # Hide the "API" tab switcher at the top of the sidebar. It is scaladoc's
        # API/Docs tab control, but we ship no static _docs site, so it is a lone
        # always-selected tab that switches to nothing. ux.js re-renders that
        # element from a menu config on load, so removing it from the static HTML
        # would not stick (and querying a removed node risks a JS error); hiding
        # it with CSS is robust against the re-render. Injected once per page.
        if "</head>" in new and "hydra-hide-switcher" not in new:
            new = new.replace(
                "</head>",
                '<style id="hydra-hide-switcher">'
                '#leftColumn .switcher-container{display:none}</style></head>',
                1)

        # Force a full page load when navigating BETWEEN package trees. Scaladoc's
        # ux.js turns every same-origin link click into an SPA transition: it
        # fetches the target, swaps #main + #leftColumn and the document title, but
        # leaves the top-bar project name AND the per-page scripts (crucially the
        # per-package searchData.js) belonging to the ORIGIN tree. That is fine for
        # a single unified site, but here each package is a SEPARATE tree with its
        # own header, search index and assets — so an SPA hop into another tree
        # leaves a stale title and a search box that still searches the origin
        # package. We install a capture-phase click listener (runs before ux.js's
        # bubble-phase handler) that, for any link pointing into a DIFFERENT
        # /<...>/scaladoc/<pkg>/ tree, stops the SPA handler and lets the browser
        # navigate normally — reloading the correct tree whole. Same-tree links
        # keep the fast SPA behavior.
        if "</body>" in new and "hydra-full-nav" not in new:
            script = (
                '<script id="hydra-full-nav">(function(){'
                'function pkg(p){var m=p.match(/\\/scaladoc\\/([^\\/]+)\\//);'
                'return m?m[1]:null;}'
                'document.addEventListener("click",function(e){'
                'var a=e.target.closest&&e.target.closest("a");'
                'if(!a||!a.href)return;'
                'var u;try{u=new URL(a.href);}catch(_){return;}'
                'if(u.origin!==location.origin)return;'
                'var to=pkg(u.pathname),here=pkg(location.pathname);'
                'if(to&&here&&to!==here){e.stopImmediatePropagation();'
                'location.href=a.href;}'
                '},true);})();</script></body>'
            )
            new = new.replace("</body>", script, 1)

        if new != txt:
            files += 1
            if not a.dry_run:
                hp.write_text(new, encoding="utf-8")

    print(f"[{a.package}] linkable dep symbols={len(linkmap)} "
          f"title-rewords={labels} "
          f"rewrote={rewrites} across {files} files"
          f"{' (dry-run)' if a.dry_run else ''}")


if __name__ == "__main__":
    main()
