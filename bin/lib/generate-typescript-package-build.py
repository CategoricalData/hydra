#!/usr/bin/env python3
"""Generate a standalone package.json + tsconfig.build.json for one Hydra
TypeScript distribution package under dist/typescript/<pkg>/.

Each emitted build is self-contained: from `dist/typescript/<pkg>/`, running
`npm pack` produces a tarball, and `npm publish` uploads it to npm. The
package.json declares inter-Hydra `dependencies` by exact version, so
a consumer that adds e.g. `hydra-rdf` to their project automatically pulls
`hydra-kernel` transitively.

Inputs:
  packages/<pkg>/package.json   (read for name, description, dependencies)
  hydra.json                    (read for currentVersion)

Outputs:
  dist/typescript/<pkg>/package.json       (publishable npm manifest)
  dist/typescript/<pkg>/tsconfig.build.json  (emit JS + .d.ts for the publish)
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
import sys


HOMEPAGE = "https://github.com/CategoricalData/hydra"
LICENSE = "Apache-2.0"
AUTHOR = "Joshua Shinavier and collaborators"
ENGINES_NODE = ">=20"

# Per-package external (non-Hydra) npm dependencies. Currently empty for all
# packages: TinkerPop/RDF native integrations live in overlay/<lang>/<pkg>/ (#511)
# exists, so no third-party npm deps are pulled into the published packages.
EXTERNAL_DEPS: dict[str, list[str]] = {}

# #786: package-root directory the kernel's JSON module universe is bundled
# under (shared name convention with the Java/Scala classpath resource).
KERNEL_JSON_RESOURCE_SUBDIR = "hydra-kernel-json"


def copy_kernel_json_resources(repo_root: str, package: str, out_dir: str) -> None:
    """#786: for hydra-kernel only, copy dist/json/hydra-kernel/src/main/json
    into <out_dir>/hydra-kernel-json/ so the published npm package bundles the
    kernel's module universe. Placed at the package root rather than under
    dist/ (the TypeScript compiler's own output dir for this package) to avoid
    confusion between the two unrelated "dist" trees. A no-op for every other
    package.
    """
    if package != "hydra-kernel":
        return
    src = os.path.join(repo_root, "dist", "json", "hydra-kernel", "src", "main", "json")
    if not os.path.isdir(src):
        print(f"error: missing kernel JSON source dir: {src}", file=sys.stderr)
        sys.exit(1)
    dest = os.path.join(out_dir, KERNEL_JSON_RESOURCE_SUBDIR)
    if os.path.isdir(dest):
        shutil.rmtree(dest)
    shutil.copytree(src, dest)


# All Hydra TypeScript packages are subpath-only by design (#600): none gets
# a "." export, "main", or "types" field. Consumers import specific submodules
# via the "./dist/*.js" subpath export instead (documented in
# docs/getting-started.md).
#
# DO NOT "FIX" THIS by pointing "." at one module (e.g. hydra-kernel ->
# hydra/core, which this generator briefly did). That is a misleading easy
# fix: it privileges one arbitrary module as *the* entry point among many,
# which is exactly the asymmetry that made #600 look like a bug in the first
# place (hydra-kernel had a hand-picked front door; hydra-build, hydra-pg,
# hydra-rdf, hydra-typescript did not). A principled root export would
# re-export symbols from MANY modules at once while resolving the cross-module
# name collisions that arise — hydra-pg alone has 35 modules across
# hydra/{pg,cypher,tinkerpop,neo4j,graphviz,...}, several sharing a basename
# (model.ts, coder.ts, syntax.ts, mapping.ts each appear under 2+ different
# subdirectories), so a naive flat re-export barrel would collide. Until such
# a collision-safe aggregate entry is designed, every package stays uniformly
# subpath-only. See #600.


def render_package_json(
    name: str, description: str, version: str, deps: list[str], readme_rel: str | None,
    package: str | None = None,
) -> str:
    hydra_deps: dict[str, str] = {d: version for d in deps}
    for ext_dep in EXTERNAL_DEPS.get(name, []):
        # external deps carry their own version spec
        dep_name, dep_ver = ext_dep.split("@", 1) if "@" in ext_dep else (ext_dep, "*")
        hydra_deps[dep_name] = dep_ver

    if hydra_deps:
        inner = json.dumps(hydra_deps, indent=2)
        # Re-indent inner lines so they align with the surrounding 2-space JSON.
        deps_block = "\n".join(
            ("  " + line if i > 0 else line) for i, line in enumerate(inner.splitlines())
        )
    else:
        deps_block = "{}"

    safe_desc = description.replace('"', '\\"')
    readme_field = f',\n  "readme": "{readme_rel}"' if readme_rel else ""

    # #786: hydra-kernel bundles its JSON module universe at the package root.
    files_list = ["dist/**/*.js", "dist/**/*.d.ts", "dist/**/*.js.map", "LICENSE", "NOTICE"]
    if package == "hydra-kernel":
        files_list.append(f"{KERNEL_JSON_RESOURCE_SUBDIR}/**/*.json")
    files_block = ",\n    ".join(f'"{f}"' for f in files_list)

    return f"""\
{{
  "name": "{name}",
  "version": "{version}",
  "description": "{safe_desc}",
  "type": "module",
  "exports": {{
    "./dist/*.js": {{
      "import": "./dist/*.js",
      "types": "./dist/*.d.ts"
    }}
  }},
  "files": [
    {files_block}
  ],
  "engines": {{
    "node": "{ENGINES_NODE}"
  }},
  "license": "{LICENSE}",
  "author": "{AUTHOR}",
  "homepage": "{HOMEPAGE}",
  "repository": {{
    "type": "git",
    "url": "git+{HOMEPAGE}.git"
  }},
  "dependencies": {deps_block}{readme_field}
}}
"""


def render_tsconfig_build(name: str) -> str:
    """tsconfig.build.json — emits compiled JS + .d.ts into dist/ subdir."""
    return f"""\
// Generated file. Do not edit.
// Used by publish-npm.sh to compile dist/typescript/{name}/src/main/typescript
// into dist/typescript/{name}/dist/ (JS + .d.ts) for npm packaging.
// bootstrap.ts is excluded: it imports from hydra-lisp (sibling pkg, absent here).
{{
  "compilerOptions": {{
    "target": "ES2022",
    "module": "NodeNext",
    "moduleResolution": "nodenext",
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,
    "outDir": "./dist",
    "rootDir": "./src/main/typescript",
    "strict": true,
    "skipLibCheck": true
  }},
  "include": ["src/main/typescript/**/*.ts"],
  "exclude": [
    "src/main/typescript/hydra/core/bootstrap.ts",
    "src/test"
  ]
}}
"""


def main() -> int:
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0] if __doc__ else None)
    p.add_argument("package", help="Package name (e.g. hydra-kernel)")
    p.add_argument(
        "--repo-root",
        default=os.environ.get("HYDRA_ROOT_DIR"),
        help="Hydra worktree root (default: $HYDRA_ROOT_DIR)",
    )
    p.add_argument(
        "--out-dir",
        help="Override output directory (default: <repo-root>/dist/typescript/<package>)",
    )
    args = p.parse_args()

    if not args.repo_root:
        print("error: --repo-root or $HYDRA_ROOT_DIR is required", file=sys.stderr)
        return 2

    pkg_json_path = os.path.join(args.repo_root, "packages", args.package, "package.json")
    if not os.path.isfile(pkg_json_path):
        print(f"error: no such package.json: {pkg_json_path}", file=sys.stderr)
        return 1

    with open(pkg_json_path) as f:
        meta = json.load(f)

    pkg_name = meta.get("name") or args.package
    description = meta.get("description") or pkg_name
    deps = list(meta.get("dependencies") or [])

    with open(os.path.join(args.repo_root, "hydra.json")) as f:
        version = json.load(f)["currentVersion"]

    out_dir = args.out_dir or os.path.join(args.repo_root, "dist", "typescript", args.package)
    os.makedirs(out_dir, exist_ok=True)

    readme_src = os.path.join(args.repo_root, "packages", args.package, "README.md")
    readme_rel: str | None
    if os.path.isfile(readme_src):
        shutil.copyfile(readme_src, os.path.join(out_dir, "README.md"))
        readme_rel = "README.md"
    else:
        readme_rel = None

    for fname in ("LICENSE", "NOTICE"):
        shutil.copyfile(os.path.join(args.repo_root, fname), os.path.join(out_dir, fname))

    # #786: bundle the kernel's JSON module universe at the package root.
    copy_kernel_json_resources(args.repo_root, args.package, out_dir)

    pkg_json_path_out = os.path.join(out_dir, "package.json")
    with open(pkg_json_path_out, "w") as f:
        f.write(render_package_json(pkg_name, description, version, deps, readme_rel, args.package))
    print(f"  wrote {pkg_json_path_out}")

    tsconfig_path = os.path.join(out_dir, "tsconfig.build.json")
    with open(tsconfig_path, "w") as f:
        f.write(render_tsconfig_build(pkg_name))
    print(f"  wrote {tsconfig_path}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
