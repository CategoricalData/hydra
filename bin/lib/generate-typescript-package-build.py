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

# For hydra-kernel the main entry point exports the top-level core module.
# Other packages are multi-module namespace directories with no single
# umbrella file (matching the shipping Python PEP 420 namespace-package
# model) — they have no "." export; consumers import specific submodules
# via the "./dist/*.js" subpath export instead.
PKG_MAIN_MODULE: dict[str, str] = {
    "hydra-kernel": "hydra/core",
}


def main_module(pkg_name: str) -> str | None:
    return PKG_MAIN_MODULE.get(pkg_name)


def render_package_json(
    name: str, description: str, version: str, deps: list[str], readme_rel: str | None
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

    mod = main_module(name)
    safe_desc = description.replace('"', '\\"')
    readme_field = f',\n  "readme": "{readme_rel}"' if readme_rel else ""

    main_types_fields = (
        f'\n  "main": "./dist/{mod}.js",\n  "types": "./dist/{mod}.d.ts",' if mod else ""
    )
    dot_export = (
        f"""    ".": {{
      "import": "./dist/{mod}.js",
      "types": "./dist/{mod}.d.ts"
    }},
"""
        if mod
        else ""
    )

    return f"""\
{{
  "name": "{name}",
  "version": "{version}",
  "description": "{safe_desc}",
  "type": "module",{main_types_fields}
  "exports": {{
{dot_export}    "./dist/*.js": {{
      "import": "./dist/*.js",
      "types": "./dist/*.d.ts"
    }}
  }},
  "files": [
    "dist/**/*.js",
    "dist/**/*.d.ts",
    "dist/**/*.js.map",
    "LICENSE",
    "NOTICE"
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
    "src/main/typescript/hydra/bootstrap.ts",
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

    pkg_json_path_out = os.path.join(out_dir, "package.json")
    with open(pkg_json_path_out, "w") as f:
        f.write(render_package_json(pkg_name, description, version, deps, readme_rel))
    print(f"  wrote {pkg_json_path_out}")

    tsconfig_path = os.path.join(out_dir, "tsconfig.build.json")
    with open(tsconfig_path, "w") as f:
        f.write(render_tsconfig_build(pkg_name))
    print(f"  wrote {tsconfig_path}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
