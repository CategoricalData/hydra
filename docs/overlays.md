# Overlays

An **overlay** is hand-written, host-native source that a Hydra distribution package needs but that
cannot be expressed as translingual DSL. Overlays live under `overlay/<lang>/<pkg>/` and are *copied
onto* the generated distribution at assembly time. This page describes the overlay system end to end:
what belongs in an overlay, the namespace rules, how host-specific third-party integrations are folded
into overlays, and how each package's build configuration (`build.json`) becomes a generated
`build.gradle` / `pyproject.toml`.

For the surrounding build pipeline (phases, caches, sync), see
[build-system.md](build-system.md); this page is the dedicated reference for the overlay layer.

## The governing equation

> **`dist/<lang>/<pkg>/`  =  transform(`packages/<pkg>/`)  +  copy(`overlay/<lang>/<pkg>/`)**

A distribution package is the combination of two inputs:

- **transform** — modules generated from the translingual source package (`packages/<pkg>/`) into the
  target language; and
- **copy** — hand-written files copied verbatim from the overlay tree (`overlay/<lang>/<pkg>/`).

Two load-bearing invariants follow:

1. **Only the copy step reads `overlay/`.** No head build, test config, or IDE project references the
   overlay tree directly — its sole purpose is to feed `dist/`.
2. **Heads depend on `dist/`, never on `overlay/`.** A head consumes the `dist/` copy of any runtime,
   not the overlay source. (Generation *drivers* and a head's own test runners are not shipped and stay
   under `heads/`.)

## What goes in `packages/` vs `overlay/`

The boundary follows the translingual line:

- **`packages/<pkg>/`** — code that describes Hydra modules *independently of any target language* (it
  becomes valid in every target after transformation): DSL module definitions, plus host-authored DSL
  helpers whose purpose is to *write* those definitions.
- **`overlay/<lang>/<pkg>/`** — host-native, language-specific source that must ship but cannot be
  translingual: primitive implementations (the actual Python/Java/Haskell bodies behind `hydra.lib.*`),
  runtime data structures (`cons_list.py`, `PersistentMap.java`), test bridges (`test_env.py`), and
  third-party library integrations (see below).

Heuristic: *if the code could be expressed in the Hydra DSL and generated into every target, it belongs
in `packages/`; if it is inherently host-specific, it belongs in `overlay/`.*

## Namespace convention

Overlay files use the `hydra.overlay.<lang>.*` namespace (e.g. `hydra.overlay.python.lib.chars`,
`hydra.overlay.java.lib.Chars`, `Hydra.Overlay.Haskell.Lib.Chars`). This enforces a hard boundary:

- **`hydra.*`** (including `hydra.<lang>.*`, `hydra.dsl.*`, `hydra.lib.*`) is exclusively *translingual*
  — every name is generated or derived from generated code.
- **`hydra.overlay.<lang>.*`** is exclusively *host-native* — hand-written, under `overlay/`.

When folding host-specific source into an overlay, only the genuinely host-native classes move to
`hydra.overlay.<lang>.*`; references to *generated* translingual types stay in `hydra.*`. For example,
the Cypher parser overlay is `hydra.overlay.java.cypher.FromCypher`, but it imports the generated Cypher
AST as `hydra.cypher.openCypher.*` and the query model as `hydra.pg.query.*` — those do **not** move.

## Folding host-specific integrations into overlays

Third-party integrations — adapters wiring a Hydra package to an external library (rdf4j, Apache
TinkerPop, ANTLR-based Cypher/GQL parsers) — are inherently host-specific, so they are overlays. They
were previously separate `bindings/<host>/<artifact>/` projects; as of #511 they are folded into the
relevant distribution package's overlay tree:

| Former binding | Now folded into |
|----------------|-----------------|
| `hydra-rdf4j` (Eclipse rdf4j) | `overlay/java/hydra-rdf/` (`hydra.overlay.java.rdf`) |
| `hydra-pg-dsl` (PG builders) | `overlay/java/hydra-pg/` (`hydra.overlay.java.pg.dsl`) |
| `hydra-neo4j` (Cypher/GQL parsers) | `overlay/java/hydra-pg/` (`hydra.overlay.java.{cypher,gql,tools}`) |
| `hydra-tinkerpop` (Gremlin bridge) | `overlay/{java,python}/hydra-pg/` (`hydra.overlay.{java,python}.tinkerpop`) |

The overlay tree carries the whole `src/` subtree — `src/main/<lang>/`, `src/main/antlr/` (ANTLR `.g4`
grammars), `src/test/<lang>/`, and `src/test/resources/` — so grammars, tests, and resources are
copied alongside the hand-written classes.

## Build configuration: `build.json` → generated build file

A folded integration usually needs **third-party build configuration** (external dependencies, build
plugins) that the generic per-package build does not provide. Overlays express this as a single file:

> **`overlay/<lang>/<pkg>/build.json`** — the JSON encoding of a Hydra build-configuration value.

The on-disk `build.json` is *exactly* the canonical JSON encoding of a Hydra type:

- **Java / Gradle:** `hydra.gradle.GradleBuildConfiguration` — `dependencies` (with `scope`:
  api/runtime/test/tool), `excludes`, `extraSourceDirs`, `plugins`, and an `antlr` block
  (`{arguments, outputDirectory}`) for grammar generation.
- **Python / PEP 621:** `hydra.python.pyproject.PyProjectBuildConfiguration` — `dependencies` (scope
  partitions them into `[project.dependencies]` vs `[project.optional-dependencies]`).

The dependency vocabulary (`PackageDependency`, `VersionSpecifier`, `DependencyScope`) is shared in
`hydra.packaging`; each build system adds only its own fields. `VersionSpecifier` covers `any`,
`exact`, `atLeast`, and `range` (rendered to each ecosystem's syntax, e.g. a range becomes `>=3.7,<4.0`
for PyPI or `[3.7,4.0)` for Maven).

> **Why keyed differently:** `hydra.gradle` is keyed by *build system* (Gradle spans languages —
> Java, Clojure, …), whereas `hydra.python.pyproject` is keyed by *language* (pyproject/PEP 621 is
> Python-specific). The axis is whatever the config actually varies over.

### The generation flow

```
overlay/<lang>/<pkg>/build.json
        │  (read by the per-package build generator)
        ▼
bin/lib/generate-<lang>-package-build.py
        │  merges Hydra inter-package deps (from package.json)
        │  + the overlay's third-party deps/plugins/antlr config
        ▼
dist/<lang>/<pkg>/build.gradle   (Java)
dist/<lang>/<pkg>/pyproject.toml (Python)
```

The generated build file (`dist/<lang>/<pkg>/build.gradle` or `pyproject.toml`) is the standalone
build for that distribution package. It is **generated, never hand-edited**: the generic generator
supplies the boilerplate (plugins, group/version, repositories, POM/PEP-621 metadata, the project's
own inter-package Hydra dependencies), and the overlay `build.json` supplies the integration-specific
deps, plugins, and (for ANTLR) grammar-generation configuration.

> **Interim note:** the generators currently read `build.json` directly (a plain JSON parse). The
> format is the canonical encoding of the Hydra build-configuration types, so when the build system is
> nativized (#416) the hand parse is replaced by the generated `hydra.decode.*` decoders with no change
> to the on-disk format.

## How overlays survive regeneration (prune protection)

Because overlay files are *copied* (not generated), they are not in a package's recorded output
digest. The copy step is `heads/<lang>/bin/copy-overlay.sh` for Java/Python (the #511
generalization; `copy-kernel-runtime.sh` remains the kernel-only batch path),
`copy-kernel-runtime.sh` for TypeScript/Scala/Go, `overlay-kernel-runtime.sh` for Haskell, and
`lisp_copy_overlay` (in `heads/lisp/bin/common.sh`) for the Lisp dialects. To keep regeneration
from deleting the copied files, the copy step records every overlay file in a
**keep-paths manifest** (keyed by source-set directory), which is honored by both prune paths:

- `bootstrap-from-json --prune-stale` (the full-regeneration prune), and
- `digest-check fresh` (the cache-hit reconcile).

This is why a sync can regenerate a package's modules without clobbering its hand-written overlay.

## Routing a new overlay-only / native module

A package's modules are routed to their owning package by name. A *generated* module is routed
automatically from the package's declared module list. A **native module** authored only in an
overlay/host DSL (e.g. the build-config modules `hydra.gradle`, `hydra.python.pyproject`) has no
generated Haskell `Module` value, so it must be declared explicitly. Wiring a new native build-config
module requires four registrations (the routing is fail-loud — an unrouted module aborts the build):

1. the host package's manifest `mainModules` (e.g. `Manifest.java` / `manifest.py`);
2. the host's JSON driver module list (`UpdateJavaJson` `SOURCE_CLASS_NAMES` /
   `update-python-json.py` `SOURCE_MODULE_NAMES`);
3. `heads/haskell/src/main/haskell/Hydra/Sources/Ext.hs` `extRoutingInput`;
4. (Java only) `PACKAGE_PREFIXES` for any derived encode/decode coder modules.

## See also

- [build-system.md](build-system.md) — the full build/sync pipeline (phases, caches, consume model).
- [Code organization](https://github.com/CategoricalData/hydra/wiki/Code-organization) — the
  human-facing architecture overview.
