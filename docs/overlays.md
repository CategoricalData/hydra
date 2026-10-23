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

Two invariants follow:

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
  translingual: primitive implementations (the actual Python/Java/Haskell bodies behind `hydra.core.lib.*`),
  runtime data structures (`cons_list.py`, `PersistentMap.java`), test bridges (`test_env.py`), and
  third-party library integrations (see below).

Heuristic: *if the code could be expressed in the Hydra DSL and generated into every target, it belongs
in `packages/`; if it is inherently host-specific, it belongs in `overlay/`.*

## Namespace convention

Overlay files use an `overlay.<lang>` namespace segment placed after the package root:
`hydra.core.overlay.<lang>.*` for `hydra-kernel` overlays (e.g. `hydra.core.overlay.python.lib.chars`,
`hydra.core.overlay.java.lib.chars.IsAlphaNum`, `Hydra.Core.Overlay.Haskell.Lib.Chars`), and
`hydra.<root>.overlay.<lang>.*` for other packages' overlays (e.g. `hydra.pg.overlay.java.*`,
`hydra.build.overlay.java.*`). The one exception is the Scheme kernel overlay, which stays at
`hydra.overlay.scheme.*`. This enforces a hard boundary:

- **`hydra.*`** outside an `overlay` segment (including `hydra.core.*`, `hydra.core.dsl.*`,
  `hydra.core.lib.*`, and package roots such as `hydra.pg.*`) is exclusively *translingual*
  — every name is generated or derived from generated code.
- **`hydra.<root>.overlay.<lang>.*`** is exclusively *host-native* — hand-written, under `overlay/`.

When folding host-specific source into an overlay, only the genuinely host-native classes move to
the overlay namespace; references to *generated* translingual types keep their generated names. For
example, the Cypher parser overlay is `hydra.pg.overlay.java.cypher.FromCypher`, but it imports the
generated Cypher AST as `hydra.pg.cypher.openCypher.*` and the query model as `hydra.pg.query.*` —
those do **not** move.

### Bare primitive references: "implementation wins" (deferred: nullary undecidability)

When overlay emission resolves a bare reference to a `hydra.core.lib.*` primitive, the host-native
implementation (`hydra.core.overlay.<lang>.*`) takes precedence — "implementation wins."
This is applied unconditionally: the overlay-namespace rewrite operates on namespace strings
and performs no term-level applied-vs-nullary distinction.

There is a known, deliberately-deferred subtlety. For a *nullary* primitive, whether a bare
reference denotes the value or the (zero-arg) implementation is undecidable without
term-level context.
Today this is latent: no currently-emittable code exercises a case where the distinction
would matter, so "implementation wins" is safe.
Introducing an applied-vs-nullary branch in overlay emission would surface it.
The interim decision ("implementation wins") stands until then; see
[#655](https://github.com/CategoricalData/hydra/issues/655).
**Do not add a naive applied-vs-nullary heuristic to "fix" this without revisiting the #655
design discussion** — the current unconditional behavior is intentional.

## Folding host-specific integrations into overlays

Third-party integrations — adapters wiring a Hydra package to an external library (rdf4j, Apache
TinkerPop, ANTLR-based Cypher/GQL parsers) — are inherently host-specific, so they are overlays. They
were previously separate `bindings/<host>/<artifact>/` projects; as of #511 they are folded into the
relevant distribution package's overlay tree:

| Former binding | Now folded into |
|----------------|-----------------|
| `hydra-rdf4j` (Eclipse rdf4j) | `overlay/java/hydra-rdf/` (`hydra.rdf.overlay.java`) |
| `hydra-pg-dsl` (PG builders) | `overlay/java/hydra-pg/` (`hydra.pg.overlay.java.dsl`) |
| `hydra-neo4j` (Cypher/GQL parsers) | `overlay/java/hydra-pg/` (`hydra.pg.overlay.java.{cypher,gql,tools}`) |
| `hydra-tinkerpop` (Gremlin bridge) | `overlay/{java,python}/hydra-pg/` (`hydra.pg.overlay.{java,python}.tinkerpop`) |

The overlay tree carries the whole `src/` subtree — `src/main/<lang>/`, `src/main/antlr/` (ANTLR `.g4`
grammars), `src/test/<lang>/`, and `src/test/resources/` — so grammars, tests, and resources are
copied alongside the hand-written classes.

## Build configuration: `build.json` → generated build file

A folded integration usually needs **third-party build configuration** (external dependencies, build
plugins) that the generic per-package build does not provide. Overlays express this as a single file:

> **`overlay/<lang>/<pkg>/build.json`** — the JSON encoding of a Hydra build-configuration value.

The on-disk `build.json` is *exactly* the canonical JSON encoding of a Hydra type:

- **Java / Gradle:** `hydra.java.gradle.GradleBuildConfiguration` — `dependencies` (with `scope`:
  api/runtime/test/tool), `excludes`, `extraSourceDirs`, `plugins`, and an `antlr` block
  (`{arguments, outputDirectory}`) for grammar generation.
- **Python / PEP 621:** `hydra.python.pyproject.PyProjectBuildConfiguration` — `dependencies` (scope
  partitions them into `[project.dependencies]` vs `[project.optional-dependencies]`).

The dependency vocabulary (`PackageDependency`, `VersionSpecifier`, `DependencyScope`) is shared in
`hydra.core.packaging`; each build system adds only its own fields. `VersionSpecifier` covers `any`,
`exact`, `atLeast`, and `range` (rendered to each ecosystem's syntax, e.g. a range becomes `>=3.7,<4.0`
for PyPI or `[3.7,4.0)` for Maven).

> **Why keyed differently:** `hydra.java.gradle` is keyed by *build system* (Gradle spans languages —
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
> nativized (#416) the hand parse is replaced by the generated `hydra.<root>.decode.*` decoders with no change
> to the on-disk format.
> The build system's other JSON formats (`hydra.json`, `package.json`, digests, manifests) are
> specified the same way, by the `hydra.build.format` module (#512); see
> [build-system.md § Build-format schemas as Hydra types](build-system.md#build-format-schemas-as-hydra-types).

## How overlays survive regeneration (prune protection)

Because overlay files are *copied* (not generated), they are not in a package's recorded output
digest. A prune pass (`bootstrap-from-json --prune-stale`, the full-regeneration prune; or
`digest-check fresh`, the cache-hit reconcile) deletes anything under a package's output dir that
it does not recognize as part of what the generator just wrote — which, left unguarded, includes
every copied overlay file. There are exactly **two correct strategies** for preventing that, and
every host must use one:

### Strategy A: copy before prune, protect with a keep-paths manifest

The copy step runs first and additionally records every file it copied into a **keep-paths
manifest** (`<sourceSetDir>\t<relPath>` lines, keyed by source-set directory). Both prune paths —
`--prune-stale` and `digest-check fresh` — accept `--keep-paths-from <manifest>` and skip deleting
anything listed in it. The manifest is what makes the ordering safe despite the copy happening
first.

Used by: Haskell (per-package `assemble-distribution.sh`, which derives the manifest from the
overlay tree directly), Java, Python (`copy-overlay.sh` / `copy-kernel-runtime.sh`), and the Lisp
dialects (`lisp_copy_overlay` in `heads/lisp/bin/common.sh`, via the `--keep-paths-from` forwarded
in `heads/lisp/bin/common.sh`'s prune invocations).

### Strategy B: copy after prune

The prune runs first, with no keep-paths manifest at all, against a dist tree that does not yet
contain the overlay files — so there is nothing overlay-shaped for it to delete. The copy step
then runs strictly afterward and is the last thing to touch the tree.

Used by: Go and Scala (`apply-assembly-plan.sh <lang> hydra-kernel`, called as a numbered "Step 3"
*after* the `--prune-stale` transform step in each language's `assemble-distribution.sh`), and
Haskell's own all-packages batch path in `sync-haskell.sh` (`bootstrap-from-json --all-packages
--prune-stale` with no `--keep-paths-from`, followed immediately by a second, idempotent
`overlay-kernel-runtime.sh` invocation that repairs whatever the prune deleted).

### Both are correct; do not mix them for one copy step

Either strategy independently satisfies the same invariant — the overlay files are present in the
final dist tree after the pipeline step completes — so neither is "more correct." What is a bug is
a copy step that uses **neither**: copying before the prune without a keep-paths manifest, which is
exactly the failure this issue (#645) describes (`overlay-kernel-runtime.sh` ran once, before
`sync-haskell.sh`'s per-package assembles, with no manifest, and the Haskell-target assemble of
`hydra-typescript` pruned a file it had just copied in). Fixed in `3f9162bde4` by giving the
per-package Haskell path a derived keep-paths manifest (Strategy A); the batch path in
`sync-haskell.sh` relies on the separate post-prune re-invocation described above (Strategy B) and
always has.

When adding a new copy step (a new host, or a new package's overlay), pick one strategy up front
and follow its existing pattern verbatim — don't invent a third ordering. A copy step that applies
Strategy A must pass its manifest into *every* prune call it precedes; a copy step that applies
Strategy B must run *after every* prune call in its pipeline, with no prune call anywhere between
the copy and the point where the dist tree is considered final for that run.

A copy step's own success log line is not evidence of survival: reporting a count of files found
in the *source* overlay tree only proves the files existed before the copy, not that they are
still present afterward. Count the **destination** after the copy (and, for Strategy B, after any
subsequent prune) — that is the only count that would actually go to zero if a future change
reintroduced the "neither strategy" bug.

## Routing a new overlay-only / native module

A package's modules are routed to their owning package by name. A *generated* module is routed
automatically from the package's declared module list. A **native module** authored only in an
overlay/host DSL (e.g. the build-config modules `hydra.java.gradle`, `hydra.python.pyproject`) has no
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
