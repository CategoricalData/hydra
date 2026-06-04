# The Hydra build system

This document is the canonical entry point for Hydra's build, sync, and code-generation
pipeline. It covers the conceptual model — what gets generated, what gets cached, and
what invalidates what — and links out to operational recipes for day-to-day workflows.

For step-by-step "how do I regenerate X" instructions, see
[recipes/code-generation.md](recipes/code-generation.md).
For the broader architectural context, see [implementation.md](implementation.md).
For known gotchas, see [troubleshooting.md](troubleshooting.md)
and [claude/pitfalls.md](../claude/pitfalls.md).

## Design philosophy: translingual source packages → complete target distributions

The organizing principle of Hydra's build is a clean separation between **source** and
**distribution**:

- **Source packages** (`packages/<pkg>/`) are *translingual*: each is authored once, in a
  host DSL, and describes Hydra modules independently of any target language. `hydra-kernel`,
  `hydra-rdf`, `hydra-pg`, and the coder packages are all source packages in this sense.
- **Distribution packages** (`dist/<lang>/<pkg>/`) are the result of *mapping* a source
  package into a specific target language. The goal for every distribution package is that
  it be **complete and deployment-ready**: a self-contained, idiomatic package in its target
  ecosystem (a Maven artifact, a PyPI/conda package, a Hackage sdist, …) that a downstream
  consumer can depend on **without any knowledge of Hydra's repository layout**. It carries
  its own build descriptor (`build.gradle`, `pyproject.toml`, `package.yaml`/`.cabal`), its
  generated source, and any hand-written runtime it needs — with no references back into
  `heads/` or sibling trees.
- **Overlay sources** (`overlay/<lang>/<pkg>/`) are the hand-written, language-specific
  source that a distribution package needs but that is *not* generated — e.g. the Haskell
  kernel runtime (`Hydra.Haskell.Lib.*`, the DSL term helpers, `Hydra.Settings`,
  `Hydra.Kernel`) and the `hydra` umbrella module. The overlay tree is a top-level sibling of
  `packages/`, `dist/`, `heads/`, and `bindings/`, mirroring the `dist/<lang>/<pkg>/` shape.
  Its contents are *overlaid onto* (merged into) the generated `dist/<lang>/<pkg>/` tree by
  the sync step, which is what makes the distribution complete. Overlay sources are authored,
  not generated, and are never compiled in place — only after being overlaid into `dist/`.
  (`overlay/haskell/`, `overlay/java/`, and `overlay/python/` are populated; TypeScript still
  keeps the analogous runtime in `heads/typescript/src` pending migration.)

So one translingual source package fans out into one complete distribution package per
selected target language, and each distribution stands on its own. "Complete" is the bar:
if a `dist/<lang>/<pkg>/` would need a `../../` reference to build, it is not yet a proper
distribution package. (The hand-written runtime that `hydra-kernel` needs is *overlaid into*
the distribution to satisfy this — see
[Hand-written runtime in hydra-kernel](#hand-written-runtime-in-hydra-kernel).)

A consequence worth stating: while `heads/<lang>/` currently both *produces* distributions
and *consumes* them (the Haskell head, for instance, compiles the `dist/haskell/*` trees
directly as part of its own build), the end state is for each head to depend on the
**published, versioned** distribution packages — at which point any remaining direct
references to `dist/` are migration shims, not the intended coupling.

## The pipeline at a glance

Hydra is self-hosting: every implementation is generated from a single set of source
modules. The pipeline has three observable stages, each living under a different
directory tree:

```
DSL sources                        JSON modules                        target distributions
packages/<pkg>/src/main/<lang>/  → dist/json/<pkg>/src/<set>/json/  →  dist/<lang>/<pkg>/src/<set>/<lang>/
       (authored)                    (interchange format)                 (generated code)
```

- **DSL sources** are human-authored modules in a host language (Haskell for most
  packages; Java for `hydra-java`; Python for `hydra-python`).
- **JSON modules** are the language-neutral interchange representation. `dist/json/` is
  tracked in git as the source of truth for non-Haskell hosts.
- **Target distributions** are generated source trees, one per (target language × package).
  `dist/haskell/` is tracked through 0.15 to bootstrap a fresh clone;
  all other `dist/<lang>/` trees are regenerated on demand and are not checked in.

A complete sync of the matrix walks these in five phases (see
[Phases](#phases-of-bin-sync-sh) below).

## Layers and scripts

The sync infrastructure is layered. Each layer is content-hash gated, so a warm run
short-circuits in seconds.

| Layer | Where | What it does |
|-------|-------|--------------|
| 1. Transform | `heads/haskell/bin/transform-haskell-dsl-to-json.sh`, `transform-json-to-<lang>.sh` | One-shot conversion, one direction, one package or `--all` |
| 2. Assemble | `heads/<lang>/bin/assemble-distribution.sh <pkg>` (one package), `assemble-all.sh` (batch) | Run Layer 1 + per-target post-processing (TestGraph patches, line-wrap, etc.). For Java/Python/TypeScript `hydra-kernel`, also copies hand-written runtime support — see [Hand-written runtime in hydra-kernel](#hand-written-runtime-in-hydra-kernel) below. |
| 2.5. Test | `heads/<lang>/bin/test-distribution.sh` | Compile and run the target's test suite |
| 3. Orchestrate | `bin/sync.sh`, `bin/sync-packages.sh`, `bin/sync-all.sh`, per-lang `bin/sync-<lang>.sh` | Walk the matrix; gate each step on its cache |

Day-to-day, you invoke a Layer 3 script. The lower layers are useful when debugging or
iterating on a single package.

Each layer boundary is **fail-loud**: it either succeeds with a verified-consistent artifact
or aborts with an error naming the layer and the mismatch — no step exits 0 on a swallowed
failure (#414). Concretely, the Layer 2 digest helpers in `bin/lib/assemble-common.sh`
distinguish their two roles: `assemble_check_fresh` is a *predicate* (a missing input or
output digest means "not fresh — rebuild", returned quietly), while `assemble_refresh_digest`
*writes* the output digest after a regen and treats a missing input digest there as a hard
error naming the path — generation just consumed that input, so its absence is a genuine
upstream inconsistency, not a routine condition. (Before #414 the latter was an
`[ -f X ] && (…)` guard that, under `set -e`, silently killed the assembler and skipped every
remaining package — the "Phase 2 silent exit" failure.)

For the full script inventory and per-script semantics, see
[implementation.md §Sync system](implementation.md) and
[recipes/code-generation.md §The sync scripts](recipes/code-generation.md#the-sync-scripts).

## Hand-written runtime in hydra-kernel

Not every file under `dist/` is generated. For Haskell, Java, Python, and
TypeScript, `hydra-kernel` ships with a hand-written runtime tree alongside the
generated kernel types so the published package is self-contained — downstream
Stack/cabal / Gradle / pip / npm consumers do not need to know about Hydra's
`overlay/` or `heads/` layout.

The mechanism: a per-language **overlay** step merges the hand-written runtime
into `dist/<lang>/hydra-kernel/src/main/<lang>/` during sync (for Haskell, Java,
and Python the canonical source is the top-level `overlay/<lang>/hydra-kernel/`
tree; TypeScript still copies from within `heads/typescript/` pending migration).
For Java/Python it runs as **Step 0** of `heads/<lang>/bin/assemble-distribution.sh
hydra-kernel`, which invokes `heads/<lang>/bin/copy-kernel-runtime.sh`; for Haskell
it runs as a post-processing step of `sync-haskell.sh`. It runs only for
`hydra-kernel` (plus, for Haskell, the `hydra` umbrella).

The copy is a *merge* into the generated tree, not a wholesale overwrite —
several subdirectories (e.g. `hydra/json/`) contain both generated and
hand-written files and would clobber the generator's output if replaced.
(For Java/Python, the script appends every copied path to a manifest consumed by
`bootstrap-from-json --keep-paths-from`, which protects hand-copied files
from the `--prune-stale` deletion pass.)

Because the `overlay/<lang>/hydra-kernel/` tree holds *only* runtime (nothing
else), the migrated copy scripts are dumb full-tree merges — no selective file
lists or per-file exclusions. Files that are NOT kernel runtime (multi-coder
drivers, test bases, json-io stubs, each language's own coder) stay in
`heads/<lang>/src` and are compiled by the developer rollup, not copied.

| Language | Canonical runtime home | Overlaid by | Approx. file count |
|----------|------------------------|-------------|--------------------|
| Haskell | `overlay/haskell/hydra-kernel/` (+ `overlay/haskell/hydra/` umbrella): `Hydra.Settings`, `Hydra.Kernel`, 13 `Hydra.Haskell.Lib.*`, `Hydra.Dsl.{Terms,Literals,Meta.Common}` | `sync-haskell.sh` (head compiles from the dist copy, not the overlay; copies gitignored) | 18 |
| Java | `overlay/java/hydra-kernel/`: `Adapters.java`, `Coders.java`, full `hydra/{util,lib,dsl,tools}/`, `hydra/json/{JsonEncoding,JsonDecoding}.java` | `copy-kernel-runtime.sh` (Step 0 of assemble) | ~282 |
| Python | `overlay/python/hydra-kernel/`: `tools.py`, `py.typed`, `hydra/{lib,dsl,sources}/` (no `__init__.py` — PEP 420) | `copy-kernel-runtime.sh` (Step 0 of assemble) | ~47 |
| TypeScript | (not yet migrated) `heads/typescript/src`: `hydra/{bootstrap,primitives,runtime}.ts`, `hydra/lib/*.ts`, test helpers | `heads/typescript/bin/copy-kernel-runtime.sh` | ~19 |
| Scala, Go, Lisp dialects | — none — | Their runtimes live under `heads/<lang>/` and are referenced via build-tool source-dir paths (sbt's `unmanagedSourceDirectories`, etc.). Nothing is copied. | 0 |

The canonical edit point is the `overlay/<lang>/` tree (Haskell/Java/Python) or
`heads/<lang>/src` (TypeScript and not-yet-migrated cases). Editing the copy in
`dist/` is wrong for the same reason editing any other `dist/` file is wrong: the
next sync/assemble overwrites it. (See CLAUDE.md hard rule 3.)

These files **do not carry** the "automatically generated — do not edit" header
that generated files carry — they aren't generated. A grep that scans `dist/` for
files missing the header will surface this set as false positives; cross-reference
against the `overlay/<lang>/` trees (and the `copy-kernel-runtime.sh` manifests)
when triaging.

The bootstrap demo's per-target setup scripts
(`demos/bootstrapping/bin/setup-<lang>-target.sh`) overlay the same
`overlay/<lang>/hydra-kernel/` trees into their flat bootstrap output, so the
bootstrapped sources stay consistent with the published distributions.

## Phases of `bin/sync.sh`

A full `bin/sync.sh` run executes five phases. Each phase has its own cache; phases run
strictly in order, but any phase can short-circuit independently.

| Phase | Driver | Output |
|-------|--------|--------|
| 0. Stack build | `stack build` of every Haskell exec | Bootstraps `update-json-main`, `update-json-test`, `update-json-manifest`, `update-json-kernel`, `verify-json-kernel`, `bootstrap-from-json`, `digest-check` |
| 1. DSL → JSON + Haskell kernel | `heads/haskell/bin/sync-haskell.sh` | `dist/json/**` and `dist/haskell/{hydra-kernel,hydra-haskell}/` |
| 1.5. Java/Python coder-JSON auto-heal | `bin/lib/check-java-python-json-fresh.py` | On a staleness miss, re-exports `dist/json/hydra-{java,python}/` via `update-json-main --include-java-python` before Phase 2 reads it |
| 2. Coder Haskell dists | per-language assemblers | `dist/haskell/hydra-<lang>/` for every L in (hosts ∪ targets) |
| 3. Kernel/pg/rdf into each target | per-target assemblers | `dist/<lang>/{hydra-kernel,hydra-pg,hydra-rdf}/` |
| 4. Cross-host coders | per-host assemblers | `dist/<host>/hydra-<target>/` for every (host, target) with host ≠ haskell |
| 5. Native DSL → JSON for hydra-java and hydra-python | `bin/generate-hydra-<lang>-from-<lang>.sh` | Overwrites `dist/json/hydra-{java,python}/` from host-native sources |

Phase 1.5 closes a warm/cold asymmetry from #344 that
[#406](https://github.com/CategoricalData/hydra/issues/406) made deterministic. Phase 1
re-exports the `hydra.java.*` / `hydra.python.*` coder JSON only on cold-start (when the
sentinel JSON is missing); on a warm tree it skips them, since Phase 5 owns those paths.
But `dist/json/hydra-{java,python}/coder.json` feeds Phase 2's
`dist/haskell/hydra-{java,python}/Coder.hs`, which compiles into the core `hydra` library.
A kernel rename that ripples into the coders therefore leaves that JSON stale against the
freshly regenerated kernel, and Phase 2 emits a `Coder.hs` referencing a renamed-away field
— breaking the next `stack build`, before Phase 5 (which would refresh the JSON) ever runs.
Reordering Phase 5 ahead of Phase 2 cannot fix this today: the native generator needs the
host built (Phase 4 output), which needs this very library. So Phase 1.5 heals in place —
it keys on the just-regenerated kernel JSON (the rename signal, which survives the eventual
deletion of the legacy Haskell coder DSL) and, on a miss, re-exports the coder JSON via
`update-json-main --include-java-python` so Phase 2 reads fresh input.

Phase 5 is the migration path for [#344](https://github.com/CategoricalData/hydra/issues/344):
`hydra-java` and `hydra-python` are now authored in their own host languages, with the
legacy Haskell-DSL copies retained as a fallback through 0.15.

Because Phase 5 is last but its output (`dist/json/hydra-{java,python}/`) is an *input* to
Phases 2–4, a change to a native coder would otherwise take two sync passes to fully land —
Phases 2–4 in pass *n* still see the pre-Phase-5 JSON. To keep a single pass self-consistent,
when the native output differs from the prior snapshot Phase 5 re-assembles
`dist/haskell/hydra-<lang>/` from the new JSON and re-runs `stack build` so
`bootstrap-from-json` embeds the updated coder. This re-assemble flows through the normal
freshness gate: Phase 1's input digest now folds in the native `hydra.<lang>.*` source
hashes (see [the cache model](#the-cache-model) and
[#400](https://github.com/CategoricalData/hydra/issues/400)), so the assembler correctly
detects the change without any output-digest force-drop. The downstream `dist/<target>/`
trees are gitignored and regenerated on the next run, so they are intentionally not
re-emitted here.

Once the legacy Haskell DSL copies for hydra-java/hydra-python are removed (post-0.16), the
native generators become the sole writers of `dist/json/hydra-{java,python}/`. This native
DSL→JSON step should then move ahead of Phase 2, eliminating the producer-ordering lag and
the re-assemble block above.

`bin/sync.sh` runs Phases 0–5 over the matrix the caller specifies via `--hosts` and
`--targets`. `bin/sync.sh` does NOT run target-language tests; only Haskell `stack test`
is invoked (via `sync-haskell.sh`'s Step 6). To validate a target's runtime, run
**`bin/test.sh`** (or `/test` from a Claude session — same scoping vocabulary as `/sync`),
which pre-syncs and then invokes each requested target's `heads/<lang>/bin/test-distribution.sh`.
You can also run a single per-target tester directly (e.g.
`heads/python/bin/test-distribution.sh hydra-kernel`) or use `bin/sync-packages.sh`,
which adds a Phase 3 test gate.

### Phase 1's memory envelope

Phase 1 ran a single universe-wide inference until [#381](https://github.com/CategoricalData/hydra/issues/381):
`inferModules` loaded every binding from every module into one giant `let` and unified
the whole thing. At ~10 packages and ~280 modules, peak Haskell heap exceeded 7 GB —
larger than GitHub Actions' `ubuntu-latest` (7 GB), so the cold-CI path silently
overflowed the heap.

Phase 1 now iterates **packages** instead. The driver
(`Hydra.Generation.inferAndWriteByPackage`) topologically sorts the package dep graph
from each `packages/<pkg>/package.json`'s `dependencies` field, then for each package
in order calls `inferModulesGiven` with the typed-so-far universe (deps that finished
in earlier iterations) plus that package's own modules as the focus subset. Each
iteration writes the focus package's JSON to disk immediately, which forces the inferred
TypeSchemes through serialization and breaks any lazy thunk chain across iterations.

Peak memory per iteration is bounded by ≈ *type-schemes of transitive deps + bindings of
the focus package*, not by the universe-wide
*bindings of every module + full substitution map + constraint set*. Trade-off: each
iteration rebuilds `modulesToGraph` over an accumulator that grows linearly, so wall
time goes up roughly 2× on a roomy host (≈5 min → ≈13 min). On `ubuntu-latest` the
old path doesn't fit at all, so this is the only path that runs there.

The committed CI heap cap is `RTS_FLAGS=-M6G`. Local syncs are free to raise it; CI
must surface a heap-overflow diagnostic if the per-package cap is ever exceeded, not
silently cancel.

Phase 1 has two paths into per-package iteration: the cold-cache fallback (above) and
the warm-cache **incremental** path that fires when a digest already exists. The
incremental path also routes through the same driver via `inferAndWriteByPackageSeeded`,
which takes the JSON-loaded clean modules as a pre-typed seed for the accumulator —
they participate in cross-package type resolution but are not re-grouped or re-inferred.
A change that dirties most of the universe (e.g. a kernel-wide rename) thus stays
within the per-package memory envelope on the incremental path too, instead of falling
back to a single mega-inference over the full dirty set.

## The cache model

Every layer of the pipeline caches its work. All caches are content-hash based
(SHA-256 of file bytes), not mtime — editing a file with no byte-level change does not
invalidate, and changing one byte invalidates. Caches stamp only after a fully-green
run; a failed step does not poison the cache.

The caches form a hierarchy: a hit at a coarser layer skips a finer one.

### Cache inventory

| Cache | Location | Granularity | What it gates |
|-------|----------|-------------|---------------|
| Phase 1 input cache | `heads/haskell/.stack-work/phase1-input-cache.txt` | Universe-wide | Skips all of Phase 1 (no stack startup, no JSON regen) |
| Universe digest | `dist/json/build/digest.json` | Per-module-name | Drives `check-dsl-fresh.py`; per-module skip inside `bootstrap-from-json` |
| Per-package input digest | `dist/json/<pkg>/build/<set>/digest.json` | Per-module-name, scoped to one package | Source-of-truth for Layer 2 freshness comparison |
| Per-package output digest | `dist/<lang>/<pkg>/build/<set>/digest.json` | Per-module-name + per-target generator stamp | Compared against input digest to skip Layer 1 + Layer 2 for one package |
| Step caches | `heads/haskell/.stack-work/{verify-json-kernel,bootstrap-from-json,haskell-test}-cache.txt` | Universe-wide hash of inputs + exec source | Skips `verify-json-kernel`, `bootstrap-from-json`, or `stack test` |
| Per-target test cache | `dist/<lang>/test-cache.json` | Universe of generated sources + test infra + runner | Skips the target's `test-distribution.sh` |

### Cache files are not tracked

Every `dist/<lang>/<pkg>/build/` directory holds derived freshness state: input digests,
output digests, generator stamps. The entire `dist/**/build/` subtree is gitignored.
The same applies to `dist/json/build/` (the universe digest). See
[#379](https://github.com/CategoricalData/hydra/issues/379) for the rationale —
in short, hashes diverge on every branch by construction, so committing them produced
merge conflicts on every multi-branch merge while providing no value (the post-merge
hashes are stale anyway, so the next build re-derives them).

A missing or stale digest is always a cache miss, never a correctness problem. The
first build after a fresh clone or after a merge runs without cache hits and rebuilds
the digests as it goes; subsequent runs hit the cache normally.

The self-hosted coders used to have an incomplete input digest: it hashed only the
`hydra.dsl.<lang>.*` modules, not the native-generator-owned `hydra.<lang>.*` modules
(including `hydra.<lang>.coder`), so a change confined to the native coder was *invisible*
to the freshness gate ([#400](https://github.com/CategoricalData/hydra/issues/400)). Fixed:
`Hydra.Digest.discoverModuleNameFiles` now also scans the native `.java`/`.py` sources, and
the per-package input digest hashes the unfiltered universe, so
`dist/json/hydra-{java,python}/build/main/digest.json` includes every native module. A
native coder edit now invalidates that digest like any other source change — Phase 5's
re-assemble relies on the honest gate instead of force-dropping the output digest.

### What invalidates what

The crucial design property: **every cache hashes the inputs that produced its output
and is keyed off them**. Editing a DSL source invalidates the universe digest →
invalidates the per-package input digest for whichever package owns the module name →
invalidates the per-package output digest for each target that consumes the package →
invalidates the per-target test cache.

There are four notable exceptions where the cause-and-effect chain is incomplete today.
All four are tracked under [#347](https://github.com/CategoricalData/hydra/issues/347).
See [Remaining gaps](#remaining-gaps).

A distinct failure mode, worth separating from the four cascade gaps above, is an
**incomplete input set**: a real input that feeds a package's generation is missing from
the input digest *entirely*, so no edit to it can ever invalidate the cache — unlike a
*stale* hash, which content-hashing catches on the next run. #400 was an instance: the
self-hosted coders' input digest omitted the native `hydra.<lang>.*` modules, so a
`Coder.java` edit produced a green sync with no effect (fixed — see above). The general
guard: the input digest for a package must hash *every* source module that feeds its
generation, including ones whose JSON output is written by a different producer.

### The per-target generator stamp

The Layer 2 cache is keyed on (a) per-module-name input hashes and (b) a per-target
**generator stamp** — a fingerprint of the transform that produced the output. Without
the stamp, edits to the transform (the per-target coder, the kernel orchestrator code,
the assembler script) would change downstream emission without changing any tracked
input, and the cache would erroneously hit.

The stamp is computed by `compute_generator_stamp <lang>` in `bin/lib/assemble-common.sh`
and exported as `HYDRA_GENERATOR_STAMP` before each Layer 2 freshness check. `digest-check
fresh` reads it and compares against the recorded stamp in the per-target output digest;
mismatches force a cache miss.

The stamp is **compositional**, mirroring the actual structure of a host transform:

```
stamp(L) = hash(
    kernel-id  = component_identity hydra-kernel,
    coder-id   = component_identity hydra-L,
    runtime-id = runtime_identity L
)
```

Each component is independent. A `hydra-kernel` change invalidates every target's stamp;
a Java-coder change invalidates only Java; an edit to `heads/python/src/main/` invalidates
only Python. This is the desired precision: the dependency graph determines the
invalidation graph.

`component_identity` is the swappable layer. Today every host is built from local source,
so it returns a content hash of `packages/<pkg>/src/main/haskell/**`. After
[#370](https://github.com/CategoricalData/hydra/issues/370) (external versioned hosts),
it will branch on published-vs-local and return the pinned version string for published
hosts. The composition above is unchanged either way; only the leaf computation differs.

#### Retired: `encoderId`

Pre-#347 universe digests carried a top-level `encoderId` field — a SHA-256 over four
hardcoded JSON-coder DSL source files (`Hydra/Sources/Json/{Encode,Decode,Model,Writer}.hs`).
It was introduced for [#343](https://github.com/CategoricalData/hydra/issues/343) as a
prototype transform fingerprint and triggered a universal cache miss when any of those
files changed.

`encoderId` is retired by the compositional stamp above. The four files it fingerprinted
are module names in the kernel package, so they're covered by `component_identity
hydra-kernel`. Their effect on output bytes now invalidates the cache through the standard
per-module-name path (in Phase 1) plus the kernel-id leaf of every target's stamp
(in Layer 2).

Legacy on-disk digests carrying `encoderId` are tolerated: `parseDigest` silently ignores
the field. They'll naturally fade out the next time any source change triggers a digest
refresh.

If you ever need to manually invalidate a per-target cache without editing real source,
delete `dist/<lang>/<pkg>/build/<set>/digest.json` — the next assemble will report a missing
output digest and regenerate. The entire `dist/**/build/` subtree is gitignored (see
[#379](https://github.com/CategoricalData/hydra/issues/379)); each `build/` directory holds
only derived freshness state, so deleting it is always safe and never affects shared history.

## What the cache currently keys on

For each generated file `dist/<lang>/<pkg>/.../foo.<ext>`:

- ✅ Content hash of the DSL source `packages/<pkg>/.../Foo.hs` (or `.java` / `.py`).
- ✅ Content hash of upstream DSL modules reachable from `Foo` (per-package input
  digest includes every module name owned by `<pkg>`).
- ✅ Per-target generator stamp covering the kernel + per-target coder + host runtime
  (see [The per-target generator stamp](#the-per-target-generator-stamp) above). A
  change to any of these three components invalidates the cache for that target.
- ✅ For Phase 1 step caches: the source of the relevant exec (`verify-json-kernel`,
  `bootstrap-from-json`) plus, for `bootstrap-from-json`'s BFJ step, every file under
  `heads/haskell/src/main/haskell/**.hs`.
- ⚠️ Coarse-grained on the source side. A change to one source file invalidates only
  its own per-module-name hash, but the *consumers* of that module name aren't automatically
  invalidated unless they themselves changed. The current design treats the per-package
  digest as the unit of invalidation, not the file-level dependency DAG. See [Remaining
  gaps](#remaining-gaps).
- ❌ Version-pin path for hosts. Today every host is built from local source, so the
  generator stamp uses content hashes. Once [#370](https://github.com/CategoricalData/hydra/issues/370)
  ships hosts as published artifacts, `component_identity` will branch on
  published-vs-local and return the version string for published hosts. The composition
  is unchanged; only the leaf computation differs.

## Operational entry points

For workflows, follow the recipes:

- [Generating code with Hydra](recipes/code-generation.md) — full end-to-end pipeline
  walkthrough, including the bootstrap-from-json CLI and Phase 2 batch mode.
- [Repository maintenance](recipes/maintenance.md) — stale-file detection, design
  violations, dist consistency, digest hygiene.
- [Syncing Python](recipes/syncing-python.md) — Python-specific workflow.
- [Exporting modules to JSON](recipes/json-kernel.md) — JSON format and verification.
- [Troubleshooting](troubleshooting.md) — general cross-cutting issues.

For slash-command shortcuts: `/sync`, `/sync-default`, `/sync-haskell`, `/sync-java`,
`/sync-python`, `/sync-scala`, `/sync-clojure`, `/sync-common-lisp`,
`/sync-emacs-lisp`, `/sync-scheme`, `/sync-go`, `/sync-bench`. Each is a thin wrapper
around the corresponding `bin/` script; see [CLAUDE.md §Shorthand commands](../CLAUDE.md#shorthand-commands).

## Remaining gaps

### 1. File-level source-dependency Merkle (A-side)

Today the per-package input digest treats each package as an opaque bag of module names.
A change to one source file `Foo.hs` invalidates the per-package digest's entry for
`Foo`'s module name — but consumers of `Foo` in *other* packages aren't automatically
flagged for regen unless they themselves changed. The current design relies on the
universe-wide invalidation cascade for cross-package propagation, which is correct but
coarse: a kernel-type edit may invalidate everything even though only a handful of
downstream module names consume the changed type.

The proper Merkle structure on the A side is the file-level dependency DAG: each
module name's cache key is `hash(its source content, its transitive deps' cache keys)`. A
change at the root propagates exactly to dependents. Substantially overlaps with
[#329 (definition-level change detection)](https://github.com/CategoricalData/hydra/issues/329);
worth treating as one piece of work.

### 2. Version-pin path for published hosts (T-side)

The compositional generator stamp is in place, but every `component_identity` call still
returns a content hash because no host is published yet. After
[#370](https://github.com/CategoricalData/hydra/issues/370) lands the publishing
machinery, `component_identity` needs a branch: when the package has a pinned published
version in this build's manifest, return that version string; otherwise fall back to the
current content hash (the migration-shim case).

This is a small, well-scoped change — one function body — gated on #370.

### 3. `modulesToGraph` realloc per Phase 1 iteration

The per-package iteration originally called `inferModulesGiven` (kernel) per package,
which rebuilds `modulesToGraph` over its `universeMods` argument every time. Across
~13 packages that meant ~13 graph builds, each over a `[Module]` accumulator that grew
linearly — retaining every prior package's full term bodies, annotations, and
dependencies. Acceptable for small dirty sets but OOM at -M6G on a kernel-wide rename
(#369-style) that dirtied ~250 modules in one shot.

`inferAndWriteByPackageSeeded` now threads a `Map Name TypeScheme` accumulator instead
of `[Module]`. After each package writes its JSON, only the inferred bindings'
`(Name, TypeScheme)` pairs are folded into the accumulator; the inferred Module values
themselves are dropped, so GC reclaims their term bodies. A Generation-side wrapper
`inferModulesGivenSchemes` augments `Graph.graphBoundTypes` and `graphSchemaTypes`
with the seed Map directly, bypassing the per-iteration `[Module]` reload. The
JSON-write `schemaMap` is built ONCE up front from the full input universe (including
the JSON-loaded clean modules in the warm-incremental path) so the encoder has
hydra.packaging.Module and other cross-package schema types available — without this,
`Maybe String` fields mis-serialize as single-element arrays.

### 4. Java and Python self-host pipelines

Phase 5 (native DSL → JSON for hydra-java and hydra-python) now routes through a
per-package iterative driver that mirrors the Haskell-side
`inferAndWriteByPackage`: `Generation.inferAndWriteByPackage` in
`heads/java/src/main/java/hydra/Generation.java` and `infer_and_write_by_package`
in `heads/python/src/main/python/hydra/generation.py`. Both drivers
(`bin/update-python-json.py` and `UpdateJavaJson`) call this routine after
loading the kernel universe and the package's source modules. Today's runs
collapse to a one-iteration loop (only hydra-java or hydra-python is being
re-inferred); the structure is in place for multi-package native-coder updates
once additional packages are owned by these native pipelines.

## The end-state design

Two pieces, one per side:

**A-side: file-level Merkle over the source DAG.**
Per-module-name cache key = `hash(source content, dep1 cache key, dep2 cache key, ...)`.
A change at the root propagates exactly to dependents. Combines naturally with #329's
definition-level checksums.

**T-side: compositional transform identity.**
Per-target generator stamp = `hash(kernel-id, coder-id, runtime-id)`. Each component
is a version pin when published, a content hash when local. Already shipped in its
content-hash form; the version-pin path activates with #370.

Both pieces are independent and can land separately. The T-side is closer to done.

## See also

- [#347 Merkle trees for cache invalidation](https://github.com/CategoricalData/hydra/issues/347) — this issue.
- [#329 Definition-level change detection](https://github.com/CategoricalData/hydra/issues/329) — finer-grained source-side invalidation; orthogonal to #347 but composable.
- [#343 Finalize JSON format](https://github.com/CategoricalData/hydra/issues/343) — `encoderId` was introduced here; closed.
- [#344 Native DSL sources for hydra-java and hydra-python](https://github.com/CategoricalData/hydra/issues/344) — drives Phase 5.
- [#233 Per-package DSL wrappers](https://github.com/CategoricalData/hydra/issues/233) — cross-referenced from #347 because of the `dslTypeModules` ↔ `writeDslHaskell` gap.
