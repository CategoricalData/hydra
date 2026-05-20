# The Hydra build system

This document is the canonical entry point for Hydra's build, sync, and code-generation
pipeline. It covers the conceptual model — what gets generated, what gets cached, and
what invalidates what — and links out to operational recipes for day-to-day workflows.

For step-by-step "how do I regenerate X" instructions, see
[recipes/code-generation.md](recipes/code-generation.md).
For the broader architectural context, see [implementation.md](implementation.md).
For known gotchas, see [troubleshooting.md](troubleshooting.md)
and [claude/pitfalls.md](../claude/pitfalls.md).

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
| 2. Assemble | `heads/<lang>/bin/assemble-distribution.sh <pkg>` (one package), `assemble-all.sh` (batch) | Run Layer 1 + per-target post-processing (TestGraph patches, line-wrap, etc.) |
| 2.5. Test | `heads/<lang>/bin/test-distribution.sh` | Compile and run the target's test suite |
| 3. Orchestrate | `bin/sync.sh`, `bin/sync-packages.sh`, `bin/sync-all.sh`, per-lang `bin/sync-<lang>.sh` | Walk the matrix; gate each step on its cache |

Day-to-day, you invoke a Layer 3 script. The lower layers are useful when debugging or
iterating on a single package.

For the full script inventory and per-script semantics, see
[implementation.md §Sync system](implementation.md) and
[recipes/code-generation.md §The sync scripts](recipes/code-generation.md#the-sync-scripts).

## Phases of `bin/sync.sh`

A full `bin/sync.sh` run executes five phases. Each phase has its own cache; phases run
strictly in order, but any phase can short-circuit independently.

| Phase | Driver | Output |
|-------|--------|--------|
| 0. Stack build | `stack build` of every Haskell exec | Bootstraps `update-json-main`, `update-json-test`, `update-json-manifest`, `verify-json-kernel`, `bootstrap-from-json`, `digest-check` |
| 1. DSL → JSON + Haskell kernel | `heads/haskell/bin/sync-haskell.sh` | `dist/json/**` and `dist/haskell/{hydra-kernel,hydra-haskell}/` |
| 2. Coder Haskell dists | per-language assemblers | `dist/haskell/hydra-<lang>/` for every L in (hosts ∪ targets) |
| 3. Kernel/pg/rdf into each target | per-target assemblers | `dist/<lang>/{hydra-kernel,hydra-pg,hydra-rdf}/` |
| 4. Cross-host coders | per-host assemblers | `dist/<host>/hydra-<target>/` for every (host, target) with host ≠ haskell |
| 5. Native DSL → JSON for hydra-java and hydra-python | `bin/generate-hydra-<lang>-from-<lang>.sh` | Overwrites `dist/json/hydra-{java,python}/` from host-native sources |

Phase 5 is the migration path for [#344](https://github.com/CategoricalData/hydra/issues/344):
`hydra-java` and `hydra-python` are now authored in their own host languages, with the
legacy Haskell-DSL copies retained as a fallback through 0.15.

`bin/sync.sh` runs Phases 0–5 over the matrix the caller specifies via `--hosts` and
`--targets`. `bin/sync.sh` does NOT run target-language tests; only Haskell `stack test`
is invoked (via `sync-haskell.sh`'s Step 6). To validate a target's runtime, run that
head's own `bin/run-tests.sh` or `bin/test-distribution.sh`, or use
`bin/sync-packages.sh` which adds a Phase 3 test gate.

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
| Universe digest | `dist/json/build/digest.json` | Per-namespace | Drives `check-dsl-fresh.py`; per-module skip inside `bootstrap-from-json` |
| Per-package input digest | `dist/json/<pkg>/build/<set>/digest.json` | Per-namespace, scoped to one package | Source-of-truth for Layer 2 freshness comparison |
| Per-package output digest | `dist/<lang>/<pkg>/build/<set>/digest.json` | Per-namespace + per-target generator stamp | Compared against input digest to skip Layer 1 + Layer 2 for one package |
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

### What invalidates what

The crucial design property: **every cache hashes the inputs that produced its output
and is keyed off them**. Editing a DSL source invalidates the universe digest →
invalidates the per-package input digest for whichever package owns the namespace →
invalidates the per-package output digest for each target that consumes the package →
invalidates the per-target test cache.

There are four notable exceptions where the cause-and-effect chain is incomplete today.
All four are tracked under [#347](https://github.com/CategoricalData/hydra/issues/347).
See [Gaps and the path to #347](#gaps-and-the-path-to-347).

### The per-target generator stamp

The Layer 2 cache is keyed on (a) per-namespace input hashes and (b) a per-target
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
are namespaces in the kernel package, so they're covered by `component_identity
hydra-kernel`. Their effect on output bytes now invalidates the cache through the standard
per-namespace path (in Phase 1) plus the kernel-id leaf of every target's stamp
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
  digest includes every namespace owned by `<pkg>`).
- ✅ Per-target generator stamp covering the kernel + per-target coder + host runtime
  (see [The per-target generator stamp](#the-per-target-generator-stamp) above). A
  change to any of these three components invalidates the cache for that target.
- ✅ For Phase 1 step caches: the source of the relevant exec (`verify-json-kernel`,
  `bootstrap-from-json`) plus, for `bootstrap-from-json`'s BFJ step, every file under
  `heads/haskell/src/main/haskell/**.hs`.
- ⚠️ Coarse-grained on the source side. A change to one source file invalidates only
  its own per-namespace hash, but the *consumers* of that namespace aren't automatically
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

Today the per-package input digest treats each package as an opaque bag of namespaces.
A change to one source file `Foo.hs` invalidates the per-package digest's entry for
`Foo`'s namespace — but consumers of `Foo` in *other* packages aren't automatically
flagged for regen unless they themselves changed. The current design relies on the
universe-wide invalidation cascade for cross-package propagation, which is correct but
coarse: a kernel-type edit may invalidate everything even though only a handful of
downstream namespaces consume the changed type.

The proper Merkle structure on the A side is the file-level dependency DAG: each
namespace's cache key is `hash(its source content, its transitive deps' cache keys)`. A
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

## The end-state design

Two pieces, one per side:

**A-side: file-level Merkle over the source DAG.**
Per-namespace cache key = `hash(source content, dep1 cache key, dep2 cache key, ...)`.
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
