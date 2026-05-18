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
| Universe digest | `dist/json/digest.main.json` | Per-namespace + `encoderId` | Drives `check-dsl-fresh.py`; per-module skip inside `bootstrap-from-json` |
| Per-package input digest | `dist/json/<pkg>/src/<set>/digest.json` | Per-namespace, scoped to one package | Source-of-truth for Layer 2 freshness comparison |
| Per-package output digest | `dist/<lang>/<pkg>/src/<set>/digest.json` | Per-namespace, per-target | Compared against input digest to skip Layer 1 + Layer 2 for one package |
| Step caches | `heads/haskell/.stack-work/{verify-json-kernel,bootstrap-from-json,haskell-test}-cache.txt` | Universe-wide hash of inputs + exec source | Skips `verify-json-kernel`, `bootstrap-from-json`, or `stack test` |
| Per-target test cache | `dist/<lang>/test-cache.json` | Universe of generated sources + test infra + runner | Skips the target's `test-distribution.sh` |

### What invalidates what

The crucial design property: **every cache hashes the inputs that produced its output
and is keyed off them**. Editing a DSL source invalidates the universe digest →
invalidates the per-package input digest for whichever package owns the namespace →
invalidates the per-package output digest for each target that consumes the package →
invalidates the per-target test cache.

There are four notable exceptions where the cause-and-effect chain is incomplete today.
All four are tracked under [#347](https://github.com/CategoricalData/hydra/issues/347).
See [Gaps and the path to #347](#gaps-and-the-path-to-347).

### The `encoderId` mechanism

`encoderId` is the one fingerprint we currently take of a *transform* (not a source).
It is a SHA-256 over the four files that govern the JSON wire format:

- `Hydra/Sources/Json/Encode.hs` — Term → Value
- `Hydra/Sources/Json/Decode.hs` — Value → Term
- `Hydra/Sources/Json/Model.hs` — the `Value` type
- `Hydra/Sources/Json/Writer.hs` — Value → bytes on disk

It is stamped into `dist/json/digest.main.json` next to the per-namespace hashes.
On the next sync, `Hydra.Generation` re-computes it and compares; a mismatch forces full
re-inference of every module, because a change to any of those four files might have
altered the bytes produced for every namespace.

`encoderId` was introduced for [#343](https://github.com/CategoricalData/hydra/issues/343)
(JSON-format finalization). It is the prototype for the Merkle-style transform
fingerprinting #347 is about — but it covers only the JSON encode/decode layer. The
broader generator stack (the synthesizers under `Hydra/Sources/Kernel/Terms/Dsls.hs`,
the per-language coders, the `bootstrap-from-json` driver itself) is not fingerprinted.

If you ever need to manually invalidate the universe-wide cache without editing a real
source, the canonical trick is to zero out `encoderId` in `dist/json/digest.main.json`
(documented in [recipes/refactoring.md](recipes/refactoring.md) and
[recipes/maintenance.md](recipes/maintenance.md)).

## What the cache currently keys on

For each generated file `dist/<lang>/<pkg>/.../foo.<ext>`:

- ✅ Content hash of the DSL source `packages/<pkg>/.../Foo.hs` (or `.java` / `.py`).
- ✅ Content hash of upstream DSL modules transitively reachable from `Foo`.
- ✅ `encoderId` (covers the JSON wire format).
- ✅ For Phase 1 step caches: the source of the relevant exec (`verify-json-kernel`,
  `bootstrap-from-json`).
- ⚠️ Partial: the compiled binary of the generator (`bootstrap-from-json`,
  `update-json-main`, `transform-json-to-<lang>`). The Phase 1 input cache hashes
  every `.hs` file under `heads/haskell/src/{main,test,exec}/haskell/`, so edits to a
  synthesizer's Haskell source *do* invalidate Phase 1. The step caches for
  `verify-json-kernel` and `bootstrap-from-json` independently hash those execs'
  source. What's missing is a single fingerprint covering each generator's full
  transitive import closure — today's caches mix exec source and runtime source into
  one universe-wide hash, so every exec invalidates whenever any of them changes.
  See [#347 §1](#1-generator-binaries-are-not-fingerprinted).
- ❌ Per-target language coders (e.g., `Hydra/Sources/Haskell/Coder.hs`). When the
  Haskell coder changes, the `dist/haskell/**.hs` output it produces *is* re-generated
  (Phase 1 cache invalidates), but for non-Haskell targets the per-target coder is its
  own binary that isn't fingerprinted.
- ❌ The version of any *published* host package the build depends on. Today the
  Haskell head builds Hydra from source; once 0.16/0.17 ships hosts as published
  dependencies, the "host changed" signal becomes a per-package version bump rather
  than a per-file content hash. See
  [Implications of published hosts in 0.16/0.17](#implications-of-published-hosts-in-0-16-0-17).

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

## Gaps and the path to #347

The current cache is "source A → output B" keyed only on A. The end-state of
[#347](https://github.com/CategoricalData/hydra/issues/347) is "source A + transform T
→ output B" keyed on both A and a Merkle hash of T. The four gaps:

### 1. Generator binaries are not fingerprinted at the right granularity

Editing `Hydra/Sources/Kernel/Terms/Dsls.hs` (the DSL-wrapper synthesizer) or
`Hydra/Sources/Haskell/Coder.hs` changes the *behavior* of `bootstrap-from-json`
without changing any *direct input* to it. The Phase 1 universe-wide caches do
invalidate (these files live under `packages/`, which Phase 1 hashes), so a `/sync`
will rerun Phase 1 — but the cache key is "did the universe change", not "did the
specific transform that produces this output change". So:

- Phase 1 reruns even for kernel edits that have nothing to do with code generation
  (over-invalidation).
- The workaround for synthesizer edits — documented in
  [recipes/code-generation.md](recipes/code-generation.md#editing-the-synthesizer-itself-dslshs-the-haskell-coder-etc) —
  is still to run `/sync-haskell` twice. The first rebuilds the binary; the second
  runs the new binary against the now-updated JSON sources (under-invalidation: the
  per-source-set caches downstream don't notice the binary changed).

`encoderId` partially closes this for the JSON wire format by hashing four specific
files, but it is a per-feature special case rather than a general mechanism. The
generalization needed for #347: a per-transform Merkle subtree hashing exactly the
sources that determine that transform's output bytes — not the universe, not a
hand-picked four files.

### 2. The `writeDsl*` per-target wrappers are not in the sync pipeline

`writeDslJsonPackageSplit` (the JSON layer) IS called from `update-json-main` in
Phase 1, driven by each package's `Manifest.dslTypeModules`. That works correctly:
adding a module to `dslTypeModules` starts emitting `dist/json/<pkg>/.../dsl/<x>.json`
on the next sync.

But `writeDslHaskell` — the function that produces the consumer-facing
`dist/haskell/<pkg>/.../Hydra/Dsl/<X>.hs` from those JSON files — has no caller in any
sync script. The checked-in `.hs` files were generated by a one-off run and have
drifted. The same gap applies to the per-target wrappers in Java, Python, Scala, etc.
See the cross-reference comment on
[#347](https://github.com/CategoricalData/hydra/issues/347) from #233 for the concrete
example (`Hydra.Dsl.Pg.Model` is missing 3 of 107 definitions because the JSON layer
regenerated but the `.hs` layer didn't).

### 3. The cache structure is flat, not hierarchical

`encoderId` is universe-wide: when it changes, *every* namespace gets re-inferred,
even those whose generation path doesn't touch the affected files. A Merkle tree would
let us invalidate only the affected subtree — e.g., a change to
`Hydra/Sources/Json/Decode.hs` should invalidate any namespace whose generation reads
JSON back, but not necessarily one that only writes it.

## The end-state design

The #347 vision: every generated artifact is keyed on a Merkle hash that transitively
covers (a) its DSL source, (b) the source's transitive DSL deps, (c) the generator
binary that produced it, and (d) the generator binary's transitive DSL/code deps.

The generator-binary hash is itself a small Merkle subtree:

- For an exec like `bootstrap-from-json`: hash of its `Main.hs`, plus hashes of every
  Haskell module it transitively imports, plus the GHC version, plus the resolver
  pinned in `stack.yaml`.
- For a target-language coder (Java, Python, …) once those hosts are published
  dependencies (0.16/0.17): hash of the *published package version* of the host. No
  per-file scan needed; the version pin is the fingerprint.

With this scheme:

- Editing a synthesizer invalidates exactly the outputs that synthesizer produces.
- A host version bump invalidates exactly the outputs that host generates.
- A DSL source edit invalidates exactly the namespaces transitively depending on it
  (already true today via #329-style definition-level checksums; see
  [#329](https://github.com/CategoricalData/hydra/issues/329)).
- The "two-sync workaround" disappears.
- `encoderId` is subsumed: it becomes one of many transform fingerprints rather than
  a special case.

## Implications of published hosts in 0.16/0.17

The 0.16/0.17 transition makes the transform-fingerprinting problem *smaller*, not
larger. Today, every host runs from source — so "the host changed" is potentially
"any file in `heads/<lang>/src/` changed", which is a wide content-hash surface. Once
hosts publish as ordinary package-manager dependencies (Hackage, Maven Central, PyPI,
etc.), the dependent build's "did the host change" check becomes:

> Did the pinned host version in this build's manifest change?

That's a single string compare per host. The Merkle subtree for a target's coder
collapses to a leaf node holding `("hydra-java", "0.16.2")`. No file scan, no transitive
import graph, no source watching — the package manager already did that work and
canonicalized it into a version number.

This shifts where Merkle-tree mechanics are needed:

- **Per-package level (necessary):** track host-version-as-leaf in the cache. Whenever
  the host version pin changes, every generated artifact produced by that host is
  invalidated.
- **Per-file level (sufficient only for `heads/haskell/` and the Haskell-resident
  generators):** the bootstrap Haskell pipeline still needs the file-level Merkle
  subtree, because that's the one host we don't have the option to depend on as a
  published artifact (it would need to be `hydra-haskell` building itself).

In practice this means #347's implementation needs two flavors of transform hash:

1. **Binary-content hash** for in-tree Haskell generators (everything under
   `heads/haskell/src/{main,exec}/`). Computed during Stack build, stored alongside
   the digest.
2. **Version-pin hash** for every other host once 0.16/0.17 lands. Read from the
   manifest (package.yaml, pyproject.toml, build.gradle, project.clj, …).

`encoderId` is the prototype for flavor (1). Flavor (2) does not yet exist in any form
and only becomes meaningful once the host-as-published-dependency work is in place.

## See also

- [#347 Merkle trees for cache invalidation](https://github.com/CategoricalData/hydra/issues/347) — this issue.
- [#329 Definition-level change detection](https://github.com/CategoricalData/hydra/issues/329) — finer-grained source-side invalidation; orthogonal to #347 but composable.
- [#343 Finalize JSON format](https://github.com/CategoricalData/hydra/issues/343) — `encoderId` was introduced here; closed.
- [#344 Native DSL sources for hydra-java and hydra-python](https://github.com/CategoricalData/hydra/issues/344) — drives Phase 5.
- [#233 Per-package DSL wrappers](https://github.com/CategoricalData/hydra/issues/233) — cross-referenced from #347 because of the `dslTypeModules` ↔ `writeDslHaskell` gap.
