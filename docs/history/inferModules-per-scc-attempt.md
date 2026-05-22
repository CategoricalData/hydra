# Attempted per-SCC fold rewrite of `inferModules` (2026-05-21)

## Motivation

GitHub Actions CI was hitting silent process cancellations during
`bin/sync.sh` Step 2 (the `update-json-main` invocation), consistently
~20 minutes into the job. Adding `+RTS -M5G -RTS` to the run surfaced
the real cause: a clean `Error: heap overflow` inside
`Hydra.Inference.writeModulesJson`'s full-inference path. Raising the
cap to `-M6G` still overflowed. The runner only has 7 GB, so this
particular path was effectively unfittable on `ubuntu-latest`.

The full-inference path is taken whenever `tryIncrementalInference`
sees no prior universe digest (e.g. fresh CI checkout) and falls
through to `inferModulesIO universeMods mods` →
`CodeGeneration.inferModules`.

## Original `inferModules` shape

```
inferModules cx bsGraph universeMods targetMods =
  let g0 = modulesToGraph bsGraph universeMods universeMods
      dataElements = concat (map moduleTermBindings universeMods)   -- every
                                                                    -- binding from
                                                                    -- every module
  in Eithers.bind (Inference.inferGraphTypes cx dataElements g0) $ \r ->
        Right (Lists.map (refreshModule (Pairs.second (Pairs.first r))) targetMods)
```

`inferGraphTypes` wraps `dataElements` into one big `let bs in unit`
and unifies the whole thing in a single call. The full substitution
map, all inferred bindings, and the constraint set live in memory
simultaneously. With 266 modules at ~thousands of bindings, this
exceeds the 7 GB runner ceiling under the cold-CI path.

## Hypothesis

Processing the universe one strongly-connected component at a time —
calling `inferModulesGiven` with the typed-so-far universe and the
next SCC as the target subset — should bound peak memory by the
largest SCC's bindings (a single module's, for an acyclic universe).
`inferModulesGiven` puts only `bindingsToInfer` into `inferGraphTypes`
and lets the rest of the typed universe contribute via
`graphBoundTypes` from `modulesToGraph`.

The universe is fully acyclic at the module level (verified with a
Tarjan SCC sweep over `dist/json/**/*.json`'s declared
`moduleDependencies`: 380/380 singletons), so each iteration would
handle exactly one module.

## What was attempted

1. **DSL rewrite** in
   `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Generation.hs`
   replacing `inferModules`' all-at-once body with
   ```
   components <- Sorting.topologicalSortNodes moduleNamespace moduleDependencies universeMods
   stepInfer typedSoFar scc = inferModulesGiven cx bsGraph typedSoFar scc
                              `Eithers.bind` \typedScc ->
                                 Right (replaceModulesByNamespace typedSoFar typedScc)
   Eithers.foldl stepInfer universeMods components
       |> Eithers.map (\finalUniverse ->
            Lists.map (refreshModule (concat (map moduleTermBindings finalUniverse))) targetMods)
   ```
   plus a helper `replaceModulesByNamespace :: [Module] -> [Module] -> [Module]`.

2. **Bootstrap patch** of the corresponding generated function in
   `dist/haskell/hydra-kernel/src/main/haskell/Hydra/Codegen.hs` so the
   binary running `update-json-main` could regenerate against the new
   DSL. (Generated files are never edited except as deliberate
   bootstrap patches that the next regen overwrites.)

3. **Dependency-declaration fix-ups** (50+ source-module edits, plus
   `Decoding.decodeModule` / `Encoding.encodeModule` / `Dsls.dslModule`
   synthesizer fixes) because the per-module `inferModulesGiven` walks
   the *declared* `moduleDependencies` for its closure: any missing
   declared dep that the term body actually references shows up as
   `Type inference failed: no such binding: hydra.foo.bar` once the
   inference is no longer all-at-once. This part of the work surfaced
   real translingual data-quality bugs and is preserved in the revert
   (see the dep-fix commit).

## Measured results

Running `stack exec update-json-main` against the patched
`inferModules` on a clean digest:

| Metric            | Old all-at-once | Per-SCC fold (attempted) |
|-------------------|-----------------|--------------------------|
| Wall time         | ~5 min          | 11.4 min                 |
| Peak RSS          | <7 GB (CI cap)  | **22 GB**                |
| CI ceiling fit?   | Barely (overflows under -M6G but lives <8 GB on a roomy host) | Far over |
| Result            | Success         | Success (locally), fails CI |

The per-SCC fold is **strictly worse**: ~4× peak memory and ~2× wall
time. It is *not* a viable replacement.

## Why it likely got worse (unconfirmed)

The fold accumulates lazy thunks: each `replaceModulesByNamespace
typedSoFar typedScc` returns a new `[Module]` whose elements are
either the prior universe entry or a freshly-typed replacement.
Lazy evaluation can retain the entire chain — 266 partially-evaluated
intermediate universes — until something downstream forces them, at
which point both the original and the typed versions of each module
live simultaneously through the chain.

A second contributor is that each iteration calls
`modulesToGraph bsGraph typedSoFar typedSoFar`, allocating a fresh
full-universe graph (266 modules × ~50 bindings = ~10k boundTypes
entries) per iteration. These too are likely retained lazily.

The all-at-once `inferModules` allocates one such graph total; the
per-SCC fold allocates 266 of them with lazy retention.

## Recommended next steps

If we revisit this:

1. **Force strictness explicitly.** Either use a strict left fold
   (`Lists.foldl'` equivalent if available at the DSL level) or
   evaluate `replaceModulesByNamespace`'s output with `deepseq` /
   force-to-WHNF on every iteration.
2. **Avoid rebuilding the full-universe graph per iteration.** The
   typed environment is the part that grows; pass an accumulated
   `Map Name TypeScheme` (or a `Graph` updated in place) instead of a
   `[Module]` list. `modulesToGraph` would be called once with the
   initial universe; subsequent steps would extend the type
   environment directly.
3. **Or: solve the cold-CI path differently.** The cold-path digest
   cache miss is the actual trigger; one alternative is to make
   `tryIncrementalInference`'s "no digest" branch attempt a cheap
   pre-pass that constructs a fake digest from raw kernel DSL sources
   (treat all modules as clean on first run), so subsequent inference
   takes the small `inferModulesGiven` path.

Whichever direction is chosen, the **dependency-declaration fixes
should remain in place** — they fix real translingual bugs that the
all-at-once `inferModules` was masking, and they are a prerequisite
for any incremental-inference design.

## Preserved on branch

The full attempt (DSL rewrite + bootstrap patch + dep fixes) is
preserved on local branch `wip_per_scc_inferModules` for future
reference. Staging has been reverted to keep only the dep fixes and
this document.
