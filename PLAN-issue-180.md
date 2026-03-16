# Plan: Finish Issue #180 — Generate Domain-Specific DSL Modules

## Status Assessment

### Completed
- [x] DSL source module: `Hydra/Sources/Kernel/Terms/Dsls.hs` — full logic for generating record constructors, field accessors, `withXxx` updaters, union injectors, wrap/unwrap accessors, deduplication, namespace mapping
- [x] Generated `Hydra/Dsls.hs` in `gen-main` (partially — see bugs below)
- [x] Generated DSL modules in `gen-main/haskell/Hydra/Dsl/` (~23 modules: Core, Graph, Module, Coders, etc.)
- [x] Moved `Hydra.Dsl.Tabular` → `Hydra.Dsl.Meta.Tabular` (imports updated across ~60 source files)
- [x] `Generation.hs` wired up with `writeDslHaskell` / `generateDslModules`
- [x] `Dsls.module_` registered in `kernelPrimaryTermsModules` via `All.hs`

### Broken (build fails)
- `Hydra/Dsl/Coders.hs` (and likely other generated DSL modules) have **duplicate function declarations** (e.g., `languageConstraints` appears twice, `languageName` appears twice)
- Root cause: **`deduplicateBindings` was silently dropped during code generation** — it doesn't appear at all in the generated `Hydra/Dsls.hs`. The function uses `Maps.empty`, `Maps.member`, `Maps.insert`, and `Pairs.first`/`Pairs.second`/`pair`, none of which are imported in the generated file.
- Consequently, `dslModule` in the generated code calls `Lists.concat dslBindings` without the `deduplicateBindings` wrapper

## Plan

### Phase 1: Fix the deduplication bug

**Goal**: Get `deduplicateBindings` to survive code generation so duplicates are resolved.

#### Step 1.1: Diagnose why `deduplicateBindings` was dropped
- The function uses `Maps.empty :: TTerm (M.Map String Bool)` with an explicit type annotation, `Maps.member`, `Maps.insert`, and `Pairs.first`/`Pairs.second`/`pair`. One or more of these likely caused a silent generation failure.
- Load the module in GHCi and try generating just the `Dsls` module to see error output:
  ```
  stack ghci hydra:lib
  import Hydra.Generation
  import Hydra.Sources.Kernel.Terms.Dsls as Dsls
  ```
- Check if the Haskell coder can handle `Map` types (polymorphic containers in generated code) and `pair` tuples

#### Step 1.2: Fix or rewrite `deduplicateBindings`
- **Option A**: Simplify `deduplicateBindings` to avoid Map/Pair constructs that may not code-generate cleanly. For example, use a list-based approach (check each binding's name against preceding names using `Lists.elem` or similar).
- **Option B**: Fix the underlying code generation issue if it's a bug in the Haskell coder's handling of Map/Pair types.
- **Option C**: Move deduplication logic into the Haskell `Generation.hs` as native code rather than in the DSL (pragmatic fallback — deduplication is a generation-time concern, not a runtime concern for other languages).

#### Step 1.3: Regenerate and verify
- Regenerate `Hydra/Dsls.hs` and verify `deduplicateBindings` appears
- Regenerate all DSL modules and verify no duplicate declarations
- Run `stack build` and `stack test` in `hydra-haskell`

### Phase 2: Verify generated DSL module quality

#### Step 2.1: Spot-check generated modules
- Compare a generated DSL module (e.g., `Hydra/Dsl/Core.hs`) against the existing hand-written `Hydra.Dsl.Core` (if one exists) to verify correctness
- Check that constructors, accessors, updaters, injectors, and wrap/unwrap functions are all present and well-formed

#### Step 2.2: Integration test
- Verify that existing code that imports DSL modules still compiles (the generated modules should be drop-in replacements or supplements)
- Run full `stack test`

### Phase 3: Sync to other languages

#### Step 3.1: Sync Haskell
```bash
cd hydra-haskell && ./bin/sync-haskell.sh
```

#### Step 3.2: Sync Ext, Java, Python
```bash
cd hydra-ext && ./bin/sync-ext.sh
cd hydra-ext && ./bin/sync-java.sh
cd hydra-ext && ./bin/sync-python.sh
```
Or use `./bin/sync-all.sh` from the repo root.

### Phase 4: Commit and PR

- Commit all changes
- Create PR referencing issue #180

## Root Cause (final — corrected)

The actual root cause was **infinite recursion** in the `isUnitType_` helper function
in `Dsls.hs`. This helper was defined as a recursive Haskell-level DSL value:

```haskell
isUnitType_ = "t" ~> cases _Type (var "t") (Just Phantoms.false) [
  _Type_annotated>>: "at" ~> isUnitType_ @@ (Core.annotatedTypeBody (var "at")),
  ...]
```

The `isUnitType_ @@` on the RHS creates a Haskell-level infinite data structure (since
`isUnitType_` references itself as a Haskell value, not as a Hydra variable). When the
code generator tries to evaluate this term, it goes into infinite recursion → stack overflow.

**The fix:** Replace the recursive `isUnitType_` with a non-recursive version that uses
`stripAnnotations` to strip annotations first, then checks for unit.

Additionally, the `hydra.dsls` module must be generated **separately** from `mainModules`
(not included in `kernelPrimaryTermsModules`) because including it causes a stack overflow
during the main kernel generation even after the `isUnitType_` fix.

## Root Cause (refined — earlier hypothesis, superseded)

The `deduplicateBindings` function at line 349 of `Dsls.hs` uses:
```haskell
(pair (Maps.empty :: TTerm (M.Map String Bool)) (list ([] :: [TTerm Binding])))
```

The Haskell type annotation `:: TTerm (M.Map String Bool)` causes the DSL to emit a
`Term.annotated` wrapper. The Haskell coder (`Sources/Haskell/Coder.hs`, lines 390-484)
handles 17 of 18 Term variants but does **not** handle `_Term_annotated`. When it hits this
variant, it calls `failInContext`, and the binding is **silently dropped** from the output.

This also explains why `deduplicateBindings` is missing entirely from the generated
`Hydra/Dsls.hs` (not just the call to it) — the function itself failed to generate.

### Fix options (Phase 1)

- **Option A (preferred)**: Rewrite `deduplicateBindings` to avoid the type annotation.
  Use a helper or restructure to avoid `Maps.empty :: TTerm (M.Map String Bool)`.
  For example, pass the empty map through a typed helper function, or use a list-based
  deduplication approach that avoids Maps entirely.
- **Option B**: Add `_Term_annotated` handling to the Haskell coder (strip the annotation
  and encode the inner term). This is a broader fix but touches the coder.
- **Option C (fallback)**: Move deduplication to native Haskell in `Generation.hs`.
  Pragmatic but means it won't be available in Java/Python DSL generation.

## Key Risk

The biggest risk is in Phase 1. Options A and B are both viable; Option A is lower-risk
since it only touches the new code. Option B is more correct long-term (other functions
may also hit this issue) but has higher blast radius.
