# Issue 192: Simplify Graph / Typing Context

GitHub: https://github.com/CategoricalData/hydra/issues/192

## Problem

`Graph` currently serves two purposes that are in tension:
1. **Track bindings** — the `elements: List<Binding>` field preserves insertion order, needed for code generation and transformations.
2. **Provide an environment for fast lexical lookups** — `lookupElement` currently does O(n) linear scan of `elements` via `Lists.find`. With many bindings (e.g., 300+ when loading kernel modules from JSON), this causes a ~4x performance regression in tests and a ~13x increase in comparison operations per lookup.

Additionally, `Graph` and `TypeContext` overlap significantly and should eventually be unified.

Along with this migration, Flow-based operations (#245) which pass graphs as state will be eliminated. Instead, graphs will be passed as explicit arguments. This will be much better for Java, Python, and other future targets without GHC's level of support for monads and higher-level functions.

## Current Graph Fields

```
Graph:
  elements:    List Binding              -- ordered bindings (name + term + optional type scheme)
  environment: Map Name (Maybe Term)     -- intended for lambda/let distinction; NEVER POPULATED
  types:       Map Name TypeScheme       -- typing environment
  body:        Term                      -- body of the term that generated this context
  primitives:  Map Name Primitive        -- primitive functions
  schema:      Maybe Graph               -- schema graph
```

## The `environment` Field

`environment` is declared as `Map Name (Maybe Term)` to distinguish lambda-bound variables (`Nothing`) from let-bound variables (`Just term`). However:

- **It is never populated.** Always `Maps.empty` or copied unchanged from parent.
- **It is never meaningfully read.** Only touched in `Adapt/Simple.hs`, where it's passed through unchanged.
- **There is an explicit TODO** in `Lexical.hs` (`resolveTerm`): "TODO: distinguish between lambda-bound and let-bound variables"
- **Its type doesn't match what `lookupElement` needs.** `lookupElement` needs `Map Name Binding` (to return the full binding), not `Map Name (Maybe Term)`.

### Could `environment` be repurposed for O(1) lookups?

Not directly — the type is wrong (`Maybe Term` vs `Binding`). We'd need either:
- Change its type to `Map Name Binding` (breaking the lambda/let semantics it was designed for)
- Add a separate index field

## Proposed Direction: "Graf" Unified Type

Create a new type (working name "Graf" for easy search-and-replace) that unifies `Graph` and `TypeContext`:

```
Graf:
  elements:     List Binding              -- ordered bindings (preserves insertion order)
  elementIndex: Map Name Binding          -- O(1) lookup index, built from elements
  types:        Map Name TypeScheme       -- typing environment
  body:         Term                      -- context body
  primitives:   Map Name Primitive        -- primitive functions
  schema:       Maybe Graf                -- schema
```

Key changes from current `Graph`:
- **Add `elementIndex`** — `Map Name Binding` built from `elements` at construction time
- **Remove or repurpose `environment`** — its lambda/let distinction can be handled differently (e.g., a `Set Name` of lambda-bound vars, or inferred from context)
- **Merge `TypeContext` and `InferenceContext` fields** as needed

### Design Tension: Graph-as-Environment vs Graph-as-Let

Graf is used in two fundamentally different ways:

**Graph-as-environment:** A context accumulated by descending into a term. At any point, it
contains everything in scope: primitive types and implementations, schema-level type schemes
bound to names (e.g., a union type bound to "LatLon"), types of lambda-bound variables, type
schemes and terms of let-bound variables, class constraints bound to variable names, type
variables introduced by enclosing type lambdas, and other metadata. This is what reduction,
type checking, and code generation need — a *read* view for lookups.

**Graph-as-let:** A module-level collection of named bindings with a body. A collection of
modules can always be represented as a let term (list of bindings + trivial body). This is
the graph you get by descending into a let term but not yet into its body. Operations like
inference and adaptation need to *iterate* over these bindings, transform them, topologically
sort them, and produce new bindings. This is a *structural* view — the graph is data to be
processed.

**The relationship:** These are the same thing at different levels. When you're *outside* a let
(processing its bindings), it's structural data. When you're *inside* one of those bindings
(reducing, type-checking), the let's other bindings become part of the environment. The
transition point is "entering the let" — bindings move from data to context.

**What Graf needs to contain (environment view):**
- `elementIndex: Map Name Binding` — let-bound variables in scope (names → terms + type schemes)
- `elements: List Binding` — same bindings, ordered, for structural operations
- `types: Map Name TypeScheme` — schema-level types bound to names
- `lambdaTypes: Map Name Type` — types of lambda-bound variables currently in scope
- `typeVariables: Set Name` — type variables introduced by enclosing type lambdas
- `constraints: Map Name [TypeClass]` — class constraints bound to variable names
- `primitives: Map Name Primitive` — primitive functions
- `schema: Maybe Graf` — schema graph
- `body: Term` — the body of the let (needed for the structural view)

**Open question:** Should the structural (let) view be a separate type that gets *converted*
to a Graf when descending into bindings? Or should Graf serve both roles, with the `elements`
list supporting iteration and the index supporting lookup? The current approach keeps them
together. Separating them would mean operations like inference take a `LetTerm` as input and
construct a Graf internally when checking individual bindings. But many operations need both
views simultaneously — iterating bindings while also having full environment access for each.

### Dual-Role Feasibility Analysis

Investigation of how Graph is used across `Inference.hs`, `Adapt/Simple.hs`, and `Schemas.hs`
shows that **clean separation of Graph-as-environment from Graph-as-let is feasible**. In most
places the two roles are already relatively distinct.

**Already separated (easy):**

| Function | File | Notes |
|----------|------|-------|
| `graphAsLet` / `graphAsTerm` / `termAsGraph` | Schemas | Purely structural — no env involvement |
| `graphToInferenceContext` / `graphToTypeContext` | Schemas | Bridge functions: read graph structurally, produce env objects (`InferenceContext`, `TypeContext`). Everything downstream uses only the env. |
| `inferGraphTypes` | Inference | Two roles are separate sub-expressions: `graphToInferenceContext` produces `cx` (env), `toLetTerm` serializes graph (structural). These flow into `inferTypeOfTerm` as separate arguments. |
| `inferInGraphContext` | Inference | Env-only: `getState` → `graphToInferenceContext` → pass `cx` to inference. No structural use. |
| `dataGraphToDefinitions` sub-pipelines | Simple | `hoistCases` and `hoistPoly` are clearly structural (`graphAsLet` → transform → `graphWithElements`) |

**Moderately tangled (straightforward to refactor):**

| Function | File | Notes |
|----------|------|-------|
| `adaptDataGraph` | Simple | `transform` lambda uses same graph for `graphToTypeContext` (env) and as `gterm` (structural). Fix: pre-extract type context before entering the lambda. |
| `schemaGraphToTypingEnvironment` | Schemas | `withState @@ g` (env) and `graphElements g` (structural) in same expression. Fix: extract one. |

**Fused at data level (not logic):**

| Function | File | Notes |
|----------|------|-------|
| `lookupElement` | Lexical | Lookup-by-name (env semantics) implemented as linear scan of `graphElements` (structural). Resolved by adding `elementIndex: Map Name Binding`. |

**Key insight:** The bridge functions (`graphToInferenceContext`, `graphToTypeContext`) already
perform structural-to-environment conversion. All downstream code uses only the resulting env
object. If Graph becomes purely an environment (with `elementIndex` for O(1) lookups) and `Let`
remains the structural view:

1. Functions that currently take `Graph` and call `graphAsTerm`/`graphAsLet` would receive a `Let` directly
2. Functions that derive env from graph (via `graphToInferenceContext`) would receive the env (or Graph-as-env) directly
3. `adaptDataGraph` needs the most restructuring — pre-extract the type context before the transform pipeline

### Eliminating Flow-Based Graph State (#245)

Currently, many operations use `Flow Graph` where the graph is threaded as monadic state:
- `dereferenceElement :: Name -> Flow Graph (Maybe Binding)` — gets graph from `Monads.getState`
- `dereferenceVariable`, `resolveTerm`, `requireElement` (Flow variants) — same pattern
- Reduction, schema operations, code generation — all use `Flow Graph`

These will be replaced with explicit graph arguments:
- `dereferenceElement :: Graph -> Name -> Maybe Binding` (same as `lookupElement`)
- `reduce :: Graph -> Term -> Term` (graph as argument, not state)
- Code generation functions take graph explicitly

This eliminates the overhead of monadic state-passing and makes the generated Java/Python code dramatically simpler — no `Flow<Graph, T>` wrappers, no `getState`/`putState`, just plain function calls. This is especially important for languages without GHC's level of support for monads and higher-order functions.

#### Impact Assessment

**Critical finding: Graph state is read-only.** No kernel function calls `putState` on Graph.
`putState` is only used for `InferenceContext` (fresh variable counters, constraints),
`TarjanState` (algorithm state), and debug flags. This means the `Flow Graph x` →
`Graph -> Either String x` migration is clean — no state threading needed for Graph.

**~175 Flow-returning functions in the kernel**, broken down by state type and migration difficulty:

| Category | State type | Count | Mutates state? | Migration |
|----------|-----------|-------|---------------|-----------|
| **A: Graph read-only** | `Flow Graph` | ~30 | No | `Graph -> Either String x` |
| **B: Inference/Checking** | `Flow InferenceContext` | ~60 | Yes (counters, constraints) | Needs design decision |
| **C: Pure + error** | `Flow s` (generic) | ~15 | No | `Either String x` |
| **D: Algorithm state** | `Flow TarjanState` | ~3 | Yes | Keep as-is or localize |

**What Flow currently provides:**
- **Error handling** (~100+ uses): `fail`/`pure` → becomes `Left`/`Right`
- **State reads** (~60 uses): `getState` for Graph lookup → becomes explicit graph arg
- **State mutation** (~15 uses): only in inference/checking, never on Graph
- **Tracing** (~40 uses): `withTrace` adds context to error messages → becomes error prefixing

**Module-by-module breakdown:**

| Module | Flow functions | State type | Notes |
|--------|---------------|-----------|-------|
| `Lexical.hs` | ~18 | `Flow Graph` | Wrappers around lookupElement; mostly collapse to direct calls |
| `Reduction.hs` | ~3 | `Flow Graph` | `reduce`, `betaReduceType`, `etaExpansionArity` — straightforward |
| `Schemas.hs` | ~50 | Mixed | Graph lookups + InferenceContext mutation for fresh vars |
| `Checking.hs` | ~40 | `Flow InferenceContext` | Type checking with mutable context |
| `Inference.hs` | ~60 | `Flow InferenceContext` | Heaviest user — constraint generation, unification |
| `Adapt/Simple.hs` | ~7 | `Flow Graph` | Data adaptation; read-only graph |
| `Monads.hs` | ~19 | Foundation | Defines Flow operations themselves |

#### DSL Complexity Assessment

There are **805 `<<~` occurrences** across kernel + ext sources. These break down as:

- **~30 `getState` binds** — simply deleted; the graph becomes an explicit parameter
- **~775 error-propagation binds** — the dominant pattern; chains fallible operations

The plan is to **redefine `<<~` to bind over `Either Error` instead of `Flow`**. This means the
vast majority of `<<~` call sites barely change — they just gain an explicit `var "g"` argument
to each called function. The mechanical burden is adding the graph argument to ~775 call sites.

**`<<~` usage by file (top contributors):**

| File | `<<~` count | `getState` count | Notes |
|------|------------|-----------------|-------|
| Java/Coder.hs | 215 | 7 | Largest single file |
| Python/Coder.hs | 116 | 5 | |
| Rewriting.hs | 84 | 0 | Pure error propagation |
| Adapt/Terms.hs | 70 | 4 | |
| Inference.hs | 59 | 1 | Has mutable InferenceContext state |
| Extract/Core.hs | 55 | 0 | Pure error propagation |
| Checking.hs | 38 | 0 | Has mutable InferenceContext state |
| Adapt/Simple.hs | 38 | 0 | |
| Reduction.hs | 27 | 0 | |
| Schemas.hs | 16 | 8 | |
| Lexical.hs | 8 | 5 | |

**What gets simpler:**
- `getState` patterns (~30 uses) — deleted entirely
- `withState` / `withSchemaContext` — monadic gymnastics disappear
- `Flows.pure` / `produce` — become `Right` / direct return
- `Flows.fail` — becomes `Left`
- Generated Java/Python — **dramatically simpler** (no Flow wrappers, state closures)

**What stays roughly the same:**
- `<<~` error chains (~775 uses) — redefined for `Either`; each call gains `var "g"`

**What needs design work:**
- `Flows.mapList` / `Flows.mapElems` / `Flows.mapMaybe` — need `Either`-based equivalents (simpler: just traverse/mapM for Either)
- `withTrace` (~40 uses) — needs replacement for error context (e.g., error message prefixing)

**Net: the DSL code will be slightly simpler overall, and the generated code dramatically simpler.**

#### The hard part: Inference and Checking

`InferenceContext` genuinely needs mutable state for:
- **Fresh type variable generation** (`freshName` increments a counter)
- **Constraint accumulation** during unification
- **Warning/error collection**

For mutable state, instead of monadic `putState`, we create the new context and pass it as an
argument to subsequent functions. Functions like `freshName` would return
`(Name, InferenceContext)` — the fresh name plus the updated context. Callers thread the updated
context forward explicitly.

This is more verbose but transparent and natural for Java/Python targets. It also makes data flow
explicit, which aids debugging and reasoning about state.

#### Tracing Without Flow

With `Flow` eliminated, we lose the `Trace` mechanism that provides essential debugging context
(error stack traces like "Error: no such element (in reduce > in checkType > in inferType)").

Trace must be independent of Graph, because we also have `Flow AdapterContext`,
`Flow InferenceContext`, etc. — all of which need tracing.

**Solution: Independent `Context` argument + trace-carrying `Error`**

`Context` is an independent, unparameterized type — essentially just `Trace` (possibly with a
few execution settings). It is passed as a separate argument, decoupled from any domain state.

```
Context:
  trace: Trace          -- execution context for debugging

Error:
  message: String       -- error description
  trace: Trace          -- trace at point of failure
```

Function signatures become:

```
Context -> Graph -> x -> Either Error y              -- reduction, lexical, codegen
Context -> AdapterContext -> x -> Either Error y      -- adaptation
Context -> InferenceContext -> x -> Either Error y    -- inference/checking (with state threading)
Context -> x -> Either Error y                       -- error-propagation with tracing
x -> Either Error y                                  -- pure error-propagation (no tracing needed)
```

**Why independent rather than bundled (`Context s`)?**

We considered wrapping state with trace (`Context s = {value: s, trace: Trace}`), but coupling
them has little value beyond saving one argument per function. The downsides of bundling:
- Introduces a type parameter and wrapping/unwrapping overhead
- Trace and domain state have different lifecycles (trace grows monotonically; state varies)
- The combination `Context Graph` collides with the "context graph" term from the LLM/RAG ecosystem
- Bundling is just argument-count optimization — not worth the conceptual coupling

Keeping `Context` unparameterized is simpler: every function gets `Context` uniformly, regardless
of what domain state (if any) it also receives.

**How trace context works:**

```haskell
-- Current DSL:
-- withTrace "in reduce" $ reduce @@ var "term"

-- New DSL:
-- reduce @@ (withTrace "in reduce" (var "cx")) @@ var "g" @@ var "term"
-- where withTrace constructs a new Context with updated trace
```

**How failure works:**

```haskell
-- Current: Flows.fail "no such element: foo"
-- New: Left(Error{message="no such element: foo", trace=cx.trace})
-- The <<~ operator captures trace from the current Context into the Error on Left
```

**Advantages:**
- `Context` is a simple, unparameterized type — no generics, no wrapping/unwrapping
- Trace propagation is uniform across all function signatures
- Graf/Graph stays purely about data — no execution context mixed in
- Phantom-state modules (`Flow s` where s is unused) can use `Context -> x -> Either Error y`
  or just `x -> Either Error y` if tracing isn't needed
- `Error` carries the trace snapshot at point of failure — exactly what you need for debugging
- For mutable state (InferenceContext), functions return `(InferenceContext, Either Error y)`
  — the updated state is threaded forward; `Context` is a separate, independent argument

#### Migration Order

Modules ranked by ease of conversion from `Flow` to `Either`, based on state interaction:

| Priority | Module | <<~ | getState | putState | withState | mapList/etc | State type | Difficulty |
|----------|--------|-----|----------|----------|-----------|------------|------------|------------|
| 1 | **Extract/Core.hs** | 55 | 0 | 0 | 0 | 3 | Flow Graph | **Easy** — good size to establish pattern |
| 2 | **Reduction.hs** | 27 | 0 | 0 | 0 | 3 | Flow Graph | **Easy** — validates approach for hot path |
| 3 | **Checking.hs** | 38 | 0 | 0 | 0 | 14 | Flow s | **Easy** — phantom state, pure error propagation |
| 4 | **Inference.hs** | 59 | 1 | 0 | 0 | 2 | Flow s | **Easy** — phantom state despite being inference |
| 5 | **Adapt/Simple.hs** | 38 | 0 | 0 | 0 | 4 | Flow s | **Easy** |
| 6 | **Rewriting.hs** | 84 | 0 | 0 | 0 | 16 | Flow s | **Easy** — high volume but mechanical |
| 7 | **Annotations.hs** | 9 | 2 | 0 | 0 | 1 | Flow s | **Easy** — small |
| 8 | **Lexical.hs** | 8 | 5 | 0 | 2 | 0 | Flow Graph | **Medium** — withState for schema context |
| 9 | **Schemas.hs** | 16 | 8 | 0 | 1 | 5 | Flow Graph | **Medium** — withState, heavy getState |
| 10 | **CodeGeneration.hs** | 9 | 1 | 0 | 4 | 4 | Flow Graph | **Medium** — 4 withState calls |
| 11 | **Adapt/Terms.hs** | 70 | 4 | **2** | 1 | 9 | Flow AdapterContext | **Hardest** — only module with putState |

Key observations:
- **Most modules have zero state interaction.** The `Flow s` parameter is phantom — used purely
  for error propagation. These are straightforward conversions.
- **Only Adapt/Terms.hs has `putState`** (2 calls, updating AdapterContext). This is the only
  module requiring explicit state threading in the return type.
- **`withState` appears in Lexical, Schemas, and CodeGeneration** — used to switch graph context
  (e.g., to schema graph for type lookups). These become explicit graph argument substitution.
- **Show/Core.hs has no Flow usage at all** — already pure.

### Construction Points to Update

These DSL functions construct or modify graphs and would need to build/maintain the index:

| Function | File | Current behavior |
|----------|------|-----------------|
| `emptyGraph` | `Lexical.hs:167` | `elements=[], environment=empty` |
| `elementsToGraph` | `Lexical.hs:155` | Takes elements list, copies environment from parent |
| `extendGraphWithBindings` | `Lexical.hs:178` | Prepends bindings to elements, copies environment |
| `graphWithElements` | `Graph.hs:53` | Replaces elements, copies environment |
| `Adapt/Simple.hs:145,189` | Reads environment, passes through unchanged |

Plus hand-written Java (`TestSuiteRunner.buildTestGraph`, `Generation` methods) that construct `Graph` directly.

### `lookupElement` Change

Once `elementIndex` exists:

```haskell
-- Current (O(n)):
lookupElement = "g" ~> "name" ~> Lists.find (\b -> b.name == name) (Graph.graphElements g)

-- New (O(1)):
lookupElement = "g" ~> "name" ~> Maps.lookup name (Graph.graphElementIndex g)
```

## Performance Impact

Benchmark data from Java tests (Feb 27, 2026):
- Hand-written graph (~24 bindings): ~1300ms total test time
- JSON-loaded graph (~315 bindings): ~5500ms total test time
- The difference is entirely due to O(n) `lookupElement` being called thousands of times during reduction and code generation

## Interim Workaround

TestSuiteRunner currently uses hand-written kernel types and term bindings instead of loading from JSON, keeping the binding list small (~24 entries). This restores performance but prevents using Hydra's own JSON-based module loading for tests.

## Module Reorganization

`hydra.compute` currently defines: `Adapter`, `Bicoder`, `Coder`, `Flow`, `FlowState`, `Trace`.
With Flow/FlowState eliminated, the module dissolves entirely. Its types migrate as follows:

| Current (`hydra.compute`) | Destination | Notes |
|---------------------------|-------------|-------|
| `Flow` | **deleted** | Replaced by explicit `Context` + domain state arguments + `Either Error` return |
| `FlowState` | **deleted** | No longer needed |
| `Trace` | **`hydra.context`** | Part of execution context; built up during execution, not only on failure |
| `Context` (new) | **`hydra.context`** | Independent, unparameterized: essentially `Trace` + execution settings |
| `Error` (new) | **`hydra.error`** | Failure representation: message + trace snapshot |
| `Coder` | **`hydra.coders`** | Already exists; natural home for transformation types |
| `Bicoder` | **`hydra.coders`** | |
| `Adapter` | **`hydra.coders`** | |

The `hydra.monads` module (which defines `pure`, `bind`, `map`, `fail`, `getState`, `putState`,
`withState`, `withTrace`, etc.) also goes away — replaced by:
- `Either`-based operations (built into the language or trivial helpers)
- `Context` helpers in `hydra.context` (e.g., `withTrace` to push a trace frame)

## Related Issues

- **#245**: Eliminate Flow-based operations that pass graphs as state

## Migration Plan (hydra-haskell only)

The migration proceeds in two major epochs. **Epoch A** migrates from `Graph` to `Graf` (unified
environment type with O(1) lookups) and back to `Graph`. **Epoch B** migrates from `Flow`-based
to `Either`-based semantics, introduces `Context`/`Error` types and module reorganization. Epoch B
begins only after Epoch A is complete. Other language targets (Java, Python) are updated after the
type-related changes in Epoch A are done.

---

### Epoch A: Graph → Graf → Graph

Each phase ends with a gate: regenerate, compile, full test suite passes, commit.

**Bootstrapping considerations:** Hydra is self-generating — the DSL source modules in
`src/main/` import `Hydra.Kernel`, which re-exports all generated modules from `src/gen-main/`.
When we change the `Graph` type (adding the `migration` field) or introduce `Graf`, the
generated `gen-main/haskell/Hydra/Graph.hs` must be manually patched before `stack build` can
succeed. Only after building with the patched files can we run the code generator to produce
clean regenerated versions. See `docs/recipes/extending-hydra-core.md` § "Adding Fields to
Existing Record Types" for the detailed procedure.

The bootstrap patching is needed in phases A1, A2, and A4 (where type definitions change).
Phases A3a/A3b change only term-level code (function bodies), so they may not require bootstrap
patches — the generated type definitions are already correct from A2.

#### Phase A1: Define new kernel types ✅ DONE

**Goal:** Create the `Graf` type and any supporting types. No behavioral changes yet.

- [x] Define `Graf` type in `hydra.graph` (Graph.hs), with 8 fields:
  - `boundTerms: Map Name Term` — terms bound by all term variables in scope
  - `boundTypes: Map Name Type` — System F types of all term variables in scope
  - `classConstraints: Map Name TypeVariableMetadata` — class constraints on type variables
  - `lambdaVariables: Set Name` — lambda-introduced term variables
  - `metadata: Map Name Term` — additional metadata
  - `primitives: Map Name Primitive` — primitive functions
  - `schemaTypes: Map Name TypeScheme` — schema types in scope
  - `typeVariables: Set Name` — type variables from type lambdas
- [x] Add DSL constructor `graf` and 8 field accessors in `Hydra/Dsl/Meta/Graph.hs`
- [x] Add `emptyGraf` DSL helper
- [x] Bootstrap and regenerate. Commit.

#### Phase A2: Add transitionalGraf field to Graph ✅ DONE

**Goal:** Every `Graph` carries a `Graf` so we can incrementally migrate consumers.

- [x] Add field to `Graph`: `transitionalGraf: Graf` — temporary bridge field (always present)
- [x] Update DSL constructor `graph` to accept the new field
- [x] Update all `graphWith*` helpers to preserve/rebuild the `transitionalGraf` field
- [x] Supply `emptyGraf` (later `buildGraf`) at every Graph construction site
- [x] Bootstrap and regenerate. Commit.

#### Phase A3: Populate Graf and switch to O(1) lookups ✅ DONE

**Goal:** Build real `Graf` values at every Graph construction point, then switch lookups
to use the Graf's maps for O(1) performance.

- [x] Create `buildGraf` helper in `hydra.lexical` that constructs a `Graf` from `[Binding]`,
  `Map Name (Maybe Term)` (environment), and `Map Name Primitive` (primitives):
  - `grafBoundTerms` = element name→term map ∪ let-bound environment vars
  - `grafBoundTypes` = element bindingType converted to System F types via `typeSchemeToFType`
  - `grafLambdaVariables` = environment keys where value is Nothing
  - `grafPrimitives` = primitives
  - Other fields empty for now (schemaTypes, classConstraints, metadata, typeVariables)
- [x] Update all Graph construction sites to use `buildGraf`:
  `emptyGraph`, `elementsToGraph`, `extendGraphWithBindings`, `inferGraphTypes`, `adaptDataGraph`
- [x] Update DSL `graphWith*` helpers: `graphWithElements`, `graphWithEnvironment`,
  `graphWithPrimitives` rebuild graf via `rebuildGraf`; `graphWithBody`, `graphWithTypes`,
  `graphWithSchema` pass through existing graf
- [x] Add `lookupTerm` — O(1) map lookup from `grafBoundTerms`
- [x] Switch `resolveTerm` to use `lookupTerm` instead of O(n) `Lists.find`
- [x] Switch `lookupElement` to use O(1) `grafBoundTerms` + `grafBoundTypes` map lookups,
  reconstructing `Binding` from name + term + type (via `fTypeToTypeScheme`)
- [x] Move `fTypeToTypeScheme` and `typeSchemeToFType` from `hydra.schemas` to `hydra.rewriting`
  (needed by `hydra.lexical` without circular dependency)
- [x] Update all references in `Schemas.hs`, `Checking.hs`, `Inference.hs` to use
  `Rewriting.typeSchemeToFType` / `Rewriting.fTypeToTypeScheme`
- [x] Bootstrap and regenerate. Commit.

**Future optimization:** Minimize TypeScheme ↔ System F type conversions by updating callers
of `lookupElement` and related functions to work with System F types directly (since that is
what Graf provides). Currently `buildGraf` converts TypeScheme→Type on construction, and
`lookupElement` converts Type→TypeScheme on lookup. Eventually callers should accept System F
types, eliminating the round-trip.

#### Phase A4: Migrate TypeContext and InferenceContext into Graf

**Goal:** Graf absorbs the fields currently split across `TypeContext` and `InferenceContext`,
eliminating the need for those types as independent structures. This MUST complete before
Graph is removed in A5, because the new Graph (née Graf) needs to be the single environment
type.

Note: Graf already has several of these fields from Phase A1. Fields marked "already in Graf"
just need to be populated; the rest need to be added.

- [ ] Populate existing Graf fields from TypeContext/InferenceContext:
  - `boundTypes: Map Name Type` — already in Graf, partially populated from element bindingType;
    needs to also include lambda-bound variable types from TypeContext.types
  - `classConstraints: Map Name TypeVariableMetadata` — already in Graf, currently empty;
    populate from InferenceContext.classConstraints
  - `lambdaVariables: Set Name` — already in Graf, populated from environment;
    needs to also reflect TypeContext.lambdaVariables
  - `metadata: Map Name Term` — already in Graf, currently empty;
    populate from TypeContext.metadata
  - `schemaTypes: Map Name TypeScheme` — already in Graf, currently empty;
    populate from schema graph's types (deferred from A3)
  - `typeVariables: Set Name` — already in Graf, currently empty;
    populate from TypeContext.typeVariables
- [ ] Add missing fields to `Graf`:
  - `letVariables: Set Name` — from TypeContext.letVariables
  - `primitiveTypes: Map Name TypeScheme` — from InferenceContext.primitiveTypes
  - `debug: Bool` — from InferenceContext.debug
  - (Exact field set subject to refinement based on what's actually needed)
- [ ] **Bootstrap:** manually patch `gen-main/haskell/Hydra/Graph.hs` to add the new fields to
  the `Graf` data definition. Patch generated files that construct `Graf` to supply the new
  fields.
- [x] Update `graphToInferenceContext` and `graphToTypeContext` (Schemas.hs) to read from Graf
  fields instead of re-deriving from `graphElements` iteration
- [x] Update `initialTypeContext` (Inference.hs) to read from `grafBoundTypes` directly
- [x] Refactor `inferGraphTypes` to use `graphAsLet` for extracting Let structure from Graph
- [x] Add `extendGrafForLambda`, `extendGrafForLet`, `extendGrafForTypeLambda` in Schemas
- [x] Migrate pure Graf-parameter functions: `lookupElement`, `lookupPrimitive`, `lookupTerm`,
  `dereferenceVariable`, `stripAndDereferenceTermEither` now take Graf directly (not Graph).
  Old Graph wrappers deleted, graf* names renamed back to originals.
- [x] Migrate `graphToInferenceContext` and `graphToTypeContext` to take Graf directly (not Graph).
  Now pure functions (no Flow). All callers updated with `graphTransitionalGraf`.
- [x] Migrate all gen-main Decode/*.hs decoders to take Graf instead of Graph.
- [x] Eliminate `withSchemaContext` dependency: `requireType` and `resolveType` now look up
  types directly from `grafSchemaTypes`/`grafBoundTypes` instead of switching to the schema graph.
  `transformModule` and `adaptedModuleDefinitions` no longer use `withSchemaContext`.
- [x] Fix `requireElement` error message to use `grafBoundTerms` keys instead of `graphElements`.

**Key finding: `Flow Graph` → `Flow Graf` cannot be done incrementally.** The DSL's phantom
type system (the `<<~` operator) requires `s` in `Flow s a` to unify across entire monadic
chains. Functions that run in `Flow Graph` context AND call kernel functions cannot mix
`Flow Graph` and `Flow Graf` in the same chain. The coder files (Haskell/Coder.hs,
CodeGeneration.hs) set `Flow Graph` via `Monads.withState` and access `Graph.graphElements`,
while also calling kernel functions like `Schemas.requireUnionType`. Changing kernel functions
to `Flow Graf` breaks the type unification. The `Flow Graph` → `Flow Graf` switch must happen
atomically across ALL Sources files at once, as part of Phase A5.

**Remaining `graphElements` accesses (15 total across 7 files):** Most are for iteration
(processing all bindings), not lookup. They need the `[Binding]` list format. The Graf
provides `grafBoundTerms: Map Name Term` which is a different structure. Converting these
would require either: (a) adding a `grafBindings: [Binding]` field to Graf, or (b) changing
consumers to work with maps. This is a structural change that should happen during Phase A5.

**Remaining `graphTransitionalGraf` accesses (~38 total across 10 files):** These are the
correct bridge pattern for now — extracting the graf from Graph state to pass to functions
that need graf data. They will be eliminated when `Flow Graph` → `Flow Graf` is done in A5.

##### Phase A4.1: Eliminate InferenceContext

**Goal:** Replace all InferenceContext parameters with Graph. Graph already contains all the
same data, just organized differently.

**Field mapping (InferenceContext → Graph):**

| InferenceContext field | Graph field | Conversion |
|------------------------|------------|------------|
| `schemaTypes: Map Name TypeScheme` | `graphSchemaTypes` | Direct (same type) |
| `primitiveTypes: Map Name TypeScheme` | `graphPrimitives` | Derived: extract TypeScheme from each Primitive |
| `dataTypes: Map Name TypeScheme` | `graphBoundTypes: Map Name Type` | Convert: `fTypeToTypeScheme` on each |
| `classConstraints: Map Name TypeVariableMetadata` | `graphClassConstraints` | Direct (same type) |
| `debug: Bool` | *(not in Graph)* | Always false; eliminate entirely or pass as separate arg |

**Key changes:**

1. **`extendContext` (Inference.hs):** Currently adds `(Name, TypeScheme)` pairs to
   `inferenceContextDataTypes`. Replace with: add `(Name, Type)` pairs to `graphBoundTypes`
   (converting TypeScheme → Type via `typeSchemeToFType`). The Graph's `extendGraphForLambda`
   and `extendGraphForLet` already do this.

2. **Primitive type lookups:** `inferTypeOfPrimitive` currently looks up from
   `inferenceContextPrimitiveTypes`. Replace with: look up from `graphPrimitives`, then extract
   `primitiveType`.

3. **Data type lookups:** Functions that read `inferenceContextDataTypes` for variable types
   currently get `TypeScheme`. Replace with: read `graphBoundTypes` to get `Type`, then convert
   via `fTypeToTypeScheme` where needed (or change consumers to work with `Type` directly).

4. **`isUnbound`:** Checks if variable is in `freeVariablesInContext` or `schemaTypes`. Change
   to check `graphBoundTypes` + `graphSchemaTypes`.

5. **`generalize`:** Uses `freeVariablesInContext` (all free vars in dataTypes values). Change
   to compute from `graphBoundTypes`.

6. **`checkTypeSubst` (Checking.hs):** Reads `inferenceContextSchemaTypes` for sanity checking.
   Change to read `graphSchemaTypes`.

7. **`substInContext` (Substitution.hs):** Applies type substitution to all InferenceContext
   fields. Change to apply substitution to Graph's `boundTypes`, `classConstraints`, etc.

**Files to modify:**
- `Sources/Kernel/Types/Typing.hs` — eventually remove InferenceContext type
- `Sources/Kernel/Terms/Inference.hs` — ~37 functions change from `InferenceContext ->` to `Graph ->`
- `Sources/Kernel/Terms/Checking.hs` — ~4 functions read InferenceContext via TypeContext
- `Sources/Kernel/Terms/Schemas.hs` — remove `graphToInferenceContext`, update `graphToTypeContext`
- `Sources/Kernel/Terms/Substitution.hs` — `substInContext` updates Graph instead

**Approach:** Since `inferTypeOfLet` calls `extendContext` to create a new InferenceContext,
and `inferTypeOfLambda` creates one with a new variable, all of these map directly to Graph
extension functions that already exist (`extendGraphForLambda`, `extendGraphForLet`).

- [x] Change all ~37 inference functions from `InferenceContext ->` to `Graph ->`
- [x] Replace `inferenceContextSchemaTypes` reads with `graphSchemaTypes`
- [x] Replace `inferenceContextPrimitiveTypes` reads with `graphPrimitives` + extract type
- [x] Replace `inferenceContextDataTypes` reads with `graphBoundTypes` + `fTypeToTypeScheme`
- [x] Replace `inferenceContextClassConstraints` reads with `graphClassConstraints`
- [x] Update `substInContext` to work on Graph
- [x] Change `requireSchemaType` to take `Map Name TypeScheme` directly (not InferenceContext or Graph)
- [x] Update HaskellCodec.hs: pass Graph instead of InferenceContext
- [x] Update TestUtils.hs: pass testGraph directly to inferTypeOf
- [x] Add `graphWithBoundTypes` and `graphWithClassConstraints` helpers to Dsl/Meta/Graph.hs
- [x] Remove `emptyInferenceContext`, `graphToInferenceContext`, `graphToTypeContext` (done in A4.2)
- [x] Gate: build, sync, test (4396 tests pass)

##### Phase A4.2: Eliminate TypeContext

**Goal:** Replace all TypeContext parameters with Graph. Graph already contains all the same
fields, plus the InferenceContext fields (now replaced by Graph in A4.1).

**Field mapping (TypeContext → Graph):**

| TypeContext field | Graph field | Notes |
|-------------------|------------|-------|
| `types: Map Name Type` | `graphBoundTypes` | Direct (same type) |
| `metadata: Map Name Term` | `graphMetadata` | Direct (same type) |
| `typeVariables: Set Name` | `graphTypeVariables` | Direct (same type) |
| `lambdaVariables: Set Name` | `graphLambdaVariables` | Direct (same type) |
| `letVariables: Set Name` | **Not needed** — derivable as `keys(boundTerms) - lambdaVariables` |
| `inferenceContext: InferenceContext` | *(eliminated in A4.1)* | Already in Graph fields |

**`letVariables` is redundant:** `boundTerms` contains all term bindings. When a lambda-bound
variable enters scope, it is added to `lambdaVariables` (and to `boundTerms`). When a let-bound
variable enters scope, it is added to `boundTerms` and removed from `lambdaVariables` (shadowing).
So `letVariables = keys(boundTerms) - lambdaVariables`. No new field needed.

**Key changes:**

1. **`extendTypeContextForLambda/Let/TypeLambda`:** These already have parallel
   `extendGraphForLambda/Let/TypeLambda` functions that do the same thing. After A4.2,
   the TypeContext versions are deleted and only the Graph versions remain.

2. **All Checking.hs functions:** Change from `TypeContext ->` to `Graph ->`. Most access
   `typeContextTypes` → `graphBoundTypes`, `typeContextInferenceContext` → already in Graph.

3. **`isComplexVariable` (CoderUtils.hs):** Does NOT use `letVariables` — only uses
   `typeContextMetadata`, `typeContextLambdaVariables`, `typeContextTypes`. No change needed
   beyond the parameter type.

4. **Hoisting.hs:** Uses `typeContextLetVariables` for `boundTermVariables` computation and
   `polyLetVariables` filtering. Replace with `Sets.fromList(Maps.keys(graphBoundTerms g))`
   minus `graphLambdaVariables g`, or just `Maps.keys(graphBoundTerms g)` for the union.

5. **Files to modify:**
   - Checking.hs (~28 functions): `TypeContext ->` → `Graph ->`
   - CoderUtils.hs: `isComplexVariable` parameter type
   - Hoisting.hs: derive letVariables from boundTerms/lambdaVariables
   - Reduction.hs: TypeContext → Graph parameter changes
   - Schemas.hs: delete `extendTypeContextFor*`, `graphToTypeContext`
   - Inference.hs: delete `initialTypeContext`
   - Lexical.hs: `resolveTerm` references TypeContext → Graph
   - Adapt/Simple.hs: `graphToTypeContext` call → remove

- [x] Change all ~28 checking functions from `TypeContext ->` to `Graph ->`
- [x] Replace `typeContextTypes` reads with `graphBoundTypes`
- [x] Replace `typeContextMetadata` reads with `graphMetadata`
- [x] Replace `typeContextTypeVariables` reads with `graphTypeVariables`
- [x] Replace `typeContextLambdaVariables` reads with `graphLambdaVariables`
- [x] Replace `typeContextLetVariables` reads with derived expression
- [x] Update CoderUtils.hs `isComplexVariable`
- [x] Update Hoisting.hs to derive let-bound variables
- [x] Update Reduction.hs TypeContext → Graph
- [x] Delete `extendTypeContextForLambda/Let/TypeLambda` (use Graph versions)
- [x] Delete `initialTypeContext`, `graphToTypeContext`, `graphToInferenceContext`
- [x] Add `graphWithBoundTerms`, `graphWithLambdaVariables`, `graphWithMetadata`, `graphWithTypeVariables`, `graphPrimitiveTypes` helpers
- [x] Update test files (TestUtils.hs, TestSuiteSpec.hs)
- [x] Gate: build, sync, test (4396 tests pass)

##### Phase A4.3: Remove InferenceContext and TypeContext types

- [x] Remove `InferenceContext` from `hydra.typing` (Sources/Kernel/Types/Typing.hs)
- [x] Remove `TypeContext` from `hydra.typing` (Sources/Kernel/Types/Typing.hs)
- [x] Remove all related DSL helpers from `Dsl/Meta/Typing.hs` (~150 lines of constructors, accessors, withX helpers)
- [x] Remove InferenceContext/TypeContext from gen-main files (Typing.hs, Encode/Decode/Typing.hs, Sources/Encode/Decode/Typing.hs)
- [x] Gate: build, sync (all 8 steps), 4396 tests pass

#### Phase A5: Remove Graph, rename Graf to Graph

**Goal:** Graf becomes the sole type. The old Graph type is retired. All `Flow Graph` becomes
`Flow Graf`, all `Graph ->` params become `Graf ->`, `Coder Graph Graph` becomes `Coder Graf Graf`.
Then Graf is renamed to Graph.

**Key constraint:** `Flow Graph` → `Flow Graf` must be done atomically across ALL Sources files
because the DSL's phantom type `<<~` operator requires state type unification within monadic chains.

**Key design principle:** Graf is purely a lexical environment — it does NOT get `elements`,
`body`, `environment`, or `schema` fields. Those belong to Graph's structural "let" view.
Functions that need those fields must get them differently:
- `graphElements` for iteration → receive `[Binding]` from Module or explicit parameter
- `graphAsLet` / `graphAsTerm` → take `Let` or `([Binding], Term)` directly
- `graphBody` → take body as explicit parameter or from Let
- `graphEnvironment` → already captured in Graf's `lambdaVariables` + `boundTerms`
- `graphSchema` → already captured in `grafSchemaTypes`

##### Step A5.1: Refactor structural Graph users to not need Graph ✅ DONE

Functions that access Graph structurally (elements, body, schema) were changed to receive
that data differently BEFORE the `Flow Graph` → `Flow Graf` switch.

- [x] Haskell/Coder.hs: `nameDecls`, `unpackForallType` → take Graf instead of Graph.
  Commit c9c772669.
- [x] Haskell/Utils.hs: `unionFieldReference` → take `S.Set Name` instead of `[Binding]`.
  `unionCons` similarly refactored. Commit c9c772669 and 0b75ceeac.
- [x] Show/Graph.hs: `graph` function → take Graf instead of Graph. Commit 840396edb.
- [x] Schemas.hs: `graphAsLet`, `graphAsTerm` → take Graf instead of Graph. Commit 840396edb.
- [x] `withSchemaContext` eliminated: `requireType` and `resolveType` use `grafSchemaTypes`
  directly. Commit 596813c51.
- [x] Remaining structural Graph users (elementsToGraph, extendGraphWithBindings, emptyGraph,
  inferGraphTypes, modulesToGraph, adaptDataGraph, etc.) kept as-is — they construct Graph
  values for the transitional bridge. Will be fully eliminated in A5.3.
- [x] **Gate:** build, test. Committed.

##### Step A5.2: Atomic Flow Graph → Flow Graf switch ✅ DONE

- [x] `Flow Graph` → `Flow Graf` in all ~146 TBinding type signatures across all Sources files.
  Commit c476c4881.
- [x] `Coder Graph Graph` → `Coder Graf Graf`
- [x] `SymmetricAdapter Graph` → `SymmetricAdapter Graf`
- [x] `AdapterContext.graph` field: changed type from Graph to Graf (in Types/Coders.hs)
- [x] Removed `Graph.graphTransitionalGraf` wrappers where getState returns Graf directly
- [x] `Monads.withState @@ someGraph` → `Monads.withState @@ (Graph.graphTransitionalGraf someGraph)`
  where `someGraph :: Graph`
- [x] `withEmptyGraph` → `withEmptyGraf` (added `emptyGraf` to gen-main Lexical.hs)
- [x] Patched gen-main, hand-written, and test files to match. Commit 0b75ceeac.
- [x] **Gate:** build passes, all 4396 tests pass. Committed.

##### Step A5.3: Delete old Graph type ✅ DONE

- [x] Remove `Graph` type definition, `graph` constructor, `graphWith*` helpers, all
  Graph-related accessors from Sources and DSL
- [x] Remove `graphTransitionalGraf` field, `rebuildGraf`, `buildGrafRef`
- [x] Remove from gen-main
- [x] **Gate:** build, test. Committed.

##### Step A5.4: Rename Graf → Graph ✅ DONE

- [x] Simple search-and-replace: `Graf` → `Graph`, `graf` → `graph` throughout all Sources,
  gen-main, and DSL files
- [x] Run `bin/sync-haskell.sh` to regenerate
- [x] Updated all language targets: Java, Python manually-created files
- [x] Fixed annotations generation test: `testState = unit` → `metaref Lexical.emptyGraph`
  (Java code generator correctly detected schema type mismatch)
- [x] Full sync-all passes (Haskell, Java, Python generation); 4396 Haskell tests pass
- [x] **Gate:** build, sync-all, test. Committed.

---

### Epoch B: Flow → Either (deferred)

Epoch B begins after Epoch A is complete and the new Graph type is stable across all targets.
The `hydra.context` and `hydra.error` modules are created in this epoch.

#### Phase B1: New types and modules

- [ ] Define `Context` type in new `hydra.context` module:
  - `trace: Trace` — execution context for debugging
  - (Possibly additional execution settings)
- [ ] Define `Error` type in new `hydra.error` module:
  - `message: String`
  - `trace: Trace` — trace at point of failure
- [ ] Move `Trace` from `hydra.compute` to `hydra.context`
- [ ] Move `Coder`, `Bicoder`, `Adapter` from `hydra.compute` to `hydra.coders`

#### Phase B2: Define new DSL operators alongside existing ones

`<<~` remains available for `Flow`-based code throughout the migration. A new temporary operator
(e.g., `<<=` or similar — name TBD) is introduced for `Either Error` binding. Migrated modules
use the new operator; unmigrated modules continue using `<<~`. Once all modules are migrated,
the temporary operator can be renamed to `<<~` and the old `Flow`-based `<<~` deleted.

- [ ] Define temporary `Either Error` bind operator (name TBD) alongside `<<~`
- [ ] Add `Either`-based equivalents for `Flows.mapList`, `Flows.mapElems`, `Flows.mapMaybe`
- [ ] Add `withTrace` helper: `Context -> String -> Context` (pushes a trace frame)
- [ ] Add failure helper: `Context -> String -> Either Error a` (captures trace into Error)

#### Phase B3: Migrate kernel modules (easy tier)

Modules with zero state interaction — pure error propagation, phantom `Flow s`:

- [ ] Extract/Core.hs (55 `<<~`, 0 getState) — establish the pattern
- [ ] Reduction.hs (27 `<<~`, 0 getState) — validate on hot path
- [ ] Checking.hs (38 `<<~`, 0 getState)
- [ ] Inference.hs (59 `<<~`, 1 getState)
- [ ] Adapt/Simple.hs (38 `<<~`, 0 getState)
- [ ] Rewriting.hs (84 `<<~`, 0 getState) — high volume, mechanical
- [ ] Annotations.hs (9 `<<~`, 2 getState)

Signature pattern: `Context -> Graph -> x -> Either Error y`
(or `Context -> x -> Either Error y` for functions that don't need Graph)

#### Phase B4: Migrate kernel modules (medium tier)

Modules with `withState` / heavy `getState` — graph context switching:

- [ ] Lexical.hs (8 `<<~`, 5 getState, 2 withState) — `withSchemaContext` becomes explicit
  schema graph argument
- [ ] Schemas.hs (16 `<<~`, 8 getState, 1 withState)
- [ ] CodeGeneration.hs (9 `<<~`, 1 getState, 4 withState)

#### Phase B5: Migrate kernel modules (hard tier)

- [ ] Adapt/Terms.hs (70 `<<~`, 4 getState, **2 putState**) — only module with mutable state.
  Functions return `(AdapterContext, Either Error y)` for explicit state threading.

#### Phase B6: Migrate ext coders

- [ ] Java/Coder.hs (215 `<<~`, 7 getState)
- [ ] Python/Coder.hs (116 `<<~`, 5 getState)
- [ ] Other ext coders as needed

#### Phase B7: Cleanup

- [ ] Delete `hydra.compute` module (Flow, FlowState)
- [ ] Delete `hydra.monads` module
- [ ] Remove all Flow-related DSL infrastructure
- [ ] Regenerate all targets, full test suite
