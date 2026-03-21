# Issue #245: Remove the Flow Monad

> **GitHub Issue**: [#245 - Remove the Flow monad](https://github.com/CategoricalData/hydra/issues/245)
>
> **Status**: Open
>
> **Created**: January 8, 2026
>
> **Category**: Architecture / Performance
>
> **Prerequisite**: #192 (Simplify Graph / Typing Context) — completed

## Executive Summary

Eliminate the Flow monad from the Hydra codebase. Flow wraps `s -> Trace -> FlowState s v`, which
creates heavy object/closure overhead in eager languages (Java, Python). The replacement is:

- **`Context`** — an independent, unparameterized type carrying execution trace, passed as an
  explicit argument
- **`Error`** — a structured error type carrying message, trace snapshot, and optional category detail
- **`Either Error a`** — the return type for fallible operations, replacing `Flow s a`

Function signatures change from `a -> Flow s b` to `Context -> s -> a -> Either Error b`.

## Problem Statement

Flow is a specialized State monad with built-in tracing and error handling. While this works well
in Haskell (with lazy evaluation), it creates significant overhead in other languages:

- Each Flow-based step introduces another closure/object requiring runtime management
- The lazy construction-then-application pattern doesn't translate efficiently to eager languages
- Python and Java implementations suffer from excessive object allocation
- Performance limitations emerge as Hydra code becomes increasingly complex

### Current Flow Architecture

```haskell
-- Flow wraps a function: s -> Trace -> FlowState s v
flow :: s -> Trace -> FlowState s v

-- FlowState contains result, final state, and trace
data FlowState s v = FlowState {
  value :: Maybe v,      -- Result or Nothing on failure
  state :: s,            -- Final state
  trace :: Trace         -- Logging and error info
}

-- Trace carries diagnostic information
data Trace = Trace {
  stack :: [String],     -- Context labels
  messages :: [String],  -- Warnings/info
  other :: Map Name Term -- Arbitrary metadata
}
```

### Critical Finding: Graph State is Read-Only

No kernel function calls `putState` on Graph. `putState` is only used for `TarjanState`
(algorithm state) and debug flags. This means the `Flow Graph x` → `Graph -> Either Error x`
migration is clean — no state threading needed for Graph.

### Scope of Impact

~175 Flow-returning functions in the kernel, broken down by state type:

| Category | State type | Count | Mutates state? | Migration |
|----------|-----------|-------|---------------|-----------|
| **A: Graph read-only** | `Flow Graph` | ~30 | No | `Context -> Graph -> x -> Either Error y` |
| **B: Pure + error** | `Flow s` (generic/phantom) | ~130 | No | `Context -> x -> Either Error y` |
| **C: Algorithm state** | `Flow TarjanState` | ~3 | Yes | Keep as-is or localize |

**What Flow currently provides:**
- **Error handling** (~100+ uses): `fail`/`pure` → becomes `Left`/`Right`
- **State reads** (~60 uses): `getState` for Graph lookup → becomes explicit graph argument
- **Tracing** (~40 uses): `withTrace` adds context to error messages → becomes `Context` manipulation
- **State mutation** (~3 uses): only in `Tarjan.hs`, never on Graph

## New Types

### `hydra.context.Context`

An independent, unparameterized type — essentially just `Trace` plus any execution settings.
Passed as a separate argument, decoupled from any domain state.

```
Context:
  trace: Trace          -- execution context for debugging

Trace:
  stack: [String]       -- context labels describing current execution path
  messages: [String]    -- warnings/info messages
  other: Map Name Term  -- arbitrary metadata
```

**Why independent rather than bundled (`Context s`)?**

We considered wrapping state with trace (`Context s = {value: s, trace: Trace}`), but coupling
them has little value beyond saving one argument per function. The downsides of bundling:
- Introduces a type parameter and wrapping/unwrapping overhead
- Trace and domain state have different lifecycles (trace grows monotonically; state varies)
- The combination `Context Graph` collides with "context graph" in the LLM/RAG ecosystem
- Bundling is just argument-count optimization — not worth the conceptual coupling

### `hydra.error.Error`

A structured error with message, trace snapshot at point of failure, and optional category detail.

```
Error:
  message: String                -- human-readable error message
  trace: Trace                   -- trace at point of failure
  detail: Maybe ErrorDetail      -- optional structured detail

ErrorDetail:
  variant: ErrorVariant          -- the error category
  context: Map String String     -- additional key-value context

ErrorVariant:
  | adaptation                   -- type or term adaptation errors
  | checking                     -- type checking errors
  | decoding                     -- term or type decoding errors
  | encoding                     -- term or type encoding errors
  | inference                    -- type inference errors
  | lexical                      -- element, field, or primitive lookup errors
  | reduction                    -- term reduction or evaluation errors
  | schema                       -- schema or type resolution errors
  | unification                  -- type unification errors
  | unexpected                   -- unexpected or internal errors
```

### Function Signature Patterns

```
Context -> Graph -> x -> Either Error y              -- reduction, lexical, codegen
Context -> AdapterContext -> x -> Either Error y      -- adaptation (with state threading)
Context -> x -> Either Error y                       -- error-propagation with tracing
x -> Either Error y                                  -- pure error-propagation (no tracing needed)
```

### How Trace Context Works

```haskell
-- Current DSL:
-- withTrace "in reduce" $ reduce @@ var "term"

-- New DSL:
-- reduce @@ (withTrace "in reduce" (var "cx")) @@ var "g" @@ var "term"
-- where withTrace constructs a new Context with updated trace
```

### How Failure Works

```haskell
-- Current: Flows.fail "no such element: foo"
-- New: Left(Error{message="no such element: foo", trace=cx.trace, detail=Nothing})
-- The <<~ operator captures trace from the current Context into the Error on Left
```

### Mutable State (Where Needed)

For `TarjanState` and `AdapterContext` (the only types with `putState`), functions return
the updated state alongside the result. Functions like `strongConnect` would return
`(TarjanState, Either Error result)`. Callers thread the updated state forward explicitly.

This is more verbose but transparent and natural for Java/Python targets. It also makes data
flow explicit, which aids debugging and reasoning about state.

## DSL Migration

There are **~805 `<<~` occurrences** across kernel + ext sources. These break down as:

- **~30 `getState` binds** — simply deleted; the graph becomes an explicit parameter
- **~775 error-propagation binds** — the dominant pattern; chains fallible operations

The plan is to **redefine `<<~` to bind over `Either Error` instead of `Flow`**. This means the
vast majority of `<<~` call sites barely change — they just gain an explicit `var "cx"` and/or
`var "g"` argument to each called function. The mechanical burden is adding these arguments to
~775 call sites.

**`<<~` usage by file (top contributors):**

| File | `<<~` count | `getState` count | Notes |
|------|------------|-----------------|-------|
| Java/Coder.hs | 215 | 7 | Largest single file |
| Python/Coder.hs | 116 | 5 | |
| Rewriting.hs | 84 | 0 | Pure error propagation |
| Adapt/Terms.hs | 70 | 4 | |
| Inference.hs | 59 | 1 | Phantom state |
| Extract/Core.hs | 55 | 0 | Pure error propagation |
| Checking.hs | 38 | 0 | Phantom state |
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
- `<<~` error chains (~775 uses) — redefined for `Either`; each call gains `var "cx"`

**What needs design work:**
- `Flows.mapList` / `Flows.mapElems` / `Flows.mapMaybe` — need `Either`-based equivalents
  (simpler: just traverse/mapM for Either)
- `withTrace` (~40 uses) — becomes `Context` manipulation (push a trace frame)

**Net: the DSL code will be slightly simpler overall, and the generated code dramatically simpler.**

## Implementation Plan

### Phase 1: Foundation

- [x] Define `Context` type in new `hydra.context` module (flat record: trace, messages, other)
- [x] Define `InContext e` polymorphic type pairing domain objects with context
- [x] Define `DecodingError`, `OtherError`, `Error` (union) in `hydra.error` module
- [x] Define `UnificationError` (record: leftType, rightType, message) in `hydra.error`
- [x] Register modules in `All.hs`, regenerate
- [x] Create DSL helpers in `Dsl/Meta/Context.hs` (context, inContext, withContext, failInContext)
- [x] Create DSL helpers in `Dsl/Meta/Error.hs` (decodingError, otherError, unificationError, etc.)
- [x] Define `Either`-based bind operator `<<=` alongside `<<~` in Phantoms.hs
- [x] Add `Either`-based equivalents: `Eithers.mapList`, `Eithers.bind`, etc. (already existed)
- [ ] Add `withTrace` helper: `Context -> String -> Context` (pushes a trace frame)
- [ ] Add failure helper: `Context -> String -> Either (InContext e) a` (captures trace into error)

### Phase 2: Migrate kernel modules (leaf-first)

Migration proceeds from leaves (modules with no Flow-using dependents) inward.
Each module uses `InContext E` where E is the appropriate error type for that domain.
Consumer modules bridge with `Monads.eitherToFlow_` until they are themselves migrated.

| Order | Module | Error type | Status |
|-------|--------|------------|--------|
| 1 | Templates.hs | DecodingError, OtherError | **Done** |
| 2 | Unification.hs | UnificationError | **Done** |
| 3 | (next leaf TBD) | | |

**Migrated signatures use:** `Context -> x -> Either (InContext E) y`
(or `Context -> Graph -> x -> Either (InContext E) y` when Graph is needed)

**Error type design notes:**
- Error types can have structured fields (not just strings)
- UnificationError: leftType, rightType, message
- Future: InferenceError (union, with UnificationError as a variant)
- Future: DecodingError may gain a Type field
- Use OtherError for one-offs; create specific types for important categories
- Eventually need `hydra.show.error` to serialize all error types to strings

### Phase 3: Migrate kernel modules (medium tier)

Modules with `withState` / heavy `getState` — graph context switching:

| Priority | Module | `<<~` | getState | withState | Difficulty |
|----------|--------|-------|----------|-----------|------------|
| 8 | Lexical.hs | 8 | 5 | 2 | Medium |
| 9 | Schemas.hs | 16 | 8 | 1 | Medium |
| 10 | CodeGeneration.hs | 9 | 1 | 4 | Medium |

`withState` calls become explicit graph argument substitution.

### Phase 4: Migrate kernel modules (hard tier)

| Priority | Module | `<<~` | getState | putState | Difficulty |
|----------|--------|-------|----------|----------|------------|
| 11 | Adapt/Terms.hs | 70 | 4 | **2** | Hardest — only module with putState on AdapterContext |

Functions return `(AdapterContext, Either Error y)` for explicit state threading.

### Phase 5: Migrate ext coders

| Module | `<<~` | getState |
|--------|-------|----------|
| Java/Coder.hs | 215 | 7 |
| Python/Coder.hs | 116 | 5 |
| Other ext coders | varies | varies |

### Phase 6: Update code generators

- Update Haskell code generator to emit `Either Error` instead of `Flow`
- Update Python code generator
- Update Java code generator
- Regenerate all kernel code

### Phase 7: Cleanup

- Delete `hydra.compute` module (Flow, FlowState)
- Delete `hydra.monads` module
- Move `Coder`, `Bicoder`, `Adapter` from `hydra.compute` to `hydra.coders`
- Remove all Flow-related DSL infrastructure
- Remove old `Trace` from `hydra.compute` (now in `hydra.context`)
- Regenerate all targets, full test suite

## Module Reorganization

| Current (`hydra.compute`) | Destination | Notes |
|---------------------------|-------------|-------|
| `Flow` | **deleted** | Replaced by explicit `Context` + domain state + `Either Error` return |
| `FlowState` | **deleted** | No longer needed |
| `Trace` | **`hydra.context`** | Part of execution context |
| `Context` (new) | **`hydra.context`** | Independent, unparameterized |
| `Error` (new) | **`hydra.error`** | Structured error type with trace snapshot |
| `Coder` | **`hydra.coders`** | Already exists; natural home |
| `Bicoder` | **`hydra.coders`** | |
| `Adapter` | **`hydra.coders`** | |

The `hydra.monads` module (which defines `pure`, `bind`, `map`, `fail`, `getState`, `putState`,
`withState`, `withTrace`, etc.) also goes away — replaced by:
- `Either`-based operations (built into the language or trivial helpers)
- `Context` helpers in `hydra.context` (e.g., `withTrace` to push a trace frame)

## Success Criteria

1. All kernel modules use `Either Error` instead of `Flow`
2. Performance improved in Python and Java implementations
3. All tests pass after migration
4. Code generation produces `Either Error`-based code
5. Error messages retain trace context (no regression in diagnostics)
6. Generated Java/Python code is dramatically simpler (no Flow wrappers or state closures)

## Related Issues

- #192 - Simplify Graph / Typing Context (prerequisite — completed)
- #240 - Python performance (Flow is a contributor)
- #244 - Minimize native primitives (overlapping goals)
- #246 - Unify tests (affects Flow-based tests)
- #183 - Missing Flow primitives (becomes obsolete)

## Risk Assessment

**High Risk:**
- Scope is massive (~175 functions, ~805 `<<~` call sites, ext coders)
- Cross-language coordination required
- Regeneration of entire codebase needed

**Mitigation:**
- `<<~` redefinition means most call sites change mechanically (add `var "cx"`)
- Most modules have zero state interaction — straightforward conversions
- Only `Adapt/Terms.hs` has `putState` (2 calls)
- Graph state is read-only — no state threading complications for the majority of code

---

*Initial analysis: 2026-01-13. Updated: 2026-03-01 (detailed plan from #192 Epoch B incorporated,
new type modules created).*
