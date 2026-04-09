# Hydra-Rust Implementation Plan and Status

## Overview

This document tracks the progress of the Hydra-Rust implementation, following the 10-step
process outlined in the [new implementation recipe](../docs/recipes/new-implementation.md).

## Design Decisions

### Type Mapping Strategy

Rust is well-suited for Hydra because it has:
- **Enums with data**: Perfect for Hydra's union types (Term, Type, Function, etc.)
- **Structs**: Perfect for Hydra's record types
- **Pattern matching**: `match` expressions for case statements
- **Strong type system**: Generics, traits, lifetimes
- **No garbage collector**: Requires explicit ownership/borrowing decisions

Key design decisions:

1. **Recursive types use `Box<T>`**: Since `Term` and `Type` are recursive, variants that
   contain them use `Box<Term>` / `Box<Type>` to allow fixed-size enum representations.

2. **`BTreeMap`/`BTreeSet` over `HashMap`/`HashSet`**: Hydra's types derive `Ord`, and
   `Term` is used as map keys. `BTreeMap` requires `Ord` (which we can derive), whereas
   `HashMap` requires `Hash` (harder with recursive floating-point types). This matches
   Haskell's `Data.Map` (which is also a balanced tree).

3. **`Either<L, R>` custom enum**: Rust doesn't have a built-in `Either`. We define our own
   rather than using `Result` because `Result` has different semantics (error handling).

4. **`Flow<S, V>` as a struct wrapping a closure**: Mirrors the Haskell `Flow s v = Flow (s -> Trace -> FlowState s v)`.
   Uses `Box<dyn Fn>` for the closure since Rust closures are unsized.

5. **`Name` as a newtype**: `Name(String)` with `Deref` to `str` for ergonomics.

6. **`bigint` maps to `num::BigInt`**: Using the `num` crate for arbitrary-precision integers.

7. **`bigfloat` maps to `f64`**: For simplicity. Could add `BigDecimal` behind a feature flag.

8. **Clone-heavy approach**: Hydra terms are immutable values. In Rust, we use `Clone` liberally
   rather than complex lifetime annotations. This matches the semantics of other implementations.
   We can optimize with `Rc`/`Arc` later if needed.

### Module Organization

Following the pattern of hydra-haskell, hydra-java, and hydra-python:

```
src/
├── main/rust/       # Hand-written source code
│   ├── lib.rs       # Crate root, re-exports
│   ├── core/mod.rs  # Core types (will be generated)
│   ├── graph/mod.rs # Graph, Primitive
│   ├── compute/mod.rs # Flow monad, Coder, Adapter
│   ├── module/mod.rs  # Module system
│   ├── syntax.rs    # Rust AST for code generation
│   ├── lib/         # Primitive implementations (hand-written)
│   │   ├── mod.rs
│   │   ├── chars.rs
│   │   ├── equality.rs
│   │   ├── eithers.rs
│   │   ├── flows.rs
│   │   ├── lists.rs
│   │   ├── literals.rs
│   │   ├── logic.rs
│   │   ├── maps.rs
│   │   ├── math.rs
│   │   ├── maybes.rs
│   │   ├── pairs.rs
│   │   ├── sets.rs
│   │   └── strings.rs
│   └── dsl/         # DSL layer (hand-written)
│       ├── mod.rs
│       ├── terms.rs   # Term construction
│       ├── types.rs   # Type construction
│       └── expect.rs  # Term value extraction
├── gen-main/rust/   # Generated kernel code (from hydra-ext)
├── gen-test/rust/   # Generated test code
└── test/rust/       # Hand-written test runner
```

## 10-Step Implementation Progress

### Step 1: Create the Syntax Model
**Status**: Not started (requires Haskell changes)
- Define Rust syntax using Hydra Core DSL in Haskell
- Would go in `hydra-ext/src/main/haskell/Hydra/Ext/Sources/Rust/Syntax.hs`
- **Blocked**: User requested no Haskell changes at this time

### Step 2: Define Language Constraints
**Status**: Not started (requires Haskell changes)
- Characterize Rust's type system capabilities
- Would go in `hydra-ext/src/main/haskell/Hydra/Ext/Sources/Rust/Language.hs`
- **Blocked**: User requested no Haskell changes at this time

**Planned constraints**:
- Supported elimination variants: record (projection), union (match), wrap
- Supported literal variants: all (binary, boolean, float, integer, string)
- Supported float types: Float32, Float64 (Bigfloat mapped to f64)
- Supported integer types: all 9 variants (i8-i64, u8-u64, BigInt)
- Supported term variants: all
- Supported type variants: all
- Supported function variants: elimination, lambda, primitive

### Step 3: Generate Haskell Sources from DSL
**Status**: Not started (requires Steps 1-2)
- **Blocked**: Requires Haskell changes

### Step 4: Create a Coder
**Status**: Not started (requires Haskell changes)
- Would go in `hydra-ext/src/main/haskell/Hydra/Ext/Staging/Rust/Coder.hs`
- **Blocked**: Requires Haskell changes

**Planned approach**:
- Records -> Rust structs with `#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]`
- Unions -> Rust enums with `#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]`
- Functions -> Closures or `Fn` trait objects
- Newtypes -> Newtype structs
- Generics -> Rust generics with trait bounds

### Step 5: Create the Serializer
**Status**: Not started (requires Haskell changes)
- Would go in `hydra-ext/src/main/haskell/Hydra/Ext/Staging/Rust/Serde.hs`
- **Blocked**: Requires Haskell changes

### Step 6: Register and Generate Code
**Status**: Not started (requires Steps 1-5)
- **Blocked**: Requires Haskell changes

### Step 7: Implement Standard Primitives
**Status**: IN PROGRESS
- Can be done without Haskell changes
- Implementing ~180 primitive functions in Rust
- See library status below

### Step 8: Implement Essential Utilities
**Status**: IN PROGRESS
- Flow monad helpers
- Type conversions
- Core DSL

### Step 9: Create Test Runner
**Status**: Not started
- Requires generated test data (Step 6)
- Can create partial test infrastructure now

### Step 10: Create Native DSLs and Build Applications
**Status**: IN PROGRESS (partial)
- Term construction DSL
- Type construction DSL
- Expect (value extraction) DSL

## Library Implementation Status

| Library | Functions | Status | Notes |
|---------|-----------|--------|-------|
| hydra.lib.chars | 6 | DONE | isAlphaNum, isLower, isSpace, isUpper, toLower, toUpper |
| hydra.lib.equality | 9 | DONE | compare, equal, identity, gt, gte, lt, lte, max, min |
| hydra.lib.eithers | 10+ | DONE | bind, bimap, either, fromLeft, fromRight, isLeft, isRight, lefts, rights, partitionEithers |
| hydra.lib.flows | 13 | DONE | apply, bind, fail, foldl, map, mapElems, mapKeys, mapList, mapMaybe, mapSet, pure, sequence, withDefault |
| hydra.lib.lists | 34 | DONE | map, filter, fold, concat, sort, etc. |
| hydra.lib.literals | 43 | DONE | Type conversions, parsing, showing |
| hydra.lib.logic | 4 | DONE | and, or, not, ifElse |
| hydra.lib.maps | 19 | DONE | lookup, insert, keys, toList, etc. |
| hydra.lib.math | 37 | DONE | add, mul, sin, sqrt, abs, etc. |
| hydra.lib.maybes | 13 | DONE | fromMaybe, maybe, isJust, etc. |
| hydra.lib.pairs | 4 | DONE | fst, snd, swap, curry, uncurry |
| hydra.lib.sets | 14 | DONE | union, intersection, member, etc. |
| hydra.lib.strings | 13 | DONE | concat, split, length, lines, etc. |

## Component Status

| Component | Status | Notes |
|-----------|--------|-------|
| Core types (Term, Type, etc.) | DONE | Hand-written, matching generated output |
| Graph types | DONE | Graph, Primitive, TermCoder |
| Compute types | DONE | Flow, FlowState, Trace, Coder, Adapter |
| Module types | DONE | Module, Library, Namespace |
| DSL: Terms | DONE | Term construction functions |
| DSL: Types | DONE | Type construction functions |
| DSL: Expect | DONE | Value extraction from terms |
| Primitive libraries | DONE | All 13 libraries |
| Cargo project setup | DONE | Builds and tests pass |
| Rust coder (Haskell) | NOT STARTED | Blocked on Haskell changes |
| Rust syntax model | NOT STARTED | Blocked on Haskell changes |
| Common test suite runner | NOT STARTED | Blocked on code generation |

## Next Steps

1. **When Haskell changes are unblocked**:
   - Create Rust syntax model (Step 1)
   - Define language constraints (Step 2)
   - Build the Rust coder (Step 4)
   - Build the serializer (Step 5)
   - Generate Rust code and validate compilation

2. **Can be done now**:
   - Refine and test primitive implementations
   - Add more comprehensive unit tests
   - Profile and optimize (Box allocations, cloning)
   - Add serde support for JSON serialization
   - Explore `Rc<Term>` for sharing to reduce cloning
