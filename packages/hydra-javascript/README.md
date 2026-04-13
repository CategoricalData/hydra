# Hydra-JavaScript

A JavaScript implementation of the [Hydra](https://github.com/CategoricalData/hydra) type system and kernel.

**Status:** early, in progress. The JavaScript target is not yet passing the common test
suite and is not part of the default sync pipeline. See
[`implementation-plan-and-status.md`](implementation-plan-and-status.md) for progress tracking.

Hydra-JavaScript will eventually provide the same core functionality as Hydra-Haskell,
Hydra-Java, Hydra-Python, Hydra-Scala, and Hydra-Lisp: a runtime for Hydra's type system
(System F with Hindley-Milner inference), bidirectional Coders for type-aware
transformations, and the full standard library of primitive functions.

## Layout

This package contains the JavaScript coder DSL sources
(`src/main/haskell/Hydra/Sources/JavaScript/`), which describe the JavaScript syntax
model and language constraints used by the JavaScript code generator. A JavaScript
runtime (primitives, DSL, tests) is planned but not yet in `heads/javascript/`.

## Design notes

### Type representation

JavaScript lacks native algebraic data types, so Hydra types will be represented using
a **tagged object pattern**:

- **Union types** (Term, Type, Literal, etc.) — objects with a `tag` string field and a `value` field
- **Record types** (Application, Lambda, Field, etc.) — plain frozen objects with named fields
- **Enum types** (IntegerType, FloatType) — string constants
- **Newtypes** (Name) — a wrapper class
- **Immutability** enforced via `Object.freeze()` on all constructed values

### Maybe and Either

- `Maybe<T>` — `{ tag: "just", value: T }` or `{ tag: "nothing" }`
- `Either<L, R>` — `{ tag: "left", value: L }` or `{ tag: "right", value: R }`

### Immutable collections

- Lists — frozen `Array` (via `Object.freeze()`)
- Maps — a `FrozenMap` wrapper around `Map`
- Sets — frozen `Set`

### Error model

Hydra-JavaScript will use `Either<Error, T>` for computations that can fail, matching
the current Hydra kernel (the Flow monad was removed in #245).

## Prerequisites

- Node.js >= 18
- npm or yarn
