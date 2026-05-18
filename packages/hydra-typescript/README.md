# Hydra-TypeScript

A TypeScript implementation of the [Hydra](https://github.com/CategoricalData/hydra) type system and kernel.

**Status:** early, in progress. The TypeScript target is not yet passing the common test
suite and is not part of the default sync pipeline. See issue
[#126](https://github.com/CategoricalData/hydra/issues/126) for the implementation effort.

Hydra-TypeScript will eventually provide the same core functionality as Hydra-Haskell,
Hydra-Java, Hydra-Python, Hydra-Scala, and Hydra-Lisp: a runtime for Hydra's type system
(System F with Hindley-Milner inference), bidirectional Coders for type-aware
transformations, and the full standard library of primitive functions.

## Layout

This package contains the TypeScript coder DSL sources
(`src/main/haskell/Hydra/Sources/TypeScript/`), which describe the TypeScript syntax
model and language constraints used by the TypeScript code generator. A TypeScript
runtime (primitives, DSL, tests) is planned but not yet in `heads/typescript/`.

## Design notes

### Type representation

Hydra types map onto TypeScript-native constructs:

- **Union types** (Term, Type, Literal, etc.) — `readonly` discriminated unions
  with a `tag` literal-string field and a `value` field
- **Record types** (Application, Lambda, Field, etc.) — `readonly interface` declarations
- **Enum types** (IntegerType, FloatType) — string-literal union types (`"int8" | "int16" | …`)
- **Newtypes** (Name) — wrapper interfaces tagged via `readonly _tag` plus a runtime helper
- **Type parameters** — TypeScript generics, with `extends` clauses where Hydra `forall`
  carries a constraint
- **Immutability** enforced at the type level (`readonly`, `as const`,
  `ReadonlyMap`, `ReadonlySet`); runtime `Object.freeze` is avoided in hot paths

### Maybe and Either

- `Maybe<T>` — `{ readonly tag: "just", readonly value: T } | { readonly tag: "nothing" }`
- `Either<L, R>` — `{ readonly tag: "left", readonly value: L } | { readonly tag: "right", readonly value: R }`

### Immutable collections

- Lists — `readonly T[]`
- Maps — `ReadonlyMap<K, V>` (potentially wrapped in a `FrozenMap` when structural sharing matters)
- Sets — `ReadonlySet<T>`

### Error model

Hydra-TypeScript uses `Either<Error, T>` for computations that can fail, matching
the current Hydra kernel (the Flow monad was removed in #245).

## Prerequisites

- Node.js >= 20 (LTS)
- npm (pnpm and yarn are not assumed)
- TypeScript 5.x (installed via the package's own `package.json`)
