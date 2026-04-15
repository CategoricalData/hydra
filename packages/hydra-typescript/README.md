# Hydra-TypeScript

A TypeScript implementation of the [Hydra](https://github.com/CategoricalData/hydra) type system and kernel.

**Status:** early, in progress. The TypeScript target is not yet passing the common test
suite and is not part of the default sync pipeline.

Hydra-TypeScript will eventually provide the same core functionality as Hydra-Haskell,
Hydra-Java, Hydra-Python, Hydra-Scala, and Hydra-Lisp: a runtime for Hydra's type system
(System F with Hindley-Milner inference), bidirectional Coders for type-aware
transformations, and the full standard library of primitive functions.

## Layout

This package contains the TypeScript coder DSL sources
(`src/main/haskell/Hydra/Sources/TypeScript/`), which describe the TypeScript syntax
model and language constraints used by the TypeScript code generator. A TypeScript
runtime (primitives, DSL, tests) lives under `src/main/typescript/hydra/`.

## Design notes

### Type representation

TypeScript maps Hydra types directly using its native type system:

- **Union types** (Term, Type, Literal, etc.) -- discriminated unions with `readonly` tag field
- **Record types** (Application, Lambda, Field, etc.) -- `readonly` interfaces
- **Enum types** (IntegerType, FloatType) -- string literal unions or `as const` objects
- **Newtypes** (Name) -- branded types or nominal wrappers
- **Type parameters** -- TypeScript generics

### Maybe and Either

- `Maybe<T>` -- `{ readonly tag: "just"; readonly value: T } | { readonly tag: "nothing" }`
- `Either<L, R>` -- `{ readonly tag: "left"; readonly value: L } | { readonly tag: "right"; readonly value: R }`

### Immutable collections

- Lists -- `readonly` arrays (`ReadonlyArray<T>`)
- Maps -- `ReadonlyMap<K, V>`
- Sets -- `ReadonlySet<T>`

### Error model

Hydra-TypeScript uses `Either<Error, T>` for computations that can fail, matching
the current Hydra kernel.

## Prerequisites

- Node.js >= 20 LTS
- npm
- TypeScript >= 5.x
