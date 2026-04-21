# Hydra-TypeScript runtime

Hand-written TypeScript runtime for [Hydra](https://github.com/CategoricalData/hydra).
This directory follows the standard `heads/` convention: it contains everything
needed to *run* generated TypeScript code — primitives, DSL helpers, build
configuration, and tests.

**Status:** early, in progress.
The TypeScript target is not yet passing the common test suite and is not part
of the default sync pipeline.

The Haskell DSL sources that *describe* TypeScript as a target language live in
[`packages/hydra-typescript/`](../../packages/hydra-typescript/).
Generated output lives in [`dist/typescript/`](../../dist/typescript/).

## Prerequisites

- Node.js >= 20 LTS
- pnpm
- TypeScript >= 5.x

## Build and test

```bash
cd heads/typescript
pnpm install
pnpm run build   # tsc
pnpm test        # vitest
```

## Layout

```
heads/typescript/
  package.json              # pnpm package definition
  tsconfig.json             # TypeScript compiler configuration
  vitest.config.ts          # Test runner configuration
  src/main/typescript/hydra/
    core.ts                 # Core Hydra types (Term, Type, etc.)
    compute.ts              # Computation model
    graph.ts                # Graph types
    index.ts                # Package entry point
    libraries.ts            # Standard library primitive registration
    testing.ts              # Test infrastructure types and utilities
    dsl/                    # DSL helpers for constructing terms, types, etc.
      expect.ts             # Term value extractors
      literals.ts           # Literal constructors
      literalTypes.ts       # Literal type constructors
      prims.ts              # Primitive construction utilities (TermCoder, primN)
      terms.ts              # Term constructors
      types.ts              # Type constructors
    lib/                    # Primitive function implementations
      chars.ts              # Character primitives
      eithers.ts            # Either combinators
      equality.ts           # Comparison operators
      lists.ts              # List manipulation
      literals.ts           # Literal conversions
      logic.ts              # Boolean logic
      maps.ts               # Map operations
      math.ts               # Arithmetic and transcendentals
      maybes.ts             # Maybe combinators
      pairs.ts              # Pair transformations
      regex.ts              # Regular expressions
      sets.ts               # Set operations
      strings.ts            # String utilities
    tools/                  # Command-line tools
  src/test/typescript/hydra/
    compute.test.ts         # Computation model tests
    core.test.ts            # Core type tests
    dsl.test.ts             # DSL tests
    lib.test.ts             # Primitive library tests
    libraries.test.ts       # Standard library registration tests
    testing.test.ts         # Test infrastructure tests
```

## Design notes

### Type representation

TypeScript maps Hydra types using its native type system:

- **Union types** (Term, Type, Literal, etc.) — discriminated unions with `readonly` tag field
- **Record types** (Application, Lambda, Field, etc.) — `readonly` interfaces
- **Enum types** (IntegerType, FloatType) — string literal unions or `as const` objects
- **Newtypes** (Name) — branded types or nominal wrappers
- **Type parameters** — TypeScript generics

### Maybe and Either

- `Maybe<T>` — `{ readonly tag: "just"; readonly value: T } | { readonly tag: "nothing" }`
- `Either<L, R>` — `{ readonly tag: "left"; readonly value: L } | { readonly tag: "right"; readonly value: R }`

### Immutable collections

- Lists — `readonly` arrays (`ReadonlyArray<T>`)
- Maps — `ReadonlyMap<K, V>`
- Sets — `ReadonlySet<T>`

### Error model

Hydra-TypeScript uses `Either<Error, T>` for computations that can fail,
matching the current Hydra kernel.
