# Hydra-TypeScript

A TypeScript implementation of the [Hydra](https://github.com/CategoricalData/hydra) type
system and kernel.

**Status:** in progress — the TypeScript kernel passes the full common test suite at
runtime (2570 of 2577 tests passing, 7 skipped as `disabled` upstream, 0 failing), but
the head is still a *head bud*: the TypeScript coder lives under `packages/hydra-typescript`
and emits valid TypeScript, but TypeScript itself is not yet a sync host. See issue
[#126](https://github.com/CategoricalData/hydra/issues/126) for the tracking effort.

Hydra is a type-aware data transformation toolkit which aims to be highly flexible and
portable. It has its roots in graph databases and type theory, and provides APIs in
Haskell, Java, Python, Scala, and Lisp. See the main Hydra
[README](https://github.com/CategoricalData/hydra) for more details.

## Layout

This package contains the TypeScript coder DSL sources
(`src/main/haskell/Hydra/Sources/TypeScript/`), which describe the TypeScript syntax model
(`Syntax.hs`), language constraints (`Language.hs`), reserved-word operators
(`Operators.hs`), serializer (`Serde.hs`), and the kernel-to-TypeScript coder itself
(`Coder.hs`).

The TypeScript runtime — hand-written `lib/*`, primitive registry, test runner — lives at
[heads/typescript/](../../heads/typescript/). The runtime is copied alongside the generated
kernel by `bin/sync-typescript.sh` so the published kernel can be consumed standalone.

## Getting started

Hydra-TypeScript requires:

- Node.js >= 20 (LTS)
- npm (pnpm and yarn are not assumed)
- TypeScript 5.x (installed via the package's own `package.json`)

To regenerate the TypeScript kernel from the Hydra DSL sources:

```bash
bin/sync-typescript.sh
```

This runs three phases:

1. Regenerate `hydra-typescript`'s own DSL sources into Haskell.
2. Generate the `hydra-kernel` TypeScript output into `dist/typescript/`.
3. Copy the hand-written TS runtime (`heads/typescript/src/main/typescript/`)
   alongside the generated kernel so `./lib/...` imports resolve.

To run the common test suite (vitest):

```bash
cd heads/typescript
npx vitest run
```

To check the generated distribution compiles under `tsc --strict`:

```bash
heads/typescript/bin/test-distribution.sh hydra-kernel
```

## Design notes

### Type representation

Hydra types map onto TypeScript-native constructs:

- **Union types** (Term, Type, Literal, etc.) — `readonly` discriminated unions
  with a `tag` literal-string field and a `value` field
- **Record types** (Application, Lambda, Field, etc.) — `readonly interface` declarations
- **Enum types** (IntegerType, FloatType) — string-literal union types
  (`"int8" | "int16" | …`)
- **Newtypes** (Name) — wrapper interfaces tagged via `readonly _tag` plus a runtime helper
- **Type parameters** — TypeScript generics, with `extends` clauses where Hydra `forall`
  carries a constraint
- **Immutability** enforced at the type level (`readonly`, `as const`,
  `ReadonlyMap`, `ReadonlySet`); runtime `Object.freeze` is avoided in hot paths

### Maybe and Either

- `Maybe<T>` — `{ readonly tag: "just", readonly value: T } | { readonly tag: "nothing" }`
- `Either<L, R>` —
  `{ readonly tag: "left", readonly value: L } | { readonly tag: "right", readonly value: R }`

### Immutable collections

- Lists — `readonly T[]`
- Maps — `ReadonlyMap<K, V>` backed by a value-equality `CanonMap` wrapper so wrapped
  keys (e.g. `Name = { value: "..." }`) compare structurally
- Sets — `ReadonlySet<T>`, similarly canonicalized

### Error model

Hydra-TypeScript uses `Either<Error, T>` for computations that can fail, matching the
current Hydra kernel (the Flow monad was removed in #245).

### Binary literals

Binary literal values are stored as base64-encoded JS strings at the Term level (the
coder emits `value: "<base64>"`, the runtime decoder accepts either `Uint8Array` or
base64 strings). `hydra.lib.literals.binaryToString` is the identity for string inputs;
`stringToBinary` is also the identity, preserving round-trip without redundant encoding.

## Future enhancements

- **Generation-side test runner.** The runtime test runner (kernel evaluation) is in
  place; a separate test runner for generated-code evaluation is still TODO. See
  [docs/test-suite-architecture.md](../../docs/test-suite-architecture.md) for the
  pattern.
- **Bootstrap demo.** TypeScript is not yet wired into the cross-host bootstrapping
  demo (`bin/run-bootstrapping-demo.sh`).
- **Parameter type annotations.** The coder currently emits untyped lambda parameters
  in some positions; tightening this to emit fully-typed parameters is tracked in #126.
