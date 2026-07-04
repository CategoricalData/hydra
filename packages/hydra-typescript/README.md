# Hydra-TypeScript

A TypeScript implementation of the [Hydra](https://github.com/CategoricalData/hydra) type
system and kernel.

**Status:** TypeScript is a full Hydra implementation.
As a **target**, the kernel passes the full common test suite
(2570 of 2577 tests passing, 7 skipped as `disabled` upstream, 0 failing).
As a **host**, TypeScript drives code generation for all other supported targets
(Haskell, Python, Scala, Clojure, Scheme, Common Lisp, Emacs Lisp, and Java)
from the language-neutral JSON kernel with no heap or stack failures (#462).
**Writing Hydra DSL sources in TypeScript** (the third pillar of host status) is
still deferred: it would require TS analogues of the Hydra DSL meta-builders,
which Scala and Lisp heads also lack today.

Hydra is a type-aware data transformation toolkit which aims to be highly flexible and
portable. It has its roots in graph databases and type theory, and provides APIs in
Haskell, Java, Python, Scala, TypeScript, and Lisp. See the main Hydra
[README](https://github.com/CategoricalData/hydra) for more details.

## Layout

This package contains the TypeScript coder DSL sources
(`src/main/haskell/Hydra/Sources/TypeScript/`), which describe the TypeScript syntax model
(`Syntax.hs`), language constraints (`Language.hs`), reserved-word operators
(`Operators.hs`), serializer (`Serde.hs`), and the kernel-to-TypeScript coder itself
(`Coder.hs`).

The TypeScript runtime — hand-written `lib/*`, primitive registry, test runner — lives at
[heads/typescript/](https://github.com/CategoricalData/hydra/tree/main/heads/typescript). The runtime is copied alongside the generated
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

### Flat-call ABI

The coder emits multi-argument closures and call sites — `f(a, b, c)`
rather than the curried `f(a)(b)(c)` that Hydra's term-level `App`
chains would naively suggest. The peeling lives in
`encodeTermDefinition` and the `_Term_lambda` arm: nested `App(App(f,a),b)`
flattens via `flattenApplication`, and nested `λa.λb.body` peels via
`analyzeFunctionTerm` into a single function declaration with multiple
typed parameters. Inner lambdas peel the same way (matching Python's
`makeUncurriedLambda`).

Consequence: every site that consumes a Hydra function value must
agree on this ABI. The hand-written `heads/typescript/src/main/typescript/hydra/lib/*.ts`
primitives use flat positional signatures; the test runtime's
`testGraph` is flat (`(testTypes, testTerms) => Graph`); HOF primitives
that re-enter the reducer call `reduceTerm(cx, g, true, term)` flat.

### Encoding mismatches that need `as any` casts

Several Hydra term shapes can't be emitted as well-typed TypeScript
without expensive type annotations the AST doesn't yet carry. Each is
wrapped in an `as any` cast at emission time (`tsAsAny` helper, backed
by the `Expression_asExpression` AST node):

- **Term_pair** — `[a, b] as any` so TS infers tuple, not `(A|B)[]`.
- **Term_inject / Term_maybe / Term_either** — discriminated-union
  literals like `{tag:"just", value:x}` need the cast because the
  literal's narrow tag type doesn't unify with the broader nominal
  target (`Term`, `Type`, `Maybe<T>`, …).
- **Term_cases** — the discriminator binds via `(u as any)` so `.value`
  access compiles on unit-shaped variants (`{tag: "lessThan"}`).

### TS-as-host Java generation

TypeScript-as-host generates clean output for all targets including Java (#462).
The generation completes in ~15 minutes with zero stack overflows; the shipped
`demos/bootstrapping/bin/invoke-typescript-host.sh` raises the V8 stack to 56 MB
(`--stack-size=57344`) which is sufficient for the deepest kernel inference paths.

The resulting Java source set does not yet compile cleanly due to a separate
codegen correctness gap: the TS Java coder does not yet emit aggregate namespace
interface files (`hydra.lib.Maps`, `hydra.lib.Strings`, etc.) that the individual
primitive stub classes reference (#499). This is a coder bug, not a stack or heap
limitation.

### TS-as-host Lisp-dialect slowness

The four Lisp targets (Clojure, Scheme, Common Lisp, Emacs Lisp) emit
correctly from TS-host but take ~19 minutes per dialect for kernel-only
codegen (vs ~1–4 minutes for the non-Lisp targets). Profiling shows
99.8% of the time is spent inside `moduleToLisp` itself, **not** in the
post-processing chain (`programToExpr` / `parenthesize` / `printExpr`,
which take <0.01% combined). The Lisp coder is structurally heavier
than the other coders in ways that interact poorly with the
PersistentMap operations underlying TS-host's runtime — same family of
constant-factor issue tracked by [#362].

The output is correct: this is purely a performance cliff.

### Function-type rendering

`Type_function A → B` renders as `((...args: any[]) => any)`, not
`(a: A) => B`. Hydra's curried function types don't match the flat-call
ABI (`(a, b) => c`), so a curried rendering would reject every closure
at every callsite. Type variables (`t0` → `T0`) are also substituted to
`any` in inline annotations because nested helper functions have no
generic-binder syntax to introduce them.

### Why `runtime.ts`, not `core.ts`

The hand-written runtime lives at
`heads/typescript/src/main/typescript/hydra/runtime.ts`. It used to be
named `core.ts`, but `heads/typescript/bin/copy-kernel-runtime.sh` copies it into the
same dist directory as the GENERATED kernel `core.ts` — and the
hand-written file silently overwrote the generated module, masking
every kernel type export (`Term`, `Type`, `Literal`, …) at type-check
time. Future hand-written runtime files should pick names that cannot
collide with kernel-namespace modules. See
`claude/pitfalls.md` ("Hand-written runtime files clobber generated
kernel modules").

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

- **TypeScript-as-host.** Today TypeScript is a target only. Adding TS-as-host
  requires meta-level DSL builders (`hydra/dsl/*.ts` analogues of
  `Hydra.Dsl.Meta.*`) plus a TS source-import path equivalent to
  `transform-haskell-dsl-to-json`. The Scala and Lisp heads also lack this today;
  not a blocker for parity with them.
- **Primitive coverage parity.** A handful of inference edge cases tagged
  `disabled` upstream (let-polymorphism corner cases, Y-combinator typing) remain
  open across all heads — they would require kernel changes, not TS-specific work.
