# Plan: Add a TypeScript coder (Issue #126)

## Goal

Add a TypeScript coder to Hydra, making TypeScript the sixth complete implementation
(after Haskell, Java, Python, Scala, and Lisp).
The coder emits `.ts` source files that map Hydra's type system directly onto
TypeScript's type system, with `tsc` providing the route to runnable JavaScript.

Tracks [issue #126](https://github.com/CategoricalData/hydra/issues/126).

## Why TypeScript and not JavaScript

A prior attempt (pre-#126) started a `hydra-javascript` package targeting plain
ECMAScript.
That work was committed but never finished, and the partial plan was left in
`packages/hydra-javascript/implementation-plan-and-status.md` — a misplaced,
nonstandard artifact that this branch removes.

Targeting TypeScript instead of JavaScript is a better fit for Hydra:

- **Hydra is strongly typed end to end.** The kernel is System F + HM, and every
  existing target (Haskell, Java, Scala, Python with `pyright`, even the Lisps
  where possible) preserves type information. A JS-only coder would have to erase
  all of that at the coder boundary. A TS coder maps Hydra types directly:
  records → interfaces, unions → discriminated unions, type parameters → generics,
  `Maybe` → `T | null` or a tagged union, `Either` → discriminated union, etc.
  This is the same shape as the Java and Scala coders.
- **One coder, both outputs.** TypeScript is a superset of modern JavaScript,
  and `tsc` downgrades `.ts` to `.js` cleanly.
  A TS target subsumes a JS target for every runtime we care about
  (Node.js, Deno, Bun, browsers).
- **No known JS-only use case.** No Hydra use case requires forbidding
  TypeScript tooling at build time; runtime environments consume the `.js`
  `tsc` produces.

Conclusion: do not pursue a separate `hydra-javascript` package.
Rename the existing partial work to `hydra-typescript` and rebuild it around a
proper TypeScript coder.

## Decisions

- **Language ID**: `typescript`. Drop the `javascript` language name entirely.
- **Package name**: `hydra-typescript` (renamed from `hydra-javascript`).
- **TypeScript version target**: TypeScript 5.x, `--strict` mode on by default.
  Emit ES2022 with ESM module resolution.
- **Runtime target**: Node.js 20+ LTS for tests and bootstrapping;
  `tsc`-compiled output is runnable anywhere `.js` is.
- **Build/tooling**: `npm` + `tsc` + `vitest` (or `jest`, tbd in Phase 1).
  `pnpm` and `yarn` are not assumed.
- **Type mapping**: prefer TypeScript-native constructs wherever possible.
  Use discriminated unions (`{ tag: "...", value: ... }`) for Hydra unions,
  `readonly` interfaces for records, generics for type parameters, and
  `ReadonlyMap`/`ReadonlySet` for kernel maps and sets.
  Avoid classes unless a class-shaped runtime object earns its keep
  (e.g., `Name` wrappers, `FrozenMap` with structural sharing).
- **Immutability**: use `readonly` and `as const` at the type level;
  at runtime, rely on TypeScript discipline rather than `Object.freeze` on
  every value (too costly, and unnecessary once the type checker enforces it).
- **Numeric types**: `number` for `int8..int32`, `uint8..uint32`, `float32`,
  `float64`; `bigint` for `int64`, `uint64`, `bigint`; `bigfloat` tbd
  (no native arbitrary-precision float in the language — probably emulate via
  `decimal.js` or equivalent, decide in Phase 2).
- **Module system**: ESM only. Generated files use `.ts` extensions with
  explicit `.js` import specifiers (TS/Node ESM convention).
- **Scope**: incremental, following `docs/recipes/new-implementation.md`
  (11 steps in the current version of that guide).

## Starting point: the hydra-javascript package

The `packages/hydra-javascript/` tree in integration was committed prematurely
and contains a mix of salvageable and unsalvageable material.
**All `javascript` artifacts — the package directory, Haskell module paths,
file names, and language ID — must be renamed to `typescript` as the first
concrete step of this branch.** The target layout is `packages/hydra-typescript/`
with `Hydra/Sources/TypeScript/*.hs` on the Haskell side and generated TypeScript
under `src/main/typescript/hydra/` (and eventually `dist/typescript/`).

Inventory (from integration, 35 files):

### Haskell DSL sources

| File | Disposition |
|------|-------------|
| `src/main/haskell/Hydra/Sources/JavaScript/Syntax.hs` | **Rewrite**. TypeScript has a different grammar (type annotations, interfaces, generics, declaration files). Base the new model on the TypeScript language specification (TS 5.x). The old model may be useful as a reference for expression/statement shapes that TS shares with JS, but the type-level syntax has to be redone. |
| `src/main/haskell/Hydra/Sources/JavaScript/Language.hs` | **Rewrite**. JavaScript language constraints erase types; TypeScript's constraints should enable nearly all kernel variants (records, unions, type parameters, foralls), which is closer to the Java or Scala constraint files. |
| `src/main/haskell/Hydra/Sources/JavaScript/Operators.hs` | **Reuse with rename**. Most operators are shared. |
| `src/main/haskell/Hydra/Sources/JavaScript/Serde.hs` | **Rewrite**. Must emit TypeScript type annotations, interface declarations, generics, and import/export with `.js` specifiers. |
| `src/main/haskell/Hydra/Dsl/JavaScript/Helpers.hs` | **Rewrite**. Parallel to `Hydra.Dsl.Java.Helpers` / `Hydra.Dsl.Python.Helpers`. |

No coder file exists yet (no `Coder.hs`). That's step 4, to be written fresh.

### Hand-written runtime JavaScript

All files under `src/main/javascript/hydra/` (core.js, compute.js, graph.js,
index.js, dsl/*, lib/*, tools/*) plus the `src/test/javascript/hydra/*.test.js`
suite. These are the largest block (~25 files).

**Disposition: port to TypeScript.**
The runtime code is not auto-generated — it is the hand-written substrate that
the coder emits against (primitives, `Flow` monad, `Graph` registry, DSL
constructors). Porting means:

1. Rename `.js` → `.ts`.
2. Add proper type annotations against the new TypeScript kernel types.
3. Replace the tagged-object-plus-`Object.freeze` idiom with `readonly`
   discriminated unions where the type checker can do the work.
4. Drop JSDoc stand-ins.
5. Re-test with `tsc --noEmit` + the test runner.

Some files will shrink significantly once TypeScript handles structural
immutability and variance.
`FrozenMap`/`FrozenSet` may be replaceable with `ReadonlyMap`/`ReadonlySet` at
the type level plus a thin runtime wrapper.

### Tests

`core.test.js`, `compute.test.js`, `dsl.test.js`, `lib.test.js`.
**Port and re-type.** The test coverage is useful (241 tests per the old plan),
but the tests currently live outside the generated test-suite framework.
In step 9, the generated test suite from the common test-case JSON should
become the canonical test runner; the existing hand-written tests become
supplementary runtime / library tests.

### Package metadata

| File | Disposition |
|------|-------------|
| `package.json` | **Rewrite**. Rename to `hydra-typescript`, add TypeScript toolchain dependencies, define `tsc` / test scripts. |
| `package-lock.json` | **Delete and regenerate** after dependency changes. |
| `README.md` | **Rewrite** around TypeScript. |

## Current state vs. the 11-step new-implementation guide

Step numbering matches `docs/recipes/new-implementation.md` as of this branch.

| Step | State | Notes |
|------|-------|-------|
| 1. Syntax model | **Not started (for TS)**. JS model exists but is the wrong language. | New `Hydra/Sources/TypeScript/Syntax.hs` against the TypeScript 5.x spec. |
| 2. Language constraints | **Not started (for TS)**. | Closer to Java/Scala than to the old JS constraints. |
| 3. Generate Haskell sources | Not started | Depends on steps 1–2. |
| 4. Coder | **Not started**. No `Coder.hs` exists. | This is the biggest single piece of work. |
| 5. Serializer | **Not started**. Old `Serde.hs` exists but targets JS. | Rewrite for TS. |
| 6. Registration + generation | Not started | Register `writeTypeScript` in `heads/haskell/src/main/haskell/Hydra/Generation.hs`, add `bin/sync-typescript.sh`. |
| 7. Standard primitives | **Partial, in JS**. ~200 functions across 13 lib modules exist in `.js`. | Port to `.ts`, verify against the kernel once the coder runs. |
| 8. Runtime foundation types | **Partial, in JS**. Flow, FlowState, Trace, Coder, Adapter, Bicoder, Graph, Primitive, TermCoder in `.js`. | Port to `.ts`. |
| 9. Test runners | **Missing**. Hand-written Jest tests exist; no generated-test-suite runner. | Wire up `src/gen-test/typescript/hydra/` once the coder emits it. |
| 10. Native DSLs | **Partial, in JS**. `dsl/{types,terms,expect,literals,literalTypes}.js`. | Port to `.ts`. |
| 11. Bootstrapping | **Missing**. | Stretch goal; follow the Lisp/Python pattern. |

## Plan

### Phase 0: Rename javascript → typescript

- [ ] 0a. `git mv packages/hydra-javascript packages/hydra-typescript`
- [ ] 0b. Rename `Hydra/Sources/JavaScript/*` → `Hydra/Sources/TypeScript/*`
      and `Hydra/Dsl/JavaScript/*` → `Hydra/Dsl/TypeScript/*` (module names and paths)
- [ ] 0c. Grep for `javascript`, `JavaScript`, `javaScript`, `js` (careful
      with the last one — scope to extensions and identifiers) throughout
      the package and rename. The language ID in constraints becomes `typescript`.
- [ ] 0d. Verify `stack build` still succeeds in `heads/haskell` with the
      renamed package. The package has no downstream consumers yet, so this
      should be local.

### Phase 1: Package skeleton and tooling

- [ ] 1a. Rewrite `package.json` with `typescript@^5`, a test framework
      (decision: vitest vs jest, default vitest), `tsx` or similar for dev runs.
- [ ] 1b. Add `tsconfig.json`: strict mode, ES2022 target, ESM, `moduleResolution`
      `nodenext`, `declaration: true`.
- [ ] 1c. Add `bin/sync-typescript.sh` skeleton (parallels `sync-python.sh`).
- [ ] 1d. Port the hand-written runtime (`core.js`, `compute.js`, `graph.js`,
      `index.js`) to typed `.ts` files. This establishes the types the coder
      will emit against.
- [ ] 1e. Port the DSL modules (`dsl/*.js`) to `.ts`.
- [ ] 1f. Port the library modules (`lib/*.js`) to `.ts`.
- [ ] 1g. Port the existing hand-written test suite to `.ts`, run it under
      the chosen runner. Green on `tsc --noEmit` + tests before moving on.

### Phase 2: Syntax model and language constraints (steps 1–2)

- [ ] 2a. Write `Hydra/Sources/TypeScript/Syntax.hs` against the TS 5.x
      language spec. Record version + URL in the file header per the recipe.
      Useful references: TC39 TypeScript docs, the `typescript-eslint` AST,
      `@typescript/vfs` AST types.
- [ ] 2b. Write `Hydra/Sources/TypeScript/Language.hs` defining supported
      variants, reserved words, primitive type mappings. Model after
      `Python/Language.hs` and `Java/Language.hs`.
- [ ] 2c. Define TS-specific reserved words (TS 5.x keywords list).
- [ ] 2d. Register in `Hydra.Sources.All`.

### Phase 3: Generation pipeline (steps 3–6)

- [ ] 3a. Generate Haskell sources from the syntax model (`writeHaskell`).
- [ ] 3b. Write `Hydra/Staging/TypeScript/Coder.hs` mapping Hydra types
      and terms to the TypeScript AST. Handle: records, unions, type
      parameters, foralls, Maybe, Either, primitives, literals, lambda,
      application, let, match, inject, project.
- [ ] 3c. Write `Hydra/Staging/TypeScript/Serde.hs` rendering the AST to
      concrete TypeScript syntax with correct indentation, semicolons,
      import specifiers (`.js` suffix for ESM), and generic parameters.
- [ ] 3d. Register `writeTypeScript` in `heads/haskell/src/main/haskell/Hydra/Generation.hs`.
- [ ] 3e. Make `bin/sync-typescript.sh` regenerate kernel modules into
      `dist/typescript/` and copy hand-written runtime files as needed.
- [ ] 3f. Iterate until generated `.ts` passes `tsc --strict`.

### Phase 4: Primitives, tests, DSLs (steps 7–10)

- [ ] 4a. Verify that all ported library modules (`lib/*.ts`) are discoverable
      via the primitive registry and match the kernel's primitive signatures.
      Fix gaps.
- [ ] 4b. Generate the test suite into `src/gen-test/typescript/hydra/`.
- [ ] 4c. Write the kernel test runner. Iterate until all kernel tests pass.
- [ ] 4d. Write the generation test runner.
- [ ] 4e. Polish DSL modules (`dsl/types.ts`, `dsl/terms.ts`, etc.) for
      symmetry with the Java/Python DSLs.
- [ ] 4f. Update `packages/hydra-typescript/README.md` with build/test
      instructions and the type-mapping table.

### Phase 5: Documentation and integration

- [ ] 5a. Update `docs/recipes/new-implementation.md` to reference
      `hydra-typescript` as the sixth complete implementation (once it passes).
- [ ] 5b. Update `CLAUDE.md` project structure section to list
      `hydra-typescript/` under `packages/` and `dist/typescript/` under `dist/`.
- [ ] 5c. Add `/sync-typescript()` shorthand to `CLAUDE.md`.
- [ ] 5d. Add `hydra-typescript` to `bin/sync-all.sh`.
- [ ] 5e. Add TypeScript to the bootstrapping demo matrix (stretch: step 11).

## Open questions

1. **Test runner**: vitest (modern, fast, ESM-native) vs jest (broader
   ecosystem, already used by the old plan). Default to vitest unless there's
   a reason to prefer jest.
2. **`bigfloat`**: `decimal.js`, `big.js`, or block the type variant in
   language constraints? Scala/Java use `BigDecimal`; the TS equivalent is
   a userland library. Defer decision to Phase 2.
3. **Flow monad runtime**: represent as a plain async function, a class,
   or a tagged object? The existing JS version uses a function-based
   representation. Revisit once types are in place.
4. **Union encoding**: `{ tag, value }` discriminated union (old JS choice)
   vs `{ kind }` or `{ _case }`. Staying with `tag` is fine; flag for
   consistency review with Java/Python variant naming.
5. **Where does `hydra-javascript` live in git history?** The old partial
   work is not salvageable as a JS coder, but the runtime files have
   significant hand-written content worth porting. Phase 0 preserves
   history via `git mv`; do not delete and recreate.

## Non-goals

- No separate `hydra-javascript` package. `tsc` + ES2022 target is our
  JavaScript story.
- No browser-specific runtime (DOM types, web APIs) in the core package.
  A separate bindings package can add browser glue later if needed.
- No bundler integration (Vite, esbuild, webpack). Consumers bring their own.

## Progress log

**2026-04-14**: Plan created. Worktree `feature_126_typescript` branched from
`integration` at `94e85344f`. Original misplaced plan document copied in from
`packages/hydra-javascript/implementation-plan-and-status.md` and replaced
with this rewrite; the misplaced document is being `git rm`'d from `integration`
as part of the companion cleanup commit.

**2026-04-15**: Major coder overhaul session (gap analysis plan implementation).
Work completed and current status below.

### Completed this session

**Tier 1: TypeScript Coder (Haskell-side) — the critical blocker**

All four sub-items completed:

- **Tier 1a – Import generation**: Rewrote `generateImports` in the bootstrap
  coder (`dist/haskell/hydra-ext/src/main/haskell/Hydra/TypeScript/Coder.hs`)
  to emit proper ES module `import * as Alias from "./relative/path.js";`
  statements. Uses `Analysis.definitionDependencyNamespaces` plus
  `Packaging.moduleTermDependencies` / `moduleTypeDependencies` to gather all
  cross-module references. Computes correct relative paths between any two
  namespace levels. Alias naming: capitalize segments after `hydra.`
  (e.g., `hydra.lib.lists` → `LibLists`, `hydra.decode.core` → `DecodeCore`).

- **Tier 1b – Missing term variants**: Extended `encodeTermExpr` from ~10 to
  all ~20 `Core.Term` constructors. Added: `TermCases` (switch statement with
  case application optimization), `TermEither` (left/right discriminated union
  literals), `TermLet` (IIFE wrapping), `TermMap` (`new Map([...])`),
  `TermSet` (`new Set([...])`), `TermProject` (field accessor lambda),
  `TermUnwrap`/`TermWrap` (identity/passthrough), `TermTypeApplication`/
  `TermTypeLambda` (type erasure). **Unsupported-term count: 27,069 → 0.**

- **Tier 1c – Type improvements**: Added `resolveQualifiedName` for namespace-
  aware name resolution (self-references → local name, cross-references →
  `Alias.LocalName`). Fixed `TypeApplication` chaining (`Coder<t0><t1>` →
  `Coder<t0, t1>`) via `collectTypeArgs` helper. Added `sanitizeTsVar` for
  reserved-word escaping (`var` → `var_`, `import` → `import_`, `let` → `let_`,
  etc.) applied to variable/parameter/binding names but NOT property names
  (TypeScript allows reserved words as property names). Added `sanitizeTsName`
  for prime-character cleanup (`term'` → `term_`). Added `encodeTypeParams`
  for generic type parameters on type definitions. Added `stripTypeAbstractions`
  to peel `TermTypeLambda` wrappers from top-level function definitions.

- **Tier 1d – tsc iteration**: Iterated until **zero TS1xxx (parse/syntax)
  errors** remain. Key fixes: parenthesized object literals after `=>` to
  prevent block-vs-object ambiguity (`({...})` instead of `{...}`), reserved-
  word escaping, type application flattening. Remaining errors are all TS2xxx
  (type errors) and TS7xxx (implicit any) — these are semantic issues caused
  by the Tier 4 integration gap (missing modules, type mismatches between
  hand-written and generated type definitions), not syntax problems.

**Tier 4: Package integration — partial**

- Updated `sync-typescript.sh` with a new step 4 that copies hand-written
  runtime files from `heads/typescript/` into `dist/typescript/hydra-kernel/`
  (lib primitives, compute.ts, libraries.ts, dsl/, tools/, index.ts).
  Only non-conflicting files are copied (generated versions take precedence).
- Added `package.json` and `tsconfig.json` to `dist/typescript/hydra-kernel/`
  for standalone type-checking.
- After copying, import resolution errors (TS2307) drop from 639 to ~0, but
  ~22k type errors remain due to type incompatibility between the hand-written
  bootstrap types and the generated kernel types.

**Other**

- All 55 hand-written tests in `heads/typescript/` continue to pass.
- `stack build` in `heads/haskell/` passes cleanly.
- Generation produces 173 main + 52 test files with zero unsupported terms.

### Not completed — remaining work

**Tier 4 (continued): Type alignment between hand-written and generated code**

The hand-written runtime (`heads/typescript/`) was built against a bootstrap
type system (`core.ts` with generic `Either<X,Y>`, `Maybe<T>`, etc.) that
differs from the generated kernel types (branded `Name`, interface-based
records, discriminated unions without generic parameters). The ~22k type
errors are cascading from this mismatch. Options:
1. Update hand-written runtime to work against generated types (correct long-term)
2. Create adapter shims
3. Relax type-checking temporarily and fix incrementally

**Tier 2: Bootstrap and generation modules**

- `generation.ts` — not started. Needs: `bootstrapSchemaMap()`,
  `bootstrapGraph()`, `loadModulesFromJson()`, `generateSources()`,
  `writeHaskell()`, `writeTypeScript()`, etc.
- `bootstrap.ts` CLI entry point — not started.
- Blocked on Tier 4 completion (generated kernel must be importable).

**Tier 3: Test suite runner**

- `test_suite_runner.ts` — not started. Needs Vitest integration, test graph
  setup, `TestGroup` walking, `UniversalTestCase` execution.
- Blocked on Tier 4 (generated test modules must compile).

**Tier 5: DSL completeness**

- Missing: `annotations.ts`, host utilities, `tools.ts`, meta-DSL.
- Independent of other tiers; lower priority.

**Tier 6: Sync pipeline hardening**

- `sync-typescript.sh` still has `warn ... Continuing...` instead of failing
  on generation errors. Should be changed once the coder produces clean output.

**Tier 7: Bootstrapping demo**

- TypeScript not yet in the bootstrapping demo matrix.
- Requires Tier 2 completion.

### Files modified this session

- `dist/haskell/hydra-ext/src/main/haskell/Hydra/TypeScript/Coder.hs` — major rewrite (bootstrap patch)
- `heads/haskell/bin/sync-typescript.sh` — added hand-written file copy step
- `dist/typescript/hydra-kernel/package.json` — new (for standalone tsc)
- `dist/typescript/hydra-kernel/tsconfig.json` — new (for standalone tsc)

### Recommended next steps

1. **Align hand-written types with generated types** — the biggest remaining
   blocker. Update `heads/typescript/src/main/typescript/hydra/core.ts` to
   re-export from the generated types, or rewrite the lib primitives to work
   against the generated type definitions.
2. **Iterate on type errors** — once types are aligned, the TS2xxx errors
   should collapse rapidly.
3. **Write test suite runner** (Tier 3) — can begin once the generated kernel
   compiles.
4. **Write bootstrap/generation** (Tier 2) — can begin once the kernel is
   importable.