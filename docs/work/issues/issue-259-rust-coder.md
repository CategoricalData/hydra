# Issue #259: Create a Rust Coder

> **GitHub Issue**: [#259 - Create a Rust coder](https://github.com/CategoricalData/hydra/issues/259)
>
> **Status**: Open (in progress)
>
> **Created**: February 27, 2026
>
> **Category**: Code Generation

## Executive Summary

Implement a Rust code generator for Hydra. The coder is written as Hydra DSL source (not native Haskell in Staging) and generates Rust source files from Hydra type and term modules. Type-level generation is complete; term-level support is planned.

This is the first Hydra coder primarily created by an LLM, with domain expertise but limited Rust-specific guidance.

## Current State (2026-02-27)

### What exists

**DSL source module**: `hydra-ext/src/main/haskell/Hydra/Ext/Sources/Rust/Coder.hs` (387 lines)
- Registered in `hydra-ext/src/main/haskell/Hydra/Ext/Sources/All.hs` (`rustModules` list)
- Generated Haskell at `hydra-ext/src/gen-main/haskell/Hydra/Ext/Rust/Coder.hs`
- `writeRust` helper registered in `hydra-ext/src/main/haskell/Hydra/Ext/Generation.hs`
- Staged copy at `hydra-rust/staged-Coder.hs`

**Generated output**: 22 Rust files in `hydra-rust/src/gen-main/rust/hydra/` covering all `kernelTypesModules`:
accessors, ast, classes, coders, compute, constraints, core, grammar, graph, json/model, module, parsing, phantoms, query, relational, tabular, testing, topology, typing, util, variants, workflow.

**writeRust flags**: `generateSources moduleToRust rustLanguage True False False False`
- doInfer=True, doExpand=False, doHoistCaseStatements=False, doHoistPolymorphicLetBindings=False

### Type encoding (complete)

The coder defines 12 TBindings:

| Binding | Purpose |
|---------|---------|
| `standardDerives` | `[Clone, Debug, PartialEq, Eq, PartialOrd, Ord]` |
| `rustPath` | Simple path type: `String -> R.Type` |
| `rustPathSegmented` | Multi-segment path: `[String] -> R.Type` |
| `rustApply1` | One-arg generic: `Vec<T>`, `Option<T>` |
| `rustApply2` | Two-arg generic: `BTreeMap<K,V>` |
| `rustUnit` | The `()` type |
| `encodeLiteralType` | Hydra literal types to Rust types |
| `encodeType` | Full type encoding with all variants |
| `encodeStructField` | Record field → struct field |
| `encodeEnumVariant` | Union field → enum variant (unit/tuple/struct) |
| `encodeTypeDefinition` | TypeDefinition → Rust Item |
| `moduleToRust` | Module entry point |

**Type mappings**:
- Records → `pub struct` with named fields
- Unions → `pub enum` with unit, tuple, or struct variants
- Wrapped types → tuple structs (newtypes)
- Literals → `bool`, `String`, `i8`–`i64`, `u8`–`u64`, `f32`, `f64`
- Containers → `Vec<T>`, `BTreeSet<T>`, `BTreeMap<K,V>`, `Option<T>`
- Pairs → tuple types `(A, B)`
- Either → `Either<L, R>` (custom type, not in std)
- Functions → `Box<dyn Fn(A) -> B>`
- Type variables → generic parameters
- Forall → strip quantifier, recurse on body
- Type applications → strip argument, recurse on function (lossy)

### Known Serde issues

These are in the Rust serializer (`Hydra.Ext.Rust.Serde`), not the coder logic:

1. `pub` placement: emitted before `#[derive(...)]` instead of after
2. Qualified paths: use `.` instead of `::` (e.g., `Hydra.core.Type` instead of `hydra::core::Type`)
3. Extra spaces in generics: `Vec <T>` instead of `Vec<T>`
4. Newtype syntax: `struct Foo (T) ;` instead of `struct Foo(T);`

### Known coder issues

1. **Type applications**: Polymorphic types like `Flow s a` collapse to circular aliases (`type Flow = Flow`) because the type argument is discarded when stripping `TypeApplication`
2. **No term definitions**: `moduleToRust` only processes type definitions (uses `Pairs.first` from `partitionDefinitions`); term definitions are silently dropped

### Language constraints

Defined in `hydra-ext/src/main/haskell/Hydra/Ext/Sources/Rust/Language.hs`. Already accurate for both type and term-level support:

- **Elimination variants**: Record, Union, Wrap
- **Function variants**: Elimination, Lambda, Primitive
- **Term variants**: Annotated, Application, Either, Function, Let, List, Literal, Map, Maybe, Pair, Record, Set, Union, Unit, Variable, Wrap (16 total)
- **Type variants**: all 16 standard variants
- **Integer types**: i8–i64, u8–u64 (no i128/u128/isize/usize)
- **Float types**: f32, f64 (no bigfloat)

No changes needed for term-level support.

### Hoisting analysis

Neither form of hoisting is needed for Rust:

- **Case statement hoisting** (used by Python): NOT needed. Rust `match` is an expression that can appear anywhere, unlike Python's `match` which is statement-level only.
- **Polymorphic let hoisting** (used by Java): NOT needed. Rust supports `let` bindings anywhere in a block. Monomorphization handles generics; no need to hoist polymorphic definitions to a special scope.

The `writeRust` flags correctly set both to False.

## Plan: Term-Level Support

### Approach

Model after the Scala coder (`hydra-ext/src/main/haskell/Hydra/Ext/Staging/Scala/Coder.hs`, ~300 lines) rather than the 4000–5000 line Java/Python coders. The Scala coder provides a simple, direct term encoding without environment management, type inference contexts, or complex variable tracking. Add ~200 lines of new TBindings to the existing DSL source.

### New helper TBindings

These construct Rust expression AST nodes:

| Binding | Signature | Purpose |
|---------|-----------|---------|
| `rustExprPath` | `String -> R.Expression` | Variable reference as path expression |
| `rustCall` | `R.Expression -> [R.Expression] -> R.Expression` | Function call expression |
| `rustBlock` | `[R.Statement] -> R.Expression -> R.Expression` | Block with statements + trailing expression |
| `rustLetStmt` | `String -> R.Expression -> R.Statement` | `let name = expr;` statement |
| `rustClosure` | `[String] -> R.Expression -> R.Expression` | Closure `\|params\| body` |

### Core encoding TBindings

#### `encodeLiteral :: Literal -> R.Expression`

Map Hydra literal values to Rust literal expressions:

| Hydra | Rust |
|-------|------|
| `LiteralBoolean b` | `true` / `false` |
| `LiteralString s` | `"s"` |
| `LiteralFloat (Float32 f)` | `f_f32` |
| `LiteralFloat (Float64 f)` | `f` (default) |
| `LiteralInteger (Int32 i)` | `i_i32` |
| `LiteralInteger (Int64 i)` | `i_i64` |
| `LiteralInteger (Uint8 i)` | `i_u8` |
| etc. | with appropriate suffix |
| `LiteralBinary bs` | `vec![...]` |

#### `encodeTerm :: Term -> Flow Graph R.Expression`

Pattern match on all `Term` variants:

| Variant | Rust expression |
|---------|----------------|
| `TermLiteral` | delegate to `encodeLiteral` |
| `TermVariable` | `rustExprPath` with snake_case sanitized name |
| `TermApplication` | flatten nested applications → `rustCall(fn, [args])` |
| `TermFunction` | delegate to `encodeFunction` |
| `TermLet` | `rustBlock` with `rustLetStmt` for each binding, body as trailing expression |
| `TermList` | `vec![e1, e2, ...]` via macro invocation |
| `TermRecord` | struct literal `R.Expression_struct` with field values |
| `TermUnion` | `TypeName::VariantName(value)` via path + call |
| `TermMaybe` | `None` or `Some(x)` |
| `TermPair` | `R.Expression_tuple [e1, e2]` |
| `TermSet` | `BTreeSet::from([...])` |
| `TermMap` | `BTreeMap::from([(k1,v1), ...])` |
| `TermWrap` | newtype constructor call `Name(inner)` |
| `TermUnit` | `()` |
| `TermEither` | `Left(x)` / `Right(x)` |
| `TermAnnotated` | strip annotation, recurse on body |

#### `encodeFunction :: Function -> Flow Graph R.Expression`

| Variant | Rust expression |
|---------|----------------|
| `FunctionLambda` | accumulate nested lambda params → `rustClosure [p1, p2, ...] body` |
| `FunctionElimination` | delegate to `encodeElimination` |
| `FunctionPrimitive` | `rustExprPath` with qualified primitive name |

#### `encodeElimination :: Elimination -> Maybe Term -> Flow Graph R.Expression`

Takes the elimination and an optional argument (if already applied via `TermApplication`):

| Variant | Rust expression |
|---------|----------------|
| `EliminationRecord (Projection _ fieldName)` | `arg.field_name` via `R.Expression_fieldAccess` |
| `EliminationUnion (CaseStatement typeName default cases)` | `match arg { Variant(v) => body, ... }` via `R.Expression_match` with `R.MatchArm` entries |
| `EliminationWrap name` | `.0` tuple index access on newtype |

For unapplied eliminations (no argument), wrap in a closure: `\|v\| match v { ... }`.

#### `encodeTermDefinition :: TermDefinition -> Flow Graph R.ItemWithComments`

Convert a term definition to a Rust function item:
- If the term is a function (top-level lambda/elimination): emit `fn name(params) -> Type { body }` using `R.Item_fn`
- If the term is a value: emit `fn name() -> Type { expr }` as a nullary function (Rust has no top-level `let` outside of `const`/`static`)

#### Update `moduleToRust`

Change from using only `Pairs.first` (type defs) to also processing `Pairs.second` (term defs) via `encodeTermDefinition`. Merge both into the crate's item list.

### Rust AST nodes used

The Rust syntax model (`Hydra.Ext.Rust.Syntax`) already has all needed expression types:

- `R.Expression_literal` / `R.Literal_bool`, `R.Literal_string`, `R.Literal_integer`, `R.Literal_float`
- `R.Expression_path` / `R.ExprPath` — variable references
- `R.Expression_call` / `R.CallExpr` — function calls
- `R.Expression_closure` / `R.ClosureExpr` — closures with params
- `R.Expression_block` / `R.Block` — blocks with statements + trailing expression
- `R.Expression_match` / `R.MatchExpr` — pattern matching
- `R.Expression_fieldAccess` / `R.FieldAccessExpr` — field access
- `R.Expression_tupleIndex` / `R.TupleIndexExpr` — `.0` access
- `R.Expression_struct` / `R.StructExpr` — struct literals
- `R.Expression_tuple` — tuple expressions
- `R.Expression_macro` / `R.MacroInvocation` — `vec![...]`, etc.
- `R.Statement_let` / `R.LetStatement` — let bindings
- `R.Pattern_identifier`, `R.Pattern_tupleStruct`, `R.Pattern_wildcard` — match patterns
- `R.Item_fn` / `R.FnDef` — function definitions

No extensions to the AST are required.

### What is NOT in scope (first pass)

- Type inference / type annotations on generated terms (types come from the adapter pipeline)
- Environment management (variable tracking, scope management)
- Ownership/borrowing annotations (clone/move/reference)
- Tail call optimization
- Import generation (use statements)
- Visibility modifiers on fn items

These can be added incrementally as the coder matures.

## Files

| File | Role |
|------|------|
| `hydra-ext/src/main/haskell/Hydra/Ext/Sources/Rust/Coder.hs` | DSL source (hand-written) |
| `hydra-ext/src/gen-main/haskell/Hydra/Ext/Rust/Coder.hs` | Generated Haskell (auto-generated from DSL) |
| `hydra-ext/src/main/haskell/Hydra/Ext/Sources/All.hs` | Module registration |
| `hydra-ext/src/main/haskell/Hydra/Ext/Generation.hs` | `writeRust` helper |
| `hydra-ext/src/main/haskell/Hydra/Ext/Sources/Rust/Language.hs` | Language constraints DSL |
| `hydra-ext/src/main/haskell/Hydra/Ext/Sources/Rust/Syntax.hs` | Rust AST DSL |
| `hydra-ext/src/main/haskell/Hydra/Ext/Sources/Rust/Serde.hs` | Rust serializer DSL |
| `hydra-rust/staged-Coder.hs` | Staging copy |
| `hydra-rust/src/gen-main/rust/` | Generated Rust output |

## Verification

1. `cd hydra-ext && stack build` — DSL source compiles
2. `stack exec update-haskell-ext-main` — regenerate generated Haskell from DSL
3. `stack build` — regenerated code compiles
4. GHCi: `writeRust "../hydra-rust/src/gen-main/rust" universe kernelTypesModules`
5. Inspect generated `.rs` files for correctness
