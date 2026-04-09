# Hydra-Go Implementation Plan and Status

## Overview

This document tracks progress on implementing Hydra in Go, following the 10-step process
described in `docs/recipes/new-implementation.md`.

## Go-Specific Design Decisions

### 1. Representing Algebraic Data Types (Sum Types)

Hydra's core is built on algebraic data types (ADTs), which Go lacks natively. Our approach:

- **Sum types**: Interface with sealed marker method + concrete struct per variant
  - Example: `Term` interface with `TermAnnotated`, `TermApplication`, ... structs
  - Pattern matching via `switch t := term.(type) { case *TermLiteral: ... }`
- **Product types (records)**: Plain Go structs with exported fields
- **Newtypes (wrappers)**: Go named types (e.g., `type Name string`)

This mirrors how Java uses abstract class + visitor pattern, but is more idiomatic for Go.

### 2. Generics Strategy

Go 1.18+ generics are used for parameterized types:
- `Flow[S, V any]` - state monad
- `Coder[S1, S2, V1, V2 any]` - bidirectional transformation
- `Adapter[S1, S2, T1, T2, V1, V2 any]` - type-aware adapter

Go's generics have limitations compared to Haskell/Java:
- No higher-kinded types
- No method-level type parameters (only function-level and type-level)
- Type constraints are limited to interfaces

This means some patterns need to be adapted (e.g., visitor pattern replaced with type switches).

### 3. Module/Package Layout

Go's package system maps well to Hydra's module system:
- `hydra/core` -> Hydra.Core
- `hydra/compute` -> Hydra.Compute
- `hydra/graph` -> Hydra.Graph
- `hydra/module` -> Hydra.Module (Go keyword conflict: use `hmodule`)

### 4. Option/Maybe Types

- `*T` (pointer, nil for None) for simple cases
- `Maybe[T]` generic wrapper for explicit optionality in APIs

### 5. Immutability

Go structs are value types by default but we use pointer receivers for interface
implementations. Types are designed to be treated as immutable even though Go doesn't
enforce this.

### 6. Type Mapping: Hydra Types -> Go Types

| Hydra Type | Go Type |
|---|---|
| `Name` | `type Name string` |
| `Term` (18 variants) | `Term` interface + 18 struct types |
| `Type` (16 variants) | `Type` interface + 16 struct types |
| `Literal` (5 variants) | `Literal` interface + 5 struct types |
| `Function` (3 variants) | `Function` interface + 3 struct types |
| `Elimination` (3 variants) | `Elimination` interface + 3 struct types |
| `IntegerValue` (9 variants) | `IntegerValue` interface + 9 struct types |
| `FloatValue` (3 variants) | `FloatValue` interface + 3 struct types |
| `IntegerType` (9 variants) | `IntegerType` = `int` enum via iota |
| `FloatType` (3 variants) | `FloatType` = `int` enum via iota |
| `LiteralType` (5 variants) | `LiteralType` interface + 5 struct types |
| `Record` | `Record` struct |
| `Field` | `Field` struct |
| `RowType` | `RowType` struct |
| `Injection` | `Injection` struct |
| `Lambda` | `Lambda` struct |
| `Let` | `Let` struct |
| `Application` | `Application` struct |
| `Binding` | `Binding` struct |
| `CaseStatement` | `CaseStatement` struct |
| `TypeScheme` | `TypeScheme` struct |
| `Flow s v` | `Flow[S, V any]` struct |
| `Coder s1 s2 v1 v2` | `Coder[S1, S2, V1, V2 any]` struct |
| `Graph` | `Graph` struct |
| `Primitive` | `Primitive` struct |
| `Module` | `Module` struct |
| `Namespace` | `type Namespace string` |
| `Map Term Term` | `map[Term]Term` (requires custom hashable wrapper) |
| `Set Term` | Custom `Set[T]` type (Go has no built-in set) |
| `[Term]` | `[]Term` |
| `Maybe Term` | `*Term` or `Maybe[Term]` |
| `Either Term Term` | `Either[L, R any]` interface |
| `ByteString` | `[]byte` |
| `Int8` | `int8` |
| `Int16` | `int16` |
| `Int32` | `int32` |
| `Int64` | `int64` |
| `Float32` | `float32` |
| `Float64` | `float64` |
| `Integer` (bigint) | `*big.Int` |
| `Double` (bigfloat) | `*big.Float` or `float64` |

## 10-Step Implementation Progress

### Step 1: Create the Syntax Model
**Status**: COMPLETE (added to hydra-ext)

Created at `hydra-ext/src/main/haskell/Hydra/Ext/Sources/Go/Syntax.hs`.

Based on the Go Language Specification (https://go.dev/ref/spec, retrieved 2025-02-05).

**Covers**:
- Source file structure (package, imports, declarations)
- All declaration types (const, var, type, function, method)
- All type literals (array, slice, struct, pointer, function, interface, map, channel)
- Generic type parameters and constraints (Go 1.18+)
- All expressions (unary, binary, primary, literals, conversions)
- All statements (if, for, switch, select, go, defer, return, etc.)
- Composite literals with keyed elements
- Channel operations and directions

### Step 2: Define Language Constraints
**Status**: COMPLETE (added to hydra-ext)

Created at `hydra-ext/src/main/haskell/Hydra/Ext/Sources/Go/Language.hs`.

**Go's language constraints defined**:
- **Elimination variants**: record, union (via type switch), wrap
- **Literal variants**: binary ([]byte), boolean, float, integer, string
- **Float types**: float32, float64 (no native bigfloat)
- **Function variants**: elimination, lambda, primitive (all three)
- **Integer types**: int8, int16, int32, int64, uint8, uint16, uint32, uint64, bigint (math/big.Int)
- **Term variants**: All 18 supported (some via interface patterns)
- **Type variants**: All 16 supported (Go 1.22+ generics enable most patterns)
- **Reserved words**: Go keywords, predeclared identifiers, Hydra-Go specific (Node, Maybe, Either, etc.)

### Step 3: Generate Haskell Sources
**Status**: Deferred (requires Haskell changes)

### Step 4: Create a Coder
**Status**: Deferred (requires Haskell changes)

### Step 5: Create the Serializer
**Status**: COMPLETE (added to hydra-ext)

Created at `hydra-ext/src/main/haskell/Hydra/Ext/Sources/Go/Serde.hs`.

The serializer converts Go AST nodes to Hydra's `Ast.Expr` representation, which can then
be printed to source code text. Provides `write*` functions for all Go syntax types:
- Source file structure (sourceFile, module, package, imports)
- Declarations (const, var, type, function, method)
- Types (all type literals, generics, channels)
- Expressions (unary, binary, primary, literals, conversions)
- Statements (if, for, switch, select, go, defer, return, etc.)
- Operators (unary, binary, assignment)
- Terminal tokens (identifiers, literals with proper escaping)

### Step 6: Register and Generate Code
**Status**: Deferred (requires Haskell changes)

### Step 7: Implement Standard Primitives
**Status**: COMPLETE (all Haskell primitives now have Go equivalents)

Implementing hand-written Go primitives in `src/main/go/hydra/lib/`.

Progress:
- [x] math (hmath package: add, sub, mul, div, mod, neg, rem, abs, max, min, signum, even, odd, pred, succ, range, trig, rounding with bigint)
- [x] logic (and, or, not, ifElse)
- [x] equality (equal, compare, gt, gte, lt, lte, identity, max, min)
- [x] strings (hstrings package: cat, cat2, charAt, fromList, intercalate, length, lines, null, splitOn, toList, toLower, toUpper, unlines)
- [x] lists (apply, at, bind, concat, concat2, cons, drop, dropWhile, elem, filter, find, foldl, group, head, init, intercalate, intersperse, last, length, map, nub, null, partition, pure, replicate, reverse, safeHead, singleton, sort, sortOn, span, tail, take, transpose, zip, zipWith)
- [x] maps (alter, bimap, delete, elems, empty, filter, filterWithKey, findWithDefault, fromList, insert, keys, lookup, lookupMaybe, map, mapKeys, member, null, singleton, size, toList, union)
- [x] sets (delete, difference, empty, fromList, insert, intersection, map, member, null, singleton, size, toList, union, unions)
- [x] maybes (apply, bind, cases, cat, catMaybes, compose, fromJust, fromMaybe, isJust, isNothing, map, mapMaybe, maybe, pure)
- [x] eithers (bimap, bind, either, fromLeft, fromRight, isLeft, isRight, lefts, map, mapLeft, mapList, mapMaybe, mapRight, partitionEithers, rights)
- [x] pairs (bimap, curry, first, mapFirst, mapSecond, second, swap, uncurry)
- [x] chars (isAlphaNum, isLower, isSpace, isUpper, toLower, toUpper) - using Int32 as per Haskell convention
- [x] flows (apply, bind, fail, foldl, map, mapElems, mapKeys, mapList, mapMaybe, mapSet, pure, sequence)
- [x] literals (all type conversions: bigfloat/bigint/int8-64/uint8-64/float32-64/binary/string, read/show functions)

### Step 8: Implement Essential Utilities
**Status**: IN PROGRESS

- [x] Either type
- [x] Maybe type
- [x] Pair/Tuple type
- [x] Unit type
- [x] Set type (custom, since Go lacks built-in sets)

### Step 9: Create a Test Runner
**Status**: NOT STARTED

Will need to implement a test runner that executes the common Hydra test suite.

### Step 10: Create Native DSLs
**Status**: IN PROGRESS

- [x] Terms DSL (basic constructors)
- [x] Types DSL (basic constructors)
- [x] Flows DSL (basic monadic operations)
- [ ] Expect DSL (term decoding)
- [ ] Shorthand type constructors
- [ ] Shorthand primitive references

## Current Status Summary

**Date**: 2026-02-05

The following has been completed without touching Haskell code:
1. Directory structure created
2. Go module initialized (go 1.22)
3. Core types implemented (hydra/core package) - all 18 Term variants, 16 Type variants
4. Compute types implemented (hydra/compute package) - Flow, Coder, Adapter, Bicoder
5. Graph types implemented (hydra/graph package) - Graph, Primitive, TermCoder
6. Module types implemented (hydra/hmodule package) - Module, Namespace, Definition, Library
7. Utility types implemented (hydra/util package) - Maybe, Either, Pair, Set, Unit
8. DSL layer completed (hydra/dsl package) - Terms, Types, Flows, Expect
9. Standard library primitives COMPLETE (all 14 Haskell libraries implemented)
10. Go Syntax model created (hydra-ext/src/main/haskell/Hydra/Ext/Sources/Go/Syntax.hs) - comprehensive Go grammar
11. Go Language constraints defined (hydra-ext/src/main/haskell/Hydra/Ext/Sources/Go/Language.hs)
12. Go Serializer created (hydra-ext/src/main/haskell/Hydra/Ext/Sources/Go/Serde.hs) - AST to source code
13. Tools/utilities implemented (hydra/tools package) - FlowError, RunFlow, FromMaybe, PrimitiveFunction

Steps 3-4, 6 (Haskell source generation, coder, registration) require
Haskell modifications and are deferred.

## Next Steps (requiring Haskell changes)

1. ~~Create Go syntax model in hydra-ext~~ DONE
2. ~~Define Go language constraints~~ DONE
3. Create Go coder (Hydra terms -> Go AST)
4. ~~Create Go serializer (Go AST -> Go source text)~~ DONE
5. Register code generation and generate Go sources
6. Replace hand-written core types with generated equivalents

## Next Steps (Go-only)

1. Complete remaining primitive function implementations
2. Implement the Expect DSL for term decoding
3. Create test runner for common test suite
4. Implement JSON serialization/deserialization
5. Add more comprehensive tests
