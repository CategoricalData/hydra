# Hydra-JavaScript Implementation Plan and Status

## Overview

This document tracks the implementation progress of Hydra-JavaScript, following the [new implementation guide](../docs/recipes/new-implementation.md).

## Implementation Steps (from guide)

### Step 1: Create the syntax model (JavaScript AST)
**Status**: DONE (locally, pending move to appropriate package)
- Created `src/main/haskell/Hydra/Ext/Sources/JavaScript/Syntax.hs`
- Based on ECMAScript 2024 specification
- Covers: identifiers, literals, types (JSDoc/TS style), expressions, statements, patterns, declarations, modules, operators, JSDoc comments
- Ready to move to `packages/hydra-javascript/src/main/haskell/Hydra/Ext/Sources/JavaScript/Syntax.hs` when validated

### Step 2: Define language constraints
**Status**: DONE (locally, pending move to appropriate package)
- Created `src/main/haskell/Hydra/Ext/Sources/JavaScript/Language.hs`
- Defines what Hydra features JavaScript supports
- Key constraints:
  - Float types: float64 (JavaScript's number)
  - Integer types: int32, bigint (JavaScript's BigInt)
  - All term variants supported
  - All type variants supported
  - Reserved words defined (keywords, future reserved, built-ins)
- Ready to move to `packages/hydra-javascript/src/main/haskell/Hydra/Ext/Sources/JavaScript/Language.hs` when validated

### Step 3: Generate Haskell sources
**Status**: Deferred (requires Haskell changes and validation of Steps 1-2)

### Step 4: Create a coder
**Status**: Deferred (requires Steps 1-3)

### Step 5: Create the serializer
**Status**: Deferred (requires Steps 1-4)

### Step 6: Register and generate code
**Status**: Deferred (requires Steps 1-5)

### Step 7: Implement standard primitives
**Status**: DONE
- All 13 library modules implemented in JavaScript
- 200+ primitive functions covered:
  - lists (38 functions): map, filter, fold, concat, sort, etc.
  - strings (14 functions): cat, split, lines, words, etc.
  - math (40+ functions): arithmetic, trig, rounding, etc.
  - maps (20 functions): lookup, insert, keys, etc.
  - sets (14 functions): union, intersection, member, etc.
  - maybes (12 functions): bind, map, fromMaybe, etc.
  - eithers (14 functions): bind, map, partition, etc.
  - logic (4 functions): and, or, not, ifElse
  - equality (9 functions): equal, compare, lt, gt, etc.
  - chars (6 functions): isAlphaNum, isLower, toUpper, etc.
  - pairs (3 functions): first, second, bimap
  - flows (16 functions): pure, fail, bind, map, apply, mapList, mapSet, mapElems, mapKeys, mapMaybe, sequence, foldl, getState, putState, withTrace, fromMaybe
  - literals (40+ functions): type conversions, read/show

### Step 8: Implement essential utilities
**Status**: DONE
- Flow monad helpers in `tools/index.js`: flowToValue, flowToMaybe, runFlow, fromMaybe
- Deep equality and comparison utilities: deepEqual, deepCompare
- FrozenMap class in `core.js`
- Name class in `core.js`

### Step 9: Create a test runner
**Status**: DONE
- Jest configured in package.json
- 4 test files with 241 tests covering:
  - Core types (Name, FrozenMap, Maybe, Either, Literals, Terms, Types)
  - Flow monad and compute infrastructure
  - DSL modules (types, terms)
  - Library functions (lists, strings, math, maps, sets, maybes, eithers, logic, equality, chars, pairs)
- Run with `npm test`

### Step 10: Create native DSLs and build applications
**Status**: DONE
- Types DSL (`dsl/types.js`): string, int32, list, record, union, forall, etc.
- Terms DSL (`dsl/terms.js`): literals, lambda, apply, record, inject, let, match, etc.
- Expect DSL (`dsl/expect.js`): string, int32, list, record, variant, etc.
- Literals DSL (`dsl/literals.js`): int8, float64, string, etc.
- LiteralTypes DSL (`dsl/literalTypes.js`): int8, float64, string, etc.

---

## What We Can Do Without Haskell Changes

Since Steps 3-6 require Haskell modifications, we focused on what can be built independently:

| Item | Status |
|------|--------|
| Core type definitions | DONE |
| Flow monad and compute infrastructure | DONE |
| DSL modules (types, terms, expect) | DONE |
| Standard library primitives (200+) | DONE |
| Graph and primitive registration | DONE |
| Package configuration | DONE |
| Main entry point (index.js) | DONE |
| JavaScript syntax model (Haskell DSL) | DONE (local) |
| Language constraints (Haskell DSL) | DONE (local) |
| Basic test suite | DONE (241 tests) |

---

## Design Decisions

### 1. Union Types: Tagged Objects

JavaScript has no native algebraic data types. We use a consistent tagged object pattern:

```javascript
// Term variants
const term = { tag: "variable", value: name };
const term = { tag: "literal", value: literal };
const term = { tag: "application", value: { function: f, argument: x } };

// Pattern matching via switch
switch (term.tag) {
  case "variable": return handleVariable(term.value);
  case "literal": return handleLiteral(term.value);
  // ...
}
```

This was chosen over:
- **Classes with instanceof**: Verbose, doesn't compose well, no exhaustiveness checking
- **Symbols**: Not serializable, harder to debug
- **Visitor pattern (Java-style)**: Too verbose for JavaScript's dynamic nature

### 2. Immutability: Object.freeze()

All constructed Hydra values are deeply frozen:

```javascript
function makeApplication(fn, arg) {
  return Object.freeze({
    tag: "application",
    value: Object.freeze({ function: fn, argument: arg })
  });
}
```

### 3. Name: Simple wrapper

```javascript
class Name {
  constructor(value) {
    this.value = value;
    Object.freeze(this);
  }
}
```

### 4. Maybe/Either: Tagged objects (consistent with union pattern)

```javascript
const just = (v) => Object.freeze({ tag: "just", value: v });
const nothing = () => Object.freeze({ tag: "nothing" });
const left = (v) => Object.freeze({ tag: "left", value: v });
const right = (v) => Object.freeze({ tag: "right", value: v });
```

### 5. Maps: Immutable Map wrapper

JavaScript `Map` is mutable. We create `FrozenMap`:

```javascript
class FrozenMap {
  constructor(entries) {
    this._map = new Map(entries);
    Object.freeze(this);
  }
  get(key) { return this._map.get(key); }
  // ... other read-only methods
}
```

### 6. Integer/Float handling

JavaScript has only `number` (64-bit float) and `BigInt`:

| Hydra Type | JavaScript Type | Notes |
|-----------|----------------|-------|
| int8, int16, int32 | number | Range-checked |
| int64 | BigInt | Native BigInt |
| uint8, uint16, uint32 | number | Range-checked |
| uint64 | BigInt | Native BigInt |
| bigint | BigInt | Native |
| float32, float64 | number | IEEE 754 |
| bigfloat | number | No native arbitrary precision |

### 7. Module System

Use ES modules (ESM) with `import`/`export`:

```javascript
// core.js
export class Name { ... }
export function termVariable(name) { ... }

// types.js
import { Name } from '../core.js';
export function string() { ... }
```

---

## File-by-File Progress

### Core Files

| File | Status | Description |
|------|--------|-------------|
| `package.json` | DONE | Project configuration |
| `README.md` | DONE | Project documentation |
| `core.js` | DONE | Term, Type, Literal, Name, FrozenMap (171 exports) |
| `compute.js` | DONE | Flow, FlowState, Trace, Coder, Adapter, Bicoder |
| `graph.js` | DONE | Graph, Primitive, TermCoder |
| `index.js` | DONE | Main entry point, re-exports all modules |

### DSL Files

| File | Status | Description |
|------|--------|-------------|
| `dsl/types.js` | DONE | Type construction DSL |
| `dsl/terms.js` | DONE | Term construction DSL |
| `dsl/expect.js` | DONE | Term decoding/extraction |
| `dsl/literals.js` | DONE | Literal constructors |
| `dsl/literalTypes.js` | DONE | Literal type constructors |

### Library Files

| File | Status | Description |
|------|--------|-------------|
| `lib/chars.js` | DONE | Character predicates (6 functions) |
| `lib/equality.js` | DONE | Comparison operators (9 functions) |
| `lib/eithers.js` | DONE | Either operations (14 functions) |
| `lib/flows.js` | DONE | Flow monad operations (16 functions) |
| `lib/lists.js` | DONE | List operations (38 functions) |
| `lib/literals.js` | DONE | Literal conversions (40+ functions) |
| `lib/logic.js` | DONE | Boolean operations (4 functions) |
| `lib/maps.js` | DONE | Map/dictionary operations (20 functions) |
| `lib/math.js` | DONE | Arithmetic functions (40+ functions) |
| `lib/maybes.js` | DONE | Maybe operations (12 functions) |
| `lib/pairs.js` | DONE | Tuple operations (3 functions) |
| `lib/sets.js` | DONE | Set operations (14 functions) |
| `lib/strings.js` | DONE | String operations (14 functions) |

### Tools and Registration

| File | Status | Description |
|------|--------|-------------|
| `tools/index.js` | DONE | Utility functions |

### Haskell DSL Files (local, pending move to appropriate package)

| File | Status | Description |
|------|--------|-------------|
| `src/main/haskell/.../JavaScript/Syntax.hs` | DONE | JavaScript AST model |
| `src/main/haskell/.../JavaScript/Language.hs` | DONE | Language constraints |

### Tests

| File | Status | Description |
|------|--------|-------------|
| `test/core.test.js` | DONE | Core type tests (Name, FrozenMap, Maybe, Either, Literals, Terms, Types) |
| `test/compute.test.js` | DONE | Flow monad tests |
| `test/dsl.test.js` | DONE | DSL tests (types, terms) |
| `test/lib.test.js` | DONE | Library tests (13 modules) |

---

## Next Steps (Requiring Haskell Changes)

When ready to proceed with Haskell changes:

1. **Move and validate syntax model** from `hydra-javascript/src/main/haskell/...` to `packages/hydra-javascript/src/main/haskell/Hydra/Ext/Sources/JavaScript/Syntax.hs`
   - Verify it compiles
   - Register in `Hydra.Ext.Sources.All`

2. **Move and validate language constraints** to `packages/hydra-javascript/src/main/haskell/Hydra/Ext/Sources/JavaScript/Language.hs`
   - Verify it compiles
   - Register in module

3. **Create the coder** in `packages/hydra-javascript/src/main/haskell/Hydra/Ext/Staging/JavaScript/Coder.hs`
   - Map Hydra types to JavaScript class/object patterns
   - Map Hydra terms to JavaScript expressions
   - Handle union types via tagged objects
   - Handle records via frozen plain objects

4. **Create the serializer** in `packages/hydra-javascript/src/main/haskell/Hydra/Ext/Staging/JavaScript/Serde.hs`
   - Render JavaScript AST to concrete syntax
   - Handle indentation, semicolons, etc.

5. **Generate code** - Run `writeJavaScript` to generate:
   - `hydra-javascript/src/gen-main/javascript/hydra/` - Generated kernel code
   - `hydra-javascript/src/gen-test/javascript/hydra/` - Generated test suite

6. **Create test runner** - Connect generated test suite to Jest

---

## Architecture Notes

### Comparison with Other Implementations

| Aspect | Haskell | Java | Python | JavaScript |
|--------|---------|------|--------|------------|
| Union types | ADTs | Visitor pattern | Metaclass + dataclasses | Tagged objects |
| Records | Data types | Classes with final fields | Frozen dataclasses | Frozen objects |
| Immutability | Default | Manual (final fields) | @dataclass(frozen=True) | Object.freeze() |
| Maybe | Maybe a | Maybe<T> class | Just/Nothing classes | Tagged objects |
| Either | Either a b | Either<L,R> class | Left/Right classes | Tagged objects |
| Maps | Map k v | Map<K,V> | FrozenDict | FrozenMap class |
| Flow | newtype | BiFunction class | Callable wrapper | Function |
| Pattern matching | Native | Visitor | match/case | switch(tag) |
| Module system | Haskell modules | Java packages | Python packages | ES modules |
| Generics | Type parameters | Java generics | Generic[T] | JSDoc/none |

### JavaScript-Specific Considerations

1. **No static type system**: We use JSDoc comments for documentation but can't enforce types at compile time. TypeScript could be added later.

2. **Prototype-based**: We avoid class hierarchies and prefer factory functions + plain objects for union types.

3. **Reference equality**: JavaScript objects compare by reference. We provide custom equality functions where needed.

4. **BigInt support**: Native BigInt for arbitrary-precision integers. Some care needed for interop with number.

5. **No pattern matching**: Use `switch(obj.tag)` instead. Could add a `match()` utility function.

6. **Weak immutability**: `Object.freeze()` is shallow by default. Our constructors freeze deeply.

---

## Summary of Progress

**Completed without Haskell changes:**
- All core JavaScript files (core.js, compute.js, graph.js, index.js)
- All DSL modules (types, terms, expect, literals, literalTypes)
- All library modules (13 files, 200+ functions)
- Tools and utilities
- Package configuration (package.json, README.md)
- JavaScript syntax model (Haskell DSL, local)
- Language constraints (Haskell DSL, local)

**Remaining (requires Haskell integration):**
- Validate Haskell files in packages/hydra-javascript and confirm compilation
- Create coder and serializer
- Generate JavaScript code from Hydra modules
- Create test runner for generated test suite
