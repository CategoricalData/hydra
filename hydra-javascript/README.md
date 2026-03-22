# Hydra-JavaScript

A JavaScript implementation of the [Hydra](https://github.com/CategoricalData/hydra) type system and kernel.

Hydra-JavaScript provides the same core functionality as [Hydra-Haskell](../hydra-haskell), [Hydra-Java](../hydra-java), and [Hydra-Python](../hydra-python): a runtime for Hydra's type system (System F with Hindley-Milner inference), the Flow monad for stateful computation, bidirectional Coders for type-aware transformations, and the full standard library of primitive functions.

## Project Structure

```
hydra-javascript/
  src/
    main/javascript/hydra/       # Hand-written source code
      core.js                    # Core type definitions (Term, Type, Literal, etc.)
      compute.js                 # Flow monad, Coder, Adapter abstractions
      graph.js                   # Graph execution context and Primitive definitions
      dsl/                       # Domain-Specific Languages
        types.js                 # Type construction DSL
        terms.js                 # Term construction DSL
        expect.js                # Term decoding/extraction DSL
        literals.js              # Literal value constructors
        literalTypes.js          # Literal type constructors
      lib/                       # Standard library primitive implementations
        chars.js                 # Character predicates and transforms
        equality.js              # Comparison operators
        eithers.js               # Either/sum type operations
        flows.js                 # Flow monad operations
        lists.js                 # List operations
        literals.js              # Literal type conversions
        logic.js                 # Boolean operations
        maps.js                  # Map/dictionary operations
        math.js                  # Arithmetic and numeric functions
        maybes.js                # Maybe/optional operations
        pairs.js                 # Tuple/pair operations
        sets.js                  # Set operations
        strings.js               # String operations
      sources/
        libraries.js             # Primitive function registration
      tools/
        index.js                 # Utility functions
    gen-main/javascript/hydra/   # Generated code (from Hydra-Haskell)
    gen-test/javascript/hydra/   # Generated test suite
  test/javascript/hydra/         # Hand-written tests
  package.json                   # Node.js project configuration
```

## Design Decisions for JavaScript

### Type Representation

JavaScript lacks native algebraic data types, so Hydra types are represented using a **tagged object pattern**:

- **Union types** (Term, Type, Literal, etc.) use objects with a `tag` string field and a `value` field
- **Record types** (Application, Lambda, Field, etc.) use plain frozen objects with named fields
- **Enum types** (IntegerType, FloatType) use string constants
- **Newtypes** (Name) use the `Node` wrapper class
- **Immutability** is enforced via `Object.freeze()` on all constructed values

### Maybe and Either

- `Maybe<T>` is represented as `{ tag: "just", value: T }` or `{ tag: "nothing" }`
- `Either<L, R>` is represented as `{ tag: "left", value: L }` or `{ tag: "right", value: R }`

### Immutable Collections

- Lists use frozen `Array` (via `Object.freeze()`)
- Maps use a custom `FrozenMap` wrapper around `Map`
- Sets use frozen `Set`

### Flow Monad

The Flow monad follows the same pattern as Python and Java: `Flow<S, V>` is a function `(state, trace) => FlowState(maybeValue, state, trace)`.

## Prerequisites

- Node.js >= 18
- npm or yarn

## Installation

```bash
cd hydra-javascript
npm install
```

## Running Tests

```bash
npm test
```

## Status

See [implementation-plan-and-status.md](implementation-plan-and-status.md) for detailed progress tracking.
