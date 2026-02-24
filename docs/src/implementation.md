# Hydra Implementation Overview

This document provides a detailed look at Hydra's implementation, from type modules to coders to primitives to DSLs.
It complements the [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) documentation by focusing on the concrete architecture and code
organization rather than abstract foundations.

## Prerequisites

**Before reading this guide**, you should:
- Understand Hydra's core concepts ([Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts))
- Be familiar with at least one of: Haskell, Java, or Python
- Have Hydra cloned and built locally (see main [README](https://github.com/CategoricalData/hydra))

**This guide is for:**
- Contributors who want to extend Hydra's kernel
- Developers implementing new language coders
- Anyone curious about Hydra's internal architecture

**If you just want to use Hydra**, start with [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) and the main README instead.

## Table of contents

1. [Architecture overview](#architecture-overview)
2. [Type modules](#type-modules)
3. [DSL system](#dsl-system)
4. [Primitive functions](#primitive-functions)
5. [Cross-language compilation (coders)](#cross-language-compilation-coders)
6. [The bootstrap process](#the-bootstrap-process)
7. [Extending Hydra](#extending-hydra)
8. [Appendix: Build scripts and executables](#appendix-build-scripts-and-executables)

---

## Architecture overview

Hydra is a **strongly-typed functional programming language that executes in multiple language environments**.
By design, developers can write Hydra source code in any of the supported host languages (Java, Python, Haskell)
and cross-compile it to any other supported language.
[Hydra-Haskell](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell) serves as the source of truth
for the **Hydra kernel** (the core type system and transformation infrastructure), but Hydra programs themselves
can be written and executed in Java, Python, or any other supported implementation.

The implementation follows a layered architecture:

```
┌──────────────────────────────────────────────────────────────┐
│                   Hydra Kernel (Source of Truth)             │
│  Type system: Term, Type, Module, Graph, primitives, etc.    │
│  Location: hydra-haskell/src/main/haskell/Hydra/Sources/     │
│  Written using: Haskell DSLs                                 │
└────────────────────────┬─────────────────────────────────────┘
                         │ Defines
                         ▼
┌──────────────────────────────────────────────────────────────┐
│              Language Implementations (Peers)                │
│                                                              │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐           │
│  │   Haskell   │  │    Java     │  │   Python    │  ...      │
│  │  (bootstrap)│  │             │  │             │           │
│  └─────────────┘  └─────────────┘  └─────────────┘           │
│                                                              │
│  Each implementation provides:                               │
│  • Hydra type system runtime                                 │
│  • Primitive function implementations                        │
│  • Ability to execute Hydra programs                         │
│  • APIs for writing Hydra code in host language              │
└────────────────────────┬─────────────────────────────────────┘
                         │ Cross-compile via
                         ▼
┌──────────────────────────────────────────────────────────────┐
│         Coders (Cross-Language Transformations)              │
│  Transform Hydra modules between language implementations    │
│  Location: hydra-ext/src/main/haskell/Hydra/Ext/Staging/     │
│  Enable: Write in Java, compile to Python (or vice versa)    │
└──────────────────────────────────────────────────────────────┘
```

### Key design principles

1. **Multi-language by design**: Hydra programs can be written in any supported host language and cross-compiled
   to others
2. **Unified type system**: All implementations share the same Hydra kernel (types, primitives, semantics)
3. **Self-hosting**: The Hydra kernel is defined in Hydra itself (using Haskell as the bootstrap language)
4. **Type safety**: Multiple layers of static type checking (host language + Hydra type system)
5. **Modularity**: Clean separation between kernel definition, language implementations, and cross-compilation

---

## Type modules

Type modules define Hydra's core type system. They are located in:
```
hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Types/
```

### Module organization

Hydra's kernel consists of **20 type modules** organized into logical categories:

#### Core Foundation (3 modules)

**Core.hs** - `hydra.core` namespace (13,531 bytes, largest module)
- Central hub defining fundamental types: `Term`, `Type`, `Literal`, `Function`, `Application`, `Lambda`, `Let`,
  `Record`, `Union`, etc.
- All other modules depend on Core directly or transitively
- Special property: imports itself as a type-level dependency

**Mantle.hs** - `hydra.mantle` namespace
- Supplements Core with metadata types NOT referenced by Core
- Defines variant enums: `TermVariant`, `TypeVariant`, `LiteralVariant`, etc.
- Provides introspection capabilities: `Precision`, `Comparison`

**Module.hs** - `hydra.module` namespace
- Defines module structure: `Module`, `Definition`, `Namespace`, `QualifiedName`
- Enables organized code with namespaces and dependencies

#### Transformation and Computation (3 modules)

**Compute.hs** - `hydra.compute` namespace
- Core abstractions: `Flow` monad, `Coder`, `Adapter`, `Bicoder`
- Generic over state and value types
- Foundation for all transformations

**Coders.hs** - `hydra.coders` namespace
- Language-specific transformation framework
- Defines: `Language`, `LanguageConstraints`, `AdapterContext`, `TraversalOrder`

**Workflow.hs** - `hydra.workflow` namespace
- High-level transformation workflows
- Schema specifications and transformations

#### Graph and Query (2 modules)

**Graph.hs** - `hydra.graph` namespace
- Extends core with graph operations
- Defines: `Graph`, `Primitive`, `TermCoder`

**Query.hs** - `hydra.query` namespace
- Language-agnostic graph pattern queries
- Triple patterns and path expressions

#### Type System Support (2 modules)

**Typing.hs** - `hydra.typing` namespace
- Type inference and reconstruction
- Type constraints and substitutions

**Constraints.hs** - `hydra.constraints` namespace
- Path and pattern-based graph constraints

#### Data Format Models (4 modules)

**Ast.hs** - `hydra.ast` - Common syntax tree for serializers
**Json.hs** - `hydra.json` - JSON data model
**Tabular.hs** - `hydra.tabular` - CSV/TSV data model (generic)
**Grammar.hs** - `hydra.grammar` - BNF grammar model

#### Utility and Specialized (6 modules)

**Accessors.hs** - `hydra.accessors` - Term access patterns
**Testing.hs** - `hydra.testing` - Unit testing framework
**Phantoms.hs** - `hydra.phantoms` - Phantom types for DSL use
**Relational.hs** - `hydra.relational` - Codd's Relational Model
**Topology.hs** - `hydra.topology` - Graph algorithms (Tarjan SCC)

### Type definition patterns

All type modules follow a consistent structure:

```haskell
module Hydra.Sources.Kernel.Types.ModuleName where

import Hydra.Kernel
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types
import qualified Hydra.Sources.Kernel.Types.Core as Core

module_ :: Module
module_ = Module ns elements termDeps typeDeps (Just description)
  where
    ns = Namespace "hydra.namespace"
    core = typeref $ moduleNamespace Core.module_
    def = datatype ns

    elements = [
      def "TypeName1" $ doc "Description" $ definition1,
      def "TypeName2" $ doc "Description" $ definition2,
      -- ...
    ]
```

#### Example: Union Type (from Core.hs)

```haskell
def "Term" $
  doc "A data term" $
  union [
    "annotated">: core "AnnotatedTerm",
    "application">: core "Application",
    "either">: Types.either_ (core "Term") (core "Term"),
    "function">: core "Function",
    "let">: core "Let",
    "list">: list $ core "Term",
    "literal">: core "Literal",
    "map">: Types.map (core "Term") (core "Term"),
    "optional">: optional $ core "Term",
    "product">: list $ core "Term",
    "record">: core "Record",
    "set">: set $ core "Term",
    "sum">: core "Sum",
    "union">: core "Injection",
    "variable">: core "Name",
    "wrap">: core "WrappedTerm"
  ]
```

#### Example: Record Type (from Module.hs)

```haskell
def "Module" $
  doc "A logical collection of elements in a namespace" $
  record [
    "namespace">: mod "Namespace",
    "elements">: list $ core "Binding",
    "termDependencies">: list $ mod "Namespace",
    "typeDependencies">: list $ mod "Namespace",
    "description">: optional string
  ]
```

#### Example: Generic Type (from Tabular.hs)

```haskell
def "Table" $
  doc "A simple table with header and data rows" $
  forAll "v" $ record [
    "header">: optional $ tabular "HeaderRow",
    "data">: list (tabular "DataRow" @@ "v")
  ]
```

#### Example: Enum Type (from Mantle.hs)

```haskell
def "TermVariant" $
  doc "The identifier of a term constructor" $
  enum [
    "annotated", "application", "either", "function",
    "let", "list", "literal", "map", "optional",
    "product", "record", "set", "sum", "union",
    "variable", "wrap"
  ]
```

### Dependency graph

```
Core (hydra.core) - Foundation
  ├─ Mantle - Supplements with variants
  ├─ Compute - Transformation abstractions
  ├─ Typing - Type system support
  ├─ Accessors - Term access patterns
  ├─ Phantoms - DSL phantom types
  ├─ Json - JSON model
  ├─ Tabular - Tabular data
  ├─ Query - Graph queries
  ├─ Testing - Test framework
  ├─ Grammar - BNF grammars
  └─ Topology - Graph algorithms

Compute
  ├─ Graph - Graph extension
  └─ Coders - Language transformations

Graph
  ├─ Workflow - Transformation workflows
  └─ Module - Namespace models

Query
  └─ Constraints - Graph constraints
```

**Key Properties:**
- No circular dependencies at type level
- Clear separation: foundation (Core/Mantle) vs. extensions
- Layered architecture: Atomic → Composite → Integrative → Specialized

---

## DSL system

Hydra uses **embedded domain-specific languages (eDSLs) in Haskell** to define its entire kernel.
The DSL system provides multiple levels of abstraction for different use cases.

### DSL module locations

```
hydra-haskell/src/main/haskell/Hydra/Dsl/     # Core DSLs (33 files)
hydra-haskell/src/main/haskell/Hydra/Dsl/Meta/Lib/ # Library DSLs (13 files)
hydra-ext/src/main/haskell/Hydra/Ext/Dsl/     # Extension DSLs
```

**See also:** [DSL guide](dsl-guide.md) - Comprehensive guide with examples and operator reference

### Three levels of DSLs

#### Level 1: Untyped DSLs (Terms.hs, Types.hs)

Direct term/type construction without compile-time safety:

```haskell
-- Terms.hs - construct Term values
term1 = var "x"
term2 = apply (var "f") (int32 42)
term3 = lambda "x" (var "x")

-- Types.hs - construct Type values
type1 = string
type2 = int32 --> string
type3 = list (optional boolean)
```

**Use Case:** Low-level term construction, minimal overhead, runtime errors possible

#### Level 2: Phantom-Typed DSLs (Phantoms.hs, Library DSLs)

Compile-time type safety via phantom types:

```haskell
-- Phantoms.hs - TTerm a where 'a' is a phantom type
goodFunc :: TTerm (Int -> String)
goodFunc = lambda "x" (Strings.toUpper (var "x"))

-- Type error at Haskell compile time!
badFunc :: TTerm (Int -> String)
badFunc = lambda "x" (int32 42)  -- Expected String, got Int
```

**Use Case:** Write Hydra code with Haskell's type checking as a safety net

#### Level 3: Term-Encoded DSLs (TTerms.hs, TTypes.hs)

Write programs that build programs (meta-programming):

```haskell
-- TTerms.hs - terms that construct terms
buildAddFunction :: TTerm (Int -> Int -> Int)
buildAddFunction =
  lambda "x" $ lambda "y" $
    primitive _math_add @@ var "x" @@ var "y"

-- Can inspect and transform this representation
```

**Use Case:** Code generators, meta-programs, self-modifying code

### Core DSL modules

#### Base Infrastructure
- **Common.hs** - Type class instances and utilities
- **Phantoms.hs** - Phantom-typed term construction DSL
- **PhantomLiterals.hs** - Literal handling with phantom types
- **TBase.hs** - Base for term-encoded DSLs

#### Term Construction
- **Terms.hs** - Plain DSL for terms (`apply`, `lambda`, `record`, `inject`)
- **TTerms.hs** - Phantom-typed term-encoded terms

#### Type Construction
- **Types.hs** - Plain DSL for types (operators `-->`, `@@`)
- **TTypes.hs** - Phantom-typed term-encoded types
- **ShorthandTypes.hs** - Convenient aliases (`tInt32`, `tString`, `tList`)

#### High-Level Construction
- **Core.hs** - High-level constructors for core concepts
- **Mantle.hs** - Metadata variants and introspection
- **Bootstrap.hs** - Bootstrapping utilities

#### Structure Definition
- **Module.hs** - Module, binding, namespace definition
- **Grammars.hs** - Grammar and syntax definitions
- **Annotations.hs** - Annotation handling

#### Utilities
- **Literals.hs**, **LiteralTypes.hs** - Literal handling
- **Accessors.hs** - Path and accessor operations
- **Graph.hs** - Graph construction
- **Coders.hs** - Encoder/decoder definitions
- **Compute.hs** - Computation and flow handling
- **Testing.hs** - Test utilities
- **Topology.hs** - Topological operations
- **Ast.hs**, **Tabular.hs** - Data format handling

### Library DSLs

Phantom-typed wrappers for standard library functions:

```
Hydra/Dsl/Meta/Lib/
├── Lists.hs       # map, filter, fold, concat, etc.
├── Maps.hs        # lookup, insert, keys, values, etc.
├── Sets.hs        # union, intersection, member, etc.
├── Strings.hs     # concat, split, toUpper, toLower, etc.
├── Chars.hs       # isAlpha, isDigit, toUpper, toLower
├── Math.hs        # add, sub, mul, div, sin, cos, sqrt, etc.
├── Logic.hs       # and, or, not, ifElse
├── Maybes.hs      # fromMaybe, maybe, isJust, etc.
├── Eithers.hs     # either, isLeft, rights, etc.
├── Equality.hs    # equal, compare, gt, lt, etc.
├── Pairs.hs       # fst, snd, curry, uncurry
├── Flows.hs       # bind, map, pure, sequence, etc.
└── Literals.hs    # Type conversions and parsing
```

### DSL operators

The DSL provides convenient operators for readable code:

```haskell
-- Type construction
(-->) :: Type -> Type -> Type          -- Function type
(@@) :: Type -> Type -> Type           -- Type application

-- Term construction
(<.>) :: Term -> Term -> Term          -- Function composition
(@@) :: Term -> Term -> Term           -- Function application
(>:) :: String -> a -> Field           -- Field definition

-- Phantom-typed construction
(~>) :: String -> TTerm a -> TTerm (x -> b)     -- Lambda
(<~) :: String -> TTerm a -> TTerm b -> TTerm b  -- Let binding
(<<~) :: String -> TTerm (Flow s a) -> TTerm (Flow s b) -> TTerm (Flow s b)  -- Flow bind

-- Examples
intToString = int32 --> string                -- Type
addOne = lambda "x" (var "x" <.> int32 1)    -- Term
person = record "Person" [
  "name" >: string,
  "age" >: int32
]
```

### DSL usage example

Here's a complete example showing DSL usage in type inference:

```haskell
-- From Hydra.Sources.Kernel.Terms.Inference
inferTypeOfEitherDef :: TBinding (InferenceContext -> Either Term Term -> Flow s InferenceResult)
inferTypeOfEitherDef = define "inferTypeOfEither" $
  doc "Infer the type of an Either term" $
  "cx" ~> "e" ~>

  -- Pattern match on left or right
  Eithers.either_
    -- Left case
    ("left" ~>
      "leftResult" <<~ ref inferTypeDef @@ var "cx" @@ var "left" $
      "type_" <~ InferenceResult.type_ (var "leftResult") $
      "cx2" <~ InferenceResult.context (var "leftResult") $
      produce $ InferenceResult.inferenceResult (var "cx2")
        (Types.either_ (var "type_") (var "any")))

    -- Right case
    ("right" ~>
      "rightResult" <<~ ref inferTypeDef @@ var "cx" @@ var "right" $
      "type_" <~ InferenceResult.type_ (var "rightResult") $
      "cx2" <~ InferenceResult.context (var "rightResult") $
      produce $ InferenceResult.inferenceResult (var "cx2")
        (Types.either_ (var "any") (var "type_")))

    (var "e")
```

**Features Demonstrated:**
- `define` - Define a named function
- `~>` - Lambda abstraction
- `<~` - Let binding
- `<<~` - Flow monad binding
- `@@` - Function application
- `ref` - Reference to another definition
- Type-safe operations on `InferenceResult` and `Either`

### Relationship to core language

```
User Code (Python/Java/Haskell)
         ↓ (serialized as Core.Term)
Hydra Core Language (Type, Term, Function, Lambda, etc.)
         ↓ (defined via DSLs)
Hydra DSLs in Haskell (Terms.hs, Types.hs, Phantoms.hs, etc.)
         ↓ (generates code for)
Generated Source Code (Haskell, Python, Java)
```

**Self-Hosting Loop:**
1. Write inference logic in Phantom DSL → `Sources/Kernel/Terms/Inference.hs`
2. DSL produces Term/Type values representing functions
3. Code generator converts to executable Haskell → `gen-main/haskell/Hydra/Inference.hs`
4. Generated code can now infer types for new Hydra code (including DSL code itself!)

---

## Primitive functions

Primitive functions are the standard library of Hydra, providing built-in operations for common data manipulations.

### Organization

Primitives are organized into **13 library modules** by category:

| Library | Count | Examples |
|---------|-------|----------|
| **hydra.lib.chars** | 6 | `isAlphaNum`, `isLower`, `toUpper` |
| **hydra.lib.equality** | 9 | `equal`, `compare`, `gt`, `lt`, `max` |
| **hydra.lib.eithers** | 8 | `either`, `isLeft`, `rights` (NEW) |
| **hydra.lib.flows** | 12 | `apply`, `bind`, `map`, `sequence` |
| **hydra.lib.lists** | 34 | `map`, `filter`, `fold`, `concat`, `sort` |
| **hydra.lib.literals** | 43 | Type conversions, parsing, showing |
| **hydra.lib.logic** | 4 | `and`, `or`, `not`, `ifElse` |
| **hydra.lib.maps** | 19 | `lookup`, `insert`, `keys`, `toList` |
| **hydra.lib.math** | 37 | `add`, `mul`, `sin`, `sqrt`, `abs` |
| **hydra.lib.maybes** | 13 | `fromMaybe`, `maybe`, `isJust` |
| **hydra.lib.sets** | 14 | `union`, `intersection`, `member` |
| **hydra.lib.strings** | 13 | `concat`, `split`, `length`, `lines` |
| **hydra.lib.tuples** | 4 | `fst`, `snd`, `curry`, `uncurry` |

**Total: ~180+ primitive functions**

### Three-level definition structure

Each primitive is defined at three levels:

#### Level 1: Core Type Definition

From `Graph.hs`:
```haskell
def "Primitive" $
  record [
    "name">: doc "Unique name of the primitive" $
      core "Name",
    "type">: doc "Type signature" $
      core "TypeScheme",
    "implementation">: doc "Concrete implementation" $
      function (list $ core "Term") (compute "Flow" @@ graph "Graph" @@ core "Term")
  ]
```

#### Level 2: Haskell Implementation

Native Haskell implementations in `hydra-haskell/src/main/haskell/Hydra/Lib/`:

```haskell
-- Math.hs
add :: Num a => a -> a -> a
add x y = x + y

sqrt :: Double -> Double
sqrt = Prelude.sqrt

-- Strings.hs
cat :: [String] -> String
cat = L.concat

toUpper :: String -> String
toUpper = fmap C.toUpper

-- Lists.hs
map :: (a -> b) -> [a] -> [b]
map = fmap

length :: [a] -> Int
length = L.length
```

#### Level 3: Primitive Registration

In `Sources/Libraries.hs`, primitives are wrapped with metadata using DSL helpers:

```haskell
-- Unary primitive
prim1 name function typeVars inputCoder outputCoder

-- Binary primitive
prim2 name function typeVars input1Coder input2Coder outputCoder

-- Ternary primitive
prim3 name function typeVars input1Coder input2Coder input3Coder outputCoder

-- Constant (nullary)
prim0 name value typeVars outputCoder
```

**Example: Lists.map**
```haskell
prim2Interp _lists_map (Just mapInterp) ["x", "y"]
  (function x y) (list x) (list y)
  where
    x = variable "x"
    y = variable "y"

    mapInterp :: Term -> Term -> Flow Graph Term
    mapInterp fun args' = do
      args <- ExtractCore.list args'
      return $ Terms.list (Terms.apply fun <$> args)
```

### TermCoder system

The `Hydra.Dsl.Prims` module provides type coding:

```haskell
-- Primitive types
int32, int64 :: TermCoder Int
float32, float64 :: TermCoder Double
bigint :: TermCoder Integer
string :: TermCoder String
boolean :: TermCoder Bool
binary :: TermCoder ByteString

-- Container types
list :: TermCoder a -> TermCoder [a]
set :: TermCoder a -> TermCoder (Set a)
optional :: TermCoder a -> TermCoder (Maybe a)
map :: TermCoder k -> TermCoder v -> TermCoder (Map k v)
tuple2 :: TermCoder a -> TermCoder b -> TermCoder (a, b)

-- Function types
function :: TermCoder a -> TermCoder b -> TermCoder (a -> b)

-- Special types
either_ :: TermCoder a -> TermCoder b -> TermCoder (Either a b)
flow :: TermCoder s -> TermCoder a -> TermCoder (Flow s a)
```

Each TermCoder contains:
1. Type representation
2. Encoder: Haskell value → Hydra Term
3. Decoder: Hydra Term → Haskell value

### Multi-language generation

Primitives defined once in Haskell generate implementations in multiple languages:

#### Java Generation

Location: `hydra-java/src/main/java/hydra/lib/`

Each primitive becomes a class extending `PrimitiveFunction`:

```java
// hydra/lib/math/Add.java
public class Add extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.math.add");
    }

    public TypeScheme type() {
        return scheme(function(int32(), int32(), int32()));
    }

    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> map2(
            Expect.int32(args.get(0)),
            Expect.int32(args.get(1)),
            (arg0, arg1) -> Terms.int32(apply(arg0, arg1))
        );
    }

    public static Integer apply(Integer augend, Integer addend) {
        return augend + addend;
    }
}
```

#### Python Generation

Location: `hydra-python/src/main/python/hydra/lib/`

Pure Python implementations:

```python
# hydra/lib/math.py
def add(x: int, y: int) -> int:
    """Add two integers."""
    return x + y

def sqrt(x: float) -> float:
    """Square root of a float."""
    return math.sqrt(x)

# hydra/lib/lists.py
def map_(f: Callable[[A], B], xs: frozenlist[A]) -> frozenlist[B]:
    """Map a function over a list."""
    return tuple(f(x) for x in xs)
```

### Key design patterns

#### Pattern 1: Type Polymorphism

```haskell
prim2 _equality_equal Equality.equal ["x"] x x boolean
  where x = variable "x"
```

The same primitive works with any type supporting equality.

#### Pattern 2: Interpreted vs. Compiled Forms

- **Interpreted**: Can be evaluated directly within Hydra (provided as `Just interpreter`)
- **Compiled**: Only work in compiled code (marked with `Nothing`)

```haskell
-- Has interpreted form
prim2Interp _lists_map (Just mapInterp) ...

-- Compiled only
prim1 _strings_toUpper Strings.toUpper [] string string
```

#### Pattern 3: Flow Monads for Error Handling

All primitives operate within a `Flow` monad:

```haskell
type Flow s a = s -> Trace -> FlowState s a

data FlowState s a = FlowState {
  flowStateValue :: Maybe a,      -- Success or failure
  flowStateState :: s,            -- Threaded state
  flowStateTrace :: Trace         -- Execution trace
}
```

This provides:
- Optional error handling
- State threading (Graph context)
- Logging via trace

---

## Cross-language compilation (coders)

Coders enable cross-compilation of Hydra programs between different language implementations.
They transform Hydra modules (types and terms) from one language's representation to another, allowing developers
to write Hydra code in their preferred language and compile it to any other supported language.

**See also:**
- [Property Graphs](https://github.com/CategoricalData/hydra/wiki/Property-graphs) - Mapping Hydra schemas to property graphs with annotations
- [Testing](https://github.com/CategoricalData/hydra/wiki/Testing) - How the common test suite validates coder parity

### Coder locations

```
hydra-ext/src/main/haskell/Hydra/Ext/Staging/
├── Java/           # Full OOP with generics
├── Python/         # Dynamic with dataclasses
├── Cpp/            # Systems language with templates
├── Csharp/         # Modern .NET
├── GraphQL/        # Schema definition language
├── Avro/           # Data serialization
├── Protobuf/       # Protocol Buffers
├── Pegasus/        # LinkedIn's data format
├── JsonSchema/     # JSON schemas
├── Graphviz/       # Visualization
├── Pg/             # PostgreSQL with GraphSON
├── Rdf/            # RDF and SHACL
└── Tinkerpop/      # Graph databases
```

**Total: 14+ language/format targets**

### Common coder structure

Each language directory typically contains:

```
Language/
├── Coder.hs        # Main transformation logic
├── Serde.hs        # AST to text serialization
├── Language.hs     # Language definition and constraints
├── Names.hs        # Name conversion and case conventions
├── Utils.hs        # Language-specific utilities
└── Settings.hs     # Configuration (optional)
```

### Entry point pattern

All coders follow the same signature:

```haskell
moduleToLanguage :: Module -> Flow Graph (M.Map FilePath String)
```

Examples:
```haskell
moduleToJava :: Module -> Flow Graph (M.Map FilePath String)
moduleToPython :: Module -> [Definition] -> Flow Graph (M.Map FilePath String)
moduleToCpp :: Module -> Flow Graph (M.Map FilePath String)
```

### Coder framework

Located in `hydra-haskell/src/gen-main/haskell/Hydra/Coders.hs`:

```haskell
-- Bidirectional transformation
data Coder s t v = Coder {
  coderEncode :: v -> Flow s t,
  coderDecode :: v -> Flow s t
}

-- Adapter for language-specific transformations
data Adapter s t u w v x = Adapter {
  adapterIsLossy :: Bool,              -- Track lossy conversions
  adapterSource :: u,                  -- Source language/type
  adapterTarget :: w,                  -- Target language/type
  adapterCoder :: Coder s t v          -- Transformation logic
}
```

### Encoding process

#### Step 1: Term to Language

Terms are recursively converted to target language expressions:

```haskell
-- Java example
encodeTerm :: Aliases -> Term -> Flow Graph Java.Expression

-- Handles:
-- - Literals (int, string, boolean, etc.)
-- - Applications (function calls)
-- - Functions (lambdas or method references)
-- - Records (class constructors)
-- - Unions (abstract class with visitors)
-- - Variables (local variables or fields)
-- - Let bindings (variable declarations)
-- - Case expressions (visitor pattern)
```

#### Step 2: Type Encoding

Hydra types map to language types:

```haskell
-- Java example
encodeType :: Aliases -> Type -> Flow Graph Java.Type

-- Maps:
-- TypeRecord → Java Class
-- TypeUnion → Abstract class with subclasses
-- TypeLambda → Generic type parameter
-- TypeForall → Java generics with bounds
-- TypeFunction → Java functional interfaces
-- TypeList → List<T>
-- TypeMap → Map<K, V>
-- TypeOptional → Optional<T>
```

#### Step 3: Module Generation

Complete module transformation:

```haskell
-- Java example from Java/Coder.hs
moduleToJava :: Module -> Flow Graph (M.Map FilePath String)
moduleToJava mod = do
  -- Extract types from module
  types <- getTypes mod

  -- Generate class for each type
  classes <- mapM (typeToJavaClass mod) types

  -- Generate package structure
  let packagePath = namespaceToPath (moduleNamespace mod)

  -- Map file paths to source code
  return $ M.fromList $ map (\cls ->
    (packagePath </> className cls <.> "java",
     renderJavaClass cls)) classes
```

### The adapter framework

Adapters handle type compatibility between languages:

```haskell
-- Core adapter functions
languageAdapter :: Language -> Type
                -> Flow Graph (Adapter... Type Type Term Term)

adaptTypeToLanguage :: Language -> Type -> Flow Graph Type

termAdapter :: Type
           -> Flow AdapterContext (Adapter... FieldType FieldType Field Field)
```

**Adapter Composition:**
```haskell
composeCoders :: Coder t0 t1 t2 t3 -> Coder t0 t1 t3 t4
              -> Coder t0 t1 t2 t4

constructCoder :: Language -> (Term -> Flow t0 t1) -> Type
              -> Flow Graph (Coder t0 t2 Term t1)
```

**Module Transformation Pipeline:**
```haskell
transformModule :: Language
               -> (Term -> Flow t0 t1)              -- encoder
               -> (Module -> M.Map Type (Coder...) -> [(Binding, TypeApplicationTerm)]
                   -> Flow Graph t3)                -- constructor
               -> Module
               -> Flow Graph t3

-- Process:
-- 1. Extract all elements as TypeApplicationTerms
-- 2. Gather unique types
-- 3. Construct coders for each type (via adapters)
-- 4. Pass coders to module constructor
-- 5. Generate output files
```

### Language constraints

Each language defines its capabilities:

```haskell
data Language = Language {
  languageName :: LanguageName,
  languageConstraints :: LanguageConstraints
}

data LanguageConstraints = LanguageConstraints {
  languageConstraintsEliminationVariants :: S.Set EliminationVariant,
  languageConstraintsLiteralVariants :: S.Set LiteralVariant,
  languageConstraintsFloatTypes :: S.Set FloatType,
  languageConstraintsFunctionVariants :: S.Set FunctionVariant,
  languageConstraintsIntegerTypes :: S.Set IntegerType,
  languageConstraintsTermVariants :: S.Set TermVariant,
  languageConstraintsTypeVariants :: S.Set TypeVariant,
  languageConstraintsTypes :: Type -> Bool  -- Custom type predicate
}
```

### Language-specific patterns

#### Java Coder

Key features:
- Generic type parameter handling
- Visitor pattern for union elimination
- Serialization support (JSON/Avro)
- Let-binding flattening with recursive variable detection
- Symbol classification (constant, nullary, unary, local variable)

```haskell
-- Java/Coder.hs (line 715-723)
TermUnion (Injection name (Field (Name fname) v)) -> do
  let (Java.Identifier typeId) = nameToJavaName aliases name
  let consId = Java.Identifier $ typeId ++ "." ++ sanitizeJavaName (capitalize fname)
  args <- if EncodeCore.isUnitTerm v
    then return []
    else do
      ex <- encode v
      return [ex]
  return $ javaConstructorCall (javaConstructorName consId Nothing) args Nothing
```

#### Python Coder

Key features:
- Metadata gathering for imports
- Type variable tracking
- Case statement deduplication
- Walrus operator for let-bindings (Python 3.8+)
- Inline type parameters (Python 3.12+)
- Automatic casting for polymorphic values

Recent fix for Issue #206:
```haskell
-- Python/Coder.hs (lines 556-574)
TermUnion (Injection tname field) -> do
  rt <- inGraphContext $ requireUnionType tname
  if isEnumRowType rt
    then return $ projectFromExpression (pyNameToPyExpression $ encodeNameQualified env tname)
      $ encodeEnumValue env $ fieldName field
    else do
      -- Omit argument for unit-valued variants (resolves #206)
      args <- if EncodeCore.isUnitTerm (fieldTerm field)
        then return []
        else do
          parg <- encode $ fieldTerm field
          return [parg]

      -- Explicitly casting to the union type avoids occasional Python type errors...
      updateMeta $ \m -> m { pythonModuleMetadataUsesCast = True }
      return $ castTo (typeVariableReference env tname) $
        functionCall (pyNameToPyPrimary $ variantName True env tname (fieldName field)) args
```

### Serialization (Serde) layer

The `Serde.hs` files bridge language AST to formatted source code:

**Java Serde** (~600+ lines)
- Java AST → formatted Java source
- Comment preservation
- Import organization

**Python Serde** (~400+ lines)
- Python AST → formatted Python source
- Indentation and block structure
- Quote styles and escaping

---

## The bootstrap process

Hydra is **self-hosting**: it defines its own type system and can regenerate itself.

### Module structure

Hydra's source modules are divided into **type modules** and **term modules**.
Type modules define data models — the types that make up Hydra's internal representation.
Term modules provide the logic and procedural aspect — the functions that operate on those types.
This distinction applies throughout, not just to the kernel.

The modules compiled in hydra-haskell are aggregated in `Hydra.Sources.All`:

- **Kernel type modules** (`kernelTypesModules`) — Hydra's internal data model:
  the core type system (`hydra.core`), the computation model (`hydra.compute`),
  graph and module structures (`hydra.graph`, `hydra.module`), and supporting types
  like `hydra.typing`, `hydra.query`, `hydra.tabular`, etc.
  Hand-written DSL definitions in `Hydra.Sources.Kernel.Types.*`.

- **Kernel term modules** (`kernelTermsModules`) — The logic of Hydra:
  type inference, type checking, term reduction, rewriting, code generation, etc.
  Hand-written DSL definitions in `Hydra.Sources.Kernel.Terms.*`.
  Also includes the encoder/decoder source modules (see below).

- **Haskell modules** (`haskellModules`) — Both type modules (the Haskell AST model)
  and term modules (the Haskell coder, serializer, and utilities). These are specific
  to hydra-haskell and enable Haskell code generation.

- **JSON modules** (`jsonModules`) — The JSON data model (type module) along with the
  JSON coder, parser, and writer (term modules).

- **Other modules** (`otherModules`) — Currently the YAML model and coder utilities.

- **Test modules** (`testModules`) — The common test suite, compiled into each target
  language as part of the sync process. Defined separately from `mainModules`.

**Encoder/decoder source modules** are a special category of term modules that are
*generated from* the type modules rather than hand-written. For each kernel type module
(e.g., `hydra.core`), a pair of modules is generated that can encode objects of that type
as Hydra Terms and decode them from Terms. These live in `Hydra.Sources.{Encode,Decode}.*`
and are included in `kernelTermsModules` alongside the hand-written term modules.

The full set is composed as:

```haskell
mainModules   = kernelModules ++ haskellModules ++ jsonModules ++ otherModules
kernelModules = kernelTypesModules ++ kernelTermsModules ++ jsonModules

kernelTermsModules = kernelPrimaryTermsModules   -- hand-written logic modules
                  ++ kernelDecodingModules        -- generated from type modules
                  ++ kernelEncodingModules         -- generated from type modules
```

### The sync pipeline

All modules in `mainModules` — regardless of category — go through the same code generation
pipeline: `writeHaskell` (or `writeJava`, `writePython`) compiles them from Hydra module
definitions into executable code in the target language.

The encoder/decoder source modules require a special staging step because they are *derived*
from the type modules rather than hand-written. The sync script (`sync-haskell.sh`) handles
this with an initial generation pass, followed by a source module generation step, followed
by a second generation pass:

| Phase | What it does |
|-------|--------------|
| 1 | Compile `mainModules` into executable Haskell (initial pass) |
| 2–3 | Generate kernel tests and eval lib |
| 4 | Generate encoder/decoder source modules from `kernelTypesModules` |
| 5 | Recompile `mainModules` into executable Haskell (picking up the new source modules) |
| 6–7 | Generate generation tests; export and verify JSON kernel |
| 8 | Run tests |

Phase 5 is necessary because the encoder/decoder source modules generated in phase 4 are
part of `kernelTermsModules` and therefore `mainModules`. They need to be compiled into
executable code just like every other module. A `stack build` between phases 4 and 5
ensures the Haskell compiler picks up the newly generated source files.

### Key generation functions (from `Hydra.Generation`)

- `writeHaskell` / `writeJava` / `writePython` — Compile Hydra modules into executable
  code in the target language. Signature: `FilePath -> [Module] -> [Module] -> IO ()`
  (output directory, universe modules for resolution, modules to generate).
- `writeDecoderSourceHaskell` / `writeEncoderSourceHaskell` — Generate encoder/decoder
  source modules (Hydra module definitions) from type modules. Used in phase 4.
- `writeDecoderHaskell` / `writeEncoderHaskell` — Convenience functions that generate
  encoder/decoder modules and immediately compile them to executable Haskell in one step.

For detailed context on encoder/decoder modules, see [Issue #47: Per-Type Term Coders](https://github.com/CategoricalData/hydra/blob/main/docs/work/issues/issue-47-per-type-term-coders.md).

### The bootstrap challenge

```
DSL defines Hydra      → Generates code for Hydra
        ↓                         ↓
But generator needs            Code generation
to understand DSL             requires understanding
                              the new DSL constructs!
                              CIRCULAR DEPENDENCY!
```

### Bootstrap solution: Gradual extension

When adding new features (like `Either` type):

#### Step 1: Define in DSL

Add to core types in `Core.hs`:
```haskell
def "Term" $
  union [
    -- ... existing variants
    "either">: Types.either_ (core "Term") (core "Term"),
    -- ...
  ]
```

Add DSL operations in `Phantoms.hs`:
```haskell
either_ :: TTerm (a -> c) -> TTerm (b -> c) -> TTerm (Either a b) -> TTerm c
```

#### Step 2: Build (Will Fail)

```bash
stack build
# Error: Generator doesn't understand 'either' yet
```

#### Step 3: Manual Patch

Hand-translate DSL definitions to Haskell in generated files:

```haskell
-- Manually edit: src/gen-main/haskell/Hydra/Inference.hs
inferTypeOfEither :: InferenceContext -> Either Term Term -> Flow s InferenceResult
inferTypeOfEither cx (Left left) = do
  leftResult <- inferType cx left
  let leftType = inferenceResultType leftResult
  let cx2 = inferenceResultContext leftResult
  return $ InferenceResult cx2 (TypeUnion [leftType, typeAny])
inferTypeOfEither cx (Right right) = do
  rightResult <- inferType cx right
  let rightType = inferenceResultType rightResult
  let cx2 = inferenceResultContext rightResult
  return $ InferenceResult cx2 (TypeUnion [typeAny, rightType])
```

#### Step 4: Rebuild

```bash
stack build
# Success! Generator now understands Either
```

#### Step 5: Regenerate

```bash
stack run hydra-ext:exe:hydra-ext-debug
# Cleanly generates all files including new Either support
```

#### Step 6: Final Build

```bash
stack build
# Self-hosting loop complete!
```

### Generated code structure

```
hydra-haskell/
├── src/main/haskell/Hydra/
│   ├── Dsl/                    # DSL definitions (manual)
│   ├── Sources/                # DSL-based specifications (manual)
│   └── Lib/                    # Native implementations (manual)
│
└── src/gen-main/haskell/       # Generated code
    ├── Hydra/
    │   ├── Core.hs             # Generated Core types
    │   ├── Mantle.hs           # Generated Mantle types
    │   ├── Inference.hs        # Generated type inference
    │   ├── Checking.hs         # Generated type checking
    │   └── ...                 # All kernel modules
```

---

## Extending Hydra

Hydra's modular architecture provides clear extension points for adding new functionality.
For detailed step-by-step guides, see the [Developer Recipes](https://github.com/CategoricalData/hydra/blob/main/docs/src/recipes/index.md).

### Key extension points

**Primitive functions**: Add new standard library functions by defining native implementations in Haskell,
registering them in `Sources/Libraries.hs`, and regenerating code for all target languages.
See the [Adding primitives recipe](https://github.com/CategoricalData/hydra/blob/main/docs/src/recipes/adding-primitives.md).

**Core types**: Extend the kernel type system by adding new type definitions to `Core.hs`, updating DSL constructors,
and following the bootstrap process to regenerate the system.
See the [Extending Hydra Core recipe](https://github.com/CategoricalData/hydra/blob/main/docs/src/recipes/extending-hydra-core.md).

**Target languages**: Add support for new programming languages by implementing a coder (term/type encoding),
serializer (AST to text), and language constraint definitions in `hydra-ext/src/main/haskell/Hydra/Ext/Staging/`.

**Standard libraries**: Create new library modules by defining types in `Sources/Kernel/Types/`,
implementing native functions in `Lib/`, registering primitives, and creating DSL wrappers.

---

## Appendix: Key file locations

### Type modules

[`hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Types/`](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Types)
```
├── Core.hs              # hydra.core - foundation
├── Mantle.hs            # hydra.mantle - metadata
├── Compute.hs           # hydra.compute - Flow monad
├── Graph.hs             # hydra.graph - primitives
├── Module.hs            # hydra.module - namespaces
└── ...                  # 15 more modules
```

### DSL system

[`hydra-haskell/src/main/haskell/Hydra/Dsl/`](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Dsl)
```
├── Terms.hs             # Untyped term DSL
├── Types.hs             # Untyped type DSL
├── Phantoms.hs          # Phantom-typed DSL
├── TTerms.hs            # Term-encoded terms
├── Core.hs              # High-level constructors
├── Bootstrap.hs         # Bootstrapping utilities
└── Lib/                 # Library DSLs
    ├── Lists.hs
    ├── Eithers.hs
    └── ...
```

### Primitive functions

[`hydra-haskell/src/main/haskell/Hydra/Lib/`](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/main/haskell/Hydra/Lib) — Native implementations
[`hydra-haskell/src/main/haskell/Hydra/Sources/Libraries.hs`](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Libraries.hs) — Primitive registration
```
├── Math.hs
├── Lists.hs
└── ...
```

### Code generators

[`hydra-ext/src/main/haskell/Hydra/Ext/Staging/`](https://github.com/CategoricalData/hydra/tree/main/hydra-ext/src/main/haskell/Hydra/Ext/Staging)
```
├── Java/               # Java coder
├── Python/             # Python coder
└── ...                 # 11+ more languages
```

### Generated code

[`hydra-haskell/src/gen-main/haskell/`](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell/src/gen-main/haskell) — Generated Haskell
[`hydra-java/src/main/java/`](https://github.com/CategoricalData/hydra/tree/main/hydra-java/src/main/java) — Generated Java
[`hydra-python/src/gen-main/python/`](https://github.com/CategoricalData/hydra/tree/main/hydra-python/src/gen-main/python) — Generated Python

---

## Summary

Hydra's implementation demonstrates a sophisticated multi-layer architecture:

1. **Type modules** define the core type system in a modular, dependency-aware manner
2. **DSLs** provide multiple levels of abstraction for writing Hydra code with compile-time safety
3. **Primitives** offer a comprehensive standard library with multi-language generation
4. **Coders** transform Hydra definitions into multiple target languages systematically
5. **Bootstrap process** enables self-hosting and gradual extension of the language

This architecture enables:
- Type-safe code generation across languages
- Self-modifying compiler capabilities
- Systematic addition of new features
- Clear separation of concerns
- Maintainable and extensible codebase

The combination of Haskell's type system, phantom types, and careful layering creates a robust foundation for
a multi-language transformation framework.

---

## Appendix: Build scripts and executables

Hydra uses a combination of shell script wrappers (in `bin/` directories) and Stack executables
for code generation and synchronization. The main sync scripts orchestrate the individual executables
in the correct order; the individual scripts and executables are useful during development when you
need to rerun a single phase.

For how these fit into the release workflow, see [Hydra release process](https://github.com/CategoricalData/hydra/wiki/Release-process).

### Top-level (`bin/`)

| Script | Purpose |
|--------|---------|
| `sync-all.sh` | **Full sync.** Run all sync scripts in order (Haskell -> Ext -> Java -> Python). Supports `--quick`. |
| `verify-release.sh` | Cross-implementation pre-release verification |
| `update-javadoc.sh` | Regenerate JavaDoc HTML for `hydra-java` and `hydra-ext` |

### Haskell (`hydra-haskell/`)

Shell script wrappers live in `hydra-haskell/bin/`. Executables without shell wrappers are run via `stack exec <name>`.

| Script / Executable | Purpose |
|---------------------|---------|
| `bin/sync-haskell.sh` | **Main sync script.** Regenerate all Haskell artifacts in the correct order and optionally run tests. Supports `--quick`. |
| `bin/update-generation-tests.sh` | Regenerate generation test files |
| `bin/update-kernel-tests.sh` | Regenerate kernel test files |
| `bin/update-json-kernel.sh` | Export the kernel to JSON |
| `bin/update-json-main.sh` | Export main (non-kernel) modules to JSON |
| `bin/update-json-test.sh` | Export test modules to JSON |
| `bin/verify-json-kernel.sh` | Verify JSON kernel round-trips correctly |
| `update-haskell-kernel` | Regenerate Haskell kernel modules (executable only, called by `sync-haskell.sh`) |
| `update-haskell-eval-lib` | Regenerate Haskell eval lib modules (executable only, called by `sync-haskell.sh`) |
| `update-haskell-sources` | Regenerate Haskell encoder/decoder source modules (executable only, called by `sync-haskell.sh`) |

### Ext, Java, and Python (`hydra-ext/`)

Shell script wrappers live in `hydra-ext/bin/`. Executables without shell wrappers are run via `stack exec <name>`.

| Script / Executable | Purpose |
|---------------------|---------|
| `bin/sync-ext.sh` | **Ext sync script.** Regenerate ext Haskell modules and JSON exports. |
| `bin/sync-haskell.sh` | **Haskell sync script (from JSON).** Regenerate Haskell kernel tests and generation tests from JSON. Supports `--quick`. |
| `bin/sync-java.sh` | **Main Java sync script.** Regenerate all Java artifacts, compile, and optionally run tests. Supports `--quick`. |
| `bin/sync-python.sh` | **Main Python sync script.** Regenerate all Python artifacts and optionally run tests. Supports `--quick`. |
| `update-haskell-ext-main` | Regenerate ext Haskell gen-main modules (executable only, called by `sync-ext.sh`) |
| `update-json-ext` | Export ext modules to JSON (executable only, called by `sync-ext.sh`) |
| `bootstrap-from-json` | Bootstrap Hydra implementations from JSON module exports (executable only, called by all sync scripts) |
