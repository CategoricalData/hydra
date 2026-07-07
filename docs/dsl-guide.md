# Hydra DSL guide (Haskell)

This guide explains Hydra's domain-specific languages (DSLs) for constructing types and terms in Haskell.

**Note**: Hydra provides DSLs in each of its implementation languages; generated DSL modules are
emitted for every target, and hand-written authoring DSLs exist for Haskell, Java, Python, and Scala.
This guide focuses on the Haskell DSLs.
Haskell is Hydra's bootstrapping language—the Hydra kernel itself is written in Haskell—so this guide is
particularly intended for Hydra developers working on the kernel or extending Hydra's core functionality.
For host-native DSL usage, see:
- [DSL Guide (Java)](dsl-guide-java.md) - Working with Hydra types and terms in Java
- [DSL Guide (Python)](dsl-guide-python.md) - Working with Hydra types and terms in Python
- [DSL Guide (Scala)](dsl-guide-scala.md) - Working with Hydra types and terms in Scala

## Prerequisites

**Before using the DSLs**, you should:
- Understand Hydra's core concepts: [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts)
- Know basic Haskell syntax (imports, functions, operators)
- Have built Hydra locally (see main [README](https://github.com/CategoricalData/hydra))

**This guide is for:**
- Writing Hydra kernel code (extending the type system or adding primitives)
- Creating language coders (e.g., in packages/hydra-pg, packages/hydra-rdf, packages/hydra-ext)
- Defining custom data models

## Table of contents

1. [Introduction](#introduction)
2. [Quick start](#quick-start)
3. [The five DSL variants](#the-five-dsl-variants)
4. [When to use each variant](#when-to-use-each-variant)
5. [What belongs under `Sources/`](#what-belongs-under-sources)
6. [Direct DSLs (untyped)](#direct-dsls-untyped)
7. [Phantom-typed DSL](#phantom-typed-dsl)
8. [Meta DSLs](#meta-dsls)
9. [Operator reference](#operator-reference)
10. [Common patterns](#common-patterns)
11. [Working with types](#working-with-types)
12. [Working with terms](#working-with-terms)
13. [Error handling with Either](#error-handling-with-either)
14. [Primitive functions](#primitive-functions)
15. [Troubleshooting](#troubleshooting)
16. [Advanced topics](#advanced-topics)

## Introduction

Hydra provides DSLs in each of its implementation languages for working with its core data
structures (types and terms).
The Haskell DSLs, described in this guide, make it easier to write Hydra programs by providing:

- Concise syntax for common operations
- Operator-based notation reducing boilerplate
- Type safety (in some variants)
- Integration with Haskell's type system

The Java and Python DSLs provide similar functionality tailored to their respective language idioms.

### Why multiple DSLs?

Different use cases require different trade-offs:

- **Defining types**: Use the direct Types DSL to construct `Type` instances
- **Defining terms with type safety**: Use the phantom-typed DSL for compile-time type composition checking
- **Building terms or types programmatically**: Use the meta DSLs to write programs that construct Hydra objects
- **Runtime manipulation**: Use the generated code directly (rare)

### Why phantom-typed DSLs

Phantom types let DSL builders encode the intended type of each
constructed term or type into the host's standard type system. In
statically typed hosts (Haskell, Java) the host's checker catches DSL
mistakes at compile time. In Python, the same annotations are read by
external static checkers (mypy, pyright) and by IDEs/readers, and a small
set of runtime checks at DSL-construction time backs them up.

Alternatives all forfeit one of the load-bearing properties: pure runtime
checks defer errors past authoring time, and a separately-checked external
DSL would require its own parser and checker in every host.

## Quick start

Here are examples showing the basics. Note that type and term modules are typically separate.

### Example 1: Constructing a type

```haskell
-- Type module (kernel style)
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations (doc)
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Overlay.Haskell.Dsl.Types as T

-- Define a record type definition
person :: TypeDefinition
person = define "Person" $
  doc "A person with a name and age" $
  T.record [
    "name">: T.string,
    "age">: T.int32]

-- Define a function type using the ~> operator
greet :: TypeDefinition
greet = define "Greet" $
  person ~> T.string  -- Person -> String
```

**Note**: Type modules define `TypeDefinition` values using `define`. The operators `>:`, `@@`, and `~>`
are imported unqualified for cleaner syntax. Other type definitions can be referenced directly
(like `person` above) thanks to the `AsType` type class.

### Example 2: Constructing terms

```haskell
-- Term module
import Hydra.Overlay.Haskell.Dsl.Terms

-- Construct a record term (referencing the Person type by name)
arthur :: Term
arthur = record (Name "Person") [
  "name">: string "Arthur",
  "age">: int32 42]

-- Construct a function
greet :: Term
greet = lambda "person" $
  string "Hello, " ++
  primitive "concat" @@ project (Name "Person") (Name "name") (var "person")
```

**Note**: The `project` function takes two `Name`s: the type name and the field name.
In real modules defined with the meta DSLs, you use generated constants like `_Person` and `_Person_name`
instead of constructing `Name`s manually (see the Meta DSLs section below).

## The five DSL variants

Hydra has five DSL variants, each serving a specific purpose:

### 1. Direct DSLs

**Modules**: `Hydra.Overlay.Haskell.Dsl.Terms`, `Hydra.Overlay.Haskell.Dsl.Types`

**Purpose**: Direct construction of Hydra domain objects (like `Type` and `Term` instances)

**Example**:
```haskell
import Hydra.Overlay.Haskell.Dsl.Terms

myFunction :: Term
myFunction = lambda "x" (int32 42)
```

**When to use**: Constructing `Type` or `Term` instances that will be used directly by Hydra.
All kernel type modules use the direct Types DSL to construct `Type` instances.

### 2. Phantom-typed DSL

**Modules**: `Hydra.Overlay.Haskell.Dsl.Typed.Phantoms`

**Purpose**: Type-safe construction of terms with Haskell compile-time checking

**Example**:
```haskell
import Hydra.Overlay.Haskell.Dsl.Typed.Phantoms

-- Type signature enforces this is a function!
myFunction :: TypedTerm (a -> Int)
myFunction = lambda "x" (int32 42)
```

**When to use**: Constructing terms where you need type composition checking.
Kernel term modules use the phantom DSL because terms have types, and the phantom DSL ensures they compose correctly.
This isn't needed for types because we don't have "types of different types".

### 3. Meta DSLs

**Modules**: `Hydra.Overlay.Haskell.Dsl.Typed.Terms`, `Hydra.Overlay.Haskell.Dsl.Typed.Types`

**Purpose**: Specifying programs that build terms or types

**Basic example**:
```haskell
import Hydra.Overlay.Haskell.Dsl.Typed.Terms

-- Creates a Term that represents a lambda
myFunction :: TypedTerm Term
myFunction = lambda "x" (int32 42)
```

**More compelling example** (from `Hydra/Sources/Test`):
```haskell
import Hydra.Overlay.Haskell.Dsl.Typed.Terms

-- Build a test group (a Term) that contains test cases (also Terms)
-- Each test case has input and output Terms
-- This is a term that encodes other terms!
stringsCat :: TestGroup
stringsCat = TestGroup "cat" Nothing [] [
    primCase "basic" _strings_cat
      [list [string "one", string "two"]]  -- input term
      (string "onetwo")]                    -- expected output term

-- primCase builds a TestCase (Term) containing the input/output Terms
primCase :: String -> Name -> [Term] -> Term -> TestCaseWithMetadata
primCase name fname args output = TestCaseWithMetadata name tcase Nothing []
  where
    tcase = TestCaseEvaluation $ EvaluationTestCase EvaluationStyleEager input output
    input = foldl (@@) (primitive fname) args
```

**When to use**: Writing programs that construct Hydra terms or types as their output.
The key indicator is when you have **terms that encode other terms** - like test cases containing input/output terms,
or modules containing type definitions. See `Hydra/Sources/Test` for real examples.

### 4. Generated code

**Modules**: `Hydra.Core`, `Hydra.Graph`, etc.

**Purpose**: The actual runtime representation

**Example**:
```haskell
import Hydra.Core

myFunction :: Term
myFunction = TermFunction $ FunctionLambda $
  Lambda (Name "x") Nothing (TermLiteral $ LiteralInteger $ IntegerValueInt32 42)
```

**When to use**: Rarely - only when you need direct access to the AST

### 5. Generated DSL modules

Hydra generates a DSL surface for each of the three kinds of module definition: types, primitives,
and term-level functions (#467).
All are synthesized by the `hydra.dsls` module from the per-package JSON and regenerated into every
target language on sync; none are hand-written.
In Java they appear as static methods on interfaces under `hydra.dsl.*`; in Python, as functions in
`hydra.dsl.*` modules; the same pattern holds for the other targets.

#### Type-module DSLs

**Modules**: `Hydra.Dsl.Core`, `Hydra.Dsl.Coders`, `Hydra.Dsl.Ast`, etc. — one per type-defining
module.

**Purpose**: Auto-generated phantom-typed constructors, accessors, and updaters for all Hydra types.

**Example**:
```haskell
import qualified Hydra.Dsl.Core as Core

-- Record constructor (all fields as TypedTerm arguments)
myAnnotatedTerm :: TypedTerm AnnotatedTerm
myAnnotatedTerm = Core.annotatedTerm myBody myAnnotation

-- Field accessor
getBody :: TypedTerm AnnotatedTerm -> TypedTerm Term
getBody = Core.annotatedTermBody

-- Field updater (original, newValue -> updated)
withNewBody :: TypedTerm AnnotatedTerm -> TypedTerm Term -> TypedTerm AnnotatedTerm
withNewBody = Core.annotatedTermWithBody
```

**When to use**: When working with Hydra types in the phantom-typed DSL. These modules
provide the standard constructors and accessors. Hand-written wrapper modules under
`Hydra.Overlay.Haskell.Dsl.Typed.*` re-export these and add custom helpers; prefer importing
via the wrapper (e.g., `Hydra.Overlay.Haskell.Dsl.Typed.Core`) when one exists.

#### Primitive-module DSLs

**Modules**: `Hydra.Dsl.Lib.Lists`, `Hydra.Dsl.Lib.Maps`, `Hydra.Dsl.Lib.Strings`, etc. — one per
`hydra.lib.*` primitive library.

**Purpose**: One typed, arity-aware reference builder per primitive, derived from the primitive's
declared signature.
For example, `Hydra.Dsl.Lib.Lists.cons :: TypedTerm x -> TypedTerm [x] -> TypedTerm [x]` builds the
term `hydra.lib.lists.cons @@ x @@ xs`.
The wrappers are functions *of* phantom-typed terms, not phantom-typed function terms.

These generated modules replaced the hand-written wrappers (Haskell `Hydra.Dsl.Lib.*`, Java and
Python `hydra.dsl.meta.lib.*`) in 0.17.
The motivation (#467): the wrappers re-encoded information the system already has (each primitive's
name and signature), and generating them means a newly added primitive — or, in the future, a
user-defined one — gets its DSL surface in every language for free.
Two behavioral differences from the old hand-written wrappers: the generated wrappers carry the
primitive's type-class constraints (e.g. `Ord` for map and set keys), so polymorphic call sites may
need concrete type annotations, and they do not auto-lift definitions (`AsTerm` is Haskell-only
authoring sugar), so a definition passed as a first-class argument needs an explicit `asTerm`.

#### Term-module DSLs

**Modules**: `Hydra.Dsl.Strip`, `Hydra.Dsl.Serialization`, `Hydra.Dsl.Names`, etc. — one per curated
term-defining module.

**Purpose**: One rename-safe, typed reference per term definition, derived from the definition's
inferred signature.
For example, `Hydra.Dsl.Strip.deannotateType :: TypedTerm Type -> TypedTerm Type` builds a reference
to `hydra.strip.deannotateType`; polymorphic definitions carry their type parameters.

Haskell's in-repo authoring layer does *not* use these: a kernel definition is already a Haskell
symbol (`Strip.deannotateType` imported from `Hydra.Sources.Kernel.Terms.Strip` *is* the
definition), so the module system provides rename-safe references natively.
The term-module DSLs exist for the consumers that lack that free path:

- **Host-native DSL sources in other languages** (Java, Python, Scala, ...), where the only
  alternative is a stringly-typed `var("hydra.strip.deannotateType")` that no rename catches and
  that fails only at inference time.
- **Haskell applications authoring against the shipped `hydra-kernel`**, which ships the runtime and
  authoring helpers but not the `Hydra.Sources.*` layer.

Emission is demand-driven rather than exhaustive: each package curates the term modules that project
a DSL via its Manifest's `mainDslModules` (for the kernel, the `dslTermModules` list in
`Hydra.Sources.Kernel.Manifest`).
Add a module there to start generating references for it.

## When to use each variant

| Scenario | Recommended DSLs | Why |
|----------|----------------|-----|
| Defining types (e.g., kernel type modules) | Direct Types DSL | Direct construction of `Type` instances |
| Defining terms with type checking | Phantom-typed DSL | Ensures terms compose correctly |
| Writing Hydra kernel sources | Meta DSLs + Generated DSLs | Used throughout `Hydra/Sources/`; generated DSLs provide constructors/accessors |
| Code generation and metaprogramming | Meta DSLs | "Code as data" approach |
| Working with Hydra types (records, unions) | Generated DSL modules | Type-safe constructors, accessors, updaters |
| Calling primitives from DSL sources | Generated `Hydra.Dsl.Lib.*` | Typed wrappers derived from each primitive's signature |
| Referencing kernel functions outside the authoring layer | Generated term-module DSLs | Rename-safe typed references for non-Haskell hosts and shipped-kernel apps |
| Runtime AST manipulation | Generated code | Direct access to data structures |

**Rule of thumb**:
- **Type modules**: Use the direct Types DSL (`qualified Hydra.Overlay.Haskell.Dsl.Types as T`)
  with unqualified operators (`>:`, `@@`, `~>`)
- **Term modules**: Use the phantom-typed DSL for type safety, or Meta DSLs for kernel work
- **Metaprogramming**: Use the Meta DSLs to treat Hydra programs as data

**See also:**
- [Implementation](implementation.md#dsl-system) - Detailed DSL architecture and module organization
- [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) -
  Understanding Types, Terms, and the Hydra type system

## What belongs under `Sources/`

Files under `packages/<pkg>/src/main/haskell/Hydra/Sources/` are part of the DSL pipeline:
they are read by the `hydra-haskell` host, translated to JSON, then regenerated into all
target languages. Not every file under `Sources/` is itself a DSL module, though, and
mistaking the categories below for each other leads to confused issues and unnecessary
refactors. Four kinds of file are legitimate:

1. **DSL source modules.** A type-level or term-level module defining `module_ :: Module`,
   importing `Hydra.Kernel` and the relevant `Hydra.Dsl.*` modules. The artifact emitted by
   the file is what flows downstream into all eight target languages. This is the dominant
   case — most files under `Sources/` look like this.

2. **DSL infrastructure.** Manifests (`Hydra/Sources/<Pkg>/Manifest.hs`), aggregator modules
   (`*All.hs`), and the per-package `Libraries.hs`. These coordinate which DSL modules are
   shipped together; they don't themselves define a `Module`, but they wire up the ones that
   do.

3. **Meta-level emission helpers.** Plain Haskell files — sometimes without any Hydra
   imports — whose values exist purely to be walked at module-construction time by a
   sibling DSL source. The plain-Haskell shape is *convenience syntax for defining the
   spec*; the artifact that reaches the DSL pipeline is whatever the sibling emits when it
   walks the helper. The canonical examples are the inline `data FeatureSet` inside
   `Hydra/Sources/Cypher/Features.hs` (a DSL source that walks its own helper data) and
   `Hydra/Sources/Cypher/Functions.hs` (a sibling-only helper consumed by `Features.hs`).
   Both look like rule violations on a quick scan and are not.

   Helpers should always carry a header comment explaining what they feed into and what
   the emitted artifact is, so future readers don't mistake them for unpromoted sources.

4. **Generated DSL modules** under `Sources/<lang>/` (e.g. `Hydra/Sources/Python/...`).
   These are read-back outputs from `hydra-haskell`, written out for downstream packages
   to consume. They carry the standard "automatically generated file" header.

**What does *not* belong under `Sources/`:**

- **Plain-Haskell runtime code** — serializers, helpers, anything that runs inside the
  Haskell host rather than describing a Hydra module. Runtime code lives under `heads/`
  (when it is part of the Haskell runtime) or under a non-`Sources/` path within the
  package (when it is package-scoped support code).
- **Stale stubs.** A plain-Haskell file under `Sources/` that is excluded from its
  package's manifest because it's incomplete is still misleading: structurally it looks
  like an unpromoted source, and the manifest's "WIP" comment is easy to miss. Stage WIP
  outside `Sources/` (or on a feature branch) until ready to commit to promotion.

**Promotion vs. relocation.** When a file under `Sources/` matches none of (1)–(4), the
question is which kind it *should* be:
- Data or types that should be available across all eight target languages → promote to
  a real DSL source (see [docs/recipes/promoting-code.md](recipes/promoting-code.md)).
- Logic that only the Haskell host needs → relocate to `heads/haskell/` or a
  non-`Sources/` path within the package.
- Genuine meta-level scaffold → leave in place, but add a header comment so the role is
  obvious from the file alone.

## Direct DSLs (untyped)

The direct DSLs provide direct functions for constructing Hydra terms and types.

### Imports

**Type modules and term modules are typically separate.**
Most Hydra source files define either types or terms, not both.

For term modules:
```haskell
import Hydra.Overlay.Haskell.Dsl.Terms
```

For type modules (kernel type definitions):
```haskell
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations (doc)
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Overlay.Haskell.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
```

In tests or mixed modules (less common):
```haskell
import Hydra.Overlay.Haskell.Dsl.Terms
import qualified Hydra.Overlay.Haskell.Dsl.Types as T
```

**Note on qualification**: Term constructs are imported unqualified and used without a prefix.
Only use the `Terms.` prefix when there's a naming conflict (e.g., `Terms.map` when `Lists.map` is also imported).
Type constructs use the `T.` prefix.

### Creating literals

```haskell
-- Numeric literals
int32 42                             -- Int32
int64 1000000                        -- Int64
float32 3.14                         -- Float32
float64 2.71828                      -- Float64
bigint 123456789                     -- BigInteger
decimal (Sci.scientific 314 (-2))    -- Decimal (arbitrary-precision Scientific/BigDecimal/Decimal)

-- String and character
string "hello"              -- String
char 'a'                    -- Character (converted to Int32)

-- Boolean
boolean True                -- Boolean
```

### Creating functions

```haskell
-- Simple lambda
lambda "x" (var "x")

-- Multi-parameter lambda (curried)
lambdas ["x", "y"] (apply
  (var "add")
  (list [var "x", var "y"]))

-- Lambda with explicit type
lambdaTyped "x" T.int32 (var "x")

-- Function application
apply (var "f") (int32 5)

-- Or using the operator
import Hydra.Overlay.Haskell.Dsl.Terms ((@@))
var "f" @@ int32 5
```

### Creating data structures

```haskell
-- Lists
list [int32 1, int32 2, int32 3]

-- Records (always require a type name)
record (Name "Person") [
  "name">: string "Ford",
  "age">: int32 40]

-- Maps
map (M.fromList [
  (string "key1", int32 100),
  (string "key2", int32 200)])

-- Sets
set (S.fromList [int32 1, int32 2])

-- Optional values
just (int32 42)
nothing

-- Either values
left (string "error")
right (int32 42)

-- Tuples
pair (string "key") (int32 value)
```

### Let bindings

```haskell
-- Single binding
let1 "x" (int32 5) (var "x")

-- Multiple bindings
lets [
  "x">: int32 5,
  "y">: int32 10]
  (apply (var "add") (list [var "x", var "y"]))
```

### Pattern matching

```haskell
-- Match on a union type
match _Result Nothing [
  _Result_success >>: "val" ~> var "val",
  _Result_error >>: "err" ~> string "Failed"]
```

### Union types and injections

```haskell
-- Create a union injection
inject _Result _Result_success (int32 42)
```

## Phantom-typed DSL

The phantom-typed DSL uses Haskell's type system to verify Hydra programs at compile time.

### Key concept: TypedTerm

The phantom-typed DSL wraps terms in `TypedTerm a` where `a` is a phantom type parameter representing the Haskell type:

```haskell
TypedTerm Int        -- A Hydra term representing an Int
TypedTerm String     -- A Hydra term representing a String
TypedTerm (Int -> String)  -- A Hydra term representing a function
```

### Imports

```haskell
import Hydra.Overlay.Haskell.Dsl.Typed.Phantoms
```

### Type-safe functions

```haskell
-- Haskell knows this is a function Int -> Int
addOne :: TypedTerm (Int -> Int)
addOne = "x" ~> Math.add (int32 1) (var "x")

-- Type error! This wouldn't compile:
-- wrongType :: TypedTerm String
-- wrongType = "x" ~> var "x"  -- ERROR: lambda produces a function type
```

### Operators

The phantom-typed DSL provides several operators:

```haskell
-- Lambda: name ~> body
"x" ~> "y" ~> (var "x" + var "y")

-- Application: function @@ argument
addOne @@ int32 5

-- Let binding: name <~ value $ body
"x" <~ int32 5 $
"y" <~ int32 10 $
var "x" + var "y"

-- Flow binding: name <<~ flowExpr $ body
"result" <<~ someFlowOperation $
produce (var "result")
```

### Primitive functions

```haskell
-- Call a primitive function
primitive2 DefMath.add (int32 2) (int32 3)

-- Common primitives are wrapped for convenience
import Hydra.Dsl.Lib.Math as Math
Math.add (int32 2) (int32 3)
```

### Built-in vs. user-defined functions

**Important distinction**: Built-in helper functions use simplified application syntax,
while user-defined functions require explicit application with `@@`:

```haskell
import Hydra.Dsl.Lib.Math as Math

-- Built-in functions: simplified syntax
result1 = Math.add (int32 1) (int32 2)

-- User-defined functions: need explicit application
"myAdd" <~ ("x" ~> "y" ~> Math.add (var "x") (var "y")) $
result2 = var "myAdd" @@ int32 1 @@ int32 2

-- Another example with user-defined function
"double" <~ ("n" ~> Math.mul (var "n") (int32 2)) $
doubled = var "double" @@ int32 5
```

This is because built-in functions like `Math.add` are Haskell functions that construct Hydra terms,
while `var "myAdd"` is itself a Hydra term that needs to be applied using the `@@` operator.

### Benefits

1. **Compile-time verification**: Haskell catches type errors before runtime
2. **Better IDE support**: Type inference helps with autocompletion
3. **Documentation**: Type signatures document what the code does
4. **Refactoring safety**: Changing types causes compile errors rather than runtime failures

### Limitations

1. **More complex type signatures**: Can be harder to read
2. **Limited to well-typed terms**: Can't construct ill-typed terms (even intentionally)
3. **Phantom types don't fully match Hydra types**: Haskell's type system is different

## Meta DSLs

The meta DSLs are used for specifying programs that build terms or types.

### Key concept: Programs that construct Hydra objects

The meta DSLs let you write programs whose output is Hydra terms or types.
A `TypedTerm Term` is a Hydra term that, when evaluated, produces another Hydra `Term`.
Similarly, `TypedTerm Type` produces a Hydra `Type`.

The key difference from the phantom-typed DSL is that meta DSLs are for **building Hydra structures programmatically** -
when you need to generate terms or types based on runtime data, loop over collections, or create Hydra data structures
that will be serialized, code-generated, or manipulated as data.

### Concrete example: Generating test cases

Suppose you want to generate test cases for string primitive functions.
Each test case is a Hydra data structure (a `TestCase`), not just executable code.
The meta DSLs let you write Haskell functions that produce these Hydra structures.

Here's a real example from Hydra's test suite:

```haskell
-- From Hydra/Sources/Test/Lib/Strings.hs
import Hydra.Overlay.Haskell.Dsl.Tests  -- Includes the meta Terms DSL

stringsCat :: TestGroup
stringsCat = TestGroup "cat" Nothing [] [
    test "basic concatenation" ["one", "two", "three"] "onetwothree",
    test "unicode strings" ["\241", "\19990"] "\241\19990",
    test "empty list" [] ""]
  where
    test name ls result = primCase name _strings_cat [list (string <$> ls)] (string result)

-- primCase constructs a TestCase (Hydra data structure)
primCase :: String -> Name -> [Term] -> Term -> TestCaseWithMetadata
primCase cname name args output = TestCaseWithMetadata cname tcase Nothing []
  where
    tcase = TestCaseEvaluation $ EvaluationTestCase EvaluationStyleEager input output
    input = foldl (\a arg -> a @@ arg) (primitive name) args
```

**What's happening here:**
1. `stringsCat` is a Haskell value of type `TestGroup` (a Hydra data structure)
2. Each call to `test` produces a `TestCaseWithMetadata` (another Hydra structure)
3. Inside `primCase`, we use meta DSL functions like `primitive`, `string`, `list`, and `@@`
4. These construct `Term` values that represent the test input and expected output
5. The entire test suite becomes Hydra data that can be:
   - Serialized to JSON
   - Code-generated to Java/Python test suites
   - Executed by the Hydra interpreter

**Why not use direct DSLs?**
Direct DSLs construct terms directly, but here we need to:
- Build terms programmatically based on test data
- Use Haskell's list comprehensions and functions (`<$>`, `foldl`)
- Create nested Hydra structures (`TestCase` contains `Term`s, which contain more `Term`s)

The meta DSLs bridge Haskell's computational capabilities with Hydra's type system,
letting you write programs that generate Hydra code.

### Imports

For term modules (most common in kernel sources):
```haskell
import Hydra.Overlay.Haskell.Dsl.Typed.Terms
```

For type modules:
```haskell
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types as T
```

In mixed modules (less common):
```haskell
import Hydra.Overlay.Haskell.Dsl.Typed.Terms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types as T
```

### Defining types in modules

When you define types in Hydra kernel modules, you use `defineType` (from `Hydra.Overlay.Haskell.Bootstrap`)
to create type definitions. These definitions can reference each other directly.

```haskell
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations (doc)
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Overlay.Haskell.Dsl.Types as T

-- Define a module-scoped 'define' helper
ns :: ModuleName
ns = ModuleName "myapp.types"

define :: String -> Type -> TypeDefinition
define = defineType ns

-- Define type definitions
person :: TypeDefinition
person = define "Person" $
  doc "A person with a name and age" $
  T.record [
    "name">: T.string,
    "age">: T.int32]

-- Reference other definitions directly (no wrapper needed)
company :: TypeDefinition
company = define "Company" $
  T.record [
    "name">: T.string,
    "employees">: T.list person]  -- Direct reference to 'person' definition
```

When this module is code-generated (e.g., to Haskell), it produces:
- A type definition for `Person`
- Generated constants: `_Person` (a `Name`), `_Person_name` (a `Name`), `_Person_age` (a `Name`)

These constants can then be used in term modules:

```haskell
-- In a term module (after the Person type is defined and generated)
import Hydra.Overlay.Haskell.Dsl.Typed.Phantoms

trillian :: Term
trillian = record _Person [
  _Person_name>>: string "Trillian",
  _Person_age>>: int32 35]

greet :: Term
greet = lambda "person" $
  string "Hello, " ++
  primitive "concat" @@ project _Person _Person_name (var "person")
```

**Note the different field syntax**:
- `>:` for field definitions with string keys (in type definitions)
- `>>:` for field assignments with `Name` constants (in term constructions using generated constants)

### Module definitions

```haskell
module_ :: Module
module_ = Module {
    moduleName = ns,
    moduleDefinitions = definitions,
    moduleDependencies = [],
    moduleDescription = Just "My application module"}
  where
    ns = ModuleName "myapp"
    definitions = [
      toDefinition $ def "addOne" $
        doc "Adds one to a number" $
        lambda "x" (Math.add (var "x") (int32 1))]
```

### When to use

Use the meta DSLs when writing programs that construct Hydra terms or types:
- Building Hydra kernel definitions (terms that produce types or other terms)
- Writing code generators (programs that output Hydra code)
- Creating DSL sources for Hydra modules
- Metaprogramming: treating Hydra code as data that can be manipulated

### Examples in the codebase

The entire Hydra kernel is defined using the meta DSLs.

**Type modules**
(see [Sources/Kernel/Types](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Types)):
- `Hydra/Sources/Kernel/Types/Core.hs` - Core type definitions (Type, Term, etc.)
- `Hydra/Sources/Kernel/Types/Graph.hs` - Graph and module types
- These modules import `qualified Hydra.Overlay.Haskell.Dsl.Types as T` along with unqualified operators `(>:)`, `(@@)`, `(~>)`

**Term modules**
(see [Sources/Kernel/Terms](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms)):
- `Hydra/Sources/Kernel/Terms/Inference.hs` - Type inference algorithm
- `Hydra/Sources/Kernel/Terms/Reduction.hs` - Term reduction logic
- `overlay/.../Hydra/Dsl/Libraries.hs` - host-side primitive registry (names derived from PrimitiveDefinitions)
- These modules import `Hydra.Overlay.Haskell.Dsl.Typed.Terms` (unqualified)

## Operator reference

### Function construction and application

| Operator | DSL | Type | Description | Example |
|----------|-----|------|-------------|---------|
| `~>` | Phantom | `String -> TypedTerm x -> TypedTerm (a -> b)` | Lambda parameter | `"x" ~> var "x"` |
| `@@` | Phantom/Meta | `TypedTerm (a -> b) -> TypedTerm a -> TypedTerm b` | Function application | `f @@ arg` |
| `<.>` | Phantom | `TypedTerm (b -> c) -> TypedTerm (a -> b) -> TypedTerm (a -> c)` | Function composition | `f <.> g` |

### Let bindings

| Operator | DSL | Type | Description | Example |
|----------|-----|------|-------------|---------|
| `<~` | Phantom | `String -> TypedTerm a -> TypedTerm b -> TypedTerm b` | Pure let binding | `"x" <~ expr $ body` |
| `<<~` | Phantom | `String -> TypedTerm (Flow s a) -> TypedTerm (Flow s b) -> TypedTerm (Flow s b)` | Flow let binding | `"x" <<~ flowExpr $ body` |

### Record construction

| Operator | DSL | Type | Description | Example |
|----------|-----|------|-------------|---------|
| `>:` | All | `String -> a -> (TypedTerm Name, a)` | Field definition | `"name">: value` |
| `>>:` | Base | `Name -> a -> (TypedTerm Name, a)` | Record field (tuple) | `fname>>: value` |

### Pattern matching

| Operator | DSL | Type | Description | Example |
|----------|-----|------|-------------|---------|
| `>>:` | Phantom | `Name -> t -> Field` | Match case (Field) | `_Type_record >>: "r" ~> ...` |

**Note**: `>>:` is overloaded. In `Base` it produces a tuple (for record definitions); in `Phantoms`
it produces a `Field` (for `cases`/`match` branches). When `Phantoms` is imported qualified, the
unqualified `>>:` resolves to the `Base` version. See [Troubleshooting](#error-produces-a-tuple-instead-of-a-field).

### Precedence

Operators are defined with these precedence levels:

```haskell
infixr 0 >:      -- Lowest precedence
infixr 0 <~
infixr 0 <<~
infixl 1 @@
infixr 9 <.>     -- Highest precedence
```

This means:
- `>:`, `<~`, `<<~` bind very loosely (use them last)
- `@@` is left-associative (`f @@ x @@ y` = `(f @@ x) @@ y`)
- `<.>` binds tightly (function composition)

## Common patterns

### Pattern 1: Simple function

```haskell
-- Direct DSLs
myFunc = lambda "x" (int32 42)

-- Phantom-typed DSL
myFunc = "x" ~> int32 42

-- Meta DSLs
myFunc = lambda "x" (int32 42)
```

### Pattern 2: Multi-argument function

```haskell
-- Direct DSLs
add = lambdas ["x", "y"] (
  apply (primitive "add")
    (list [var "x", var "y"]))

-- Phantom-typed DSL
add = "x" ~> "y" ~>
  primitive2 DefMath.add (var "x") (var "y")

-- Or using library functions
import Hydra.Dsl.Lib.Math as Math
add = "x" ~> "y" ~> Math.add (var "x") (var "y")
```

### Pattern 3: Let bindings

```haskell
-- Direct DSLs
expr = lets [
  "x">: int32 5,
  "y">: int32 10]
  (apply (var "add") (list [var "x", var "y"]))

-- Phantom-typed DSL
expr =
  "x" <~ int32 5 $
  "y" <~ int32 10 $
  Math.add (var "x") (var "y")
```

### Pattern 4: Pattern matching

```haskell
-- Match on an optional value (hydra.lib.optionals)
handleOptional = Optionals.cases optValue
  defaultValue                          -- the `nothing` case
  ("val" ~> processValue (var "val"))   -- the `just` case

-- Match on a (hypothetical) union type with `match` / `>>:`
handleResult = match _Result Nothing [
  _Result_success >>: "val" ~> var "val",
  _Result_error >>: "err" ~> handleError (var "err")]
```

### Pattern 5: Record construction

```haskell
-- Direct DSLs (produces a Term)
zaphod :: Term
zaphod = record (Name "Person") [
  "name">: string "Zaphod",
  "age">: int32 42,
  "email">: string "zaphod@heartofgold.com"]

-- Phantom-typed DSL (produces a typed TypedTerm)
zaphod :: TypedTerm Person
zaphod = record _Person [
  _Person_name>>: string "Zaphod",
  _Person_age>>: int32 42,
  _Person_email>>: string "zaphod@heartofgold.com"]
```

**Note the differences**:
- Direct DSLs: Type signature is `Term`, uses `Name "Person"` and string field names with `>:`
- Phantom DSL: Type signature is `TypedTerm Person`, uses `_Person` and generated field constants with `>>:`

### Pattern 6: List operations

```haskell
import Hydra.Overlay.Haskell.Dsl.Terms
import Hydra.Dsl.Lib.Lists as Lists

-- Map over a list
doubleList = Lists.map (lambda "x" (Math.mul (var "x") (int32 2))) myList

-- Filter a list
evens = Lists.filter (lambda "x" (Math.even (var "x"))) myList

-- Fold a list
sum = Lists.foldl (lambda "acc" (lambda "x" (Math.add (var "acc") (var "x")))) (int32 0) myList
```

**Note on naming conflicts**: In this example, `Lists.map` is qualified because the `Lists` library is imported.
If you also need `Hydra.Overlay.Haskell.Dsl.Terms.map` (for constructing Map terms), you would use `Terms.map` to disambiguate:

```haskell
import Hydra.Overlay.Haskell.Dsl.Terms as Terms
import Hydra.Dsl.Lib.Lists as Lists

-- Use Terms.map when constructing a Map term
myMap = Terms.map (M.fromList [...])

-- Use Lists.map when mapping over a list
myList = Lists.map (lambda "x" (var "x")) someList
```

### Pattern 7: Documenting definitions

The `doc` combinator (from `Hydra.Overlay.Haskell.Dsl.Annotations` or `Hydra.Overlay.Haskell.Dsl.Typed.Phantoms`)
attaches a human-readable description to a term or type binding.

```haskell
import Hydra.Overlay.Haskell.Dsl.Annotations (doc)

myFunction :: TypedTermDefinition (Int -> Int)
myFunction = define "myFunction" $
  doc "Add one to an integer" $
  "x" ~> Math.add (var "x") (int32 1)
```

`doc` must be the **outermost wrapper around the function body**, before lambdas
and let bindings. The `Validate.Packaging.checkDefinitionDocumentation` check
(part of `kernelPackage` for the kernel) verifies this placement; it peels
`TypeLambda` and `TypeApplication` layers from the body and then requires the
result to be an `Annotated` node carrying a `description` annotation.
Burying `doc` inside the lambda body — e.g. `"x" ~> doc "..." (...)` — does not
satisfy the check.

**Synthesizer doc propagation.** When a synthesizer (e.g.
`generateRecordAccessor` in `Hydra/Sources/Kernel/Terms/Dsls.hs`) builds a
`Binding` whose body is computed at meta-DSL interpretation time, host-level
`doc` is not available because the description string depends on runtime
values. Use `Annotations.setTermDescription` from
`Hydra.Sources.Kernel.Terms.Annotations` instead:

```haskell
"description" <~ (Strings.cat $ list [string "DSL accessor for the ",
  Core.unName (var "fieldName"), string " field"]) $
"body" <~ (Annotations.setTermDescription @@ (just (var "description")) @@ var "rawBody") $
Core.binding (var "name") (var "body") (just (var "ts"))
```

This produces a binding whose interpreted term carries the description at the
outermost layer, satisfying the same validator check as host-level `doc`.

## Working with types

### Basic types

```haskell
import qualified Hydra.Overlay.Haskell.Dsl.Types as T

-- Literal types
T.int32
T.int64
T.bigint
T.float32
T.float64
T.decimal
T.string
T.boolean
T.binary

-- Type variables (string literals work directly via AsType instance)
"a"              -- In type module context, string literals become type variables
T.var "a"        -- Explicit form (equivalent)

-- Function types (using ~> operator, imported unqualified)
T.int32 ~> T.string  -- Int32 -> String

-- Application types (using @@ operator, imported unqualified)
someType @@ T.int32  -- Apply type to argument
```

### Compound types

```haskell
-- Record type (using >: operator, imported unqualified)
T.record [
  "name">: T.string,
  "age">: T.int32]

-- Union type
T.union [
  "success">: T.int32,
  "error">: T.string]

-- List type
T.list T.int32

-- Map type
T.map T.string T.int32

-- Optional type
T.optional T.int32

-- Either type
T.either_ T.string T.int32  -- Either String Int32
```

### Polymorphic types

```haskell
-- Forall type (System F)
T.forAll "a" $ "a" ~> "a"
-- ∀a. a -> a

-- Multiple type variables
T.forAlls ["a", "b"] $ "a" ~> "b" ~> T.pair "a" "b"
-- ∀a b. a -> b -> (a, b)
```

### Referencing other type bindings

In kernel type modules, types are defined as `TypeDefinition` values. These can be referenced
directly in type expressions without any wrapper function, thanks to the `AsType` type class:

```haskell
-- Example from Hydra.Sources.Kernel.Types.Core
name :: TypeDefinition
name = define "Name" $ T.wrap T.string

field :: TypeDefinition
field = define "Field" $
  T.record [
    "name">: name,      -- Reference to another TypeDefinition (no wrapper needed)
    "term">: term]      -- Self-reference also works
```

The `AsType` class provides implicit coercion from `TypeDefinition`, `Type`, and `String` to `Type`:
- `TypeDefinition` → `TypeVariable` with the definition's name
- `Type` → identity (no conversion)
- `String` → `TypeVariable` with the string as name

## Working with terms

### Variables and references

```haskell
-- Variable reference
var "x"

-- Primitive reference
primitive "hydra.lib.math.add"

-- Qualified name reference
ref (Name "hydra.core.Term")
```

### Function application

```haskell
-- Apply function to argument
apply (var "f") (int32 5)

-- Apply to multiple arguments (curried)
apply (apply (var "add") (int32 2)) (int32 3)

-- Using operators (more concise)
var "add" @@ int32 2 @@ int32 3
```

### Data access

```haskell
-- Project field from record (requires type name and field name)
project (Name "Person") (Name "name") (var "person")

-- Or with generated constants from meta DSLs
project _Person _Person_name (var "person")

-- Extract value from union
match _Result Nothing [
  _Result_success >>: "val" ~> var "val",
  _Result_error >>: "err" ~> string "error"]
```

## Error handling with Either

Hydra uses `Either Error a` for computations that can fail. `Error` is a structured union
type from `hydra.errors`; an `InferenceContext` value carrying the fresh-type-variable
counter and the current subterm-path trace is threaded alongside the graph as an
explicit parameter.

### Basic Either operations

```haskell
import Hydra.Dsl.Lib.Eithers as Eithers

-- Success value
right (int32 42)

-- Error value
left (string "something went wrong")

-- Bind operation (chain computations that may fail)
Eithers.bind eitherExpr (lambda "x" (processValue (var "x")))

-- Map over a successful value
Eithers.map (lambda "x" (Math.add (var "x") (int32 1))) eitherExpr
```

### Either let bindings

```haskell
-- Sequential operations with error short-circuiting
"x" <~ fetchValue $
"y" <~ processValue (var "x") $
right (Math.add (var "x") (var "y"))
```

### Subterm-path tracing

The `InferenceContext` type carries a `trace` field (a list of `SubtermStep`s,
accumulated backward as inference descends into a term) used for error reporting.
Functions that need tracing accept an `InferenceContext` parameter explicitly.

## Primitive functions

Hydra provides many primitive functions organized into libraries.

### Math operations

```haskell
import Hydra.Dsl.Lib.Math as Math

Math.add (int32 2) (int32 3)        -- Addition
Math.sub (int32 5) (int32 2)        -- Subtraction
Math.mul (int32 4) (int32 3)        -- Multiplication
Math.maybeDiv (int32 10) (int32 2)  -- Safe division (returns Maybe; Nothing on divisor 0)
Math.maybeMod (int32 10) (int32 3)  -- Safe modulo (returns Maybe; Nothing on divisor 0)
Math.abs (int32 (-5))               -- Absolute value
```

### String operations

```haskell
import Hydra.Dsl.Lib.Strings as Strings

Strings.concat (string "Hello, ") (string "world!")
Strings.length (string "hello")
Strings.toUpper (string "hello")
Strings.toLower (string "HELLO")
Strings.substring (int32 0) (int32 5) (string "Hello, world!")
```

### List operations

```haskell
import Hydra.Dsl.Lib.Lists as Lists

Lists.map (lambda "x" (Math.add (var "x") (int32 1))) myList
Lists.filter (lambda "x" (Math.gt (var "x") (int32 0))) myList
Lists.foldl (lambda "acc" (lambda "x" (Math.add (var "acc") (var "x")))) (int32 0) myList
Lists.maybeHead myList              -- Maybe<a>: first element, Nothing if empty
Lists.maybeTail myList              -- Maybe<[a]>: all but first, Nothing if empty
Lists.uncons myList                 -- Maybe<(a, [a])>: head-and-tail combined
Lists.concat list1 list2
Lists.reverse myList
Lists.length myList
```

### Map operations

```haskell
import Hydra.Dsl.Lib.Maps as Maps

Maps.empty
Maps.insert key value myMap
Maps.lookup key myMap
Maps.remove key myMap
Maps.keys myMap
Maps.values myMap  -- Actually Maps.elems
Maps.fromList (list [tuple2 key1 val1, tuple2 key2 val2])
```

### Optional operations

```haskell
import Hydra.Dsl.Lib.Optionals as Optionals

Optionals.isGiven optionalValue
Optionals.isNone optionalValue
Optionals.fromOptional defaultValue optionalValue              -- extract with fallback
Optionals.cases optionalValue defaultValue (lambda "x" ...)    -- fold over the two branches (subject-first)
Optionals.map (lambda "x" (Math.add (var "x") (int32 1))) optionalValue
```

### Equality and comparison

```haskell
import Hydra.Dsl.Lib.Equality as Eq

Eq.eq value1 value2                 -- Equality
Eq.ne value1 value2                 -- Inequality

import Hydra.Dsl.Lib.Logic as Logic

Logic.and (boolean True) (boolean False)
Logic.or (boolean True) (boolean False)
Logic.not (boolean True)
```

## Troubleshooting

### Common errors and solutions

#### Error: "Ambiguous type variable"

**Problem**: Without a type signature, Haskell doesn't know which phantom type to use

```haskell
-- Error: What type is 'a' in TypedTerm a?
myFunc = "x" ~> var "x"
```

**Solution**: Add a type signature to activate compile-time checking

```haskell
myFunc :: TypedTerm (a -> a)
myFunc = "x" ~> var "x"
```

**Note**: Hydra can infer types at runtime, but the type signature in phantom DSL code is for your benefit.
It activates Haskell's compile-time type checking so your IDE can help you write valid code and catch errors early.

#### Error: "Couldn't match type ... with ..."

**Problem**: Type mismatch in phantom-typed DSL

```haskell
-- Error: int32 returns TypedTerm Int, but we claimed TypedTerm String
myFunc :: TypedTerm String
myFunc = int32 42
```

**Solution**: Fix the type signature or the implementation

```haskell
myFunc :: TypedTerm Int
myFunc = int32 42
```

#### Error: "Variable not in scope"

**Problem**: Missing import

```haskell
-- Error: Not in scope: 'lambda'
myFunc = lambda "x" (var "x")
```

**Solution**: Import the DSL module

```haskell
import Hydra.Overlay.Haskell.Dsl.Typed.Phantoms

myFunc = lambda "x" (var "x")
```

#### Error: "No instance for (Num (TypedTerm a))"

**Problem**: Trying to use Haskell's numeric operators on TypedTerm

```haskell
-- Error: Can't use + directly on TypedTerm
result = int32 2 + int32 3
```

**Solution**: Use Hydra's primitive functions

```haskell
import Hydra.Dsl.Lib.Math as Math

result = Math.add (int32 2) (int32 3)
```

#### Error: "`>>:` produces a tuple instead of a `Field`"

**Problem**: The `>>:` operator is defined in two places with different types:

- `Hydra.Overlay.Haskell.Dsl.Typed.Phantoms`: `Name -> t -> Field` (for `cases`/`match` branches)
- `Hydra.Overlay.Haskell.Dsl.Typed.Base`: `Name -> a -> (TypedTerm Name, a)` (for record field definitions)

If `Phantoms` is imported qualified (as in test source files), the unqualified `>>:` resolves to the
`Base` version, which produces a tuple. Passing these tuples to `Phantoms.cases` causes a type error:

```haskell
-- Error: Couldn't match expected type 'Field' with actual type '(TypedTerm Name, TypedTerm (a -> b))'
Phantoms.cases _Term (Phantoms.var "t") (Just defaultVal) [
  _Term_literal >>: Phantoms.lambda "lit" $ ...]   -- >>: is Base.>>:, returns a tuple
```

**Solutions**:

1. Use `Phantoms.>>:` qualified (awkward but explicit)
2. Define a local alias: `(~>:) = (Phantoms.>>:); infixr 0 ~>:`
3. Import `Phantoms` unqualified (as kernel source files do) — but this may conflict with other imports

```haskell
-- Using a local alias
(~>:) :: AsTerm t a => Name -> t -> Field
(~>:) = (Phantoms.>>:)
infixr 0 ~>:

Phantoms.cases _Term (Phantoms.var "t") (Just defaultVal) [
  _Term_literal ~>: Phantoms.lambda "lit" $ ...]   -- correct: produces a Field
```

### Debugging tips

1. **Start simple**: Build complex expressions incrementally
2. **Check types**: Use GHCi's `:type` command to verify types
3. **Use qualified imports**: Avoid name conflicts with `import qualified`
4. **Read error messages carefully**: Type errors often point to the exact issue
5. **Look at examples**: See `Hydra/Sources/` for real-world usage

## Advanced topics

### Type schemes and polymorphism

Type schemes allow polymorphic types:

```haskell
-- Identity function: ∀a. a -> a (verbose)
idScheme = TypeScheme {
  typeVariables = [Name "a"],
  typeConstraints = [],
  type_ = T.function (T.variable "a") (T.variable "a")
}

-- More succinctly:
idScheme = T.poly ["a"] $ T.function (T.var "a") (T.var "a")
```

### Annotations

Add metadata to terms. The host-side API accepts a `Map Name Term` of
annotations; on the wire (and in `AnnotatedTerm.annotation`) those are
encoded as a `TermMap` whose keys are `TermVariable`s — see #386 below.

```haskell
import qualified Data.Map as M

-- Attach an annotation to a term (annotations first, then term)
-- annots :: TypedTerm (Map Name Term) -> TypedTerm Term -> TypedTerm Term
annots (Phantoms.map (M.fromList [(Name "comment", string "A User ID")]))
       (var "userId")

-- Alternative: term first, then annotations
-- annotated :: TypedTerm Term -> TypedTerm (Map Name Term) -> TypedTerm Term
annotated (var "userId")
          (Phantoms.map (M.fromList [(Name "comment", string "A User ID")]))
```

#### Annotation shape (#386)

`AnnotatedTerm.annotation` and `AnnotatedType.annotation` are typed as
`Term` and `Type` (respectively), not `Map<Name, Term>`. The canonical
encoding for an annotation map at the schema level is:

```
inject(Term){map: TermMap [(inject(Term){variable: wrap(Name){"k"}}, value), …]}
```

Two kernel helpers bridge the host-friendly Map view and the schema:

- `wrapAnnotationMap :: M.Map Name Term -> Term` — produces the encoded
  Term form above.
- `getAnnotationMap :: Term -> M.Map Name Term` — projects the Map back
  out (accepts both `TermVariable` and the transitional `TermWrap`-of-Name
  key shapes).

DSL helpers like `annots` / `annotated` call `wrapAnnotationMap` for you,
so most callers never see the encoded shape directly. Test cases that
inspect annotation Terms via `cases _Term ...` do need to know the
encoded form — see `Hydra.Sources.Test.Annotations` for examples.

#### Limitation: `metaref`/`@@` chains into a polymorphic parameter (#550)

`metaref :: TypedTermDefinition a -> TypedTerm Term` erases a definition's
phantom type to `Term`, and chaining `metaref def @@ arg1 @@ arg2 @@ ...`
(using the polymorphic `Phantoms.@@ :: (AsTerm f (a -> b), AsTerm g a) => f
-> g -> TypedTerm b`) leaves each intermediate result's phantom type as a
flexible metavariable. This composes fine when the chain's result feeds a
*monomorphic* parameter (e.g. a helper typed `TypedTerm Term -> TypedTerm
Term -> ...`), but fails with an `AsTerm`/functional-dependency ambiguity
error when it feeds a parameter that is itself `AsTerm`-polymorphic, such
as `Phantoms.pair :: (AsTerm t1 a, AsTerm t2 b) => t1 -> t2 -> TypedTerm (a,
b)` — GHC has two independent open constraints to solve at once and neither
side supplies enough concrete type information. This is the same class of
inference fragility as the `rewriteDef` combinator removed in `6ed3de4fd4`.

**Workaround**: build the term with `annots` (monomorphic, no `AsTerm`
chain) instead of `metaref` + `@@`, and pull annotation keys via `asTerm`
(not `metaref`) — a `TypedTermDefinition Name` like `Constants.keyType`
already yields `TypedTerm Name` directly through the plain `AsTerm
(TypedTermDefinition a) a` instance, with no erasure to `Term` and back:

```haskell
-- Fails: metaref erases to Term; chaining @@ leaves b flexible, so
-- Phantoms.pair's two AsTerm constraints can't both resolve.
badPair = Phantoms.pair (nm "a")
  (metaref Annotations.setTermAnnotation @@ Constants.keyType @@ someType @@ subject)

-- Works: annots is monomorphic; asTerm keeps keyType's phantom type as Name.
goodPair = Phantoms.pair (nm "a")
  (annots (Phantoms.map (M.fromList [(Phantoms.asTerm Constants.keyType, someTypeTerm)]))
          subject)
```

See `Hydra.Sources.Test.Dependencies` (`sortBindingsCase "type-annotated
binding is dependency-free"`) for a full worked example.

### Module definitions

Create Hydra modules:

```haskell
myModule :: Module
myModule = Module {
    moduleName = ns,
    moduleDefinitions = definitions,
    moduleDependencies = unqualifiedDep <$> [mathNs, coreNs],
    moduleDescription = Just "Utility functions"}
  where
    ns = ModuleName "myapp.utils"
    definitions = [
      toDefinition $ def "addOne" $ lambda "x" (Math.add (var "x") (int32 1)),
      toDefinition $ def "double" $ lambda "x" (Math.mul (var "x") (int32 2))]
```

### Code generation workflow

When defining Hydra sources for code generation:

1. **Define types**: Use the meta DSL to define data types
2. **Define functions**: Use the meta DSL to define logic
3. **Create modules**: Group definitions into modules
4. **Generate code**: Run `writeHaskell`, `writeJava`, or `writePython`

Example:

```haskell
-- In Hydra/Sources/MyApp/Types.hs
module_ :: Module
module_ = Module {
    moduleName = ns,
    moduleDefinitions = definitions,
    moduleDependencies = [],
    moduleDescription = Just "User-defined types"}
  where
    ns = ModuleName "myapp.types"
    definitions = [
      toDefinition $ def "Person" $ record [
        "name">: string,
        "age">: int32]]

-- Generate Haskell code
-- First argument: output directory
-- Second argument: universe modules (for dependency resolution)
-- Third argument: modules to generate
writeHaskell "../../dist/haskell/hydra-kernel/src/main/haskell" [module_] [module_]
```

### Working with the generated code

Once code is generated, you can use it:

```haskell
-- Generated code creates a Person constructor
import MyApp.Types (Person(..))

myPerson :: Person
myPerson = Person {
  personName = "Ford",
  personAge = 40
}
```

### Integration with Either for error handling

For computations that can fail:

```haskell
import Hydra.Dsl.Lib.Logic as Logic
import Hydra.Dsl.Lib.Eithers as Eithers

safeDivide :: TypedTerm (Int -> Int -> Either String Int)
safeDivide = "x" ~> "y" ~>
  Optionals.cases
    (Math.maybeDiv (var "x") (var "y"))
    (left (string "Division by zero"))
    ("q" ~> right (var "q"))
```

## Import conventions

For the full import conventions — eight categories of source module, each with a canonical
import block — see [docs/import-conventions.md](import-conventions.md). Each class of source
module has a conventional import block that is copied verbatim into every source file of that
class. When creating a new module, copy the import block from an existing module of the same
kind.

## Application styles

There are three distinct ways to apply functions in Hydra DSLs, and confusing them is
a common source of errors.

### DSL helpers (direct Haskell application)

Functions from `Hydra.Dsl.Lib.*` and `Hydra.Overlay.Haskell.Dsl.Typed.Phantoms` are Haskell functions
on `TypedTerm` values. They take arguments directly via Haskell function application -- no `@@`
needed. This includes all primitive function wrappers (`Lists.concat`, `Strings.cat`,
`Optionals.cases`, `Logic.ifElse`, etc.) and DSL combinators (`list`, `lambda`, `cases`,
`project`, `lets`, etc.).

```haskell
Strings.cat2 (string "foo") (string "bar")
Lists.concat (list [var "xs", var "ys"])
```

### Element definitions (apply with `@@`)

`TypedTermDefinition`s created with `define` are applied using the `@@` operator:

```haskell
myAddDef @@ int32 1 @@ int32 2
Serialization.cst @@ string "hello"   -- Serialization helpers are TypedTermDefinitions
```

### Passing primitives as arguments

When a primitive needs to be passed as a function argument (not called directly),
use `unaryFunction` or `binaryFunction`:

```haskell
Lists.foldl (binaryFunction Math.add) (int32 0) (var "numbers")
```

## Related topics

- [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) - Core Hydra concepts
- [Implementation](implementation.md) - Detailed implementation guide
- [Code organization](https://github.com/CategoricalData/hydra/wiki/Code-organization) - Project structure
- [Java DSL guide](dsl-guide-java.md) - Java-specific DSL reference
- [Python DSL guide](dsl-guide-python.md) - Python-specific DSL reference
- [Scala DSL guide](dsl-guide-scala.md) - Scala-specific DSL reference

---

This guide covers the essential aspects of Hydra's DSLs. For more examples, explore the
`Hydra/Sources/` directory in the codebase, which contains extensive real-world usage of
these DSLs.
