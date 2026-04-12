# Hydra Test Suite Architecture

This document explains the architecture of Hydra's common test suite,
including the test kernel pattern and how test modules are structured for code generation.

## Overview

Hydra's test suite is designed to ensure parity across all Hydra language implementations
(Haskell, Java, Python, Scala, and Lisp).
The test suite uses the same module-based code generation approach as the main Hydra kernel,
allowing tests to be written once in Haskell and automatically translated to other languages.

## Key Concepts

### Test Kernel (Test Graph)

The **test kernel** or **test graph** (`Hydra/Sources/Test/TestGraph.hs`) defines a shared resource graph containing:

- **Test Types** - Common type definitions used across tests (e.g., `Person`, `Triple`, `UnionMonomorphic`)
- **Test Terms** - Shared term definitions and test data
- **Test Namespace** - The namespace for test resources
- **Test Schema Namespace** - The namespace for test schemas

The test kernel provides a centralized repository of test resources that can be referenced from any test module
using `ref`. This ensures consistency and eliminates duplication across test cases.

**Example from TestGraph.hs:**
```haskell
module_ :: Module
module_ = Module (Namespace "hydra.test.testGraph") definitions
    [TestTerms.module_, TestTypes.module_]
    kernelTypesModules $
    Just "A module defining the graph used in the test suite."
  where
   definitions = [
     toDefinition testTermsDef,
     toDefinition testTypesDef,
     toDefinition testNamespaceDef,
     toDefinition testSchemaNamespaceDef]

testTypesDef :: TTermDefinition (M.Map Name Type)
testTypesDef = define "testTypes" $
  Maps.fromList $ Phantoms.list [
    Phantoms.tuple2 (ref TestTypes.testTypePersonNameDef) (ref TestTypes.testTypePersonDef),
    Phantoms.tuple2 (ref TestTypes.testTypeTripleNameDef) (ref TestTypes.testTypeTripleDef),
    -- ... more test types
  ]
```

### Module-Based Test Organization

Tests are organized as proper Hydra modules with:

1. **Namespace** - Determines the generated file location
2. **Elements** - Named test group bindings exported from the module
3. **Module Dependencies** - References to other test modules
4. **Schema Dependencies** - Type schemas needed for test construction

When you run `writeHaskell "../../dist/haskell/hydra-kernel/src/test/haskell" allModules baseTestModules`
(where `allModules = mainModules ++ testModules`),
each module generates a separate file based on its namespace:
- `hydra.test.checking.fundamentals` → `Hydra/Test/Checking/Fundamentals.hs`
- `hydra.test.inference.algebraicTypes` → `Hydra/Test/Inference/AlgebraicTypes.hs`
- `hydra.test.etaExpansion` → `Hydra/Test/EtaExpansion.hs`

## Test Module Structure

### Basic Pattern

Every test module follows this pattern:

```haskell
module Hydra.Sources.Test.MyTest where

import Hydra.Kernel
import qualified Hydra.Dsl.Meta.Testing as Testing
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Sources.Test.TestGraph as TestGraph

module_ :: Module
module_ = Module (Namespace "hydra.test.myTest") elements
    [TestGraph.module_]           -- Module dependencies
    kernelTypesModules           -- Schema dependencies
    (Just "Description of this test module")
  where
    definitions = [
      Phantoms.toDefinition allTestsDef]

define :: String -> TTerm a -> TTermDefinition a
define = Phantoms.definitionInModule module_

allTestsDef :: TTermDefinition TestGroup
allTestsDef = define "allTests" $
  Phantoms.doc "Description of test group" $
  Testing.testGroup (Phantoms.string "myTest")
    Phantoms.nothing
    (Phantoms.list subgroups)
    (Phantoms.list [])
  where
    subgroups = [...]
```

### Key Components

1. **`module_`** - The module definition with namespace, elements, dependencies, and description

2. **`define`** - Helper function using `Phantoms.definitionInModule` to create bindings

3. **`allTestsDef`** - The root test group binding, always named `allTestsDef` by convention

4. **Test Groups** - Constructed using `Testing.testGroup` with meta-level parameters:
   - Name: `Phantoms.string "name"`
   - Description: `Phantoms.nothing` or `Phantoms.just $ Phantoms.string "desc"`
   - Subgroups: `Phantoms.list [...]`
   - Test cases: `Phantoms.list [...]`

### Referencing the Test Kernel

Test modules reference the test kernel's shared resources using `ref`:

```haskell
-- Reference a test type
project (ref TestTypes.testTypePersonNameDef) (Core.name "firstName")

-- Reference in a type annotation
lambdaTyped "x" (Core.typeVariable $ ref TestTypes.testTypeUnionMonomorphicNameDef) $ ...

-- Using test type in function signature
T.function (Core.typeVariable $ ref TestTypes.testTypePersonNameDef) T.string
```

The `ref` function creates a reference to a binding defined in another module, which is resolved during code generation.

## Test Types

Hydra supports several test case types:

### Type Checking Tests

```haskell
testCase :: String -> [Tag] -> TTerm Term -> TTerm Term -> TTerm Type -> TTerm TestCaseWithMetadata
testCase name tags input outputTerm outputType =
  Testing.testCaseWithMetadata (Phantoms.string name)
    (Testing.testCaseTypeChecking $ Testing.typeCheckingTestCase input outputTerm outputType)
    Phantoms.nothing
    (Phantoms.list $ tag . unTag <$> tags)
```

### Type Inference Tests

```haskell
infTest :: String -> [Tag] -> TTerm Term -> TTerm TypeScheme -> TTerm TestCaseWithMetadata
infTest name tags term ts =
  Testing.testCaseWithMetadata (Phantoms.string name)
    (Testing.testCaseInference $ Testing.inferenceTestCase term ts)
    Phantoms.nothing
    (Phantoms.list $ tag . unTag <$> tags)
```

### Eta Expansion Tests

```haskell
testCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
testCase name input output =
  Testing.testCaseWithMetadata (Phantoms.string name) tcase Phantoms.nothing (Phantoms.list [])
  where
    tcase = Testing.testCaseEtaExpansion $ Testing.etaExpansionTestCase input output
```

### Case Conversion Tests

```haskell
testCase :: Int -> CaseConvention -> CaseConvention -> String -> String -> TTerm TestCaseWithMetadata
testCase i fromConvention toConvention fromString toString =
  Testing.testCaseWithMetadata name tcase Phantoms.nothing (Phantoms.list [])
  where
    tcase = Testing.testCaseCaseConversion $ Testing.caseConversionTestCase
      (metaConv fromConvention)
      (metaConv toConvention)
      (Phantoms.string fromString)
      (Phantoms.string toString)
```

## Meta-Level vs Term-Level DSLs

Understanding the distinction between meta-level and term-level DSLs is crucial for writing tests:

### Term-Level DSL (`Hydra.Dsl.Terms`, `Hydra.Dsl.Tests`)

Used for constructing **Haskell `Term` values** representing Hydra terms:

```haskell
import Hydra.Dsl.Tests

-- These create Term values (Haskell data)
lambda "x" $ var "x"                    -- Term
splitOn @@ string "," @@ var "input"    -- Term
record personType ["name">: string "Alice", "age">: int32 30]  -- Term
```

### Meta-Level DSL (`Hydra.Dsl.Meta.Phantoms`, `Hydra.Dsl.Meta.Testing`)

Used for constructing **`TTerm a` values** - meta-representations of Hydra terms used in modules:

```haskell
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Meta.Testing as Testing

-- These create TTerm values (meta-representations)
Phantoms.string "name"                  -- TTerm String
Phantoms.list [...]                     -- TTerm [a]
Phantoms.nothing                        -- TTerm (Maybe a)
Testing.testGroup (Phantoms.string "name") ...  -- TTerm TestGroup
```

### Converting Between Levels

For tests that construct `Term` values (like eta expansion tests),
use the `TTerm` constructor to lift terms to the meta-level:

```haskell
-- Helper to convert Term to TTerm Term
metaTerm :: Term -> TTerm Term
metaTerm = TTerm  -- TTerm is a newtype wrapper around Term

-- Usage in test helper
testCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
testCase name input output = Testing.testCaseWithMetadata (Phantoms.string name) tcase ...
  where
    tcase = Testing.testCaseEtaExpansion $ Testing.etaExpansionTestCase input output
```

Note that in the user's updated code, test helpers now accept `TTerm Term` directly, which is cleaner:

```haskell
-- Current pattern
testCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata

-- Called with term-level DSL wrapped in TTerm:
testCase "my test" (TTerm $ lambda "x" $ var "x") (TTerm $ lambda "x" $ var "x")
```

## Hierarchical Test Organization

### Aggregator Modules

Aggregator modules group related test modules:

```haskell
-- Hydra/Sources/Test/Checking/All.hs
module_ :: Module
module_ = Module (Namespace "hydra.test.checking.all") elements modules kernelTypesModules $
    Just "All type checking tests"
  where
    definitions = [Phantoms.toDefinition allTestsDef]
    modules = [
      Fundamentals.module_,
      AlgebraicTypes.module_,
      NominalTypes.module_,
      Collections.module_,
      Advanced.module_,
      Failures.module_]

allTestsDef :: TTermDefinition TestGroup
allTestsDef = definitionInModule module_ "allTests" $
    Phantoms.doc "The group of all checking tests" $
    Testing.testGroup (Phantoms.string "checking") Phantoms.nothing (Phantoms.list subgroups) (Phantoms.list [])
  where
    subgroups = [
      Phantoms.ref Fundamentals.allTestsDef,
      Phantoms.ref AlgebraicTypes.allTestsDef,
      Phantoms.ref NominalTypes.allTestsDef,
      Phantoms.ref Collections.allTestsDef,
      Phantoms.ref Advanced.allTestsDef,
      Phantoms.ref Failures.allTestsDef]
```

### Top-Level Test Suite

The top-level `TestSuite` module aggregates all test categories:

```haskell
-- Hydra/Sources/Test/TestSuite.hs (simplified)
module_ :: Module
module_ = Module ns definitions namespaces kernelTypesNamespaces $
    Just "Hydra's common test suite..."
  where
    definitions = [Phantoms.toDefinition allTests]
    namespaces = fst <$> testPairs

allTests :: TTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    doc "The group of all common tests" $
    Testing.testGroup (string "common") nothing (list subgroups) (list [])
  where
    subgroups = snd <$> testPairs

-- Test pairs organized into library and other categories
libPairs :: [(Namespace, TTermDefinition TestGroup)]
libPairs = [
  (Chars.ns, Chars.allTests),
  (Eithers.ns, Eithers.allTests),
  (Lists.ns, Lists.allTests),
  (Strings.ns, Strings.allTests),
  -- ... plus Equality, Flows, Literals, Logic, Maps, Math, Maybes, Pairs, Sets
  ]

otherPairs :: [(Namespace, TTermDefinition TestGroup)]
otherPairs = [
  (CheckingAll.ns, CheckingAll.allTests),
  (InferenceAll.ns, InferenceAll.allTests),
  (EtaExpansion.ns, EtaExpansion.allTests),
  (Formatting.ns, Formatting.allTests),
  -- ... plus Annotations, Hoisting, Json.*, Monads, Reduction, Rewriting,
  --   Serialization, Sorting, Substitution, Unification
  ]

testPairs = libPairs ++ otherPairs
```

This creates a hierarchy:
```
TestSuite (common)
├── Lib tests (Chars, Eithers, Equality, Flows, Lists, Literals, Logic, Maps, ...)
├── Checking (all checking tests)
│   ├── Fundamentals
│   ├── Algebraic Types
│   ├── Nominal Types
│   └── ...
├── Inference (all inference tests)
│   ├── Fundamentals
│   ├── Algebraic Types
│   └── ...
├── JSON tests (Coder, Parser, Roundtrip, Writer)
├── Eta Expansion
├── Formatting
├── Reduction, Rewriting, Hoisting
└── Substitution, Unification, ...
```

## Code Generation

### Generating Test Code

```haskell
-- In GHCi:
import Hydra.Sources.All
import Hydra.Generation

-- Generate all test modules
-- First arg: output directory
-- Second arg: universe modules (for dependency resolution)
-- Third arg: modules to generate
let allModules = mainModules ++ testModules
writeHaskell "../../dist/haskell/hydra-kernel/src/test/haskell" allModules baseTestModules
```

This generates separate files for each test module based on their namespaces:

```
dist/haskell/hydra-kernel/src/test/haskell/
└── Hydra/
    └── Test/
        ├── TestSuite.hs
        ├── TestGraph.hs
        ├── Checking/
        │   ├── All.hs
        │   ├── Fundamentals.hs
        │   ├── AlgebraicTypes.hs
        │   └── ...
        ├── Inference/
        │   ├── All.hs
        │   ├── Fundamentals.hs
        │   └── ...
        ├── EtaExpansion.hs
        └── Formatting.hs
```

### Cross-Language Generation

The same test modules can be generated for other languages:

```haskell
-- From hydra-ext package
import Hydra.Sources.All
import Hydra.Ext.Generation

-- Set up the universe
let allModules = mainModules ++ testModules

-- Generate Python tests
writePython "../../dist/python/hydra-kernel/src/test/python" allModules baseTestModules

-- Generate Java tests
writeJava "../../dist/java/hydra-kernel/src/test/java" allModules baseTestModules
```

## Best Practices

### 1. Use the Test Kernel

Always define shared test types and terms in the test kernel modules rather than duplicating them:

```haskell
-- Good: Reference test kernel
project (ref TestTypes.testTypePersonNameDef) (Core.name "firstName")

-- Bad: Hardcode type name
project (Core.name "Person") (Core.name "firstName")
```

### 2. Keep the Common Test Suite Language-Agnostic

The common test suite (`hydra.test.*`) must not depend on any `hydra.ext.*` module.
Every test runner — Haskell, Java, Python, and future implementations — must be able
to run the common tests without shipping language-specific extension modules. If a
test needs data values that happen to exist in an extension module (e.g., operator
definitions), define them locally in the test module instead of importing them.

### 3. Consistent Naming

- Test modules: `Hydra.Sources.Test.CategoryName`
- Root binding: Always `allTestsDef`
- Helper function: Always `define`
- Namespace: `hydra.test.categoryName` (camelCase)

### 4. Use Meta-Level Functions for Structure

When constructing test groups, always use the meta-level DSL:

```haskell
-- Correct
Testing.testGroup (Phantoms.string "name") Phantoms.nothing (Phantoms.list subgroups) (Phantoms.list [])

-- Incorrect
Testing.testGroup "name" nothing (list subgroups) (list [])  -- Wrong DSL level
```

### 5. Document Your Tests

Add documentation to test groups:

```haskell
allTestsDef :: TTermDefinition TestGroup
allTestsDef = define "allTests" $
  Phantoms.doc "Clear description of what this test group validates" $
  Testing.testGroup (Phantoms.string "name") ...
```

### 6. Organize by Functionality

Group tests by the functionality they validate, not by implementation details:
- ✓ Fundamentals, Algebraic Types, Nominal Types
- ✗ Chapter 1, Chapter 2, Misc Tests

## Adding New Tests

To add a new test module:

1. **Create the test module** in `packages/hydra-haskell/src/main/haskell/Hydra/Sources/Test/YourTest.hs`

2. **Define the module structure**:
   ```haskell
   module_ :: Module
   module_ = Module (Namespace "hydra.test.yourTest") elements
       [TestGraph.module_]
       kernelTypesModules
       (Just "Description")
     where
       definitions = [Phantoms.toDefinition allTestsDef]
   ```

3. **Create test cases** using appropriate helper functions

4. **Add to parent aggregator** (or TestSuite directly):
   ```haskell
   -- In Checking/All.hs or TestSuite.hs
   modules = [
     ...,
     YourTest.module_]

   subgroups = [
     ...,
     ref YourTest.allTestsDef]
   ```

5. **Generate and verify**:
   ```bash
   stack ghci
   import Hydra.Sources.All
   import Hydra.Generation
   let allModules = mainModules ++ testModules
   writeHaskell "../../dist/haskell/hydra-kernel/src/test/haskell" allModules baseTestModules
   :q
   stack test
   ```

## See Also

- [Testing Wiki](https://github.com/CategoricalData/hydra/wiki/Testing) - Overview of Hydra's testing approach
- [Implementation Wiki](https://github.com/CategoricalData/hydra/blob/main/docs/implementation.md) -
  Details on the DSL system
- [Test Suite Source](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-haskell/src/main/haskell/Hydra/Sources/Test)
