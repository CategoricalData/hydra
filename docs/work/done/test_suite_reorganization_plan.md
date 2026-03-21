# Test Suite Reorganization Plan

## Problem
Currently, `writeHaskell "src/gen-test/haskell" testModules` generates a single monolithic file `Hydra/Test/TestSuite.hs` because all test groups are elements of a single module with namespace `hydra.test.testSuite`.

## Solution
Follow the pattern used in `Sources/Kernel/Terms`: create separate source modules for each test category, where each module has its own namespace and defines `TBinding TestGroup` values.

## How Hydra Maps Modules to Files

1. **Each Module has a Namespace**: e.g., `Namespace "hydra.constants"`
2. **File path is determined by namespace**: The Haskell coder calls `namespaceToFilePath` which converts the namespace to a file path
   - `"hydra.constants"` → `Hydra/Constants.hs`
   - `"hydra.test.testSuite.inference.fundamentals"` → `Hydra/Test/TestSuite/Inference/Fundamentals.hs`
3. **Elements within a module go to the same file**: All bindings in a module's elements list are written to that module's file

## Current Structure

```haskell
-- TestSuite.hs
module Hydra.Sources.Test.TestSuite where

testSuiteNs = Namespace "hydra.test.testSuite"

testSuiteModule :: Module
testSuiteModule = Module testSuiteNs elements [...] [...]
  where
    elements = [
      allTestsEl,
      checkingTestsEl,  -- Binding for entire checking test tree
      inferenceTestsEl, -- Binding for entire inference test tree
      ...]

-- checkingTests is a TTerm TestGroup constructed from CheckingSuite
checkingTestsEl = encodedTestGroupToBinding testSuiteNs "checkingTests" checkingTests

-- This creates ONE element in ONE module → ONE file
```

## Target Structure

### 1. Convert Test Source Files to Module Definitions

Each test source file (e.g., `Inference/Fundamentals.hs`) needs to:

**Before:**
```haskell
module Hydra.Sources.Test.Inference.Fundamentals (fundamentalsTests) where
import ...

fundamentalsTests :: TTerm TestGroup
fundamentalsTests = supergroup "Fundamentals" [
  literalsTests,
  lambdasTests,
  ...]

literalsTests :: TTerm TestGroup
literalsTests = subgroup "Literals" [
  expectMono 1 [] (int32 42) T.int32,
  ...]
```

**After:**
```haskell
module Hydra.Sources.Test.Inference.Fundamentals where
import ...

module_ :: Module
module_ = Module (Namespace "hydra.test.testSuite.inference.fundamentals") elements
    []  -- dependencies
    testGraphModule :  -- or whatever dependencies needed
    Just "Inference tests for fundamental language features"
  where
    elements = [
      el fundamentalsTestsDef,
      el literalsTestsDef,
      el lambdasTestsDef,
      ...]

-- Helper function to create definitions in this module
define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- Each test group becomes a binding
fundamentalsTestsDef :: TBinding TestGroup
fundamentalsTestsDef = define "fundamentalsTests" $
  doc "Fundamental language feature tests" $
  supergroup "Fundamentals" [
    ref literalsTestsDef,
    ref lambdasTestsDef,
    ...]

literalsTestsDef :: TBinding TestGroup
literalsTestsDef = define "literalsTests" $
  subgroup "Literals" [
    expectMono 1 [] (int32 42) T.int32,
    ...]

lambdasTestsDef :: TBinding TestGroup
lambdasTestsDef = define "lambdasTests" $
  subgroup "Lambdas" [
    ...]
```

### 2. Create Aggregator Modules

**Inference/All.hs:**
```haskell
module Hydra.Sources.Test.Inference.All where

import qualified Hydra.Sources.Test.Inference.Fundamentals as Fundamentals
import qualified Hydra.Sources.Test.Inference.AlgebraicTypes as AlgebraicTypes
import qualified Hydra.Sources.Test.Inference.NominalTypes as NominalTypes
import qualified Hydra.Sources.Test.Inference.Failures as Failures
import qualified Hydra.Sources.Test.Inference.AlgorithmW as AlgorithmW
import qualified Hydra.Sources.Test.Inference.KernelExamples as KernelExamples

inferenceTestModules :: [Module]
inferenceTestModules = [
  Fundamentals.module_,
  AlgebraicTypes.module_,
  NominalTypes.module_,
  Failures.module_,
  AlgorithmW.module_,
  KernelExamples.module_]
```

**Checking/All.hs:** (similar structure for checking tests)

### 3. Update Main TestSuite

**TestSuite.hs:**
```haskell
module Hydra.Sources.Test.TestSuite where

import qualified Hydra.Sources.Test.Inference.All as Inference
import qualified Hydra.Sources.Test.Checking.All as Checking
-- other imports...

-- Collect all test modules
testModules :: [Module]
testModules = concat [
  Inference.inferenceTestModules,
  Checking.checkingTestModules,
  [formattingModule, etaExpansionModule, ...]]

-- Optional: create a top-level aggregator module
testSuiteModule :: Module
testSuiteModule = Module (Namespace "hydra.test.testSuite") elements
    testModules  -- Include all test modules as dependencies
    [] $
    Just "Hydra's common test suite"
  where
    elements = [
      el allTestsDef]

-- Top-level test group that references sub-modules
allTestsDef :: TBinding TestGroup
allTestsDef = define "allTests" $
  supergroup "All tests" [
    ref Checking.Fundamentals.fundamentalsTestsDef,
    ref Inference.Fundamentals.fundamentalsTestsDef,
    ...]
```

## Key Changes Summary

### For Each Test Source File:

1. **Add module definition**:
   ```haskell
   module_ :: Module
   module_ = Module (Namespace "hydra.test.testSuite.<category>.<subcategory>") elements deps modules desc
   ```

2. **Add define helper**:
   ```haskell
   define :: String -> TTerm a -> TBinding a
   define = definitionInModule module_
   ```

3. **Convert test groups to bindings**:
   ```haskell
   -- Before:
   myTests :: TTerm TestGroup
   myTests = supergroup "My Tests" [...]

   -- After:
   myTestsDef :: TBinding TestGroup
   myTestsDef = define "myTests" $
     doc "Description" $
     supergroup "My Tests" [...]
   ```

4. **Reference other test groups by binding**:
   ```haskell
   -- Before:
   supergroup "Parent" [childTests, otherTests]

   -- After:
   supergroup "Parent" [ref childTestsDef, ref otherTestsDef]
   ```

5. **Export module in elements**:
   ```haskell
   elements = [
     el myTestsDef,
     el childTestsDef,
     ...]
   ```

### Create Aggregator Modules:

- `Inference/All.hs` - exports `inferenceTestModules :: [Module]`
- `Checking/All.hs` - exports `checkingTestModules :: [Module]`

### Update Generation:

In `Generation.hs` or where tests are generated, use:
```haskell
writeHaskell "src/gen-test/haskell" TestSuite.testModules
```

## Result

After `writeHaskell "src/gen-test/haskell" testModules`:

```
src/gen-test/haskell/
└── Hydra/
    └── Test/
        ├── TestSuite.hs                          -- Top-level aggregator
        └── TestSuite/
            ├── Inference/
            │   ├── Fundamentals.hs               -- fundamentalsTests, literalsTests, lambdasTests, etc.
            │   ├── AlgebraicTypes.hs             -- algebraicTypesTests, listsTests, productsTests, etc.
            │   ├── NominalTypes.hs               -- nominalTypesTests, recordsTests, etc.
            │   ├── Failures.hs                   -- failureTests
            │   ├── AlgorithmW.hs                 -- algorithmWTests
            │   └── KernelExamples.hs             -- kernelExamplesTests
            └── Checking/
                ├── Fundamentals.hs               -- fundamentalsTests, literalsTests, variablesTests, etc.
                ├── AlgebraicTypes.hs             -- algebraicTypesTests, unitTests, pairsTests, etc.
                ├── NominalTypes.hs               -- nominalTypesTests, recordsTests, unionsTests, etc.
                ├── Collections.hs                -- collectionsTests, listsTests, setsTests, mapsTests
                ├── Advanced.hs                   -- advancedTests, annotatedTermsTests, flowsTests
                └── Failures.hs                   -- failuresTests
```

## Benefits

1. **Maintainability**: Generated test files mirror the source organization
2. **Modularity**: Each test category is in its own file
3. **Clarity**: File structure reflects logical test organization
4. **Consistency**: Follows the same pattern as kernel terms
5. **Scalability**: Easy to add new test categories without bloating a single file
