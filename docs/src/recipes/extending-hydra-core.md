# Adding New Type and Term Constructors to Hydra Core

This guide documents the process of adding native support for new type constructors and corresponding term constructors to Hydra Core. This was documented while adding `Either` type support and serves as a reference for adding other constructs like pairs, sum types, etc.

---

## Table of Contents

- [Overview](#overview)
- [Prerequisites](#prerequisites)
- [The Bootstrap Problem](#the-bootstrap-problem)
- [Step-by-Step Guide](#step-by-step-guide)
  - [Step 1: Update Core Schema](#step-1-update-core-schema)
  - [Step 2: Verify DSL Constructors](#step-2-verify-dsl-constructors)
  - [Step 3: Add Type Inference](#step-3-add-type-inference)
  - [Step 3.5: Add Type Checking](#step-35-add-type-checking)
  - [Step 4: Update Rewriting Functions](#step-4-update-rewriting-functions)
  - [Step 5: Update Other Term Traversals](#step-5-update-other-term-traversals)
  - [Step 6: Register Library Functions](#step-6-register-library-functions)
  - [Step 7: Initial Build](#step-7-initial-build)
  - [Step 8: Solve Bootstrap Problem](#step-8-solve-bootstrap-problem)
  - [Step 9: Rebuild](#step-9-rebuild)
  - [Step 10: Run Code Generation](#step-10-run-code-generation)
  - [Step 11: Final Verification](#step-11-final-verification)
  - [Step 12: Add Comprehensive Tests](#step-12-add-comprehensive-tests)
- [Common Pitfalls](#common-pitfalls)
- [Tips for Success](#tips-for-success)
- [File Modification Checklist](#file-modification-checklist)

---

## Overview

Adding a new type/term constructor to Hydra Core involves:

1. **Schema updates** - Defining the new constructors in the type system
2. **Type inference** - Teaching the type checker about the new constructors
3. **Rewriting/traversal** - Updating all term manipulation functions
4. **Code generation** - Adding target language encoding support
5. **Bootstrap patching** - Manually patching generated files to break circular dependencies

The most challenging aspect is the **bootstrap problem**: the code generator needs to understand the new constructors to generate itself.

---

## Prerequisites

- Understanding of Hydra's type system and DSL
- Familiarity with the Haskell DSL used in `Hydra.Sources` modules
- Access to the Hydra codebase with write permissions
- `stack` build tool installed

---

## The Bootstrap Problem

Hydra uses **self-generation**: the schema definitions in `Hydra.Sources` generate Haskell code that includes the code generator itself. When adding new constructors:

```
New DSL Code → Needs Generator → Generator Needs New Code → ⚠️  Circular Dependency
```

**Solution:** Manually patch generated files as an intermediate step, then regenerate cleanly.

---

## Step-by-Step Guide

### Step 1: Update Core Schema

**File:** `src/main/haskell/Hydra/Sources/Kernel/Types/Core.hs`

Add your new type constructor to the `Type` union and your new term constructor to the `Term` union.

**Example (Either):**

```haskell
def "Term" $
  doc "A data term" $
  union [
    "annotated">:
      doc "A term annotated with metadata" $
      core "AnnotatedTerm",
    "application">:
      doc "A function application" $
      core "Application",
    "either">:                                    -- ← NEW
      doc "An either value" $
      Types.either_ (core "Term") (core "Term"),  -- ← NEW
    "function">:
      doc "A function term" $
      core "Function",
    -- ... rest of cases
  ]
```

**Key Points:**
- Maintain alphabetical order within the union
- Use appropriate type combinators:
  - `Types.either_` for Either types
  - `optional` for Maybe types
  - `list` for list types
  - `Types.map` for map types
- Add supporting type definitions if needed (e.g., `EitherType`)

---

### Step 2: Verify DSL Constructors

**File:** `src/main/haskell/Hydra/Dsl/Phantoms.hs`

Check that term-level constructors exist in the DSL:

```haskell
-- Example: Either constructors (already present)
left :: TTerm a -> TTerm (Either a b)
left (TTerm term) = TTerm $ Terms.left term

right :: TTerm b -> TTerm (Either a b)
right (TTerm term) = TTerm $ Terms.right term
```

If missing, add them following existing patterns like `just` and `nothing` for `Maybe`.

---

### Step 3: Add Type Inference

**File:** `src/main/haskell/Hydra/Sources/Kernel/Terms/Inference.hs`

#### 3.1: Create Inference Function

```haskell
inferTypeOfEitherDef :: TBinding (InferenceContext -> Prelude.Either Term Term -> Flow s InferenceResult)
inferTypeOfEitherDef = define "inferTypeOfEither" $
  doc "Infer the type of an either value" $
  "cx" ~> "e" ~>
  Eithers.either_
    ("l" ~>
      "r1" <<~ (ref inferTypeOfTermDef @@ var "cx" @@ var "l" @@ string "either left value") $
      "iterm" <~ Typing.inferenceResultTerm (var "r1") $
      "leftType" <~ Typing.inferenceResultType (var "r1") $
      "subst" <~ Typing.inferenceResultSubst (var "r1") $
      "rightType" <<~ ref freshVariableTypeDef $
      "eitherTerm" <~ (Core.termEither $ left $ var "iterm") $
      -- Add two type applications: left(x)⟨leftType⟩⟨rightType⟩
      "termWithLeftType" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "eitherTerm") (var "leftType")) $
      "termWithBothTypes" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "termWithLeftType") (var "rightType")) $
      "eitherType" <~ (Core.typeEither $ Core.eitherType (var "leftType") (var "rightType")) $
      ref yieldCheckedDef @@ var "termWithBothTypes" @@ var "eitherType" @@ var "subst")
    ("r" ~>
      -- Mirror logic for right values with type applications
      "r1" <<~ (ref inferTypeOfTermDef @@ var "cx" @@ var "r" @@ string "either right value") $
      "iterm" <~ Typing.inferenceResultTerm (var "r1") $
      "rightType" <~ Typing.inferenceResultType (var "r1") $
      "subst" <~ Typing.inferenceResultSubst (var "r1") $
      "leftType" <<~ ref freshVariableTypeDef $
      "eitherTerm" <~ (Core.termEither $ right $ var "iterm") $
      "termWithLeftType" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "eitherTerm") (var "leftType")) $
      "termWithBothTypes" <~ (Core.termTypeApplication $ Core.typeApplicationTerm (var "termWithLeftType") (var "rightType")) $
      "eitherType" <~ (Core.typeEither $ Core.eitherType (var "leftType") (var "rightType")) $
      ref yieldCheckedDef @@ var "termWithBothTypes" @@ var "eitherType" @@ var "subst")
    (var "e")
```

#### 3.2: Add Case to Main Inference

In `inferTypeOfTermDef`, add:

```haskell
_Term_either>>: "e" ~> ref inferTypeOfEitherDef @@ var "cx" @@ var "e",
```

#### 3.3: Register in Module Exports

In the `elements` list (around line 69), add:

```haskell
elements = [
  el bindConstraintsDef,
  -- ...
  el inferTypeOfDef,
  el inferTypeOfEitherDef,        -- ← NEW (alphabetical order)
  el inferTypeOfEliminationDef,
  -- ...
]
```

#### 3.4: Add Imports

If using library functions:

```haskell
import qualified Hydra.Dsl.Lib.Eithers as Eithers
```

---

### Step 3.5: Add Type Checking

**File:** `src/main/haskell/Hydra/Sources/Kernel/Terms/Checking.hs`

Type checking reconstructs types from terms that already have type applications (added during inference).

```haskell
typeOfEitherDef :: TBinding (TypeContext -> [Type] -> Prelude.Either Term Term -> Flow s Type)
typeOfEitherDef = define "typeOfEither" $
  doc "Reconstruct the type of an either value" $
  "tx" ~> "typeArgs" ~> "et" ~>
  -- Verify exactly 2 type arguments provided
  "checkLength" <~ (
    "n" <~ Lists.length (var "typeArgs") $
    Logic.ifElse (Equality.equal (var "n") (int32 2))
      (Flows.pure unit)
      (Flows.fail $ "either type requires 2 type arguments, got " ++ Literals.showInt32 (var "n"))) $
  exec (var "checkLength") $
  Eithers.either_
    ("leftTerm" ~>
      "leftType" <<~ ref typeOfDef @@ var "tx" @@ list [] @@ var "leftTerm" $
      exec (ref checkTypeVariablesDef @@ var "tx" @@ var "leftType") $
      Flows.pure $ Core.typeEither $ Core.eitherType (var "leftType") (Lists.at (int32 1) $ var "typeArgs"))
    ("rightTerm" ~>
      "rightType" <<~ ref typeOfDef @@ var "tx" @@ list [] @@ var "rightTerm" $
      exec (ref checkTypeVariablesDef @@ var "tx" @@ var "rightType") $
      Flows.pure $ Core.typeEither $ Core.eitherType (Lists.at (int32 0) $ var "typeArgs") (var "rightType"))
    (var "et")
```

Add case to main checking function and register in elements list, similar to inference.

---

### Step 4: Update Rewriting Functions

**File:** `src/main/haskell/Hydra/Sources/Kernel/Terms/Rewriting.hs`

> **⚠️ Critical Distinction:** Use library elimination functions (`Eithers.either_`) for Haskell's **built-in** types, not `cases _Either`. The DSL `cases` construct only works on Hydra union types, not Haskell's native types.

#### 4.1: In `rewriteTermDef`

```haskell
_Term_either>>: "e" ~> Core.termEither $ Eithers.either_
  ("l" ~> left $ var "recurse" @@ var "l")
  ("r" ~> right $ var "recurse" @@ var "r")
  (var "e"),
```

#### 4.2: In `subtermsDef`

```haskell
_Term_either>>: "e" ~> Eithers.either_
  ("l" ~> list [var "l"])
  ("r" ~> list [var "r"])
  (var "e"),
```

#### 4.3: Add Import

```haskell
import qualified Hydra.Dsl.Lib.Eithers as Eithers
```

---

### Step 5: Update Other Term Traversals

Find other files that pattern match on Term constructors:

```bash
grep -l "Core\.TermApplication" src/main/haskell/Hydra/Sources/Kernel/Terms/*.hs
```

Common files to update:

#### 5.1: `Hydra/Sources/Kernel/Terms/Extract/Core.hs`

```haskell
_Term_either>>: "et" ~> Eithers.either_
  ("l" ~> Flows.map (unaryFunction left) (var "leftFun" @@ var "l"))
  ("r" ~> Flows.map (unaryFunction right) (var "rightFun" @@ var "r"))
  (var "et")
```

Add import: `import qualified Hydra.Dsl.Lib.Eithers as Eithers`

#### 5.2: Other files

Check `Hydra/Sources/Kernel/Terms/Monads.hs` and similar files, but **be careful**: some may use custom Hydra union types (like `Hydra.Mantle.Either`) which should continue using `cases _Either`.

---

### Step 6: Register Library Functions

**File:** `src/main/haskell/Hydra/Sources/Libraries.hs`

If using library functions, ensure they're registered:

```haskell
standardLibraries :: [Library]
standardLibraries = [
  hydraLibChars,
  hydraLibEithers,  -- ← Verify this is included
  hydraLibEquality,
  hydraLibFlows,
  -- ...
]
```

---

### Step 7: Initial Build

```bash
stack build
```

**Expected Result:** Build succeeds (source files compile correctly)

**If build fails:** Fix syntax/type errors before proceeding

---

### Step 8: Solve Bootstrap Problem

This is the most involved step. You must manually patch generated files that reference the new constructors.

#### 8.1: Patch `src/gen-main/haskell/Hydra/Inference.hs`

Add the inference function. Translate from the DSL source to plain Haskell:

```haskell
-- Around line 570
inferTypeOfEither :: (Typing_.InferenceContext -> Either Core.Term Core.Term -> Compute.Flow t0 Typing_.InferenceResult)
inferTypeOfEither cx e = ((\x -> case x of
    Left l -> (Flows.bind (inferTypeOfTerm cx l "either left value") (\r1 ->
      let leftType = (Typing_.inferenceResultType r1)
      in (Flows.bind freshVariableType (\rightType -> Flows.pure (Typing_.InferenceResult {
        Typing_.inferenceResultTerm = (Core.TermEither (Left l)),
        Typing_.inferenceResultType = (Core.TypeEither (Core.EitherType {
          Core.eitherTypeLeft = leftType,
          Core.eitherTypeRight = rightType})),
        Typing_.inferenceResultSubst = (Typing_.inferenceResultSubst r1)})))))
    Right r -> (Flows.bind (inferTypeOfTerm cx r "either right value") (\r1 ->
      let rightType = (Typing_.inferenceResultType r1)
      in (Flows.bind freshVariableType (\leftType -> Flows.pure (Typing_.InferenceResult {
        Typing_.inferenceResultTerm = (Core.TermEither (Right r)),
        Typing_.inferenceResultType = (Core.TypeEither (Core.EitherType {
          Core.eitherTypeLeft = leftType,
          Core.eitherTypeRight = rightType})),
        Typing_.inferenceResultSubst = (Typing_.inferenceResultSubst r1)})))))) e)
```

Add case to `inferTypeOfTerm` (around line 734):

```haskell
Core.TermEither v1 -> (inferTypeOfEither cx v1)
```

> **Note:** The inference function shown above is simplified. The actual implementation should include type applications as shown in Step 3.

#### 8.2: Patch `src/gen-main/haskell/Hydra/Checking.hs`

Similarly, add the type checking function. This requires exact type arguments:

```haskell
typeOfEither :: (Typing_.TypeContext -> [Core.Type] -> Either Core.Term Core.Term -> Compute.Flow t0 Core.Type)
typeOfEither tx typeArgs et = -- Implementation similar to Checking.hs source
```

Add case to main type checking function and follow similar patterns to inference.

#### 8.3: Patch `src/gen-main/haskell/Hydra/Ext/Haskell/Coder.hs`

Add type encoding in `encodeType` function (after `Core.TypeApplication`, before `Core.TypeFunction`):

```haskell
Core.TypeEither v1 ->
  let left = (Core.eitherTypeLeft v1)
      right = (Core.eitherTypeRight v1)
  in (Flows.map Utils.toTypeApplication (Flows.sequence [
    Flows.pure (Ast.TypeVariable (Utils.rawName "Either")),
    encode left,
    (encode right)]))
```

> **Note:** Term encoding (`encodeTerm`) may already be present if the constructor was partially implemented before.

#### 8.4: Verify `src/gen-main/haskell/Hydra/Core.hs`

Check that the generated Core has your constructor:

```bash
grep "TermEither" src/gen-main/haskell/Hydra/Core.hs
```

Should see something like:
```haskell
data Term =
  TermAnnotated AnnotatedTerm |
  TermApplication Application |
  TermEither (Either Term Term) |  -- ← Should be present
  -- ...
```

---

### Step 9: Rebuild

```bash
stack build
```

**Expected Result:** Build succeeds with manually patched files

**If build fails:** Check error messages carefully - they usually indicate which generated file needs additional patching

---

### Step 10: Run Code Generation

```bash
stack runghc tmp_write_haskell.hs
```

**Expected Result:**
- Process completes with only warnings (no errors)
- Outputs key/value pairs at the end
- All files in `src/gen-main/haskell/` are regenerated

**If code generation fails:**
- Error messages indicate which file/function needs patching
- Most common: missing encoder/decoder functions in Coder.hs

**Verification:**

```bash
# Check that generated files have your constructor
grep -c "TermEither" src/gen-main/haskell/Hydra/Inference.hs
# Should return a non-zero count
```

---

### Step 11: Final Verification

```bash
stack build
```

**Expected Result:** Clean build with all newly generated files

**Success Indicators:**
- No compilation errors
- Build completes successfully
- Generated files compile correctly

---

### Step 12: Add Comprehensive Tests

After verifying that the build succeeds, add thorough test coverage for both type inference and type checking.

#### 12.1: Type Checking Tests

**File:** `src/test/haskell/Hydra/CheckingSpec.hs`

Add test cases in the "Type checking" describe block. Use helper functions for cleaner syntax:

```haskell
-- Helper functions (already defined in the file)
tyapps :: TTerm a -> [Type] -> TTerm b          -- Multiple type applications
tylams :: [Name] -> TTerm a -> TTerm b          -- Multiple type lambda abstractions
forAll :: Name -> Type -> Type                   -- Polymorphic type quantification
```

**Test Categories:**

1. **Basic values** (isolated Either terms are polymorphic):
```haskell
expectTermWithType "left int"
  (left $ int32 42)
  (tylam "t0" $ tyapps (left $ int32 42) [Types.int32, Types.var "t0"])
  (Types.forAll "t0" $ Types.either_ Types.int32 (Types.var "t0"))
```

2. **Polymorphic in lambdas**:
```haskell
expectTermWithType "lambda returning left"
  (lam "x" $ left $ var "x")
  (tylams ["t0", "t1"] $ lam "x" $ tyapps (left $ var "x") [Types.var "t0", Types.var "t1"])
  (Types.forAll "t0" $ Types.forAll "t1" $
    Types.fun (Types.var "t0") (Types.either_ (Types.var "t0") (Types.var "t1")))
```

3. **Concrete context** (type resolved by context):
```haskell
expectTermWithType "tuple with left"
  (tuple [Types.string, Types.either_ Types.int32 Types.boolean]
         [str "hello", left $ int32 42])
  (tuple [Types.string, Types.either_ Types.int32 Types.boolean]
         [str "hello", tyapps (left $ int32 42) [Types.int32, Types.boolean]])
  (Types.tuple [Types.string, Types.either_ Types.int32 Types.boolean])
```

4. **Nested and complex types**:
```haskell
expectTermWithType "nested eithers"
  (left $ right $ int32 42)
  (tylam "t0" $ tyapps (left $ tyapps (right $ int32 42) [Types.string, Types.int32])
    [Types.either_ Types.string Types.int32, Types.var "t0"])
  (Types.forAll "t0" $
    Types.either_ (Types.either_ Types.string Types.int32) (Types.var "t0"))
```

**Key Testing Principles:**

- **Type applications are required**: Either terms need TWO type applications: `left(x)⟨leftType⟩⟨rightType⟩`
- **Isolated values are polymorphic**: Like `nothing`, standalone Either values have polymorphic types
- **Use `tyapps` helper**: Cleaner than nested `tyapp (tyapp ...)`  calls
- **Context resolves types**: When used in typed contexts (tuples, records), polymorphism is resolved

#### 12.2: Type Inference Tests

**File:** `src/test/haskell/Hydra/InferenceSpec.hs`

Add inference test cases following the same patterns. Inference tests verify that types are correctly inferred without explicit type annotations.

#### 12.3: Run Tests

```bash
# Run checking tests specifically
stack test --test-arguments "--match 'Type checking'"

# Run inference tests specifically
stack test --test-arguments "--match 'Type inference'"

# Run all tests
stack test
```

**Expected Result:** All tests pass (e.g., 2278 examples, 0 failures)

---

## Common Pitfalls

| Error | Cause | Solution |
|-------|-------|----------|
| `No such field: either` | Term union missing the constructor in schema | Add to `src/main/haskell/Hydra/Sources/Kernel/Types/Core.hs` |
| `Variable not bound to type: hydra.inference.inferTypeOfX` | Function not registered in module exports | Add `el inferTypeOfXDef` to elements list |
| Using `cases _Either` on built-in types | Confusion between Hydra unions and Haskell types | Use library functions (`Eithers.either_`) for Haskell built-ins |
| `unexpected type: either<...>` in codegen | Coder doesn't know how to encode the type | Manually patch `Hydra/Ext/Haskell/Coder.hs` |
| Type signature mismatch | Using wrong Monad operations | Use `<<~` for `Flow` binds, `<~` for pure lets |

---

## Tips for Success

1. **Work incrementally** - Build after each major step to catch errors early

2. **Use existing patterns** - Study how `Maybe` is handled throughout the codebase

3. **Strategic grepping** - Find all places needing updates:
   ```bash
   grep -r "TermApplication" src/main/haskell/Hydra/Sources/Kernel/Terms/
   ```

4. **Understand the distinction:**
   - **Hydra union types** (like `Hydra.Mantle.Either`) → use `cases`
   - **Haskell built-in types** (like `Prelude.Either`) → use library functions

5. **Don't skip bootstrap patching** - Manual patching is necessary to break the circular dependency

6. **Check both Term and Type** - Most constructors need updates to both

7. **Follow alphabetical order** - Makes code easier to navigate and maintain

---

## File Modification Checklist

### Source Files (Always Modified)

- [ ] `src/main/haskell/Hydra/Sources/Kernel/Types/Core.hs`
  - [ ] Add to Term union
  - [ ] Add to Type union (if applicable)
  - [ ] Add supporting type definitions

- [ ] `src/main/haskell/Hydra/Sources/Kernel/Terms/Inference.hs`
  - [ ] Create `inferTypeOfXDef` function with type applications
  - [ ] Add case to `inferTypeOfTermDef`
  - [ ] Register in `elements` list
  - [ ] Add necessary imports

- [ ] `src/main/haskell/Hydra/Sources/Kernel/Terms/Checking.hs`
  - [ ] Create `typeOfXDef` function
  - [ ] Verify type arguments are provided correctly
  - [ ] Add case to main checking function
  - [ ] Register in `elements` list
  - [ ] Add necessary imports

- [ ] `src/main/haskell/Hydra/Sources/Kernel/Terms/Rewriting.hs`
  - [ ] Add case in `rewriteTermDef`
  - [ ] Add case in `subtermsDef`
  - [ ] Add case in other rewriting functions
  - [ ] Add necessary imports

### Source Files (Sometimes Modified)

- [ ] `src/main/haskell/Hydra/Dsl/Phantoms.hs` - Add DSL constructors if missing
- [ ] `src/main/haskell/Hydra/Sources/Kernel/Terms/Extract/Core.hs` - Add extraction logic
- [ ] `src/main/haskell/Hydra/Sources/Libraries.hs` - Register library functions

### Generated Files (Bootstrap Patches)

- [ ] `src/gen-main/haskell/Hydra/Inference.hs`
  - [ ] Add inference function implementation (with type applications)
  - [ ] Add case to main inference function

- [ ] `src/gen-main/haskell/Hydra/Checking.hs`
  - [ ] Add type checking function implementation
  - [ ] Add case to main checking function

- [ ] `src/gen-main/haskell/Hydra/Ext/Haskell/Coder.hs`
  - [ ] Add `encodeType` case for new Type constructor
  - [ ] Add `encodeTerm` case for new Term constructor (if needed)

- [ ] `src/gen-main/haskell/Hydra/Core.hs` - Verify constructors present

### Test Files (Always Modified)

- [ ] `src/test/haskell/Hydra/CheckingSpec.hs`
  - [ ] Add basic value tests (polymorphic cases)
  - [ ] Add lambda/function tests
  - [ ] Add concrete context tests (tuples, lists, records)
  - [ ] Add nested/complex type tests
  - [ ] Use `tyapps` helper for clean syntax

- [ ] `src/test/haskell/Hydra/InferenceSpec.hs`
  - [ ] Add corresponding inference tests

### Build and Test Checkpoints

- [ ] Initial build succeeds (after source changes)
- [ ] Build succeeds with bootstrap patches
- [ ] Code generation completes successfully
- [ ] Final build succeeds with regenerated files
- [ ] All tests pass with comprehensive coverage

---

## Example: Either Type Implementation

See commit history for the complete implementation of Either type support, which follows all steps in this guide.

**Key files modified:**
- Core schema: Added `either` to Term and Type unions
- Inference: Added `inferTypeOfEither` with type applications for polymorphic support
- Checking: Added `typeOfEither` requiring exactly 2 type arguments
- Rewriting: Used `Eithers.either_` for elimination
- Bootstrap patches: Manually updated Inference.hs and Coder.hs
- Tests: Added comprehensive test coverage (CheckingSpec.hs, InferenceSpec.hs)
- Result: Clean code generation, successful build, all 2278+ tests passing

---

## Further Reading

- [Hydra Type System](./Type-System.md) - Understanding Hydra's type system
- [DSL Guide](./DSL-Guide.md) - Using the Hydra DSL effectively
- [Code Generation Architecture](./Code-Generation.md) - How Hydra generates code

---

**Questions or Issues?** Open an issue on the Hydra repository with the `documentation` label.
