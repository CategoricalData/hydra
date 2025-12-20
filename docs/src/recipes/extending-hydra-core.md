# Adding new type and term constructors to Hydra Core

This guide documents the process of adding native support for new type constructors and corresponding term constructors to Hydra Core. This was documented while adding `Either` type support and serves as a reference for adding other constructs like pairs, sum types, etc.

---

## Table of Contents

- [Overview](#overview)
- [Prerequisites](#prerequisites)
- [The Bootstrap Problem](#the-bootstrap-problem)
- [Step-by-Step Guide](#step-by-step-guide)
  - [Step 1: Update Core Schema](#step-1-update-core-schema)
  - [Step 2: Verify DSL Constructors](#step-2-verify-dsl-constructors)
  - [Step 2.5: Update Variants and Mantle DSL](#step-25-update-variants-and-mantle-dsl)
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
  - [Step 11.5: Update Language Support](#step-115-update-language-support-optional)
  - [Step 12: Add Comprehensive Tests](#step-12-add-comprehensive-tests)
- [Common Pitfalls](#common-pitfalls)
- [Tips for Success](#tips-for-success)
- [File Modification Checklist](#file-modification-checklist)

---

## Overview

This guide documents how the `Either` type was added to Hydra Core and serves as a template for adding other constructors in the future. While `Either` is now built-in (available as `hydra.core.EitherType` and `Term.either`), the process described here can be followed for adding new type/term constructors like pairs, sum types, or other constructs.

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
  - See the [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) wiki page for core concepts
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

### Step 2.5: Update Variants and Mantle DSL

#### 2.5.1: Update Hydra.Variants

**File:** `src/gen-main/haskell/Hydra/Variants.hs`

This is a generated file, but you'll need to manually patch it during bootstrap. Add two things:

1. **Add case to `termVariant` function:**
```haskell
termVariant :: (Core.Term -> Mantle.TermVariant)
termVariant x = case x of
  Core.TermAnnotated _ -> Mantle.TermVariantAnnotated
  Core.TermApplication _ -> Mantle.TermVariantApplication
  Core.TermEither _ -> Mantle.TermVariantEither  -- ← ADD THIS
  Core.TermFunction _ -> Mantle.TermVariantFunction
  -- ...
```

2. **Add to `termVariants` list:**
```haskell
termVariants :: [Mantle.TermVariant]
termVariants = [
  Mantle.TermVariantAnnotated,
  Mantle.TermVariantApplication,
  Mantle.TermVariantEither,  -- ← ADD THIS (alphabetical order)
  Mantle.TermVariantFunction,
  -- ...
]
```

Similarly, if adding a Type constructor, update `typeVariant` and `typeVariants`.

#### 2.5.2: Update Hydra.Dsl.Mantle

**File:** `src/main/haskell/Hydra/Dsl/Mantle.hs`

Add DSL helpers for the variant metadata:

1. **Add case to `termVariant` pattern match:**
```haskell
termVariant :: TermVariant -> TTerm TermVariant
termVariant v = unitVariant _TermVariant $ case v of
  TermVariantAnnotated -> _TermVariant_annotated
  TermVariantApplication -> _TermVariant_application
  TermVariantEither -> _TermVariant_either  -- ← ADD THIS
  TermVariantFunction -> _TermVariant_function
  -- ...
```

2. **Add helper function:**
```haskell
termVariantEither :: TTerm TermVariant
termVariantEither = unitVariant _TermVariant _TermVariant_either
```

Place in alphabetical order among the other `termVariantX` helpers.

#### 2.5.3: Update Meta Enum Definitions (CRITICAL!)

**File:** `src/main/haskell/Hydra/Sources/Kernel/Types/Meta.hs`

> **⚠️ CRITICAL:** You MUST add your new constructor to the TermVariant and TypeVariant enum definitions in the Meta module. This is the source of the "No such field: X" error during code generation.

Add to the `TermVariant` enum (around line 70-91):
```haskell
def "TermVariant" $
  doc "The identifier of a term expression constructor" $
  enum [
    "annotated",
    "application",
    "either",      -- ← ADD THIS (alphabetical order)
    "function",
    -- ... rest of variants
  ]
```

Add to the `TypeVariant` enum (around line 99-118):
```haskell
def "TypeVariant" $
  doc "The identifier of a type constructor" $
  enum [
    "annotated",
    "application",
    "either",      -- ← ADD THIS (alphabetical order)
    "forall",
    "function",
    -- ... rest of variants
  ]
```

**Why this is necessary:** The `Variants` module uses these enums to map between Term/Type constructors and their metadata. The enums are defined in the `hydra.meta` module (in the source file [`Hydra/Sources/Kernel/Types/Meta.hs`](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Types/Meta.hs)), which provides metadata and reflection types that describe the structure of Hydra core types and terms. Without adding your constructor here, code generation will fail with "No such field: X".

---

### Step 3: Add Type Inference

**File:** `src/main/haskell/Hydra/Sources/Kernel/Terms/Inference.hs`

#### 3.1: Create Inference Function

> **⚠️ Variable Naming:** Avoid using variable names that clash with Prelude functions (`fst`, `snd`, `head`, `tail`, `map`, `filter`, etc.). The DSL code generates Haskell that imports Prelude, so these names will cause compilation errors. Use descriptive names like `pairFst`, `pairSnd`, `listHead`, etc.

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
import qualified Hydra.Dsl.Meta.Lib.Eithers as Eithers
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

> **⚠️ Critical Distinction:** Use library elimination functions (`Eithers.either_`) for Haskell's **built-in** types like `Prelude.Either`, `Prelude.Maybe`, etc. The DSL `cases` construct only works on Hydra union types (union types defined with the `union` combinator in Core.hs), not Haskell's native algebraic data types.

> **⚠️ Important:** This file contains multiple rewriting functions with different characteristics (pure vs monadic, with/without context, with/without accumulator). Add your constructor case to **all of them**, following the existing patterns.

**Key functions to update:**
- `rewriteTermDef` - Pure rewriting
- `rewriteTermMDef` - Monadic rewriting
- `rewriteTermWithContextDef` / `rewriteTermWithContextMDef` - Context-passing variants
- `rewriteAndFoldTermDef` / `rewriteAndFoldTermMDef` - Accumulator variants
- `subtermsDef` - Subterm extraction

**Example (pure rewriting):**

```haskell
-- In rewriteTermDef
_Term_either>>: "e" ~> Core.termEither $ Eithers.either_
  ("l" ~> left $ var "recurse" @@ var "l")
  ("r" ~> right $ var "recurse" @@ var "r")
  (var "e"),
```

**Example (monadic with accumulator):**

```haskell
-- In rewriteAndFoldTermMDef
_Term_either>>: "e" ~> Eithers.either_
  ("l" ~>
    "rl" <<~ var "recurse" @@ var "val0" @@ var "l" $
    produce $ pair (first $ var "rl") (Core.termEither $ left $ second $ var "rl"))
  ("r" ~>
    "rr" <<~ var "recurse" @@ var "val0" @@ var "r" $
    produce $ pair (first $ var "rr") (Core.termEither $ right $ second $ var "rr"))
  (var "e"),
```

**Add import:**

```haskell
import qualified Hydra.Dsl.Meta.Lib.Eithers as Eithers
```

**Verification:** Check your constructor appears in the file:
```bash
grep "_Term_either" src/main/haskell/Hydra/Sources/Kernel/Terms/Rewriting.hs
```

---

### Step 5: Update Other Term Traversals

Find other files that pattern match on Term constructors. Replace `Either` with your constructor name:

```bash
# Find files that might need updates
grep -l "TermApplication\|TermLet\|TermList" src/main/haskell/Hydra/Sources/Kernel/Terms/*.hs

# After adding, verify your constructor is handled everywhere
grep -c "TermEither" src/main/haskell/Hydra/Sources/Kernel/Terms/*.hs
```

Common files to update:

#### 5.1: `Hydra/Sources/Kernel/Terms/Extract/Core.hs`

```haskell
_Term_either>>: "et" ~> Eithers.either_
  ("l" ~> Flows.map (unaryFunction left) (var "leftFun" @@ var "l"))
  ("r" ~> Flows.map (unaryFunction right) (var "rightFun" @@ var "r"))
  (var "et")
```

Add import: `import qualified Hydra.Dsl.Meta.Lib.Eithers as Eithers`

#### 5.2: Other traversal files

Check other term traversal files that may need updates:
- `Hydra/Sources/Kernel/Terms/Monads.hs`
- `Hydra/Sources/Kernel/Terms/Encode/Core.hs`
- `Hydra/Sources/Kernel/Terms/Decode/Core.hs`
- `Hydra/Sources/Kernel/Terms/Show/Core.hs`
- `Hydra/Sources/Kernel/Terms/Describe/Core.hs`

Follow the same pattern: use library elimination functions for Haskell built-in types.

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

### Step 11.5: Update Language Support (Optional)

If you want target languages (Python, Java, Scala, etc.) to support the new constructor, update their language definitions and regenerate code.

#### 11.5.1: Update Python Language Definition

**File:** `hydra-ext/src/main/haskell/Hydra/Ext/Sources/Python/Language.hs`

Add the new term variant to the supported features:

```haskell
-- Around line 115-130
"termVariants">: Sets.fromList $ list [
  Mantle.termVariantAnnotated,
  Mantle.termVariantApplication,
  Mantle.termVariantEither,  -- ← ADD THIS
  Mantle.termVariantFunction,
  Mantle.termVariantLet,
  -- ...
]
```

Add the type variant if you added a new type constructor:

```haskell
"typeVariants">: Sets.fromList $ list [
  Mantle.typeVariantAnnotated,
  Mantle.typeVariantApplication,
  Mantle.typeVariantEither,  -- ← ADD THIS
  Mantle.typeVariantFunction,
  -- ...
]
```

#### 11.5.2: Regenerate Python Code

From `hydra-ext`:

```bash
cd hydra-ext
stack runghc debug/WritePython.hs
```

Or use the REPL:
```haskell
stack exec ghci
> :l debug/WritePython.hs
> writePython "../hydra-python/src/main/python" kernelModules Nothing
```

This regenerates:
- `hydra-python/src/main/python/hydra/core.py` (adds `TermEither` constructor)
- `hydra-python/src/main/python/hydra/meta.py` (adds variant enums and type classes)
- `hydra-python/src/main/python/hydra/util.py` (adds utility types)
- All other Hydra kernel modules in Python

#### 11.5.3: Update Other Languages

Follow similar patterns for Java, Scala, etc.:

- **Java:** Update `Hydra/Ext/Sources/Java/Language.hs`, regenerate with `WriteJava.hs`
- **Scala:** Update `Hydra/Ext/Sources/Scala/Language.hs`, regenerate with `WriteScala.hs`

Each language coder needs to know how to encode/decode the new types in the target language.

#### 11.5.4: Verify Generated Code

```bash
# Python
cd hydra-python
python3 -m py_compile src/main/python/hydra/core.py
python3 -m py_compile src/main/python/hydra/meta.py
python3 -m py_compile src/main/python/hydra/util.py

# Java (if applicable)
cd hydra-java
./gradlew compileJava

# Scala (if applicable)
cd hydra-scala
sbt compile
```

**Note:** Target language support is optional but recommended if those languages are actively used in your project.

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
| `No such field: pair` during code generation | Missing from TermVariant/TypeVariant enums in Meta module | **CRITICAL:** Add to `TermVariant` and `TypeVariant` enum definitions in `src/main/haskell/Hydra/Sources/Kernel/Types/Meta.hs` |
| `Variable not in scope: fst` compilation error | Variable name clashes with Prelude function | Use descriptive names: `pairFst`, `pairSnd`, not `fst`, `snd` |
| `No such field: either` | Term union missing the constructor in Core schema | Add to `src/main/haskell/Hydra/Sources/Kernel/Types/Core.hs` |
| `Variable not bound to type: hydra.inference.inferTypeOfX` | Function not registered in module exports | Add `el inferTypeOfXDef` to elements list |
| Using `cases _Either` on built-in types | Confusion between Hydra unions and Haskell types | Use library functions (`Eithers.either_`) for Haskell built-ins |
| `unexpected type: either<...>` in codegen | Coder doesn't know how to encode the type | Manually patch `Hydra/Ext/Haskell/Coder.hs` |
| Type signature mismatch | Using wrong Monad operations | Use `<<~` for `Flow` binds, `<~` for pure lets |
| Missing rewrite function cases | Forgot to add to all rewriting variants | Search for `_Term_maybe` in `Rewriting.hs` to find all functions needing updates |
| Variants not updated | Missing from `Hydra.Variants` or `Hydra.Dsl.Mantle` | Add to `termVariant` function, `termVariants` list, and DSL helpers |
| Language support missing | Constructor not in language definition | Add to `termVariants` in `Hydra/Ext/Sources/Python/Language.hs` (or other language) and regenerate |

---

## Tips for Success

1. **Work incrementally** - Build after each major step to catch errors early

2. **Use existing patterns** - Study how `Maybe` is handled throughout the codebase

3. **Strategic grepping** - Find all places needing updates (replace with your constructor):
   ```bash
   # Find files handling term constructors
   grep -r "TermApplication\|TermLet" src/main/haskell/Hydra/Sources/Kernel/Terms/

   # After adding, verify your constructor is everywhere it should be
   grep -r "TermEither" src/main/haskell/Hydra/Sources/Kernel/Terms/
   ```

4. **Understand the distinction:**
   - **Hydra union types** (defined with `union` combinator in DSL) → use `cases`
   - **Haskell built-in types** (like `Prelude.Either`, `Prelude.Maybe`) → use library functions (`Eithers.either_`, `Maybes.maybe`)

5. **Don't skip bootstrap patching** - Manual patching is necessary to break the circular dependency

6. **Check both Term and Type** - Most constructors need updates to both

7. **Follow alphabetical order** - Makes code easier to navigate and maintain

8. **Check all rewrite functions** - The `Rewriting.hs` file has multiple variants (pure/monadic, with/without context, with/without accumulator). Search the file for existing constructors like `_Term_maybe` to see all the places you need to add your constructor.

9. **Update variants metadata** - Both `Hydra.Variants` and `Hydra.Dsl.Mantle` need updates for language support and introspection

10. **Test target languages** - If you regenerate Python/Java/Scala, verify the generated code compiles

---

## File Modification Checklist

### Source Files (Always Modified)

- [ ] `src/main/haskell/Hydra/Sources/Kernel/Types/Core.hs`
  - [ ] Add to Term union
  - [ ] Add to Type union (if applicable)
  - [ ] Add supporting type definitions

- [ ] `src/main/haskell/Hydra/Sources/Kernel/Types/Meta.hs` (**CRITICAL!**)
  - [ ] Add to `TermVariant` enum definition
  - [ ] Add to `TypeVariant` enum definition (if applicable)

- [ ] `src/main/haskell/Hydra/Dsl/Mantle.hs`
  - [ ] Add case to `termVariant` pattern match
  - [ ] Add `termVariantX` helper function
  - [ ] Add case to `typeVariant` pattern match (if applicable)
  - [ ] Add `typeVariantX` helper function (if applicable)

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
  - [ ] Add cases to all `rewriteTerm*` functions (search for `_Term_maybe` to find them all)
  - [ ] Add case in `subtermsDef`
  - [ ] Add necessary imports

### Source Files (Sometimes Modified)

- [ ] `src/main/haskell/Hydra/Dsl/Phantoms.hs` - Add DSL constructors if missing
- [ ] `src/main/haskell/Hydra/Sources/Kernel/Terms/Extract/Core.hs` - Add extraction logic
- [ ] `src/main/haskell/Hydra/Sources/Libraries.hs` - Register library functions

### Generated Files (Bootstrap Patches)

- [ ] `src/gen-main/haskell/Hydra/Variants.hs`
  - [ ] Add case to `termVariant` function
  - [ ] Add to `termVariants` list
  - [ ] Add case to `typeVariant` function (if applicable)
  - [ ] Add to `typeVariants` list (if applicable)

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

### Language Support Files (Optional)

- [ ] `hydra-ext/src/main/haskell/Hydra/Ext/Sources/Python/Language.hs`
  - [ ] Add to `termVariants` list
  - [ ] Add to `typeVariants` list (if applicable)

- [ ] Regenerate Python code
  - [ ] Run `writePython` from hydra-ext
  - [ ] Verify `hydra-python/src/main/python/hydra/core.py`
  - [ ] Verify `hydra-python/src/main/python/hydra/meta.py`
  - [ ] Verify `hydra-python/src/main/python/hydra/util.py`
  - [ ] Compile Python modules

- [ ] Java (if needed)
  - [ ] Update `Hydra/Ext/Sources/Java/Language.hs`
  - [ ] Regenerate Java code

- [ ] Scala (if needed)
  - [ ] Update `Hydra/Ext/Sources/Scala/Language.hs`
  - [ ] Regenerate Scala code

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

- [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts) - Core Hydra concepts and type system
- [Testing](https://github.com/CategoricalData/hydra/wiki/Testing) - Common test suite architecture
- [Hydra Developers](https://github.com/CategoricalData/hydra/wiki/Hydra-developers) - Source code organization
- [Main README](https://github.com/CategoricalData/hydra) - Project overview

---

**Questions or Issues?** Open an issue on the [Hydra repository](https://github.com/CategoricalData/hydra/issues) or ask on the [LambdaGraph Discord](https://bit.ly/lg-discord).
