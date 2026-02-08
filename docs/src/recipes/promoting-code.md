# Promoting Raw Code to Hydra Modules

A step-by-step guide for "promoting" raw Haskell code into Hydra source modules that can be generated to multiple target languages.

## Prerequisites

- Familiarity with Hydra's DSL (Domain Specific Language) for constructing terms
- Understanding of the module structure in `Hydra.Sources.*`
- The raw code to be promoted should be pure (no I/O operations)

## Overview

"Promotion" is the process of converting raw Haskell code into Hydra source modules. Raw Haskell code uses native Haskell syntax directly, while Hydra source modules use a DSL to construct terms that can be generated to multiple languages (Haskell, Python, Java, etc.).

**When to promote code:**
- Code that needs to work across multiple language targets
- Pure functional code (no I/O)
- Code that's part of a larger system being ported to other languages

**What cannot be promoted:**
- I/O operations (file reading, network calls, console output)
- Code relying on language-specific features not supported by Hydra

## Example: Promoting GraphSON Coder

Here's a concrete example of promoting `Hydra.Ext.Staging.Pg.Graphson.Coder` (raw Haskell) to `Hydra.Ext.Sources.Pg.Graphson.Coder` (Hydra source module).

### Original Raw Haskell

```haskell
-- Raw Haskell in Hydra.Ext.Staging.Pg.Graphson.Coder

doubleValueToJson :: G.DoubleValue -> Json.Value
doubleValueToJson v = case v of
  G.DoubleValueFinite d -> Json.ValueNumber d
  G.DoubleValueInfinity -> Json.ValueString "Infinity"
  G.DoubleValueNegativeInfinity -> Json.ValueString "-Infinity"
  G.DoubleValueNotANumber -> Json.ValueString "NaN"
```

### Promoted Hydra Source

```haskell
-- Hydra source module in Hydra.Ext.Sources.Pg.Graphson.Coder

doubleValueToJson :: TBinding (G.DoubleValue -> JM.Value)
doubleValueToJson = define "doubleValueToJson" $
  doc "Convert a GraphSON DoubleValue to a JSON Value" $
  match G._DoubleValue Nothing [
    G._DoubleValue_finite>>: lambda "d" $ Json.valueNumber (var "d"),
    G._DoubleValue_infinity>>: constant $ Json.valueString (string "Infinity"),
    G._DoubleValue_negativeInfinity>>: constant $ Json.valueString (string "-Infinity"),
    G._DoubleValue_notANumber>>: constant $ Json.valueString (string "NaN")]
```

### Key Differences

| Raw Haskell | Hydra Source |
|-------------|--------------|
| `case v of` | `match G._DoubleValue Nothing [...]` |
| `G.DoubleValueFinite d -> ...` | `G._DoubleValue_finite>>: lambda "d" $ ...` |
| `Json.ValueNumber d` | `Json.valueNumber (var "d")` |
| Pattern matching on values | Pattern matching on names + lambdas |
| Type signatures with concrete types | Type signatures with `TBinding` wrapper |

## Step-by-Step Guide

### 1. Create the Source Module File

Create a new file in the appropriate location under `Hydra/Sources/` or `Hydra/Ext/Sources/`:

```haskell
module Hydra.Ext.Sources.Pg.Graphson.Coder where
```

### 2. Add Standard Imports

Use this template for term-level source modules:

```haskell
-- Standard imports for term-level sources
import Hydra.Kernel hiding (<names to avoid conflicts>)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Json     as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows    as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import qualified Hydra.Dsl.Meta.Lib.Strings  as Strings
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import           Hydra.Sources.Kernel.Types.All
-- Import generated phantom types for domain-specific types
import qualified Hydra.Pg.Graphson.Syntax as G
import           Prelude hiding ((++))
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
```

**Important:** The `(++)` operator from `Hydra.Dsl.Meta.Phantoms` is for `TTerm String` concatenation. Use `L.++` for regular list concatenation.

Additional DSL modules available:
- `Chars` - Character operations (`isSpace`, `toUpper`, `toLower`, etc.)
- `Eithers` - Either operations (`map`, `bind`, `mapList`)
- `Equality` - Equality comparison (`equal`)
- `Flows` - Flow monad operations (`pure`, `bind`, `map`, `mapList`)
- `Sets` - Set operations (`empty`, `insert`, `union`, `toList`, etc.)

### 3. Define Namespace and Module

```haskell
ns :: Namespace
ns = Namespace "hydra.pg.graphson.coder"

module_ :: Module
module_ = Module ns elements
    [Reduction.ns, Rewriting.ns]  -- term dependencies (other term modules you call)
    (kernelTypesNamespaces L.++ [GraphsonSyntax.ns, JsonModel.ns]) $  -- type dependencies
    Just "Description of the module."
  where
    elements = [
      toBinding function1,
      toBinding function2,
      -- ... more bindings
      ]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_
```

**Module dependencies:**
- **Term dependencies**: Other source modules whose functions you call (e.g., `Reduction.ns` if you call `Reduction.reduceTerm`)
- **Type dependencies**: Namespaces of types your functions use. Always include `kernelTypesNamespaces` plus any domain-specific type modules.

If code generation fails with "No such schema type: hydra.foo.Bar", add the namespace containing that type to the type dependencies.

### 4. Translate Functions

#### Variable Bindings (Let Expressions)

Raw Haskell `let` or `where` bindings become `<~` (let binding operator):

```haskell
-- Raw: let x = expr1 in expr2
-- Promoted:
"x" <~ expr1 $
expr2

-- Multiple bindings:
"x" <~ expr1 $
"y" <~ expr2 $
finalExpr
```

#### Pattern Matching

Raw Haskell case expressions become `match` expressions:

```haskell
-- Raw: case x of ...
-- Promoted:
match _TypeName Nothing [
  _TypeName_variant1>>: lambda "x" $ ...,
  _TypeName_variant2>>: constant $ ...]
```

Use `constant` for branches that ignore their argument.

#### Record Access

Raw Haskell field access becomes `project`:

```haskell
-- Raw: recordField record
-- Promoted:
project _RecordType _RecordType_field @@ var "record"
```

#### Newtype Unwrapping and Wrapping

Raw Haskell newtype access becomes `unwrap`:

```haskell
-- Raw: unNewtype value
-- Promoted:
unwrap _Newtype @@ var "value"
```

Newtype construction becomes `wrap`:

```haskell
-- Raw: Newtype value
-- Promoted:
wrap _Newtype (var "value")
```

**Important:** When using `wrap` inside `Lists.map`, you need a lambda:

```haskell
-- Wrong: Lists.map (wrap _DataRow) (var "rows")
-- Correct:
Lists.map ("r" ~> wrap _DataRow (var "r")) (var "rows")
```

#### Record Construction

Raw record construction becomes `record`:

```haskell
-- Raw: MyRecord { field1 = x, field2 = y }
-- Promoted:
record _MyRecord [
  _MyRecord_field1>>: var "x",
  _MyRecord_field2>>: var "y"]
```

#### Function Application

Raw function application becomes `@@`:

```haskell
-- Raw: f x y
-- Promoted:
f @@ var "x" @@ var "y"
```

When calling a `TBinding` (a defined function in the same module), use `@@`:

```haskell
-- If you have: myHelper :: TBinding (String -> Int)
-- Call it with:
myHelper @@ var "str"

-- Not: myHelper (var "str")  -- This won't work
```

#### Lambdas

Raw lambdas become `~>` (preferred) or `lambda`/`lambdas`:

```haskell
-- Raw: \x -> expr
-- Promoted (preferred):
"x" ~> ...

-- Alternative:
lambda "x" $ ...

-- Raw: \x y -> expr
-- Promoted:
"x" ~> "y" ~> ...

-- Or using lambdas:
lambdas ["x", "y"] $ ...
```

**Important:** When passing a function to a higher-order function like `Lists.map` or `Eithers.map`,
you must use a lambda, not a direct function reference:

```haskell
-- Wrong: Lists.map Maybes.isNothing (var "xs")
-- Correct:
Lists.map ("x" ~> Maybes.isNothing (var "x")) (var "xs")

-- Wrong: Eithers.map just (var "either")
-- Correct:
Eithers.map ("x" ~> just (var "x")) (var "either")
```

#### Conditionals

Raw `if-then-else` becomes `Logic.ifElse`:

```haskell
-- Raw: if cond then x else y
-- Promoted:
Logic.ifElse (var "cond") (var "x") (var "y")
```

#### Lists

Raw list literals become `list`:

```haskell
-- Raw: [a, b, c]
-- Promoted:
list [a, b, c]
```

#### Map/List Operations

Use the DSL wrappers from `Hydra.Dsl.Meta.Lib.*`:

```haskell
-- Raw: map f xs
-- Promoted:
Lists.map ("x" ~> ...) (var "xs")

-- Raw: M.null m
-- Promoted:
Maps.null (var "m")

-- Raw: M.fromList pairs
-- Promoted:
Maps.fromList (var "pairs")

-- Raw: foldl f acc xs
-- Promoted:
Lists.foldl ("acc" ~> "x" ~> ...) (var "initial") (var "xs")

-- Raw: filter pred xs
-- Promoted:
Lists.filter ("x" ~> ...) (var "xs")

-- Raw: dropWhile pred xs
-- Promoted:
Lists.dropWhile ("x" ~> ...) (var "xs")
```

#### Either Operations

Use `Eithers` for Either-based operations:

```haskell
-- Raw: Left err
-- Promoted:
left (var "err")

-- Raw: Right val
-- Promoted:
right (var "val")

-- Raw: fmap f either
-- Promoted:
Eithers.map ("x" ~> ...) (var "either")

-- Raw: either >>= f (bind)
-- Promoted:
Eithers.bind (var "either") ("x" ~> ...)

-- Raw: mapM f xs (for Either)
-- Promoted:
Eithers.mapList ("x" ~> ...) (var "xs")
```

#### Flow (Monadic) Operations

For code using the `Flow` monad (Hydra's effect system):

```haskell
-- Raw: pure x
-- Promoted:
Flows.pure (var "x")

-- Raw: flow >>= f
-- Promoted:
Flows.bind (var "flow") ("x" ~> ...)

-- Raw: fmap f flow
-- Promoted:
Flows.map ("x" ~> ...) (var "flow")

-- Raw: mapM f xs (for Flow)
-- Promoted:
Flows.mapList ("x" ~> ...) (var "xs")
```

#### Type Conversions

For integer-to-float conversions, compose the appropriate primitives:

```haskell
-- Raw: fromIntegral i :: Double
-- Promoted (for Int32 -> Double):
Literals.bigintToBigfloat $ Literals.int32ToBigint $ var "i"
```

### 5. Add to Module Registry

Add the import and register the module:

```haskell
-- In Hydra/Ext/Sources/All.hs

import qualified Hydra.Ext.Sources.Pg.Graphson.Coder as GraphsonCoder

hydraExtModules :: [Module]
hydraExtModules = [
  -- ...
  GraphsonCoder.module_,
  -- ...
  ]
```

### 6. Build and Test

```bash
cd hydra-ext
stack build
```

Fix any type errors, which usually involve:
- Missing phantom type imports
- Incorrect DSL function usage
- Namespace issues

### 7. Regenerate the Target Code

After the source module builds, regenerate the Haskell (or other language) code:

```bash
# In GHCi (stack ghci)
import Hydra.Ext.Sources.All
import Hydra.Ext.Generation
import qualified Hydra.Ext.Sources.YourModule as YourModule
import qualified Data.List as L

writeHaskell "src/gen-main/haskell" (mainModules `L.union` hydraExtModules) [YourModule.module_]
```

**Important:** The first argument to `writeHaskell` is the universe of all modules (for type resolution).
Use `mainModules `L.union` hydraExtModules` to include both kernel and extension modules.

### 8. Update the Consumer Code

After regeneration, update the code that was using the raw Haskell functions to use the generated module:

```haskell
-- Before:
import MyRawModule (myFunction)

-- After:
import qualified Hydra.Generated.MyModule as MyModule

-- Then use:
MyModule.myFunction args
```

## Common Patterns

### Constructing Union Values

```haskell
-- Raw: Json.ValueString s
-- Promoted:
Json.valueString (var "s")

-- For injection with specific names:
inject _TypeName _TypeName_variant @@ var "value"
```

### Optional Values

```haskell
-- Raw: Just x
-- Promoted:
just (var "x")

-- Raw: Nothing
-- Promoted:
nothing

-- Raw: maybe default f opt
-- Promoted:
Maybes.maybe (var "default") f (var "opt")

-- Raw: catMaybes xs
-- Promoted:
Maybes.cat (var "xs")
```

### Pairs/Tuples

```haskell
-- Raw: (a, b)
-- Promoted:
pair (var "a") (var "b")

-- Raw: fst p
-- Promoted:
Pairs.first (var "p")

-- Raw: snd p
-- Promoted:
Pairs.second (var "p")
```

### String Literals

```haskell
-- Raw: "hello"
-- Promoted:
string "hello"
```

### Boolean Literals

```haskell
-- Raw: True / False
-- Promoted:
boolean True / boolean False
```

## Checklist

When promoting code:

- [ ] Create source module file with proper namespace
- [ ] Add standard imports (watch for name conflicts)
- [ ] Define namespace, module, and `define` helper
- [ ] Translate all functions using DSL constructs
- [ ] Use phantom types from generated modules (e.g., `Hydra.Pg.Graphson.Syntax`)
- [ ] Add module to registry in `All.hs`
- [ ] Build and fix type errors
- [ ] Keep original "staging" code in place until generated code is verified

## Tips

1. **Start with simpler functions**: Promote helper functions first, then build up to more complex ones.

2. **Use phantom types**: Import generated phantom types (`_TypeName`, `_TypeName_variant`) from the generated modules.

3. **Watch for operator precedence**: Use parentheses liberally when combining DSL operators like `@@` and `$`.

4. **Check DSL exports**: If a function isn't found in a DSL module, check what's actually exported (e.g., `Maybes.cat` not `Maybes.catMaybes`).

5. **Compose type conversions**: There's no direct `int32ToBigfloat`; compose `int32ToBigint` with `bigintToBigfloat`.

6. **Avoid custom state types**: If your Haskell code uses a custom record type for state (e.g., in a fold),
   consider using nested tuples instead. Custom types require schema definitions, while tuples work directly:

   ```haskell
   -- Instead of: data ParseState = ParseState { acc :: [String], field :: String, inQuotes :: Bool }
   -- Use nested tuples: ((acc, field), inQuotes) :: (([String], String), Bool)

   -- Access with:
   "acc" <~ Pairs.first (Pairs.first $ var "state") $
   "field" <~ Pairs.second (Pairs.first $ var "state") $
   "inQuotes" <~ Pairs.second (var "state") $
   ...
   ```

7. **Missing library functions**: Some common Haskell functions may not exist in the DSL.
   You can define them yourself:

   ```haskell
   -- If you need 'any' but Lists doesn't export it:
   listAny :: TBinding ((a -> Bool) -> [a] -> Bool)
   listAny = define "listAny" $
     "pred" ~> "xs" ~>
       Logic.not $ Lists.null $ Lists.filter (var "pred") (var "xs")
   ```

8. **DSL module naming conventions**: Functions in DSL modules often have different names than their Haskell counterparts:
   - `Flows.getState` doesn't exist - use `Monads.getState`
   - `Maybes.catMaybes` doesn't exist - use `Maybes.mapMaybe ("x" ~> var "x")`
   - `Maps.values` doesn't exist - use `Maps.toList` and map over pairs
   - `Lists.any` doesn't exist - use foldl with `Logic.or`

   When in doubt, check the actual exports in `Hydra.Dsl.Meta.Lib.*` or `Hydra.Sources.Kernel.Terms.*`.

9. **Namespace collisions**: When your DSL module defines a function with the same name as something in `Hydra.Kernel`, you'll get ambiguity errors. Either:
   - Rename your function (e.g., `partitionDefs` instead of `partitionDefinitions`)
   - Hide the conflicting import: `import Hydra.Kernel hiding (partitionDefinitions)`

8. **Separate I/O from pure logic**: When promoting code from an I/O-heavy module, extract as much pure logic
   as possible into the promoted module. The I/O wrapper in Haskell should become a thin shell that:
   - Reads input (files, network, etc.)
   - Calls the generated pure functions
   - Handles errors
   - Writes output

   This maximizes code reuse when porting to other languages.

## Recommended Approach: Incremental Hybrid Testing

When promoting a large module like a language coder, use this incremental approach that provides continuous verification:

### Phase 1: Simplify Staging Code First

Before starting the actual promotion, refactor the staging (raw Haskell) code:

1. **Break up complex functions** into smaller, composable helpers
2. **Simplify pattern matching** to align with Hydra's case statement patterns
3. **Extract pure logic** from functions with side effects
4. **Remove unnecessary callbacks** - if functions take callbacks to break circular dependencies, consider whether direct calls would work
5. **Test the refactored staging code** to verify it still works correctly

This preparation makes the actual promotion much easier and catches issues early.

### Phase 2: Create DSL Module Structure

Create the source module file with:
- Standard imports
- Module namespace and dependencies
- Empty elements list

Build to verify the structure compiles.

### Phase 3: Incremental Promotion with Hybrid Testing

The key insight is to **keep both versions working simultaneously**:

1. **Import the generated module in the staging module**:
   ```haskell
   -- In Hydra.Ext.Staging.Python.Coder
   import qualified Hydra.Ext.Python.Coder as Generated
   ```

2. **Promote functions one at a time** to the DSL Sources module

3. **After regenerating**, comment out the corresponding function in staging:
   ```haskell
   -- PROMOTED: function is now in Generated module
   -- originalFunction :: ...
   -- originalFunction = ...
   ```

4. **Use the generated function from the import**:
   ```haskell
   -- Staging code now uses:
   Generated.originalFunction
   ```

5. **Test after each promotion** using the sync script (e.g., `bin/sync-python.sh`)

6. **Verify identical output** - the generated code should produce the same results

This approach:
- Provides immediate feedback when something breaks
- Makes it clear which functions have been promoted
- Allows incremental progress with minimal risk
- Documents the promotion progress visually

### Phase 4: Handle Interface Differences

When promoting, watch for functions with callback parameters used to break circular dependencies:

```haskell
-- Staging version (with callback)
encodeDefinition :: Env -> (Env -> Term -> Expression) -> Definition -> Expression

-- DSL version (direct call)
encodeDefinition :: TBinding (Env -> Definition -> Expression)
-- Calls encodeTermInline directly inside
```

The DSL can handle mutual recursion directly - functions can reference each other without explicit callbacks. Remove callback parameters and replace `var "callback" @@ args` with direct calls like `otherFunction @@ args`.

### Phase 5: Complete the Migration

Once all functions are promoted and the staging module just re-exports from the generated module:

1. Update consumers to import the generated module directly
2. Remove or archive the staging module
3. Update documentation

### Example: Regeneration Script

When regenerating, ensure you include all required modules:

```haskell
main = do
  writeHaskell "src/gen-main/haskell"
    (allHaskellModules <> hydraExtModules)  -- Full module universe
    [PythonCoder.module_]  -- Module to generate
  where
    allHaskellModules = kernelModules <> otherModules  -- Include CoderUtils, etc.
```

Common mistake: Using just `kernelModules` misses helper modules like `CoderUtils` that contain shared functionality.

### Lessons from Python Coder Promotion

Key issues encountered and their solutions:

1. **Callback parameters vs direct calls**: The original DSL versions added callback parameters to break circular dependencies. These are unnecessary - the DSL handles mutual recursion directly. Remove callbacks and use direct function references.

2. **TBinding reference vs callback**: When passing a TBinding function as a callback argument, you need a lambda wrapper:
   ```haskell
   -- Wrong: encodeMultiline @@ encodeTermMultiline
   -- Correct: encodeMultiline @@ ("e" ~> "t" ~> encodeTermMultiline @@ var "e" @@ var "t")
   ```

3. **Module dependencies in regeneration**: The `writeHaskell` function needs the complete universe of modules that might be referenced. Include `otherModules` (which contains `CoderUtils`) not just `kernelModules`.

4. **Interface alignment**: Ensure DSL function signatures match staging signatures exactly. Extra parameters break callers.

5. **Non-exhaustive patterns in nested cases**: When using nested `cases` expressions, each needs a default case:
   ```haskell
   -- Wrong - inner cases without default causes non-exhaustive pattern errors:
   cases _Outer (var "x") Nothing [
     _Outer_foo>>: "f" ~> cases _Inner (var "f") Nothing [...]]

   -- Correct - extract default logic and provide it to nested cases:
   "dfltLogic" <~ someDefaultExpr $
   cases _Outer (var "x") (Just $ var "dfltLogic") [
     _Outer_foo>>: "f" ~> cases _Inner (var "f") (Just $ var "dfltLogic") [...]]
   ```

6. **Metadata flags for generated imports**: When encoding produces output that requires imports (like `@lru_cache` decorators), ensure the corresponding metadata flag is set via `updateMeta`:
   ```haskell
   -- When adding a decorator that requires an import:
   updateMeta @@ (setMetaUsesLruCache @@ true)
   ```
   Missing metadata flags cause `NameError` in generated code because imports won't be added.

7. **Bindings with vs without type schemes**: When processing bindings, check whether a type scheme is present - bindings WITH type schemes typically need different handling (e.g., `encodeTermAssignment`) than bindings without:
   ```haskell
   Maybes.maybe
     (handleNoTypeScheme @@ ...)  -- No type scheme
     ("ts" ~> handleWithTypeScheme @@ var "ts" @@ ...)  -- Has type scheme
     (Core.bindingType $ var "binding")
   ```

### What Cannot Be Promoted

Some code is inherently "driver" or "orchestration" code that's best kept in hand-written Haskell:

1. **Module-level orchestration** (`moduleToPython`, `encodeModule`): Functions that:
   - Reorder definitions using topological sorting
   - Generate import statements based on collected metadata
   - Serialize ASTs to strings and map to file paths
   - Use `CM.mapM` for traversing lists in Flow context

2. **Test generation infrastructure**: Code that:
   - Creates test codecs and test generators
   - Builds file paths for test output
   - Uses complex Haskell features like `FlowState` pattern matching

These functions typically live in a thin wrapper module (e.g., `Hydra.Ext.Python.Module`) that:
- Imports everything from the Generated module
- Provides the entry points that call into generated encoding functions
- Handles final serialization and file I/O concerns

This pattern keeps the DSL focused on encoding logic while hand-written Haskell handles language-specific orchestration.

## Related Recipes

- [Extending Hydra Core](extending-hydra-core.md) - Adding new types to the kernel
- [Syncing Python](syncing-python.md) - Generating Python from Hydra modules
- [New Implementation](new-implementation.md) - Adding a new target language
