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

8. **Separate I/O from pure logic**: When promoting code from an I/O-heavy module, extract as much pure logic
   as possible into the promoted module. The I/O wrapper in Haskell should become a thin shell that:
   - Reads input (files, network, etc.)
   - Calls the generated pure functions
   - Handles errors
   - Writes output

   This maximizes code reuse when porting to other languages.

## Related Recipes

- [Extending Hydra Core](extending-hydra-core.md) - Adding new types to the kernel
- [Syncing Python](syncing-python.md) - Generating Python from Hydra modules
- [New Implementation](new-implementation.md) - Adding a new target language
