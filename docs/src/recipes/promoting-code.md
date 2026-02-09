# Promoting raw code to Hydra modules

A step-by-step guide for "promoting" raw Haskell code into Hydra source modules that can be generated to multiple target languages.
Haskell code can potentially be promoted when it uses APIs which were generated from Hydra type definitions.
As Hydra syntax is fairly close to Haskell syntax, promotion generally involves eliminating external dependencies,
rewriting the code to avoid unsupported language features, and finally transposing the code from Haskell syntax to Hydra syntax.

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
- Code which depends on external libraries, unless those dependencies can be replaced

## The promotion workflow

Promotion is done **incrementally, one function at a time**, with testing at each step. This is the recommended workflow:

1. **Simplify the staging code** — refactor the raw Haskell to prepare it for promotion (break up complex functions, extract pure logic, remove unnecessary callbacks). A useful technique is to edit the original function to bring it as close to Hydra syntax as possible *before* promoting: replace unsupported primitives with supported ones, replace complex pattern matching with simpler case expressions, and eliminate syntax features that have no DSL equivalent. Test the refactored code to verify it still works.
2. **Create the DSL module structure** — set up the source module file with standard imports, namespace, module definition, and an empty elements list. Build to verify it compiles.
3. **Promote one function** — translate it into the Hydra DSL and add it to the module's elements list.
4. **Regenerate** — generate the promoted function into `gen-main` (e.g., `writeHaskell`).
5. **Comment out the original** — in the staging module, comment out the original function. This keeps it available as a reference while the generated version takes over. Import and use the generated version instead.
6. **Test** — run the relevant test suite (e.g., `bin/sync-python.sh` or `./gradlew test`). Verify the generated code produces the same results.
7. **Repeat** steps 3–6 for the next function. The commented-out originals serve as a reference in case issues arise later in the promotion process.
8. **Complete the migration** — once all functions are promoted and verified, update consumers to import the generated module directly, and remove the staging module.

## Step-by-step guide

### 1. Simplify staging code first

Before starting the actual promotion, refactor the staging (raw Haskell) code:

1. **Break up complex functions** into smaller, composable helpers
2. **Simplify pattern matching** to align with Hydra's case statement patterns
3. **Extract pure logic** from functions with side effects
4. **Remove unnecessary callbacks** — if functions take callbacks to break circular dependencies, consider whether direct calls would work. The DSL can handle mutual recursion directly.
5. **Separate I/O from pure logic** — extract as much pure logic as possible. The I/O wrapper should become a thin shell that reads input, calls the generated pure functions, handles errors, and writes output.
6. **Test the refactored staging code** to verify it still works correctly

### 2. Create the source module file

Create a new file in the appropriate location under `Hydra/Sources/` or `Hydra/Ext/Sources/`:

```haskell
module Hydra.Ext.Sources.Pg.Graphson.Coder where
```

### 3. Add standard imports

The import block for a term-level source module is large but follows a consistent structure. Copy it from an existing source module such as the [Haskell Coder](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/src/main/haskell/Hydra/Sources/Haskell/Coder.hs). The block has these sections:

1. **Unqualified core imports** — `Hydra.Kernel`, `Hydra.Sources.Libraries`, `Hydra.Dsl.Meta.Phantoms`, `Hydra.Dsl.Meta.Lib.Strings`
2. **Qualified DSL imports** — `Hydra.Dsl.*` modules (Bootstrap, Annotations, Grammars, LiteralTypes, Literals, Types, Terms, etc.)
3. **Qualified meta DSL imports** — `Hydra.Dsl.Meta.*` modules (Accessors, Ast, Base, Coders, Compute, Core, Graph, Json, Module, Terms, Types, Variants, etc.)
4. **Qualified library DSL imports** — `Hydra.Dsl.Meta.Lib.*` (Chars, Eithers, Equality, Flows, Lists, Literals, Logic, Maps, Math, Maybes, Pairs, Sets)
5. **Qualified kernel sources imports** — `Hydra.Sources.Kernel.Terms.*` modules for calling other promoted functions (Reduction, Rewriting, Inference, etc.)
6. **Standard Haskell imports** — `Prelude hiding ((++))`, `Data.List as L`, `Data.Map as M`, `Data.Set as S`
7. **Domain-specific imports** — generated phantom types for the types your module uses (e.g., `Hydra.Ext.Haskell.Ast as H`)

Not every module needs all of these — include only what you use. The `(++)` from `Phantoms` is for `TTerm String` concatenation; use `L.++` for regular list concatenation.

### 4. Define namespace and module

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

### 5. Add to module registry

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

### 6. Promote functions incrementally

Translate functions one at a time into the DSL, adding each to the module's elements list. After each function:

1. Build: `stack build` in `hydra-ext`
2. Regenerate the target code (see "Regenerating target code" below)
3. Comment out the original staging function, keeping it as a reference
4. Import and use the generated version in the staging module:
   ```haskell
   -- In Hydra.Ext.Staging.Python.Coder
   import qualified Hydra.Ext.Python.Coder as Generated

   -- PROMOTED: now using generated version
   -- originalFunction :: ...
   -- originalFunction = ...
   ```
5. Test to verify identical behavior

Start with simpler helper functions and build up to more complex ones.

### 7. Regenerate target code

After promoting functions, regenerate the Haskell (or other language) code:

```haskell
-- In GHCi (stack ghci)
import Hydra.Ext.Sources.All
import Hydra.Ext.Generation
import qualified Hydra.Ext.Sources.YourModule as YourModule
import qualified Data.List as L

writeHaskell "src/gen-main/haskell" (mainModules `L.union` hydraExtModules) [YourModule.module_]
```

**Important:** The first argument to `writeHaskell` is the universe of all modules (for type resolution).
Use `mainModules `L.union` hydraExtModules` to include both kernel and extension modules.
Using just `kernelModules` instead of `mainModules` misses helper modules like `CoderUtils` that contain shared functionality.

### 8. Complete the migration

Once all functions are promoted and the staging module just re-exports from the generated module:

1. Update consumers to import the generated module directly
2. Remove or archive the staging module
3. Update documentation

## DSL translation reference

### Example: promoting a function

**Original raw Haskell:**

```haskell
-- Raw Haskell in Hydra.Ext.Staging.Pg.Graphson.Coder

doubleValueToJson :: G.DoubleValue -> Json.Value
doubleValueToJson v = case v of
  G.DoubleValueFinite d -> Json.ValueNumber d
  G.DoubleValueInfinity -> Json.ValueString "Infinity"
  G.DoubleValueNegativeInfinity -> Json.ValueString "-Infinity"
  G.DoubleValueNotANumber -> Json.ValueString "NaN"
```

Notice that while the code itself is written in pure Haskell, it uses GraphSON domain types which were defined using Hydra.

**Promoted Hydra source:**

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

### Variable bindings (let expressions)

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

### Pattern matching

Raw Haskell case expressions become `match` expressions:

```haskell
-- Raw: case x of ...
-- Promoted:
match _TypeName Nothing [
  _TypeName_variant1>>: lambda "x" $ ...,
  _TypeName_variant2>>: constant $ ...]
```

Use `constant` for branches that ignore their argument.

### Record access

Raw Haskell field access becomes `project`:

```haskell
-- Raw: recordField record
-- Promoted:
project _RecordType _RecordType_field @@ var "record"
```

### Newtype unwrapping and wrapping

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

### Record construction

Raw record construction becomes `record`:

```haskell
-- Raw: MyRecord { field1 = x, field2 = y }
-- Promoted:
record _MyRecord [
  _MyRecord_field1>>: var "x",
  _MyRecord_field2>>: var "y"]
```

### Function application

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

### Lambdas

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

### Conditionals

Raw `if-then-else` becomes `Logic.ifElse`:

```haskell
-- Raw: if cond then x else y
-- Promoted:
Logic.ifElse (var "cond") (var "x") (var "y")
```

### Lists

Raw list literals become `list`:

```haskell
-- Raw: [a, b, c]
-- Promoted:
list [a, b, c]
```

### Map/list operations

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

### Either operations

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

### Flow (monadic) operations

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

### Literals, constructors, and common types

```haskell
-- Strings and booleans
string "hello"
boolean True

-- Union values (use generated lowercase constructors)
Json.valueString (var "s")
-- Or with explicit injection:
inject _TypeName _TypeName_variant @@ var "value"

-- Optionals
just (var "x")
nothing
Maybes.maybe (var "default") ("x" ~> ...) (var "opt")
Maybes.cat (var "xs")

-- Pairs
pair (var "a") (var "b")
Pairs.first (var "p")
Pairs.second (var "p")

-- Type conversions (compose primitives; there's no direct int32ToBigfloat)
Literals.bigintToBigfloat $ Literals.int32ToBigint $ var "i"
```

### Mutual recursion

The DSL can handle mutual recursion directly — functions in the same module can reference each other without explicit callbacks. If the staging code passes callback parameters to break circular dependencies, remove them and replace `var "callback" @@ args` with direct calls like `otherFunction @@ args`.

## Tips

1. **Exact carbon copies**: The promoted function should be a carbon copy of the original function, transposed into Hydra syntax. Do not refactor behavior during promotion — any behavioral refactoring should be done beforehand in the staging code, where it can be tested independently.

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
   - `Flows.getState` doesn't exist — use `Monads.getState`
   - `Maybes.catMaybes` doesn't exist — use `Maybes.mapMaybe ("x" ~> var "x")`
   - `Maps.values` doesn't exist — use `Maps.elems`
   - `Lists.any` doesn't exist — use foldl with `Logic.or`

   When in doubt, check the actual exports in `Hydra.Dsl.Meta.Lib.*` or `Hydra.Sources.Kernel.Terms.*`.

9. **`inject` takes direct arguments, not `@@`**: The `inject` function takes three Haskell arguments directly:

   ```haskell
   -- Correct:
   inject _Definition _Definition_type (var "td")

   -- Wrong (will not type-check):
   inject _Definition _Definition_type @@ var "td"
   ```

   This is because `inject` returns a `TTerm b`, not a curried `TTerm (a -> b)`. Other functions like `wrap` follow the same pattern.

10. **`TBinding` vs `TTerm`**: A `TBinding a` value must be explicitly converted with `asTerm` when used in a context expecting `TTerm a`. This comes up when passing module-level configuration bindings to DSL functions like `Logic.and` or `Equality.equal`:

    ```haskell
    -- If you have: useInlineTypeParams :: TBinding Bool
    -- Wrong:
    Logic.and useInlineTypeParams (var "flag")
    -- Correct:
    Logic.and (asTerm useInlineTypeParams) (var "flag")
    ```

    Note: `TBinding` values work fine with `@@` (application) without conversion, since `@@` has an `AsTerm` constraint. The issue only arises with DSL functions that expect `TTerm` directly.

11. **Wrapping Haskell-level DSL functions in lambdas for higher-order use**: DSL library functions like `Lists.concat` are Haskell functions (`TTerm [[a]] -> TTerm [a]`), not DSL-level function values. When passing them to higher-order DSL functions like `Flows.map` (which expects an `AsTerm f (x -> y)`), wrap them in a lambda:

    ```haskell
    -- Wrong (type error — Lists.concat is a Haskell function, not a TTerm):
    Flows.map Lists.concat (var "flowOfLists")

    -- Correct:
    Flows.map ("xs" ~> Lists.concat (var "xs")) (var "flowOfLists")
    ```

12. **Record updates require setter functions**: The DSL has no record update syntax. If the original code uses `rec { field = newValue }`, create a setter function that reconstructs the entire record, copying all unchanged fields:

    ```haskell
    setMyField :: TBinding (MyRecord -> Int -> MyRecord)
    setMyField = define "setMyField" $
      "r" ~> "v" ~>
        record _MyRecord [
          _MyRecord_field1>>: project _MyRecord _MyRecord_field1 @@ var "r",
          _MyRecord_field2>>: var "v",  -- the updated field
          _MyRecord_field3>>: project _MyRecord _MyRecord_field3 @@ var "r"]
    ```

13. **Where clauses become separate TBindings**: Haskell `where` clauses have no DSL equivalent. Extract local helper functions into separate top-level `TBinding` definitions and add them to the module's elements list.

14. **Constructing syntax types without DSL helpers**: Not all generated types have convenience constructors in DSL helper modules. When no helper exists, use `record`, `wrap`, and `inject` directly with the phantom type names:

    ```haskell
    -- If there's no helper like PyDsl.importName, build it directly:
    inject Py._ImportStatement Py._ImportStatement_name $
      record Py._ImportName [
        Py._ImportName_names>>: list [
          record Py._DottedAsName [
            Py._DottedAsName_name>>: var "dotted",
            Py._DottedAsName_as>>: nothing]]]
    ```

15. **Namespace collisions**: When your DSL module defines a function with the same name as something in `Hydra.Kernel`, you'll get ambiguity errors. Either:
   - Rename your function (e.g., `partitionDefs` instead of `partitionDefinitions`)
   - Hide the conflicting import: `import Hydra.Kernel hiding (partitionDefinitions)`

16. **GHCi regeneration in hydra-ext**: Use stdin redirect to run GHCi scripts in `hydra-ext`:

    ```bash
    cd hydra-ext && stack ghci hydra-ext:lib < my_regen_script.ghci
    ```

    The script should contain the imports and `writeHaskell` call. Interactive flags like `--ghci-options="-e"` or `--no-load` don't work reliably for this purpose.

## Checklist

- [ ] Simplify staging code (break up complex functions, extract pure logic)
- [ ] Create source module file with proper namespace
- [ ] Add standard imports (watch for name conflicts)
- [ ] Define namespace, module, and `define` helper
- [ ] Add module to registry in `All.hs`
- [ ] Build empty module to verify structure compiles
- [ ] Promote functions one at a time, testing after each:
  - [ ] Translate function into DSL
  - [ ] Add to module's elements list
  - [ ] Regenerate target code
  - [ ] Comment out original staging function
  - [ ] Import and use generated version
  - [ ] Run tests to verify identical behavior
- [ ] Once all functions promoted, update consumers and remove staging module

## Related recipes

- [Extending Hydra Core](extending-hydra-core.md) — Adding new types to the kernel
- [Syncing Python](syncing-python.md) — Generating Python from Hydra modules
- [New Implementation](new-implementation.md) — Adding a new target language
