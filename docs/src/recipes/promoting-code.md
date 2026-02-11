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
5. **Comment out the original** — in the staging module, comment out the original function definition but **preserve it as a comment**. This is critical: the commented-out version serves as the authoritative reference for what the generated code should do. Import and use the generated version instead. The comment format should include the full function signature and body, not just the name.
6. **Test** — run the relevant test suite (e.g., `bin/sync-python.sh` or `./gradlew test`). Verify the generated code produces the same results.
7. **Repeat** steps 3–6 for the next function. The commented-out originals serve as a reference in case issues arise later in the promotion process. After refactoring staging code (e.g., extracting `where`-clause helpers), the original pre-refactoring versions may not be in git history, so preserving them in comments is the only way to keep them accessible.
8. **Complete the migration** — once all functions are promoted and verified, update consumers to import the generated module directly, and remove the staging module. This is the riskiest step: subtle behavioral differences (inclusive vs exclusive ranges, missing character escaping, type annotation placement) can produce different output even when the code compiles and passes unit tests. Run the full end-to-end pipeline — regenerate all target language code, diff the output against the staging version, and run integration tests — before considering the promotion complete.

**Important: Maintain a 1:1 relationship between staging and DSL code.** Each staging function should have a corresponding DSL element. When you need to break up complex staging functions into smaller helpers, always refactor the staging code *first*, verify the refactored staging still compiles and passes tests, and *then* promote the newly refactored functions to DSL. Never write new DSL elements that don't correspond to staging functions — this breaks the 1:1 mapping and makes it impossible to fall back on individual staging functions if something goes wrong during promotion.

## Step-by-step guide

### 1. Simplify staging code first

Before starting the actual promotion, refactor the staging (raw Haskell) code *in place*:

1. **Break up complex functions** into smaller, composable helpers. Extract nested `where`-clause helpers to top-level functions. This creates more leaf functions that can be promoted independently.
2. **Simplify pattern matching** to align with Hydra's case statement patterns
3. **Extract pure logic** from functions with side effects
4. **Remove unnecessary callbacks** — if functions take callbacks to break circular dependencies, consider whether direct calls would work. The DSL can handle mutual recursion directly.
5. **Separate I/O from pure logic** — extract as much pure logic as possible. The I/O wrapper should become a thin shell that reads input, calls the generated pure functions, handles errors, and writes output.
6. **Test the refactored staging code** to verify it still works correctly

The key principle is that the staging code and the DSL code remain in a **1:1 correspondence** throughout the process. Every top-level staging function should map directly to a DSL element. This makes the promotion incremental and safe — if a promoted function has a bug, you can always revert to the staging version of just that function.

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

16. **Bulk swapping with module re-exports**: When many functions have been promoted, you can swap the entire staging module to import from the generated module in bulk, rather than function by function. Use an explicit import list and re-export the imported modules so downstream consumers don't need changes:

    ```haskell
    module Hydra.Ext.Staging.Java.Utils (
      module Hydra.Ext.Staging.Java.Utils,
      module Hydra.Ext.Java.Helpers,
      module Hydra.Ext.Java.Utils,
      ) where

    -- Import types from the generated Helpers module
    import Hydra.Ext.Java.Helpers (Aliases(..))

    -- Import promoted functions from the generated Utils module
    import Hydra.Ext.Java.Utils (
      addExpressions, fieldExpression, javaIdentifier,
      -- ... list all functions being swapped
      )

    -- Comment out the original implementations, keeping them as reference
    -- addExpressions :: [Java.MultiplicativeExpression] -> Java.AdditiveExpression
    -- addExpressions exprs = ...

    -- Keep staging-only functions that haven't been promoted yet
    importAliasesForModule :: Module -> Aliases
    importAliasesForModule mod = ...
    ```

    The module re-export pattern `module Foo (module Foo, module Bar) where` ensures that the imported symbols are re-exported, so downstream consumers (like the staging Coder) see everything they need without import changes.

    When swapping, also swap any local type definitions (e.g., `data JavaFeatures`, `data JavaEnvironment`) to use the generated versions from the Helpers module. This may require renaming constructors or field accessors to match the generated names (e.g., `supportsDiamondOperator` becomes `javaFeaturesSupportsDiamondOperator`). Use `replace_all` or find-and-replace to update references throughout the staging code.

    **Important**: Before bulk swapping, verify that the generated functions produce identical output by regenerating target code and running tests. Differences in the generated code (e.g., a function that sanitizes names when the staging version doesn't) indicate bugs in the DSL source definitions that must be fixed before the swap.

17. **Known constructor naming issues**: Some generated Haskell constructors have naming conflicts with their parent type (e.g., `PrimaryNoNewArray` is both a type and a constructor of `Primary`). The generated code uses `PrimaryNoNewArray` but the actual constructor is `PrimaryNoNewArray_` (with trailing underscore). After each regeneration, apply a targeted fix:

    ```bash
    # Fix "PrimaryNoNewArray (" → "PrimaryNoNewArray_ ("
    # Fix "PrimaryNoNewArray Syntax.PrimaryNoNewArrayThis" → "PrimaryNoNewArray_ Syntax.PrimaryNoNewArrayThis"
    ```

    Use `replace_all` in the Edit tool for this. Be careful not to use overly broad replacements (e.g., replacing `PrimaryNoNewArray ` with a trailing space can corrupt adjacent identifiers). Instead, replace specific patterns like `PrimaryNoNewArray (` and `PrimaryNoNewArray Syntax.PrimaryNoNewArrayThis`.

18. **Eliminating Haskell-specific polymorphism**: Staging code may use typeclass polymorphism (e.g., `Integral a => a -> ...`) that can't be expressed in the monomorphic DSL. Check all call sites to determine the actual types used. If only one concrete type is used, promote with that type. If multiple types are used, create separate functions for each type, or add `fromIntegral`/conversion at the call site. For example, `javaInt :: Integral a => a -> Java.Literal` can become `javaInt :: Integer -> Java.Literal` with `fromIntegral` at call sites that pass `Int`.

19. **Record construction and updates for state types**: When promoting functions that construct or update large record types (like an `Aliases` state record), use `record` with all fields for construction, and reconstruct the entire record for updates (copying unchanged fields via `project`):

    ```haskell
    -- Construction: set all fields explicitly
    importAliasesForModule = def "importAliasesForModule" $
      lambda "mod" $ record _Aliases [
        _Aliases_currentNamespace>>: Module.moduleNamespace (var "mod"),
        _Aliases_packages>>: (Maps.empty :: TTerm (M.Map Namespace Java.PackageName)),
        _Aliases_branchVars>>: (Sets.empty :: TTerm (S.Set Name)),
        -- ... all other fields
        ]

    -- Update: copy all fields, changing only what's needed
    addInScopeVar = def "addInScopeVar" $
      lambda "name" $ lambda "aliases" $
        record _Aliases [
          _Aliases_currentNamespace>>:
            project _Aliases _Aliases_currentNamespace @@ var "aliases",
          _Aliases_inScopeJavaVars>>:
            Sets.insert (var "name") (project _Aliases _Aliases_inScopeJavaVars @@ var "aliases"),
          -- ... copy all other fields unchanged
          ]
    ```

    This is verbose but works reliably. Consider promoting all update functions for a record type together, since they share the same boilerplate pattern.

20. **GHCi regeneration in hydra-ext**: Use stdin redirect to run GHCi scripts in `hydra-ext`:

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

21. **Promoting mutually recursive function groups**: Functions that call each other (e.g., `encodeTerm` calls `encodeApplication` which calls `encodeTerm`) can be promoted incrementally using **stubs**. Promote one function at a time, adding stubs for the functions it calls that haven't been promoted yet:

    ```haskell
    -- Full definition for encodeTerm (calls encodeApplication)
    encodeTerm :: TBinding (...)
    encodeTerm = def "encodeTerm" $
      lambda "env" $ lambda "term" $
        ... encodeApplication @@ var "env" @@ var "app" ...

    -- Stub for encodeApplication (not yet promoted)
    encodeApplication :: TBinding (...)
    encodeApplication = def "encodeApplication" $
      lambda "env" $ lambda "app" $
        Monads.unexpected @@ string "encodeApplication stub" @@ string "encodeApplication"
    ```

    All stubs must appear in the module's elements list. As you promote each function, replace its stub with the real implementation. The generated code will compile and work correctly for the promoted functions, while any path that hits a stub will fail with a clear error message.

    **Extracting nested helpers**: Complex staging functions often have `where`-clause helpers that capture closed-over variables. When promoting, extract these into separate top-level `TBinding` definitions, passing the captured variables as explicit parameters. Add the extracted helpers to the elements list.

    **Fallback pattern**: When a function has complex nested case analysis (e.g., `encodeApplication` has a `fallback` helper), extract the fallback logic into a separate `TBinding` with an explicit name like `encodeApplication_fallback`. This simplifies the main function and creates a clean separation of concerns.

22. **DSL function availability pitfalls**: When translating Haskell code to DSL, several common functions have different names or don't exist:

    | Haskell / Staging | DSL Equivalent |
    |---|---|
    | `S.isSubsetOf a b` | `Sets.null (Sets.difference a b)` |
    | `S.unions xs` | `Sets.unions xs` (exists in Sets DSL) |
    | `M.unions xs` | `Lists.foldl (\acc m -> Maps.union acc m) Maps.empty xs` |
    | `M.values m` | `Maps.elems m` |
    | `M.isEmpty m` | `Maps.null m` |
    | `S.empty` / `S.null` | `Sets.empty` / `Sets.null` |
    | `L.zipWith f a b` | `Lists.zipWith f a b` |
    | `L.replicate n x` | `Lists.replicate n x` |
    | `x == y` | `Equality.equal x y` (not `Equality.eq`) |
    | `x /= y` | `Logic.not (Equality.equal x y)` |
    | `foldM f acc xs` | `Flows.foldl f acc xs` (monadic fold) |
    | `ShowCore.functionVariant` | `ShowCore.function` |
    | `ShowCore.type_` | `ShowCore.type_` |

23. **Triples in the DSL**: Hydra encodes triples as nested pairs: `triple a b c = pair a (pair b c)`. To destructure a triple result:

    ```haskell
    "result" <~ someFunction @@ ... $
    "first"  <~ Pairs.first (var "result") $
    "second" <~ Pairs.first (Pairs.second (var "result")) $
    "third"  <~ Pairs.second (Pairs.second (var "result")) $
    ```

24. **`Graph` module accessors**: To access graph state fields like `graphPrimitives`, import `Hydra.Dsl.Meta.Graph` and use the accessor functions:

    ```haskell
    import qualified Hydra.Dsl.Meta.Graph as Graph

    -- In DSL code:
    "g" <<~ Monads.getState $
    "prims" <~ Graph.graphPrimitives (var "g") $
    ```

    Note: `Graph.graphPrimitives`, `Graph.primitiveType`, etc. are Haskell-level DSL helpers (not `TBinding`s), so they take direct arguments without `@@`.

25. **`Arity` module**: The `typeArity` function for computing function type arity is in `Hydra.Sources.Kernel.Terms.Arity`. Import it as `qualified Hydra.Sources.Kernel.Terms.Arity as Arity` and add `Arity.ns` to the module's namespace dependencies.

26. **More DSL function availability pitfalls (batch 27-28)**:

    | Staging code | DSL equivalent | Notes |
    |---|---|---|
    | `S.filter pred set` | `Sets.intersection set2 set` (if filtering by membership) | No `Sets.filter`; use `Sets.intersection` or convert to list |
    | `L.concatMap f xs` | `Lists.concat (Lists.map f xs)` | No `Lists.concatMap` |
    | `Y.catMaybes xs` | `Maybes.cat xs` | `Maybes.cat :: [Maybe a] -> [a]` |
    | `Right x` | `right x` (from DSL prelude) | Not `inject _Either _Either_right` |
    | `Left x` | `Phantoms.left x` | For `Either` left values |
    | `Just $ Left $ ExpressionName ...` | `just (Phantoms.left (JavaDsl.expressionName nothing id))` | Common in methodInvocation calls |

27. **Extracting large `where`-clause helpers**: When promoting complex functions like `encodeElimination` or `bindingsToStatements`, extract substantial `where`-clause helpers (e.g., `otherwiseBranch`, `visitBranch`, `toDeclInit`, `toDeclStatement`) as separate TBindings. Pass shared state (like `aliases`, `tcExtended`, `recursiveVars`) as explicit parameters. This makes each TBinding manageable and independently testable.

28. **`encodeTypeAsTerm` helper**: The staging code uses `EncodeCore.type_ typ` to encode a `Type` as a `Term` for annotations. In the DSL, use the pre-defined helper `encodeTypeAsTerm @@ var "typ"` (defined as `TTerm $ TermVariable $ Name "hydra.encode.core.type"`). Alternatively, `Phantoms.encoderFor _Type @@ var "typ"` works too.

29. **`Let` record fields**: The `Let` type has fields `_Let_bindings` and `_Let_body` (not `_Let_environment`). Check generated Core.hs for exact field names.

30. **`Core.field` vs `Core.fieldType`**: `Core.field` constructs a `Field` (name + term), while `Core.fieldType` constructs a `FieldType` (name + type). When the staging code uses `FieldType (Name "value") someType`, use `Core.fieldType (wrap _Name (string "value")) (var "someType")`, NOT `Core.field`.

31. **`project` and `unwrap` require `@@`**: These DSL functions return a `TTerm (a -> b)`, not a function at the Haskell level. You must apply them with `@@`: `project _Foo _Foo_bar @@ var "x"`, NOT `project _Foo _Foo_bar (var "x")`. Similarly, `unwrap _Name @@ var "x"`, NOT `unwrap _Name (var "x")`.

32. **`Equality.lte`/`Equality.gte` for comparisons**: Use `Equality.lte (var "n") (int32 0)` for `n <= 0`, NOT `Math.lte`. The `Math` module handles arithmetic (`Math.sub`, `Math.add`), while `Equality` handles comparisons.

33. **`Lists.zip` + `Flows.mapList` for `CM.zipWithM`**: Haskell's `Control.Monad.zipWithM f xs ys` becomes `Flows.mapList (lambda "pair" $ f @@ Pairs.first (var "pair") @@ Pairs.second (var "pair")) (Lists.zip xs ys)`. There is no `Flows.mapList2`.

34. **Haskell code generator `PrimaryNoNewArray_` bug**: When a union variant name, capitalized and prefixed with the type name, collides with the type name itself (e.g., `Primary.noNewArray` → `PrimaryNoNewArray` collides with the type `PrimaryNoNewArray`), the Haskell code generator should append `_` to disambiguate. Inline `inject` calls may not handle this correctly. **Workaround**: Use a pre-existing helper function (e.g., `JavaUtilsSource.javaMethodInvocationToJavaPrimary`) that handles the wrapping, rather than constructing the Primary inline.

35. **`Rewriting.deannotateType` is a TBinding**: Like other TBindings, it must be applied with `@@`: `Rewriting.deannotateType @@ var "t"`, NOT `Rewriting.deannotateType (var "t")`.

36. **Naming collisions with kernel imports**: If your TBinding name (e.g., `isSerializableType`) conflicts with a name imported from `Hydra.Kernel`, rename it (e.g., `isSerializableJavaType`) to avoid ambiguity.

37. **`Serialization` module for `printExpr`/`parenthesize`**: These functions live in `Hydra.Sources.Kernel.Terms.Serialization`, NOT in the Java Serde module. Import as `qualified Hydra.Sources.Kernel.Terms.Serialization as SerializationSource` and add `SerializationSource.ns` to the module dependencies.

38. **Language-specific character escaping in serialization**: When promoting code that generates source files (e.g., a Java or Python serializer), be aware that target languages may require specific escape sequences for non-ASCII characters. For example, Java source files require `\uXXXX` escape sequences for characters outside ASCII. You may need to write custom escape functions (e.g., `escapeJavaChar`, `escapeJavaString`) in the DSL rather than relying on Haskell's default `show` behavior, which passes non-ASCII characters through as literal UTF-8. Test generated source files with non-ASCII input (e.g., `\u03BB` for lambda) to verify correct escaping.

## Related recipes

- [Extending Hydra Core](extending-hydra-core.md) — Adding new types to the kernel
- [Syncing Python](syncing-python.md) — Generating Python from Hydra modules
- [New Implementation](new-implementation.md) — Adding a new target language
