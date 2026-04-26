# Refactoring the Hydra Kernel

This recipe documents how to create, rename, move, or delete kernel elements (definitions) and modules (namespaces),
and propagate the changes across all implementations.

## Overview

Hydra kernel code lives in multiple places:
- **Source modules** (`packages/hydra-haskell/src/main/haskell/Hydra/Sources/...`) - DSL definitions
- **Generated Haskell** (`dist/haskell/hydra-kernel/src/main/haskell/Hydra/...`) - Generated implementations
- **Generated Python** (`dist/python/hydra-kernel/src/main/python/hydra/...`)
- **Generated Java** (`dist/java/hydra-kernel/src/main/java/hydra/...`)
- **JSON kernel** (`dist/json/hydra-kernel/src/main/json/...`)

Changes to kernel code must be propagated to all of these locations.

## Quick Reference

| Operation | Key Steps |
|-----------|-----------|
| **Create module** | Create source file → Add to registry → Build → Regenerate |
| **Create element** | Add definition to module → Build → Regenerate |
| **Delete element** | Remove definition → Update references → Build → Regenerate |
| **Delete module** | Remove from registry → Delete source → Update references → Regenerate |
| **Move element** | Add to new location → Update references → Remove from old → Regenerate |
| **Rename element** | Update name in source → Update all references → Regenerate |
| **Move/rename module** | See [detailed section](#moving-or-renaming-modules) and [Refactoring Namespaces](refactoring-namespaces.md) |
| **Consolidate types** | Introduce new type → Migrate consumers → Delete old type → Regenerate. See [detailed section](#consolidating-or-replacing-types) |

## Prerequisites

- Working Haskell build environment (`stack build` succeeds in packages/hydra-haskell)
- Understanding of Hydra's module system and DSL
- For Python: virtual environment set up in packages/hydra-python
- For Java: Gradle configured in packages/hydra-java

---

## Creating a New Module

### Step 1: Create the Source File

Create a new file in the appropriate location under `packages/hydra-haskell/src/main/haskell/Hydra/Sources/`.

**For a types-only module** (no term definitions):
```
Hydra/Sources/Kernel/Types/MyModule.hs
```

**For a terms module** (functions/values):
```
Hydra/Sources/Kernel/Terms/MyModule.hs
```

### Step 2: Write the Module Structure

```haskell
module Hydra.Sources.Kernel.Terms.MyModule where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
-- ... other standard imports (copy from an existing module)

-- Define the namespace
ns :: Namespace
ns = Namespace "hydra.mymodule"

-- Define the module
module_ :: Module
module_ = Module ns definitions
    [DependencyModule1.ns, DependencyModule2.ns]  -- dependencies
    kernelTypesNamespaces $
    Just "Description of this module"
  where
    definitions = [
      toDefinition myFunction1,
      toDefinition myFunction2]

-- Helper for defining elements
define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- Define elements
myFunction1 :: TTermDefinition (Int -> Int)
myFunction1 = define "myFunction1" $
  doc "Description of myFunction1" $
  "x" ~> var "x"

myFunction2 :: TTermDefinition (String -> String)
myFunction2 = define "myFunction2" $
  doc "Description of myFunction2" $
  "s" ~> var "s"
```

### Step 3: Register the Module

**For types modules**, add to `Hydra/Sources/Kernel/Types/All.hs`:
```haskell
import qualified Hydra.Sources.Kernel.Types.MyModule as MyModule
-- ...
kernelTypesModules = [
  -- ... existing modules ...
  MyModule.module_]
```

**For terms modules**, add to `Hydra/Sources/Kernel/Terms/All.hs`:
```haskell
import qualified Hydra.Sources.Kernel.Terms.MyModule as MyModule
-- ...
kernelPrimaryTermsModules = [
  -- ... existing modules ...
  MyModule.module_]
```

### Step 4: Build and Regenerate

The simplest approach is to run `bin/sync.sh` from the repo root
(or `bin/sync-default.sh` for the haskell/java/python triad).
The matrix tool handles all regeneration in dependency order. For
incremental work, you can run the steps individually:

```bash
cd heads/haskell

# Build to verify the source compiles
stack build

# Regenerate Haskell
stack ghci
> import Hydra.Generation
> import Hydra.Sources.All
> writeHaskell "../dist/haskell/hydra-kernel/src/main/haskell" mainModules kernelModules
> :quit

# Rebuild with generated code
stack build

# Regenerate JSON kernel
./bin/update-json-kernel.sh

# Run tests
stack test
```

### Step 5: Regenerate Other Implementations

```bash
# From heads/haskell
cd ../heads/haskell
./bin/sync-python.sh --no-tests
./bin/sync-java.sh --no-tests
```

---

## Creating a New Element (Definition)

### Step 1: Add the Definition

In the source module, add the new definition:

```haskell
myNewFunction :: TTermDefinition (A -> B)
myNewFunction = define "myNewFunction" $
  doc "Description" $
  "x" ~> someExpression (var "x")
```

### Step 2: Register in the Module's Element List

```haskell
definitions = [
  -- ... existing elements ...
  toDefinition myNewFunction]
```

### Step 3: Build and Regenerate

Same as for creating a module - build, regenerate Haskell, rebuild, regenerate other implementations.

---

## Deleting an Element

### Step 1: Remove from Element List

In the source module, remove the element from the `definitions` list.

### Step 2: Update All References

Search for references to the deleted element:
```bash
grep -rn 'myDeletedFunction' packages/hydra-haskell/src/main/haskell/
grep -rn 'hydra.mymodule.myDeletedFunction' packages/hydra-haskell/src/
```

Update or remove all references.

### Step 3: Remove the Definition

Delete the definition itself from the source file.

### Step 4: Build and Regenerate

Build and regenerate all implementations.

---

## Deleting a Module

### Step 1: Remove from Registry

Remove the import and module reference from `All.hs`.

### Step 2: Update All References

Search for imports and references:
```bash
grep -rn 'MyModule' packages/hydra-haskell/src/main/haskell/
grep -rn 'hydra.mymodule' packages/hydra-haskell/src/
```

### Step 3: Delete the Source File

```bash
rm packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/MyModule.hs
```

### Step 4: Delete Generated Files

```bash
rm ../../dist/haskell/hydra-kernel/src/main/haskell/Hydra/MyModule.hs
rm -rf ../dist/python/hydra-kernel/src/main/python/hydra/mymodule/
rm -rf ../dist/java/hydra-kernel/src/main/java/hydra/mymodule/
```

### Step 5: Build and Regenerate

---

## Moving an Element Between Modules

### Step 1: Add to New Module

Copy the definition to the new module and update its `define` helper usage.

### Step 2: Update All References

The element's fully-qualified name changes (e.g., `hydra.oldmodule.foo` → `hydra.newmodule.foo`).

Search and update:
```bash
grep -rn 'hydra.oldmodule.foo' packages/hydra-haskell/src/
grep -rn 'OldModule.foo' packages/hydra-haskell/src/
```

### Step 3: Update Dependencies

If other modules imported the old module just for this element, update their dependency lists.

### Step 4: Remove from Old Module

Remove the definition and element registration from the old module.

### Step 5: Build and Regenerate

---

## Renaming an Element

### Step 1: Update the Definition Name

Change the name in the `define` call:
```haskell
-- From:
myOldName = define "myOldName" $ ...
-- To:
myNewName = define "myNewName" $ ...
```

Also update the Haskell binding name if desired.

### Step 2: Update the Element Registration

```haskell
definitions = [
  -- Change:
  toDefinition myOldName
  -- To:
  toDefinition myNewName]
```

### Step 3: Update All References

```bash
grep -rn 'myOldName' packages/hydra-haskell/src/
grep -rn 'hydra.mymodule.myOldName' packages/hydra-haskell/src/
```

### Step 4: Build and Regenerate

---

## Moving or Renaming Modules

> **Note**: This section provides an overview of namespace refactoring.
> For a comprehensive, step-by-step guide with detailed examples (especially for decoder/encoder modules and
> multi-repository coordination), see [Refactoring Hydra Namespaces](refactoring-namespaces.md).

This is the most complex refactoring operation. A Hydra namespace like `hydra.foo` corresponds to:
- A Haskell source module (e.g., `Hydra/Sources/Kernel/Terms/Foo.hs`)
- Generated Haskell code (e.g., `Hydra/Foo.hs`)
- Generated decoder/encoder source modules
- Generated decoder/encoder implementations
- Generated Python code (e.g., `hydra/foo.py` or `hydra/foo/__init__.py`)
- Generated Java code (e.g., `hydra/foo/Element.java`)
- JSON kernel exports

### When You Might Need This

- Resolving module/package conflicts (e.g., Python can't have both `json.py` and `json/` directory)
- Reorganizing the namespace hierarchy
- Preparing for semantic versioning with a cleaner API surface

### Phase 1: Update the Source Module

1. **Move/rename the source file**
   ```bash
   # Example: moving Foo.hs to Foo/Bar.hs
   mkdir -p packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Foo
   mv packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Foo.hs \
      packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Foo/Bar.hs
   ```

2. **Update the namespace declaration**
   ```haskell
   -- Change from:
   ns = Namespace "hydra.foo"
   -- To:
   ns = Namespace "hydra.foo.bar"
   ```

3. **Update the Haskell module declaration**
   ```haskell
   -- Change from:
   module Hydra.Sources.Kernel.Terms.Foo where
   -- To:
   module Hydra.Sources.Kernel.Terms.Foo.Bar where
   ```

### Phase 2: Update References

1. **Update module registry**
   ```haskell
   -- In All.hs, change:
   import qualified Hydra.Sources.Kernel.Terms.Foo as Foo
   -- To:
   import qualified Hydra.Sources.Kernel.Terms.Foo.Bar as FooBar
   ```

2. **Find and update all references**
   ```bash
   grep -rn 'Hydra\.Foo[^.]' packages/hydra-haskell/src/main/haskell/
   grep -rn 'hydra\.foo[^.]' packages/hydra-haskell/src/main/haskell/
   ```

3. **Bootstrap generated module if needed**
   If the generated code doesn't exist yet, create a minimal version.

4. **Build and verify**
   ```bash
   stack build
   ```

### Phase 3: Move/Update Generated Files

1. **Move generated Haskell files**
   ```bash
   mkdir -p dist/haskell/hydra-kernel/src/main/haskell/Hydra/Foo
   mv dist/haskell/hydra-kernel/src/main/haskell/Hydra/Foo.hs dist/haskell/hydra-kernel/src/main/haskell/Hydra/Foo/Bar.hs
   ```

2. **Update module declarations in generated files**
   ```bash
   perl -i -pe 's/module Hydra\.Foo where/module Hydra.Foo.Bar where/g' \
     dist/haskell/hydra-kernel/src/main/haskell/Hydra/Foo/Bar.hs
   ```

3. **Update namespace strings in generated files**
   ```bash
   perl -i -pe 's/hydra\.foo\.Element/hydra.foo.bar.Element/g' \
     dist/haskell/hydra-kernel/src/main/haskell/Hydra/Foo/Bar.hs
   ```

4. **Clean up orphan files**
   ```bash
   rm -f dist/haskell/hydra-kernel/src/main/haskell/Hydra/Foo.hs  # old location
   ```

### Phase 4: Regenerate All Implementations

```bash
# Rebuild after manual updates
stack build

# Regenerate JSON kernel
./bin/update-json-kernel.sh
./bin/verify-json-kernel.sh

# Run tests
stack test

# Regenerate Python and Java (from heads/haskell)
cd ../heads/haskell
./bin/sync-python.sh --no-tests
./bin/sync-java.sh --no-tests

# Clean up orphan Python files
rm -f ../dist/python/hydra-kernel/src/main/python/hydra/foo.py
```

---

## Consolidating or Replacing Types

When a refactoring eliminates types or merges multiple types into one, the scope goes beyond renaming —
you need to rewrite every consumer of the old types to use the new one, often with different field names and semantics.

### When You Might Need This

- Merging related types that have converged in purpose (e.g., separate "context" types that can be unified)
- Eliminating wrapper types whose indirection is no longer needed
- Replacing a type with a fundamentally different structure (different fields, not just renamed fields)

### General Approach

1. **Introduce the new type alongside the old one**.
   If the new type doesn't exist yet, add it to the type definition source.
   If replacing type A with existing type B, this step is already done.

2. **Add a transitional bridge** (optional but recommended for large changes).
   Add a field or helper function that converts between old and new representations.
   This lets you migrate consumers incrementally rather than all at once.

3. **Migrate consumers module by module**. Update each Source file that references the old type:
   - Change function signatures to use the new type
   - Replace field accessors with the new type's accessors
   - Update DSL helper calls (constructor, `with*` helpers, field projections)

4. **Update DSL helpers** in `Dsl/Meta/`. Delete old constructors and accessors, add new ones or rename as appropriate.

5. **Delete the old type** once no consumers remain.
   Remove it from the type definition source and the module's element list.

6. **Remove the transitional bridge** if one was used.

7. **Build, regenerate, and test** across all implementations.

### Updating Non-Generated Code

Type consolidation affects not only Sources and the generated `dist/haskell/<pkg>/` trees, but also hand-written code that references the old types:

- **Test infrastructure**: Test runners in Java (`TestSuiteRunner.java`), Python, and Haskell (`TestUtils.hs`,
  `TestSuiteSpec.hs`) construct kernel types directly.
  These need manual updates to use new constructors and field names.
- **Executables**: `verify-json-kernel/Main.hs` and `Generation.hs` use kernel types directly.
- **DSL bootstrap**: `Dsl/Bootstrap.hs` constructs initial graphs using Haskell record syntax on kernel types.
- **Language coders**: Coders in `packages/hydra-pg`, `packages/hydra-rdf`, and
  `packages/hydra-ext` reference kernel types in their state management.

Search broadly for the old type name:
```bash
# Search across all subprojects, not just Sources
grep -rn 'OldTypeName' packages/hydra-haskell/ packages/hydra-java/ packages/hydra-python/ packages/hydra-pg/ packages/hydra-rdf/ packages/hydra-ext/
```

### Pitfalls Specific to Type Consolidation

- **Silent semantic mismatches**: When a field changes meaning (not just name),
  consumers may compile but produce wrong results.
  For example, if a field changes from `Type` to `TypeScheme`,
  code that previously stored a bare type must now properly extract/wrap forall binders.
  Tests are essential for catching these.

- **Types threaded through computations**: If the old type was passed as a `Context` or `Graph` parameter through
  computations, every call site must be updated.
  These are easy to miss because the compiler may not flag them if the new type happens to unify.

- **Multiple types with similar roles**: When consolidating types like `TypeContext`, `InferenceContext`,
  and `Graph` into a single `Graph`, different consumers may have used different subsets of the old types' fields.
  Map each consumer's actual field usage to the new type's fields rather than doing a mechanical rename.

---

## Common Pitfalls

### Chicken-and-Egg Bootstrap Problem
Generated Haskell code depends on modules that need to be generated. Solution:
1. Create minimal bootstrap versions of generated modules
2. Build incrementally
3. Regenerate fully once the build works

**Adding a field to a kernel type** (e.g., adding `transitionalGraf` to `Graph`):
1. Add the field to the type definition in Sources (e.g., `Sources/Kernel/Types/Graph.hs`)
2. Regenerate just the types module into the generated tree (e.g., via `writeHaskell` in ghci into `dist/haskell/hydra-kernel/src/main/haskell/`)
3. Manually patch **all** record construction sites in the generated tree to supply the new field —
   search for `TypeName {` across `dist/haskell/`
4. If the new field's type needs a default value (like `emptyGraf`), add that helper manually to the generated file
5. Update the DSL helpers (e.g., `Dsl/Meta/Graph.hs`) — constructor, accessors, and all `with*` helpers
6. Update all Source-level constructor calls to supply the new field
7. `stack build` to verify everything compiles
8. Regenerate cleanly — the generated files will now replace your manual patches
9. `stack build` again and run tests to confirm

The key insight: the patches in step 3 are temporary scaffolding.
They only need to be correct enough for the build to succeed so that regeneration can produce the real versions.

### Silent State Pipeline Bugs
When refactoring types that are threaded through computations (e.g., as `Context` or `Graph` parameters),
a function may compile but produce wrong results because a field is empty or has the wrong representation. For example:
- A graph passed through computations might have an empty `schemaTypes` map,
  causing type alias lookups to silently fail and generate wrapper classes instead of transparent aliases.
- A function that previously read schema types from a nested `Maybe Graph` field might now need to read from a flat
  `Map Name TypeScheme` field -- the code compiles either way, but one path returns nothing.

These bugs don't cause compilation errors or even runtime exceptions — they produce subtly wrong output.
The defense is to run the full test suite after every regeneration and investigate any new failures carefully,
even if they seem unrelated to the change.

### Orphan files
When modules or elements are moved, renamed, or deleted, stale generated files may be left behind.
The sync scripts generate new files but do not delete old ones.
After any rename, move, or delete operation, check for and remove orphan files
in all implementations (Haskell, Java, Python, Scala, Lisp dialects, and JSON).

Java is especially prone to orphans because it generates **one file per type** —
renaming or deleting a single type leaves an orphan `.java` file on the classpath.

For a comprehensive guide to finding stale files across all implementations,
including systematic procedures and known patterns, see [Repository maintenance](maintenance.md).

### Python Module/Package Conflicts
Python can't have both `foo.py` and `foo/` directory.
Use a structure like `hydra/foo/bar.py` instead of `hydra/foo.py` alongside `hydra/foo/baz.py`.

### Decoder/Encoder Module Paths
When renaming `hydra.foo` to `hydra.foo.bar`, the decoder/encoder modules also move:
- `Hydra.Sources.Decode.Foo` → `Hydra.Sources.Decode.Foo.Bar`
- `Hydra.Decode.Foo` → `Hydra.Decode.Foo.Bar`

---

## The Sync Pipeline

After making changes to Sources and rebuilding Haskell, regeneration must propagate through several stages.
The `bin/sync.sh` script runs all of these in the correct order (use `--no-tests` to skip tests at each stage).
For the haskell/java/python bootstrapping triad, `bin/sync-default.sh` is a shorthand wrapper.
However, it's useful to understand the individual stages, especially when debugging failures.

### Pipeline Order

`bin/sync.sh` (with `--hosts`/`--targets` resolved from CLI flags) executes four phases:

```
Phase 1: heads/haskell/bin/sync-haskell.sh
    → DSL → JSON via update-json-{main,test,manifest}
    → verify-json-kernel (round-trip check)
    → JSON → Haskell for hydra-kernel + hydra-haskell via bootstrap-from-json
    → stack test (unless --no-tests)
    (Lexicon regen was retired from sync; run bin/regenerate-lexicon.sh on demand.)
Phase 2: assemble-distribution.sh per coder package (Haskell from JSON)
    → one call per language in (hosts ∪ targets) \ {haskell}
Phase 3: hydra-kernel into each language in (hosts ∪ targets) \ {haskell}
    → via transform-json-to-target.sh, both main and test
Phase 4: cross-host coders for each (host, target) with host ≠ haskell
    → hydra-<target> coder generated in host's language
```

### Diagnosing Failures at Each Stage

| Stage | Typical Failures | What To Check |
|-------|-----------------|---------------|
| `stack build` (heads/haskell) | Missing fields, wrong types in Sources | Source files, DSL helpers, generated bootstrap under `dist/haskell/hydra-kernel/` |
| `sync-haskell.sh` | Haskell test failures | Generated code correctness, `verify-json-kernel` |
| `stack build` (extension packages) | Coders using old type names/fields | Extension package source files referencing old types |
| `sync-java.sh` | Java compilation errors | Hand-written Java code (test runners, utilities) |
| `gradle test` | Test failures (wrong results, not just compilation) | Test graph construction, method signatures, type representations |
| `sync-python.sh` | Python import errors, test failures | Hand-written Python code, `__init__.py` structure |

### Key Principle

Fix errors at the earliest stage before moving to the next.
A Haskell compilation error in an extension package will cascade into meaningless sync-java failures.
Similarly, if Haskell tests fail, investigate before regenerating Java/Python — the generated code may be wrong.

---

## Verification Checklist

- [ ] packages/hydra-haskell builds (`stack build`)
- [ ] packages/hydra-haskell tests pass (`stack test`)
- [ ] JSON kernel regenerated and verified
- [ ] Extension packages build (`stack build` in packages/hydra-pg, hydra-rdf, hydra-ext)
- [ ] Python kernel regenerated
- [ ] Python tests pass
- [ ] Java compilation succeeds (`gradle compileTestJava`)
- [ ] Java tests pass (`gradle test`)
- [ ] Orphan files cleaned up (stale generated files from old type structure)
- [ ] All references updated (no broken imports)
- [ ] Hand-written test infrastructure updated (test runners, test utilities)

---

## Example: Creating hydra.hoisting Module

This section documents the creation of `hydra.hoisting` by extracting hoisting functions from `hydra.reduction`.

### Context

The `hydra.reduction` module contained hoisting functions (`hoistSubterms`, `hoistCaseStatements`,
etc.) that conceptually belong in a dedicated module focused on term hoisting.
We created `hydra.hoisting` to separate these concerns.

### Steps Performed

1. **Created source file**: `Hydra/Sources/Kernel/Terms/Hoisting.hs`

   Created the new module with:
   - Namespace: `hydra.hoisting`
   - Dependencies: `Rewriting.ns`, `Schemas.ns`
   - Module description: "Functions for hoisting subterms into let bindings."

2. **Moved definitions** from `hydra.reduction`:
   - `hoistSubterms` - main hoisting function with path-aware predicates
   - `hoistCaseStatements` - convenience function for case statement hoisting
   - `hoistCaseStatementsInGraph` - graph-level case statement hoisting
   - `shouldHoistCaseStatement` - predicate for case statements
   - `isUnionElimination`, `isEliminationUnion` - case statement detection
   - `updateHoistState`, `normalizePathForHoisting` - path tracking helpers
   - `isApplicationFunction`, `isLambdaBody` - accessor predicates
   - `rewriteAndFoldTermWithTypeContext` - context-aware rewriting
   - `rewriteAndFoldTermWithTypeContextAndPath` - path+context rewriting
   - `rewriteTermWithTypeContext` - simple context-aware rewriting

3. **Updated `hydra.reduction`**:
   - Removed all hoisting functions and rewriting-with-context functions
   - Updated import hiding list (removed hoisting-related names)
   - Added `Hoisting.ns` to module dependencies
   - Updated element list to only include reduction-specific functions

4. **Registered new module** in `Kernel/Terms/All.hs`:
   - Added import: `import qualified Hydra.Sources.Kernel.Terms.Hoisting as Hoisting`
   - Added to `kernelPrimaryTermsModules` list

5. **Updated references** in callers:
   - `Hydra/Sources/Kernel/Terms/Adapt/Simple.hs`:
     - Added import for `Hoisting`
     - Added `Hoisting.ns` to module dependencies
     - Changed `Reduction.hoistCaseStatementsInGraph` to `Hoisting.hoistCaseStatementsInGraph`
   - `heads/haskell/src/test/haskell/Hydra/TestSuiteSpec.hs`:
     - Added import for `Hydra.Hoisting`
     - Changed test references to use `Hoisting.*` functions

6. **Regenerated** implementations:
   ```bash
   # Build to verify source compiles
   stack build

   # Regenerate Haskell
   stack ghci hydra:lib -e 'import Hydra.Generation' \
     -e 'import Hydra.Sources.All' \
     -e 'writeHaskell "../dist/haskell/hydra-kernel/src/main/haskell" mainModules kernelModules'

   # Rebuild with generated code
   stack build

   # Regenerate JSON kernel
   ./bin/update-json-kernel.sh
   ./bin/verify-json-kernel.sh

   # Run tests
   stack test
   ```

### Files Changed

**Created:**
- `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Hoisting.hs` (source module)

**Modified:**
- `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Reduction.hs` (removed hoisting functions)
- `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/All.hs` (registered new module)
- `packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Adapt/Simple.hs` (updated imports)
- `heads/haskell/src/test/haskell/Hydra/TestSuiteSpec.hs` (updated test imports)

**Generated:**
- `dist/haskell/hydra-kernel/src/main/haskell/Hydra/Hoisting.hs` (generated implementation)
- `dist/json/hydra-kernel/src/main/json/hydra/hoisting.json` (JSON kernel)

---

## Example: Changing Graph.elements from Map to List

This section documents a deep type change to the Hydra kernel: changing `Graph.elements` from `map<Name,
Binding>` to `list<Binding>` to preserve element order in graphs.

### Context

The `Graph` type had an `elements` field of type `Map<Name, Binding>`.
This caused element order to be non-deterministic (sorted by Name),
which was undesirable when element ordering matters (e.g., for code generation output stability).
The change preserves insertion order by using a list instead.

### The Bootstrap Challenge

This change is particularly complex because:
1. **Generated files depend on source files** that define the types
2. **Source files (DSL)** use generated types to construct terms
3. Changing a fundamental type like `Graph` affects both sides

The solution is to update files in the correct order:
1. First update generated files under `dist/haskell/<pkg>/src/main/haskell/` to make them compile with the new type
2. Then update source files (`Sources`) to generate the correct definitions
3. Regenerate to verify consistency

### Adding a New Primitive

When changing from `Map.lookup` to a list-based lookup, we needed a `Lists.find` primitive that didn't exist.
Adding a primitive requires updates to **six files**:

1. **`Hydra.Lib.Lists`** - The actual Haskell implementation:
   ```haskell
   find :: (a -> Bool) -> [a] -> Maybe a
   find = L.find
   ```

2. **`Hydra.Staging.Lib.Names`** - The primitive name constant:
   ```haskell
   _lists_find = qname _hydra_lib_lists "find" :: Name
   ```

3. **`Hydra.Sources.Libraries`** - The primitive registration:
   ```haskell
   prim2Eval _lists_find EvalLists.find [_x] (function x_ boolean) (list x_) (optional x_),
   ```

4. **`Hydra.Sources.Eval.Lib.Lists`** - The interpreter-friendly definition:
   ```haskell
   find_ :: TTermDefinition (Term -> Term -> Flow s Term)
   find_ = define "find" $ ...
   ```

5. **`Hydra.Dsl.Meta.Lib.Lists`** - The DSL helper:
   ```haskell
   find :: TTerm (a -> Bool) -> TTerm [a] -> TTerm (Maybe a)
   find = primitive2 _lists_find
   ```

6. **`Hydra.Eval.Lib.Lists` (generated)** - Initially copy from source, then regenerate

### Common Map-to-List Conversion Patterns

| Map Operation | List Equivalent |
|--------------|-----------------|
| `Maps.lookup name map` | `Lists.find (\b -> bindingName b == name) list` |
| `Maps.elems map` | Direct list access (no wrapper needed) |
| `Maps.fromList pairs` | The list itself |
| `Maps.union m1 m2` | `Lists.concat2 l1 l2` |
| `Maps.empty` | `list []` (empty list) |

### Steps Performed

1. **Updated type definition** in `Hydra/Sources/Kernel/Types/Graph.hs`:
   ```haskell
   -- From:
   graphElements :: T.map Core.name Core.binding
   -- To:
   graphElements :: T.list Core.binding
   ```

2. **Updated generated type** in `dist/haskell/hydra-kernel/src/main/haskell/Hydra/Graph.hs`:
   ```haskell
   -- From:
   graphElements :: (M.Map Core.Name Core.Binding)
   -- To:
   graphElements :: [Core.Binding]
   ```

3. **Updated DSL helpers** in `Hydra/Dsl/Meta/Graph.hs`:
   ```haskell
   -- Updated function signatures to use [Binding] instead of M.Map Name Binding
   graph :: ...
   graphElements :: TTerm Graph -> TTerm [Binding]
   graphWithElements :: [Binding] -> TTerm Graph
   ```

4. **Added Lists.find primitive** (see above)

5. **Updated kernel source files** - Key changes:
   - `Lexical.hs`: `lookupElement` uses `Lists.find` instead of `Maps.lookup`
   - `Schemas.hs`: `typesToElements` returns `[Binding]` instead of `M.Map Name Binding`
   - `Adapt/Simple.hs`: Element manipulation uses list operations
   - Many files: Removed `Maps.elems` wrapper around `graphElements`

6. **Updated extension package files**:
   - `Python/Coder.hs`: Changed element lookup pattern
   - `Analysis/Dependencies.hs`, `Summaries.hs`, `AvroWorkflows.hs`: Removed `M.elems` wrappers

### DSL Equality in Source Files

When writing predicates in Hydra DSL source files, use `Equality.equal` instead of backtick operators:

```haskell
-- Correct:
Lists.find ("b" ~> Equality.equal (Core.bindingName (var "b")) (var "name")) elements

-- Incorrect (won't compile in DSL):
Lists.find ("b" ~> (Core.bindingName (var "b")) `eq` (var "name")) elements
```

### Files Changed

**Types (Sources + Generated):**
- `Hydra/Sources/Kernel/Types/Graph.hs` - Type definition source
- `dist/haskell/hydra-kernel/src/main/haskell/Hydra/Graph.hs` - Generated type

**DSL Helpers:**
- `Hydra/Dsl/Meta/Graph.hs` - Graph construction helpers

**New Primitive (6 files):**
- `Hydra/Lib/Lists.hs`
- `Hydra/Staging/Lib/Names.hs`
- `Hydra/Sources/Libraries.hs`
- `Hydra/Sources/Eval/Lib/Lists.hs`
- `Hydra/Dsl/Meta/Lib/Lists.hs`
- `Hydra/Eval/Lib/Lists.hs` (generated)

**Kernel Terms (Sources + Generated):**
- `Hydra/Sources/Kernel/Terms/Lexical.hs` + `Hydra/Lexical.hs`
- `Hydra/Sources/Kernel/Terms/Schemas.hs` + `Hydra/Schemas.hs`
- `Hydra/Sources/Kernel/Terms/Inference.hs` + `Hydra/Inference.hs`
- `Hydra/Sources/Kernel/Terms/Templates.hs` + `Hydra/Templates.hs`
- `Hydra/Sources/Kernel/Terms/Adapt/Simple.hs` + `Hydra/Adapt/Simple.hs`
- `Hydra/Sources/Kernel/Terms/Show/Graph.hs` + `Hydra/Show/Graph.hs`
- `Hydra/Sources/Kernel/Terms/Haskell/Coder.hs` + `Hydra/Haskell/Coder.hs`

**Extension packages (packages/hydra-ext, etc.):**
- `Hydra/Python/Coder.hs`
- `Hydra/Python/TestCodec.hs`
- `Hydra/Tools/Analysis/Dependencies.hs`
- `Hydra/Tools/Analysis/Summaries.hs`
- `Hydra/Tools/AvroWorkflows.hs`

**Other:**
- `heads/haskell/src/exec/verify-json-kernel/Main.hs`
- `Hydra/Generation.hs`

### Key Lessons

1. **Update generated files first**: When changing fundamental types, update files under `dist/haskell/<pkg>/`
   first so the project compiles, then update source files.

2. **Adding primitives is multi-file**: Plan for updating 6 files when adding a new primitive function.

3. **Watch for transitive dependencies**: A type change in `Graph` propagates through many modules that use
   `graphElements`.

4. **DSL has its own syntax**: The Hydra DSL uses `Equality.equal` and similar functions, not Haskell infix operators.

5. **Return types matter**: Functions like `typesToElements` that return the changed type need signature updates in
   both source and generated files.

6. **Test incrementally**: Build after each major file change to catch errors early rather than facing many errors at
   once.

7. **Pre-existing bugs may surface**: Regeneration may reveal pre-existing issues (e.g., missing type constraints).
   Be prepared to restore files from git if regeneration introduces unrelated problems.
