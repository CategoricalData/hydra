# Refactoring the Hydra Kernel

This recipe documents how to create, rename, move, or delete kernel elements (definitions) and modules (namespaces), and propagate the changes across all implementations.

## Overview

Hydra kernel code lives in multiple places:
- **Source modules** (`hydra-haskell/src/main/haskell/Hydra/Sources/...`) - DSL definitions
- **Generated Haskell** (`hydra-haskell/src/gen-main/haskell/Hydra/...`) - Generated implementations
- **Generated Python** (`hydra-python/src/main/python/hydra/...`)
- **Generated Java** (`hydra-java/src/gen-main/java/hydra/...`)
- **JSON kernel** (`hydra-haskell/src/gen-main/json/...`)

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
| **Move/rename module** | See [detailed section](#moving-or-renaming-modules) |

## Prerequisites

- Working Haskell build environment (`stack build` succeeds in hydra-haskell)
- Understanding of Hydra's module system and DSL
- For Python: virtual environment set up in hydra-python
- For Java: Gradle configured in hydra-java

---

## Creating a New Module

### Step 1: Create the Source File

Create a new file in the appropriate location under `hydra-haskell/src/main/haskell/Hydra/Sources/`.

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
module_ = Module ns elements
    [DependencyModule1.ns, DependencyModule2.ns]  -- dependencies
    kernelTypesNamespaces $
    Just "Description of this module"
  where
    elements = [
      toBinding myFunction1,
      toBinding myFunction2]

-- Helper for defining elements
define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- Define elements
myFunction1 :: TBinding (Int -> Int)
myFunction1 = define "myFunction1" $
  doc "Description of myFunction1" $
  "x" ~> var "x"

myFunction2 :: TBinding (String -> String)
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

```bash
cd hydra-haskell

# Build to verify the source compiles
stack build

# Regenerate Haskell
stack ghci
> import Hydra.Generation
> import Hydra.Sources.All
> writeHaskell "src/gen-main/haskell" mainModules kernelModules
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
# Python (from hydra-ext)
cd ../hydra-ext
./bin/update-python-kernel.sh

# Java (from hydra-ext)
./bin/update-java-kernel-types.sh  # for types
./bin/update-java-kernel.sh        # for terms (may have issues)
```

---

## Creating a New Element (Definition)

### Step 1: Add the Definition

In the source module, add the new definition:

```haskell
myNewFunction :: TBinding (A -> B)
myNewFunction = define "myNewFunction" $
  doc "Description" $
  "x" ~> someExpression (var "x")
```

### Step 2: Register in the Module's Element List

```haskell
elements = [
  -- ... existing elements ...
  toBinding myNewFunction]
```

### Step 3: Build and Regenerate

Same as for creating a module - build, regenerate Haskell, rebuild, regenerate other implementations.

---

## Deleting an Element

### Step 1: Remove from Element List

In the source module, remove the element from the `elements` list.

### Step 2: Update All References

Search for references to the deleted element:
```bash
grep -rn 'myDeletedFunction' src/main/haskell/
grep -rn 'hydra.mymodule.myDeletedFunction' src/
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
grep -rn 'MyModule' src/main/haskell/
grep -rn 'hydra.mymodule' src/
```

### Step 3: Delete the Source File

```bash
rm src/main/haskell/Hydra/Sources/Kernel/Terms/MyModule.hs
```

### Step 4: Delete Generated Files

```bash
rm src/gen-main/haskell/Hydra/MyModule.hs
rm -rf ../hydra-python/src/main/python/hydra/mymodule/
rm -rf ../hydra-java/src/gen-main/java/hydra/mymodule/
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
grep -rn 'hydra.oldmodule.foo' src/
grep -rn 'OldModule.foo' src/
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
elements = [
  -- Change:
  toBinding myOldName
  -- To:
  toBinding myNewName]
```

### Step 3: Update All References

```bash
grep -rn 'myOldName' src/
grep -rn 'hydra.mymodule.myOldName' src/
```

### Step 4: Build and Regenerate

---

## Moving or Renaming Modules

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
   mkdir -p hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Terms/Foo
   mv hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Terms/Foo.hs \
      hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Terms/Foo/Bar.hs
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
   grep -rn 'Hydra\.Foo[^.]' src/main/haskell/
   grep -rn 'hydra\.foo[^.]' src/main/haskell/
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
   mkdir -p src/gen-main/haskell/Hydra/Foo
   mv src/gen-main/haskell/Hydra/Foo.hs src/gen-main/haskell/Hydra/Foo/Bar.hs
   ```

2. **Update module declarations in generated files**
   ```bash
   perl -i -pe 's/module Hydra\.Foo where/module Hydra.Foo.Bar where/g' \
     src/gen-main/haskell/Hydra/Foo/Bar.hs
   ```

3. **Update namespace strings in generated files**
   ```bash
   perl -i -pe 's/hydra\.foo\.Element/hydra.foo.bar.Element/g' \
     src/gen-main/haskell/Hydra/Foo/Bar.hs
   ```

4. **Clean up orphan files**
   ```bash
   rm -f src/gen-main/haskell/Hydra/Foo.hs  # old location
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

# Regenerate Python (from hydra-ext)
cd ../hydra-ext
./bin/update-python-kernel.sh

# Clean up orphan Python files
rm -f ../hydra-python/src/main/python/hydra/foo.py

# Regenerate Java
./bin/update-java-kernel-types.sh
```

---

## Common Pitfalls

### Chicken-and-Egg Bootstrap Problem
Generated Haskell code depends on modules that need to be generated. Solution:
1. Create minimal bootstrap versions of generated modules
2. Build incrementally
3. Regenerate fully once the build works

### Orphan Files
When modules are moved or regenerated, old files may be left behind:
- After moving `Foo.hs` to `Foo/Bar.hs`, delete the old `Foo.hs`
- After regenerating Python, delete old `.py` files that are now packages

### Python Module/Package Conflicts
Python can't have both `foo.py` and `foo/` directory. Use a structure like `hydra/foo/bar.py` instead of `hydra/foo.py` alongside `hydra/foo/baz.py`.

### Decoder/Encoder Module Paths
When renaming `hydra.foo` to `hydra.foo.bar`, the decoder/encoder modules also move:
- `Hydra.Sources.Decode.Foo` → `Hydra.Sources.Decode.Foo.Bar`
- `Hydra.Decode.Foo` → `Hydra.Decode.Foo.Bar`

---

## Verification Checklist

- [ ] hydra-haskell builds (`stack build`)
- [ ] hydra-haskell tests pass (`stack test`)
- [ ] JSON kernel regenerated and verified
- [ ] hydra-ext builds (`stack build` in hydra-ext)
- [ ] Python kernel regenerated
- [ ] Python tests pass
- [ ] Java files updated
- [ ] Orphan files cleaned up
- [ ] All references updated (no broken imports)

---

## Example: Creating hydra.hoisting Module

This section documents the creation of `hydra.hoisting` by extracting hoisting functions from `hydra.reduction`.

### Context

The `hydra.reduction` module contained hoisting functions (`hoistSubterms`, `hoistCaseStatements`, etc.) that conceptually belong in a dedicated module focused on term hoisting. We created `hydra.hoisting` to separate these concerns.

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
   - `src/test/haskell/Hydra/TestSuiteSpec.hs`:
     - Added import for `Hydra.Hoisting`
     - Changed test references to use `Hoisting.*` functions

6. **Regenerated** implementations:
   ```bash
   # Build to verify source compiles
   stack build

   # Regenerate Haskell
   stack ghci hydra:lib -e 'import Hydra.Generation' \
     -e 'import Hydra.Sources.All' \
     -e 'writeHaskell "src/gen-main/haskell" mainModules kernelModules'

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
- `src/main/haskell/Hydra/Sources/Kernel/Terms/Hoisting.hs` (source module)

**Modified:**
- `src/main/haskell/Hydra/Sources/Kernel/Terms/Reduction.hs` (removed hoisting functions)
- `src/main/haskell/Hydra/Sources/Kernel/Terms/All.hs` (registered new module)
- `src/main/haskell/Hydra/Sources/Kernel/Terms/Adapt/Simple.hs` (updated imports)
- `src/test/haskell/Hydra/TestSuiteSpec.hs` (updated test imports)

**Generated:**
- `src/gen-main/haskell/Hydra/Hoisting.hs` (generated implementation)
- `src/gen-main/json/hydra/hoisting.json` (JSON kernel)
