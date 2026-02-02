# Refactoring Hydra Namespaces

This recipe documents the process for renaming or moving a Hydra namespace (module) across all implementations. This is a comprehensive task that affects multiple repositories and requires careful coordination.

> **See also**: For simpler refactoring operations (creating, deleting, or renaming individual elements), see [Refactoring the Hydra Kernel](refactoring.md).

## Overview

A Hydra namespace like `hydra.json` corresponds to:
- A Haskell source module (e.g., `Hydra/Sources/Kernel/Types/Json.hs`)
- Generated Haskell code (e.g., `Hydra/Json.hs`)
- Generated decoder/encoder source modules (e.g., `Hydra/Sources/Decode/Json.hs`, `Hydra/Sources/Encode/Json.hs`)
- Generated decoder/encoder implementations (e.g., `Hydra/Decode/Json.hs`, `Hydra/Encode/Json.hs`)
- Generated Python code (e.g., `hydra/json.py` or `hydra/json/__init__.py`)
- Generated Java code (e.g., `hydra/json/Value.java`)
- JSON kernel exports (e.g., `hydra/json.json`)

Renaming a namespace requires updating all of these, plus any code that imports from them.

## When You Might Need This

- Resolving module/package conflicts (e.g., Python can't have both `json.py` and `json/` directory)
- Reorganizing the namespace hierarchy
- Preparing for semantic versioning with a cleaner API surface

## Prerequisites

- Working Haskell build environment (`stack build` succeeds in hydra-haskell)
- Understanding of Hydra's module system and DSL

## Step-by-Step Process

### Phase 1: Update the Source Module

1. **Move/rename the source file**
   ```bash
   # Example: moving Json.hs to Sources/Json/Model.hs
   mkdir -p hydra-haskell/src/main/haskell/Hydra/Sources/Json
   mv hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Types/Json.hs \
      hydra-haskell/src/main/haskell/Hydra/Sources/Json/Model.hs
   ```

2. **Update the namespace declaration in the source**
   ```haskell
   -- Change from:
   ns = Namespace "hydra.json"
   -- To:
   ns = Namespace "hydra.json.model"
   ```

3. **Update the Haskell module declaration**
   ```haskell
   -- Change from:
   module Hydra.Sources.Kernel.Types.Json where
   -- To:
   module Hydra.Sources.Json.Model where
   ```

### Phase 2: Update hydra-haskell References

1. **Update module registry files**
   - `Hydra/Sources/Kernel/Types/All.hs` - Update imports and module lists for the type module
   - `Hydra/Sources/Kernel/Terms/All.hs` - Update imports for the decoder/encoder modules:
     ```haskell
     -- Change:
     import qualified Hydra.Sources.Decode.Json as DecodeJson
     import qualified Hydra.Sources.Encode.Json as EncodeJson
     -- To:
     import qualified Hydra.Sources.Decode.Json.Model as DecodeJson
     import qualified Hydra.Sources.Encode.Json.Model as EncodeJson
     ```

2. **Update files that reference the old namespace**
   Use grep to find all references:
   ```bash
   grep -rn 'Hydra\.Json[^.]' src/main/haskell/
   grep -rn 'hydra\.json[^.]' src/main/haskell/
   ```

   Update imports:
   ```haskell
   -- Change:
   import qualified Hydra.Json as Json
   -- To:
   import qualified Hydra.Json.Model as Json
   ```

3. **Bootstrap the generated module**
   If the generated code doesn't exist yet, create a minimal version:
   ```bash
   mkdir -p src/gen-main/haskell/Hydra/Json
   # Create Model.hs with necessary exports
   ```

4. **Build and verify**
   ```bash
   stack build
   ```

5. **Move and update decoder/encoder modules**
   The generated decoder and encoder modules need to be moved to match the new namespace structure:
   ```bash
   # Move source decoder/encoder modules
   mkdir -p src/gen-main/haskell/Hydra/Sources/Decode/Json
   mkdir -p src/gen-main/haskell/Hydra/Sources/Encode/Json
   mv src/gen-main/haskell/Hydra/Sources/Decode/Json.hs \
      src/gen-main/haskell/Hydra/Sources/Decode/Json/Model.hs
   mv src/gen-main/haskell/Hydra/Sources/Encode/Json.hs \
      src/gen-main/haskell/Hydra/Sources/Encode/Json/Model.hs

   # Move implementation decoder/encoder modules
   mkdir -p src/gen-main/haskell/Hydra/Decode/Json
   mkdir -p src/gen-main/haskell/Hydra/Encode/Json
   mv src/gen-main/haskell/Hydra/Decode/Json.hs \
      src/gen-main/haskell/Hydra/Decode/Json/Model.hs
   mv src/gen-main/haskell/Hydra/Encode/Json.hs \
      src/gen-main/haskell/Hydra/Encode/Json/Model.hs
   ```

6. **Update module declarations in moved files**
   Update the module declarations to match the new paths:
   ```bash
   # Update module names
   perl -i -pe 's/module Hydra\.Sources\.Decode\.Json where/module Hydra.Sources.Decode.Json.Model where/g' \
     src/gen-main/haskell/Hydra/Sources/Decode/Json/Model.hs
   perl -i -pe 's/module Hydra\.Sources\.Encode\.Json where/module Hydra.Sources.Encode.Json.Model where/g' \
     src/gen-main/haskell/Hydra/Sources/Encode/Json/Model.hs
   perl -i -pe 's/module Hydra\.Decode\.Json where/module Hydra.Decode.Json.Model where/g' \
     src/gen-main/haskell/Hydra/Decode/Json/Model.hs
   perl -i -pe 's/module Hydra\.Encode\.Json where/module Hydra.Encode.Json.Model where/g' \
     src/gen-main/haskell/Hydra/Encode/Json/Model.hs
   ```

7. **Update namespace references in generated files**
   The generated files contain namespace strings that need updating:
   ```bash
   # Update namespace strings (e.g., in type names, error messages)
   perl -i -pe 's/hydra\.json\.Value/hydra.json.model.Value/g' \
     src/gen-main/haskell/Hydra/Sources/Decode/Json/Model.hs \
     src/gen-main/haskell/Hydra/Decode/Json/Model.hs \
     src/gen-main/haskell/Hydra/Sources/Encode/Json/Model.hs \
     src/gen-main/haskell/Hydra/Encode/Json/Model.hs

   # Update decoder function references (e.g., hydra.decode.json.value -> hydra.decode.json.model.value)
   perl -i -pe 's/hydra\.decode\.json\.value/hydra.decode.json.model.value/g' \
     src/gen-main/haskell/Hydra/Sources/Decode/Testing.hs
   perl -i -pe 's/hydra\.encode\.json\.value/hydra.encode.json.model.value/g' \
     src/gen-main/haskell/Hydra/Sources/Encode/Testing.hs
   ```

8. **Update import aliases in generated files**
   Some generated files import the type module with an alias that should be updated:
   ```bash
   # Change "import qualified Hydra.Json.Model as Json" to "import qualified Hydra.Json.Model as Model"
   # in files that use the module for types (not the namespace DSL)
   perl -i -pe 's/import qualified Hydra\.Json\.Model as Json/import qualified Hydra.Json.Model as Model/g' \
     src/gen-main/haskell/Hydra/Json/Decode.hs \
     src/gen-main/haskell/Hydra/Json/Encode.hs \
     src/gen-main/haskell/Hydra/Testing.hs

   # Then update the type references
   perl -i -pe 's/Json\.Value/Model.Value/g; s/Json\.ValueNull/Model.ValueNull/g; s/Json\.ValueArray/Model.ValueArray/g' \
     src/gen-main/haskell/Hydra/Json/Decode.hs \
     src/gen-main/haskell/Hydra/Testing.hs
   ```

9. **Update files that import the decoder/encoder modules**
   ```bash
   # Update imports in Testing.hs and other files
   perl -i -pe 's/import qualified Hydra\.Decode\.Json as Json/import qualified Hydra.Decode.Json.Model as Json/g' \
     src/gen-main/haskell/Hydra/Decode/Testing.hs
   perl -i -pe 's/import qualified Hydra\.Encode\.Json as Json/import qualified Hydra.Encode.Json.Model as Json/g' \
     src/gen-main/haskell/Hydra/Encode/Testing.hs
   ```

10. **Update test files**
    ```bash
    # Update imports in test files
    perl -i -pe 's/import Hydra\.Json \(Value\)/import Hydra.Json.Model (Value)/g' \
      src/test/haskell/Hydra/Json/AesonSpec.hs

    # Update generated test files
    perl -i -pe 's/Json\.Value/Model.Value/g' \
      src/gen-test/haskell/Hydra/Test/Json/*.hs
    ```

11. **Regenerate decoder/encoder source modules**
    The decoder and encoder source modules are generated from type definitions. Use GHCI to regenerate them:
    ```bash
    stack ghci
    ```
    Then in GHCI:
    ```haskell
    import Hydra.Sources.All
    import Hydra.Generation
    writeDecoderSourceHaskell "src/gen-main/haskell" mainModules kernelTypesModules
    writeEncoderSourceHaskell "src/gen-main/haskell" mainModules kernelTypesModules
    :quit
    ```
    This regenerates the `Hydra.Sources.Decode.*` and `Hydra.Sources.Encode.*` modules with correct namespace references.

12. **Clean up orphan files**
    After regeneration, remove any orphan files left at the old locations:
    ```bash
    # Remove old decoder/encoder files if they still exist
    rm -f src/gen-main/haskell/Hydra/Sources/Decode/Json.hs
    rm -f src/gen-main/haskell/Hydra/Sources/Encode/Json.hs
    rm -f src/gen-main/haskell/Hydra/Decode/Json.hs
    rm -f src/gen-main/haskell/Hydra/Encode/Json.hs
    rm -f src/gen-main/haskell/Hydra/Json.hs
    ```

13. **Rebuild after regeneration**
    ```bash
    stack build
    ```

14. **Regenerate and verify JSON kernel**
    ```bash
    ./bin/update-json-kernel.sh
    ./bin/verify-json-kernel.sh
    ```

15. **Run tests**
    ```bash
    stack test
    ```

### Phase 3: Update hydra-ext References

1. **Update source module imports**
   ```bash
   grep -rln 'import.*Hydra\.Json[^.]' src/main/haskell/
   ```

   Update each file to use the new namespace.

2. **Update generated files**
   ```bash
   grep -rln 'import.*Hydra\.Json[^.]' src/gen-main/haskell/
   ```

3. **Build and verify**
   ```bash
   stack build
   ```

### Phase 4: Regenerate Python Implementation

1. **Run the Python kernel generator**
   From the hydra-ext directory, use the shell script (which includes proper RTS flags to avoid stack overflow):
   ```bash
   cd hydra-ext
   ./bin/update-python-kernel.sh
   ```

2. **Verify the new module structure**
   ```bash
   ls -la ../hydra-python/src/main/python/hydra/json/
   # Should show: model.py (instead of old json.py)
   ```

3. **Clean up orphan Python files**
   Remove any old Python files at the previous locations:
   ```bash
   rm -f ../hydra-python/src/main/python/hydra/json.py
   ```

4. **Test the Python implementation**
   ```bash
   cd ../hydra-python
   source .venv/bin/activate
   PYTHONPATH=src/main/python:src/gen-test/python pytest src/test/python -x
   ```

### Phase 5: Update Java Implementation

Note: Java generation may have unrelated issues. Update what you can manually.

1. **Update hand-written utility files**
   ```bash
   perl -i -pe 's/import hydra\.json\.Value;/import hydra.json.model.Value;/g' \
     src/main/java/hydra/json/*.java
   ```

2. **Move generated files to new package**
   ```bash
   mkdir -p src/gen-main/java/hydra/json/model
   mv src/gen-main/java/hydra/json/Value.java src/gen-main/java/hydra/json/model/
   perl -i -pe 's/package hydra\.json;/package hydra.json.model;/g' \
     src/gen-main/java/hydra/json/model/Value.java
   perl -i -pe 's/hydra\.json\.Value/hydra.json.model.Value/g' \
     src/gen-main/java/hydra/json/model/Value.java
   ```

3. **Update testing files**
   ```bash
   perl -i -pe 's/hydra\.json\.Value/hydra.json.model.Value/g' \
     src/gen-main/java/hydra/testing/*.java
   ```

## Common Pitfalls

### Chicken-and-Egg Bootstrap Problem
Generated Haskell code depends on modules that need to be generated. Solution:
1. Create minimal bootstrap versions of generated modules
2. Build incrementally
3. Regenerate fully once the build works

### Stale Generated Files
The decoder/encoder modules (`Hydra.Sources.Decode.*`, `Hydra.Sources.Encode.*`) contain hardcoded namespace strings in error messages and type references. To properly regenerate them, use GHCI:
```haskell
import Hydra.Sources.All
import Hydra.Generation
writeDecoderSourceHaskell "src/gen-main/haskell" mainModules kernelTypesModules
writeEncoderSourceHaskell "src/gen-main/haskell" mainModules kernelTypesModules
```

### Orphan Files
When modules are moved or regenerated, the old files may be left behind as "orphans". These must be manually deleted:
- After moving `Foo.hs` to `Foo/Bar.hs`, delete the old `Foo.hs`
- After regenerating decoder/encoder modules, delete files at old paths
- After regenerating Python, delete old `.py` files that are now packages

### Decoder/Encoder Module Paths
When renaming `hydra.foo` to `hydra.foo.bar`, the decoder/encoder modules also move:
- `Hydra.Sources.Decode.Foo` → `Hydra.Sources.Decode.Foo.Bar`
- `Hydra.Decode.Foo` → `Hydra.Decode.Foo.Bar`
- Same for Encode modules

The Terms/All.hs module registry needs to be updated to import from the new paths.

### Python Module/Package Conflicts
Python can't have both `foo.py` and `foo/` directory. This is often the motivation for namespace refactoring. The new namespace should use a structure that avoids this (e.g., `hydra/json/model.py` instead of `hydra/json.py` with `hydra/json/decode.py`).

### Import Alias Conventions
In **generated implementation code** (e.g., `Hydra/Json/Decode.hs`), the type module is typically imported as `Model`:
```haskell
import qualified Hydra.Json.Model as Model
```

In **DSL source code** (e.g., `Hydra/Sources/Json/Decode.hs`), you may keep a shorter alias for convenience:
```haskell
import qualified Hydra.Json.Model as Json
```

### Function Reference Updates
Generated code contains function references that include the namespace, like `hydra.decode.json.value`. When renaming, these become `hydra.decode.json.model.value`. Look for these patterns:
- `hydra.decode.<namespace>.value` → `hydra.decode.<new-namespace>.value`
- `hydra.encode.<namespace>.value` → `hydra.encode.<new-namespace>.value`

## Verification Checklist

- [ ] hydra-haskell builds (`stack build`)
- [ ] Decoder/encoder modules regenerated (GHCI: `writeDecoderSourceHaskell`, `writeEncoderSourceHaskell`)
- [ ] Orphan files cleaned up (old `.hs` files at previous locations)
- [ ] hydra-haskell tests pass (`stack test`)
- [ ] JSON kernel regenerated (`./bin/update-json-kernel.sh`)
- [ ] JSON kernel verified (`./bin/verify-json-kernel.sh`)
- [ ] hydra-ext builds (`stack build` in hydra-ext)
- [ ] Python kernel regenerated (`./bin/update-python-kernel.sh` in hydra-ext)
- [ ] Orphan Python files cleaned up (old `.py` files)
- [ ] Python tests pass (or at least don't regress)
- [ ] Java files updated (manual updates if generation fails)

## Files Typically Affected

In a namespace rename from `hydra.foo` to `hydra.foo.bar`:

**hydra-haskell source:**
- `src/main/haskell/Hydra/Sources/.../Foo.hs` → `src/main/haskell/Hydra/Sources/.../Foo/Bar.hs` (DSL source module)
- `src/main/haskell/Hydra/Sources/Kernel/Types/All.hs` (types registry)
- `src/main/haskell/Hydra/Sources/Kernel/Terms/All.hs` (terms registry, decoder/encoder imports)
- `src/test/haskell/Hydra/Foo/*.hs` (test files)

**hydra-haskell generated:**
- `src/gen-main/haskell/Hydra/Foo.hs` → `src/gen-main/haskell/Hydra/Foo/Bar.hs` (generated types)
- `src/gen-main/haskell/Hydra/Sources/Decode/Foo.hs` → `src/gen-main/haskell/Hydra/Sources/Decode/Foo/Bar.hs`
- `src/gen-main/haskell/Hydra/Sources/Encode/Foo.hs` → `src/gen-main/haskell/Hydra/Sources/Encode/Foo/Bar.hs`
- `src/gen-main/haskell/Hydra/Decode/Foo.hs` → `src/gen-main/haskell/Hydra/Decode/Foo/Bar.hs`
- `src/gen-main/haskell/Hydra/Encode/Foo.hs` → `src/gen-main/haskell/Hydra/Encode/Foo/Bar.hs`
- `src/gen-main/haskell/Hydra/Sources/Decode/Testing.hs` (function references)
- `src/gen-main/haskell/Hydra/Sources/Encode/Testing.hs` (function references)
- `src/gen-main/haskell/Hydra/Testing.hs` (type imports)
- `src/gen-test/haskell/Hydra/Test/Foo/*.hs` (generated test files)

**hydra-ext:**
- `src/main/haskell/Hydra/Ext/Staging/*/Coder.hs` (various coders)
- `src/gen-main/haskell/Hydra/Ext/*/*.hs` (generated files)

**hydra-python:**
- `src/main/python/hydra/foo.py` → `src/main/python/hydra/foo/bar.py`

**hydra-java:**
- `src/gen-main/java/hydra/foo/Type.java` → `src/gen-main/java/hydra/foo/bar/Type.java`
- `src/main/java/hydra/foo/*.java` (hand-written utilities)
