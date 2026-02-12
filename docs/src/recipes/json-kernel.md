# Exporting and Verifying Hydra Modules as JSON

This recipe explains how to export Hydra modules to JSON and verify the export is consistent.

## Background

Hydra's kernel (types, modules, and test data) is defined in Haskell as the source of truth. To make this data available to other implementations (Python, Java, etc.) without embedding Haskell-specific code, the modules are exported to JSON format.

The JSON export provides a language-agnostic representation of:
- All kernel modules (types, terms, dependencies)
- Module metadata (namespace, descriptions)
- Type definitions and term bindings with inferred types

Type inference is performed on modules before export, so the JSON files contain type-annotated terms (see [issue #253](https://github.com/CategoricalData/hydra/issues/253)). This eliminates the need for downstream consumers to run inference themselves.

This supports [issue #243](https://github.com/CategoricalData/hydra/issues/243) - enabling introspection and testing in non-Haskell implementations.

## Generated Files

There are three sets of JSON exports:

**Kernel modules** (`hydra-haskell/src/gen-main/json/`):
```
src/gen-main/json/
├── hydra/
│   ├── core.json           # Core types and terms
│   ├── module.json         # Module type definitions
│   ├── graph.json          # Graph structures
│   └── ...
```

**Main modules** (also `hydra-haskell/src/gen-main/json/`):
Includes all kernel modules plus additional main modules (encoders, decoders, JSON support, etc.).

**Test modules** (`hydra-haskell/src/gen-test/json/`):
```
src/gen-test/json/
├── hydra/
│   └── test/
│       ├── ...
```

Each file contains a single module encoded as JSON, using Hydra's type-directed JSON encoding.

## Quick Commands

### Export kernel modules to JSON

```bash
cd hydra-haskell
./bin/update-json-kernel.sh
```

### Export all main modules to JSON

```bash
cd hydra-haskell
./bin/update-json-main.sh
```

### Export test modules to JSON

```bash
cd hydra-haskell
./bin/update-json-test.sh
```

### Verify JSON consistency (kernel)

```bash
cd hydra-haskell
./bin/verify-json-kernel.sh
```

All scripts build the necessary executables automatically.

## When to Run

### Update JSON

- **update-json-kernel**: Run after changes to kernel type definitions (`Hydra.Sources.*`), module structure, or term encodings
- **update-json-main**: Run after changes to any main modules (kernel + encoders, decoders, JSON support, etc.)
- **update-json-test**: Run after changes to test modules (`Hydra.Sources.Test.*`)

### Verify JSON (verify-json-kernel)

Run to confirm:
- JSON files are valid and parseable
- Decoded modules match original sources exactly
- JSON encoding/decoding is lossless

This is especially useful after:
- Changes to JSON encoding (`Hydra.Json.Encode`)
- Changes to JSON decoding (`Hydra.Json.Decode`)
- Changes to module encoding (`Hydra.Encode.Module`)
- Upgrading dependencies that affect serialization

## Manual Steps

### Building executables

```bash
cd hydra-haskell
stack build hydra:update-json-kernel hydra:update-json-main hydra:update-json-test hydra:verify-json-kernel
```

### Running update (kernel)

```bash
stack exec update-json-kernel
```

### Running update (main)

```bash
stack exec update-json-main
```

### Running update (test)

```bash
stack exec update-json-test
```

### Running verification

```bash
stack exec verify-json-kernel
```

Output (success):
```
=== SUCCESS ===
All 112 modules verified successfully!
```

Output (failure):
```
=== FAILED ===
3 modules failed verification:
  hydra.core: element count differs: 42 vs 41
  hydra.module: type differs for moduleNamespace
  ...
```

## How Verification Works

The verification process:

1. **Builds a graph** from all kernel modules to provide type information
2. **Creates a schema map** mapping type names to their definitions
3. **For each module**:
   - Reads the JSON file from `src/gen-main/json/`
   - Parses JSON using Aeson (fast native parser)
   - Decodes JSON to a Hydra Term using type-directed decoding
   - Decodes the Term to a Module structure
   - Compares with the original source module

Type-directed decoding uses the schema map to resolve type references (`TypeVariable`) and handle wrapped types (`TypeWrap`).

## Troubleshooting

### "file not found" errors

Ensure JSON files exist by running `update-json-kernel` first:
```bash
./bin/update-json-kernel.sh
./bin/verify-json-kernel.sh
```

### "unknown type variable" errors

The JSON decoder couldn't resolve a type reference. This indicates:
- A missing type in the schema map
- A type that wasn't exported to JSON
- A mismatch between source and JSON

Check that the referenced type exists in `kernelModules`.

### "unknown wrapped type" errors

Similar to type variable errors, but for newtype/wrapped types. Ensure the wrapped type's definition is included in the kernel modules.

### "content mismatch" errors

The decoded module differs from the original. The error message indicates:
- Which field differs (namespace, elements, dependencies, description)
- For element differences, which binding differs (name, type, or term)

Debug by:
1. Examining the specific JSON file
2. Checking recent changes to the module source
3. Comparing field by field using the verification output

### Verification is slow

Verification builds the full module graph and schema, which takes time. This is expected for 100+ modules. The tool uses Aeson for fast JSON parsing to minimize overhead.

## Architecture

### Key files

| File | Purpose |
|------|---------|
| `src/exec/update-json-kernel/Main.hs` | Executable to generate kernel module JSON files |
| `src/exec/update-json-main/Main.hs` | Executable to generate main module JSON files |
| `src/exec/update-json-test/Main.hs` | Executable to generate test module JSON files |
| `src/exec/verify-json-kernel/Main.hs` | Executable to verify JSON consistency |
| `bin/update-json-kernel.sh` | Wrapper script for kernel generation |
| `bin/update-json-main.sh` | Wrapper script for main generation |
| `bin/update-json-test.sh` | Wrapper script for test generation |
| `bin/verify-json-kernel.sh` | Wrapper script for verification |
| `src/gen-main/json/` | Generated JSON output (kernel + main) |
| `src/gen-test/json/` | Generated JSON output (test) |

### Dependencies

- **update-json-kernel/main/test**: Uses `Hydra.Generation.writeModulesJson` with type inference
- **verify-json-kernel**: Uses `Hydra.Json.Decode.fromJson` with type-directed decoding

### JSON encoding format

Modules are encoded using Hydra's compact JSON format:
- Records omit type names (inferred from schema)
- Unions use single-key objects (`{"variant": value}`)
- Wrapped types encode the inner value directly
- Maps use `[{"@key": k, "@value": v}, ...]` format
- Optionals use `null` for Nothing, `[value]` for Just

## Related Documentation

- [Synchronizing Hydra-Python](syncing-python.md) - Related workflow for Python artifacts
- [Creating a new Hydra implementation](new-implementation.md) - Using JSON kernel in new implementations
- [GitHub Issue #243](https://github.com/CategoricalData/hydra/issues/243) - Original feature request
