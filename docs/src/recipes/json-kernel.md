# Exporting and Verifying the Hydra Kernel as JSON

This recipe explains how to export the Hydra kernel to JSON and verify the export is consistent.

## Background

Hydra's kernel (types, modules, and test data) is defined in Haskell as the source of truth. To make this data available to other implementations (Python, Java, etc.) without embedding Haskell-specific code, the kernel is exported to JSON format.

The JSON export provides a language-agnostic representation of:
- All kernel modules (types, terms, dependencies)
- Module metadata (namespace, descriptions)
- Type definitions and term bindings

This supports [issue #243](https://github.com/CategoricalData/hydra/issues/243) - enabling introspection and testing in non-Haskell implementations.

## Generated Files

JSON files are generated to `hydra-haskell/src/gen-main/json/`:

```
src/gen-main/json/
├── hydra/
│   ├── core.json           # Core types and terms
│   ├── module.json         # Module type definitions
│   ├── graph.json          # Graph structures
│   ├── adapt/
│   │   ├── literals.json
│   │   ├── modules.json
│   │   └── ...
│   ├── decode/
│   │   ├── core.json
│   │   └── ...
│   └── ...
```

Each file contains a single module encoded as JSON, using Hydra's type-directed JSON encoding.

## Quick Commands

### Export kernel to JSON

```bash
cd hydra-haskell
./bin/update-json-kernel.sh
```

### Verify JSON consistency

```bash
cd hydra-haskell
./bin/verify-json-kernel.sh
```

Both scripts build the necessary executables automatically.

## When to Run

### Update JSON (update-json-kernel)

Run after making changes to:
- Kernel type definitions (`Hydra.Sources.*`)
- Module structure or metadata
- Term encodings or bindings

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
stack build hydra:update-json-kernel hydra:verify-json-kernel
```

### Running update

```bash
stack exec update-json-kernel
```

Output:
```
=== Generate Hydra kernel JSON ===

Generating 103 kernel modules to JSON...

=== Done! ===

Generated files are in: src/gen-main/json/
```

### Running verification

```bash
stack exec verify-json-kernel
```

Output (success):
```
=== Verify JSON Kernel ===

Building graph from kernel modules...
Building schema map for JSON decoding...
  Schema map has 190 type definitions
Using TypeVariable for Module type (decoder will resolve it).
Counting modules...
Verifying 103 kernel modules...

  Processing: hydra.accessors
    JSON parsed successfully
    JSON decoded to Term
  ✓ hydra.accessors
  ...

=== SUCCESS ===
All 103 modules verified successfully!
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
| `src/exec/update-json-kernel/Main.hs` | Executable to generate JSON files |
| `src/exec/verify-json-kernel/Main.hs` | Executable to verify JSON consistency |
| `bin/update-json-kernel.sh` | Wrapper script for generation |
| `bin/verify-json-kernel.sh` | Wrapper script for verification |
| `src/gen-main/json/` | Generated JSON output directory |

### Dependencies

- **update-json-kernel**: Uses `Hydra.Generation.writeModulesJson`
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
