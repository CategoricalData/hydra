# Issue 261: Remove Aeson and HsYAML Dependencies

**Issue:** https://github.com/CategoricalData/hydra/issues/261
**Status:** Complete
**Date:** 2026-03-07

## Summary

Remove the `aeson`, `aeson-pretty`, and `HsYAML` third-party dependencies from `hydra-haskell` and `hydra-ext`, since Hydra now has native JSON support (parser + writer) and YAML support is being addressed separately (#224).

## Current Usage

### Aeson (`Data.Aeson`)

1. **`Hydra.Generation`** (`hydra-haskell/src/main/haskell/Hydra/Generation.hs`)
   - `parseJsonFile`: Uses `A.eitherDecode` to parse JSON files, then converts to Hydra JSON via `aesonToHydra`
   - `readManifestField` and `loadModulesFromJson`: Call `parseJsonFile`
   - Also imports `Data.Scientific` and `Data.Vector` (only used alongside Aeson)
   - **Replacement:** Use `Hydra.Json.Parser.parseJson` (native Hydra parser) instead

2. **`Hydra.Staging.Json.Serde`** (`hydra-haskell/src/test/haskell/Hydra/Staging/Json/Serde.hs`)
   - Legacy JSON serializer/deserializer used as sanity check
   - Provides `jsonValueToString`, `stringToJsonValue`, `jsonByteStringCoder`, `jsonStringCoder`
   - **Replacement:** Can be removed entirely; the native JSON writer/parser are the primary implementation now

3. **`Hydra.Json.AesonSpec`** (`hydra-haskell/src/test/haskell/Hydra/Json/AesonSpec.hs`)
   - Cross-implementation round-trip tests comparing Aeson with native Hydra JSON
   - **Replacement:** Remove this test file; native JSON tests are sufficient

4. **`verify-json-kernel/Main.hs`** (`hydra-haskell/src/exec/verify-json-kernel/Main.hs`)
   - Standalone executable that verifies kernel modules round-trip through JSON
   - Uses Aeson for fast JSON parsing
   - **Replacement:** Use `Hydra.Json.Parser.parseJson` instead

### HsYAML (`Data.YAML`)

1. **`Hydra.Staging.Yaml.Serde`** (`hydra-haskell/src/main/haskell/Hydra/Staging/Yaml/Serde.hs`)
   - Converts between HsYAML nodes and Hydra YAML nodes
   - Provides `yamlByteStringCoder`, `yamlStringCoder`, `hydraYamlToString`, etc.
   - Used by `Hydra.Staging.Yaml.Modules` for YAML generation
   - **Replacement:** Needs a native YAML writer (see #224); HsYAML removal may be blocked on this

2. **`Hydra.Staging.Yaml.SerdeSpec`** (`hydra-haskell/src/test/haskell/Hydra/Staging/Yaml/SerdeSpec.hs`)
   - Tests for YAML serialization, depends on HsYAML via Serde
   - **Replacement:** Will need native YAML tests once #224 provides a native YAML writer

### `aeson-pretty` (hydra-ext only)

- Listed as a dependency in `hydra-ext/package.yaml` but **not imported in any `.hs` file** in hydra-ext
- **Replacement:** Simply remove from `package.yaml`

### Bootstrapping demo

- `hydra-ext/demos/bootstrapping/resources/haskell/package.yaml` also lists `aeson` and `HsYAML`
- Should be updated in tandem

## Dependency Cascade

Removing `aeson` also allows removing `scientific` and `vector` from hydra-haskell (only used in Aeson conversion code). However, `scientific` and `vector` are still listed in `hydra-ext/package.yaml` — need to verify if hydra-ext uses them independently.

## Implementation Plan

### Phase 1: Remove Aeson from hydra-haskell

1. **Replace `parseJsonFile` in `Hydra.Generation`** with native `Hydra.Json.Parser.parseJson`
   - Read file as String, parse with native parser
   - Handle `escapeControlCharsInJson` pre-processing (currently operates on ByteString; may need String equivalent)
   - Remove `aesonToHydra`, `Data.Aeson`, `Data.Scientific`, `Data.Vector` imports

2. **Replace `parseJsonFile`/`aesonToHydra` in `verify-json-kernel/Main.hs`** similarly

3. **Remove `Hydra.Staging.Json.Serde`** (test helper module)

4. **Remove `Hydra.Json.AesonSpec`** (cross-implementation test)

5. **Remove `aeson` from `hydra-haskell/package.yaml`**
   - Also remove `scientific` and `vector` if no longer needed

### Phase 2: Remove Aeson from hydra-ext

1. **Remove `aeson`, `aeson-pretty` from `hydra-ext/package.yaml`**
   - No source files in hydra-ext import Aeson
   - Also remove `scientific` and `vector` if unused

2. **Update bootstrapping demo `package.yaml`**

### Phase 3: Remove HsYAML (may be blocked on #224)

1. **Replace `Hydra.Staging.Yaml.Serde`** with native YAML writer
   - Requires native YAML serialization (issue #224)
   - `hydraYamlToString` is the key function used by `Hydra.Staging.Yaml.Modules`

2. **Update or remove `Hydra.Staging.Yaml.SerdeSpec`**

3. **Remove `HsYAML` from all `package.yaml` files**

## Risks

- **Performance:** Native Hydra JSON parser may be slower than Aeson for large files (kernel JSON modules can be large). The `verify-json-kernel` executable explicitly notes using "Aeson (fast!)". May need benchmarking.
- **HsYAML removal blocked:** If #224 (native YAML support) is not complete, HsYAML cannot be fully removed yet. Aeson removal can proceed independently.
- **Control character escaping:** `escapeControlCharsInJson` in `Generation.hs` operates on ByteString via Aeson. Need to verify the native parser handles these cases.

## References

- Issue #224: YAML serde (native YAML support)
- Issue #242: JSON coder v2 (native JSON encode/decode — completed)
- Native JSON parser: `hydra-haskell/src/gen-main/haskell/Hydra/Json/Parser.hs`
- Native JSON writer: `hydra-haskell/src/gen-main/haskell/Hydra/Json/Writer.hs`
