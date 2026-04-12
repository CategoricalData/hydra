# Generating code with Hydra

A guide to generating source code in target languages from Hydra module definitions.

## Prerequisites

- Familiarity with Hydra's module system (see [Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts))
- A working build environment for at least one host language (see [build
  instructions](../../CLAUDE.md#build-and-test-commands))

## Overview

Hydra modules can be defined in any host language's DSL (Haskell, Java, Python, Scala, Lisp) and generated into
any target language. There are two source representations for modules:

- **DSL modules** are human-authored source code, designed for readability and maintainability.
  They are written in a host language's DSL and consumed directly by the `writeXxx` functions.
- **JSON modules** are a language-neutral interchange format, exported from DSL modules.
  They allow any host language to load modules and generate code without depending on the
  original authoring language. The bootstrap CLI consumes these.

The typical workflow is:

```
DSL modules  ---(export)--->  JSON modules  ---(bootstrap)--->  generated code
(human-authored)              (interchange)                      (any target language)
```

For Hydra's own kernel, the DSL modules are in Haskell, but this is not a requirement.
Any host language's DSL can be the source of truth for a given set of modules.

## The writeXxx functions

All code generation functions follow the same signature pattern:

```
writeXxx(outputDir, universeModules, modulesToGenerate) -> filesWritten
```

- **outputDir**: The base directory where generated files are written.
- **universeModules**: All modules needed for type and term resolution. This is the
  dependency context -- it must include all transitive dependencies of the modules being
  generated, even if those dependencies are not themselves being generated.
- **modulesToGenerate**: The subset of modules that actually get written to files.

The universe and generate lists are often the same, but they differ when you generate
a subset of modules that depend on others:

```haskell
-- Generate kernel, using kernel as its own universe (no external dependencies)
writeHaskell "../../dist/haskell/hydra-kernel/src/main/haskell" kernelModules kernelModules

-- Generate test modules, which depend on main modules.
-- Universe includes both; only tests are generated.
let allMods = mainModules ++ testModules
writeJava "../../dist/java/hydra-kernel/src/test/java" allMods testModules

-- Generate only ext modules, using main + ext as universe
writeJava "../../dist/java/hydra-misc/src/main/java" (mainModules ++ extModules) extModules
```

If the universe is missing a transitive dependency, generation may fail with
unresolved type references or produce incomplete output.

### Available targets

Full implementations (types + terms):

| Function | Target | Notes |
|----------|--------|-------|
| `writeHaskell` | Haskell | |
| `writeJava` | Java | Hoists polymorphic let bindings to class fields |
| `writePython` | Python | Hoists case statements to let bindings |
| `writeScala` | Scala 3 | |
| `writeClojure` | Clojure | |
| `writeScheme` | Scheme | |
| `writeCommonLisp` | Common Lisp | |
| `writeEmacsLisp` | Emacs Lisp | |

Schema-only (types, no terms):

| Function | Target |
|----------|--------|
| `writeCpp` | C++ headers |
| `writeGraphql` | GraphQL schema |
| `writeJsonSchema` | JSON Schema (2020-12) |
| `writeProtobuf` | Protocol Buffers (Proto3) |
| `writePdl` | Pegasus Data Language |
| `writeRust` | Rust |

### Output conventions

Generated files follow language-specific naming conventions:

| Language | Path pattern | Example for `hydra.core` |
|----------|-------------|--------------------------|
| Haskell | `Namespace/Module.hs` | `Hydra/Core.hs` |
| Java | `namespace/module/Module.java` | `hydra/core/Core.java` |
| Python | `namespace/module/module.py` | `hydra/core/core.py` |
| Scala | `namespace/module/Module.scala` | `hydra/core/Core.scala` |

## Path 1: generating from DSL modules

When you have direct access to DSL-defined modules (e.g., in a GHCi session, a Java program,
or a Python script), call the `writeXxx` functions directly.

### Haskell example

```bash
cd heads/haskell
stack ghci --ghci-options='+RTS -K256M -A32M -RTS'
```

```haskell
import Hydra.Ext.Generation

-- Generate Java for all main modules
writeJava "../../dist/java/hydra-kernel/src/main/java" mainModules mainModules

-- Generate Python for kernel only
writePython "../../dist/python/hydra-kernel/src/main/python" kernelModules kernelModules

-- Generate GraphQL schema for specific modules
writeGraphql "/tmp/graphql" mainModules [myCustomModule]
```

Module lists are Haskell values from `Hydra.Sources.All` and `Hydra.Ext.Sources.All`:

| Constant | Contents |
|----------|----------|
| `kernelModules` | Core kernel types and terms |
| `mainModules` | Kernel + standard libraries (encoders, decoders, DSLs) |
| `testModules` | Common test suite modules |
| `hydraExtModules` | All ext modules (coders, demos, domain models) |
| `hydraBootstrapCoderModules` | Coder modules needed for cross-language bootstrapping |

### Python example

```python
from hydra.generation import write_java, load_modules_from_json, read_manifest_field

# Load modules from JSON (see Path 2 below for details)
namespaces = read_manifest_field("path/to/json", "mainModules")
modules = load_modules_from_json(False, "path/to/json", namespaces)

# Generate Java
write_java("output/java", modules, modules)
```

The Python functions have the same arguments: `write_xxx(base_path, universe, mods)`.

Extra memory is typically needed for large module sets. The sync scripts handle this
automatically; for manual invocation, use `+RTS -K256M -A32M -RTS` (Haskell) or ensure
adequate stack size.

## Path 2: generating from JSON modules

When you don't have the DSL source (or prefer a language-independent workflow), load
modules from their JSON representation and generate from there. This is how the
bootstrapping demo works and how non-Haskell hosts generate code.

### JSON directory layout

JSON module files are exported to two locations:

| Directory | Contents |
|-----------|----------|
| `dist/json/hydra-kernel/src/main/json/` | Kernel and main modules |
| `dist/json/hydra-kernel/src/test/json/` | Test modules |
| `dist/json/hydra-misc/src/main/json/` | Ext modules (coders, demos, domain models) |

Each directory contains a `manifest.json` that lists available module groups:

**Kernel manifest** (`dist/json/hydra-kernel/src/main/json/manifest.json`):

| Field | Contents |
|-------|----------|
| `kernelModules` | Core kernel modules |
| `mainModules` | Kernel + standard libraries |
| `evalLibModules` | Higher-order primitive eval elements |
| `dslModules` | DSL generator output modules |
| `testModules` | Test suite modules |

**Ext manifest** (`dist/json/hydra-misc/src/main/json/manifest.json`):

| Field | Contents |
|-------|----------|
| `hydraBootstrapCoderModules` | Language coders needed for bootstrapping |
| `hydraExtEssentialModules` | Core ext modules (Java + Python coders) |
| `hydraExtModules` | All ext modules |
| `hydraExtDemoModules` | Demo-specific modules |

### Bootstrap CLI

The bootstrap CLI loads JSON modules and generates code:

```bash
# Generate Java from kernel modules
python -m hydra.bootstrap \
  --target java \
  --json-dir ../dist/json/hydra-kernel/src/main/json \
  --output /tmp/hydra-gen

# Generate Python, including ext coders
python -m hydra.bootstrap \
  --target python \
  --json-dir ../dist/json/hydra-kernel/src/main/json \
  --ext-json-dir ../dist/json/hydra-misc/src/main/json \
  --include-coders \
  --output /tmp/hydra-gen

# Generate Java, kernel modules only
python -m hydra.bootstrap \
  --target java \
  --json-dir ../dist/json/hydra-kernel/src/main/json \
  --kernel-only \
  --output /tmp/hydra-gen

# Generate Haskell, including tests
python -m hydra.bootstrap \
  --target haskell \
  --json-dir ../dist/json/hydra-kernel/src/main/json \
  --include-tests \
  --output /tmp/hydra-gen
```

**CLI options:**

| Option | Description |
|--------|-------------|
| `--target` | Target language: `haskell`, `java`, `python`, `scala`, `clojure`, `scheme`, `common-lisp`, `emacs-lisp` |
| `--json-dir` | Directory containing kernel/main JSON modules and manifest |
| `--output` | Output base directory (default: `/tmp/hydra-bootstrapping-demo`) |
| `--ext-json-dir` | Directory containing ext JSON modules (required with `--include-coders`) |
| `--include-coders` | Also generate ext coder modules |
| `--include-tests` | Also generate test modules |
| `--kernel-only` | Only generate kernel modules |
| `--types-only` | Only generate type-defining modules |

The Scala bootstrap has the same options: `sbt "runMain hydra.bootstrap --target java --json-dir ..."`.

### How the bootstrap CLI works

1. Reads `mainModules` and `evalLibModules` from the manifest in `--json-dir`
2. Loads those modules from JSON files using `load_modules_from_json`
3. If `--include-coders`, reads `hydraBootstrapCoderModules` from `--ext-json-dir` manifest and loads those too
4. Applies filters (`--kernel-only`, `--types-only`)
5. Calls `write_xxx` with all loaded modules as universe and the filtered set as modules to generate
6. If `--include-tests`, loads and generates test modules separately

## Using the sync scripts

For regenerating Hydra's own implementations, the sync scripts automate the full workflow.
These are the standard developer entry point:

```bash
# Full sync: Haskell -> Ext -> Java -> Python -> Scala -> Lisp
bin/sync-all.sh

# Individual phases
heads/haskell/bin/sync-haskell.sh      # Regenerate Haskell kernel
heads/haskell/bin/sync-ext.sh          # Regenerate ext modules and JSON exports
heads/haskell/bin/sync-java.sh         # Regenerate Java from JSON
heads/haskell/bin/sync-python.sh       # Regenerate Python from JSON
heads/haskell/bin/sync-scala.sh        # Regenerate Scala

# Quick mode (skip tests)
bin/sync-all.sh --quick
heads/haskell/bin/sync-java.sh --quick
```

The sync scripts handle memory flags, build ordering, and test execution automatically.

## Exporting DSL modules to JSON

To make DSL-defined modules available for JSON-based generation, export them:

```bash
cd heads/haskell
./bin/update-json-main.sh      # Export main modules
./bin/update-json-test.sh      # Export test modules
./bin/verify-json-kernel.sh    # Verify round-trip consistency
```

See [Exporting modules to JSON](json-kernel.md) for details on the JSON format and
verification process.

## Troubleshooting

### Stack overflow during generation

Large module sets require extra memory:

```bash
# Haskell
stack ghci --ghci-options='+RTS -K256M -A32M -RTS'

# The sync scripts handle this automatically
```

### Unresolved type references

The universe modules are missing a transitive dependency. Ensure the universe includes
all modules that the generated modules depend on, even indirectly.

### "file not found" for JSON modules

Check that `--json-dir` points to the correct directory. The path is relative to the
current working directory:

```bash
# From the repo root:
python -m hydra.bootstrap --json-dir dist/json/hydra-kernel/src/main/json ...

# From heads/haskell/:
python -m hydra.bootstrap --json-dir ../../dist/json/hydra-kernel/src/main/json ...
```

### --include-coders fails silently

`--include-coders` requires `--ext-json-dir` to know where ext JSON modules live.
Without it, coder modules won't be loaded.

### Generated files don't compile

Ensure the universe is complete. If generating ext modules, the universe must include
main modules as dependencies. If generating tests, the universe must include both main
and test dependencies.

## Related documentation

- [Exporting modules to JSON](json-kernel.md) -- JSON format, export scripts, verification
- [Synchronizing Hydra-Python](syncing-python.md) -- Python-specific sync workflow
- [Creating a new implementation](new-implementation.md) -- Using code generation for a new target language
