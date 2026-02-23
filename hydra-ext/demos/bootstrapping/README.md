# Bootstrapping demo - everything-to-everything code generation

This demo demonstrates Hydra's self-hosting capability by regenerating all
generated code from a language-independent JSON representation.

See [GitHub issue #254](https://github.com/CategoricalData/hydra/issues/254).

## Overview

Hydra has three complete implementations (Haskell, Java, Python). Each can
independently load Hydra modules from a language-independent JSON
representation and generate code for any target language. This demo validates
each bootstrapping path by:

1. Exporting all modules (kernel + extensions + eval lib + tests) to JSON with
   System F type annotations (per issue #253)
2. Loading those JSON modules back into Hydra (from any host language)
3. Generating code + tests for a target language
4. Copying minimal static resources (primitives, build files, test runners)

All output goes to `/tmp/hydra-bootstrapping-demo/` with subdirectories:

```
/tmp/hydra-bootstrapping-demo/
├── haskell-to-haskell/          # Haskell loads JSON, generates Haskell
├── haskell-to-java/             # Haskell loads JSON, generates Java
├── haskell-to-python/           # Haskell loads JSON, generates Python
├── java-to-java/                # Java loads JSON, generates Java
├── java-to-python/              # Java loads JSON, generates Python
├── java-to-haskell/             # Java loads JSON, generates Haskell
├── python-to-java/              # Python loads JSON, generates Java
├── python-to-python/            # Python loads JSON, generates Python
└── python-to-haskell/           # Python loads JSON, generates Haskell
```

This provides a clear view of which files are generated vs. static, and
demonstrates what is actually required of a Hydra implementation.

## Directory structure

```
hydra-ext/
├── demos/bootstrapping/
│   ├── README.md                    # This file
│   ├── bootstrap-all.sh             # Run all Haskell bootstrapping paths
│   ├── bootstrap-to-haskell.sh      # Haskell -> JSON -> Haskell
│   ├── bootstrap-to-java.sh         # Haskell -> JSON -> Java
│   ├── bootstrap-to-python.sh       # Haskell -> JSON -> Python
│   ├── java-bootstrap.sh            # Java -> JSON -> target
│   └── python-bootstrap.sh          # Python -> JSON -> target
├── src/exec/bootstrap-from-json/
│   └── Main.hs                      # Haskell bootstrap executable
hydra-java/
└── src/main/java/hydra/
    ├── Generation.java              # Java I/O wrapper
    └── Bootstrap.java               # Java bootstrap CLI
hydra-python/
└── src/main/python/hydra/
    ├── generation.py                # Python I/O wrapper
    └── bootstrap.py                 # Python bootstrap CLI
```

## Prerequisites

- The repository is checked out at a stable revision (e.g., the latest
  `main` branch) and is fully synced. All generated code (Haskell, Java,
  Python gen-main directories) and JSON exports should be up to date.

- If you have made local changes to Sources or other DSL code, run the
  top-level sync script before running the demo:
  ```bash
  ./bin/sync-all.sh    # from repo root; or --quick to skip tests
  ```
  This runs all sync scripts in the correct order (Haskell, Ext, Java, Python).

- All three language environments are set up:
  - Haskell: `stack build` works in hydra-haskell and hydra-ext
  - Java: `./gradlew compileJava` works in hydra-java
  - Python: Python 3.10+ (match/case syntax required). PyPy3 strongly
    recommended for term-level generation

## Usage

### Haskell bootstrapping (full pipeline)

Run a single bootstrapping path:

```bash
cd hydra-ext
./demos/bootstrapping/bootstrap-to-java.sh
```

Run all Haskell bootstrapping paths:

```bash
cd hydra-ext
./demos/bootstrapping/bootstrap-all.sh
```

Or run the bootstrap executable directly (JSON must be up-to-date):

```bash
cd hydra-ext
stack exec bootstrap-from-json -- --target java +RTS -K256M -A32M -RTS
```

### Java bootstrapping

```bash
cd hydra-ext
./demos/bootstrapping/java-bootstrap.sh --target java
```

### Python bootstrapping

```bash
cd hydra-ext
./demos/bootstrapping/python-bootstrap.sh --target python
```

Or run the Python bootstrap module directly (PyPy3 recommended for term-level):

```bash
cd hydra-python
PYTHONPATH=src/main/python:src/gen-main/python \
  pypy3 -m hydra.bootstrap \
  --target haskell --kernel-only --json-dir ../hydra-haskell/src/gen-main/json
```

## Architecture

### I/O wrappers

Each language has a thin I/O wrapper (the equivalent of Haskell's
`Hydra.Generation` module) that provides file I/O around the pure/Flow-based
functions in the generated `hydra.codeGeneration` module:

| Language | I/O Wrapper | Bootstrap CLI |
|----------|-------------|---------------|
| Haskell  | `Hydra.Generation` (hydra-haskell) | `bootstrap-from-json` (hydra-ext) |
| Java     | `hydra.Generation` (hydra-java) | `hydra.Bootstrap` (hydra-java) |
| Python   | `hydra.generation` (hydra-python) | `hydra.bootstrap` (hydra-python) |

Each I/O wrapper provides:
- `bootstrapGraph()` -- create an empty graph with standard primitives
- `parseJsonFile()` -- read and parse a JSON file to a Hydra Value
- `decodeModule()` -- decode a Hydra Value to a Module
- `loadAllModulesFromJsonDir()` -- discover and load all modules from a directory
- `generateSources()` -- run code generation and write files to disk
- `writeJava()`, `writePython()`, `writeHaskell()` -- language-specific convenience methods

### Kernel modules (type universe)

The JSON decoder (`decodeModuleFromJson`) needs a universe of modules containing
type definitions to build a schema map for type-directed JSON decoding. Each
language provides this differently:

- **Haskell**: Uses `kernelModules` -- DSL Source modules computed from Haskell code
- **Python**: Uses `kernel_modules()` -- generated Python Source modules with `module()` functions
- **Java**: Uses native JSON decoding (bypasses schema-based decoding)

### How it works

#### Step 1: JSON modules (pre-exported)

The JSON modules are generated during synchronization and checked into the
repository. The bootstrapping scripts assume they are already up to date.

Three executables produce the JSON exports (run by the sync scripts):

- `hydra-haskell:update-json-main` -- exports `mainModules` + `evalLibModules`
  to `hydra-haskell/src/gen-main/json/`
- `hydra-haskell:update-json-test` -- exports `testModules` to
  `hydra-haskell/src/gen-test/json/`
- `hydra-ext:update-json-ext` -- exports `hydraExtModules` to
  `hydra-ext/src/gen-main/json/`

The JSON includes System F type annotations.

#### Step 2: Load from JSON

Each host language uses its I/O wrapper to:

1. Discover all `.json` files in the export directories
2. Parse each JSON file (Haskell: Aeson; Java: json-io; Python: built-in json)
3. Decode JSON -> Term using Hydra's type-directed JSON decoder
4. Decode Term -> Module using Hydra's module decoder
5. Strip TypeSchemes from term bindings (to avoid stale type annotations after
   adaptation -- see "Known limitations" below)

#### Step 3: Generate code

The loaded modules are passed to the appropriate code generation function
(`writeJava`, `writePython`, or `writeHaskell`), which performs adaptation
(type inference, eta expansion, case hoisting, etc.) and code printing.

#### Step 4: Copy static resources

Each bootstrapping path copies a minimal set of static resources from the
Hydra source tree:

- **Primitive library implementations**: Hand-written native functions that
  cannot be bootstrapped (e.g., `Hydra.Lib.Lists`, `Hydra.Lib.Maps`)
- **Build configuration**: Language-specific build files (stack.yaml,
  build.gradle, pyproject.toml)
- **Test infrastructure**: Test runners and base classes

## Known limitations

### TypeScheme stripping

JSON-loaded modules have their TypeSchemes stripped before code generation.
This is because JSON modules carry type annotations from the original
compilation, which may contain types (like `bigfloat`) that become stale
after adaptation to a target language (e.g., `bigfloat` -> `float64`).

Stripping TypeSchemes means that type-class constraints (like `Ord`) that
were originally derived from TypeSchemes cannot be propagated. This may
cause minor differences in the generated code compared to the kernel-
regenerated code (e.g., missing `Ord` constraints on some Haskell functions).

A future improvement would be to *adapt* TypeSchemes (converting their types
alongside the term adaptation) rather than stripping them, preserving
constraints. The user should be able to tell Hydra via the API whether to
expect typed or untyped terms.

### Two-pass inference

When TypeSchemes are stripped, two inference passes are needed: one before
eta expansion (if applicable) and one after adaptation. This is less
efficient than the single-pass approach used when TypeSchemes are available.

### Performance

Flow evaluation for term-level generation (adaptation + type inference +
code generation) for 120 kernel modules takes multiple hours in both Java
and Python. The bottleneck is `dataGraphToDefinitions` which builds a schema
graph and performs type inference over all ~1630 bindings. Observed runtimes:

- Java: 3+ hours at 100% CPU, ~2GB RSS
- Python (PyPy3): 2+ hours at 100% CPU, ~900MB RSS

PyPy3 is strongly recommended for the Python host. CPython 3.12 is too slow
for term-level generation.

### Python inline match expression placeholders

The Python code generator emits `unsupported("inline match expressions")`
placeholders where it cannot express Haskell `case` expressions inline
within a lambda or walrus tuple. For the Haskell coder path, 8 such
placeholders have been manually fixed. The Java and Python coder paths
(`ext/java/coder.py`, `ext/python/coder.py`) still have ~20+ unresolved
placeholders that need manual fixes before Python-to-Java and
Python-to-Python term-level generation can complete.

### Generated Source module limitations

Two Python Source modules (`hydra.sources.decoding`, `hydra.sources.hoisting`)
exceed Python's parser limit for nested parentheses and cannot be imported.
They are skipped when building the kernel module universe; this does not
affect module decoding since they are term modules (not type modules).

## Module coverage

The bootstrapping demo generates the following module sets for each target:

- **Main modules** (129): kernel types, kernel terms, JSON, Haskell, eval lib
- **Extension modules** (74): Java, Python, Rust, Go, C++, C#, GraphQL,
  Protobuf, and many more
- **Test modules** (46): checking, inference, lib, serialization, etc.
- **Generation tests**: language-specific test suites

This covers the full Hydra codebase, including the Java and Python coders
themselves.
