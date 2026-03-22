# Bootstrapping demo — everything-to-everything code generation

This demo demonstrates Hydra's self-hosting capability by regenerating all
generated code from a language-independent JSON representation. Each of Hydra's
five complete implementations (Haskell, Java, Python, Clojure, Scala) can independently load
Hydra modules from JSON and generate code for any target language, producing a
matrix of bootstrapping paths — all of which produce functionally equivalent
output that passes the common test suite.

## Overview

Each bootstrapping path proceeds as follows:

1. Load modules from language-independent JSON exports (with System F type
   annotations)
2. Generate code for a target language (adaptation, type inference, eta
   expansion, code printing)
3. Copy minimal static resources (primitive libraries, build files, test runners)
4. Build and test the generated project

By default, all output goes to `/tmp/hydra-bootstrapping-demo/` with subdirectories
(override with `--output`):

```
/tmp/hydra-bootstrapping-demo/
├── haskell-to-haskell/          # Haskell loads JSON, generates Haskell
├── haskell-to-java/             # Haskell loads JSON, generates Java
├── haskell-to-python/           # Haskell loads JSON, generates Python
├── java-to-haskell/             # Java loads JSON, generates Haskell
├── java-to-java/                # Java loads JSON, generates Java
├── java-to-python/              # Java loads JSON, generates Python
├── python-to-haskell/           # Python loads JSON, generates Haskell
├── python-to-java/              # Python loads JSON, generates Java
└── python-to-python/            # Python loads JSON, generates Python
```

## Directory structure

```
hydra-ext/
├── demos/bootstrapping/
│   ├── README.md                    # This file
│   ├── bin/
│   │   ├── bootstrap-all.sh         # Run all 9 bootstrapping paths
│   │   ├── haskell-to-haskell.sh    # Haskell -> JSON -> Haskell
│   │   ├── haskell-to-java.sh       # Haskell -> JSON -> Java
│   │   ├── haskell-to-python.sh     # Haskell -> JSON -> Python
│   │   └── ...                      # Other host-to-target scripts
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

This demo requires all three language environments to be set up. Unlike a
typical Hydra application (which uses only one language, or sometimes two in bridging scenarios), the bootstrapping
demo exercises all three implementations.

### Haskell

Install the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable):

```bash
# On macOS
brew install haskell-stack

# Other platforms: see https://docs.haskellstack.org/en/stable/install_and_upgrade/
```

Verify that both Haskell packages build:

```bash
cd hydra-haskell && stack build
cd hydra-ext && stack build
```

See the [Hydra-Haskell README](https://github.com/CategoricalData/hydra/blob/main/hydra-haskell/README.md)
for more details.

### Java

Install JDK 11 or later. Build with Gradle from the repository root:

```bash
./gradlew compileJava
```

You may need to set `JAVA_HOME` if your default Java version is too old:

```bash
JAVA_HOME=/path/to/java11 ./gradlew compileJava
```

See the [Hydra-Java README](https://github.com/CategoricalData/hydra/blob/main/hydra-java/README.md)
for more details.

### Python

Install Python 3.12 or later and [uv](https://github.com/astral-sh/uv):

```bash
curl -LsSf https://astral.sh/uv/install.sh | sh
```

Set up the Python environment:

```bash
cd hydra-python
uv venv --python 3.12
source .venv/bin/activate
uv sync
```

See the [Hydra-Python README](https://github.com/CategoricalData/hydra/blob/main/hydra-python/README.md)
for more details.

### Repository state

- The repository should be checked out at a stable revision (e.g., the
  latest `main` branch) and fully synced. All generated code and JSON
  exports should be up to date.

- If you have made local changes to Sources or other DSL code, run the
  top-level sync script before running the demo:
  ```bash
  ./bin/sync-all.sh    # from repo root; or --quick to skip tests
  ```

## Usage

### Run all 9 bootstrapping paths

```bash
cd hydra-ext
./demos/bootstrapping/bin/bootstrap-all.sh
```

This generates code for all host/target combinations, compares the output
against canonical baselines, and produces a summary matrix with timing and
file counts.

### Run a single path

```bash
cd hydra-ext

# Haskell host
./demos/bootstrapping/bin/haskell-to-java.sh

# Java host
./demos/bootstrapping/bin/java-bootstrap.sh --target java

# Python host
./demos/bootstrapping/bin/python-bootstrap.sh --target python
```

### Run the bootstrap executable directly

```bash
# Haskell
cd hydra-ext
stack exec bootstrap-from-json -- --target java +RTS -K256M -A32M -RTS

# Java
cd hydra-ext
./demos/bootstrapping/bin/java-bootstrap.sh --target haskell

# Python
cd hydra-python
PYTHONPATH=src/main/python:src/gen-main/python \
  python3 -m hydra.bootstrap \
  --target haskell --kernel-only --json-dir ../hydra-haskell/src/gen-main/json
```

### Command-line options

All three bootstrap executables accept the same options:

| Option | Description |
|--------|-------------|
| `--target <haskell\|java\|python>` | Target language (required) |
| `--output <dir>` | Output directory (default: `/tmp/hydra-bootstrapping-demo`) |
| `--include-coders` | Include extension coder modules |
| `--include-tests` | Include test modules |
| `--include-gentests` | Generate generation tests |
| `--kernel-only` | Exclude `hydra.ext.*` modules |
| `--types-only` | Only type-defining modules |
| `--json-dir <dir>` | Override kernel JSON directory |
| `--ext-json-dir <dir>` | Override extension JSON directory |

## Architecture

### I/O wrappers

Each language has a thin I/O wrapper that provides file I/O around the pure,
generated `hydra.codeGeneration` module:

| Language | I/O Wrapper | Bootstrap CLI |
|----------|-------------|---------------|
| Haskell  | `Hydra.Generation` (hydra-haskell) | `bootstrap-from-json` (hydra-ext) |
| Java     | `hydra.Generation` (hydra-java) | `hydra.Bootstrap` (hydra-java) |
| Python   | `hydra.generation` (hydra-python) | `hydra.bootstrap` (hydra-python) |

Each I/O wrapper provides:
- `bootstrapGraph()` — create an empty graph with standard primitives
- `parseJsonFile()` — read and parse a JSON file to a Hydra JSON Value
- `decodeModule()` — decode a JSON Value to a Module using type-directed decoding
- `loadModulesFromJson()` — discover and load all modules from a directory
- `writeHaskell()`, `writeJava()`, `writePython()` — generate and write code

### Kernel modules (type universe)

The JSON decoder needs a universe of type-defining modules to build a schema
map for type-directed decoding. Each language provides this differently:

- **Haskell**: Uses `kernelModules` — DSL Source modules computed from Haskell code
- **Python**: Uses `kernel_modules()` — generated Python Source modules
- **Java**: Uses native JSON decoding (bypasses schema-based decoding)

### How it works

#### Step 1: JSON modules (pre-exported)

The JSON modules are generated during synchronization and checked into the
repository. Three executables produce the JSON exports (run by the sync scripts):

- `hydra-haskell:update-json-main` — exports main + eval lib modules
- `hydra-haskell:update-json-test` — exports test modules
- `hydra-ext:update-json-ext` — exports extension modules

The JSON includes System F type annotations.

#### Step 2: Load from JSON

Each host language uses its I/O wrapper to:

1. Discover all `.json` files in the export directories
2. Parse each JSON file
3. Decode JSON to Hydra Terms using type-directed decoding
4. Decode Terms to Modules

#### Step 3: Generate code

The loaded modules are passed to the target language's code generation function,
which performs adaptation (type inference, eta expansion, case hoisting, etc.)
and writes the generated source files.

#### Step 4: Copy static resources and test

Each bootstrapping path copies a minimal set of static resources from the
Hydra source tree:

- **Primitive library implementations**: Hand-written native functions that
  cannot be bootstrapped (e.g., `Hydra.Lib.Lists`, `Hydra.Lib.Maps`)
- **Build configuration**: Language-specific build files (stack.yaml,
  build.gradle, pyproject.toml)
- **Test infrastructure**: Test runners and base classes

The project is then built and tested to verify that the generated code is
functionally equivalent to the canonical baseline.

## Module coverage

The bootstrapping demo generates the following module sets for each target:

- **Main modules** (129): kernel types, kernel terms, JSON, Haskell, eval lib
- **Extension modules** (74): Java, Python, Rust, Go, C++, GraphQL,
  Protobuf, and more
- **Test modules** (46): checking, inference, lib, serialization, etc.
- **Generation tests**: language-specific test suites

This covers the full Hydra codebase, including the Java and Python coders
themselves.
