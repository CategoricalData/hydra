# Bootstrapping — cross-implementation code generation and verification

Hydra's bootstrapping system validates that all implementations can independently
regenerate each other from a language-independent JSON representation.
Each of Hydra's seven bootstrapping hosts (Haskell, Java, Python, Scala,
Clojure, Common Lisp, and Scheme) can load Hydra modules from JSON and generate code
for any target language, producing a matrix of generation paths — all of which produce
functionally equivalent output that passes the common test suite.

This is a core part of Hydra's build and verification system, not just a demo.
It ensures that changes to one implementation do not silently break cross-language
compatibility.

## Overview

Each bootstrapping path proceeds as follows:

1. Load modules from language-independent JSON exports (with System F type
   annotations)
2. Generate code for a target language (adaptation, type inference, eta
   expansion, code printing)
3. Copy minimal static resources (primitive libraries, build files, test runners)
4. Build and test the generated project

By default, the bootstrapping system runs the 3×3 matrix of Haskell, Java, and Python
as both hosts and targets. Additional hosts and targets (Scala, Clojure, Common Lisp,
Emacs Lisp, Scheme) can be included with `--hosts` and `--targets` flags.
All output goes to `/tmp/hydra-bootstrapping-demo/` (override with `--output`):

```
/tmp/hydra-bootstrapping-demo/
├── haskell-to-haskell/          # Haskell loads JSON, generates Haskell
├── haskell-to-java/             # Haskell loads JSON, generates Java
├── haskell-to-python/           # Haskell loads JSON, generates Python
├── java-to-haskell/             # Java loads JSON, generates Haskell
├── java-to-java/                # Java loads JSON, generates Java
├── java-to-python/              # Java loads JSON, generates Python
├── scala-to-python/             # Scala loads JSON, generates Python
├── python-to-haskell/           # Python loads JSON, generates Haskell
├── python-to-java/              # Python loads JSON, generates Java
├── python-to-scala/             # Python loads JSON, generates Scala
└── python-to-python/            # Python loads JSON, generates Python
```

## Directory structure

```
demos/
├── bootstrapping/
│   ├── README.md                    # This file
│   ├── bin/
│   │   ├── bootstrap-all.sh         # Run all bootstrapping paths
│   │   ├── haskell-to-haskell.sh    # Haskell -> JSON -> Haskell
│   │   ├── haskell-to-java.sh       # Haskell -> JSON -> Java
│   │   ├── haskell-to-python.sh     # Haskell -> JSON -> Python
│   │   └── ...                      # Other host-to-target scripts
heads/haskell/
├── src/exec/bootstrap-from-json/
│   └── Main.hs                      # Haskell bootstrap executable
heads/java/
└── src/main/java/hydra/
    ├── Generation.java              # Java I/O wrapper
    └── Bootstrap.java               # Java bootstrap CLI
heads/scala/
└── src/main/scala/hydra/
    ├── Generation.scala             # Scala I/O wrapper
    └── Bootstrap.scala              # Scala bootstrap CLI
heads/python/
└── src/main/python/hydra/
    ├── generation.py                # Python I/O wrapper
    └── bootstrap.py                 # Python bootstrap CLI
```

## Prerequisites

Running the full bootstrapping matrix requires all language environments to be set up.
The default 3×3 matrix needs Haskell, Java, and Python.
Additional hosts require their respective toolchains
(sbt for Scala, Clojure CLI, SBCL for Common Lisp, Emacs for Emacs Lisp, Guile for Scheme).

### Haskell

Install the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable):

```bash
# On macOS
brew install haskell-stack

# Other platforms: see https://docs.haskellstack.org/en/stable/install_and_upgrade/
```

Verify that both Haskell packages build:

```bash
cd heads/haskell && stack build
```

See the [Hydra-Haskell README](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-haskell/README.md)
for more details.

### Java

Install JDK 11 or later. Build with Gradle from the repository root:

```bash
./gradlew :hydra-java:compileJava
```

You may need to set `JAVA_HOME` if your default Java version is too old:

```bash
JAVA_HOME=/path/to/java11 ./gradlew :hydra-java:compileJava
```

See the [Hydra-Java README](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-java/README.md)
for more details.

### Python

Install Python 3.12 or later and [uv](https://github.com/astral-sh/uv):

```bash
curl -LsSf https://astral.sh/uv/install.sh | sh
```

Set up the Python environment:

```bash
cd heads/python
uv venv --python 3.12
source .venv/bin/activate
uv sync
```

See the [Hydra-Python README](https://github.com/CategoricalData/hydra/blob/main/packages/hydra-python/README.md)
for more details.

### Repository state

- The repository should be checked out at a stable revision (e.g., the
  latest `main` branch) and fully synced. All generated code and JSON
  exports should be up to date.

- If you have made local changes to Sources or other DSL code, run the
  top-level sync script before running the bootstrapping verification:
  ```bash
  ./bin/sync-all.sh    # from repo root; or --quick to skip tests
  ```

## Usage

### Run all 9 bootstrapping paths

```bash
./demos/bootstrapping/bin/bootstrap-all.sh
```

This generates code for all host/target combinations, compares the output
against canonical baselines, and produces a summary matrix with timing and
file counts.

### Run a single path

```bash
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
cd heads/haskell
stack exec bootstrap-from-json -- --target java +RTS -K256M -A32M -RTS

# Java
./demos/bootstrapping/bin/java-bootstrap.sh --target haskell

# Python
cd heads/python
PYTHONPATH=src/main/python:../../dist/python/hydra-kernel/src/main/python \
  python3 -m hydra.bootstrap \
  --target haskell --kernel-only --json-dir ../../dist/json/hydra-kernel/src/main/json
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
| `--kernel-only` | Exclude `hydra.*` modules |
| `--types-only` | Only type-defining modules |
| `--json-dir <dir>` | Override kernel JSON directory |
| `--ext-json-dir <dir>` | Override extension JSON directory |

## Architecture

### I/O wrappers

Each language has a thin I/O wrapper that provides file I/O around the pure,
generated `hydra.codeGeneration` module:

| Language | I/O Wrapper | Bootstrap CLI |
|----------|-------------|---------------|
| Haskell  | `Hydra.Generation` (hydra-haskell) | `bootstrap-from-json` (heads/haskell) |
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
- `hydra-haskell:update-json-main` also exports per-package extension modules
  (coders, PG, RDF) to `dist/json/hydra-<pkg>/`

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

The bootstrapping system generates the following module sets for each target:

- **Main modules** (129): kernel types, kernel terms, JSON, Haskell, eval lib
- **Extension modules** (74): Java, Python, Rust, Go, C++, GraphQL,
  Protobuf, and more
- **Test modules** (46): checking, inference, lib, serialization, etc.
- **Generation tests**: language-specific test suites

This covers the full Hydra codebase, including the Java and Python coders
themselves.

## Known limitations

### Lisp test timing in the dashboard

The dashboard's `test:` column shows near-zero times for Lisp targets (Clojure,
Common Lisp, Scheme).
This is a measurement artifact, not an indication that tests didn't run.

Lisp dialects use eager top-level `def`/`define` forms, so the `reduceTerm`
interpreter invocations happen during module loading, before the test runner's
timer starts.
Java and Python defer evaluation via `allTests()` / `@lru_cache`, so their
timers capture the interpreter work.

All Lisp test assertions do execute and verify correct results — the interpreter
work is simply attributed to load time rather than test time.
