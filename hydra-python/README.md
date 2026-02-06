# Hydra-Python

This directory contains a Python implementation of Hydra.

Hydra is a type-aware data transformation toolkit which aims to be highly flexible and portable.
It has its roots in graph databases and type theory, and provides APIs in Haskell, Java, and Python.
See the main Hydra [README](https://github.com/CategoricalData/hydra) for more details.

## Getting Started

Hydra-Python requires Python 3.12 or later.

Install [uv](https://github.com/astral-sh/uv):

```bash
curl -LsSf https://astral.sh/uv/install.sh | sh
```

Create the Python virtual environment:

```bash
uv venv --python 3.12
source .venv/bin/activate
```

Install the dependencies:

```bash
uv sync
```

## Documentation

For comprehensive documentation about Hydra's architecture and usage, see:

- **[Concepts](https://github.com/CategoricalData/hydra/wiki/Concepts)** - Core concepts and type system
- **[Implementation](https://github.com/CategoricalData/hydra/wiki/Implementation)** - Implementation guide
- **[Code Organization](https://github.com/CategoricalData/hydra/wiki/Code-organization)** - The src/main vs src/gen-main pattern
- **[Testing](https://github.com/CategoricalData/hydra/wiki/Testing)** - Common test suite documentation
- **[Developer Recipes](https://github.com/CategoricalData/hydra/blob/main/docs/src/recipes/index.md)** - Step-by-step guides
- **[Syncing Hydra-Python](https://github.com/CategoricalData/hydra/blob/main/docs/src/recipes/syncing-python.md)** - Regenerating Python from Haskell

## Testing

Hydra-Python has two types of tests: the **common test suite** (shared across all Hydra implementations) and **Python-specific tests**. See the [Testing wiki page](https://github.com/CategoricalData/hydra/wiki/Testing) for comprehensive documentation.

### Common Test Suite

The common test suite (`hydra.test.testSuite`) ensures parity across all Hydra implementations. **Passing all common test suite cases is the criterion for a true Hydra implementation.**

To run all tests:

```bash
pytest
```

To run only the common test suite:

```bash
pytest src/test/python/test_suite_runner.py
```

The test suite is generated from Hydra DSL sources and includes:
- Primitive function tests (lists, strings, math, etc.)
- Case conversion tests (camelCase, snake_case, etc.)
- Type inference tests
- Type checking tests
- Evaluation tests
- JSON coder tests
- Rewriting and hoisting tests

### Python-Specific Tests

Python-specific tests validate implementation details and Python-specific functionality. These are located in `src/test/python/` alongside the common test suite runner.

To run a specific test file:

```bash
pytest src/test/python/test_grammar.py
```

To match a specific test by name:

```bash
pytest -k test_grammar
```

To see printed outputs, use the `-s` flag:

```bash
pytest -s
```

To generate a categorized summary report:

```bash
python src/test/python/test_summary_report.py
```

## Code Organization

Hydra-Python uses the **src/main vs src/gen-main** separation pattern (see [Code organization wiki page](https://github.com/CategoricalData/hydra/wiki/Code-organization) for details).

- **`src/main/python/`** - Hand-written Python code
  - `hydra/lib/` - Primitive function implementations
  - `hydra/dsl/` - DSL utilities (FrozenDict, Maybe, etc.)
  - Language-specific parsers and extensions

- **`src/gen-main/python/`** - Generated Python code
  - `hydra/core.py` - Core types (Term, Type, Literal, etc.)
  - `hydra/graph.py`, `hydra/module.py` - Graph and module structures
  - `hydra/coders.py`, `hydra/compute.py` - Type adapters and computational abstractions
  - `hydra/reduction.py`, `hydra/rewriting.py`, `hydra/hoisting.py` - Term transformations
  - `hydra/inference.py`, `hydra/checking.py` - Type inference and checking
  - Generated from Haskell DSL sources using the Python coder in hydra-ext

- **`src/gen-test/python/`** - Generated test suite
  - `hydra/test/` - Common tests ensuring parity with Haskell and Java
  - `generation/` - Generation tests (terms generated to Python and executed)

## Generate Python Code

The Python code in `src/gen-main/python` and `src/gen-test/python` is generated from sources in Hydra's bootstrapping implementation, Hydra-Haskell.
See the [Hydra-Haskell README](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell) for more information on how this works.

The recommended way to regenerate all Python code is to use the sync script:

```bash
cd ../hydra-ext
./bin/sync-python.sh
```

This will:
1. Generate the kernel modules
2. Generate the kernel tests
3. Generate the generation tests
4. Run all tests

For manual generation, enter GHCi from hydra-ext:

```bash
cd ../hydra-ext && stack ghci
```

And run the following commands in the GHC REPL:

```haskell
-- Generate the kernel
writePython "../hydra-python/src/gen-main/python" kernelModules kernelModules

-- Generate the test suite
let allModules = mainModules ++ testModules
writePython "../hydra-python/src/gen-test/python" allModules baseTestModules
```

### Validate Generated Code

The generated Hydra kernel code is in `src/gen-main/python` and `src/gen-test/python`.
From the `hydra-python` directory, you can validate this code with:

```bash
find src/gen-main/ -name "*.py" -exec python3 -m py_compile {} +
find src/gen-test/ -name "*.py" -exec python3 -m py_compile {} +
```

## Formatting, Linting, and Type Checking

Install [Ruff](https://github.com/astral-sh/ruff),
[pyright](https://github.com/microsoft/pyright), and
[pytest](https://docs.pytest.org/en/stable), e.g. on macOS:

```bash
brew install ruff
brew install pyright
brew install pytest
```

All of these commands can run from the `hydra-python` root directory, but files/directories can be specified as arguments as well.

### Formatting

Format the hand-written Python code:

```bash
ruff format
```

### Linting

Run the linter:

```bash
ruff check
```

Fix fixable linting errors (e.g. removing unused imports):

```bash
ruff check --fix
```

### Static Type Checking

Run the type checker:

```bash
pyright
```
