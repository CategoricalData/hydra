# Hydra-Python

This package contains the **Python coder DSL sources**: Haskell modules that describe
how to translate Hydra modules into Python source code. The runnable Python head
(hand-written primitives, DSL runtime, pyproject.toml, test runner) lives in
[`heads/python/`](https://github.com/CategoricalData/hydra/tree/main/heads/python).
The generated Python kernel lives in
[`dist/python/hydra-kernel/`](https://github.com/CategoricalData/hydra/tree/main/dist/python/hydra-kernel).

Hydra is a type-aware data transformation toolkit which aims to be highly flexible and portable.
It has its roots in graph databases and type theory, and provides APIs in Haskell, Java, Python, Scala, and Lisp.
See the main Hydra [README](https://github.com/CategoricalData/hydra) for more details.

## Getting started

Hydra-Python requires Python 3.12 or later.

Install [uv](https://github.com/astral-sh/uv):

```bash
curl -LsSf https://astral.sh/uv/install.sh | sh
```

Create the Python virtual environment in the Python head directory:

```bash
cd heads/python
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
- **[Implementation](https://github.com/CategoricalData/hydra/blob/main/docs/implementation.md)** - Implementation guide
- **[Code Organization](https://github.com/CategoricalData/hydra/wiki/Code-organization)** -
  The `packages/`, `heads/`, `dist/` layout
- **[Testing](https://github.com/CategoricalData/hydra/wiki/Testing)** -
  Common test suite documentation
- **[Developer Recipes](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/index.md)** -
  Step-by-step guides
- **[Syncing Hydra-Python](https://github.com/CategoricalData/hydra/blob/main/docs/recipes/syncing-python.md)** -
  Regenerating Python from Haskell

## Testing

Hydra-Python has two types of tests: the **common test suite** (shared across all Hydra implementations)
and **Python-specific tests**.
See the [Testing wiki page](https://github.com/CategoricalData/hydra/wiki/Testing)
for comprehensive documentation.

### Common test suite

The common test suite (`hydra.test.testSuite`) ensures parity across all Hydra implementations.
**Passing all common test suite cases is the criterion for a true Hydra implementation.**

To run all tests (from `heads/python/`):

```bash
cd heads/python && pytest
```

To run only the common test suite:

```bash
cd heads/python && pytest src/test/python/test_suite_runner.py
```

The test suite is generated from Hydra DSL sources and includes:
- Primitive function tests (lists, strings, math, etc.)
- Case conversion tests (camelCase, snake_case, etc.)
- Type inference tests
- Type checking tests
- Evaluation tests
- JSON coder tests
- Rewriting and hoisting tests

### Python-specific tests

Python-specific tests validate implementation details and Python-specific functionality.
These are located in `heads/python/src/test/python/` alongside the common test suite runner.

To run a specific test file:

```bash
cd heads/python && pytest src/test/python/test_grammar.py
```

To match a specific test by name:

```bash
cd heads/python && pytest -k test_grammar
```

To see printed outputs, use the `-s` flag:

```bash
cd heads/python && pytest -s
```

## Code organization

In 0.15, Hydra's Python code is split across three locations
(see [Code organization wiki page](https://github.com/CategoricalData/hydra/wiki/Code-organization) for the full picture):

- **This package** (`packages/hydra-python/src/main/haskell/`) — the Python coder DSL sources
  (written in Haskell): `Hydra/Sources/Python/` contains `Syntax`, `Language`, `Coder`, `Serde`,
  `Names`, `Utils`, `Environment`, and `Testing` modules.

- **Python head** ([`heads/python/src/main/python/`](https://github.com/CategoricalData/hydra/tree/main/heads/python/src/main/python))
  — hand-written Python runtime
  - `hydra/lib/` — primitive function implementations
  - `hydra/dsl/` — DSL utilities (FrozenDict, Maybe, ...)
  - `hydra/sources/libraries.py` — primitive registration
  - `pyproject.toml` lives in `heads/python/`

- **Generated Python kernel** ([`dist/python/hydra-kernel/src/main/python/`](https://github.com/CategoricalData/hydra/tree/main/dist/python/hydra-kernel/src/main/python))
  - `hydra/core.py` — core types (Term, Type, Literal, ...)
  - `hydra/graph.py`, `hydra/packaging.py` — graph and packaging structures
  - `hydra/coders.py` — type adapters and coder framework
  - `hydra/reduction.py`, `hydra/rewriting.py`, `hydra/hoisting.py` — term transformations
  - `hydra/inference.py`, `hydra/checking.py` — type inference and checking
  - Generated from the kernel DSL sources using the Python coder

- **Generated Python test suite** (`dist/python/hydra-kernel/src/test/python/`)
  - Common tests ensuring parity with Haskell, Java, Scala, and Lisp

## Generate Python code

Python code is generated from the Haskell head. See the
[Hydra-Haskell README](https://github.com/CategoricalData/hydra/tree/main/packages/hydra-haskell)
for background on code generation.

The recommended way to regenerate all Python code is the sync script (from the repo root):

```bash
heads/haskell/bin/sync-python.sh
```

This will:
1. Generate the kernel modules into `dist/python/hydra-kernel/src/main/python`
2. Generate the kernel tests into `dist/python/hydra-kernel/src/test/python`
3. Run the pytest suite

For manual generation, enter GHCi from `heads/haskell/`:

```bash
cd heads/haskell && stack ghci
```

And run in the REPL:

```haskell
import Hydra.Generation
import Hydra.Sources.All

-- Generate the kernel
writePython "../../dist/python/hydra-kernel/src/main/python" kernelModules kernelModules

-- Generate the test suite
let allModules = mainModules ++ testModules
writePython "../../dist/python/hydra-kernel/src/test/python" allModules baseTestModules
```

### Validate generated code

```bash
find dist/python/hydra-kernel/src -name "*.py" -exec python3 -m py_compile {} +
```

## Formatting, linting, and type checking

Install [Ruff](https://github.com/astral-sh/ruff),
[pyright](https://github.com/microsoft/pyright), and
[pytest](https://docs.pytest.org/en/stable), e.g. on macOS:

```bash
brew install ruff
brew install pyright
brew install pytest
```

All of these commands run from the `heads/python/` directory
(files/directories can also be specified as arguments).

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

### Static type checking

Run the type checker:

```bash
pyright
```
