# Hydra-Python

This directory contains a Python implementation of Hydra, still in progress.

## Getting Started

Install [uv](https://github.com/astral-sh/uv).
```bash
curl -LsSf https://astral.sh/uv/install.sh | sh
```

Make the python virtual environment.
```bash
uv venv --python 3.12
source .venv/bin/activate
```

Install the dependencies.
```bash
uv sync
```

Install [Ruff](https://github.com/astral-sh/ruff),
[pyright](https://github.com/microsoft/pyright), and
[pytest](https://docs.pytest.org/en/stable), e.g. on macOS.
```bash
brew install ruff
brew install pyright
brew install pytest
```

## Formatting, Linting, and Type Checking

All of these commands can run from the `hydra-python` root directory, but files/directories can be specified as arguments
as well to get more specific results.

#### Formatting

Format the hand-written Python code.
```bash
ruff format
```

#### Linting

Run the linter.
```bash
ruff check
```
Fix the linting errors in the code if they are fixable (e.g. removing unused imports).
```bash
ruff check --fix
```

#### Static Type Checking

Run the type checker.
```bash
pyright
```

## Testing

Hydra-Python has two types of tests: the **common test suite** (shared across all Hydra implementations) and **Python-specific tests**. See the [Testing wiki page](https://github.com/CategoricalData/hydra/wiki/Testing) for comprehensive documentation.

### Common Test Suite

The common test suite (`hydra.test.testSuite`) ensures parity across all Hydra implementations. **Passing all common test suite cases is the criterion for a true Hydra implementation.**

To run only the common test suite:
```bash
pytest src/test/python/test_suite_runner.py
```

The test suite is generated from Hydra DSL sources and includes:
- Primitive function tests (lists, strings, etc.)
- Case conversion tests (camelCase, snake_case, etc.)
- Type inference tests
- Evaluation tests (currently skipped - integration in progress)

### Python-Specific Tests

Python-specific tests validate implementation details and Python-specific functionality:

To run all tests (common suite + Python-specific):
```bash
pytest
```

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

## Generate Python code

The Python code in `src/gen-main/python` and `src/gen-test/python` are generated from sources in Hydra's bootstrapping implementation, Hydra-Haskell.
See the [Hydra-Haskell README](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell)
for more information on how this works.
You can generate Hydra-Python kernel and test code by first entering GHCi

```bash
cd ../hydra-ext && stack ghci
```

And then running the following commands in the GHC REPL.

```haskell
-- Generate the kernel
-- First arg: output directory
-- Second arg: universe modules (for dependency resolution)
-- Third arg: modules to generate
writePython "../hydra-python/src/main/python" kernelModules kernelModules

-- Generate the test suite
let allModules = mainModules ++ testModules
writePython "../hydra-python/src/gen-test/python" allModules baseTestModules
```

This will generate the entire Hydra kernel, as well as the common test suite.
Support for generating the rest of the kernel code is currently in progress.

### Validate generated code

The generated Hydra Kernel code is in `src/gen-main/python` and `src/gen-test/python`.
From the `hydra-python` directory, you can validate this code with a command like:

```bash
find src/gen-main/ -name "*.py" -exec python3 -m py_compile {} +
find src/gen-test/ -name "*.py" -exec python3 -m py_compile {} +
```
