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
  - `hydra/python/util/` — `ConsList`, `Lazy`, `PersistentMap`, `PersistentSet`
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
bin/sync-python.sh
```

(equivalent to `bin/sync.sh --hosts python --targets python`)

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

## Numeric types

Hydra's `decimal` type is implemented as Python `decimal.Decimal` with the
default 28-digit context precision.
Operations exceeding this precision round per the active context;
users requiring higher precision should adjust `decimal.getcontext().prec`
before performing arithmetic.
This differs from Haskell `Scientific` and Java `BigDecimal`
(which are effectively unbounded for exact operations)
but matches Python's standard decimal behavior.

## Collections

Hydra-Python uses an API/implementation split for list, map, and set values,
mirroring [Hydra-Java](../hydra-java/README.md):

- **At the type level**, generated Python uses the standard
  [`collections.abc`](https://docs.python.org/3/library/collections.abc.html)
  abstract base classes — `Sequence[E]` for lists, `Mapping[K, V]` for maps,
  and `Set[E]` for sets. Public function signatures stay generic and
  dependency-free; callers can pass any compatible collection.
- **At the implementation level**, generated term-level literals construct
  immutable collection classes from
  [`hydra.python.util`](../../heads/python/src/main/python/hydra/python/util/):
  `ConsList` (a frozen sequence), `PersistentMap` (a frozen map), and
  `PersistentSet` (a frozen set). Each implements the corresponding
  `collections.abc` ABC, so `ConsList` IS a `Sequence`, `PersistentMap` IS
  a `Mapping`, and `PersistentSet` IS a `Set`.

These classes are thin facades over native `tuple`, `dict`, and `frozenset`.
All mutations build a fresh native container via `{**self, **other}`,
`tuple(...)`, or `frozenset(...)` and freeze it under the immutable wrapper.
The cost is full O(n) copy on every update (no structural sharing); the
benefit is C-speed inner loops. Hydra-Python has **no third-party runtime
dependencies** beyond the standard library.

Where ordered iteration matters (`hydra.lib.maps.{keys, elems, to_list}`, the
various `*_list()` extraction helpers, `PersistentSet.__iter__`), elements
are sorted at extraction time via a fall-through comparator: natural `<`
where it works, structural comparison for Hydra `Term`/`Type` and other
complex values that don't define ordering in Python.

The classes live under `hydra.python.util` rather than `hydra.util` because the
latter is already a kernel-generated module (containing `Comparison`,
`CaseConvention`, etc.) shared across all Hydra implementations. Putting the
Python-runtime helpers under `hydra.python.util` keeps the kernel namespace
intact while making the host/kernel separation explicit. `hydra.python` itself
is a `pkgutil`-style namespace package so heads-side helpers and the
kernel-generated `hydra.python.{coder,environment,...}` modules coexist
cleanly.

## CPython vs PyPy

The bootstrap demo and `bin/run-bootstrapping-demo.sh` prefer `pypy3` when
available and fall back to CPython 3.12+. Both interpreters pass the full
test suite. For most real workloads, **PyPy is the better choice** — its JIT
makes term-level transformation (the dominant Python-host cost) several times
faster than CPython.

Rough guide:

| Workload | Faster on |
|---|---|
| `bin/run-bootstrapping-demo.sh` codegen | CPython by ~5% |
| Type inference on large modules (`hydra.codegen.infer_modules_given`) | PyPy by ~4× |
| `hydra.lib.*` primitive microbenchmarks | CPython by ~2.5× |

The microbench gap reflects CPython's C-level `dict`/`frozenset`/`tuple`
operations beating PyPy's pure-Python equivalents. The inference gap reflects
PyPy's JIT amortizing per-call dispatch overhead in long-running term
walks. For day-to-day development you can pick whichever is convenient
(CPython is usually already installed); PyPy becomes worthwhile when you
hit term-level workloads measured in seconds or minutes.

Set `HYDRA_PYTHON_INTERPRETER=pypy3` (or any path/name) in the environment
to force a specific interpreter for the bootstrap demo.
