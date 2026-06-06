"""Shared DSL helpers used across hydra.sources.python.* modules.

Mirrors Haskell's per-source-module idioms. These centralize patterns that
were previously duplicated as `_def`, `_local`, `_ap`, `_let_chain`,
`_lets_flat`, `_py_name` in each source file.

Conventions:
- All helpers prefixed with underscore are intended for use inside the
  source DSL modules (treated as internal API).
- Functions taking a `mod` argument operate on a module-local placeholder
  that the caller constructs once at module load.
"""

from hydra.dsl.meta.phantoms import *  # noqa: F401,F403
from hydra.dsl.python import None_
from hydra.core import Name
from hydra.packaging import ModuleDependency, ModuleName
import hydra.dsl.python.syntax as PySyn


def unqualified_dep(module: ModuleName) -> ModuleDependency:
    """Construct an unqualified ModuleDependency (no package qualifier)."""
    return ModuleDependency(module, None_())


# The kernel "types" module dependencies — every coder package's source module list
# depends on these so the inferencer can resolve every TypeDefinition that the
# DSL references. Centralized here so each source module doesn't re-declare
# the same 24-element list.
KERNEL_TYPES_NAMESPACES = [
    unqualified_dep(ModuleName(n)) for n in [
        "hydra.paths", "hydra.ast", "hydra.classes", "hydra.coders",
        "hydra.core", "hydra.error.checking", "hydra.error.core",
        "hydra.error.packaging", "hydra.errors", "hydra.graph", "hydra.json.model",
        "hydra.packaging", "hydra.parsing", "hydra.query",
        "hydra.relational", "hydra.tabular", "hydra.testing", "hydra.topology",
        "hydra.typed", "hydra.typing", "hydra.util", "hydra.validation", "hydra.variants",
    ]
]


def make_def(placeholder):
    """Return a closure `(local_name, term) -> TypedBinding` bound to `placeholder`.

    Usage at the top of a source DSL module:

        _PLACEHOLDER = Module(...)
        _def = make_def(_PLACEHOLDER)
    """
    return lambda local_name, term: definition_in_module(placeholder, local_name, term)


def make_local(ns_str: str):
    """Return a closure `(local_name) -> TypedTerm` that builds a `var("<ns>.<local>")`.

    Usage at the top of a source DSL module:

        _local = make_local("hydra.python.coder")
        ...
        _local("encodeTerm")  # → var("hydra.python.coder.encodeTerm")
    """
    prefix = ns_str + "."
    return lambda local_name: var(prefix + local_name)


def ap(fun, *args):
    """Apply a function term to multiple arguments (left-associative).

    Equivalent to `apply(apply(apply(fun, a), b), c)` for `ap(fun, a, b, c)`.
    """
    out = fun
    for a in args:
        out = apply(out, a)
    return out


def py_name(s):
    """Wrap a str-or-TypedTerm into hydra.python.syntax.Name.

    - str input is auto-coerced via `string(s)`.
    - TypedTerm input passes through.
    """
    if isinstance(s, str):
        s = string(s)
    return PySyn.name(s)


def py_helper_name(local_name: str):
    """Reference a sibling helper in hydra.python.names by local name."""
    return var(f"hydra.python.names.{local_name}")


def lets_flat(bindings_pairs, body):
    """Build a single flat `let` from (name, value) pairs (matches Haskell `lets [...]`)."""
    fields = [field_op(n, v) for n, v in bindings_pairs]
    return lets(fields, body)


def proj(type_fq: str, field_name: str, var_name: str):
    """`project(typeName, fieldName) @@ var(varName)` — collapses the common
    project-then-apply pattern. Java analogue: `Phantoms.proj(...)`.

    Source modules with a fixed type-namespace prefix typically wrap this
    with a thinner local helper, e.g. in coder.py:
        def _env(field, var_name):
            return proj("hydra.python.environment.PythonEnvironment", field, var_name)
    """
    return project(Name(type_fq), Name(field_name))(var(var_name))
