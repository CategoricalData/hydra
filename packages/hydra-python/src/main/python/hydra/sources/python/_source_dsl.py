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

from hydra.overlay.python.dsl.meta.phantoms import *  # noqa: F401,F403
from hydra.overlay.python.dsl.python import None_
from hydra.core import Name, Type, TypeScheme
from hydra.packaging import DefinitionType, ModuleDependency, ModuleName, TypeDefinition
import hydra.dsl.python.syntax as PySyn
import hydra.overlay.python.dsl.types as T


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


_MISSING = object()


class _DefBuilder:
    """Fluent builder for a term definition: `_def("name").doc("...").lam("x").to(body)`.

    Reads top-to-bottom instead of the inside-out `_def("name", doc("...", lambdas(["x"], body)))`
    nesting. `.to(body)` closes the builder and yields the same `TypedBinding` the flat form
    would: it wraps `body` as `doc(description, lambdas([params], body))`, omitting the `doc`/
    `lambdas` wrappers when none were specified. Unlike the Java builder there is no laziness —
    each Python def is built when its enclosing function runs, so `.to` takes the already-built
    term (mirroring the eager `make_def` two-arg form).
    """

    __slots__ = ("_placeholder", "_name", "_description", "_params")

    def __init__(self, placeholder, local_name):
        self._placeholder = placeholder
        self._name = local_name
        self._description = None
        self._params = []

    def doc(self, description):
        """Attach a doc description, wrapping the body in `doc(description, ...)`."""
        self._description = description
        return self

    def lam(self, param):
        """Add one lambda parameter (applied outermost-first, matching `lambdas(["x", ...], ...)`)."""
        self._params.append(param)
        return self

    def lams(self, *params):
        """Add several lambda parameters in order (equivalent to chained `.lam` calls)."""
        self._params.extend(params)
        return self

    def to(self, body):
        """Close over the body and produce the `TypedBinding`."""
        term = body if not self._params else lambdas(self._params, body)
        if self._description is not None:
            term = doc(self._description, term)
        return definition_in_module(self._placeholder, self._name, term)


def make_def(placeholder):
    """Return a `_def` callable bound to `placeholder`, supporting two forms:

    - Flat:   `_def(local_name, term) -> TypedBinding` (the original form).
    - Fluent: `_def(local_name).doc("...").lam("x").to(body) -> TypedBinding`.

    Usage at the top of a source DSL module:

        _PLACEHOLDER = Module(...)
        _def = make_def(_PLACEHOLDER)
    """
    def _def(local_name, term=_MISSING):
        if term is _MISSING:
            return _DefBuilder(placeholder, local_name)
        return definition_in_module(placeholder, local_name, term)
    return _def


def make_local_str(ns_str: str):
    """Return a closure `(local_name) -> TypedTerm` that builds a `var("<ns>.<local>")`.

    A string-argument convenience wrapper over phantoms' `make_local(ns: ModuleName)`
    (distinct name to avoid shadowing it; the two share one implementation).

    Usage at the top of a source DSL module:

        _local = make_local_str("hydra.python.coder")
        ...
        _local("encodeTerm")  # → var("hydra.python.coder.encodeTerm")
    """
    return make_local(ModuleName(ns_str))


def type_ref(ns: ModuleName, local: str) -> Type:
    """A namespace-qualified TypeVariable reference: `<ns>.<local>`.

    The type-level analogue of `make_local`'s `var("<ns>.<local>")`. Type-defining
    source modules typically wrap this with a thinner per-namespace helper, e.g.:

        def _py(local):  # in syntax.py
            return type_ref(ModuleName("hydra.python.syntax"), local)
    """
    return T.variable(f"{ns.value}.{local}")


def make_type_def(ns: ModuleName):
    """Return a closure `(local_name, typ) -> DefinitionType` bound to `ns`.

    The type-module analogue of `make_def` (which builds term `TypedBinding`s).
    Wraps a bare `Type` in a monomorphic `TypeScheme` and names it `<ns>.<local>`.

    Usage at the top of a type-defining source DSL module:

        NS = ModuleName("hydra.python.syntax")
        _def = make_type_def(NS)
        ...
        _def("Module", T.wrap(...))  # → DefinitionType(TypeDefinition(...))

    Note: the Python type-codegen path does not tolerate a doc-annotated TypeScheme
    body, so this leaves the type structure undecorated (field docs, where present,
    live on the type itself rather than the scheme).
    """
    def _def(local_name: str, typ: Type) -> DefinitionType:
        name = Name(f"{ns.value}.{local_name}")
        ts = TypeScheme((), typ, None_())
        return DefinitionType(TypeDefinition(name, None_(), ts))
    return _def


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
