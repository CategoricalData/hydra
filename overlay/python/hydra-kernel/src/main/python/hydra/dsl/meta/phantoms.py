"""Term-level DSL which makes use of phantom types.

Use this DSL for defining programs as opposed to data type definitions.
The phantom types provide static type checking in Python prior to Hydra's runtime type checking.
"""

import inspect
from collections.abc import Mapping, Sequence
from typing import TypeVar

import hydra.constants
import hydra.dsl.terms as terms
import hydra.dsl.annotations as annotations
import hydra.formatting
from hydra.util import CaseConvention, QualifiedName
from hydra.core import Binding, Field, Name, Term, Type
from hydra.packaging import Module, ModuleName
from hydra.typed import TypedBinding, TypedTerm
from hydra.dsl.python import FrozenDict, Optional, Given, None_


def _tbinding_matmul(self, other):
    """Apply a TypedBinding as a function reference to an argument."""
    ref_term = TypedTerm(terms.TermVariable(self.name))
    return ref_term @ other

TypedBinding.__matmul__ = _tbinding_matmul


def _tterm_matmul(self, other):
    """Apply a TypedTerm function-term to a TypedTerm argument: `f @ x` ≡ `apply(f, x)`.

    Enables the `@@` Haskell idiom in Python:
        var("f") @ var("x")           # apply(f, x)
        var("f") @ var("x") @ var("y") # apply(apply(f, x), y)
    """
    if not isinstance(other, TypedTerm):
        return NotImplemented
    return TypedTerm(terms.apply(self.value, other.value))

TypedTerm.__matmul__ = _tterm_matmul


def _tterm_call(self, *args):
    """Treat a TypedTerm function-term as callable: `f(x, y, z)` ≡ `apply(apply(apply(f, x), y), z)`.

    This is the natural Python idiom for term application — equivalent to Haskell's
    `f @@ x @@ y @@ z`. All args must themselves be TypedTerm.
    """
    out = self
    for a in args:
        if not isinstance(a, TypedTerm):
            raise TypeError(
                f"TypedTerm() called with non-TypedTerm argument of type {type(a).__name__}"
            )
        out = TypedTerm(terms.apply(out.value, a.value))
    return out

TypedTerm.__call__ = _tterm_call


def _name_rshift(self, val):
    """Create a Field from a Name and a TypedTerm: name >> term."""
    return Field(self, un_tterm(val))

Name.__rshift__ = _name_rshift

from enum import Enum

def _enum_rshift(self, val):
    """Create a Field from an enum member (whose value is a Name) and a TypedTerm: member >> term."""
    return Field(self.value, un_tterm(val))

Enum.__rshift__ = _enum_rshift


# Re-export phantom literals
from hydra.dsl.meta.phantom_literals import (
    bigint,
    binary,
    boolean,
    char,
    decimal,
    double,
    false,
    float_,
    float32,
    float64,
    int_,
    int8,
    int16,
    int32,
    int64,
    integer,
    string,
    true,
    uint8,
    uint16,
    uint32,
    uint64,
)

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")
X = TypeVar("X")


def _name(n) -> Name:
    """Coerce a str or Name to a Name. Convenience for accepting either form."""
    return n if isinstance(n, Name) else Name(n)


def un_tterm(t: TypedTerm[A]) -> Term:
    """Extract the underlying Term from a TypedTerm."""
    return t.value


# Operators - Python equivalents for Haskell operators
# (~>) :: String -> TypedTerm x -> TypedTerm (a -> b) - use lam(name, body)
# (<~) :: String -> TypedTerm a -> TypedTerm b -> TypedTerm b - use let1(name, value, body)
# (<.>) :: TypedTerm (b -> c) -> TypedTerm (a -> b) -> TypedTerm (a -> c) - use compose(f, g)
# (@@) :: TypedTerm (a -> b) -> TypedTerm a -> TypedTerm b - use apply(fun, arg)
# (>:) :: String -> TypedTerm a -> Field - use field_op(name, term)
# (>>:) :: Name -> TypedTerm a -> Field - use field_name_op(fname, term)


def annot(key: Name, mvalue: Optional[Term], term: TypedTerm[A]) -> TypedTerm[A]:
    """Add an annotation to a term."""
    return TypedTerm[A](annotations.annotate_term(key, mvalue, un_tterm(term)))


def apply(fun: TypedTerm[A], arg: TypedTerm[B]) -> TypedTerm[C]:
    """Apply a function to an argument."""
    return TypedTerm[C](terms.apply(un_tterm(fun), un_tterm(arg)))


def binary_function(f) -> TypedTerm[A]:
    """Extract a binary function from a function application."""
    term = un_tterm(f(var("x"), var("y")))
    match term:
        case terms.TermApplication(terms.Application(
            terms.TermApplication(terms.Application(lhs, _)), _
        )):
            return TypedTerm[A](lhs)
        case _:
            return TypedTerm[A](terms.string(f"unexpected term as binary function: {term}"))


def cases(name, arg: TypedTerm[A], dflt: Optional[TypedTerm[B]] = None, fields: Sequence[Field] = ()) -> TypedTerm[B]:
    """Apply a named case match to an argument.

    Accepts a str or Name. Without a default, pass `dflt=None` (treated as None_()).
    With a default, pass `dflt=Given(default_tterm)` or use `cases_with_default(...)`.
    """
    if dflt is None:
        dflt_term = None_()
    else:
        match dflt:
            case Given(d):
                dflt_term = Given(un_tterm(d))
            case None_():
                dflt_term = None_()

    return TypedTerm[B](terms.apply(terms.match(_name(name), dflt_term, fields), un_tterm(arg)))


def cases_with_default(name, arg: TypedTerm[A], default: TypedTerm[B], *fields: Field) -> TypedTerm[B]:
    """Apply a named case match with a default branch.

    Java-style alternative to `cases(name, arg, Given(default), [f1, f2, ...])`.
    Accepts variadic field args for ergonomics.
    """
    return TypedTerm[B](terms.apply(
        terms.match(_name(name), Given(un_tterm(default)), list(fields)),
        un_tterm(arg)
    ))


def compose(f: TypedTerm[B], g: TypedTerm[A]) -> TypedTerm[C]:
    """Compose two functions (g then f)."""
    return TypedTerm[C](terms.compose(un_tterm(f), un_tterm(g)))


def constant(term: TypedTerm[A]) -> TypedTerm[B]:
    """Create a constant function that always returns the same value."""
    return TypedTerm[B](terms.constant(un_tterm(term)))


def definition_in_module(mod: Module, lname: str, term: TypedTerm[A]) -> TypedBinding[A]:
    """Create a definition in a module."""
    return definition_in_namespace(mod.name, lname, term)


def definition_in_namespace(ns: ModuleName, lname: str, term: TypedTerm[A] | None = None) -> "TypedBinding[A] | DefineBuilder":
    """Create a definition in a namespace.

    With 3 arguments: returns a TypedBinding (direct definition).
    With 2 arguments: returns a DefineBuilder for fluent chaining.
    """
    qname = QualifiedName(Given(ns), lname)
    name = unqualify_name(qname)
    if term is not None:
        return TypedBinding(name, term)
    return DefineBuilder(name, [])


def make_local(ns: ModuleName):
    """Create a local reference function for a namespace.

    Returns a function that takes a local name and produces a TypedTerm variable
    reference with the full qualified name. Use this for recursive or
    forward references within a module.

    Usage:
        local = make_local(ns)
        ...
        .to(local("rewriteTerm") @ var("rewrite") @ var("term"))
    """
    prefix = ns.value + "."
    return lambda lname: var(prefix + lname)


def to_binding(tb: TypedBinding[A]) -> Binding:
    """Convert a TypedBinding to an untyped Binding for use in module element lists.

    This mirrors Haskell's toBinding and Java's Phantoms.toBinding().
    """
    return Binding(tb.name, tb.term.value, None_())


def to_definition(tb: TypedBinding[A]):
    """Convert a TypedBinding to a term Definition for use in Module.definitions.

    Mirrors Haskell's Hydra.Dsl.Meta.Phantoms.toDefinition.
    """
    from hydra.packaging import DefinitionTerm, TermDefinition
    return DefinitionTerm(TermDefinition(tb.name, None_(), None_(), tb.term.value))


def to_term_definition(tb: TypedBinding[A]):
    """Convert a TypedBinding to a term Definition (alias for to_definition)."""
    return to_definition(tb)


def derive_primitive_name() -> Name:
    """
    Automatically derive the primitive name from the calling function's context.

    Uses introspection to get:
    - Function name from the call stack
    - Module name to extract the library name

    Returns:
        The fully qualified primitive name (e.g., "hydra.lib.strings.fromList")
    """
    # Get the calling function's frame (2 levels up: this function -> lib_primitiveN -> actual DSL function)
    current_frame = inspect.currentframe()
    assert current_frame is not None, "Cannot get current frame"
    caller_frame = current_frame.f_back
    assert caller_frame is not None, "Cannot get caller frame"
    dsl_function_frame = caller_frame.f_back
    assert dsl_function_frame is not None, "Cannot get DSL function frame"

    func_name = dsl_function_frame.f_code.co_name
    module_name = dsl_function_frame.f_globals['__name__']

    # Extract library name from module (e.g., "hydra.dsl.lib.strings" -> "strings")
    lib_name = module_name.split('.')[-1]

    # Remove trailing underscore if present (e.g., abs_ -> abs), then convert snake_case to camelCase
    clean_name = func_name.rstrip('_')
    camel_name = hydra.formatting.convert_case(
        CaseConvention.LOWER_SNAKE,
        CaseConvention.CAMEL,
        clean_name
    )

    # Construct full primitive name
    return Name(f"hydra.lib.{lib_name}.{camel_name}")


def doc(s: str, term: TypedTerm[A] | None = None) -> "TypedTerm[A] | ExprBuilder":
    """Add documentation to a term.

    With 2 arguments: returns a TypedTerm (direct doc annotation).
    With 1 argument: returns an ExprBuilder for fluent chaining.
    """
    if term is not None:
        return TypedTerm[A](annotations.set_term_description(Given(s), un_tterm(term)))
    return ExprBuilder([("doc", s, None)])


def doc_wrapped(length: int, s: str, term: TypedTerm[A]) -> TypedTerm[A]:
    """Add documentation with line wrapping at the specified width."""
    return TypedTerm[A](annotations.data_doc(annotations.wrap_line(length, s), un_tterm(term)))


def el(binding: TypedBinding[A]) -> terms.Binding:
    """Convert a typed element to an untyped element."""
    return terms.Binding(binding.name, un_tterm(binding.term), None_())


def field(fname, val: TypedTerm[A]) -> Field:
    """Create a field with the given name and value. Accepts str or Name."""
    return Field(_name(fname), un_tterm(val))


def field_name_op(fname, d: TypedTerm[A]) -> Field:
    """Field definition operator with pre-constructed name: fname>>: value."""
    return Field(_name(fname), un_tterm(d))


def field_op(name: str, term: TypedTerm[A]) -> Field:
    """Field definition operator: name>: value."""
    return Field(Name(name), un_tterm(term))


def first(pair: TypedTerm[tuple[A, B]]) -> TypedTerm[A]:
    """First element projection function for pairs."""
    return apply(TypedTerm[A](terms.first()), pair)


def first_class_type(typ: TypedTerm[Type]) -> TypedTerm[Type]:
    """Mark a type as first-class."""
    return annot(
        hydra.constants.key_first_class_type,
        Given(terms.boolean(True)),
        typ
    )


def fold(f: TypedTerm[A]) -> TypedTerm[B]:
    """Create a fold function to process lists."""
    return apply(primitive(Name("hydra.lib.lists.foldl")), f)


def identity() -> TypedTerm[A]:
    """Identity function that returns its argument unchanged."""
    return TypedTerm[A](terms.identity())


def inject(name, fname, term: TypedTerm[A]) -> TypedTerm[B]:
    """Create a union injection. Accepts str or Name for type/field names."""
    return TypedTerm[B](terms.inject(_name(name), _name(fname), un_tterm(term)))


def inject_lambda(name, fname) -> TypedTerm[A]:
    """Create a function that injects its argument into a union variant."""
    return lam("injected_", inject(name, fname, var("injected_")))


def just(term: TypedTerm[A]) -> TypedTerm[Optional[A]]:
    """Create a 'Given' optional value."""
    return TypedTerm[Optional[A]](terms.just(un_tterm(term)))


def just_() -> TypedTerm[A]:
    """Function that wraps a value in 'Given'."""
    return TypedTerm[A](terms.lambda_("just_", terms.just(terms.var("just_"))))


def lambdas(params: Sequence[str], body: TypedTerm[X]) -> TypedTerm[A]:
    """Create a multi-parameter lambda function."""
    return TypedTerm[A](terms.lambdas(params, un_tterm(body)))


def left(term: TypedTerm[A]) -> TypedTerm[A]:
    """Create a 'Left' either value."""
    return TypedTerm[A](terms.left(un_tterm(term)))


def left_() -> TypedTerm[A]:
    """Function that wraps a value in 'Left'."""
    return TypedTerm[A](terms.lambda_("left_", terms.left(terms.var("left_"))))


def right(term: TypedTerm[A]) -> TypedTerm[A]:
    """Create a 'Right' either value."""
    return TypedTerm[A](terms.right(un_tterm(term)))


def right_() -> TypedTerm[A]:
    """Function that wraps a value in 'Right'."""
    return TypedTerm[A](terms.lambda_("right_", terms.right(terms.var("right_"))))


def let1(name: str, value: TypedTerm[A], env: TypedTerm[B]) -> TypedTerm[B]:
    """Create a let expression with a single binding (alias for let with explicit args)."""
    return TypedTerm[B](terms.let_term(Name(name), un_tterm(value), un_tterm(env)))


def unsafe_cast(t: TypedTerm[A]) -> TypedTerm[B]:
    """Cast a TypedTerm to a different phantom type. Used to bridge type inference gaps."""
    return TypedTerm[B](un_tterm(t))


def _build_term(intros: list[tuple[str, str, TypedTerm | None]], body: TypedTerm[B]) -> TypedTerm[B]:
    """Build a nested term from a list of intro forms and a body."""
    result = body
    for kind, name, value in reversed(intros):
        if kind == "doc":
            result = TypedTerm[B](annotations.set_term_description(Given(name), un_tterm(result)))
        elif kind == "lambda":
            result = TypedTerm[B](terms.lambda_(name, un_tterm(result)))
        else:
            result = TypedTerm[B](terms.let_term(Name(name), un_tterm(value), un_tterm(result)))
    return result


class ExprBuilder:
    """Fluent builder for chaining doc, lambdas, and single-binding lets without nesting.

    Usage:
        (doc("description")
          .lam("x")
          .lam("y")
          .let("z", expr)
          .to(body))

    Produces: doc "description" (lambda x -> lambda y -> let z = expr in body)
    """

    def __init__(self, intros: list[tuple[str, str, TypedTerm | None]]):
        # Each intro is ("doc", description, None), ("lambda", param, None),
        # or ("let", name, value)
        self._intros = intros

    def doc(self, s: str) -> "ExprBuilder":
        """Add a doc annotation."""
        return ExprBuilder(self._intros + [("doc", s, None)])

    def lam(self, v: str) -> "ExprBuilder":
        """Add a lambda parameter."""
        return ExprBuilder(self._intros + [("lambda", v, None)])

    def lams(self, *vs: str) -> "ExprBuilder":
        """Add multiple lambda parameters."""
        return ExprBuilder(self._intros + [("lambda", v, None) for v in vs])

    def let(self, name: str, value: TypedTerm[A]) -> "ExprBuilder":
        """Add a single-binding let."""
        return ExprBuilder(self._intros + [("let", name, value)])

    def to(self, body: TypedTerm[B]) -> TypedTerm[B]:
        """Finalize the chain with a body."""
        return _build_term(self._intros, body)


class DefineBuilder:
    """Fluent builder that starts with a definition name and produces a TypedBinding.

    Usage:
        define("freeVariablesInType")
          .doc("Find the free variables...")
          .lam("typ")
          .let("dfltVars", expr)
          .to(body)
    """

    def __init__(self, name: Name, intros: list[tuple[str, str, TypedTerm | None]]):
        self._name = name
        self._intros = intros

    def doc(self, s: str) -> "DefineBuilder":
        """Add a doc annotation."""
        return DefineBuilder(self._name, self._intros + [("doc", s, None)])

    def lam(self, v: str) -> "DefineBuilder":
        """Add a lambda parameter."""
        return DefineBuilder(self._name, self._intros + [("lambda", v, None)])

    def lams(self, *vs: str) -> "DefineBuilder":
        """Add multiple lambda parameters."""
        return DefineBuilder(self._name, self._intros + [("lambda", v, None) for v in vs])

    def let(self, name: str, value: TypedTerm[A]) -> "DefineBuilder":
        """Add a single-binding let."""
        return DefineBuilder(self._name, self._intros + [("let", name, value)])

    def to(self, body: TypedTerm[B]) -> TypedBinding[B]:
        """Finalize the chain with a body, producing a TypedBinding."""
        return TypedBinding(self._name, _build_term(self._intros, body))


def lam(v: str, body: TypedTerm[X] | None = None) -> TypedTerm[A] | ExprBuilder:
    """Create a lambda function with one parameter.

    With 2 arguments: returns a TypedTerm (direct lambda).
    With 1 argument: returns an ExprBuilder for fluent chaining.
    """
    if body is not None:
        return TypedTerm[A](terms.lambda_(v, un_tterm(body)))
    return ExprBuilder([("lambda", v, None)])


def lams(*vs: str) -> ExprBuilder:
    """Create multiple lambda parameters as an ExprBuilder for fluent chaining."""
    return ExprBuilder([("lambda", v, None) for v in vs])


def let(name: str, value: TypedTerm[A], env: TypedTerm[B] | None = None) -> TypedTerm[B] | ExprBuilder:
    """Create a let expression with a single binding.

    With 3 arguments: returns a TypedTerm (direct let expression).
    With 2 arguments: returns an ExprBuilder for fluent chaining.
    """
    if env is not None:
        return TypedTerm[B](terms.let_term(Name(name), un_tterm(value), un_tterm(env)))
    return ExprBuilder([("let", name, value)])


def lets(fields: Sequence[Field], env: TypedTerm[A]) -> TypedTerm[A]:
    """Create a let expression with multiple bindings."""
    return TypedTerm[A](terms.lets(fields, un_tterm(env)))


def bindings(*pairs) -> list[Field]:
    """Sugar for building a list of bindings from (name, value) tuples or Field objects.

    Examples:
        bindings(("x", val1), ("y", val2))  →  [Field("x", val1.value), Field("y", val2.value)]
        bindings(field("x", val1), ("y", val2))  →  works with mixed forms
    """
    out = []
    for p in pairs:
        if isinstance(p, Field):
            out.append(p)
        elif isinstance(p, tuple) and len(p) == 2:
            n, v = p
            out.append(Field(_name(n), un_tterm(v)))
        else:
            raise TypeError(f"bindings: expected Field or (name, value) tuple, got {type(p).__name__}")
    return out


def let_chain(pairs, body: TypedTerm[A]) -> TypedTerm[A]:
    """Emit nested singleton lets (matches Haskell's `<~` chaining).

    Each pair is (name, value). Reverses the list to build inside-out so the
    first pair is the outermost binding (in scope for all subsequent ones).
    """
    out = body
    for p in reversed(pairs):
        if isinstance(p, Field):
            # Field: extract name and value
            out = TypedTerm[A](terms.let_term(p.name, p.value, un_tterm(out)))
        else:
            n, v = p
            out = let1(n if isinstance(n, str) else n.value, v, out)
    return out


def lib_primitive() -> TypedTerm:
    """
    Automatically derive and apply a library primitive with no arguments.

    The primitive name is derived from the calling function's name and module.
    """
    return primitive(derive_primitive_name())


def lib_primitive1(a: TypedTerm) -> TypedTerm:
    """
    Automatically derive and apply a library primitive with one argument.

    The primitive name is derived from the calling function's name and module.
    """
    return primitive1(derive_primitive_name(), a)


def lib_primitive2(a: TypedTerm, b: TypedTerm) -> TypedTerm:
    """
    Automatically derive and apply a library primitive with two arguments.

    The primitive name is derived from the calling function's name and module.
    """
    return primitive2(derive_primitive_name(), a, b)


def lib_primitive3(a: TypedTerm, b: TypedTerm, c: TypedTerm) -> TypedTerm:
    """
    Automatically derive and apply a library primitive with three arguments.

    The primitive name is derived from the calling function's name and module.
    """
    return primitive3(derive_primitive_name(), a, b, c)


def list_(els: Sequence[TypedTerm[A]]) -> TypedTerm[list[A]]:
    """Create a list of terms."""
    return TypedTerm[list[A]](terms.list_([un_tterm(el) for el in els]))


def map_(m: Mapping[TypedTerm[A], TypedTerm[B]]) -> TypedTerm[dict[A, B]]:
    """Create a map/dictionary term."""
    return TypedTerm[dict[A, B]](
        terms.map_({un_tterm(k): un_tterm(v) for k, v in m.items()})
    )


def match(name, dflt: Optional[TypedTerm[B]] = None, fields: Sequence[Field] = ()) -> TypedTerm[A]:
    """Create a pattern match on a union term. Accepts str or Name."""
    if dflt is None:
        dflt_term = None_()
    else:
        match dflt:
            case Given(d):
                dflt_term = Given(un_tterm(d))
            case None_():
                dflt_term = None_()

    return TypedTerm[A](terms.match(_name(name), dflt_term, fields))


def module_name(mod: Module) -> ModuleName:
    """Get the namespace from a module."""
    return mod.name


def nothing() -> TypedTerm[Optional[A]]:
    """Create a 'None_' optional value."""
    return TypedTerm[Optional[A]](terms.nothing())


def opt(mc: Optional[TypedTerm[A]]) -> TypedTerm[Optional[A]]:
    """Create an optional value from an optional."""
    match mc:
        case Given(c):
            return TypedTerm[Optional[A]](terms.optional(Given(un_tterm(c))))
        case None_():
            return TypedTerm[Optional[A]](terms.optional(None_()))


def opt_cases(arg: TypedTerm[Optional[A]], if_nothing: TypedTerm[B], if_just: TypedTerm[A]) -> TypedTerm[B]:
    """Pattern match on an optional value."""
    return primitive3(
        Name("hydra.lib.optionals.cases"),
        arg,
        if_nothing,
        if_just
    )


def pair(l: TypedTerm[A], r: TypedTerm[B]) -> TypedTerm[tuple[A, B]]:
    """Create a pair (2-tuple)."""
    return TypedTerm[tuple[A, B]](terms.pair(un_tterm(l), un_tterm(r)))


def primitive(prim_name: Name) -> TypedTerm[A]:
    """Primitive function by name."""
    return TypedTerm[A](terms.primitive(prim_name))


def primitive1(prim_name: Name, a: TypedTerm[A]) -> TypedTerm[B]:
    """Apply a primitive function to one argument."""
    return TypedTerm[B](terms.apply(terms.primitive(prim_name), un_tterm(a)))


def primitive2(prim_name: Name, a: TypedTerm[A], b: TypedTerm[B]) -> TypedTerm[C]:
    """Apply a primitive function to two arguments."""
    return TypedTerm[C](
        terms.apply(
            terms.apply(terms.primitive(prim_name), un_tterm(a)),
            un_tterm(b)
        )
    )


def primitive3(prim_name: Name, a: TypedTerm[A], b: TypedTerm[B], c: TypedTerm[C]) -> TypedTerm[X]:
    """Apply a primitive function to three arguments."""
    return TypedTerm[X](
        terms.apply(
            terms.apply(
                terms.apply(terms.primitive(prim_name), un_tterm(a)),
                un_tterm(b)
            ),
            un_tterm(c)
        )
    )


def project(name, fname) -> TypedTerm[A]:
    """Extract a field from a record. Accepts str or Name."""
    return TypedTerm[A](terms.project(_name(name), _name(fname)))


def record(name, fields: Sequence[Field]) -> TypedTerm[A]:
    """Create a record with named fields. Accepts str or Name."""
    return TypedTerm[A](terms.record(_name(name), fields))


def ref(binding: TypedBinding[A]) -> TypedTerm[A]:
    """Reference a defined element."""
    return TypedTerm[A](terms.TermVariable(binding.name))


def second(pair: TypedTerm[tuple[A, B]]) -> TypedTerm[B]:
    """Second element projection function for pairs."""
    return apply(TypedTerm[B](terms.second()), pair)


def set_(els: Sequence[TypedTerm[A]]) -> TypedTerm[set[A]]:
    """Create a set of terms."""
    return TypedTerm[set[A]](terms.set_({un_tterm(el) for el in els}))


def triple(a: TypedTerm[A], b: TypedTerm[B], c: TypedTerm[C]) -> TypedTerm[tuple[A, B, C]]:
    """Create a triple."""
    return TypedTerm[tuple[A, B, C]](
        terms.triple(un_tterm(a), un_tterm(b), un_tterm(c))
    )


def tuple4(a: TypedTerm[A], b: TypedTerm[B], c: TypedTerm[C], d: TypedTerm[X]) -> TypedTerm[tuple[A, B, C, X]]:
    """Create a 4-tuple."""
    return TypedTerm[tuple[A, B, C, X]](
        terms.tuple4(un_tterm(a), un_tterm(b), un_tterm(c), un_tterm(d))
    )


def tuple5(
        a: TypedTerm[A], b: TypedTerm[B], c: TypedTerm[C], d: TypedTerm[X], e: TypedTerm[X]
) -> TypedTerm[tuple[A, B, C, X, X]]:
    """Create a 5-tuple."""
    return TypedTerm[tuple[A, B, C, X, X]](
        terms.tuple5(un_tterm(a), un_tterm(b), un_tterm(c), un_tterm(d), un_tterm(e))
    )


def unary_function(f) -> TypedTerm[A]:
    """Extract a unary function from a function application."""
    term = un_tterm(f(var("x")))
    match term:
        case terms.TermApplication(terms.Application(lhs, _)):
            return TypedTerm[A](lhs)
        case terms.TermOptional(Given(_)):
            return TypedTerm[A](terms.primitive(Name("hydra.lib.optionals.pure")))
        case terms.TermInject(terms.Injection(tname, Field(fname, _))):
            return lam("x", inject(tname, fname, var("x")))
        case terms.TermWrap(terms.WrappedTerm(tname, _)):
            return lam("x", wrap(tname, var("x")))
        case _:
            raise ValueError(f"Cannot extract unary function from: {term}")


def unit() -> TypedTerm[A]:
    """Unit value (empty record)."""
    return TypedTerm[A](terms.unit())


def inject_unit(name, fname) -> TypedTerm[A]:
    """Create a unit injection of a union. Accepts str or Name."""
    return TypedTerm[A](terms.inject(_name(name), _name(fname), terms.unit()))


def unqualify_name(qname: QualifiedName) -> Name:
    """Convert a QualifiedName to a Name."""
    match qname.module_name:
        case Given(ns):
            return Name(f"{ns.value}.{qname.local}")
        case None_():
            return Name(qname.local)


def unwrap(name) -> TypedTerm[A]:
    """Create an unwrap function for a wrapped type. Accepts str or Name."""
    return TypedTerm[A](terms.unwrap(_name(name)))


def var(v: str) -> TypedTerm[A]:
    """Create a variable reference."""
    return TypedTerm[A](terms.var(v))




def with_eq(v: str, term: TypedTerm[A]) -> TypedTerm[A]:
    """Associate the Eq type class with the inferred type of a term."""
    return with_type_classes(
        FrozenDict({Name(v): frozenset([Name("equality")])}),
        term
    )


def with_ord(v: str, term: TypedTerm[A]) -> TypedTerm[A]:
    """Associate the Ord type class with the inferred type of a term."""
    return with_type_classes(
        FrozenDict({Name(v): frozenset([Name("ordering")])}),
        term
    )


def with_type_classes(classes: FrozenDict[Name, frozenset], term: TypedTerm[A]) -> TypedTerm[A]:
    """Associate type classes with the inferred type of a term."""
    import hydra.annotations
    return TypedTerm[A](hydra.annotations.set_type_classes(classes, un_tterm(term)))


def wrap(name, term: TypedTerm[A]) -> TypedTerm[B]:
    """Create a wrapped term (instance of a newtype). Accepts str or Name."""
    return TypedTerm[B](terms.wrap(_name(name), un_tterm(term)))
