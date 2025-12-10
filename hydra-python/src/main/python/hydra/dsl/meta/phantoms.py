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
import hydra.classes
import hydra.util
from hydra.core import Field, Name, Term, Type
from hydra.module import Module, Namespace, QualifiedName
from hydra.phantoms import TBinding, TTerm
from hydra.dsl.python import FrozenDict, Maybe, Just, Nothing

# Re-export phantom literals
from hydra.dsl.meta.phantom_literals import (
    bigfloat,
    bigint,
    binary,
    boolean,
    double,
    false,
    float_,
    float32,
    float64,
    int8,
    int16,
    int32,
    int64,
    integer,
    string,
    true,
)

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")
X = TypeVar("X")


def un_tterm(t: TTerm[A]) -> Term:
    """Extract the underlying Term from a TTerm."""
    return t.value


# Operators - Python equivalents for Haskell operators
# (~>) :: String -> TTerm x -> TTerm (a -> b) - use lambda_(name, body)
# (<~) :: String -> TTerm a -> TTerm b -> TTerm b - use let1(name, value, body)
# (<<~) :: String -> TTerm (Flow s a) -> TTerm (Flow s b) -> TTerm (Flow s b) - use bind(name, def_, body)
# (<.>) :: TTerm (b -> c) -> TTerm (a -> b) -> TTerm (a -> c) - use compose(f, g)
# (@@) :: TTerm (a -> b) -> TTerm a -> TTerm b - use apply(fun, arg)
# (>:) :: String -> TTerm a -> Field - use field_op(name, term)
# (>>:) :: Name -> TTerm a -> Field - use field_name_op(fname, term)


def annot(key: Name, mvalue: Maybe[Term], term: TTerm[A]) -> TTerm[A]:
    """Add an annotation to a term."""
    return TTerm[A](annotations.annotate_term(key, mvalue, un_tterm(term)))


def apply(fun: TTerm[A], arg: TTerm[B]) -> TTerm[C]:
    """Apply a function to an argument."""
    return TTerm[C](terms.apply(un_tterm(fun), un_tterm(arg)))


def binary_function(f) -> TTerm[A]:
    """Extract a binary function from a function application."""
    term = un_tterm(f(var("x"), var("y")))
    match term:
        case terms.TermApplication(terms.Application(
            terms.TermApplication(terms.Application(lhs, _)), _
        )):
            return TTerm[A](lhs)
        case _:
            return TTerm[A](terms.string(f"unexpected term as binary function: {term}"))


def bind(v: str, def_: TTerm[A], body: TTerm[B]) -> TTerm[B]:
    """Bind a value in a Flow context."""
    return primitive2(
        Name("hydra.lib.flows.bind"),
        def_,
        lambda_(v, body)
    )


def binds(fields: Sequence[Field], rhs: TTerm[A]) -> TTerm[A]:
    """Create multiple bindings in a Flow context."""
    result = rhs
    for field in reversed(fields):
        fname = field.name.value
        fterm = TTerm[A](field.term)
        result = bind(fname, fterm, result)
    return result


def cases(name: Name, arg: TTerm[A], dflt: Maybe[TTerm[B]], fields: Sequence[Field]) -> TTerm[B]:
    """Apply a named case match to an argument."""
    match dflt:
        case Just(d):
            dflt_term = Just(un_tterm(d))
        case Nothing():
            dflt_term = Nothing()

    return TTerm[B](terms.apply(terms.match(name, dflt_term, fields), un_tterm(arg)))


def compose(f: TTerm[B], g: TTerm[A]) -> TTerm[C]:
    """Compose two functions (g then f)."""
    return TTerm[C](terms.compose(un_tterm(f), un_tterm(g)))


def constant(term: TTerm[A]) -> TTerm[B]:
    """Create a constant function that always returns the same value."""
    return TTerm[B](terms.constant(un_tterm(term)))


def definition_in_module(mod: Module, lname: str, term: TTerm[A]) -> TBinding[A]:
    """Create a definition in a module."""
    return definition_in_namespace(mod.namespace, lname, term)


def definition_in_namespace(ns: Namespace, lname: str, term: TTerm[A]) -> TBinding[A]:
    """Create a definition in a namespace."""
    qname = QualifiedName(Just(ns), lname)
    name = unqualify_name(qname)
    return TBinding(name, term)


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
        hydra.util.CaseConvention.LOWER_SNAKE,
        hydra.util.CaseConvention.CAMEL,
        clean_name
    )

    # Construct full primitive name
    return Name(f"hydra.lib.{lib_name}.{camel_name}")


def doc(s: str, term: TTerm[A]) -> TTerm[A]:
    """Add documentation to a term."""
    return TTerm[A](annotations.set_term_description(Just(s), un_tterm(term)))


def doc_wrapped(length: int, s: str, term: TTerm[A]) -> TTerm[A]:
    """Add documentation with line wrapping at the specified width."""
    return TTerm[A](annotations.data_doc(annotations.wrap_line(length, s), un_tterm(term)))


def el(binding: TBinding[A]) -> terms.Binding:
    """Convert a typed element to an untyped element."""
    return terms.Binding(binding.name, un_tterm(binding.term), Nothing())


def exec_(f: TTerm[A], b: TTerm[B]) -> TTerm[B]:
    """Execute a Flow action and discard its result."""
    return primitive2(
        Name("hydra.lib.flows.bind"),
        f,
        lambda_(hydra.constants.ignored_variable, b)
    )


def field(fname: Name, val: TTerm[A]) -> Field:
    """Create a field with the given name and value."""
    return Field(fname, un_tterm(val))


def field_name_op(fname: Name, d: TTerm[A]) -> Field:
    """Field definition operator with pre-constructed name: fname>>: value."""
    return Field(fname, un_tterm(d))


def field_op(name: str, term: TTerm[A]) -> Field:
    """Field definition operator: name>: value."""
    return Field(Name(name), un_tterm(term))


def first(pair: TTerm[tuple[A, B]]) -> TTerm[A]:
    """First element projection function for pairs."""
    return apply(TTerm[A](terms.first()), pair)


def first_class_type(typ: TTerm[Type]) -> TTerm[Type]:
    """Mark a type as first-class."""
    return annot(
        hydra.constants.key_first_class_type,
        Just(terms.boolean(True)),
        typ
    )


def fold(f: TTerm[A]) -> TTerm[B]:
    """Create a fold function to process lists."""
    return apply(primitive(Name("hydra.lib.lists.foldl")), f)


def identity() -> TTerm[A]:
    """Identity function that returns its argument unchanged."""
    return TTerm[A](terms.identity())


def inject(name: Name, fname: Name, term: TTerm[A]) -> TTerm[B]:
    """Create a union injection."""
    return TTerm[B](terms.inject(name, fname, un_tterm(term)))


def inject_lambda(name: Name, fname: Name) -> TTerm[A]:
    """Create a function that injects its argument into a union variant."""
    return lambda_("injected_", inject(name, fname, var("injected_")))


def just(term: TTerm[A]) -> TTerm[Maybe[A]]:
    """Create a 'Just' optional value."""
    return TTerm[Maybe[A]](terms.just(un_tterm(term)))


def just_() -> TTerm[A]:
    """Function that wraps a value in 'Just'."""
    return TTerm[A](terms.lambda_("just_", terms.just(terms.var("just_"))))


def lambda_(v: str, body: TTerm[X]) -> TTerm[A]:
    """Create a lambda function with one parameter."""
    return TTerm[A](terms.lambda_(v, un_tterm(body)))


def lambdas(params: Sequence[str], body: TTerm[X]) -> TTerm[A]:
    """Create a multi-parameter lambda function."""
    return TTerm[A](terms.lambdas(params, un_tterm(body)))


def let1(name: str, value: TTerm[A], env: TTerm[B]) -> TTerm[B]:
    """Create a let expression with a single binding."""
    return TTerm[B](terms.let_term(Name(name), un_tterm(value), un_tterm(env)))


def lets(fields: Sequence[Field], env: TTerm[A]) -> TTerm[A]:
    """Create a let expression with multiple bindings."""
    return TTerm[A](terms.lets(fields, un_tterm(env)))


def lib_primitive() -> TTerm:
    """
    Automatically derive and apply a library primitive with no arguments.

    The primitive name is derived from the calling function's name and module.
    """
    return primitive(derive_primitive_name())


def lib_primitive1(a: TTerm) -> TTerm:
    """
    Automatically derive and apply a library primitive with one argument.

    The primitive name is derived from the calling function's name and module.
    """
    return primitive1(derive_primitive_name(), a)


def lib_primitive2(a: TTerm, b: TTerm) -> TTerm:
    """
    Automatically derive and apply a library primitive with two arguments.

    The primitive name is derived from the calling function's name and module.
    """
    return primitive2(derive_primitive_name(), a, b)


def lib_primitive3(a: TTerm, b: TTerm, c: TTerm) -> TTerm:
    """
    Automatically derive and apply a library primitive with three arguments.

    The primitive name is derived from the calling function's name and module.
    """
    return primitive3(derive_primitive_name(), a, b, c)


def list_(els: Sequence[TTerm[A]]) -> TTerm[list[A]]:
    """Create a list of terms."""
    return TTerm[list[A]](terms.list_([un_tterm(el) for el in els]))


def map_(m: Mapping[TTerm[A], TTerm[B]]) -> TTerm[dict[A, B]]:
    """Create a map/dictionary term."""
    return TTerm[dict[A, B]](
        terms.map_({un_tterm(k): un_tterm(v) for k, v in m.items()})
    )


def match(name: Name, dflt: Maybe[TTerm[B]], fields: Sequence[Field]) -> TTerm[A]:
    """Create a pattern match on a union term."""
    match dflt:
        case Just(d):
            dflt_term = Just(un_tterm(d))
        case Nothing():
            dflt_term = Nothing()

    return TTerm[A](terms.match(name, dflt_term, fields))


def module_namespace(mod: Module) -> Namespace:
    """Get the namespace from a module."""
    return mod.namespace


def nothing() -> TTerm[Maybe[A]]:
    """Create a 'Nothing' optional value."""
    return TTerm[Maybe[A]](terms.nothing())


def opt(mc: Maybe[TTerm[A]]) -> TTerm[Maybe[A]]:
    """Create an optional value from a Maybe."""
    match mc:
        case Just(c):
            return TTerm[Maybe[A]](terms.optional(Just(un_tterm(c))))
        case Nothing():
            return TTerm[Maybe[A]](terms.optional(Nothing()))


def opt_cases(arg: TTerm[Maybe[A]], if_nothing: TTerm[B], if_just: TTerm[A]) -> TTerm[B]:
    """Pattern match on an optional value."""
    return primitive3(
        Name("hydra.lib.maybes.maybe"),
        if_nothing,
        if_just,
        arg
    )


def pair(l: TTerm[A], r: TTerm[B]) -> TTerm[tuple[A, B]]:
    """Create a pair (2-tuple)."""
    return TTerm[tuple[A, B]](terms.pair(un_tterm(l), un_tterm(r)))


def primitive(prim_name: Name) -> TTerm[A]:
    """Primitive function by name."""
    return TTerm[A](terms.primitive(prim_name))


def primitive1(prim_name: Name, a: TTerm[A]) -> TTerm[B]:
    """Apply a primitive function to one argument."""
    return TTerm[B](terms.apply(terms.primitive(prim_name), un_tterm(a)))


def primitive2(prim_name: Name, a: TTerm[A], b: TTerm[B]) -> TTerm[C]:
    """Apply a primitive function to two arguments."""
    return TTerm[C](
        terms.apply(
            terms.apply(terms.primitive(prim_name), un_tterm(a)),
            un_tterm(b)
        )
    )


def primitive3(prim_name: Name, a: TTerm[A], b: TTerm[B], c: TTerm[C]) -> TTerm[X]:
    """Apply a primitive function to three arguments."""
    return TTerm[X](
        terms.apply(
            terms.apply(
                terms.apply(terms.primitive(prim_name), un_tterm(a)),
                un_tterm(b)
            ),
            un_tterm(c)
        )
    )


def produce(value: TTerm[A]) -> TTerm[A]:
    """Lift a value into a Flow context."""
    return primitive1(Name("hydra.lib.flows.pure"), value)


def project(name: Name, fname: Name) -> TTerm[A]:
    """Extract a field from a record."""
    return TTerm[A](terms.project(name, fname))


def record(name: Name, fields: Sequence[Field]) -> TTerm[A]:
    """Create a record with named fields."""
    return TTerm[A](terms.record(name, fields))


def ref(binding: TBinding[A]) -> TTerm[A]:
    """Reference a defined element."""
    return TTerm[A](terms.TermVariable(binding.name))


def second(pair: TTerm[tuple[A, B]]) -> TTerm[B]:
    """Second element projection function for pairs."""
    return apply(TTerm[B](terms.second()), pair)


def set_(els: Sequence[TTerm[A]]) -> TTerm[set[A]]:
    """Create a set of terms."""
    return TTerm[set[A]](terms.set_({un_tterm(el) for el in els}))


def trace(msg: TTerm[str], flow: TTerm[A]) -> TTerm[A]:
    """Add tracing to a Flow."""
    return apply(apply(var("hydra.monads.withTrace"), msg), flow)


def triple(a: TTerm[A], b: TTerm[B], c: TTerm[C]) -> TTerm[tuple[A, B, C]]:
    """Create a triple."""
    return TTerm[tuple[A, B, C]](
        terms.triple(un_tterm(a), un_tterm(b), un_tterm(c))
    )


def tuple4(a: TTerm[A], b: TTerm[B], c: TTerm[C], d: TTerm[X]) -> TTerm[tuple[A, B, C, X]]:
    """Create a 4-tuple."""
    return TTerm[tuple[A, B, C, X]](
        terms.tuple4(un_tterm(a), un_tterm(b), un_tterm(c), un_tterm(d))
    )


def tuple5(
        a: TTerm[A], b: TTerm[B], c: TTerm[C], d: TTerm[X], e: TTerm[X]
) -> TTerm[tuple[A, B, C, X, X]]:
    """Create a 5-tuple."""
    return TTerm[tuple[A, B, C, X, X]](
        terms.tuple5(un_tterm(a), un_tterm(b), un_tterm(c), un_tterm(d), un_tterm(e))
    )


def unary_function(f) -> TTerm[A]:
    """Extract a unary function from a function application."""
    term = un_tterm(f(var("x")))
    match term:
        case terms.TermApplication(terms.Application(lhs, _)):
            return TTerm[A](lhs)
        case terms.TermMaybe(Just(_)):
            return TTerm[A](terms.primitive(Name("hydra.lib.maybes.pure")))
        case terms.TermUnion(terms.Injection(tname, Field(fname, _))):
            return lambda_("x", inject(tname, fname, var("x")))
        case terms.TermWrap(terms.WrappedTerm(tname, _)):
            return lambda_("x", wrap(tname, var("x")))
        case _:
            raise ValueError(f"Cannot extract unary function from: {term}")


def unit() -> TTerm[A]:
    """Unit value (empty record)."""
    return TTerm[A](terms.unit())


def inject_unit(name: Name, fname: Name) -> TTerm[A]:
    """Create a unit injection of a union."""
    return TTerm[A](terms.inject(name, fname, terms.unit()))


def unqualify_name(qname: QualifiedName) -> Name:
    """Convert a QualifiedName to a Name."""
    match qname.namespace:
        case Just(ns):
            return Name(f"{ns.value}.{qname.local}")
        case Nothing():
            return Name(qname.local)


def unwrap(name: Name) -> TTerm[A]:
    """Create an unwrap function for a wrapped type."""
    return TTerm[A](terms.unwrap(name))


def var(v: str) -> TTerm[A]:
    """Create a variable reference."""
    return TTerm[A](terms.var(v))




def with_eq(v: str, term: TTerm[A]) -> TTerm[A]:
    """Associate the Eq type class with the inferred type of a term."""
    return with_type_classes(
        FrozenDict({Name(v): frozenset([hydra.classes.TypeClass.EQUALITY])}),
        term
    )


def with_ord(v: str, term: TTerm[A]) -> TTerm[A]:
    """Associate the Ord type class with the inferred type of a term."""
    return with_type_classes(
        FrozenDict({Name(v): frozenset([hydra.classes.TypeClass.ORDERING])}),
        term
    )


def with_type_classes(classes: FrozenDict[Name, frozenset], term: TTerm[A]) -> TTerm[A]:
    """Associate type classes with the inferred type of a term."""
    import hydra.annotations
    return TTerm[A](hydra.annotations.set_type_classes(classes, un_tterm(term)))


def wrap(name: Name, term: TTerm[A]) -> TTerm[B]:
    """Create a wrapped term (instance of a newtype)."""
    return TTerm[B](terms.wrap(name, un_tterm(term)))
