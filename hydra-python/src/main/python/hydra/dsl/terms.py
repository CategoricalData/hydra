"""A DSL for constructing Hydra terms in Python."""

from collections.abc import Mapping, Sequence
from functools import reduce

import hydra.constants
import hydra.dsl.literals as lt
import hydra.util
from hydra.core import (
    AnnotatedTerm,
    Application,
    CaseStatement,
    Elimination,
    EliminationRecord,
    EliminationUnion,
    EliminationWrap,
    Field,
    FloatValue,
    FunctionElimination,
    FunctionLambda,
    FunctionPrimitive,
    Injection,
    IntegerValue,
    Lambda,
    Let,
    Binding,
    Literal,
    Name,
    Projection,
    Record,
    Term,
    TermAnnotated,
    TermApplication,
    TermEither,
    TermFunction,
    TermLet,
    TermList,
    TermLiteral,
    TermMap,
    TermMaybe,
    TermPair,
    TermRecord,
    TermSet,
    TermTypeApplication,
    TermTypeLambda,
    TermUnion,
    TermUnit,
    TermVariable,
    TermWrap,
    Type,
    TypeApplicationTerm,
    TypeLambda,
    TypeScheme,
    WrappedTerm,
)
from hydra.dsl.python import FrozenDict, Maybe, Just, Nothing, Left, Right


# Operators - TODO: find Python equivalents for these special syntax forms
# (<.>) :: Term -> Term -> Term  -- function composition operator
# (@@) :: Term -> Term -> Term   -- term application operator
# (>:) :: String -> Term -> Field  -- field definition operator


def annot(ann: Mapping[str, Term], t: Term) -> Term:
    """Attach an annotation to a term.

    Example: annot({"comment": string("A User ID")}, var("userId"))
    """
    return TermAnnotated(
        AnnotatedTerm(t, FrozenDict({Name(k): v for k, v in ann.items()}))
    )


def annotated(t: Term, ann: Mapping[str, Term]) -> Term:
    """Attach an annotation to a term (alternative argument order).

    Example: annotated(var("userId"), {"comment": string("A User ID")})
    """
    return annot(ann, t)


def apply(func: Term, arg: Term) -> Term:
    """Apply a function term to an argument.

    Example: apply(var("capitalize"), string("arthur"))
    """
    return TermApplication(Application(func, arg))


def apply_all(func: Term, args: Sequence[Term]) -> Term:
    """Apply a function to multiple arguments (curried application).

    Example: apply_all(var("add"), [int32(1), int32(2)])
    """
    return reduce(apply, args, func)


def bigfloat(value: float) -> Term:
    """Create a bigfloat literal.

    Example: bigfloat(3.14159265359)
    """
    return literal(lt.bigfloat(value))


def bigint(value: int) -> Term:
    """Create a bigint literal.

    Example: bigint(9223372036854775808)
    """
    return literal(lt.bigint(value))


def binary(value: bytes) -> Term:
    """Create a binary data literal from bytes.

    Example: binary(b'\\x48\\x65\\x00\\xff\\x20\\x7a\\x1b\\x80')
    """
    return literal(lt.binary(value))


def boolean(value: bool) -> Term:
    """Create a boolean literal.

    Example: boolean(True)
    """
    return literal(lt.boolean(value))


def char(value: str) -> Term:
    """Create a character term (represented as int32 code point).

    Example: char('A')  # Creates int32(65)
    """
    return int32(ord(value))


def comparison(comp: hydra.util.Comparison) -> Term:
    """Create a comparison result term (lessThan, equalTo, or greaterThan)."""
    match comp:
        case hydra.util.Comparison.EQUAL_TO:
            return inject_unit(
                hydra.util.COMPARISON__NAME,
                hydra.util.COMPARISON__EQUAL_TO__NAME
            )
        case hydra.util.Comparison.LESS_THAN:
            return inject_unit(
                hydra.util.COMPARISON__NAME,
                hydra.util.COMPARISON__LESS_THAN__NAME
            )
        case hydra.util.Comparison.GREATER_THAN:
            return inject_unit(
                hydra.util.COMPARISON__NAME,
                hydra.util.COMPARISON__GREATER_THAN__NAME
            )


def compose(f: Term, g: Term) -> Term:
    """Compose two functions (apply g then f) to create a new function.

    Example: compose(var("stringLength"), var("toString"))
    This creates a function equivalent to: lambda x: stringLength(toString(x))
    Function composition applies right-to-left: (f . g)(x) = f(g(x))
    """
    return TermFunction(
        FunctionLambda(Lambda(Name("arg_"), Nothing(), apply(f, apply(g, var("arg_")))))
    )


def constant(term: Term) -> Term:
    """Create a constant function that always returns the same value.

    Example: constant(true())  # A function that always returns True
    """
    return lambda_(hydra.constants.ignored_variable, term)


def elimination(elim: Elimination) -> Term:
    """Construct a term from an elimination (record projection, union case, or unwrap)."""
    return TermFunction(FunctionElimination(elim))


def false() -> Term:
    """Boolean false literal."""
    return boolean(False)


def field(n: str, t: Term) -> Field:
    """Create a field with the given name and value.

    Example: field("age", int32(30))
    """
    return Field(Name(n), t)


def fields_to_map(fields: Sequence[Field]) -> FrozenDict[Name, Term]:
    """Convert a sequence of fields to a map from names to terms."""
    return FrozenDict({field.name: field.term for field in fields})


def first() -> Term:
    """First element projection function for pairs."""
    return primitive(Name("hydra.lib.pairs.first"))


def float32(value: float) -> Term:
    """Create a float32 literal.

    Example: float32(3.14)
    """
    return literal(lt.float32(value))


def float64(value: float) -> Term:
    """Create a float64 literal.

    Example: float64(3.14159265359)
    """
    return literal(lt.float64(value))


def float_(value: FloatValue) -> Term:
    """Create a floating-point literal with specified precision.

    Example: float_(FloatValueFloat32(3.14))
    """
    return literal(lt.float_(value))


def identity() -> Term:
    """Identity function that returns its argument unchanged."""
    return lambda_("x_", var("x_"))


def inject(tname: Name, fname: Name, term: Term) -> Term:
    """Create a union value by injecting a value into a specific variant.

    Example: inject(Name("Result"), Name("success"), int32(42))
    This creates a "Result" union with the "success" variant containing value 42.
    Use this to construct values of union types at runtime.
    """
    return TermUnion(Injection(tname, Field(fname, term)))


def int16(value: int) -> Term:
    """Create an int16 literal.

    Example: int16(32767)
    """
    return literal(lt.int16(value))


def int32(value: int) -> Term:
    """Create an int32 literal.

    Example: int32(42)
    """
    return literal(lt.int32(value))


def int64(value: int) -> Term:
    """Create an int64 literal.

    Example: int64(9223372036854775807)
    """
    return literal(lt.int64(value))


def int8(value: int) -> Term:
    """Create an int8 literal.

    Example: int8(127)
    """
    return literal(lt.int8(value))


def integer(value: IntegerValue) -> Term:
    """Create an integer literal with specified bit width.

    Example: integer(IntegerValueInt32(42))
    """
    return literal(lt.integer(value))


def just(term: Term) -> Term:
    """Create a 'Just' optional value.

    Example: just(string("found"))
    """
    return optional(Just(term))


def lambda_(param: str, body: Term) -> Term:
    """Create a lambda function with one parameter.

    Example: lambda_("x", apply(var("x"), int32(1)))
    """
    return TermFunction(FunctionLambda(Lambda(Name(param), Nothing(), body)))


def lambdas(params: Sequence[str], body: Term) -> Term:
    """Create a multi-parameter lambda function (curried).

    Example: lambdas(["x", "y"], apply_all(var("add"), [var("x"), var("y")]))
    This creates the function: lambda x: lambda y: add(x, y)
    """
    if not params:
        return body
    return lambda_(params[0], lambdas(params[1:], body))


def lambda_typed(param: str, dom: Type, body: Term) -> Term:
    """Create a lambda function with an explicit domain type.

    Example: lambda_typed("x", Types.int32, list_([var("x")]))
    """
    return TermFunction(FunctionLambda(Lambda(Name(param), Just(dom), body)))


def let_multi(bindings: Sequence[tuple[str, Term]], body: Term) -> Term:
    """Create a let expression with multiple bindings.

    Example: let_multi([("x", int32(1)), ("y", int32(2))], apply_all(var("add"), [var("x"), var("y")]))
    """
    return TermLet(
        Let(
            tuple([Binding(Name(name), term, Nothing()) for name, term in bindings]),
            body,
        )
    )


def let_term(v: Name, t1: Term, t2: Term) -> Term:
    """Create a let expression with a single binding."""
    return TermLet(Let(tuple([Binding(v, t1, Nothing())]), t2))


def lets(bindings: Sequence[Field], env: Term) -> Term:
    """Create a let term with any number of bindings.

    Example: lets([field("x", int32(1)), field("y", int32(2))], pair(var("x"), var("y")))
    """
    def to_binding(f: Field) -> Binding:
        return Binding(f.name, f.term, Nothing())

    return TermLet(Let(tuple([to_binding(f) for f in bindings]), env))


def lets_typed(bindings: Sequence[tuple[str, Term, TypeScheme]], env: Term) -> Term:
    """Create a let expression with typed bindings."""
    def to_binding(b: tuple[str, Term, TypeScheme]) -> Binding:
        name, term, ts = b
        return Binding(Name(name), term, Just(ts))

    return TermLet(Let(tuple([to_binding(b) for b in bindings]), env))


def left(term: Term) -> Term:
    """Create a 'Left' either value.

    Example: left(string("error"))
    """
    return TermEither(Left(term))


def list_(terms: Sequence[Term]) -> Term:
    """Create a list of terms.

    Example: list_([int32(1), int32(2), int32(3)])
    """
    return TermList(tuple(terms))


def literal(value: Literal) -> Term:
    """Create a term from a literal value.

    Example: literal(LiteralString("hello"))
    """
    return TermLiteral(value)


def map_(terms: Mapping[Term, Term]) -> Term:
    """Create a map/dictionary term.

    Example: map_({string("January"): int32(31), string("February"): int32(28)})
    """
    return TermMap(FrozenDict(terms))


def map_term(terms: Mapping[Term, Term]) -> Term:
    """Create a map/dictionary term (alias for map_)."""
    return TermMap(FrozenDict(terms))


def match(tname: Name, def_: Maybe[Term], fields: Sequence[Field]) -> Term:
    """Create a pattern match on a union type.

    Example: match(Name("Result"), Just(string("unknown")),
                   [field("success", lambda_("s", apply(var("processSuccess"), var("s")))),
                    field("error", lambda_("e", apply(var("handleError"), var("e"))))])
    This allows handling different cases of a union type with specific logic for each variant.
    The optional second parameter provides a default case for any unmatched variants.
    """
    return TermFunction(
        FunctionElimination(EliminationUnion(CaseStatement(tname, def_, tuple(fields))))
    )


def match_with_variants(
        tname: Name, def_: Maybe[Term], pairs: Sequence[tuple[Name, Name]]
) -> Term:
    """Create a match term that maps variant names to other variants."""
    return match(
        tname,
        def_,
        [Field(from_, constant(inject_unit(tname, to_))) for from_, to_ in pairs],
    )


def nothing() -> Term:
    """Create a 'Nothing' optional value."""
    return optional(Nothing())


def optional(term: Maybe[Term]) -> Term:
    """Create an optional (nullable) term.

    Example: optional(Just(string("found")))
    """
    return TermMaybe(term)


def pair(a: Term, b: Term) -> Term:
    """Create a pair.

    Example: pair(string("name"), int32(42))
    """
    return TermPair((a, b))


def primitive(name: Name) -> Term:
    """Create a primitive function reference.

    Example: primitive(Name("hydra.lib.strings.length"))
    """
    return TermFunction(FunctionPrimitive(name))


def project(tname: Name, fname: Name) -> Term:
    """Create a field projection function.

    Example: project(Name("Person"), Name("firstName"))
    """
    return TermFunction(
        FunctionElimination(EliminationRecord(Projection(tname, fname)))
    )


def record(tname: Name, fields: Sequence[Field]) -> Term:
    """Create a record with named fields of the specified type.

    Example: record(Name("Person"), [
               field("name", string("John")),
               field("age", int32(30)),
               field("email", string("john@example.com"))])
    Records are products of named fields with values that can be accessed by field name.
    """
    return TermRecord(Record(tname, tuple(fields)))


def right(term: Term) -> Term:
    """Create a 'Right' either value.

    Example: right(int32(42))
    """
    return TermEither(Right(term))


def second() -> Term:
    """Second element projection function for pairs."""
    return primitive(Name("hydra.lib.pairs.second"))


def set_(s: set[Term]) -> Term:
    """Create a set of terms.

    Example: set_({string("a"), string("b"), string("c")})
    """
    return TermSet(frozenset(s))


def string(value: str) -> Term:
    """Create a string literal.

    Example: string("hello world")
    """
    return literal(lt.string(value))


def triple(a: Term, b: Term, c: Term) -> Term:
    """Create a triple using nested pairs."""
    return pair(a, pair(b, c))


def true() -> Term:
    """Boolean true literal."""
    return boolean(True)


def tuple_(terms: Sequence[Term]) -> Term:
    """Create a tuple using nested pairs.

    Example: tuple_([a, b, c]) creates pair(a, pair(b, c))
    """
    if len(terms) == 0:
        return unit()
    elif len(terms) == 1:
        return terms[0]
    elif len(terms) == 2:
        return pair(terms[0], terms[1])
    else:
        return pair(terms[0], tuple_(terms[1:]))


def tuple2(a: Term, b: Term) -> Term:
    """Create a 2-tuple (same as pair)."""
    return pair(a, b)


def tuple3(a: Term, b: Term, c: Term) -> Term:
    """Create a 3-tuple using nested pairs."""
    return pair(a, pair(b, c))


def tuple4(a: Term, b: Term, c: Term, d: Term) -> Term:
    """Create a 4-tuple using nested pairs."""
    return pair(a, pair(b, pair(c, d)))


def tuple5(a: Term, b: Term, c: Term, d: Term, e: Term) -> Term:
    """Create a 5-tuple using nested pairs."""
    return pair(a, pair(b, pair(c, pair(d, e))))


def type_application(term: Term, types: Sequence[Type]) -> Term:
    """Apply type arguments to a polymorphic term.

    Example: type_application(var("map"), [Types.int32, Types.string])
    This instantiates a polymorphic function with concrete types.
    For instance, if 'map' has type 'forall a b. (a -> b) -> list a -> list b',
    the example would instantiate it to '(int32 -> string) -> list int32 -> list string'.
    """
    return reduce(
        lambda t, ty: TermTypeApplication(TypeApplicationTerm(t, ty)),
        types,
        term
    )


def type_lambda(vars: Sequence[Name], body: Term) -> Term:
    """Create a type abstraction (universal quantification).

    Example: type_lambda([Name("a"), Name("b")],
               lambda_typed("f", Types.function(Types.var("a"), Types.var("b")),
                 lambda_typed("x", Types.var("a"), apply(var("f"), var("x")))))
    This creates a polymorphic term with type variables.
    The example creates a higher-order function with type 'forall a b. (a -> b) -> a -> b'.
    """
    return reduce(
        lambda b, v: TermTypeLambda(TypeLambda(v, b)),
        vars,
        body
    )


def tyapp(term: Term, typ: Type) -> Term:
    """Apply a single type argument to a polymorphic term."""
    return TermTypeApplication(TypeApplicationTerm(term, typ))


def tyapps(term: Term, types: Sequence[Type]) -> Term:
    """Apply type arguments to a polymorphic term (alias for type_application)."""
    return type_application(term, types)


def tylam(var: str, body: Term) -> Term:
    """Create a type abstraction with a single type variable."""
    return TermTypeLambda(TypeLambda(Name(var), body))


def tylams(vars: Sequence[str], body: Term) -> Term:
    """Create a type abstraction with multiple type variables."""
    return reduce(
        lambda b, v: TermTypeLambda(TypeLambda(Name(v), b)),
        reversed(vars),
        body
    )


def uint16(value: int) -> Term:
    """Create a uint16 literal.

    Example: uint16(65535)
    """
    return literal(lt.uint16(value))


def uint32(value: int) -> Term:
    """Create a uint32 literal.

    Example: uint32(4294967295)
    """
    return literal(lt.uint32(value))


def uint64(value: int) -> Term:
    """Create a uint64 literal.

    Example: uint64(18446744073709551615)
    """
    return literal(lt.uint64(value))


def uint8(value: int) -> Term:
    """Create a uint8 literal.

    Example: uint8(255)
    """
    return literal(lt.uint8(value))


def unit() -> Term:
    """Unit value (empty record)."""
    return TermUnit()


def inject_unit(tname: Name, fname: Name) -> Term:
    """Create a unit variant of a union (convenience function).

    Example: inject_unit(Name("Result"), Name("success"))
    Equivalent to inject but automatically uses unit as the value.
    """
    return inject(tname, fname, unit())




def unwrap(name: Name) -> Term:
    """Create an unwrap function for a wrapped type.

    Example: unwrap(Name("Email"))
    """
    return TermFunction(FunctionElimination(EliminationWrap(name)))


def var(name: str) -> Term:
    """Create a variable reference.

    Example: var("x")
    """
    return TermVariable(Name(name))


def with_variant(tname: Name, fname: Name) -> Term:
    """Create a constant function that produces a unit variant."""
    return constant(inject_unit(tname, fname))


def wrap(name: Name, term: Term) -> Term:
    """Create a wrapped term (instance of a newtype).

    Example: wrap(Name("Email"), string("user@example.com"))
    """
    return TermWrap(WrappedTerm(name, term))
