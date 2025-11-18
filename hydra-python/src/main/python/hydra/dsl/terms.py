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
    EliminationProduct,
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
    Sum,
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
    TermProduct,
    TermRecord,
    TermSet,
    TermSum,
    TermTypeApplication,
    TermTypeLambda,
    TermUnion,
    TermVariable,
    TermWrap,
    TupleProjection,
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
    """Annotate a term with a map of names to terms."""
    return TermAnnotated(
        AnnotatedTerm(t, FrozenDict({Name(k): v for k, v in ann.items()}))
    )


def annotated(t: Term, ann: Mapping[str, Term]) -> Term:
    """Annotate a term with a map of names to terms (alternative argument order)."""
    return annot(ann, t)


def apply(func: Term, arg: Term) -> Term:
    """Apply a function to an argument."""
    return TermApplication(Application(func, arg))


def apply_all(func: Term, args: Sequence[Term]) -> Term:
    """Apply a function to multiple arguments."""
    return reduce(apply, args, func)


def bigfloat(value: float) -> Term:
    """Construct a bigfloat term."""
    return literal(lt.bigfloat(value))


def bigint(value: int) -> Term:
    """Construct a bigint term."""
    return literal(lt.bigint(value))


def binary(value: bytes) -> Term:
    """Construct a binary term."""
    return literal(lt.binary(value))


def boolean(value: bool) -> Term:
    """Construct a boolean term."""
    return literal(lt.boolean(value))


def char(value: str) -> Term:
    """Construct a character term as an int32."""
    return int32(ord(value))


def comparison(comp: hydra.util.Comparison) -> Term:
    """Construct a comparison term."""
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
    """Compose two terms."""
    return TermFunction(
        FunctionLambda(Lambda(Name("arg_"), Nothing(), apply(f, apply(g, var("arg_")))))
    )


def constant(term: Term) -> Term:
    """Construct a constant term."""
    return lambda_(hydra.constants.ignored_variable, term)


def elimination(elim: Elimination) -> Term:
    """Construct an elimination term."""
    return TermFunction(FunctionElimination(elim))


def false() -> Term:
    """Construct a false term."""
    return boolean(False)


def field(n: str, t: Term) -> Field:
    """Construct a field."""
    return Field(Name(n), t)


def fields_to_map(fields: Sequence[Field]) -> FrozenDict[Name, Term]:
    """Construct a map from fields to terms."""
    return FrozenDict({field.name: field.term for field in fields})


def first() -> Term:
    """Construct a first term."""
    return untuple(2, 0, Nothing())


def float32(value: float) -> Term:
    """Construct a float32 term."""
    return literal(lt.float32(value))


def float64(value: float) -> Term:
    """Construct a float64 term."""
    return literal(lt.float64(value))


def float_(value: FloatValue) -> Term:
    """Construct a float term."""
    return literal(lt.float_(value))


def identity() -> Term:
    """Construct an identity term."""
    return lambda_("x_", var("x_"))


def inject(tname: Name, fname: Name, term: Term) -> Term:
    """Construct an injection term."""
    return TermUnion(Injection(tname, Field(fname, term)))


def int16(value: int) -> Term:
    """Construct an int16 term."""
    return literal(lt.int16(value))


def int32(value: int) -> Term:
    """Construct an int32 term."""
    return literal(lt.int32(value))


def int64(value: int) -> Term:
    """Construct an int64 term."""
    return literal(lt.int64(value))


def int8(value: int) -> Term:
    """Construct an int8 term."""
    return literal(lt.int8(value))


def integer(value: IntegerValue) -> Term:
    """Construct an integer term."""
    return literal(lt.integer(value))


def just(term: Term) -> Term:
    """Construct a just term."""
    return optional(Just(term))


def lambda_(param: str, body: Term) -> Term:
    """Construct a lambda term."""
    return TermFunction(FunctionLambda(Lambda(Name(param), Nothing(), body)))


def lambdas(params: Sequence[str], body: Term) -> Term:
    """Construct a multi-parameter lambda function (curried)."""
    if not params:
        return body
    return lambda_(params[0], lambdas(params[1:], body))


def lambda_typed(param: str, dom: Type, body: Term) -> Term:
    """Construct a lambda term with explicit domain type."""
    return TermFunction(FunctionLambda(Lambda(Name(param), Just(dom), body)))


def let_multi(bindings: Sequence[tuple[str, Term]], body: Term) -> Term:
    """Construct a 'let' term with multiple bindings."""
    return TermLet(
        Let(
            tuple([Binding(Name(name), term, Nothing()) for name, term in bindings]),
            body,
        )
    )


def let_term(v: Name, t1: Term, t2: Term) -> Term:
    """Construct a 'let' term with a single binding."""
    return TermLet(Let(tuple([Binding(v, t1, Nothing())]), t2))


def lets(bindings: Sequence[Field], env: Term) -> Term:
    """Construct a 'let' term with any number of bindings."""
    def to_binding(f: Field) -> Binding:
        return Binding(f.name, f.term, Nothing())

    return TermLet(Let(tuple([to_binding(f) for f in bindings]), env))


def lets_typed(bindings: Sequence[tuple[str, Term, TypeScheme]], env: Term) -> Term:
    """Construct a 'let' term with typed bindings."""
    def to_binding(b: tuple[str, Term, TypeScheme]) -> Binding:
        name, term, ts = b
        return Binding(Name(name), term, Just(ts))

    return TermLet(Let(tuple([to_binding(b) for b in bindings]), env))


def left(term: Term) -> Term:
    """Construct a 'Left' either value."""
    return TermEither(Left(term))


def list_(terms: Sequence[Term]) -> Term:
    """Construct a list term."""
    return TermList(tuple(terms))


def literal(value: Literal) -> Term:
    """Construct a literal term."""
    return TermLiteral(value)


def map_(terms: Mapping[Term, Term]) -> Term:
    """Construct a map term."""
    return TermMap(FrozenDict(terms))


def map_term(terms: Mapping[Term, Term]) -> Term:
    """Construct a map term."""
    return TermMap(FrozenDict(terms))


def match(tname: Name, def_: Maybe[Term], fields: Sequence[Field]) -> Term:
    """Construct a match term."""
    return TermFunction(
        FunctionElimination(EliminationUnion(CaseStatement(tname, def_, tuple(fields))))
    )


def match_with_variants(
        tname: Name, def_: Maybe[Term], pairs: Sequence[tuple[Name, Name]]
) -> Term:
    """Construct a match term with variants."""
    return match(
        tname,
        def_,
        [Field(from_, constant(inject_unit(tname, to_))) for from_, to_ in pairs],
    )


def nothing() -> Term:
    """Construct a nothing term."""
    return optional(Nothing())


def optional(term: Maybe[Term]) -> Term:
    """Construct an optional term."""
    return TermMaybe(term)


def pair(a: Term, b: Term) -> Term:
    """Construct a pair term."""
    return TermProduct((a, b))


def primitive(name: Name) -> Term:
    """Construct a primitive term."""
    return TermFunction(FunctionPrimitive(name))


def product(terms: Sequence[Term]) -> Term:
    """Construct a product term."""
    return TermProduct(tuple(terms))


def project(tname: Name, fname: Name) -> Term:
    """Construct a projection term."""
    return TermFunction(
        FunctionElimination(EliminationRecord(Projection(tname, fname)))
    )


def record(tname: Name, fields: Sequence[Field]) -> Term:
    """Construct a record term."""
    return TermRecord(Record(tname, tuple(fields)))


def right(term: Term) -> Term:
    """Construct a 'Right' either value."""
    return TermEither(Right(term))


def second() -> Term:
    """Construct a second term."""
    return untuple(2, 1, Nothing())


def set_(s: set[Term]) -> Term:
    """Construct a set term."""
    return TermSet(frozenset(s))


def string(value: str) -> Term:
    """Construct a string term."""
    return literal(lt.string(value))


def sum_(i: int, s: int, term: Term) -> Term:
    """Construct a sum term."""
    return TermSum(Sum(i, s, term))


def triple(a: Term, b: Term, c: Term) -> Term:
    """Construct a triple term."""
    return tuple_([a, b, c])


def true() -> Term:
    """Construct a true term."""
    return boolean(True)


def tuple_(terms: Sequence[Term]) -> Term:
    """Construct a tuple term."""
    return TermProduct(tuple(terms))


def tuple4(a: Term, b: Term, c: Term, d: Term) -> Term:
    """Construct a 4-tuple term."""
    return tuple_([a, b, c, d])


def tuple5(a: Term, b: Term, c: Term, d: Term, e: Term) -> Term:
    """Construct a 5-tuple term."""
    return tuple_([a, b, c, d, e])


def type_application(term: Term, types: Sequence[Type]) -> Term:
    """Apply type arguments to a polymorphic term."""
    return reduce(
        lambda t, ty: TermTypeApplication(TypeApplicationTerm(t, ty)),
        types,
        term
    )


def type_lambda(vars: Sequence[Name], body: Term) -> Term:
    """Create a type abstraction (universal quantification)."""
    return reduce(
        lambda b, v: TermTypeLambda(TypeLambda(v, b)),
        vars,
        body
    )


def tyapp(term: Term, typ: Type) -> Term:
    """Apply a type argument to a polymorphic term."""
    return TermTypeApplication(TypeApplicationTerm(term, typ))


def tyapps(term: Term, types: Sequence[Type]) -> Term:
    """Apply type arguments to a polymorphic term."""
    return type_application(term, types)


def tylam(var: str, body: Term) -> Term:
    """Create a type abstraction with a single variable."""
    return TermTypeLambda(TypeLambda(Name(var), body))


def tylams(vars: Sequence[str], body: Term) -> Term:
    """Create a type abstraction with multiple variables."""
    return reduce(
        lambda b, v: TermTypeLambda(TypeLambda(Name(v), b)),
        reversed(vars),
        body
    )


def uint16(value: int) -> Term:
    """Construct a uint16 term."""
    return literal(lt.uint16(value))


def uint32(value: int) -> Term:
    """Construct a uint32 term."""
    return literal(lt.uint32(value))


def uint64(value: int) -> Term:
    """Construct a uint64 term."""
    return literal(lt.uint64(value))


def uint8(value: int) -> Term:
    """Construct a uint8 term."""
    return literal(lt.uint8(value))


def unit() -> Term:
    """Construct a unit term."""
    return record(Name("_Unit"), [])


def inject_unit(tname: Name, fname: Name) -> Term:
    """Construct a unit injection term."""
    return inject(tname, fname, unit())


def untuple(arity: int, idx: int, mtypes: Maybe[Sequence[Type]]) -> Term:
    """Construct a tuple projection term."""
    match mtypes:
        case Just(types):
            domain = Just(tuple(types))
        case Nothing():
            domain = Nothing()

    return TermFunction(
        FunctionElimination(
            EliminationProduct(
                TupleProjection(arity, idx, domain)
            )
        )
    )


def unwrap(name: Name) -> Term:
    """Construct an unwrap term."""
    return TermFunction(FunctionElimination(EliminationWrap(name)))


def var(name: str) -> Term:
    """Construct a variable term."""
    return TermVariable(Name(name))




def with_variant(tname: Name, fname: Name) -> Term:
    """Create a constant function that produces a unit variant."""
    return constant(inject_unit(tname, fname))


def wrap(name: Name, term: Term) -> Term:
    """Construct a wrap term."""
    return TermWrap(WrappedTerm(name, term))
