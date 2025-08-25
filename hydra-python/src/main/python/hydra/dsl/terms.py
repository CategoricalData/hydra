"""A DSL for constructing Hydra terms in Python."""

from collections.abc import Mapping, Sequence

import hydra.dsl.literals as lt
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
    TermFunction,
    TermLet,
    TermList,
    TermLiteral,
    TermMap,
    TermOptional,
    TermProduct,
    TermRecord,
    TermSet,
    TermSum,
    TermTyped,
    TermUnion,
    TermVariable,
    TermWrap,
    TupleProjection,
    Type,
    TypedTerm,
    WrappedTerm,
)
from hydra.dsl.python import FrozenDict


def annot(ann: Mapping[str, Term], t: Term) -> Term:
    """Annotate a term with a map of names to terms."""
    return TermAnnotated(
        AnnotatedTerm(t, FrozenDict({Name(k): v for k, v in ann.items()}))
    )


def apply(func: Term, arg: Term) -> Term:
    """Apply a function to an argument."""
    return TermApplication(Application(func, arg))


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


def compose(f: Term, g: Term) -> Term:
    """Compose two terms."""
    return TermFunction(
        FunctionLambda(Lambda(Name("x"), None, apply(f, apply(g, var("x")))))
    )


def constant(term: Term) -> Term:
    """Construct a constant term."""
    return lam("_", term)


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
    return untuple(2, 0, None)


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
    return lam("x", var("x"))


def inject(tname: Name, field: Field) -> Term:
    """Construct an inject term."""
    return TermUnion(Injection(tname, field))


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
    return optional(term)


def lam(param: str, body: Term) -> Term:
    """Construct a lambda term."""
    return TermFunction(FunctionLambda(Lambda(Name(param), None, body)))


def let_multi(bindings: Sequence[tuple[str, Term]], body: Term) -> Term:
    """Construct a 'let' term with multiple bindings."""
    return TermLet(
        Let(
            tuple([Binding(Name(name), term, None) for name, term in bindings]),
            body,
        )
    )


def let_term(v: Name, t1: Term, t2: Term) -> Term:
    """Construct a 'let' term with a single binding."""
    return TermLet(Let(tuple([Binding(v, t1, None)]), t2))


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


def match(tname: Name, def_: Term | None, fields: Sequence[Field]) -> Term:
    """Construct a match term."""
    return TermFunction(
        FunctionElimination(EliminationUnion(CaseStatement(tname, def_, tuple(fields))))
    )


def match_with_variants(
    tname: Name, def_: Term | None, pairs: Sequence[tuple[Name, Name]]
) -> Term:
    """Construct a match term with variants."""
    return match(
        tname,
        def_,
        [Field(from_, constant(unit_variant(tname, to_))) for from_, to_ in pairs],
    )


def nothing() -> Term:
    """Construct a nothing term."""
    return optional(None)


def optional(term: Term | None) -> Term:
    """Construct an optional term."""
    return TermOptional(term)


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


def second() -> Term:
    """Construct a second term."""
    return untuple(2, 1, None)


def set(s: set[Term]) -> Term:
    """Construct a set term."""
    return TermSet(frozenset(s))


def string(value: str) -> Term:
    """Construct a string term."""
    return literal(lt.string(value))


def sum(i: int, s: int, term: Term) -> Term:
    """Construct a sum term."""
    return TermSum(Sum(i, s, term))


def true() -> Term:
    """Construct a true term."""
    return boolean(True)


def typed(typ: Type, term: Term) -> Term:
    """Construct a typed term."""
    return TermTyped(TypedTerm(term, typ))


def uint16(value: int) -> Term:
    """Construct a uint16 term."""
    return literal(lt.uint16(value))


def uint32(value: int) -> Term:
    """Construct a uint32 term."""
    return literal(lt.uint32(value))


def uint64(value: int) -> Term:
    """Construct a uint64 term."""
    return literal(lt.uint64(value))


def unit() -> Term:
    """Construct a unit term."""
    return record(Name("_Unit"), [])


def unit_variant(tname: Name, fname: Name) -> Term:
    """Construct a unit variant term."""
    return variant(tname, fname, unit())


def untuple(arity: int, idx: int, mtypes: Sequence[Type] | None) -> Term:
    """Construct a tuple projection term."""
    return TermFunction(
        FunctionElimination(
            EliminationProduct(
                TupleProjection(arity, idx, tuple(mtypes) if mtypes else None)
            )
        )
    )


def unwrap(name: Name) -> Term:
    """Construct an unwrap term."""
    return TermFunction(FunctionElimination(EliminationWrap(name)))


def var(name: str) -> Term:
    """Construct a variable term."""
    return TermVariable(Name(name))


def variant(tname: Name, fname: Name, term: Term) -> Term:
    """Construct a variant term."""
    return TermUnion(Injection(tname, Field(fname, term)))


def with_(env: Term, bindings: Sequence[Field]) -> Term:
    """Construct a with variant term."""

    def to_binding(field: Field) -> Binding:
        return Binding(field.name, field.term, None)

    return TermLet(Let(tuple([to_binding(field) for field in bindings]), env))


def wrap(name: Name, term: Term) -> Term:
    """Construct a wrap term."""
    return TermWrap(WrappedTerm(name, term))
