"""A module for decoding terms to native objects."""

from __future__ import annotations

from collections.abc import Callable

import hydra.gen.core
import hydra.gen.strip
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.optionals
from hydra.dsl.python import frozenlist


def bigfloat(v1: hydra.gen.core.Term) -> float | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: float_literal(v1), v1), lambda v1: bigfloat_value(v1), v1)

def bigfloat_value(v1: hydra.gen.core.FloatValue) -> float | None:
    match v1:
        case hydra.gen.core.FloatValueBigfloat(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def bigint(v1: hydra.gen.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: bigint_value(v1), v1)

def bigint_value(v1: hydra.gen.core.IntegerValue) -> int | None:
    match v1:
        case hydra.gen.core.IntegerValueBigint(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def binary(v1: hydra.gen.core.Term) -> bytes | None:
    return hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: binary_literal(v1), v1)

def binary_literal(v1: hydra.gen.core.Literal) -> bytes | None:
    match v1:
        case hydra.gen.core.LiteralBinary(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def boolean(v1: hydra.gen.core.Term) -> bool | None:
    return hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: boolean_literal(v1), v1)

def boolean_literal(v1: hydra.gen.core.Literal) -> bool | None:
    match v1:
        case hydra.gen.core.LiteralBoolean(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def cases_case(tname: hydra.gen.core.Name, fname: hydra.gen.core.Name, v1: hydra.gen.core.Term) -> hydra.gen.core.Term | None:
    return hydra.lib.optionals.compose(lambda v1: cases(tname, v1), lambda v1: field(fname, v1), v1)

def cases() -> Callable[[hydra.gen.core.Name, hydra.gen.core.Term], frozenlist[hydra.gen.core.Field] | None]:
    def match_union(v1: hydra.gen.core.Elimination) -> hydra.gen.core.CaseStatement | None:
        match v1:
            case hydra.gen.core.EliminationUnion(matched_):
                return hydra.lib.optionals.pure(matched_)
            
            case _:
                return None
    def match_elimination(v1: hydra.gen.core.Function) -> hydra.gen.core.Elimination | None:
        match v1:
            case hydra.gen.core.FunctionElimination(matched_):
                return hydra.lib.optionals.pure(matched_)
            
            case _:
                return None
    def match_function(arg_: hydra.gen.core.Term) -> hydra.gen.core.Function | None:
        match hydra.gen.strip.fully_strip_term(arg_):
            case hydra.gen.core.TermFunction(matched_):
                return hydra.lib.optionals.pure(matched_)
            
            case _:
                return None
    return lambda v1: lambda v2: lambda v3: lambda v4: lambda v5: nominal(v1, v2, v3, v4, v5)()(lambda v1: v1.type_name)(lambda v1: v1.cases)(lambda v1: hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(match_function, match_elimination, v1), match_union, v1))

def field(fname: hydra.gen.core.Name, fields: frozenlist[hydra.gen.core.Field]) -> hydra.gen.core.Term | None:
    matches = hydra.lib.lists.filter(lambda f: hydra.lib.equality.equal(f.name, fname), fields)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(1, hydra.lib.lists.length(matches)), hydra.lib.lists.head(matches).term, None)

def float32(v1: hydra.gen.core.Term) -> float | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: float_literal(v1), v1), lambda v1: float32_value(v1), v1)

def float32_value(v1: hydra.gen.core.FloatValue) -> float | None:
    match v1:
        case hydra.gen.core.FloatValueFloat32(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def float64(v1: hydra.gen.core.Term) -> float | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: float_literal(v1), v1), lambda v1: float64_value(v1), v1)

def float64_value(v1: hydra.gen.core.FloatValue) -> float | None:
    match v1:
        case hydra.gen.core.FloatValueFloat64(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def float_literal(v1: hydra.gen.core.Literal) -> hydra.gen.core.FloatValue | None:
    match v1:
        case hydra.gen.core.LiteralFloat(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def int16(v1: hydra.gen.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: int16_value(v1), v1)

def int16_value(v1: hydra.gen.core.IntegerValue) -> int | None:
    match v1:
        case hydra.gen.core.IntegerValueInt16(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def int32(v1: hydra.gen.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: int32_value(v1), v1)

def int32_value(v1: hydra.gen.core.IntegerValue) -> int | None:
    match v1:
        case hydra.gen.core.IntegerValueInt32(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def int64(v1: hydra.gen.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: int64_value(v1), v1)

def int64_value(v1: hydra.gen.core.IntegerValue) -> int | None:
    match v1:
        case hydra.gen.core.IntegerValueInt64(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def int8(v1: hydra.gen.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: int8_value(v1), v1)

def int8_value(v1: hydra.gen.core.IntegerValue) -> int | None:
    match v1:
        case hydra.gen.core.IntegerValueInt8(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def integer_literal(v1: hydra.gen.core.Literal) -> hydra.gen.core.IntegerValue | None:
    match v1:
        case hydra.gen.core.LiteralInteger(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def lambda_(v1: hydra.gen.core.Term) -> hydra.gen.core.Lambda | None:
    def match_lambda(v1: hydra.gen.core.Function) -> hydra.gen.core.Lambda | None:
        match v1:
            case hydra.gen.core.FunctionLambda(matched_):
                return hydra.lib.optionals.pure(matched_)
            
            case _:
                return None
    def match_function(arg_: hydra.gen.core.Term) -> hydra.gen.core.Function | None:
        match hydra.gen.strip.fully_strip_term(arg_):
            case hydra.gen.core.TermFunction(matched_):
                return hydra.lib.optionals.pure(matched_)
            
            case _:
                return None
    return hydra.lib.optionals.compose(match_function, match_lambda, v1)

def let_binding(fname: hydra.gen.core.Name, term: hydra.gen.core.Term) -> hydra.gen.core.LetBinding | None:
    return hydra.lib.optionals.bind(hydra.lib.optionals.map(lambda v1: v1.bindings, let_term(term)), lambda v1: let_binding_with_key(fname, v1))

def let_binding_with_key(fname: hydra.gen.core.Name, bindings: frozenlist[hydra.gen.core.LetBinding]) -> hydra.gen.core.LetBinding | None:
    matches = hydra.lib.lists.filter(lambda b: hydra.lib.equality.equal(b.name, fname), bindings)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(1, hydra.lib.lists.length(matches)), hydra.lib.lists.head(matches), None)

def let_term(arg_: hydra.gen.core.Term) -> hydra.gen.core.Let | None:
    match hydra.gen.strip.fully_strip_term(arg_):
        case hydra.gen.core.TermLet(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def list(arg_: hydra.gen.core.Term) -> frozenlist[hydra.gen.core.Term] | None:
    match hydra.gen.strip.fully_strip_term(arg_):
        case hydra.gen.core.TermList(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def literal(arg_: hydra.gen.core.Term) -> hydra.gen.core.Literal | None:
    match hydra.gen.strip.fully_strip_term(arg_):
        case hydra.gen.core.TermLiteral(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def map(arg_: hydra.gen.core.Term) -> FrozenDict[hydra.gen.core.Term, hydra.gen.core.Term] | None:
    match hydra.gen.strip.fully_strip_term(arg_):
        case hydra.gen.core.TermMap(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def name(term: hydra.gen.core.Term) -> hydra.gen.core.Name | None:
    return hydra.lib.optionals.map(lambda s: hydra.gen.core.Name(s), hydra.lib.optionals.bind(wrap(hydra.gen.core.Name("hydra.gen.core.Name"), term), lambda v1: string(v1)))

nominal = lambda getName: lambda getB: lambda getA: lambda expected: "let terms are not supported here"

def optional(arg_: hydra.gen.core.Term) -> None | None:
    match hydra.gen.strip.fully_strip_term(arg_):
        case hydra.gen.core.TermOptional(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def pair(v1: hydra.gen.core.Term) -> "type = TypeProduct [TypeVariable (Name {unName = \"hydra.gen.core.Term\"}),TypeVariable (Name {unName = \"hydra.gen.core.Term\"})]" | None:
    def match_product(arg_: hydra.gen.core.Term) -> frozenlist[hydra.gen.core.Term] | None:
        match hydra.gen.strip.fully_strip_term(arg_):
            case hydra.gen.core.TermProduct(matched_):
                return hydra.lib.optionals.pure(matched_)
            
            case _:
                return None
    return hydra.lib.optionals.compose(match_product, lambda l: hydra.lib.logic.if_else(hydra.lib.equality.equal(2, hydra.lib.lists.length(l)), (hydra.lib.lists.at(0, l), hydra.lib.lists.at(1, l)), None), v1)

record = lambda v1: lambda v2: lambda v3: lambda v4: lambda v5: nominal(v1, v2, v3, v4, v5)()(lambda v1: v1.type_name)(lambda v1: v1.fields)(lambda arg_: "inline match expressions are unsupported")

def set(arg_: hydra.gen.core.Term) -> frozenset[hydra.gen.core.Term] | None:
    match hydra.gen.strip.fully_strip_term(arg_):
        case hydra.gen.core.TermSet(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def string(v1: hydra.gen.core.Term) -> str | None:
    return hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: string_literal(v1), v1)

def string_literal(v1: hydra.gen.core.Literal) -> str | None:
    match v1:
        case hydra.gen.core.LiteralString(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def uint16(v1: hydra.gen.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: uint16_value(v1), v1)

def uint16_value(v1: hydra.gen.core.IntegerValue) -> int | None:
    match v1:
        case hydra.gen.core.IntegerValueUint16(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def uint32(v1: hydra.gen.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: uint32_value(v1), v1)

def uint32_value(v1: hydra.gen.core.IntegerValue) -> int | None:
    match v1:
        case hydra.gen.core.IntegerValueUint32(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def uint64(v1: hydra.gen.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: uint64_value(v1), v1)

def uint64_value(v1: hydra.gen.core.IntegerValue) -> int | None:
    match v1:
        case hydra.gen.core.IntegerValueUint64(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def uint8(v1: hydra.gen.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: uint8_value(v1), v1)

def uint8_value(v1: hydra.gen.core.IntegerValue) -> int | None:
    match v1:
        case hydra.gen.core.IntegerValueUint8(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def unit(term: hydra.gen.core.Term) -> None | None:
    return hydra.lib.optionals.map(lambda _: hydra.gen.core.Unit(), record(hydra.gen.core.Name("hydra.gen.core.Unit"), term))

def unit_variant(tname: hydra.gen.core.Name, term: hydra.gen.core.Term) -> hydra.gen.core.Name | None:
    return hydra.lib.optionals.map(lambda v1: v1.name, variant(tname, term))

def variable(arg_: hydra.gen.core.Term) -> hydra.gen.core.Name | None:
    match hydra.gen.strip.fully_strip_term(arg_):
        case hydra.gen.core.TermVariable(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

variant = lambda v1: lambda v2: lambda v3: lambda v4: lambda v5: nominal(v1, v2, v3, v4, v5)()(lambda v1: v1.type_name)(lambda v1: v1.field)(lambda arg_: "inline match expressions are unsupported")

wrap = lambda v1: lambda v2: lambda v3: lambda v4: lambda v5: nominal(v1, v2, v3, v4, v5)()(lambda v1: v1.type_name)(lambda v1: v1.object)(lambda arg_: "inline match expressions are unsupported")
