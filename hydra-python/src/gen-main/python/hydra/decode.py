"""A module for decoding terms to native objects."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import frozenlist
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.optionals
import hydra.strip

def bigfloat(v1: hydra.core.Term) -> float | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: float_literal(v1), v1), lambda v1: bigfloat_value(v1), v1)

def bigfloat_value(v1: hydra.core.FloatValue) -> float | None:
    match v1:
        case hydra.core.FloatValueBigfloat(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def bigint(v1: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: bigint_value(v1), v1)

def bigint_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueBigint(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def binary(v1: hydra.core.Term) -> bytes | None:
    return hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: binary_literal(v1), v1)

def binary_literal(v1: hydra.core.Literal) -> bytes | None:
    match v1:
        case hydra.core.LiteralBinary(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def boolean(v1: hydra.core.Term) -> bool | None:
    return hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: boolean_literal(v1), v1)

def boolean_literal(v1: hydra.core.Literal) -> bool | None:
    match v1:
        case hydra.core.LiteralBoolean(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def cases_case(tname: hydra.core.Name, fname: hydra.core.Name, v1: hydra.core.Term) -> hydra.core.Term | None:
    return hydra.lib.optionals.compose(lambda v1: cases(tname, v1), lambda v1: field(fname, v1), v1)

def cases() -> Callable[[hydra.core.Name, hydra.core.Term], frozenlist[hydra.core.Field] | None]:
    def match_union(v1: hydra.core.Elimination) -> hydra.core.CaseStatement | None:
        match v1:
            case hydra.core.EliminationUnion(matched_):
                return hydra.lib.optionals.pure(matched_)
            
            case _:
                return None
    def match_elimination(v1: hydra.core.Function) -> hydra.core.Elimination | None:
        match v1:
            case hydra.core.FunctionElimination(matched_):
                return hydra.lib.optionals.pure(matched_)
            
            case _:
                return None
    def match_function(arg_: hydra.core.Term) -> hydra.core.Function | None:
        match hydra.strip.fully_strip_term(arg_):
            case hydra.core.TermFunction(matched_):
                return hydra.lib.optionals.pure(matched_)
            
            case _:
                return None
    return lambda v1: lambda v2: lambda v3: lambda v4: lambda v5: nominal(v1, v2, v3, v4, v5)()(lambda v1: v1.type_name)(lambda v1: v1.cases)(lambda v1: hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(match_function, match_elimination, v1), match_union, v1))

def field(fname: hydra.core.Name, fields: frozenlist[hydra.core.Field]) -> hydra.core.Term | None:
    matches = hydra.lib.lists.filter(lambda f: hydra.lib.equality.equal(f.name, fname), fields)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(1, hydra.lib.lists.length(matches)), hydra.lib.lists.head(matches).term, None)

def float32(v1: hydra.core.Term) -> float | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: float_literal(v1), v1), lambda v1: float32_value(v1), v1)

def float32_value(v1: hydra.core.FloatValue) -> float | None:
    match v1:
        case hydra.core.FloatValueFloat32(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def float64(v1: hydra.core.Term) -> float | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: float_literal(v1), v1), lambda v1: float64_value(v1), v1)

def float64_value(v1: hydra.core.FloatValue) -> float | None:
    match v1:
        case hydra.core.FloatValueFloat64(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def float_literal(v1: hydra.core.Literal) -> hydra.core.FloatValue | None:
    match v1:
        case hydra.core.LiteralFloat(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def int16(v1: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: int16_value(v1), v1)

def int16_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueInt16(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def int32(v1: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: int32_value(v1), v1)

def int32_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueInt32(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def int64(v1: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: int64_value(v1), v1)

def int64_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueInt64(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def int8(v1: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: int8_value(v1), v1)

def int8_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueInt8(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def integer_literal(v1: hydra.core.Literal) -> hydra.core.IntegerValue | None:
    match v1:
        case hydra.core.LiteralInteger(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def lambda_(v1: hydra.core.Term) -> hydra.core.Lambda | None:
    def match_lambda(v1: hydra.core.Function) -> hydra.core.Lambda | None:
        match v1:
            case hydra.core.FunctionLambda(matched_):
                return hydra.lib.optionals.pure(matched_)
            
            case _:
                return None
    def match_function(arg_: hydra.core.Term) -> hydra.core.Function | None:
        match hydra.strip.fully_strip_term(arg_):
            case hydra.core.TermFunction(matched_):
                return hydra.lib.optionals.pure(matched_)
            
            case _:
                return None
    return hydra.lib.optionals.compose(match_function, match_lambda, v1)

def let_binding(fname: hydra.core.Name, term: hydra.core.Term) -> hydra.core.LetBinding | None:
    return hydra.lib.optionals.bind(hydra.lib.optionals.map(lambda v1: v1.bindings, let_term(term)), lambda v1: let_binding_with_key(fname, v1))

def let_binding_with_key(fname: hydra.core.Name, bindings: frozenlist[hydra.core.LetBinding]) -> hydra.core.LetBinding | None:
    matches = hydra.lib.lists.filter(lambda b: hydra.lib.equality.equal(b.name, fname), bindings)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(1, hydra.lib.lists.length(matches)), hydra.lib.lists.head(matches), None)

def let_term(arg_: hydra.core.Term) -> hydra.core.Let | None:
    match hydra.strip.fully_strip_term(arg_):
        case hydra.core.TermLet(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def list(arg_: hydra.core.Term) -> frozenlist[hydra.core.Term] | None:
    match hydra.strip.fully_strip_term(arg_):
        case hydra.core.TermList(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def literal(arg_: hydra.core.Term) -> hydra.core.Literal | None:
    match hydra.strip.fully_strip_term(arg_):
        case hydra.core.TermLiteral(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def map(arg_: hydra.core.Term) -> FrozenDict[hydra.core.Term, hydra.core.Term] | None:
    match hydra.strip.fully_strip_term(arg_):
        case hydra.core.TermMap(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def name(term: hydra.core.Term) -> hydra.core.Name | None:
    return hydra.lib.optionals.map(lambda s: hydra.core.Name(s), hydra.lib.optionals.bind(wrap(hydra.core.Name("hydra.core.Name"), term), lambda v1: string(v1)))

nominal = lambda getName: lambda getB: lambda getA: lambda expected: "let terms are not supported here"

def opt_cases(v1: hydra.core.Term) -> hydra.core.OptionalCases | None:
    def match_optional(v1: hydra.core.Elimination) -> hydra.core.OptionalCases | None:
        match v1:
            case hydra.core.EliminationOptional(matched_):
                return hydra.lib.optionals.pure(matched_)
            
            case _:
                return None
    def match_elimination(v1: hydra.core.Function) -> hydra.core.Elimination | None:
        match v1:
            case hydra.core.FunctionElimination(matched_):
                return hydra.lib.optionals.pure(matched_)
            
            case _:
                return None
    def match_function(arg_: hydra.core.Term) -> hydra.core.Function | None:
        match hydra.strip.fully_strip_term(arg_):
            case hydra.core.TermFunction(matched_):
                return hydra.lib.optionals.pure(matched_)
            
            case _:
                return None
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(match_function, match_elimination, v1), match_optional, v1)

def opt_cases_just(term: hydra.core.Term) -> hydra.core.Term | None:
    return hydra.lib.optionals.map(lambda v1: v1.just, opt_cases(term))

def opt_cases_nothing(term: hydra.core.Term) -> hydra.core.Term | None:
    return hydra.lib.optionals.map(lambda v1: v1.nothing, opt_cases(term))

def optional(arg_: hydra.core.Term) -> None | None:
    match hydra.strip.fully_strip_term(arg_):
        case hydra.core.TermOptional(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def pair(v1: hydra.core.Term) -> "type = TypeProduct [TypeVariable (Name {unName = \"hydra.core.Term\"}),TypeVariable (Name {unName = \"hydra.core.Term\"})]" | None:
    def match_product(arg_: hydra.core.Term) -> frozenlist[hydra.core.Term] | None:
        match hydra.strip.fully_strip_term(arg_):
            case hydra.core.TermProduct(matched_):
                return hydra.lib.optionals.pure(matched_)
            
            case _:
                return None
    return hydra.lib.optionals.compose(match_product, lambda l: hydra.lib.logic.if_else(hydra.lib.equality.equal(2, hydra.lib.lists.length(l)), (hydra.lib.lists.at(0, l), hydra.lib.lists.at(1, l)), None), v1)

record = lambda v1: lambda v2: lambda v3: lambda v4: lambda v5: nominal(v1, v2, v3, v4, v5)()(lambda v1: v1.type_name)(lambda v1: v1.fields)(lambda arg_: "inline match expressions are unsupported")

def set(arg_: hydra.core.Term) -> frozenset[hydra.core.Term] | None:
    match hydra.strip.fully_strip_term(arg_):
        case hydra.core.TermSet(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def string(v1: hydra.core.Term) -> str | None:
    return hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: string_literal(v1), v1)

def string_literal(v1: hydra.core.Literal) -> str | None:
    match v1:
        case hydra.core.LiteralString(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def uint16(v1: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: uint16_value(v1), v1)

def uint16_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueUint16(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def uint32(v1: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: uint32_value(v1), v1)

def uint32_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueUint32(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def uint64(v1: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: uint64_value(v1), v1)

def uint64_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueUint64(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def uint8(v1: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v1: hydra.lib.optionals.compose(lambda v1: literal(v1), lambda v1: integer_literal(v1), v1), lambda v1: uint8_value(v1), v1)

def uint8_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueUint8(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

def unit(term: hydra.core.Term) -> None | None:
    return hydra.lib.optionals.map(lambda _: hydra.core.Unit(), record(hydra.core.Name("hydra.core.Unit"), term))

def unit_variant(tname: hydra.core.Name, term: hydra.core.Term) -> hydra.core.Name | None:
    return hydra.lib.optionals.map(lambda v1: v1.name, variant(tname, term))

def variable(arg_: hydra.core.Term) -> hydra.core.Name | None:
    match hydra.strip.fully_strip_term(arg_):
        case hydra.core.TermVariable(matched_):
            return hydra.lib.optionals.pure(matched_)
        
        case _:
            return None

variant = lambda v1: lambda v2: lambda v3: lambda v4: lambda v5: nominal(v1, v2, v3, v4, v5)()(lambda v1: v1.type_name)(lambda v1: v1.field)(lambda arg_: "inline match expressions are unsupported")

wrap = lambda v1: lambda v2: lambda v3: lambda v4: lambda v5: nominal(v1, v2, v3, v4, v5)()(lambda v1: v1.type_name)(lambda v1: v1.object)(lambda arg_: "inline match expressions are unsupported")
