"""A module for decoding terms to native objects."""

from __future__ import annotations
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.optionals
import hydra.strip

def bigfloat(v3: hydra.core.Term) -> float | None:
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: float_literal(v1))(v3))(lambda v1: bigfloat_value(v1))(v3)

def bigfloat_value(v1: hydra.core.FloatValue) -> float | None:
    match v1:
        case hydra.core.FloatValueBigfloat(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def bigint(v3: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integer_literal(v1))(v3))(lambda v1: bigint_value(v1))(v3)

def bigint_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueBigint(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def binary(v3: hydra.core.Term) -> bytes | None:
    return hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: binary_literal(v1))(v3)

def binary_literal(v1: hydra.core.Literal) -> bytes | None:
    match v1:
        case hydra.core.LiteralBinary(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def boolean(v3: hydra.core.Term) -> bool | None:
    return hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: boolean_literal(v1))(v3)

def boolean_literal(v1: hydra.core.Literal) -> bool | None:
    match v1:
        case hydra.core.LiteralBoolean(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def cases_case(tname: hydra.core.Name, fname: hydra.core.Name, v3: hydra.core.Term) -> hydra.core.Term | None:
    return hydra.lib.optionals.compose(lambda v2: cases(tname)(v2))(lambda v2: field(fname)(v2))(v3)

def cases(v4: hydra.core.Name, v5: hydra.core.Term) -> frozenlist[hydra.core.Field] | None:
    def match_union(v1: hydra.core.Elimination) -> T760:
        match v1:
            case hydra.core.EliminationUnion(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    def match_elimination(v1: hydra.core.Function) -> T755:
        match v1:
            case hydra.core.FunctionElimination(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    def match_function(x: T743) -> T750:
        match hydra.strip.fully_strip_term(x):
            case hydra.core.TermFunction(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    return nominal(lambda v1: v1.type_name)(lambda v1: v1.cases)(lambda v3: hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(match_function)(match_elimination)(v3))(match_union)(v3))(v4)(v5)

def field(fname: hydra.core.Name, fields: frozenlist[hydra.core.Field]) -> hydra.core.Term | None:
    matches = hydra.lib.lists.filter(lambda f: hydra.lib.equality.equal(f.name)(fname))(fields)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(1)(hydra.lib.lists.length(matches)))(hydra.lib.lists.head(matches).term)(None)

def float32(v3: hydra.core.Term) -> float | None:
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: float_literal(v1))(v3))(lambda v1: float32_value(v1))(v3)

def float32_value(v1: hydra.core.FloatValue) -> float | None:
    match v1:
        case hydra.core.FloatValueFloat32(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def float64(v3: hydra.core.Term) -> float | None:
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: float_literal(v1))(v3))(lambda v1: float64_value(v1))(v3)

def float64_value(v1: hydra.core.FloatValue) -> float | None:
    match v1:
        case hydra.core.FloatValueFloat64(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def float_literal(v1: hydra.core.Literal) -> hydra.core.FloatValue | None:
    match v1:
        case hydra.core.LiteralFloat(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def int16(v3: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integer_literal(v1))(v3))(lambda v1: int16_value(v1))(v3)

def int16_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueInt16(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def int32(v3: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integer_literal(v1))(v3))(lambda v1: int32_value(v1))(v3)

def int32_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueInt32(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def int64(v3: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integer_literal(v1))(v3))(lambda v1: int64_value(v1))(v3)

def int64_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueInt64(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def int8(v3: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integer_literal(v1))(v3))(lambda v1: int8_value(v1))(v3)

def int8_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueInt8(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def integer_literal(v1: hydra.core.Literal) -> hydra.core.IntegerValue | None:
    match v1:
        case hydra.core.LiteralInteger(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def lambda_(v3: hydra.core.Term) -> hydra.core.Lambda | None:
    def match_lambda(v1: hydra.core.Function) -> T606:
        match v1:
            case hydra.core.FunctionLambda(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    def match_function(x: T594) -> T601:
        match hydra.strip.fully_strip_term(x):
            case hydra.core.TermFunction(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    return hydra.lib.optionals.compose(match_function)(match_lambda)(v3)

def let_binding(fname: hydra.core.Name, term: hydra.core.Term) -> hydra.core.LetBinding | None:
    return hydra.lib.optionals.bind(hydra.lib.optionals.map(lambda v1: v1.bindings)(let_term(term)))(lambda v2: let_binding_with_key(fname)(v2))

def let_binding_with_key(fname: hydra.core.Name, bindings: frozenlist[hydra.core.LetBinding]) -> hydra.core.LetBinding | None:
    matches = hydra.lib.lists.filter(lambda b: hydra.lib.equality.equal(b.name)(fname))(bindings)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(1)(hydra.lib.lists.length(matches)))(hydra.lib.lists.head(matches))(None)

def let_term(x: hydra.core.Term) -> hydra.core.Let | None:
    match hydra.strip.fully_strip_term(x):
        case hydra.core.TermLet(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def list(x: hydra.core.Term) -> frozenlist[hydra.core.Term] | None:
    match hydra.strip.fully_strip_term(x):
        case hydra.core.TermList(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def literal(x: hydra.core.Term) -> hydra.core.Literal | None:
    match hydra.strip.fully_strip_term(x):
        case hydra.core.TermLiteral(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def map(x: hydra.core.Term) -> FrozenDict[hydra.core.Term, hydra.core.Term] | None:
    match hydra.strip.fully_strip_term(x):
        case hydra.core.TermMap(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def name(term: hydra.core.Term) -> hydra.core.Name | None:
    return hydra.lib.optionals.map(lambda s: hydra.core.Name(s))(hydra.lib.optionals.bind(wrap(hydra.core.Name("hydra.core.Name"))(term))(lambda v1: string(v1)))

def nominal(get_name: Callable[[A], hydra.core.Name], get_b: Callable[[A], B], get_a: Callable[[C], A | None], expected: hydra.core.Name, v3: C) -> B | None:
    return hydra.lib.optionals.compose(get_a)(lambda a: hydra.lib.logic.if_else(hydra.lib.equality.equal(get_name(a))(expected))(get_b(a))(None))(v3)

def opt_cases(v3: hydra.core.Term) -> hydra.core.OptionalCases | None:
    def match_optional(v1: hydra.core.Elimination) -> T486:
        match v1:
            case hydra.core.EliminationOptional(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    def match_elimination(v1: hydra.core.Function) -> T481:
        match v1:
            case hydra.core.FunctionElimination(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    def match_function(x: T469) -> T476:
        match hydra.strip.fully_strip_term(x):
            case hydra.core.TermFunction(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(match_function)(match_elimination)(v3))(match_optional)(v3)

def opt_cases_just(term: hydra.core.Term) -> hydra.core.Term | None:
    return hydra.lib.optionals.map(lambda v1: v1.just)(opt_cases(term))

def opt_cases_nothing(term: hydra.core.Term) -> hydra.core.Term | None:
    return hydra.lib.optionals.map(lambda v1: v1.nothing)(opt_cases(term))

def optional(x: hydra.core.Term) -> None | None:
    match hydra.strip.fully_strip_term(x):
        case hydra.core.TermOptional(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def pair(v3: hydra.core.Term) -> "type = TypeProduct [TypeVariable (Name {unName = \"hydra.core.Term\"}),TypeVariable (Name {unName = \"hydra.core.Term\"})]" | None:
    def match_product(x: T419) -> T426:
        match hydra.strip.fully_strip_term(x):
            case hydra.core.TermProduct(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    return hydra.lib.optionals.compose(match_product)(lambda l: hydra.lib.logic.if_else(hydra.lib.equality.equal(2)(hydra.lib.lists.length(l)))((hydra.lib.lists.at(0)(l), hydra.lib.lists.at(1)(l)))(None))(v3)

def record(v4: hydra.core.Name, v5: hydra.core.Term) -> frozenlist[hydra.core.Field] | None:
    return nominal(lambda v1: v1.type_name)(lambda v1: v1.fields)(lambda x: "inline match expressions are unsupported")(v4)(v5)

def set(x: hydra.core.Term) -> frozenset[hydra.core.Term] | None:
    match hydra.strip.fully_strip_term(x):
        case hydra.core.TermSet(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def string(v3: hydra.core.Term) -> str | None:
    return hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: string_literal(v1))(v3)

def string_literal(v1: hydra.core.Literal) -> str | None:
    match v1:
        case hydra.core.LiteralString(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def uint16(v3: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integer_literal(v1))(v3))(lambda v1: uint16_value(v1))(v3)

def uint16_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueUint16(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def uint32(v3: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integer_literal(v1))(v3))(lambda v1: uint32_value(v1))(v3)

def uint32_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueUint32(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def uint64(v3: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integer_literal(v1))(v3))(lambda v1: uint64_value(v1))(v3)

def uint64_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueUint64(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def uint8(v3: hydra.core.Term) -> int | None:
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integer_literal(v1))(v3))(lambda v1: uint8_value(v1))(v3)

def uint8_value(v1: hydra.core.IntegerValue) -> int | None:
    match v1:
        case hydra.core.IntegerValueUint8(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def unit(term: hydra.core.Term) -> None | None:
    return hydra.lib.optionals.map(lambda _: hydra.core.Unit())(record(hydra.core.Name("hydra.core.Unit"))(term))

def unit_variant(tname: hydra.core.Name, term: hydra.core.Term) -> hydra.core.Name | None:
    return hydra.lib.optionals.map(lambda v1: v1.name)(variant(tname)(term))

def variable(x: hydra.core.Term) -> hydra.core.Name | None:
    match hydra.strip.fully_strip_term(x):
        case hydra.core.TermVariable(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def variant(v4: hydra.core.Name, v5: hydra.core.Term) -> hydra.core.Field | None:
    return nominal(lambda v1: v1.type_name)(lambda v1: v1.field)(lambda x: "inline match expressions are unsupported")(v4)(v5)

def wrap(v4: hydra.core.Name, v5: hydra.core.Term) -> hydra.core.Term | None:
    return nominal(lambda v1: v1.type_name)(lambda v1: v1.object)(lambda x: "inline match expressions are unsupported")(v4)(v5)