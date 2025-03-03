"""A module for decoding terms to native objects."""

from __future__ import annotations
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.optionals
import hydra.strip

def bigfloat(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(V1))(lambda v1: float_literal(V1))(V3))(lambda v1: bigfloat_value(V1))(V3)

def bigfloat_value(v1):
    match V1:
        case hydra.core.FloatValueBigfloat(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def bigint(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(V1))(lambda v1: integer_literal(V1))(V3))(lambda v1: bigint_value(V1))(V3)

def bigint_value(v1):
    match V1:
        case hydra.core.IntegerValueBigint(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def binary(v3):
    return hydra.lib.optionals.compose(lambda v1: literal(V1))(lambda v1: binary_literal(V1))(V3)

def binary_literal(v1):
    match V1:
        case hydra.core.LiteralBinary(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def boolean(v3):
    return hydra.lib.optionals.compose(lambda v1: literal(V1))(lambda v1: boolean_literal(V1))(V3)

def boolean_literal(v1):
    match V1:
        case hydra.core.LiteralBoolean(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def cases_case(tname, fname, v3):
    return hydra.lib.optionals.compose(lambda v2: cases(Tname)(V2))(lambda v2: field(Fname)(V2))(V3)

def cases(v4, v5):
    def match_union(v1):
        match V1:
            case hydra.core.EliminationUnion(x):
                return hydra.lib.optionals.pure(X)
            
            case _:
                return None
    def match_elimination(v1):
        match V1:
            case hydra.core.FunctionElimination(x):
                return hydra.lib.optionals.pure(X)
            
            case _:
                return None
    def match_function(x):
        match hydra.strip.fully_strip_term(X):
            case hydra.core.TermFunction(x):
                return hydra.lib.optionals.pure(X)
            
            case _:
                return None
    return nominal(lambda v1: V1.type_name)(lambda v1: V1.cases)(lambda v3: hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(MatchFunction)(MatchElimination)(V3))(MatchUnion)(V3))(V4)(V5)

def field(fname, fields):
    matches = hydra.lib.lists.filter(lambda f: hydra.lib.equality.equal(F.name)(Fname))(Fields)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(1)(hydra.lib.lists.length(Matches)))(hydra.lib.lists.head(Matches).term)(None)

def float32(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(V1))(lambda v1: float_literal(V1))(V3))(lambda v1: float32_value(V1))(V3)

def float32_value(v1):
    match V1:
        case hydra.core.FloatValueFloat32(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def float64(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(V1))(lambda v1: float_literal(V1))(V3))(lambda v1: float64_value(V1))(V3)

def float64_value(v1):
    match V1:
        case hydra.core.FloatValueFloat64(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def float_literal(v1):
    match V1:
        case hydra.core.LiteralFloat(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def int16(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(V1))(lambda v1: integer_literal(V1))(V3))(lambda v1: int16_value(V1))(V3)

def int16_value(v1):
    match V1:
        case hydra.core.IntegerValueInt16(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def int32(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(V1))(lambda v1: integer_literal(V1))(V3))(lambda v1: int32_value(V1))(V3)

def int32_value(v1):
    match V1:
        case hydra.core.IntegerValueInt32(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def int64(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(V1))(lambda v1: integer_literal(V1))(V3))(lambda v1: int64_value(V1))(V3)

def int64_value(v1):
    match V1:
        case hydra.core.IntegerValueInt64(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def int8(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(V1))(lambda v1: integer_literal(V1))(V3))(lambda v1: int8_value(V1))(V3)

def int8_value(v1):
    match V1:
        case hydra.core.IntegerValueInt8(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def integer_literal(v1):
    match V1:
        case hydra.core.LiteralInteger(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def lambda_(v3):
    def match_lambda(v1):
        match V1:
            case hydra.core.FunctionLambda(x):
                return hydra.lib.optionals.pure(X)
            
            case _:
                return None
    def match_function(x):
        match hydra.strip.fully_strip_term(X):
            case hydra.core.TermFunction(x):
                return hydra.lib.optionals.pure(X)
            
            case _:
                return None
    return hydra.lib.optionals.compose(MatchFunction)(MatchLambda)(V3)

def let_binding(fname, term):
    return hydra.lib.optionals.bind(hydra.lib.optionals.map(lambda v1: V1.bindings)(let_term(Term)))(lambda v2: let_binding_with_key(Fname)(V2))

def let_binding_with_key(fname, bindings):
    matches = hydra.lib.lists.filter(lambda b: hydra.lib.equality.equal(B.name)(Fname))(Bindings)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(1)(hydra.lib.lists.length(Matches)))(hydra.lib.lists.head(Matches))(None)

def let_term(x):
    match hydra.strip.fully_strip_term(X):
        case hydra.core.TermLet(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def list(x):
    match hydra.strip.fully_strip_term(X):
        case hydra.core.TermList(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def literal(x):
    match hydra.strip.fully_strip_term(X):
        case hydra.core.TermLiteral(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def map(x):
    match hydra.strip.fully_strip_term(X):
        case hydra.core.TermMap(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def name(term):
    return hydra.lib.optionals.map(lambda s: hydra.core.Name(S))(hydra.lib.optionals.bind(wrap(hydra.core.Name("hydra.core.Name"))(Term))(lambda v1: string(V1)))

def nominal(get_name, get_b, get_a, expected, v3):
    return hydra.lib.optionals.compose(GetA)(lambda a: hydra.lib.logic.if_else(hydra.lib.equality.equal(get_name(A))(Expected))(get_b(A))(None))(V3)

def opt_cases(v3):
    def match_optional(v1):
        match V1:
            case hydra.core.EliminationOptional(x):
                return hydra.lib.optionals.pure(X)
            
            case _:
                return None
    def match_elimination(v1):
        match V1:
            case hydra.core.FunctionElimination(x):
                return hydra.lib.optionals.pure(X)
            
            case _:
                return None
    def match_function(x):
        match hydra.strip.fully_strip_term(X):
            case hydra.core.TermFunction(x):
                return hydra.lib.optionals.pure(X)
            
            case _:
                return None
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(MatchFunction)(MatchElimination)(V3))(MatchOptional)(V3)

def opt_cases_just(term):
    return hydra.lib.optionals.map(lambda v1: V1.just)(opt_cases(Term))

def opt_cases_nothing(term):
    return hydra.lib.optionals.map(lambda v1: V1.nothing)(opt_cases(Term))

def optional(x):
    match hydra.strip.fully_strip_term(X):
        case hydra.core.TermOptional(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def pair(v3):
    def match_product(x):
        match hydra.strip.fully_strip_term(X):
            case hydra.core.TermProduct(x):
                return hydra.lib.optionals.pure(X)
            
            case _:
                return None
    return hydra.lib.optionals.compose(MatchProduct)(lambda l: hydra.lib.logic.if_else(hydra.lib.equality.equal(2)(hydra.lib.lists.length(L)))((hydra.lib.lists.at(0)(L), hydra.lib.lists.at(1)(L)))(None))(V3)

def record(v4, v5):
    return nominal(lambda v1: V1.type_name)(lambda v1: V1.fields)(lambda x: "inline match expressions are unsupported")(V4)(V5)

def set(x):
    match hydra.strip.fully_strip_term(X):
        case hydra.core.TermSet(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def string(v3):
    return hydra.lib.optionals.compose(lambda v1: literal(V1))(lambda v1: string_literal(V1))(V3)

def string_literal(v1):
    match V1:
        case hydra.core.LiteralString(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def uint16(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(V1))(lambda v1: integer_literal(V1))(V3))(lambda v1: uint16_value(V1))(V3)

def uint16_value(v1):
    match V1:
        case hydra.core.IntegerValueUint16(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def uint32(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(V1))(lambda v1: integer_literal(V1))(V3))(lambda v1: uint32_value(V1))(V3)

def uint32_value(v1):
    match V1:
        case hydra.core.IntegerValueUint32(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def uint64(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(V1))(lambda v1: integer_literal(V1))(V3))(lambda v1: uint64_value(V1))(V3)

def uint64_value(v1):
    match V1:
        case hydra.core.IntegerValueUint64(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def uint8(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(V1))(lambda v1: integer_literal(V1))(V3))(lambda v1: uint8_value(V1))(V3)

def uint8_value(v1):
    match V1:
        case hydra.core.IntegerValueUint8(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def unit(term):
    return hydra.lib.optionals.map(lambda _: hydra.core.Unit())(record(hydra.core.Name("hydra.core.Unit"))(Term))

def unit_variant(tname, term):
    return hydra.lib.optionals.map(lambda v1: V1.name)(variant(Tname)(Term))

def variable(x):
    match hydra.strip.fully_strip_term(X):
        case hydra.core.TermVariable(x):
            return hydra.lib.optionals.pure(X)
        
        case _:
            return None

def variant(v4, v5):
    return nominal(lambda v1: V1.type_name)(lambda v1: V1.field)(lambda x: "inline match expressions are unsupported")(V4)(V5)

def wrap(v4, v5):
    return nominal(lambda v1: V1.type_name)(lambda v1: V1.object)(lambda x: "inline match expressions are unsupported")(V4)(V5)