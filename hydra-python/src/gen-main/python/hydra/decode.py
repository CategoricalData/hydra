"""A module for decoding terms to native objects."""

from __future__ import annotations
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.optionals
import hydra.strip

def bigfloat(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: floatLiteral(v1))(v3))(lambda v1: bigfloatValue(v1))(v3)

def bigfloatValue(v1):
    match v1:
        case hydra.core.FloatValueBigfloat(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def bigint(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integerLiteral(v1))(v3))(lambda v1: bigintValue(v1))(v3)

def bigintValue(v1):
    match v1:
        case hydra.core.IntegerValueBigint(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def binary(v3):
    return hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: binaryLiteral(v1))(v3)

def binaryLiteral(v1):
    match v1:
        case hydra.core.LiteralBinary(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def boolean(v3):
    return hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: booleanLiteral(v1))(v3)

def booleanLiteral(v1):
    match v1:
        case hydra.core.LiteralBoolean(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def casesCase(tname, fname, v3):
    return hydra.lib.optionals.compose(lambda v2: cases(tname)(v2))(lambda v2: field(fname)(v2))(v3)

def cases(v4, v5):
    def matchUnion(v1):
        match v1:
            case hydra.core.EliminationUnion(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    def matchElimination(v1):
        match v1:
            case hydra.core.FunctionElimination(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    def matchFunction(x):
        match hydra.strip.fullyStripTerm(x):
            case hydra.core.TermFunction(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    return nominal(lambda v1: v1.type_name)(lambda v1: v1.cases)(lambda v3: hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(matchFunction)(matchElimination)(v3))(matchUnion)(v3))(v4)(v5)

def field(fname, fields):
    matches = hydra.lib.lists.filter(lambda f: hydra.lib.equality.equal(f.name)(fname))(fields)
    return hydra.lib.logic.ifElse(hydra.lib.equality.equal(1)(hydra.lib.lists.length(matches)))(hydra.lib.lists.head(matches).term)(None)

def float32(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: floatLiteral(v1))(v3))(lambda v1: float32Value(v1))(v3)

def float32Value(v1):
    match v1:
        case hydra.core.FloatValueFloat32(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def float64(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: floatLiteral(v1))(v3))(lambda v1: float64Value(v1))(v3)

def float64Value(v1):
    match v1:
        case hydra.core.FloatValueFloat64(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def floatLiteral(v1):
    match v1:
        case hydra.core.LiteralFloat(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def int16(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integerLiteral(v1))(v3))(lambda v1: int16Value(v1))(v3)

def int16Value(v1):
    match v1:
        case hydra.core.IntegerValueInt16(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def int32(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integerLiteral(v1))(v3))(lambda v1: int32Value(v1))(v3)

def int32Value(v1):
    match v1:
        case hydra.core.IntegerValueInt32(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def int64(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integerLiteral(v1))(v3))(lambda v1: int64Value(v1))(v3)

def int64Value(v1):
    match v1:
        case hydra.core.IntegerValueInt64(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def int8(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integerLiteral(v1))(v3))(lambda v1: int8Value(v1))(v3)

def int8Value(v1):
    match v1:
        case hydra.core.IntegerValueInt8(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def integerLiteral(v1):
    match v1:
        case hydra.core.LiteralInteger(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def lambda_(v3):
    def matchLambda(v1):
        match v1:
            case hydra.core.FunctionLambda(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    def matchFunction(x):
        match hydra.strip.fullyStripTerm(x):
            case hydra.core.TermFunction(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    return hydra.lib.optionals.compose(matchFunction)(matchLambda)(v3)

def letBinding(fname, term):
    return hydra.lib.optionals.bind(hydra.lib.optionals.map(lambda v1: v1.bindings)(letTerm(term)))(lambda v2: letBindingWithKey(fname)(v2))

def letBindingWithKey(fname, bindings):
    matches = hydra.lib.lists.filter(lambda b: hydra.lib.equality.equal(b.name)(fname))(bindings)
    return hydra.lib.logic.ifElse(hydra.lib.equality.equal(1)(hydra.lib.lists.length(matches)))(hydra.lib.lists.head(matches))(None)

def letTerm(x):
    match hydra.strip.fullyStripTerm(x):
        case hydra.core.TermLet(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def list(x):
    match hydra.strip.fullyStripTerm(x):
        case hydra.core.TermList(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def literal(x):
    match hydra.strip.fullyStripTerm(x):
        case hydra.core.TermLiteral(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def map(x):
    match hydra.strip.fullyStripTerm(x):
        case hydra.core.TermMap(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def name(term):
    return hydra.lib.optionals.map(lambda s: hydra.core.Name(s))(hydra.lib.optionals.bind(wrap(hydra.core.Name("hydra.core.Name"))(term))(lambda v1: string(v1)))

def nominal(getName, getB, getA, expected, v3):
    return hydra.lib.optionals.compose(getA)(lambda a: hydra.lib.logic.ifElse(hydra.lib.equality.equal(getName(a))(expected))(getB(a))(None))(v3)

def optCases(v3):
    def matchOptional(v1):
        match v1:
            case hydra.core.EliminationOptional(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    def matchElimination(v1):
        match v1:
            case hydra.core.FunctionElimination(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    def matchFunction(x):
        match hydra.strip.fullyStripTerm(x):
            case hydra.core.TermFunction(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(matchFunction)(matchElimination)(v3))(matchOptional)(v3)

def optCasesJust(term):
    return hydra.lib.optionals.map(lambda v1: v1.just)(optCases(term))

def optCasesNothing(term):
    return hydra.lib.optionals.map(lambda v1: v1.nothing)(optCases(term))

def optional(x):
    match hydra.strip.fullyStripTerm(x):
        case hydra.core.TermOptional(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def pair(v3):
    def matchProduct(x):
        match hydra.strip.fullyStripTerm(x):
            case hydra.core.TermProduct(x):
                return hydra.lib.optionals.pure(x)
            
            case _:
                return None
    return hydra.lib.optionals.compose(matchProduct)(lambda l: hydra.lib.logic.ifElse(hydra.lib.equality.equal(2)(hydra.lib.lists.length(l)))((hydra.lib.lists.at(0)(l), hydra.lib.lists.at(1)(l)))(None))(v3)

def record(v4, v5):
    return nominal(lambda v1: v1.type_name)(lambda v1: v1.fields)(lambda x: "inline match expressions are unsupported")(v4)(v5)

def set(x):
    match hydra.strip.fullyStripTerm(x):
        case hydra.core.TermSet(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def string(v3):
    return hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: stringLiteral(v1))(v3)

def stringLiteral(v1):
    match v1:
        case hydra.core.LiteralString(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def uint16(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integerLiteral(v1))(v3))(lambda v1: uint16Value(v1))(v3)

def uint16Value(v1):
    match v1:
        case hydra.core.IntegerValueUint16(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def uint32(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integerLiteral(v1))(v3))(lambda v1: uint32Value(v1))(v3)

def uint32Value(v1):
    match v1:
        case hydra.core.IntegerValueUint32(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def uint64(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integerLiteral(v1))(v3))(lambda v1: uint64Value(v1))(v3)

def uint64Value(v1):
    match v1:
        case hydra.core.IntegerValueUint64(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def uint8(v3):
    return hydra.lib.optionals.compose(lambda v3: hydra.lib.optionals.compose(lambda v1: literal(v1))(lambda v1: integerLiteral(v1))(v3))(lambda v1: uint8Value(v1))(v3)

def uint8Value(v1):
    match v1:
        case hydra.core.IntegerValueUint8(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def unit(term):
    return hydra.lib.optionals.map(lambda _: hydra.core.Unit())(record(hydra.core.Name("hydra.core.Unit"))(term))

def unitVariant(tname, term):
    return hydra.lib.optionals.map(lambda v1: v1.name)(variant(tname)(term))

def variable(x):
    match hydra.strip.fullyStripTerm(x):
        case hydra.core.TermVariable(x):
            return hydra.lib.optionals.pure(x)
        
        case _:
            return None

def variant(v4, v5):
    return nominal(lambda v1: v1.type_name)(lambda v1: v1.field)(lambda x: "inline match expressions are unsupported")(v4)(v5)

def wrap(v4, v5):
    return nominal(lambda v1: v1.type_name)(lambda v1: v1.object)(lambda x: "inline match expressions are unsupported")(v4)(v5)