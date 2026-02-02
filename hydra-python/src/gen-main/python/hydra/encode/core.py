# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.core."""

from __future__ import annotations
from decimal import Decimal
from hydra.dsl.python import Either, FrozenDict, Maybe, frozenlist
from typing import cast
import hydra.core
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets

def name(x: hydra.core.Name) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def projection(x: hydra.core.Projection) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Projection"), (hydra.core.Field(hydra.core.Name("typeName"), name(x.type_name)), hydra.core.Field(hydra.core.Name("field"), name(x.field))))))

def float_type(v1: hydra.core.FloatType) -> hydra.core.Type:
    match v1:
        case hydra.core.FloatType.BIGFLOAT:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("bigfloat"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.FloatType.FLOAT32:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float32"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.FloatType.FLOAT64:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float64"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def integer_type(v1: hydra.core.IntegerType) -> hydra.core.Type:
    match v1:
        case hydra.core.IntegerType.BIGINT:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("bigint"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.IntegerType.INT8:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int8"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.IntegerType.INT16:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int16"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.IntegerType.INT32:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int32"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.IntegerType.INT64:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int64"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.IntegerType.UINT8:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint8"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.IntegerType.UINT16:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint16"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.IntegerType.UINT32:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint32"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.IntegerType.UINT64:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint64"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def literal_type(v1: hydra.core.LiteralType) -> hydra.core.Type:
    match v1:
        case hydra.core.LiteralTypeBinary():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("binary"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.LiteralTypeBoolean():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("boolean"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.LiteralTypeFloat(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("float"), float_type(y3)))))
        
        case hydra.core.LiteralTypeInteger(value=y4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("integer"), integer_type(y4)))))
        
        case hydra.core.LiteralTypeString():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("string"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def type_variable_metadata(x: hydra.core.TypeVariableMetadata) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeVariableMetadata"), (hydra.core.Field(hydra.core.Name("classes"), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map(name, x.classes)))),))))

def float_value(v1: hydra.core.FloatValue) -> hydra.core.Type:
    match v1:
        case hydra.core.FloatValueBigfloat(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("bigfloat"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueBigfloat(y))))))))))
        
        case hydra.core.FloatValueFloat32(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float32"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat32(y2))))))))))
        
        case hydra.core.FloatValueFloat64(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float64"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat64(y3))))))))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def integer_value(v1: hydra.core.IntegerValue) -> hydra.core.Type:
    match v1:
        case hydra.core.IntegerValueBigint(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("bigint"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueBigint(y))))))))))
        
        case hydra.core.IntegerValueInt8(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int8"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt8(y2))))))))))
        
        case hydra.core.IntegerValueInt16(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int16"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt16(y3))))))))))
        
        case hydra.core.IntegerValueInt32(value=y4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int32"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(y4))))))))))
        
        case hydra.core.IntegerValueInt64(value=y5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int64"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(y5))))))))))
        
        case hydra.core.IntegerValueUint8(value=y6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint8"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint8(y6))))))))))
        
        case hydra.core.IntegerValueUint16(value=y7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint16"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint16(y7))))))))))
        
        case hydra.core.IntegerValueUint32(value=y8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint32"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint32(y8))))))))))
        
        case hydra.core.IntegerValueUint64(value=y9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint64"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint64(y9))))))))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def literal(v1: hydra.core.Literal) -> hydra.core.Type:
    match v1:
        case hydra.core.LiteralBinary(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("binary"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBinary(y))))))))
        
        case hydra.core.LiteralBoolean(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("boolean"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(y2))))))))
        
        case hydra.core.LiteralFloat(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("float"), float_value(y3)))))
        
        case hydra.core.LiteralInteger(value=y4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("integer"), integer_value(y4)))))
        
        case hydra.core.LiteralString(value=y5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("string"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(y5))))))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def annotated_term(x: hydra.core.AnnotatedTerm) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.AnnotatedTerm"), (hydra.core.Field(hydra.core.Name("body"), term(x.body)), hydra.core.Field(hydra.core.Name("annotation"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(name, term, x.annotation))))))))

def annotated_type(x: hydra.core.AnnotatedType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.AnnotatedType"), (hydra.core.Field(hydra.core.Name("body"), type(x.body)), hydra.core.Field(hydra.core.Name("annotation"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(name, term, x.annotation))))))))

def application(x: hydra.core.Application) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Application"), (hydra.core.Field(hydra.core.Name("function"), term(x.function)), hydra.core.Field(hydra.core.Name("argument"), term(x.argument))))))

def application_type(x: hydra.core.ApplicationType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ApplicationType"), (hydra.core.Field(hydra.core.Name("function"), type(x.function)), hydra.core.Field(hydra.core.Name("argument"), type(x.argument))))))

def binding(x: hydra.core.Binding) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Binding"), (hydra.core.Field(hydra.core.Name("name"), name(x.name)), hydra.core.Field(hydra.core.Name("term"), term(x.term)), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(type_scheme, x.type))))))))

def case_statement(x: hydra.core.CaseStatement) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.CaseStatement"), (hydra.core.Field(hydra.core.Name("typeName"), name(x.type_name)), hydra.core.Field(hydra.core.Name("default"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(term, x.default)))), hydra.core.Field(hydra.core.Name("cases"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(field, x.cases))))))))

def either_type(x: hydra.core.EitherType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.EitherType"), (hydra.core.Field(hydra.core.Name("left"), type(x.left)), hydra.core.Field(hydra.core.Name("right"), type(x.right))))))

def elimination(v1: hydra.core.Elimination) -> hydra.core.Type:
    match v1:
        case hydra.core.EliminationRecord(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("record"), projection(y)))))
        
        case hydra.core.EliminationUnion(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("union"), case_statement(y2)))))
        
        case hydra.core.EliminationWrap(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("wrap"), name(y3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def field(x: hydra.core.Field) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Field"), (hydra.core.Field(hydra.core.Name("name"), name(x.name)), hydra.core.Field(hydra.core.Name("term"), term(x.term))))))

def field_type(x: hydra.core.FieldType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FieldType"), (hydra.core.Field(hydra.core.Name("name"), name(x.name)), hydra.core.Field(hydra.core.Name("type"), type(x.type))))))

def forall_type(x: hydra.core.ForallType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ForallType"), (hydra.core.Field(hydra.core.Name("parameter"), name(x.parameter)), hydra.core.Field(hydra.core.Name("body"), type(x.body))))))

def function(v1: hydra.core.Function) -> hydra.core.Type:
    match v1:
        case hydra.core.FunctionElimination(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("elimination"), elimination(y)))))
        
        case hydra.core.FunctionLambda(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("lambda"), lambda_(y2)))))
        
        case hydra.core.FunctionPrimitive(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("primitive"), name(y3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def function_type(x: hydra.core.FunctionType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FunctionType"), (hydra.core.Field(hydra.core.Name("domain"), type(x.domain)), hydra.core.Field(hydra.core.Name("codomain"), type(x.codomain))))))

def injection(x: hydra.core.Injection) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Injection"), (hydra.core.Field(hydra.core.Name("typeName"), name(x.type_name)), hydra.core.Field(hydra.core.Name("field"), field(x.field))))))

def lambda_(x: hydra.core.Lambda) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Lambda"), (hydra.core.Field(hydra.core.Name("parameter"), name(x.parameter)), hydra.core.Field(hydra.core.Name("domain"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(type, x.domain)))), hydra.core.Field(hydra.core.Name("body"), term(x.body))))))

def let(x: hydra.core.Let) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Let"), (hydra.core.Field(hydra.core.Name("bindings"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(binding, x.bindings)))), hydra.core.Field(hydra.core.Name("body"), term(x.body))))))

def map_type(x: hydra.core.MapType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.MapType"), (hydra.core.Field(hydra.core.Name("keys"), type(x.keys)), hydra.core.Field(hydra.core.Name("values"), type(x.values))))))

def pair_type(x: hydra.core.PairType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.PairType"), (hydra.core.Field(hydra.core.Name("first"), type(x.first)), hydra.core.Field(hydra.core.Name("second"), type(x.second))))))

def record(x: hydra.core.Record) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Record"), (hydra.core.Field(hydra.core.Name("typeName"), name(x.type_name)), hydra.core.Field(hydra.core.Name("fields"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(field, x.fields))))))))

def row_type(x: hydra.core.RowType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.RowType"), (hydra.core.Field(hydra.core.Name("typeName"), name(x.type_name)), hydra.core.Field(hydra.core.Name("fields"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(field_type, x.fields))))))))

def term(v1: hydra.core.Term) -> hydra.core.Type:
    match v1:
        case hydra.core.TermAnnotated(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("annotated"), annotated_term(y)))))
        
        case hydra.core.TermApplication(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("application"), application(y2)))))
        
        case hydra.core.TermEither(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("either"), cast(hydra.core.Term, hydra.core.TermEither(hydra.lib.eithers.bimap(term, term, y3)))))))
        
        case hydra.core.TermFunction(value=y4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("function"), function(y4)))))
        
        case hydra.core.TermLet(value=y5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("let"), let(y5)))))
        
        case hydra.core.TermList(value=y6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("list"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(term, y6)))))))
        
        case hydra.core.TermLiteral(value=y7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("literal"), literal(y7)))))
        
        case hydra.core.TermMap(value=y8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("map"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(term, term, y8)))))))
        
        case hydra.core.TermMaybe(value=y9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("maybe"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(term, y9)))))))
        
        case hydra.core.TermPair(value=y10):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("pair"), cast(hydra.core.Term, hydra.core.TermPair(hydra.lib.pairs.bimap(term, term, y10)))))))
        
        case hydra.core.TermRecord(value=y11):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("record"), record(y11)))))
        
        case hydra.core.TermSet(value=y12):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("set"), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map(term, y12)))))))
        
        case hydra.core.TermTypeApplication(value=y13):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeApplication"), type_application_term(y13)))))
        
        case hydra.core.TermTypeLambda(value=y14):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeLambda"), type_lambda(y14)))))
        
        case hydra.core.TermUnion(value=y15):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("union"), injection(y15)))))
        
        case hydra.core.TermUnit():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("unit"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.TermVariable(value=y17):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("variable"), name(y17)))))
        
        case hydra.core.TermWrap(value=y18):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("wrap"), wrapped_term(y18)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def type(v1: hydra.core.Type) -> hydra.core.Type:
    match v1:
        case hydra.core.TypeAnnotated(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("annotated"), annotated_type(y)))))
        
        case hydra.core.TypeApplication(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("application"), application_type(y2)))))
        
        case hydra.core.TypeEither(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("either"), either_type(y3)))))
        
        case hydra.core.TypeForall(value=y4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("forall"), forall_type(y4)))))
        
        case hydra.core.TypeFunction(value=y5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("function"), function_type(y5)))))
        
        case hydra.core.TypeList(value=y6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("list"), type(y6)))))
        
        case hydra.core.TypeLiteral(value=y7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("literal"), literal_type(y7)))))
        
        case hydra.core.TypeMap(value=y8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("map"), map_type(y8)))))
        
        case hydra.core.TypeMaybe(value=y9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("maybe"), type(y9)))))
        
        case hydra.core.TypePair(value=y10):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("pair"), pair_type(y10)))))
        
        case hydra.core.TypeRecord(value=y11):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("record"), row_type(y11)))))
        
        case hydra.core.TypeSet(value=y12):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("set"), type(y12)))))
        
        case hydra.core.TypeUnion(value=y13):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("union"), row_type(y13)))))
        
        case hydra.core.TypeUnit():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("unit"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.TypeVariable(value=y15):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("variable"), name(y15)))))
        
        case hydra.core.TypeWrap(value=y16):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("wrap"), wrapped_type(y16)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def type_application_term(x: hydra.core.TypeApplicationTerm) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeApplicationTerm"), (hydra.core.Field(hydra.core.Name("body"), term(x.body)), hydra.core.Field(hydra.core.Name("type"), type(x.type))))))

def type_lambda(x: hydra.core.TypeLambda) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeLambda"), (hydra.core.Field(hydra.core.Name("parameter"), name(x.parameter)), hydra.core.Field(hydra.core.Name("body"), term(x.body))))))

def type_scheme(x: hydra.core.TypeScheme) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeScheme"), (hydra.core.Field(hydra.core.Name("variables"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(name, x.variables)))), hydra.core.Field(hydra.core.Name("type"), type(x.type)), hydra.core.Field(hydra.core.Name("constraints"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(name, type_variable_metadata, m)))), x.constraints))))))))

def wrapped_term(x: hydra.core.WrappedTerm) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedTerm"), (hydra.core.Field(hydra.core.Name("typeName"), name(x.type_name)), hydra.core.Field(hydra.core.Name("body"), term(x.body))))))

def wrapped_type(x: hydra.core.WrappedType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedType"), (hydra.core.Field(hydra.core.Name("typeName"), name(x.type_name)), hydra.core.Field(hydra.core.Name("body"), type(x.body))))))
