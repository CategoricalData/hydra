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
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.value))))

def projection(x: hydra.core.Projection) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Projection"), (hydra.core.Field(hydra.core.Name("typeName"), name(x.type_name)), hydra.core.Field(hydra.core.Name("field"), name(x.field))))))

def float_type(v1: hydra.core.FloatType) -> hydra.core.Type:
    match v1:
        case hydra.core.FloatType.BIGFLOAT:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("bigfloat"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.core.FloatType.FLOAT32:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float32"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.core.FloatType.FLOAT64:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float64"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def integer_type(v1: hydra.core.IntegerType) -> hydra.core.Type:
    match v1:
        case hydra.core.IntegerType.BIGINT:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("bigint"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.core.IntegerType.INT8:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int8"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.core.IntegerType.INT16:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int16"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.core.IntegerType.INT32:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int32"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.core.IntegerType.INT64:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int64"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.core.IntegerType.UINT8:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint8"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.core.IntegerType.UINT16:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint16"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.core.IntegerType.UINT32:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint32"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.core.IntegerType.UINT64:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint64"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def literal_type(v1: hydra.core.LiteralType) -> hydra.core.Type:
    match v1:
        case hydra.core.LiteralTypeBinary():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("binary"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.core.LiteralTypeBoolean():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("boolean"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.core.LiteralTypeFloat(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("float"), float_type(v3)))))
        
        case hydra.core.LiteralTypeInteger(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("integer"), integer_type(v4)))))
        
        case hydra.core.LiteralTypeString():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("string"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def type_variable_metadata(x: hydra.core.TypeVariableMetadata) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeVariableMetadata"), (hydra.core.Field(hydra.core.Name("classes"), (lambda s: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map(name, s))))(x.classes)),))))

def float_value(v1: hydra.core.FloatValue) -> hydra.core.Type:
    match v1:
        case hydra.core.FloatValueBigfloat(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("bigfloat"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueBigfloat(x)))))))(v)))))
        
        case hydra.core.FloatValueFloat32(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float32"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat32(x)))))))(v2)))))
        
        case hydra.core.FloatValueFloat64(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float64"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat64(x)))))))(v3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def integer_value(v1: hydra.core.IntegerValue) -> hydra.core.Type:
    match v1:
        case hydra.core.IntegerValueBigint(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("bigint"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueBigint(x)))))))(v)))))
        
        case hydra.core.IntegerValueInt8(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int8"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt8(x)))))))(v2)))))
        
        case hydra.core.IntegerValueInt16(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int16"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt16(x)))))))(v3)))))
        
        case hydra.core.IntegerValueInt32(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int32"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x)))))))(v4)))))
        
        case hydra.core.IntegerValueInt64(value=v5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int64"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(x)))))))(v5)))))
        
        case hydra.core.IntegerValueUint8(value=v6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint8"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint8(x)))))))(v6)))))
        
        case hydra.core.IntegerValueUint16(value=v7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint16"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint16(x)))))))(v7)))))
        
        case hydra.core.IntegerValueUint32(value=v8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint32"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint32(x)))))))(v8)))))
        
        case hydra.core.IntegerValueUint64(value=v9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint64"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint64(x)))))))(v9)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def literal(v1: hydra.core.Literal) -> hydra.core.Type:
    match v1:
        case hydra.core.LiteralBinary(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("binary"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBinary(x)))))(v)))))
        
        case hydra.core.LiteralBoolean(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("boolean"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(x)))))(v2)))))
        
        case hydra.core.LiteralFloat(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("float"), float_value(v3)))))
        
        case hydra.core.LiteralInteger(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("integer"), integer_value(v4)))))
        
        case hydra.core.LiteralString(value=v5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("string"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x)))))(v5)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def annotated_term(x: hydra.core.AnnotatedTerm) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.AnnotatedTerm"), (hydra.core.Field(hydra.core.Name("body"), term(x.body)), hydra.core.Field(hydra.core.Name("annotation"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(name, term, m))))(x.annotation))))))

def annotated_type(x: hydra.core.AnnotatedType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.AnnotatedType"), (hydra.core.Field(hydra.core.Name("body"), type(x.body)), hydra.core.Field(hydra.core.Name("annotation"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(name, term, m))))(x.annotation))))))

def application(x: hydra.core.Application) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Application"), (hydra.core.Field(hydra.core.Name("function"), term(x.function)), hydra.core.Field(hydra.core.Name("argument"), term(x.argument))))))

def application_type(x: hydra.core.ApplicationType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ApplicationType"), (hydra.core.Field(hydra.core.Name("function"), type(x.function)), hydra.core.Field(hydra.core.Name("argument"), type(x.argument))))))

def binding(x: hydra.core.Binding) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Binding"), (hydra.core.Field(hydra.core.Name("name"), name(x.name)), hydra.core.Field(hydra.core.Name("term"), term(x.term)), hydra.core.Field(hydra.core.Name("type"), (lambda opt: cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(type_scheme, opt))))(x.type))))))

def case_statement(x: hydra.core.CaseStatement) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.CaseStatement"), (hydra.core.Field(hydra.core.Name("typeName"), name(x.type_name)), hydra.core.Field(hydra.core.Name("default"), (lambda opt: cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(term, opt))))(x.default)), hydra.core.Field(hydra.core.Name("cases"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(field, xs))))(x.cases))))))

def either_type(x: hydra.core.EitherType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.EitherType"), (hydra.core.Field(hydra.core.Name("left"), type(x.left)), hydra.core.Field(hydra.core.Name("right"), type(x.right))))))

def elimination(v1: hydra.core.Elimination) -> hydra.core.Type:
    match v1:
        case hydra.core.EliminationRecord(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("record"), projection(v)))))
        
        case hydra.core.EliminationUnion(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("union"), case_statement(v2)))))
        
        case hydra.core.EliminationWrap(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("wrap"), name(v3)))))
        
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
        case hydra.core.FunctionElimination(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("elimination"), elimination(v)))))
        
        case hydra.core.FunctionLambda(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("lambda"), lambda_(v2)))))
        
        case hydra.core.FunctionPrimitive(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("primitive"), name(v3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def function_type(x: hydra.core.FunctionType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FunctionType"), (hydra.core.Field(hydra.core.Name("domain"), type(x.domain)), hydra.core.Field(hydra.core.Name("codomain"), type(x.codomain))))))

def injection(x: hydra.core.Injection) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Injection"), (hydra.core.Field(hydra.core.Name("typeName"), name(x.type_name)), hydra.core.Field(hydra.core.Name("field"), field(x.field))))))

def lambda_(x: hydra.core.Lambda) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Lambda"), (hydra.core.Field(hydra.core.Name("parameter"), name(x.parameter)), hydra.core.Field(hydra.core.Name("domain"), (lambda opt: cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(type, opt))))(x.domain)), hydra.core.Field(hydra.core.Name("body"), term(x.body))))))

def let(x: hydra.core.Let) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Let"), (hydra.core.Field(hydra.core.Name("bindings"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(binding, xs))))(x.bindings)), hydra.core.Field(hydra.core.Name("body"), term(x.body))))))

def map_type(x: hydra.core.MapType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.MapType"), (hydra.core.Field(hydra.core.Name("keys"), type(x.keys)), hydra.core.Field(hydra.core.Name("values"), type(x.values))))))

def pair_type(x: hydra.core.PairType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.PairType"), (hydra.core.Field(hydra.core.Name("first"), type(x.first)), hydra.core.Field(hydra.core.Name("second"), type(x.second))))))

def record(x: hydra.core.Record) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Record"), (hydra.core.Field(hydra.core.Name("typeName"), name(x.type_name)), hydra.core.Field(hydra.core.Name("fields"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(field, xs))))(x.fields))))))

def row_type(x: hydra.core.RowType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.RowType"), (hydra.core.Field(hydra.core.Name("typeName"), name(x.type_name)), hydra.core.Field(hydra.core.Name("fields"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(field_type, xs))))(x.fields))))))

def term(v1: hydra.core.Term) -> hydra.core.Type:
    match v1:
        case hydra.core.TermAnnotated(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("annotated"), annotated_term(v)))))
        
        case hydra.core.TermApplication(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("application"), application(v2)))))
        
        case hydra.core.TermEither(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("either"), (lambda e: cast(hydra.core.Term, hydra.core.TermEither(hydra.lib.eithers.bimap(term, term, e))))(v3)))))
        
        case hydra.core.TermFunction(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("function"), function(v4)))))
        
        case hydra.core.TermLet(value=v5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("let"), let(v5)))))
        
        case hydra.core.TermList(value=v6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("list"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(term, xs))))(v6)))))
        
        case hydra.core.TermLiteral(value=v7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("literal"), literal(v7)))))
        
        case hydra.core.TermMap(value=v8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("map"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(term, term, m))))(v8)))))
        
        case hydra.core.TermMaybe(value=v9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("maybe"), (lambda opt: cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(term, opt))))(v9)))))
        
        case hydra.core.TermPair(value=v10):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("pair"), (lambda p: cast(hydra.core.Term, hydra.core.TermPair(hydra.lib.pairs.bimap(term, term, p))))(v10)))))
        
        case hydra.core.TermRecord(value=v11):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("record"), record(v11)))))
        
        case hydra.core.TermSet(value=v12):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("set"), (lambda s: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map(term, s))))(v12)))))
        
        case hydra.core.TermTypeApplication(value=v13):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeApplication"), type_application_term(v13)))))
        
        case hydra.core.TermTypeLambda(value=v14):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeLambda"), type_lambda(v14)))))
        
        case hydra.core.TermUnion(value=v15):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("union"), injection(v15)))))
        
        case hydra.core.TermUnit():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("unit"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.core.TermVariable(value=v17):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("variable"), name(v17)))))
        
        case hydra.core.TermWrap(value=v18):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("wrap"), wrapped_term(v18)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def type(v1: hydra.core.Type) -> hydra.core.Type:
    match v1:
        case hydra.core.TypeAnnotated(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("annotated"), annotated_type(v)))))
        
        case hydra.core.TypeApplication(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("application"), application_type(v2)))))
        
        case hydra.core.TypeEither(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("either"), either_type(v3)))))
        
        case hydra.core.TypeForall(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("forall"), forall_type(v4)))))
        
        case hydra.core.TypeFunction(value=v5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("function"), function_type(v5)))))
        
        case hydra.core.TypeList(value=v6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("list"), type(v6)))))
        
        case hydra.core.TypeLiteral(value=v7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("literal"), literal_type(v7)))))
        
        case hydra.core.TypeMap(value=v8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("map"), map_type(v8)))))
        
        case hydra.core.TypeMaybe(value=v9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("maybe"), type(v9)))))
        
        case hydra.core.TypePair(value=v10):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("pair"), pair_type(v10)))))
        
        case hydra.core.TypeRecord(value=v11):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("record"), row_type(v11)))))
        
        case hydra.core.TypeSet(value=v12):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("set"), type(v12)))))
        
        case hydra.core.TypeUnion(value=v13):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("union"), row_type(v13)))))
        
        case hydra.core.TypeUnit():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("unit"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.core.TypeVariable(value=v15):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("variable"), name(v15)))))
        
        case hydra.core.TypeWrap(value=v16):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("wrap"), wrapped_type(v16)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def type_application_term(x: hydra.core.TypeApplicationTerm) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeApplicationTerm"), (hydra.core.Field(hydra.core.Name("body"), term(x.body)), hydra.core.Field(hydra.core.Name("type"), type(x.type))))))

def type_lambda(x: hydra.core.TypeLambda) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeLambda"), (hydra.core.Field(hydra.core.Name("parameter"), name(x.parameter)), hydra.core.Field(hydra.core.Name("body"), term(x.body))))))

def type_scheme(x: hydra.core.TypeScheme) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeScheme"), (hydra.core.Field(hydra.core.Name("variables"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(name, xs))))(x.variables)), hydra.core.Field(hydra.core.Name("type"), type(x.type)), hydra.core.Field(hydra.core.Name("constraints"), (lambda opt: cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(name, type_variable_metadata, m)))), opt))))(x.constraints))))))

def wrapped_term(x: hydra.core.WrappedTerm) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedTerm"), (hydra.core.Field(hydra.core.Name("typeName"), name(x.type_name)), hydra.core.Field(hydra.core.Name("body"), term(x.body))))))

def wrapped_type(x: hydra.core.WrappedType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedType"), (hydra.core.Field(hydra.core.Name("typeName"), name(x.type_name)), hydra.core.Field(hydra.core.Name("body"), type(x.body))))))
