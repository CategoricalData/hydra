# Note: this is an automatically generated file. Do not edit.

r"""Mapping of hydra.core constructs in a host language like Haskell or Java  to their native Hydra counterparts as terms.  This includes an implementation of LambdaGraph's epsilon encoding (types to terms)."""

from __future__ import annotations
from hydra.dsl.python import Either, Left, Right
from typing import cast
import hydra.core
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.rewriting

def name(fn: hydra.core.Name) -> hydra.core.Term:
    r"""Encode a name as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(fn.value)))))))

def projection(p: hydra.core.Projection) -> hydra.core.Term:
    r"""Encode a projection as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Projection"), (hydra.core.Field(hydra.core.Name("typeName"), name(p.type_name)), hydra.core.Field(hydra.core.Name("field"), name(p.field))))))

def float_type(v1: hydra.core.FloatType) -> hydra.core.Term:
    r"""Encode a floating-point type as a term."""
    
    match v1:
        case hydra.core.FloatType.BIGFLOAT:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("bigfloat"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.FloatType.FLOAT32:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float32"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.FloatType.FLOAT64:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float64"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def integer_type(v1: hydra.core.IntegerType) -> hydra.core.Term:
    r"""Encode an integer type as a term."""
    
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

def literal_type(v1: hydra.core.LiteralType) -> hydra.core.Term:
    r"""Encode a literal type as a term."""
    
    match v1:
        case hydra.core.LiteralTypeBinary():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("binary"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.LiteralTypeBoolean():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("boolean"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.LiteralTypeFloat(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("float"), float_type(v)))))
        
        case hydra.core.LiteralTypeInteger(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("integer"), integer_type(v2)))))
        
        case hydra.core.LiteralTypeString():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("string"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def application_type(at: hydra.core.ApplicationType) -> hydra.core.Term:
    r"""Encode an application type as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ApplicationType"), (hydra.core.Field(hydra.core.Name("function"), type(at.function)), hydra.core.Field(hydra.core.Name("argument"), type(at.argument))))))

def either_type(et: hydra.core.EitherType) -> hydra.core.Term:
    r"""Encode an either type as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.EitherType"), (hydra.core.Field(hydra.core.Name("left"), type(et.left)), hydra.core.Field(hydra.core.Name("right"), type(et.right))))))

def field_type(ft: hydra.core.FieldType) -> hydra.core.Term:
    r"""Encode a field type as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FieldType"), (hydra.core.Field(hydra.core.Name("name"), name(ft.name)), hydra.core.Field(hydra.core.Name("type"), type(ft.type))))))

def forall_type(lt: hydra.core.ForallType) -> hydra.core.Term:
    r"""Encode a forall type as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ForallType"), (hydra.core.Field(hydra.core.Name("parameter"), name(lt.parameter)), hydra.core.Field(hydra.core.Name("body"), type(lt.body))))))

def function_type(ft: hydra.core.FunctionType) -> hydra.core.Term:
    r"""Encode a function type as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FunctionType"), (hydra.core.Field(hydra.core.Name("domain"), type(ft.domain)), hydra.core.Field(hydra.core.Name("codomain"), type(ft.codomain))))))

def map_type(mt: hydra.core.MapType) -> hydra.core.Term:
    r"""Encode a map type as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.MapType"), (hydra.core.Field(hydra.core.Name("keys"), type(mt.keys)), hydra.core.Field(hydra.core.Name("values"), type(mt.values))))))

def pair_type(pt: hydra.core.PairType) -> hydra.core.Term:
    r"""Encode a pair type as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.PairType"), (hydra.core.Field(hydra.core.Name("first"), type(pt.first)), hydra.core.Field(hydra.core.Name("second"), type(pt.second))))))

def row_type(rt: hydra.core.RowType) -> hydra.core.Term:
    r"""Encode a row type as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.RowType"), (hydra.core.Field(hydra.core.Name("typeName"), name(rt.type_name)), hydra.core.Field(hydra.core.Name("fields"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(field_type, rt.fields))))))))

def type(v1: hydra.core.Type) -> hydra.core.Term:
    r"""Encode a type as a term (epsilon encoding)."""
    
    match v1:
        case hydra.core.TypeAnnotated(value=v):
            return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(type(v.body), v.annotation)))
        
        case hydra.core.TypeApplication(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("application"), application_type(v2)))))
        
        case hydra.core.TypeEither(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("either"), either_type(v3)))))
        
        case hydra.core.TypeFunction(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("function"), function_type(v4)))))
        
        case hydra.core.TypeForall(value=v5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("forall"), forall_type(v5)))))
        
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
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("unit"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.core.TypeVariable(value=v14):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("variable"), name(v14)))))
        
        case hydra.core.TypeWrap(value=v15):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("wrap"), wrapped_type(v15)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def wrapped_type(nt: hydra.core.WrappedType) -> hydra.core.Term:
    r"""Encode a wrapped type as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedType"), (hydra.core.Field(hydra.core.Name("typeName"), name(nt.type_name)), hydra.core.Field(hydra.core.Name("body"), type(nt.body))))))

def type_scheme(ts: hydra.core.TypeScheme) -> hydra.core.Term:
    r"""Encode a type scheme as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeScheme"), (hydra.core.Field(hydra.core.Name("variables"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(name, ts.variables)))), hydra.core.Field(hydra.core.Name("type"), type(ts.type))))))

def float_value(v1: hydra.core.FloatValue) -> hydra.core.Term:
    r"""Encode a floating-point value as a term."""
    
    match v1:
        case hydra.core.FloatValueBigfloat(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("bigfloat"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueBigfloat(v))))))))))
        
        case hydra.core.FloatValueFloat32(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float32"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat32(v2))))))))))
        
        case hydra.core.FloatValueFloat64(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float64"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat64(v3))))))))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def integer_value(v1: hydra.core.IntegerValue) -> hydra.core.Term:
    r"""Encode an integer value as a term."""
    
    match v1:
        case hydra.core.IntegerValueBigint(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("bigint"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueBigint(v))))))))))
        
        case hydra.core.IntegerValueInt8(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int8"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt8(v2))))))))))
        
        case hydra.core.IntegerValueInt16(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int16"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt16(v3))))))))))
        
        case hydra.core.IntegerValueInt32(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int32"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(v4))))))))))
        
        case hydra.core.IntegerValueInt64(value=v5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int64"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(v5))))))))))
        
        case hydra.core.IntegerValueUint8(value=v6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint8"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint8(v6))))))))))
        
        case hydra.core.IntegerValueUint16(value=v7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint16"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint16(v7))))))))))
        
        case hydra.core.IntegerValueUint32(value=v8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint32"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint32(v8))))))))))
        
        case hydra.core.IntegerValueUint64(value=v9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint64"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint64(v9))))))))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def literal(v1: hydra.core.Literal) -> hydra.core.Term:
    r"""Encode a literal as a term."""
    
    match v1:
        case hydra.core.LiteralBinary(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("binary"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBinary(v))))))))
        
        case hydra.core.LiteralBoolean(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("boolean"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(v2))))))))
        
        case hydra.core.LiteralFloat(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("float"), float_value(v3)))))
        
        case hydra.core.LiteralInteger(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("integer"), integer_value(v4)))))
        
        case hydra.core.LiteralString(value=v5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("string"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(v5))))))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def annotated_term(a: hydra.core.AnnotatedTerm) -> hydra.core.Term:
    r"""Encode an annotated term as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(term(a.body), a.annotation)))

def application(app: hydra.core.Application) -> hydra.core.Term:
    r"""Encode an application as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Application"), (hydra.core.Field(hydra.core.Name("function"), term(app.function)), hydra.core.Field(hydra.core.Name("argument"), term(app.argument))))))

def binding(b: hydra.core.Binding) -> hydra.core.Term:
    r"""Encode a let binding as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Binding"), (hydra.core.Field(hydra.core.Name("name"), name(b.name)), hydra.core.Field(hydra.core.Name("term"), term(b.term)), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(type_scheme, b.type))))))))

def case_statement(cs: hydra.core.CaseStatement) -> hydra.core.Term:
    r"""Encode a case statement as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.CaseStatement"), (hydra.core.Field(hydra.core.Name("typeName"), name(cs.type_name)), hydra.core.Field(hydra.core.Name("default"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(term, cs.default)))), hydra.core.Field(hydra.core.Name("cases"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(field, cs.cases))))))))

def elimination(v1: hydra.core.Elimination) -> hydra.core.Term:
    r"""Encode an elimination as a term."""
    
    match v1:
        case hydra.core.EliminationRecord(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("record"), projection(v)))))
        
        case hydra.core.EliminationUnion(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("union"), case_statement(v2)))))
        
        case hydra.core.EliminationWrap(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("wrap"), name(v3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def field(f: hydra.core.Field) -> hydra.core.Term:
    r"""Encode a field as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Field"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(f.name.value)))))))), hydra.core.Field(hydra.core.Name("term"), term(f.term))))))

def function(v1: hydra.core.Function) -> hydra.core.Term:
    r"""Encode a function as a term."""
    
    match v1:
        case hydra.core.FunctionElimination(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("elimination"), elimination(v)))))
        
        case hydra.core.FunctionLambda(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("lambda"), lambda_(v2)))))
        
        case hydra.core.FunctionPrimitive(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("primitive"), name(v3)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def injection(i: hydra.core.Injection) -> hydra.core.Term:
    r"""Encode an injection as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Injection"), (hydra.core.Field(hydra.core.Name("typeName"), name(i.type_name)), hydra.core.Field(hydra.core.Name("field"), field(i.field))))))

def lambda_(l: hydra.core.Lambda) -> hydra.core.Term:
    r"""Encode a lambda as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Lambda"), (hydra.core.Field(hydra.core.Name("parameter"), name(l.parameter)), hydra.core.Field(hydra.core.Name("domain"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(type, l.domain)))), hydra.core.Field(hydra.core.Name("body"), term(l.body))))))

def let(l: hydra.core.Let) -> hydra.core.Term:
    r"""Encode a let expression as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Let"), (hydra.core.Field(hydra.core.Name("bindings"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(binding, l.bindings)))), hydra.core.Field(hydra.core.Name("body"), term(l.body))))))

def record(r: hydra.core.Record) -> hydra.core.Term:
    r"""Encode a record as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Record"), (hydra.core.Field(hydra.core.Name("typeName"), name(r.type_name)), hydra.core.Field(hydra.core.Name("fields"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(field, r.fields))))))))

def term(v1: hydra.core.Term) -> hydra.core.Term:
    r"""Encode a term as a term (identity encoding)."""
    
    match v1:
        case hydra.core.TermAnnotated(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("annotated"), annotated_term(v)))))
        
        case hydra.core.TermApplication(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("application"), application(v2)))))
        
        case hydra.core.TermEither(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("either"), cast(hydra.core.Term, hydra.core.TermEither(hydra.lib.eithers.either((lambda l: cast(Either[hydra.core.Term, hydra.core.Term], Left(term(l)))), (lambda r: cast(Either[hydra.core.Term, hydra.core.Term], Right(term(r)))), v3)))))))
        
        case hydra.core.TermFunction(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("function"), function(v4)))))
        
        case hydra.core.TermLet(value=v5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("let"), let(v5)))))
        
        case hydra.core.TermList(value=v6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("list"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(term, v6)))))))
        
        case hydra.core.TermLiteral(value=v7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("literal"), literal(v7)))))
        
        case hydra.core.TermMap(value=v8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("map"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(term, term, v8)))))))
        
        case hydra.core.TermMaybe(value=v9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("maybe"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(term, v9)))))))
        
        case hydra.core.TermPair(value=v10):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("pair"), cast(hydra.core.Term, hydra.core.TermPair(cast(Tuple[hydra.core.Term, hydra.core.Term], (term(hydra.lib.pairs.first(v10)), term(hydra.lib.pairs.second(v10))))))))))
        
        case hydra.core.TermRecord(value=v11):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("record"), record(v11)))))
        
        case hydra.core.TermSet(value=v12):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("set"), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map(term, v12)))))))
        
        case hydra.core.TermTypeApplication(value=v13):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeApplication"), type_application_term(v13)))))
        
        case hydra.core.TermTypeLambda(value=v14):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeLambda"), type_lambda(v14)))))
        
        case hydra.core.TermUnion(value=v15):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("union"), injection(v15)))))
        
        case hydra.core.TermUnit(value=v16):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("unit"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v16)))))
        
        case hydra.core.TermVariable(value=v17):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("variable"), name(v17)))))
        
        case hydra.core.TermWrap(value=v18):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("wrap"), wrapped_term(v18)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def type_application_term(tt: hydra.core.TypeApplicationTerm) -> hydra.core.Term:
    r"""Encode a type application term as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeApplicationTerm"), (hydra.core.Field(hydra.core.Name("body"), term(tt.body)), hydra.core.Field(hydra.core.Name("type"), type(tt.type))))))

def type_lambda(l: hydra.core.TypeLambda) -> hydra.core.Term:
    r"""Encode a type lambda as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeLambda"), (hydra.core.Field(hydra.core.Name("parameter"), name(l.parameter)), hydra.core.Field(hydra.core.Name("body"), term(l.body))))))

def wrapped_term(n: hydra.core.WrappedTerm) -> hydra.core.Term:
    r"""Encode a wrapped term as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedTerm"), (hydra.core.Field(hydra.core.Name("typeName"), name(n.type_name)), hydra.core.Field(hydra.core.Name("body"), term(n.body))))))

def annotated_type(at: hydra.core.AnnotatedType) -> hydra.core.Term:
    r"""Encode an annotated type as a term."""
    
    return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(type(at.body), at.annotation)))

def is_encoded_type(t: hydra.core.Term) -> bool:
    r"""Determines whether a given term is an encoded type."""
    
    match hydra.rewriting.deannotate_term(t):
        case hydra.core.TermApplication(value=a):
            return is_encoded_type(a.function)
        
        case hydra.core.TermUnion(value=i):
            return hydra.lib.equality.equal("hydra.core.Type", i.type_name.value)
        
        case _:
            return False

def is_type(t: hydra.core.Type) -> bool:
    r"""Check whether a type is a type (always true for non-encoded types)."""
    
    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypeApplication(value=a):
            return is_type(a.function)
        
        case hydra.core.TypeForall(value=l):
            return is_type(l.body)
        
        case hydra.core.TypeUnion(value=rt):
            return hydra.lib.equality.equal("hydra.core.Type", rt.type_name.value)
        
        case hydra.core.TypeVariable(value=v):
            return hydra.lib.equality.equal(v, hydra.core.Name("hydra.core.Type"))
        
        case _:
            return False

def is_unit_term(v1: hydra.core.Term) -> bool:
    r"""Check whether a term is the unit term."""
    
    match v1:
        case hydra.core.TermUnit():
            return True
        
        case _:
            return False

def is_unit_type(v1: hydra.core.Type) -> bool:
    r"""Check whether a type is the unit type."""
    
    match v1:
        case hydra.core.TypeUnit():
            return True
        
        case _:
            return False
