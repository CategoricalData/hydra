# Note: this is an automatically generated file. Do not edit.

"""Mapping of hydra.core constructs in a host language like Haskell or Java  to their native Hydra counterparts as terms.  This includes an implementation of LambdaGraph's epsilon encoding (types to terms)."""

from __future__ import annotations
from hydra.dsl.python import frozenlist
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.optionals
import hydra.lib.sets
import hydra.rewriting

def name(fn: hydra.core.Name) -> hydra.core.Term:
    return hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), hydra.core.TermLiteral(hydra.core.LiteralString(fn.value))))

def projection(p: hydra.core.Projection) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Projection"), (hydra.core.Field(hydra.core.Name("typeName"), name(p.type_name)), hydra.core.Field(hydra.core.Name("field"), name(p.field)))))

def float_type(v1: hydra.core.FloatType) -> hydra.core.Term:
    match v1:
        case hydra.core.FloatType.BIGFLOAT:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("bigfloat"), hydra.core.TermUnit(None))))
        
        case hydra.core.FloatType.FLOAT32:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float32"), hydra.core.TermUnit(None))))
        
        case hydra.core.FloatType.FLOAT64:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float64"), hydra.core.TermUnit(None))))

def integer_type(v1: hydra.core.IntegerType) -> hydra.core.Term:
    match v1:
        case hydra.core.IntegerType.BIGINT:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("bigint"), hydra.core.TermUnit(None))))
        
        case hydra.core.IntegerType.INT8:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int8"), hydra.core.TermUnit(None))))
        
        case hydra.core.IntegerType.INT16:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int16"), hydra.core.TermUnit(None))))
        
        case hydra.core.IntegerType.INT32:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int32"), hydra.core.TermUnit(None))))
        
        case hydra.core.IntegerType.INT64:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int64"), hydra.core.TermUnit(None))))
        
        case hydra.core.IntegerType.UINT8:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint8"), hydra.core.TermUnit(None))))
        
        case hydra.core.IntegerType.UINT16:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint16"), hydra.core.TermUnit(None))))
        
        case hydra.core.IntegerType.UINT32:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint32"), hydra.core.TermUnit(None))))
        
        case hydra.core.IntegerType.UINT64:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint64"), hydra.core.TermUnit(None))))

def literal_type(v1: hydra.core.LiteralType) -> hydra.core.Term:
    match v1:
        case hydra.core.LiteralTypeBinary():
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("binary"), hydra.core.TermUnit(None))))
        
        case hydra.core.LiteralTypeBoolean():
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("boolean"), hydra.core.TermUnit(None))))
        
        case hydra.core.LiteralTypeFloat(value=v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("float"), float_type(v))))
        
        case hydra.core.LiteralTypeInteger(value=v2):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("integer"), integer_type(v2))))
        
        case hydra.core.LiteralTypeString():
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("string"), hydra.core.TermUnit(None))))

def application_type(at: hydra.core.ApplicationType) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ApplicationType"), (hydra.core.Field(hydra.core.Name("function"), type(at.function)), hydra.core.Field(hydra.core.Name("argument"), type(at.argument)))))

def field_type(ft: hydra.core.FieldType) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FieldType"), (hydra.core.Field(hydra.core.Name("name"), name(ft.name)), hydra.core.Field(hydra.core.Name("type"), type(ft.type)))))

def forall_type(lt: hydra.core.ForallType) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ForallType"), (hydra.core.Field(hydra.core.Name("parameter"), name(lt.parameter)), hydra.core.Field(hydra.core.Name("body"), type(lt.body)))))

def function_type(ft: hydra.core.FunctionType) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FunctionType"), (hydra.core.Field(hydra.core.Name("domain"), type(ft.domain)), hydra.core.Field(hydra.core.Name("codomain"), type(ft.codomain)))))

def map_type(mt: hydra.core.MapType) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.MapType"), (hydra.core.Field(hydra.core.Name("keys"), type(mt.keys)), hydra.core.Field(hydra.core.Name("values"), type(mt.values)))))

def row_type(rt: hydra.core.RowType) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.RowType"), (hydra.core.Field(hydra.core.Name("typeName"), name(rt.type_name)), hydra.core.Field(hydra.core.Name("fields"), hydra.core.TermList(hydra.lib.lists.map(field_type, rt.fields))))))

def type(v1: hydra.core.Type) -> hydra.core.Term:
    match v1:
        case hydra.core.TypeAnnotated(value=v):
            return hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(type(v.body), v.annotation))
        
        case hydra.core.TypeApplication(value=v2):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("application"), application_type(v2))))
        
        case hydra.core.TypeFunction(value=v3):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("function"), function_type(v3))))
        
        case hydra.core.TypeForall(value=v4):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("forall"), forall_type(v4))))
        
        case hydra.core.TypeList(value=v5):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("list"), type(v5))))
        
        case hydra.core.TypeLiteral(value=v6):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("literal"), literal_type(v6))))
        
        case hydra.core.TypeMap(value=v7):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("map"), map_type(v7))))
        
        case hydra.core.TypeOptional(value=v8):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("optional"), type(v8))))
        
        case hydra.core.TypeProduct(value=v9):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("product"), hydra.core.TermList(hydra.lib.lists.map(type, v9)))))
        
        case hydra.core.TypeRecord(value=v10):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("record"), row_type(v10))))
        
        case hydra.core.TypeSet(value=v11):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("set"), type(v11))))
        
        case hydra.core.TypeSum(value=v12):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("sum"), hydra.core.TermList(hydra.lib.lists.map(type, v12)))))
        
        case hydra.core.TypeUnion(value=v13):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("union"), row_type(v13))))
        
        case hydra.core.TypeUnit():
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("unit"), hydra.core.TermUnit(None))))
        
        case hydra.core.TypeVariable(value=v14):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("variable"), name(v14))))
        
        case hydra.core.TypeWrap(value=v15):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("wrap"), wrapped_type(v15))))

def wrapped_type(nt: hydra.core.WrappedType) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedType"), (hydra.core.Field(hydra.core.Name("typeName"), name(nt.type_name)), hydra.core.Field(hydra.core.Name("body"), type(nt.body)))))

def tuple_projection(tp: hydra.core.TupleProjection) -> hydra.core.Term:
    def encode_types(types: frozenlist[hydra.core.Type]) -> hydra.core.Term:
        return hydra.core.TermList(hydra.lib.lists.map(type, types))
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TupleProjection"), (hydra.core.Field(hydra.core.Name("arity"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(tp.arity)))), hydra.core.Field(hydra.core.Name("index"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(tp.index)))), hydra.core.Field(hydra.core.Name("domain"), hydra.core.TermOptional(hydra.lib.optionals.map(encode_types, tp.domain))))))

def type_scheme(ts: hydra.core.TypeScheme) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeScheme"), (hydra.core.Field(hydra.core.Name("variables"), hydra.core.TermList(hydra.lib.lists.map(name, ts.variables))), hydra.core.Field(hydra.core.Name("type"), type(ts.type)))))

def float_value(v1: hydra.core.FloatValue) -> hydra.core.Term:
    match v1:
        case hydra.core.FloatValueBigfloat(value=v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("bigfloat"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueBigfloat(v))))))
        
        case hydra.core.FloatValueFloat32(value=v2):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float32"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(v2))))))
        
        case hydra.core.FloatValueFloat64(value=v3):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float64"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat64(v3))))))

def integer_value(v1: hydra.core.IntegerValue) -> hydra.core.Term:
    match v1:
        case hydra.core.IntegerValueBigint(value=v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("bigint"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueBigint(v))))))
        
        case hydra.core.IntegerValueInt8(value=v2):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int8"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt8(v2))))))
        
        case hydra.core.IntegerValueInt16(value=v3):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int16"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt16(v3))))))
        
        case hydra.core.IntegerValueInt32(value=v4):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int32"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(v4))))))
        
        case hydra.core.IntegerValueInt64(value=v5):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int64"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt64(v5))))))
        
        case hydra.core.IntegerValueUint8(value=v6):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint8"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueUint8(v6))))))
        
        case hydra.core.IntegerValueUint16(value=v7):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint16"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueUint16(v7))))))
        
        case hydra.core.IntegerValueUint32(value=v8):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint32"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueUint32(v8))))))
        
        case hydra.core.IntegerValueUint64(value=v9):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint64"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueUint64(v9))))))

def literal(v1: hydra.core.Literal) -> hydra.core.Term:
    match v1:
        case hydra.core.LiteralBinary(value=v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("binary"), hydra.core.TermLiteral(hydra.core.LiteralBinary(v)))))
        
        case hydra.core.LiteralBoolean(value=v2):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("boolean"), hydra.core.TermLiteral(hydra.core.LiteralBoolean(v2)))))
        
        case hydra.core.LiteralFloat(value=v3):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("float"), float_value(v3))))
        
        case hydra.core.LiteralInteger(value=v4):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("integer"), integer_value(v4))))
        
        case hydra.core.LiteralString(value=v5):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("string"), hydra.core.TermLiteral(hydra.core.LiteralString(v5)))))

def annotated_term(a: hydra.core.AnnotatedTerm) -> hydra.core.Term:
    return hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(term(a.body), a.annotation))

def application(app: hydra.core.Application) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Application"), (hydra.core.Field(hydra.core.Name("function"), term(app.function)), hydra.core.Field(hydra.core.Name("argument"), term(app.argument)))))

def binding(b: hydra.core.Binding) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Binding"), (hydra.core.Field(hydra.core.Name("name"), name(b.name)), hydra.core.Field(hydra.core.Name("term"), term(b.term)), hydra.core.Field(hydra.core.Name("type"), hydra.core.TermOptional(hydra.lib.optionals.map(type_scheme, b.type))))))

def case_statement(cs: hydra.core.CaseStatement) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.CaseStatement"), (hydra.core.Field(hydra.core.Name("typeName"), name(cs.type_name)), hydra.core.Field(hydra.core.Name("default"), hydra.core.TermOptional(hydra.lib.optionals.map(term, cs.default))), hydra.core.Field(hydra.core.Name("cases"), hydra.core.TermList(hydra.lib.lists.map(field, cs.cases))))))

def elimination(v1: hydra.core.Elimination) -> hydra.core.Term:
    match v1:
        case hydra.core.EliminationProduct(value=v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("product"), tuple_projection(v))))
        
        case hydra.core.EliminationRecord(value=v2):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("record"), projection(v2))))
        
        case hydra.core.EliminationUnion(value=v3):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("union"), case_statement(v3))))
        
        case hydra.core.EliminationWrap(value=v4):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("wrap"), name(v4))))

def field(f: hydra.core.Field) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Field"), (hydra.core.Field(hydra.core.Name("name"), hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), hydra.core.TermLiteral(hydra.core.LiteralString(f.name.value))))), hydra.core.Field(hydra.core.Name("term"), term(f.term)))))

def function(v1: hydra.core.Function) -> hydra.core.Term:
    match v1:
        case hydra.core.FunctionElimination(value=v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("elimination"), elimination(v))))
        
        case hydra.core.FunctionLambda(value=v2):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("lambda"), lambda_(v2))))
        
        case hydra.core.FunctionPrimitive(value=v3):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("primitive"), name(v3))))

def injection(i: hydra.core.Injection) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Injection"), (hydra.core.Field(hydra.core.Name("typeName"), name(i.type_name)), hydra.core.Field(hydra.core.Name("field"), field(i.field)))))

def lambda_(l: hydra.core.Lambda) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Lambda"), (hydra.core.Field(hydra.core.Name("parameter"), name(l.parameter)), hydra.core.Field(hydra.core.Name("domain"), hydra.core.TermOptional(hydra.lib.optionals.map(type, l.domain))), hydra.core.Field(hydra.core.Name("body"), term(l.body)))))

def let(l: hydra.core.Let) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Let"), (hydra.core.Field(hydra.core.Name("bindings"), hydra.core.TermList(hydra.lib.lists.map(binding, l.bindings))), hydra.core.Field(hydra.core.Name("body"), term(l.body)))))

def record(r: hydra.core.Record) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Record"), (hydra.core.Field(hydra.core.Name("typeName"), name(r.type_name)), hydra.core.Field(hydra.core.Name("fields"), hydra.core.TermList(hydra.lib.lists.map(field, r.fields))))))

def sum(s: hydra.core.Sum) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Sum"), (hydra.core.Field(hydra.core.Name("index"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(s.index)))), hydra.core.Field(hydra.core.Name("size"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(s.size)))), hydra.core.Field(hydra.core.Name("term"), term(s.term)))))

def term(v1: hydra.core.Term) -> hydra.core.Term:
    match v1:
        case hydra.core.TermAnnotated(value=v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("annotated"), annotated_term(v))))
        
        case hydra.core.TermApplication(value=v2):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("application"), application(v2))))
        
        case hydra.core.TermFunction(value=v3):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("function"), function(v3))))
        
        case hydra.core.TermLet(value=v4):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("let"), let(v4))))
        
        case hydra.core.TermLiteral(value=v5):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("literal"), literal(v5))))
        
        case hydra.core.TermList(value=v6):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("list"), hydra.core.TermList(hydra.lib.lists.map(term, v6)))))
        
        case hydra.core.TermMap(value=v7):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("map"), hydra.core.TermMap(hydra.lib.maps.bimap(term, term, v7)))))
        
        case hydra.core.TermOptional(value=v8):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("optional"), hydra.core.TermOptional(hydra.lib.optionals.map(term, v8)))))
        
        case hydra.core.TermProduct(value=v9):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("product"), hydra.core.TermList(hydra.lib.lists.map(term, v9)))))
        
        case hydra.core.TermRecord(value=v10):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("record"), record(v10))))
        
        case hydra.core.TermSet(value=v11):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("set"), hydra.core.TermSet(hydra.lib.sets.map(term, v11)))))
        
        case hydra.core.TermSum(value=v12):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("sum"), sum(v12))))
        
        case hydra.core.TermTypeLambda(value=v13):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeLambda"), type_lambda(v13))))
        
        case hydra.core.TermTypeApplication(value=v14):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeApplication"), type_application_term(v14))))
        
        case hydra.core.TermUnion(value=v15):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("union"), injection(v15))))
        
        case hydra.core.TermUnit(value=v16):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("unit"), (lambda _: hydra.core.TermUnit(None))(v16))))
        
        case hydra.core.TermVariable(value=v17):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("variable"), name(v17))))
        
        case hydra.core.TermWrap(value=v18):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("wrap"), wrapped_term(v18))))

def type_application_term(tt: hydra.core.TypeApplicationTerm) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeApplicationTerm"), (hydra.core.Field(hydra.core.Name("body"), term(tt.body)), hydra.core.Field(hydra.core.Name("type"), type(tt.type)))))

def type_lambda(l: hydra.core.TypeLambda) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeLambda"), (hydra.core.Field(hydra.core.Name("parameter"), name(l.parameter)), hydra.core.Field(hydra.core.Name("body"), term(l.body)))))

def wrapped_term(n: hydra.core.WrappedTerm) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedTerm"), (hydra.core.Field(hydra.core.Name("typeName"), name(n.type_name)), hydra.core.Field(hydra.core.Name("body"), term(n.body)))))

def annotated_type(at: hydra.core.AnnotatedType) -> hydra.core.Term:
    return hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(type(at.body), at.annotation))

def is_encoded_type(t: hydra.core.Term) -> bool:
    """Determines whether a given term is an encoded type."""
    
    match hydra.rewriting.deannotate_term(t):
        case hydra.core.TermApplication(value=a):
            return is_encoded_type(a.function)
        
        case hydra.core.TermUnion(value=i):
            return hydra.lib.equality.equal("hydra.core.Type", i.type_name.value)
        
        case _:
            return False

def is_type(t: hydra.core.Type) -> bool:
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
    match v1:
        case hydra.core.TermUnit():
            return True
        
        case _:
            return False

def is_unit_type(v1: hydra.core.Type) -> bool:
    match v1:
        case hydra.core.TypeUnit():
            return True
        
        case _:
            return False
