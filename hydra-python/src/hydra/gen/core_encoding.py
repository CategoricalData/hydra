"""Mapping of hydra.gen.core constructs in a host language like Haskell or Java  to their native Hydra counterparts as terms.  This includes an implementation of LambdaGraph's epsilon encoding (types to terms)."""

from __future__ import annotations
from collections.abc import Callable
import hydra.gen.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.optionals
import hydra.lib.sets
import hydra.gen.strip

def core_encode_annotated_term(a: hydra.gen.core.AnnotatedTerm) -> hydra.gen.core.Term:
    return hydra.gen.core.TermAnnotated(hydra.gen.core.AnnotatedTerm(core_encode_term(a.subject), a.annotation))

def core_encode_annotated_type(at: hydra.gen.core.AnnotatedType) -> hydra.gen.core.Term:
    return hydra.gen.core.TermAnnotated(hydra.gen.core.AnnotatedTerm(core_encode_type(at.subject), at.annotation))

def core_encode_application(app: hydra.gen.core.Application) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Application"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("function"), core_encode_term(app.function)),
      hydra.gen.core.Field(hydra.gen.core.Name("argument"), core_encode_term(app.argument))])))

def core_encode_application_type(at: hydra.gen.core.ApplicationType) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.ApplicationType"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("function"), core_encode_type(at.function)),
      hydra.gen.core.Field(hydra.gen.core.Name("argument"), core_encode_type(at.argument))])))

def core_encode_case_statement(cs: hydra.gen.core.CaseStatement) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.CaseStatement"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("typeName"), core_encode_name(cs.type_name)),
      hydra.gen.core.Field(hydra.gen.core.Name("default"), hydra.lib.optionals.map(lambda v1: core_encode_term(v1), cs.default)),
      hydra.gen.core.Field(hydra.gen.core.Name("cases"), hydra.lib.lists.map(lambda v1: core_encode_field(v1), cs.cases)))])))

def core_encode_elimination(v1: hydra.gen.core.Elimination) -> hydra.gen.core.Term:
    match v1:
        case hydra.gen.core.EliminationProduct(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Elimination"), hydra.gen.core.Field(hydra.gen.core.Name("product"), core_encode_tuple_projection(v))))
        
        case hydra.gen.core.EliminationRecord(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Elimination"), hydra.gen.core.Field(hydra.gen.core.Name("record"), core_encode_projection(v))))
        
        case hydra.gen.core.EliminationUnion(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Elimination"), hydra.gen.core.Field(hydra.gen.core.Name("union"), core_encode_case_statement(v))))
        
        case hydra.gen.core.EliminationWrap(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Elimination"), hydra.gen.core.Field(hydra.gen.core.Name("wrap"), core_encode_name(v))))

def core_encode_field(f: hydra.gen.core.Field) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Field"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("name"), hydra.gen.core.TermWrap(hydra.gen.core.WrappedTerm(hydra.gen.core.Name("hydra.gen.core.Name"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralString(f.name.value))))),
      hydra.gen.core.Field(hydra.gen.core.Name("term"), core_encode_term(f.term))])))

def core_encode_field_type(ft: hydra.gen.core.FieldType) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.FieldType"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("name"), core_encode_name(ft.name)),
      hydra.gen.core.Field(hydra.gen.core.Name("type"), core_encode_type(ft.type))])))

def core_encode_float_type(v1: hydra.gen.core.FloatType) -> hydra.gen.core.Term:
    match v1:
        case hydra.gen.core.FloatType.BIGFLOAT:
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.FloatType"), hydra.gen.core.Field(hydra.gen.core.Name("bigfloat"), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))))
        
        case hydra.gen.core.FloatType.FLOAT32:
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.FloatType"), hydra.gen.core.Field(hydra.gen.core.Name("float32"), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))))
        
        case hydra.gen.core.FloatType.FLOAT64:
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.FloatType"), hydra.gen.core.Field(hydra.gen.core.Name("float64"), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))))

def core_encode_float_value(v1: hydra.gen.core.FloatValue) -> hydra.gen.core.Term:
    match v1:
        case hydra.gen.core.FloatValueBigfloat(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.FloatValue"), hydra.gen.core.Field(hydra.gen.core.Name("bigfloat"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralFloat(hydra.gen.core.FloatValueBigfloat(v))))))
        
        case hydra.gen.core.FloatValueFloat32(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.FloatValue"), hydra.gen.core.Field(hydra.gen.core.Name("float32"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralFloat(hydra.gen.core.FloatValueFloat32(v))))))
        
        case hydra.gen.core.FloatValueFloat64(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.FloatValue"), hydra.gen.core.Field(hydra.gen.core.Name("float64"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralFloat(hydra.gen.core.FloatValueFloat64(v))))))

def core_encode_function(v1: hydra.gen.core.Function) -> hydra.gen.core.Term:
    match v1:
        case hydra.gen.core.FunctionElimination(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Function"), hydra.gen.core.Field(hydra.gen.core.Name("elimination"), core_encode_elimination(v))))
        
        case hydra.gen.core.FunctionLambda(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Function"), hydra.gen.core.Field(hydra.gen.core.Name("lambda"), core_encode_lambda(v))))
        
        case hydra.gen.core.FunctionPrimitive(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Function"), hydra.gen.core.Field(hydra.gen.core.Name("primitive"), core_encode_name(v))))

def core_encode_function_type(ft: hydra.gen.core.FunctionType) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.FunctionType"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("domain"), core_encode_type(ft.domain)),
      hydra.gen.core.Field(hydra.gen.core.Name("codomain"), core_encode_type(ft.codomain))])))

def core_encode_injection(i: hydra.gen.core.Injection) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Injection"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("typeName"), core_encode_name(i.type_name)),
      hydra.gen.core.Field(hydra.gen.core.Name("field"), core_encode_field(i.field))])))

def core_encode_integer_type(v1: hydra.gen.core.IntegerType) -> hydra.gen.core.Term:
    match v1:
        case hydra.gen.core.IntegerType.BIGINT:
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerType"), hydra.gen.core.Field(hydra.gen.core.Name("bigint"), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))))
        
        case hydra.gen.core.IntegerType.INT8:
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerType"), hydra.gen.core.Field(hydra.gen.core.Name("int8"), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))))
        
        case hydra.gen.core.IntegerType.INT16:
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerType"), hydra.gen.core.Field(hydra.gen.core.Name("int16"), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))))
        
        case hydra.gen.core.IntegerType.INT32:
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerType"), hydra.gen.core.Field(hydra.gen.core.Name("int32"), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))))
        
        case hydra.gen.core.IntegerType.INT64:
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerType"), hydra.gen.core.Field(hydra.gen.core.Name("int64"), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))))
        
        case hydra.gen.core.IntegerType.UINT8:
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerType"), hydra.gen.core.Field(hydra.gen.core.Name("uint8"), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))))
        
        case hydra.gen.core.IntegerType.UINT16:
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerType"), hydra.gen.core.Field(hydra.gen.core.Name("uint16"), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))))
        
        case hydra.gen.core.IntegerType.UINT32:
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerType"), hydra.gen.core.Field(hydra.gen.core.Name("uint32"), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))))
        
        case hydra.gen.core.IntegerType.UINT64:
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerType"), hydra.gen.core.Field(hydra.gen.core.Name("uint64"), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))))

def core_encode_integer_value(v1: hydra.gen.core.IntegerValue) -> hydra.gen.core.Term:
    match v1:
        case hydra.gen.core.IntegerValueBigint(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerValue"), hydra.gen.core.Field(hydra.gen.core.Name("bigint"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralInteger(hydra.gen.core.IntegerValueBigint(v))))))
        
        case hydra.gen.core.IntegerValueInt8(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerValue"), hydra.gen.core.Field(hydra.gen.core.Name("int8"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralInteger(hydra.gen.core.IntegerValueInt8(v))))))
        
        case hydra.gen.core.IntegerValueInt16(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerValue"), hydra.gen.core.Field(hydra.gen.core.Name("int16"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralInteger(hydra.gen.core.IntegerValueInt16(v))))))
        
        case hydra.gen.core.IntegerValueInt32(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerValue"), hydra.gen.core.Field(hydra.gen.core.Name("int32"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralInteger(hydra.gen.core.IntegerValueInt32(v))))))
        
        case hydra.gen.core.IntegerValueInt64(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerValue"), hydra.gen.core.Field(hydra.gen.core.Name("int64"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralInteger(hydra.gen.core.IntegerValueInt64(v))))))
        
        case hydra.gen.core.IntegerValueUint8(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerValue"), hydra.gen.core.Field(hydra.gen.core.Name("uint8"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralInteger(hydra.gen.core.IntegerValueUint8(v))))))
        
        case hydra.gen.core.IntegerValueUint16(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerValue"), hydra.gen.core.Field(hydra.gen.core.Name("uint16"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralInteger(hydra.gen.core.IntegerValueUint16(v))))))
        
        case hydra.gen.core.IntegerValueUint32(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerValue"), hydra.gen.core.Field(hydra.gen.core.Name("uint32"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralInteger(hydra.gen.core.IntegerValueUint32(v))))))
        
        case hydra.gen.core.IntegerValueUint64(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.IntegerValue"), hydra.gen.core.Field(hydra.gen.core.Name("uint64"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralInteger(hydra.gen.core.IntegerValueUint64(v))))))

def core_encode_lambda(l: hydra.gen.core.Lambda) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Lambda"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("parameter"), core_encode_name(l.parameter)),
      hydra.gen.core.Field(hydra.gen.core.Name("domain"), hydra.gen.core.TermOptional(hydra.lib.optionals.map(lambda v1: core_encode_type(v1), l.domain))),
      hydra.gen.core.Field(hydra.gen.core.Name("body"), core_encode_term(l.body))])))

def core_encode_forall_type(lt: hydra.gen.core.ForallType) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.ForallType"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("parameter"), core_encode_name(lt.parameter)),
      hydra.gen.core.Field(hydra.gen.core.Name("body"), core_encode_type(lt.body))])))

def core_encode_let(l: hydra.gen.core.Let) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Let"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("bindings"), hydra.gen.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_let_binding(v1), l.bindings))),
      hydra.gen.core.Field(hydra.gen.core.Name("environment"), core_encode_term(l.environment))])))

def core_encode_let_binding(b: hydra.gen.core.LetBinding) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.LetBinding"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("name"), core_encode_name(b.name)),
      hydra.gen.core.Field(hydra.gen.core.Name("term"), core_encode_term(b.term)),
      hydra.gen.core.Field(hydra.gen.core.Name("type"), hydra.gen.core.TermOptional(hydra.lib.optionals.map(lambda v1: core_encode_type_scheme(v1), b.type)))])))

def core_encode_literal(v1: hydra.gen.core.Literal) -> hydra.gen.core.Term:
    match v1:
        case hydra.gen.core.LiteralBinary(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Literal"), hydra.gen.core.Field(hydra.gen.core.Name("binary"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralBinary(v)))))
        
        case hydra.gen.core.LiteralBoolean(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Literal"), hydra.gen.core.Field(hydra.gen.core.Name("boolean"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralBoolean(v)))))
        
        case hydra.gen.core.LiteralFloat(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Literal"), hydra.gen.core.Field(hydra.gen.core.Name("float"), core_encode_float_value(v))))
        
        case hydra.gen.core.LiteralInteger(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Literal"), hydra.gen.core.Field(hydra.gen.core.Name("integer"), core_encode_integer_value(v))))
        
        case hydra.gen.core.LiteralString(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Literal"), hydra.gen.core.Field(hydra.gen.core.Name("string"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralString(v)))))

def core_encode_literal_type(v1: hydra.gen.core.LiteralType) -> hydra.gen.core.Term:
    match v1:
        case hydra.gen.core.LiteralTypeBinary(_):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.LiteralType"), hydra.gen.core.Field(hydra.gen.core.Name("binary"), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))))
        
        case hydra.gen.core.LiteralTypeBoolean(_):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.LiteralType"), hydra.gen.core.Field(hydra.gen.core.Name("boolean"), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))))
        
        case hydra.gen.core.LiteralTypeFloat(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.LiteralType"), hydra.gen.core.Field(hydra.gen.core.Name("float"), core_encode_float_type(v))))
        
        case hydra.gen.core.LiteralTypeInteger(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.LiteralType"), hydra.gen.core.Field(hydra.gen.core.Name("integer"), core_encode_integer_type(v))))
        
        case hydra.gen.core.LiteralTypeString(_):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.LiteralType"), hydra.gen.core.Field(hydra.gen.core.Name("string"), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))))

def core_encode_map_type(mt: hydra.gen.core.MapType) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.MapType"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("keys"), core_encode_type(mt.keys)),
      hydra.gen.core.Field(hydra.gen.core.Name("values"), core_encode_type(mt.values))])))

def core_encode_name(fn: hydra.gen.core.Name) -> hydra.gen.core.Term:
    return hydra.gen.core.TermWrap(hydra.gen.core.WrappedTerm(hydra.gen.core.Name("hydra.gen.core.Name"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralString(fn.value))))

def core_encode_projection(p: hydra.gen.core.Projection) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Projection"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("typeName"), core_encode_name(p.type_name)),
      hydra.gen.core.Field(hydra.gen.core.Name("field"), core_encode_name(p.field))])))

def core_encode_record(r: hydra.gen.core.Record) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Record"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("typeName"), core_encode_name(r.type_name)),
      hydra.gen.core.Field(hydra.gen.core.Name("fields"), hydra.gen.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_field(v1), r.fields)))])))

def core_encode_row_type(rt: hydra.gen.core.RowType) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.RowType"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("typeName"), core_encode_name(rt.type_name)),
      hydra.gen.core.Field(hydra.gen.core.Name("fields"), hydra.gen.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_field_type(v1), rt.fields)))])))

def core_encode_sum(s: hydra.gen.core.Sum) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Sum"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("index"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralInteger(hydra.gen.core.IntegerValueInt32(s.index)))),
      hydra.gen.core.Field(hydra.gen.core.Name("size"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralInteger(hydra.gen.core.IntegerValueInt32(s.size)))),
      hydra.gen.core.Field(hydra.gen.core.Name("term"), core_encode_term(s.term))])))

def core_encode_term(v1: hydra.gen.core.Term) -> hydra.gen.core.Term:
    match v1:
        case hydra.gen.core.TermAnnotated(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("annotated"), core_encode_annotated_term(v))))
        
        case hydra.gen.core.TermApplication(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("application"), core_encode_application(v))))
        
        case hydra.gen.core.TermFunction(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("function"), core_encode_function(v))))
        
        case hydra.gen.core.TermLet(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("let"), core_encode_let(v))))
        
        case hydra.gen.core.TermLiteral(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("literal"), core_encode_literal(v))))
        
        case hydra.gen.core.TermList(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("list"), hydra.gen.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_term(v1), v)))))
        
        case hydra.gen.core.TermMap(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("map"), hydra.gen.core.TermMap(hydra.lib.maps.bimap(lambda v1: core_encode_term(v1), lambda v1: core_encode_term(v1), v)))))
        
        case hydra.gen.core.TermOptional(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("optional"), hydra.gen.core.TermOptional(hydra.lib.optionals.map(lambda v1: core_encode_term(v1), v)))))
        
        case hydra.gen.core.TermProduct(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("product"), hydra.gen.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_term(v1), v)))))
        
        case hydra.gen.core.TermRecord(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("record"), core_encode_record(v))))
        
        case hydra.gen.core.TermSet(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("set"), hydra.gen.core.TermSet(hydra.lib.sets.map(lambda v1: core_encode_term(v1), v)))))
        
        case hydra.gen.core.TermSum(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("sum"), core_encode_sum(v))))
        
        case hydra.gen.core.TermTypeAbstraction(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("typeAbstraction"), core_encode_type_abstraction(v))))
        
        case hydra.gen.core.TermTypeApplication(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("typeApplication"), core_encode_typed_term(v))))
        
        case hydra.gen.core.TermTyped(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("typed"), core_encode_typed_term(v))))
        
        case hydra.gen.core.TermUnion(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("union"), core_encode_injection(v))))
        
        case hydra.gen.core.TermVariable(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("variable"), core_encode_name(v))))
        
        case hydra.gen.core.TermWrap(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Term"), hydra.gen.core.Field(hydra.gen.core.Name("wrap"), core_encode_wrapped_term(v))))

def core_encode_tuple_projection(tp: hydra.gen.core.TupleProjection) -> hydra.gen.core.Term:
    def encode_types(types: frozenlist[hydra.gen.core.Type]) -> hydra.gen.core.Term:
        return hydra.gen.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_type(v1), types))
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.TupleProjection"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("arity"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralInteger(hydra.gen.core.IntegerValueInt32(tp.arity)))),
      hydra.gen.core.Field(hydra.gen.core.Name("index"), hydra.gen.core.TermLiteral(hydra.gen.core.LiteralInteger(hydra.gen.core.IntegerValueInt32(tp.index)))),
      hydra.gen.core.Field(hydra.gen.core.Name("domain"), hydra.gen.core.TermOptional(hydra.lib.optionals.map(encode_types, tp.domain)))])))

def core_encode_type(v1: hydra.gen.core.Type) -> hydra.gen.core.Term:
    match v1:
        case hydra.gen.core.TypeAnnotated(v):
            return hydra.gen.core.TermAnnotated(hydra.gen.core.AnnotatedTerm(core_encode_type(v.subject), v.annotation))
        
        case hydra.gen.core.TypeApplication(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Type"), hydra.gen.core.Field(hydra.gen.core.Name("application"), core_encode_application_type(v))))
        
        case hydra.gen.core.TypeFunction(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Type"), hydra.gen.core.Field(hydra.gen.core.Name("function"), core_encode_function_type(v))))
        
        case hydra.gen.core.TypeForall(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Type"), hydra.gen.core.Field(hydra.gen.core.Name("forall"), core_encode_forall_type(v))))
        
        case hydra.gen.core.TypeList(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Type"), hydra.gen.core.Field(hydra.gen.core.Name("list"), core_encode_type(v))))
        
        case hydra.gen.core.TypeLiteral(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Type"), hydra.gen.core.Field(hydra.gen.core.Name("literal"), core_encode_literal_type(v))))
        
        case hydra.gen.core.TypeMap(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Type"), hydra.gen.core.Field(hydra.gen.core.Name("map"), core_encode_map_type(v))))
        
        case hydra.gen.core.TypeOptional(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Type"), hydra.gen.core.Field(hydra.gen.core.Name("optional"), core_encode_type(v))))
        
        case hydra.gen.core.TypeProduct(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Type"), hydra.gen.core.Field(hydra.gen.core.Name("product"), hydra.gen.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_type(v1), v)))))
        
        case hydra.gen.core.TypeRecord(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Type"), hydra.gen.core.Field(hydra.gen.core.Name("record"), core_encode_row_type(v))))
        
        case hydra.gen.core.TypeSet(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Type"), hydra.gen.core.Field(hydra.gen.core.Name("set"), core_encode_type(v))))
        
        case hydra.gen.core.TypeSum(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Type"), hydra.gen.core.Field(hydra.gen.core.Name("sum"), hydra.gen.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_type(v1), v)))))
        
        case hydra.gen.core.TypeUnion(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Type"), hydra.gen.core.Field(hydra.gen.core.Name("union"), core_encode_row_type(v))))
        
        case hydra.gen.core.TypeVariable(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Type"), hydra.gen.core.Field(hydra.gen.core.Name("variable"), core_encode_name(v))))
        
        case hydra.gen.core.TypeWrap(v):
            return hydra.gen.core.TermUnion(hydra.gen.core.Injection(hydra.gen.core.Name("hydra.gen.core.Type"), hydra.gen.core.Field(hydra.gen.core.Name("wrap"), core_encode_wrapped_type(v))))

def core_encode_type_abstraction(l: hydra.gen.core.TypeAbstraction) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.TypeAbstraction"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("parameter"), core_encode_name(l.parameter)),
      hydra.gen.core.Field(hydra.gen.core.Name("body"), core_encode_term(l.body))])))

def core_encode_type_scheme(ts: hydra.gen.core.TypeScheme) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.TypeScheme"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("variables"), hydra.gen.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_name(v1), ts.variables))),
      hydra.gen.core.Field(hydra.gen.core.Name("type"), core_encode_type(ts.type))])))

def core_encode_typed_term(tt: hydra.gen.core.TypedTerm) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.TypedTerm"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("term"), core_encode_term(tt.term)),
      hydra.gen.core.Field(hydra.gen.core.Name("type"), core_encode_type(tt.type))])))

def core_encode_wrapped_term(n: hydra.gen.core.WrappedTerm) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.WrappedTerm"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("typeName"), core_encode_name(n.type_name)),
      hydra.gen.core.Field(hydra.gen.core.Name("object"), core_encode_term(n.object))])))

def core_encode_wrapped_type(nt: hydra.gen.core.WrappedType) -> hydra.gen.core.Term:
    return hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.WrappedType"), tuple([
      hydra.gen.core.Field(hydra.gen.core.Name("typeName"), core_encode_name(nt.type_name)),
      hydra.gen.core.Field(hydra.gen.core.Name("object"), core_encode_type(nt.object))])))

def is_encoded_type(t: hydra.gen.core.Term) -> bool:
    match hydra.strip.strip_term(t):
        case hydra.gen.core.TermApplication(a):
            return is_encoded_type(a.function)
        
        case hydra.gen.core.TermUnion(i):
            return hydra.lib.equality.equal_string("hydra.gen.core.Type", i.type_name.value)
        
        case _:
            return False

def is_type(t: hydra.gen.core.Type) -> bool:
    match hydra.strip.strip_type(t):
        case hydra.gen.core.TypeApplication(a):
            return is_type(a.function)
        
        case hydra.gen.core.TypeForall(l):
            return is_type(l.body)
        
        case hydra.gen.core.TypeUnion(rt):
            return hydra.lib.equality.equal_string("hydra.gen.core.Type", rt.type_name.value)
        
        case _:
            return False

def is_unit_term(t: hydra.gen.core.Term) -> bool:
    return hydra.lib.equality.equal_term(hydra.strip.fully_strip_term(t), hydra.gen.core.TermRecord(hydra.gen.core.Record(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))

def is_unit_type(t: hydra.gen.core.Type) -> bool:
    return hydra.lib.equality.equal_type(hydra.strip.strip_type(t), hydra.gen.core.TypeRecord(hydra.gen.core.RowType(hydra.gen.core.Name("hydra.gen.core.Unit"), tuple([]))))
