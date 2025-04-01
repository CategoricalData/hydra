"""Mapping of hydra.core constructs in a host language like Haskell or Java  to their native Hydra counterparts as terms.  This includes an implementation of LambdaGraph's epsilon encoding (types to terms)."""

from __future__ import annotations
from collections.abc import Callable
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.optionals
import hydra.lib.sets
import hydra.strip

def core_encode_annotated_term(a: hydra.core.AnnotatedTerm) -> hydra.core.Term:
    return hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(core_encode_term(a.subject), a.annotation))

def core_encode_annotated_type(at: hydra.core.AnnotatedType) -> hydra.core.Term:
    return hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(core_encode_type(at.subject), at.annotation))

def core_encode_application(app: hydra.core.Application) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Application"), tuple([
      hydra.core.Field(hydra.core.Name("function"), core_encode_term(app.function)),
      hydra.core.Field(hydra.core.Name("argument"), core_encode_term(app.argument))])))

def core_encode_application_type(at: hydra.core.ApplicationType) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ApplicationType"), tuple([
      hydra.core.Field(hydra.core.Name("function"), core_encode_type(at.function)),
      hydra.core.Field(hydra.core.Name("argument"), core_encode_type(at.argument))])))

def core_encode_case_statement(cs: hydra.core.CaseStatement) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.CaseStatement"), tuple([
      hydra.core.Field(hydra.core.Name("typeName"), core_encode_name(cs.type_name)),
      hydra.core.Field(hydra.core.Name("default"), hydra.core.TermOptional(hydra.lib.optionals.map(lambda v1: core_encode_term(v1), cs.default))),
      hydra.core.Field(hydra.core.Name("cases"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_field(v1), cs.cases)))])))

def core_encode_elimination(v1: hydra.core.Elimination) -> hydra.core.Term:
    match v1:
        case hydra.core.EliminationProduct(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("product"), core_encode_tuple_projection(v))))
        
        case hydra.core.EliminationRecord(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("record"), core_encode_projection(v))))
        
        case hydra.core.EliminationUnion(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("union"), core_encode_case_statement(v))))
        
        case hydra.core.EliminationWrap(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("wrap"), core_encode_name(v))))

def core_encode_field(f: hydra.core.Field) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Field"), tuple([
      hydra.core.Field(hydra.core.Name("name"), hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), hydra.core.TermLiteral(hydra.core.LiteralString(f.name.value))))),
      hydra.core.Field(hydra.core.Name("term"), core_encode_term(f.term))])))

def core_encode_field_type(ft: hydra.core.FieldType) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FieldType"), tuple([
      hydra.core.Field(hydra.core.Name("name"), core_encode_name(ft.name)),
      hydra.core.Field(hydra.core.Name("type"), core_encode_type(ft.type))])))

def core_encode_float_type(v1: hydra.core.FloatType) -> hydra.core.Term:
    match v1:
        case hydra.core.FloatType.BIGFLOAT:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("bigfloat"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.FloatType.FLOAT32:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float32"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.FloatType.FLOAT64:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float64"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))

def core_encode_float_value(v1: hydra.core.FloatValue) -> hydra.core.Term:
    match v1:
        case hydra.core.FloatValueBigfloat(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("bigfloat"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueBigfloat(v))))))
        
        case hydra.core.FloatValueFloat32(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float32"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(v))))))
        
        case hydra.core.FloatValueFloat64(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float64"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat64(v))))))

def core_encode_function(v1: hydra.core.Function) -> hydra.core.Term:
    match v1:
        case hydra.core.FunctionElimination(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("elimination"), core_encode_elimination(v))))
        
        case hydra.core.FunctionLambda(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("lambda"), core_encode_lambda(v))))
        
        case hydra.core.FunctionPrimitive(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("primitive"), core_encode_name(v))))

def core_encode_function_type(ft: hydra.core.FunctionType) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FunctionType"), tuple([
      hydra.core.Field(hydra.core.Name("domain"), core_encode_type(ft.domain)),
      hydra.core.Field(hydra.core.Name("codomain"), core_encode_type(ft.codomain))])))

def core_encode_injection(i: hydra.core.Injection) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Injection"), tuple([
      hydra.core.Field(hydra.core.Name("typeName"), core_encode_name(i.type_name)),
      hydra.core.Field(hydra.core.Name("field"), core_encode_field(i.field))])))

def core_encode_integer_type(v1: hydra.core.IntegerType) -> hydra.core.Term:
    match v1:
        case hydra.core.IntegerType.BIGINT:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("bigint"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerType.INT8:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int8"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerType.INT16:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int16"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerType.INT32:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int32"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerType.INT64:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int64"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerType.UINT8:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint8"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerType.UINT16:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint16"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerType.UINT32:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint32"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerType.UINT64:
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint64"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))

def core_encode_integer_value(v1: hydra.core.IntegerValue) -> hydra.core.Term:
    match v1:
        case hydra.core.IntegerValueBigint(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("bigint"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueBigint(v))))))
        
        case hydra.core.IntegerValueInt8(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int8"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt8(v))))))
        
        case hydra.core.IntegerValueInt16(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int16"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt16(v))))))
        
        case hydra.core.IntegerValueInt32(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int32"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(v))))))
        
        case hydra.core.IntegerValueInt64(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int64"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt64(v))))))
        
        case hydra.core.IntegerValueUint8(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint8"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueUint8(v))))))
        
        case hydra.core.IntegerValueUint16(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint16"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueUint16(v))))))
        
        case hydra.core.IntegerValueUint32(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint32"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueUint32(v))))))
        
        case hydra.core.IntegerValueUint64(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint64"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueUint64(v))))))

def core_encode_lambda(l: hydra.core.Lambda) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Lambda"), tuple([
      hydra.core.Field(hydra.core.Name("parameter"), core_encode_name(l.parameter)),
      hydra.core.Field(hydra.core.Name("domain"), hydra.core.TermOptional(hydra.lib.optionals.map(lambda v1: core_encode_type(v1), l.domain))),
      hydra.core.Field(hydra.core.Name("body"), core_encode_term(l.body))])))

def core_encode_forall_type(lt: hydra.core.ForallType) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ForallType"), tuple([
      hydra.core.Field(hydra.core.Name("parameter"), core_encode_name(lt.parameter)),
      hydra.core.Field(hydra.core.Name("body"), core_encode_type(lt.body))])))

def core_encode_let(l: hydra.core.Let) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Let"), tuple([
      hydra.core.Field(hydra.core.Name("bindings"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_let_binding(v1), l.bindings))),
      hydra.core.Field(hydra.core.Name("environment"), core_encode_term(l.environment))])))

def core_encode_let_binding(b: hydra.core.LetBinding) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.LetBinding"), tuple([
      hydra.core.Field(hydra.core.Name("name"), core_encode_name(b.name)),
      hydra.core.Field(hydra.core.Name("term"), core_encode_term(b.term)),
      hydra.core.Field(hydra.core.Name("type"), hydra.core.TermOptional(hydra.lib.optionals.map(lambda v1: core_encode_type_scheme(v1), b.type)))])))

def core_encode_literal(v1: hydra.core.Literal) -> hydra.core.Term:
    match v1:
        case hydra.core.LiteralBinary(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("binary"), hydra.core.TermLiteral(hydra.core.LiteralBinary(v)))))
        
        case hydra.core.LiteralBoolean(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("boolean"), hydra.core.TermLiteral(hydra.core.LiteralBoolean(v)))))
        
        case hydra.core.LiteralFloat(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("float"), core_encode_float_value(v))))
        
        case hydra.core.LiteralInteger(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("integer"), core_encode_integer_value(v))))
        
        case hydra.core.LiteralString(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("string"), hydra.core.TermLiteral(hydra.core.LiteralString(v)))))

def core_encode_literal_type(v1: hydra.core.LiteralType) -> hydra.core.Term:
    match v1:
        case hydra.core.LiteralTypeBinary(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("binary"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.LiteralTypeBoolean(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("boolean"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.LiteralTypeFloat(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("float"), core_encode_float_type(v))))
        
        case hydra.core.LiteralTypeInteger(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("integer"), core_encode_integer_type(v))))
        
        case hydra.core.LiteralTypeString(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("string"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))

def core_encode_map_type(mt: hydra.core.MapType) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.MapType"), tuple([
      hydra.core.Field(hydra.core.Name("keys"), core_encode_type(mt.keys)),
      hydra.core.Field(hydra.core.Name("values"), core_encode_type(mt.values))])))

def core_encode_name(fn: hydra.core.Name) -> hydra.core.Term:
    return hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), hydra.core.TermLiteral(hydra.core.LiteralString(fn.value))))

def core_encode_projection(p: hydra.core.Projection) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Projection"), tuple([
      hydra.core.Field(hydra.core.Name("typeName"), core_encode_name(p.type_name)),
      hydra.core.Field(hydra.core.Name("field"), core_encode_name(p.field))])))

def core_encode_record(r: hydra.core.Record) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Record"), tuple([
      hydra.core.Field(hydra.core.Name("typeName"), core_encode_name(r.type_name)),
      hydra.core.Field(hydra.core.Name("fields"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_field(v1), r.fields)))])))

def core_encode_row_type(rt: hydra.core.RowType) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.RowType"), tuple([
      hydra.core.Field(hydra.core.Name("typeName"), core_encode_name(rt.type_name)),
      hydra.core.Field(hydra.core.Name("fields"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_field_type(v1), rt.fields)))])))

def core_encode_sum(s: hydra.core.Sum) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Sum"), tuple([
      hydra.core.Field(hydra.core.Name("index"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(s.index)))),
      hydra.core.Field(hydra.core.Name("size"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(s.size)))),
      hydra.core.Field(hydra.core.Name("term"), core_encode_term(s.term))])))

def core_encode_term(v1: hydra.core.Term) -> hydra.core.Term:
    match v1:
        case hydra.core.TermAnnotated(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("annotated"), core_encode_annotated_term(v))))
        
        case hydra.core.TermApplication(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("application"), core_encode_application(v))))
        
        case hydra.core.TermFunction(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("function"), core_encode_function(v))))
        
        case hydra.core.TermLet(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("let"), core_encode_let(v))))
        
        case hydra.core.TermLiteral(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("literal"), core_encode_literal(v))))
        
        case hydra.core.TermList(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("list"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_term(v1), v)))))
        
        case hydra.core.TermMap(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("map"), hydra.core.TermMap(hydra.lib.maps.bimap(lambda v1: core_encode_term(v1), lambda v1: core_encode_term(v1), v)))))
        
        case hydra.core.TermOptional(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("optional"), hydra.core.TermOptional(hydra.lib.optionals.map(lambda v1: core_encode_term(v1), v)))))
        
        case hydra.core.TermProduct(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("product"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_term(v1), v)))))
        
        case hydra.core.TermRecord(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("record"), core_encode_record(v))))
        
        case hydra.core.TermSet(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("set"), hydra.core.TermSet(hydra.lib.sets.map(lambda v1: core_encode_term(v1), v)))))
        
        case hydra.core.TermSum(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("sum"), core_encode_sum(v))))
        
        case hydra.core.TermTypeAbstraction(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeAbstraction"), core_encode_type_abstraction(v))))
        
        case hydra.core.TermTypeApplication(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeApplication"), core_encode_typed_term(v))))
        
        case hydra.core.TermTyped(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typed"), core_encode_typed_term(v))))
        
        case hydra.core.TermUnion(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("union"), core_encode_injection(v))))
        
        case hydra.core.TermVariable(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("variable"), core_encode_name(v))))
        
        case hydra.core.TermWrap(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("wrap"), core_encode_wrapped_term(v))))

def core_encode_tuple_projection(tp: hydra.core.TupleProjection) -> hydra.core.Term:
    def encode_types(types: frozenlist[hydra.core.Type]) -> hydra.core.Term:
        return hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_type(v1), types))
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TupleProjection"), tuple([
      hydra.core.Field(hydra.core.Name("arity"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(tp.arity)))),
      hydra.core.Field(hydra.core.Name("index"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(tp.index)))),
      hydra.core.Field(hydra.core.Name("domain"), hydra.core.TermOptional(hydra.lib.optionals.map(encode_types, tp.domain)))])))

def core_encode_type(v1: hydra.core.Type) -> hydra.core.Term:
    match v1:
        case hydra.core.TypeAnnotated(v):
            return hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(core_encode_type(v.subject), v.annotation))
        
        case hydra.core.TypeApplication(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("application"), core_encode_application_type(v))))
        
        case hydra.core.TypeFunction(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("function"), core_encode_function_type(v))))
        
        case hydra.core.TypeForall(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("forall"), core_encode_forall_type(v))))
        
        case hydra.core.TypeList(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("list"), core_encode_type(v))))
        
        case hydra.core.TypeLiteral(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("literal"), core_encode_literal_type(v))))
        
        case hydra.core.TypeMap(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("map"), core_encode_map_type(v))))
        
        case hydra.core.TypeOptional(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("optional"), core_encode_type(v))))
        
        case hydra.core.TypeProduct(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("product"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_type(v1), v)))))
        
        case hydra.core.TypeRecord(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("record"), core_encode_row_type(v))))
        
        case hydra.core.TypeSet(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("set"), core_encode_type(v))))
        
        case hydra.core.TypeSum(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("sum"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_type(v1), v)))))
        
        case hydra.core.TypeUnion(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("union"), core_encode_row_type(v))))
        
        case hydra.core.TypeVariable(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("variable"), core_encode_name(v))))
        
        case hydra.core.TypeWrap(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("wrap"), core_encode_wrapped_type(v))))

def core_encode_type_abstraction(l: hydra.core.TypeAbstraction) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeAbstraction"), tuple([
      hydra.core.Field(hydra.core.Name("parameter"), core_encode_name(l.parameter)),
      hydra.core.Field(hydra.core.Name("body"), core_encode_term(l.body))])))

def core_encode_type_scheme(ts: hydra.core.TypeScheme) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeScheme"), tuple([
      hydra.core.Field(hydra.core.Name("variables"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_name(v1), ts.variables))),
      hydra.core.Field(hydra.core.Name("type"), core_encode_type(ts.type))])))

def core_encode_typed_term(tt: hydra.core.TypedTerm) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypedTerm"), tuple([
      hydra.core.Field(hydra.core.Name("term"), core_encode_term(tt.term)),
      hydra.core.Field(hydra.core.Name("type"), core_encode_type(tt.type))])))

def core_encode_wrapped_term(n: hydra.core.WrappedTerm) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedTerm"), tuple([
      hydra.core.Field(hydra.core.Name("typeName"), core_encode_name(n.type_name)),
      hydra.core.Field(hydra.core.Name("object"), core_encode_term(n.object))])))

def core_encode_wrapped_type(nt: hydra.core.WrappedType) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedType"), tuple([
      hydra.core.Field(hydra.core.Name("typeName"), core_encode_name(nt.type_name)),
      hydra.core.Field(hydra.core.Name("object"), core_encode_type(nt.object))])))

def is_encoded_type(t: hydra.core.Term) -> bool:
    match hydra.strip.strip_term(t):
        case hydra.core.TermApplication(a):
            return is_encoded_type(a.function)
        
        case hydra.core.TermUnion(i):
            return hydra.lib.equality.equal_string("hydra.core.Type", i.type_name.value)
        
        case _:
            return False

def is_type(t: hydra.core.Type) -> bool:
    match hydra.strip.strip_type(t):
        case hydra.core.TypeApplication(a):
            return is_type(a.function)
        
        case hydra.core.TypeForall(l):
            return is_type(l.body)
        
        case hydra.core.TypeUnion(rt):
            return hydra.lib.equality.equal_string("hydra.core.Type", rt.type_name.value)
        
        case _:
            return False

def is_unit_term(t: hydra.core.Term) -> bool:
    return hydra.lib.equality.equal_term(hydra.strip.fully_strip_term(t), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))

def is_unit_type(t: hydra.core.Type) -> bool:
    return hydra.lib.equality.equal_type(hydra.strip.strip_type(t), hydra.core.TypeRecord(hydra.core.RowType(hydra.core.Name("hydra.core.Unit"), tuple([]))))
