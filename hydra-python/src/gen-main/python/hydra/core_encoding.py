"""Mapping of hydra.core constructs in a host language like Haskell or Java  to their native Hydra counterparts as terms.  This includes an implementation of LambdaGraph's epsilon encoding (types to terms)."""

from __future__ import annotations
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.optionals
import hydra.lib.sets
import hydra.strip

def core_encode_annotated_term(a):
    return hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(core_encode_term(A.subject), A.annotation))

def core_encode_annotated_type(at):
    return hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(core_encode_type(At.subject), At.annotation))

def core_encode_application(app):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Application"), tuple([
      hydra.core.Field(hydra.core.Name("function"), core_encode_term(App.function)),
      hydra.core.Field(hydra.core.Name("argument"), core_encode_term(App.argument))])))

def core_encode_application_type(at):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ApplicationType"), tuple([
      hydra.core.Field(hydra.core.Name("function"), core_encode_type(At.function)),
      hydra.core.Field(hydra.core.Name("argument"), core_encode_type(At.argument))])))

def core_encode_case_statement(cs):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.CaseStatement"), tuple([
      hydra.core.Field(hydra.core.Name("typeName"), core_encode_name(Cs.type_name)),
      hydra.core.Field(hydra.core.Name("default"), hydra.core.TermOptional(hydra.lib.optionals.map(lambda v1: core_encode_term(V1))(Cs.default))),
      hydra.core.Field(hydra.core.Name("cases"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_field(V1))(Cs.cases)))])))

def core_encode_elimination(v1):
    match V1:
        case hydra.core.EliminationList(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("list"), core_encode_term(V))))
        
        case hydra.core.EliminationOptional(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("optional"), core_encode_optional_cases(V))))
        
        case hydra.core.EliminationProduct(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("product"), core_encode_tuple_projection(V))))
        
        case hydra.core.EliminationRecord(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("record"), core_encode_projection(V))))
        
        case hydra.core.EliminationUnion(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("union"), core_encode_case_statement(V))))
        
        case hydra.core.EliminationWrap(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("wrap"), core_encode_name(V))))
        
        case _:
            raise TypeError("Unsupported Elimination")

def core_encode_field(f):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Field"), tuple([
      hydra.core.Field(hydra.core.Name("name"), hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), hydra.core.TermLiteral(hydra.core.LiteralString(F.name.value))))),
      hydra.core.Field(hydra.core.Name("term"), core_encode_term(F.term))])))

def core_encode_field_type(ft):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FieldType"), tuple([
      hydra.core.Field(hydra.core.Name("name"), core_encode_name(Ft.name)),
      hydra.core.Field(hydra.core.Name("type"), core_encode_type(Ft.type))])))

def core_encode_float_type(v1):
    match V1:
        case hydra.core.FloatTypeBigfloat(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("bigfloat"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.FloatTypeFloat32(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float32"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.FloatTypeFloat64(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float64"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case _:
            raise TypeError("Unsupported FloatType")

def core_encode_float_value(v1):
    match V1:
        case hydra.core.FloatValueBigfloat(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("bigfloat"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueBigfloat(V))))))
        
        case hydra.core.FloatValueFloat32(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float32"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(V))))))
        
        case hydra.core.FloatValueFloat64(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float64"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat64(V))))))
        
        case _:
            raise TypeError("Unsupported FloatValue")

def core_encode_function(v1):
    match V1:
        case hydra.core.FunctionElimination(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("elimination"), core_encode_elimination(V))))
        
        case hydra.core.FunctionLambda(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("lambda"), core_encode_lambda(V))))
        
        case hydra.core.FunctionPrimitive(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("primitive"), core_encode_name(V))))
        
        case _:
            raise TypeError("Unsupported Function")

def core_encode_function_type(ft):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FunctionType"), tuple([
      hydra.core.Field(hydra.core.Name("domain"), core_encode_type(Ft.domain)),
      hydra.core.Field(hydra.core.Name("codomain"), core_encode_type(Ft.codomain))])))

def core_encode_injection(i):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Injection"), tuple([
      hydra.core.Field(hydra.core.Name("typeName"), core_encode_name(I.type_name)),
      hydra.core.Field(hydra.core.Name("field"), core_encode_field(I.field))])))

def core_encode_integer_type(v1):
    match V1:
        case hydra.core.IntegerTypeBigint(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("bigint"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerTypeInt8(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int8"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerTypeInt16(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int16"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerTypeInt32(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int32"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerTypeInt64(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int64"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerTypeUint8(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint8"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerTypeUint16(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint16"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerTypeUint32(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint32"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.IntegerTypeUint64(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint64"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case _:
            raise TypeError("Unsupported IntegerType")

def core_encode_integer_value(v1):
    match V1:
        case hydra.core.IntegerValueBigint(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("bigint"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueBigint(V))))))
        
        case hydra.core.IntegerValueInt8(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int8"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt8(V))))))
        
        case hydra.core.IntegerValueInt16(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int16"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt16(V))))))
        
        case hydra.core.IntegerValueInt32(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int32"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(V))))))
        
        case hydra.core.IntegerValueInt64(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("int64"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt64(V))))))
        
        case hydra.core.IntegerValueUint8(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint8"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueUint8(V))))))
        
        case hydra.core.IntegerValueUint16(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint16"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueUint16(V))))))
        
        case hydra.core.IntegerValueUint32(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint32"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueUint32(V))))))
        
        case hydra.core.IntegerValueUint64(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(hydra.core.Name("uint64"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueUint64(V))))))
        
        case _:
            raise TypeError("Unsupported IntegerValue")

def core_encode_lambda(l):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Lambda"), tuple([
      hydra.core.Field(hydra.core.Name("parameter"), core_encode_name(L.parameter)),
      hydra.core.Field(hydra.core.Name("domain"), hydra.core.TermOptional(hydra.lib.optionals.map(lambda v1: core_encode_type(V1))(L.domain))),
      hydra.core.Field(hydra.core.Name("body"), core_encode_term(L.body))])))

def core_encode_lambda_type(lt):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.LambdaType"), tuple([
      hydra.core.Field(hydra.core.Name("parameter"), core_encode_name(Lt.parameter)),
      hydra.core.Field(hydra.core.Name("body"), core_encode_type(Lt.body))])))

def core_encode_let(l):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Let"), tuple([
      hydra.core.Field(hydra.core.Name("bindings"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_let_binding(V1))(L.bindings))),
      hydra.core.Field(hydra.core.Name("environment"), core_encode_term(L.environment))])))

def core_encode_let_binding(b):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.LetBinding"), tuple([
      hydra.core.Field(hydra.core.Name("name"), core_encode_name(B.name)),
      hydra.core.Field(hydra.core.Name("term"), core_encode_term(B.term)),
      hydra.core.Field(hydra.core.Name("type"), hydra.core.TermOptional(hydra.lib.optionals.map(lambda v1: core_encode_type_scheme(V1))(B.type)))])))

def core_encode_literal(v1):
    match V1:
        case hydra.core.LiteralBinary(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("binary"), hydra.core.TermLiteral(hydra.core.LiteralBinary(V)))))
        
        case hydra.core.LiteralBoolean(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("boolean"), hydra.core.TermLiteral(hydra.core.LiteralBoolean(V)))))
        
        case hydra.core.LiteralFloat(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("float"), core_encode_float_value(V))))
        
        case hydra.core.LiteralInteger(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("integer"), core_encode_integer_value(V))))
        
        case hydra.core.LiteralString(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("string"), hydra.core.TermLiteral(hydra.core.LiteralString(V)))))
        
        case _:
            raise TypeError("Unsupported Literal")

def core_encode_literal_type(v1):
    match V1:
        case hydra.core.LiteralTypeBinary(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("binary"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.LiteralTypeBoolean(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("boolean"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case hydra.core.LiteralTypeFloat(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("float"), core_encode_float_type(V))))
        
        case hydra.core.LiteralTypeInteger(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("integer"), core_encode_integer_type(V))))
        
        case hydra.core.LiteralTypeString(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("string"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))))
        
        case _:
            raise TypeError("Unsupported LiteralType")

def core_encode_map_type(mt):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.MapType"), tuple([
      hydra.core.Field(hydra.core.Name("keys"), core_encode_type(Mt.keys)),
      hydra.core.Field(hydra.core.Name("values"), core_encode_type(Mt.values))])))

def core_encode_name(fn):
    return hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), hydra.core.TermLiteral(hydra.core.LiteralString(Fn.value))))

def core_encode_optional_cases(oc):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.OptionalCases"), tuple([
      hydra.core.Field(hydra.core.Name("nothing"), core_encode_term(Oc.nothing)),
      hydra.core.Field(hydra.core.Name("just"), core_encode_term(Oc.just))])))

def core_encode_projection(p):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Projection"), tuple([
      hydra.core.Field(hydra.core.Name("typeName"), core_encode_name(P.type_name)),
      hydra.core.Field(hydra.core.Name("field"), core_encode_name(P.field))])))

def core_encode_record(r):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Record"), tuple([
      hydra.core.Field(hydra.core.Name("typeName"), core_encode_name(R.type_name)),
      hydra.core.Field(hydra.core.Name("fields"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_field(V1))(R.fields)))])))

def core_encode_row_type(rt):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.RowType"), tuple([
      hydra.core.Field(hydra.core.Name("typeName"), core_encode_name(Rt.type_name)),
      hydra.core.Field(hydra.core.Name("fields"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_field_type(V1))(Rt.fields)))])))

def core_encode_sum(s):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Sum"), tuple([
      hydra.core.Field(hydra.core.Name("index"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(S.index)))),
      hydra.core.Field(hydra.core.Name("size"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(S.size)))),
      hydra.core.Field(hydra.core.Name("term"), core_encode_term(S.term))])))

def core_encode_term(v1):
    match V1:
        case hydra.core.TermAnnotated(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("annotated"), core_encode_annotated_term(V))))
        
        case hydra.core.TermApplication(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("application"), core_encode_application(V))))
        
        case hydra.core.TermFunction(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("function"), core_encode_function(V))))
        
        case hydra.core.TermLet(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("let"), core_encode_let(V))))
        
        case hydra.core.TermLiteral(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("literal"), core_encode_literal(V))))
        
        case hydra.core.TermList(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("list"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_term(V1))(V)))))
        
        case hydra.core.TermMap(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("map"), hydra.core.TermMap(hydra.lib.maps.bimap(lambda v1: core_encode_term(V1))(lambda v1: core_encode_term(V1))(V)))))
        
        case hydra.core.TermOptional(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("optional"), hydra.core.TermOptional(hydra.lib.optionals.map(lambda v1: core_encode_term(V1))(V)))))
        
        case hydra.core.TermProduct(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("product"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_term(V1))(V)))))
        
        case hydra.core.TermRecord(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("record"), core_encode_record(V))))
        
        case hydra.core.TermSet(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("set"), hydra.core.TermSet(hydra.lib.sets.map(lambda v1: core_encode_term(V1))(V)))))
        
        case hydra.core.TermSum(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("sum"), core_encode_sum(V))))
        
        case hydra.core.TermTypeAbstraction(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeAbstraction"), core_encode_type_abstraction(V))))
        
        case hydra.core.TermTypeApplication(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeApplication"), core_encode_typed_term(V))))
        
        case hydra.core.TermTyped(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typed"), core_encode_typed_term(V))))
        
        case hydra.core.TermUnion(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("union"), core_encode_injection(V))))
        
        case hydra.core.TermVariable(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("variable"), core_encode_name(V))))
        
        case hydra.core.TermWrap(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("wrap"), core_encode_wrapped_term(V))))
        
        case _:
            raise TypeError("Unsupported Term")

def core_encode_tuple_projection(tp):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TupleProjection"), tuple([
      hydra.core.Field(hydra.core.Name("arity"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(Tp.arity)))),
      hydra.core.Field(hydra.core.Name("index"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(Tp.index))))])))

def core_encode_type(v1):
    match V1:
        case hydra.core.TypeAnnotated(v):
            return hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(core_encode_type(V.subject), V.annotation))
        
        case hydra.core.TypeApplication(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("application"), core_encode_application_type(V))))
        
        case hydra.core.TypeFunction(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("function"), core_encode_function_type(V))))
        
        case hydra.core.TypeLambda(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("lambda"), core_encode_lambda_type(V))))
        
        case hydra.core.TypeList(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("list"), core_encode_type(V))))
        
        case hydra.core.TypeLiteral(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("literal"), core_encode_literal_type(V))))
        
        case hydra.core.TypeMap(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("map"), core_encode_map_type(V))))
        
        case hydra.core.TypeOptional(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("optional"), core_encode_type(V))))
        
        case hydra.core.TypeProduct(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("product"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_type(V1))(V)))))
        
        case hydra.core.TypeRecord(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("record"), core_encode_row_type(V))))
        
        case hydra.core.TypeSet(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("set"), core_encode_type(V))))
        
        case hydra.core.TypeSum(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("sum"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_type(V1))(V)))))
        
        case hydra.core.TypeUnion(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("union"), core_encode_row_type(V))))
        
        case hydra.core.TypeVariable(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("variable"), core_encode_name(V))))
        
        case hydra.core.TypeWrap(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("wrap"), core_encode_wrapped_type(V))))
        
        case _:
            raise TypeError("Unsupported Type")

def core_encode_type_abstraction(l):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeAbstraction"), tuple([
      hydra.core.Field(hydra.core.Name("parameter"), core_encode_name(L.parameter)),
      hydra.core.Field(hydra.core.Name("body"), core_encode_term(L.body))])))

def core_encode_type_scheme(ts):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeScheme"), tuple([
      hydra.core.Field(hydra.core.Name("variables"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: core_encode_name(V1))(Ts.variables))),
      hydra.core.Field(hydra.core.Name("type"), core_encode_type(Ts.type))])))

def core_encode_typed_term(tt):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypedTerm"), tuple([
      hydra.core.Field(hydra.core.Name("term"), core_encode_term(Tt.term)),
      hydra.core.Field(hydra.core.Name("type"), core_encode_type(Tt.type))])))

def core_encode_wrapped_term(n):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedTerm"), tuple([
      hydra.core.Field(hydra.core.Name("typeName"), core_encode_name(N.type_name)),
      hydra.core.Field(hydra.core.Name("object"), core_encode_term(N.object))])))

def core_encode_wrapped_type(nt):
    return hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedType"), tuple([
      hydra.core.Field(hydra.core.Name("typeName"), core_encode_name(Nt.type_name)),
      hydra.core.Field(hydra.core.Name("object"), core_encode_type(Nt.object))])))

def is_encoded_type(t):
    match hydra.strip.strip_term(T):
        case hydra.core.TermApplication(a):
            return is_encoded_type(A.function)
        
        case hydra.core.TermUnion(i):
            return hydra.lib.equality.equal_string("hydra.core.Type")(I.type_name.value)
        
        case _:
            return False

def is_type(t):
    match hydra.strip.strip_type(T):
        case hydra.core.TypeApplication(a):
            return is_type(A.function)
        
        case hydra.core.TypeLambda(l):
            return is_type(L.body)
        
        case hydra.core.TypeUnion(rt):
            return hydra.lib.equality.equal_string("hydra.core.Type")(Rt.type_name.value)
        
        case _:
            return False

def is_unit_term(t):
    return hydra.lib.equality.equal_term(hydra.strip.fully_strip_term(T))(hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), tuple([]))))

def is_unit_type(t):
    return hydra.lib.equality.equal_type(hydra.strip.strip_type(T))(hydra.core.TypeRecord(hydra.core.RowType(hydra.core.Name("hydra.core.Unit"), tuple([]))))