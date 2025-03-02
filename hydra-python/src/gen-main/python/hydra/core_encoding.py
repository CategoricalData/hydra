"""Mapping of hydra.core constructs in a host language like Haskell or Java  to their native Hydra counterparts as terms.  This includes an implementation of LambdaGraph's epsilon encoding (types to terms)."""

from __future__ import annotations
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.optionals
import hydra.lib.sets
import hydra.strip

def coreEncodeAnnotatedTerm(a):
    hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(coreEncodeTerm(a.subject), a.annotation))

def coreEncodeAnnotatedType(at):
    hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(coreEncodeType(at.subject), at.annotation))

def coreEncodeApplication(app):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Application"), [
      hydra.core.Field(hydra.core.Name("function"), coreEncodeTerm(app.function)),
      hydra.core.Field(hydra.core.Name("argument"), coreEncodeTerm(app.argument))]))

def coreEncodeApplicationType(at):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.ApplicationType"), [
      hydra.core.Field(hydra.core.Name("function"), coreEncodeType(at.function)),
      hydra.core.Field(hydra.core.Name("argument"), coreEncodeType(at.argument))]))

def coreEncodeCaseStatement(cs):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.CaseStatement"), [
      hydra.core.Field(hydra.core.Name("typeName"), coreEncodeName(cs.type_name)),
      hydra.core.Field(hydra.core.Name("default"), hydra.core.TermOptional(hydra.lib.optionals.map(lambda v1: coreEncodeTerm(v1))(cs.default))),
      hydra.core.Field(hydra.core.Name("cases"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: coreEncodeField(v1))(cs.cases)))]))

def coreEncodeElimination(v1):
    match v1:
        case hydra.core.EliminationList(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("list"), coreEncodeTerm(v))))
        
        case hydra.core.EliminationOptional(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("optional"), coreEncodeOptionalCases(v))))
        
        case hydra.core.EliminationProduct(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("product"), coreEncodeTupleProjection(v))))
        
        case hydra.core.EliminationRecord(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("record"), coreEncodeProjection(v))))
        
        case hydra.core.EliminationUnion(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("union"), coreEncodeCaseStatement(v))))
        
        case hydra.core.EliminationWrap(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Elimination"), hydra.core.Field(hydra.core.Name("wrap"), coreEncodeName(v))))
        
        case _:
            raise TypeError("Unsupported Elimination")

def coreEncodeField(f):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Field"), [
      hydra.core.Field(hydra.core.Name("name"), hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), hydra.core.TermLiteral(hydra.core.LiteralString(f.name.value))))),
      hydra.core.Field(hydra.core.Name("term"), coreEncodeTerm(f.term))]))

def coreEncodeFieldType(ft):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FieldType"), [
      hydra.core.Field(hydra.core.Name("name"), coreEncodeName(ft.name)),
      hydra.core.Field(hydra.core.Name("type"), coreEncodeType(ft.type))]))

def coreEncodeFloatType(v1):
    match v1:
        case hydra.core.FloatTypeBigfloat(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("bigfloat"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))))
        
        case hydra.core.FloatTypeFloat32(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float32"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))))
        
        case hydra.core.FloatTypeFloat64(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatType"), hydra.core.Field(hydra.core.Name("float64"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))))
        
        case _:
            raise TypeError("Unsupported FloatType")

def coreEncodeFloatValue(v1):
    match v1:
        case hydra.core.FloatValueBigfloat(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("bigfloat"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueBigfloat(v))))))
        
        case hydra.core.FloatValueFloat32(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float32"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat32(v))))))
        
        case hydra.core.FloatValueFloat64(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(hydra.core.Name("float64"), hydra.core.TermLiteral(hydra.core.LiteralFloat(hydra.core.FloatValueFloat64(v))))))
        
        case _:
            raise TypeError("Unsupported FloatValue")

def coreEncodeFunction(v1):
    match v1:
        case hydra.core.FunctionElimination(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("elimination"), coreEncodeElimination(v))))
        
        case hydra.core.FunctionLambda(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("lambda"), coreEncodeLambda(v))))
        
        case hydra.core.FunctionPrimitive(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Function"), hydra.core.Field(hydra.core.Name("primitive"), coreEncodeName(v))))
        
        case _:
            raise TypeError("Unsupported Function")

def coreEncodeFunctionType(ft):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.FunctionType"), [
      hydra.core.Field(hydra.core.Name("domain"), coreEncodeType(ft.domain)),
      hydra.core.Field(hydra.core.Name("codomain"), coreEncodeType(ft.codomain))]))

def coreEncodeInjection(i):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Injection"), [
      hydra.core.Field(hydra.core.Name("typeName"), coreEncodeName(i.type_name)),
      hydra.core.Field(hydra.core.Name("field"), coreEncodeField(i.field))]))

def coreEncodeIntegerType(v1):
    match v1:
        case hydra.core.IntegerTypeBigint(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("bigint"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))))
        
        case hydra.core.IntegerTypeInt8(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int8"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))))
        
        case hydra.core.IntegerTypeInt16(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int16"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))))
        
        case hydra.core.IntegerTypeInt32(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int32"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))))
        
        case hydra.core.IntegerTypeInt64(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("int64"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))))
        
        case hydra.core.IntegerTypeUint8(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint8"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))))
        
        case hydra.core.IntegerTypeUint16(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint16"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))))
        
        case hydra.core.IntegerTypeUint32(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint32"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))))
        
        case hydra.core.IntegerTypeUint64(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerType"), hydra.core.Field(hydra.core.Name("uint64"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))))
        
        case _:
            raise TypeError("Unsupported IntegerType")

def coreEncodeIntegerValue(v1):
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
        
        case _:
            raise TypeError("Unsupported IntegerValue")

def coreEncodeLambda(l):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Lambda"), [
      hydra.core.Field(hydra.core.Name("parameter"), coreEncodeName(l.parameter)),
      hydra.core.Field(hydra.core.Name("domain"), hydra.core.TermOptional(hydra.lib.optionals.map(lambda v1: coreEncodeType(v1))(l.domain))),
      hydra.core.Field(hydra.core.Name("body"), coreEncodeTerm(l.body))]))

def coreEncodeLambdaType(lt):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.LambdaType"), [
      hydra.core.Field(hydra.core.Name("parameter"), coreEncodeName(lt.parameter)),
      hydra.core.Field(hydra.core.Name("body"), coreEncodeType(lt.body))]))

def coreEncodeLet(l):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Let"), [
      hydra.core.Field(hydra.core.Name("bindings"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: coreEncodeLetBinding(v1))(l.bindings))),
      hydra.core.Field(hydra.core.Name("environment"), coreEncodeTerm(l.environment))]))

def coreEncodeLetBinding(b):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.LetBinding"), [
      hydra.core.Field(hydra.core.Name("name"), coreEncodeName(b.name)),
      hydra.core.Field(hydra.core.Name("term"), coreEncodeTerm(b.term)),
      hydra.core.Field(hydra.core.Name("type"), hydra.core.TermOptional(hydra.lib.optionals.map(lambda v1: coreEncodeTypeScheme(v1))(b.type)))]))

def coreEncodeLiteral(v1):
    match v1:
        case hydra.core.LiteralBinary(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("binary"), hydra.core.TermLiteral(hydra.core.LiteralBinary(v)))))
        
        case hydra.core.LiteralBoolean(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("boolean"), hydra.core.TermLiteral(hydra.core.LiteralBoolean(v)))))
        
        case hydra.core.LiteralFloat(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("float"), coreEncodeFloatValue(v))))
        
        case hydra.core.LiteralInteger(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("integer"), coreEncodeIntegerValue(v))))
        
        case hydra.core.LiteralString(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("string"), hydra.core.TermLiteral(hydra.core.LiteralString(v)))))
        
        case _:
            raise TypeError("Unsupported Literal")

def coreEncodeLiteralType(v1):
    match v1:
        case hydra.core.LiteralTypeBinary(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("binary"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))))
        
        case hydra.core.LiteralTypeBoolean(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("boolean"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))))
        
        case hydra.core.LiteralTypeFloat(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("float"), coreEncodeFloatType(v))))
        
        case hydra.core.LiteralTypeInteger(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("integer"), coreEncodeIntegerType(v))))
        
        case hydra.core.LiteralTypeString(_):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.LiteralType"), hydra.core.Field(hydra.core.Name("string"), hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))))
        
        case _:
            raise TypeError("Unsupported LiteralType")

def coreEncodeMapType(mt):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.MapType"), [
      hydra.core.Field(hydra.core.Name("keys"), coreEncodeType(mt.keys)),
      hydra.core.Field(hydra.core.Name("values"), coreEncodeType(mt.values))]))

def coreEncodeName(fn):
    hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), hydra.core.TermLiteral(hydra.core.LiteralString(fn.value))))

def coreEncodeOptionalCases(oc):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.OptionalCases"), [
      hydra.core.Field(hydra.core.Name("nothing"), coreEncodeTerm(oc.nothing)),
      hydra.core.Field(hydra.core.Name("just"), coreEncodeTerm(oc.just))]))

def coreEncodeProjection(p):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Projection"), [
      hydra.core.Field(hydra.core.Name("typeName"), coreEncodeName(p.type_name)),
      hydra.core.Field(hydra.core.Name("field"), coreEncodeName(p.field))]))

def coreEncodeRecord(r):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Record"), [
      hydra.core.Field(hydra.core.Name("typeName"), coreEncodeName(r.type_name)),
      hydra.core.Field(hydra.core.Name("fields"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: coreEncodeField(v1))(r.fields)))]))

def coreEncodeRowType(rt):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.RowType"), [
      hydra.core.Field(hydra.core.Name("typeName"), coreEncodeName(rt.type_name)),
      hydra.core.Field(hydra.core.Name("fields"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: coreEncodeFieldType(v1))(rt.fields)))]))

def coreEncodeSum(s):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Sum"), [
      hydra.core.Field(hydra.core.Name("index"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(s.index)))),
      hydra.core.Field(hydra.core.Name("size"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(s.size)))),
      hydra.core.Field(hydra.core.Name("term"), coreEncodeTerm(s.term))]))

def coreEncodeTerm(v1):
    match v1:
        case hydra.core.TermAnnotated(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("annotated"), coreEncodeAnnotatedTerm(v))))
        
        case hydra.core.TermApplication(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("application"), coreEncodeApplication(v))))
        
        case hydra.core.TermFunction(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("function"), coreEncodeFunction(v))))
        
        case hydra.core.TermLet(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("let"), coreEncodeLet(v))))
        
        case hydra.core.TermLiteral(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("literal"), coreEncodeLiteral(v))))
        
        case hydra.core.TermList(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("list"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: coreEncodeTerm(v1))(v)))))
        
        case hydra.core.TermMap(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("map"), hydra.core.TermMap(hydra.lib.maps.bimap(lambda v1: coreEncodeTerm(v1))(lambda v1: coreEncodeTerm(v1))(v)))))
        
        case hydra.core.TermOptional(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("optional"), hydra.core.TermOptional(hydra.lib.optionals.map(lambda v1: coreEncodeTerm(v1))(v)))))
        
        case hydra.core.TermProduct(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("product"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: coreEncodeTerm(v1))(v)))))
        
        case hydra.core.TermRecord(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("record"), coreEncodeRecord(v))))
        
        case hydra.core.TermSet(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("set"), hydra.core.TermSet(hydra.lib.sets.map(lambda v1: coreEncodeTerm(v1))(v)))))
        
        case hydra.core.TermSum(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("sum"), coreEncodeSum(v))))
        
        case hydra.core.TermTypeAbstraction(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeAbstraction"), coreEncodeTypeAbstraction(v))))
        
        case hydra.core.TermTypeApplication(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typeApplication"), coreEncodeTypedTerm(v))))
        
        case hydra.core.TermTyped(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("typed"), coreEncodeTypedTerm(v))))
        
        case hydra.core.TermUnion(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("union"), coreEncodeInjection(v))))
        
        case hydra.core.TermVariable(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("variable"), coreEncodeName(v))))
        
        case hydra.core.TermWrap(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("wrap"), coreEncodeWrappedTerm(v))))
        
        case _:
            raise TypeError("Unsupported Term")

def coreEncodeTupleProjection(tp):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TupleProjection"), [
      hydra.core.Field(hydra.core.Name("arity"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(tp.arity)))),
      hydra.core.Field(hydra.core.Name("index"), hydra.core.TermLiteral(hydra.core.LiteralInteger(hydra.core.IntegerValueInt32(tp.index))))]))

def coreEncodeType(v1):
    match v1:
        case hydra.core.TypeAnnotated(v):
            return hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(coreEncodeType(v.subject), v.annotation))
        
        case hydra.core.TypeApplication(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("application"), coreEncodeApplicationType(v))))
        
        case hydra.core.TypeFunction(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("function"), coreEncodeFunctionType(v))))
        
        case hydra.core.TypeLambda(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("lambda"), coreEncodeLambdaType(v))))
        
        case hydra.core.TypeList(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("list"), coreEncodeType(v))))
        
        case hydra.core.TypeLiteral(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("literal"), coreEncodeLiteralType(v))))
        
        case hydra.core.TypeMap(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("map"), coreEncodeMapType(v))))
        
        case hydra.core.TypeOptional(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("optional"), coreEncodeType(v))))
        
        case hydra.core.TypeProduct(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("product"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: coreEncodeType(v1))(v)))))
        
        case hydra.core.TypeRecord(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("record"), coreEncodeRowType(v))))
        
        case hydra.core.TypeSet(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("set"), coreEncodeType(v))))
        
        case hydra.core.TypeSum(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("sum"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: coreEncodeType(v1))(v)))))
        
        case hydra.core.TypeUnion(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("union"), coreEncodeRowType(v))))
        
        case hydra.core.TypeVariable(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("variable"), coreEncodeName(v))))
        
        case hydra.core.TypeWrap(v):
            return hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Type"), hydra.core.Field(hydra.core.Name("wrap"), coreEncodeWrappedType(v))))
        
        case _:
            raise TypeError("Unsupported Type")

def coreEncodeTypeAbstraction(l):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeAbstraction"), [
      hydra.core.Field(hydra.core.Name("parameter"), coreEncodeName(l.parameter)),
      hydra.core.Field(hydra.core.Name("body"), coreEncodeTerm(l.body))]))

def coreEncodeTypeScheme(ts):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypeScheme"), [
      hydra.core.Field(hydra.core.Name("variables"), hydra.core.TermList(hydra.lib.lists.map(lambda v1: coreEncodeName(v1))(ts.variables))),
      hydra.core.Field(hydra.core.Name("type"), coreEncodeType(ts.type))]))

def coreEncodeTypedTerm(tt):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.TypedTerm"), [
      hydra.core.Field(hydra.core.Name("term"), coreEncodeTerm(tt.term)),
      hydra.core.Field(hydra.core.Name("type"), coreEncodeType(tt.type))]))

def coreEncodeWrappedTerm(n):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedTerm"), [
      hydra.core.Field(hydra.core.Name("typeName"), coreEncodeName(n.type_name)),
      hydra.core.Field(hydra.core.Name("object"), coreEncodeTerm(n.object))]))

def coreEncodeWrappedType(nt):
    hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedType"), [
      hydra.core.Field(hydra.core.Name("typeName"), coreEncodeName(nt.type_name)),
      hydra.core.Field(hydra.core.Name("object"), coreEncodeType(nt.object))]))

def isEncodedType(t):
    match hydra.strip.stripTerm(t):
        case hydra.core.TermApplication(a):
            return isEncodedType(a.function)
        
        case hydra.core.TermUnion(i):
            return hydra.lib.equality.equalString("hydra.core.Type")(i.type_name.value)
        
        case _:
            return False

def isType(t):
    match hydra.strip.stripType(t):
        case hydra.core.TypeApplication(a):
            return isType(a.function)
        
        case hydra.core.TypeLambda(l):
            return isType(l.body)
        
        case hydra.core.TypeUnion(rt):
            return hydra.lib.equality.equalString("hydra.core.Type")(rt.type_name.value)
        
        case _:
            return False

def isUnitTerm(t):
    hydra.lib.equality.equalTerm(hydra.strip.fullyStripTerm(t))(hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Unit"), [])))

def isUnitType(t):
    hydra.lib.equality.equalType(hydra.strip.stripType(t))(hydra.core.TypeRecord(hydra.core.RowType(hydra.core.Name("hydra.core.Unit"), [])))