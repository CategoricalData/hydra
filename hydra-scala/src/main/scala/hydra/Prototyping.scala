package hydra

import hydra.Core;


def atomicTypeAsTerm(at: Core.AtomicType): Core.Term = at match
  case Core.AtomicType_binary() => unitVariant("binary")
  case Core.AtomicType_boolean() => unitVariant("boolean")
  case Core.AtomicType_float(ft) => variant("float", floatTypeAsTerm(ft))
  case Core.AtomicType_integer(it) => variant("integer", integerTypeAsTerm(it))
  case Core.AtomicType_string() => unitVariant("string")

def atomicTypeVariant(at: Core.AtomicType): Core.AtomicVariant = at match
  case Core.AtomicType_binary() => Core.AtomicVariant_boolean()
  case Core.AtomicType_boolean() => Core.AtomicVariant_boolean()
  case Core.AtomicType_float(_) => Core.AtomicVariant_float()
  case Core.AtomicType_integer(_) => Core.AtomicVariant_integer()
  case Core.AtomicType_string() => Core.AtomicVariant_string()

def atomicValueVariant(av: Core.AtomicValue): Core.AtomicVariant = av match
  case Core.AtomicValue_binary(_) => Core.AtomicVariant_binary()
  case Core.AtomicValue_boolean(_) => Core.AtomicVariant_boolean()
  case Core.AtomicValue_float(_) => Core.AtomicVariant_float()
  case Core.AtomicValue_integer(_) => Core.AtomicVariant_integer()
  case Core.AtomicValue_string(_) => Core.AtomicVariant_string()

def fieldTypeAsTerm(ft : Core.FieldType): Core.Term = Core.Term_record(Seq(
  Core.Field("name", string(ft.name)),
  Core.Field("type", typeAsTerm(ft.`type`))))

def floatTypeAsTerm(ft: Core.FloatType): Core.Term = unitVariant(ft match
    case Core.FloatType_bigfloat() => "bigfloat"
    case Core.FloatType_float32() => "float32"
    case Core.FloatType_float64() => "float64")

def floatTypeVariant(ft: Core.FloatType): Core.FloatVariant = ft match
  case Core.FloatType_bigfloat() => Core.FloatVariant_bigfloat()
  case Core.FloatType_float32() => Core.FloatVariant_float32()
  case Core.FloatType_float64() => Core.FloatVariant_float64()

def floatValueVariant(fv: Core.FloatValue): Core.FloatVariant = fv match
  case Core.FloatValue_bigfloat(_) => Core.FloatVariant_bigfloat()
  case Core.FloatValue_float32(_) => Core.FloatVariant_float32()
  case Core.FloatValue_float64(_) => Core.FloatVariant_float64()

def freeVariables(term: Core.Term): Set[Core.Variable] = {
  def free(bound: Set[Core.Variable], t: Core.Term): List[Core.Variable] = term match
    case Core.Term_application(Core.Application(t1, t2)) => free(bound, t1) ++ free(bound, t2)
    case Core.Term_atomic(_) => List()
    case Core.Term_cases(Core.CaseStatement(cases, dflt)) =>
      free(bound, dflt) ++ cases.flatMap(f => free(bound, f.term)).toList
    case Core.Term_compareTo(t) => free(bound, t)
    case Core.Term_data() => List()
    case Core.Term_element(_) => List()
    case Core.Term_function(_) => List()
    case Core.Term_lambda(Core.Lambda(v, t)) => free(bound + v, t)
    case Core.Term_list(els) => els.flatMap(t => free(bound, t)).toList
    case Core.Term_projection(_) => List()
    case Core.Term_record(fields) => fields.flatMap(f => free(bound, f.term)).toList
    case Core.Term_union(f) => free(bound, f.term)
    case Core.Term_variable(v) => if bound.contains(v) then List() else List(v)

  free(Set(), term).toSet
}

def functionTypeAsTerm(ft: Core.FunctionType): Core.Term = Core.Term_record(Seq(
  Core.Field("domain", typeAsTerm(ft.domain)),
  Core.Field("codomain", typeAsTerm(ft.codomain))))

def integerTypeAsTerm(it: Core.IntegerType): Core.Term = unitVariant(it match
  case Core.IntegerType_bigint() => "bigint"
  case Core.IntegerType_int8() => "int8"
  case Core.IntegerType_int16() => "int16"
  case Core.IntegerType_int32() => "int32"
  case Core.IntegerType_int64() => "int64"
  case Core.IntegerType_uint8() => "uint8"
  case Core.IntegerType_uint16() => "uint16"
  case Core.IntegerType_uint32() => "uint32"
  case Core.IntegerType_uint64() => "uint64")

def integerTypeVariant(it: Core.IntegerType): Core.IntegerVariant = it match
  case Core.IntegerType_bigint() => Core.IntegerVariant_bigint()
  case Core.IntegerType_int8() => Core.IntegerVariant_int8()
  case Core.IntegerType_int16() => Core.IntegerVariant_int16()
  case Core.IntegerType_int32() => Core.IntegerVariant_int32()
  case Core.IntegerType_int64() => Core.IntegerVariant_int64()
  case Core.IntegerType_uint8() => Core.IntegerVariant_uint8()
  case Core.IntegerType_uint16() => Core.IntegerVariant_uint16()
  case Core.IntegerType_uint32() => Core.IntegerVariant_uint32()
  case Core.IntegerType_uint64() => Core.IntegerVariant_uint64()

def integerValueVariant(it: Core.IntegerValue): Core.IntegerVariant = it match
  case Core.IntegerValue_bigint(_) => Core.IntegerVariant_bigint()
  case Core.IntegerValue_int8(_) => Core.IntegerVariant_int8()
  case Core.IntegerValue_int16(_) => Core.IntegerVariant_int16()
  case Core.IntegerValue_int32(_) => Core.IntegerVariant_int32()
  case Core.IntegerValue_int64(_) => Core.IntegerVariant_int64()
  case Core.IntegerValue_uint8(_) => Core.IntegerVariant_uint8()
  case Core.IntegerValue_uint16(_) => Core.IntegerVariant_uint16()
  case Core.IntegerValue_uint32(_) => Core.IntegerVariant_uint32()
  case Core.IntegerValue_uint64(_) => Core.IntegerVariant_uint64()

def string(s: String): Core.Term = Core.Term_atomic(Core.AtomicValue_string(s))

/**
 * Whether a term is closed, i.e. represents a complete program
 */
def termIsClosed(term : Core.Term) : Boolean = freeVariables(term).isEmpty

def termVariant(term: Core.Term): Core.TermVariant = term match
  case Core.Term_application(_) => Core.TermVariant_application()
  case Core.Term_atomic(_) => Core.TermVariant_atomic()
  case Core.Term_cases(_) => Core.TermVariant_cases()
  case Core.Term_compareTo(_) => Core.TermVariant_compareTo()
  case Core.Term_data() => Core.TermVariant_data()
  case Core.Term_element(_) => Core.TermVariant_element()
  case Core.Term_function(_) => Core.TermVariant_function()
  case Core.Term_lambda(_) => Core.TermVariant_lambda()
  case Core.Term_list(_) => Core.TermVariant_list()
  case Core.Term_projection(_) => Core.TermVariant_projection()
  case Core.Term_record(_) => Core.TermVariant_record()
  case Core.Term_union(_) => Core.TermVariant_union()
  case Core.Term_variable(_) => Core.TermVariant_variable()

def typeAsTerm(typ: Core.Type): Core.Term = typ match
  case Core.Type_atomic(at) => variant("atomic", atomicTypeAsTerm(at))
  case Core.Type_element(t) => variant("element", typeAsTerm(t))
  case Core.Type_function(ft) => variant("function", functionTypeAsTerm(ft))
  case Core.Type_list(t) => variant("list", typeAsTerm(t))
  case Core.Type_nominal(name) => variant("nominal", string(name))
  case Core.Type_record(fields) => variant("record", Core.Term_list(fields.map(fieldTypeAsTerm)))
  case Core.Type_union(fields) => variant("union", Core.Term_list(fields.map(fieldTypeAsTerm)))

def typeVariant(typ: Core.Type): Core.TypeVariant = typ match
  case Core.Type_atomic(_) => Core.TypeVariant_atomic()
  case Core.Type_element(_) => Core.TypeVariant_element()
  case Core.Type_function(_) => Core.TypeVariant_function()
  case Core.Type_list(_) => Core.TypeVariant_list()
  case Core.Type_nominal(_) => Core.TypeVariant_nominal()
  case Core.Type_record(_) => Core.TypeVariant_record()
  case Core.Type_union(_) => Core.TypeVariant_union()

val unitTerm: Core.Term = Core.Term_record(Seq())

def unitVariant(fname: Core.FieldName): Core.Term = variant(fname, unitTerm)

def variant(fname: Core.FieldName, term: Core.Term): Core.Term = Core.Term_union(Core.Field(fname, term))
