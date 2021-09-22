package hydra

import hydra.Core;


def atomicTypeAsTerm(at: Core.AtomicType): Core.Term = at match
  case Core.AtomicType.binary() => unitVariant("binary")
  case Core.AtomicType.boolean() => unitVariant("boolean")
  case Core.AtomicType.float(ft) => variant("float", floatTypeAsTerm(ft))
  case Core.AtomicType.integer(it) => variant("integer", integerTypeAsTerm(it))
  case Core.AtomicType.string() => unitVariant("string")

def atomicTypeVariant(at: Core.AtomicType): Core.AtomicVariant = at match
  case Core.AtomicType.binary() => Core.AtomicVariant.boolean()
  case Core.AtomicType.boolean() => Core.AtomicVariant.boolean()
  case Core.AtomicType.float(_) => Core.AtomicVariant.float()
  case Core.AtomicType.integer(_) => Core.AtomicVariant.integer()
  case Core.AtomicType.string() => Core.AtomicVariant.string()

def atomicValueVariant(av: Core.AtomicValue): Core.AtomicVariant = av match
  case Core.AtomicValue.binary(_) => Core.AtomicVariant.binary()
  case Core.AtomicValue.boolean(_) => Core.AtomicVariant.boolean()
  case Core.AtomicValue.float(_) => Core.AtomicVariant.float()
  case Core.AtomicValue.integer(_) => Core.AtomicVariant.integer()
  case Core.AtomicValue.string(_) => Core.AtomicVariant.string()

def fieldTypeAsTerm(ft : Core.FieldType): Core.Term = Core.Term.record(Seq(
  Core.Field("name", string(ft.name)),
  Core.Field("type", typeAsTerm(ft.`type`))))

def floatTypeAsTerm(ft: Core.FloatType): Core.Term = unitVariant(ft match
    case Core.FloatType.bigfloat() => "bigfloat"
    case Core.FloatType.float32() => "float32"
    case Core.FloatType.float64() => "float64")

def floatTypeVariant(ft: Core.FloatType): Core.FloatVariant = ft match
  case Core.FloatType.bigfloat() => Core.FloatVariant.bigfloat()
  case Core.FloatType.float32() => Core.FloatVariant.float32()
  case Core.FloatType.float64() => Core.FloatVariant.float64()

def floatValueVariant(fv: Core.FloatValue): Core.FloatVariant = fv match
  case Core.FloatValue.bigfloat(_) => Core.FloatVariant.bigfloat()
  case Core.FloatValue.float32(_) => Core.FloatVariant.float32()
  case Core.FloatValue.float64(_) => Core.FloatVariant.float64()

def freeVariables(term: Core.Term): Set[Core.Variable] = {
  def free(bound: Set[Core.Variable], t: Core.Term): List[Core.Variable] = term match
    case Core.Term.application(Core.Application(t1, t2)) => free(bound, t1) ++ free(bound, t2)
    case Core.Term.atomic(_) => List()
    case Core.Term.cases(Core.CaseStatement(cases, dflt)) =>
      free(bound, dflt) ++ cases.flatMap(f => free(bound, f.term)).toList
    case Core.Term.compareTo(t) => free(bound, t)
    case Core.Term.data() => List()
    case Core.Term.element(_) => List()
    case Core.Term.function(_) => List()
    case Core.Term.lambda(Core.Lambda(v, t)) => free(bound + v, t)
    case Core.Term.list(els) => els.flatMap(t => free(bound, t)).toList
    case Core.Term.projection(_) => List()
    case Core.Term.record(fields) => fields.flatMap(f => free(bound, f.term)).toList
    case Core.Term.union(f) => free(bound, f.term)
    case Core.Term.variable(v) => if bound.contains(v) then List() else List(v)

  free(Set(), term).toSet
}

def functionTypeAsTerm(ft: Core.FunctionType): Core.Term = Core.Term.record(Seq(
  Core.Field("domain", typeAsTerm(ft.domain)),
  Core.Field("codomain", typeAsTerm(ft.codomain))))

def integerTypeAsTerm(it: Core.IntegerType): Core.Term = unitVariant(it match
  case Core.IntegerType.bigint() => "bigint"
  case Core.IntegerType.int8() => "int8"
  case Core.IntegerType.int16() => "int16"
  case Core.IntegerType.int32() => "int32"
  case Core.IntegerType.int64() => "int64"
  case Core.IntegerType.uint8() => "uint8"
  case Core.IntegerType.uint16() => "uint16"
  case Core.IntegerType.uint32() => "uint32"
  case Core.IntegerType.uint64() => "uint64")

def integerTypeVariant(it: Core.IntegerType): Core.IntegerVariant = it match
  case Core.IntegerType.bigint() => Core.IntegerVariant.bigint()
  case Core.IntegerType.int8() => Core.IntegerVariant.int8()
  case Core.IntegerType.int16() => Core.IntegerVariant.int16()
  case Core.IntegerType.int32() => Core.IntegerVariant.int32()
  case Core.IntegerType.int64() => Core.IntegerVariant.int64()
  case Core.IntegerType.uint8() => Core.IntegerVariant.uint8()
  case Core.IntegerType.uint16() => Core.IntegerVariant.uint16()
  case Core.IntegerType.uint32() => Core.IntegerVariant.uint32()
  case Core.IntegerType.uint64() => Core.IntegerVariant.uint64()

def integerValueVariant(it: Core.IntegerValue): Core.IntegerVariant = it match
  case Core.IntegerValue.bigint(_) => Core.IntegerVariant.bigint()
  case Core.IntegerValue.int8(_) => Core.IntegerVariant.int8()
  case Core.IntegerValue.int16(_) => Core.IntegerVariant.int16()
  case Core.IntegerValue.int32(_) => Core.IntegerVariant.int32()
  case Core.IntegerValue.int64(_) => Core.IntegerVariant.int64()
  case Core.IntegerValue.uint8(_) => Core.IntegerVariant.uint8()
  case Core.IntegerValue.uint16(_) => Core.IntegerVariant.uint16()
  case Core.IntegerValue.uint32(_) => Core.IntegerVariant.uint32()
  case Core.IntegerValue.uint64(_) => Core.IntegerVariant.uint64()

def string(s: String): Core.Term = Core.Term.atomic(Core.AtomicValue.string(s))

/**
 * Whether a term is closed, i.e. represents a complete program
 */
def termIsClosed(term : Core.Term) : Boolean = freeVariables(term).isEmpty

def termVariant(term: Core.Term): Core.TermVariant = term match
  case Core.Term.application(_) => Core.TermVariant.application()
  case Core.Term.atomic(_) => Core.TermVariant.atomic()
  case Core.Term.cases(_) => Core.TermVariant.cases()
  case Core.Term.compareTo(_) => Core.TermVariant.compareTo()
  case Core.Term.data() => Core.TermVariant.data()
  case Core.Term.element(_) => Core.TermVariant.element()
  case Core.Term.function(_) => Core.TermVariant.function()
  case Core.Term.lambda(_) => Core.TermVariant.lambda()
  case Core.Term.list(_) => Core.TermVariant.list()
  case Core.Term.projection(_) => Core.TermVariant.projection()
  case Core.Term.record(_) => Core.TermVariant.record()
  case Core.Term.union(_) => Core.TermVariant.union()
  case Core.Term.variable(_) => Core.TermVariant.variable()

def typeAsTerm(typ: Core.Type): Core.Term = typ match
  case Core.Type.atomic(at) => variant("atomic", atomicTypeAsTerm(at))
  case Core.Type.element(t) => variant("element", typeAsTerm(t))
  case Core.Type.function(ft) => variant("function", functionTypeAsTerm(ft))
  case Core.Type.list(t) => variant("list", typeAsTerm(t))
  case Core.Type.nominal(name) => variant("nominal", string(name))
  case Core.Type.record(fields) => variant("record", Core.Term.list(fields.map(fieldTypeAsTerm)))
  case Core.Type.union(fields) => variant("union", Core.Term.list(fields.map(fieldTypeAsTerm)))

def typeVariant(typ: Core.Type): Core.TypeVariant = typ match
  case Core.Type.atomic(_) => Core.TypeVariant.atomic()
  case Core.Type.element(_) => Core.TypeVariant.element()
  case Core.Type.function(_) => Core.TypeVariant.function()
  case Core.Type.list(_) => Core.TypeVariant.list()
  case Core.Type.nominal(_) => Core.TypeVariant.nominal()
  case Core.Type.record(_) => Core.TypeVariant.record()
  case Core.Type.union(_) => Core.TypeVariant.union()

val unitTerm: Core.Term = Core.Term.record(Seq())

def unitVariant(fname: Core.FieldName): Core.Term = variant(fname, unitTerm)

def variant(fname: Core.FieldName, term: Core.Term): Core.Term = Core.Term.union(Core.Field(fname, term))
