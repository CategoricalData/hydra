package hydra.prototyping

import hydra.core.*;


def atomicTypeVariant(at: AtomicType): AtomicVariant = at match
  case AtomicType.binary() => AtomicVariant.boolean()
  case AtomicType.boolean() => AtomicVariant.boolean()
  case AtomicType.float(_) => AtomicVariant.float()
  case AtomicType.integer(_) => AtomicVariant.integer()
  case AtomicType.string() => AtomicVariant.string()

def atomicValueType(av: AtomicValue): AtomicType = av match
  case AtomicValue.binary(_) => AtomicType.binary()
  case AtomicValue.boolean(_) => AtomicType.boolean()
  case AtomicValue.float(fv) => AtomicType.float(floatValueType(fv))
  case AtomicValue.integer(iv) => AtomicType.integer(integerValueType(iv))
  case AtomicValue.string(_) => AtomicType.string()

def atomicValueVariant(av: AtomicValue): AtomicVariant = av match
  case AtomicValue.binary(_) => AtomicVariant.binary()
  case AtomicValue.boolean(_) => AtomicVariant.boolean()
  case AtomicValue.float(_) => AtomicVariant.float()
  case AtomicValue.integer(_) => AtomicVariant.integer()
  case AtomicValue.string(_) => AtomicVariant.string()

def floatTypeVariant(ft: FloatType): FloatVariant = ft match
  case FloatType.bigfloat() => FloatVariant.bigfloat()
  case FloatType.float32() => FloatVariant.float32()
  case FloatType.float64() => FloatVariant.float64()

def floatValueType(fv: FloatValue): FloatType = fv match
  case FloatValue.bigfloat(_) => FloatType.bigfloat()
  case FloatValue.float32(_) => FloatType.float32()
  case FloatValue.float64(_) => FloatType.float64()

def floatValueVariant(fv: FloatValue): FloatVariant = fv match
  case FloatValue.bigfloat(_) => FloatVariant.bigfloat()
  case FloatValue.float32(_) => FloatVariant.float32()
  case FloatValue.float64(_) => FloatVariant.float64()

def freeVariables(term: Term): Set[Variable] = {
  def free(bound: Set[Variable], t: Term): List[Variable] = term match
    case Term.application(Application(t1, t2)) => free(bound, t1) ++ free(bound, t2)
    case Term.atomic(_) => List()
    case Term.cases(cases) => cases.flatMap(f => free(bound, f.term)).toList
    case Term.compareTo(t) => free(bound, t)
    case Term.data() => List()
    case Term.element(_) => List()
    case Term.function(_) => List()
    case Term.lambda(Lambda(v, t)) => free(bound + v, t)
    case Term.list(els) => els.flatMap(t => free(bound, t)).toList
    case Term.projection(_) => List()
    case Term.record(fields) => fields.flatMap(f => free(bound, f.term)).toList
    case Term.union(f) => free(bound, f.term)
    case Term.variable(v) => if bound.contains(v) then List() else List(v)

  free(Set(), term).toSet
}

def integerTypeVariant(it: IntegerType): IntegerVariant = it match
  case IntegerType.bigint() => IntegerVariant.bigint()
  case IntegerType.int8() => IntegerVariant.int8()
  case IntegerType.int16() => IntegerVariant.int16()
  case IntegerType.int32() => IntegerVariant.int32()
  case IntegerType.int64() => IntegerVariant.int64()
  case IntegerType.uint8() => IntegerVariant.uint8()
  case IntegerType.uint16() => IntegerVariant.uint16()
  case IntegerType.uint32() => IntegerVariant.uint32()
  case IntegerType.uint64() => IntegerVariant.uint64()

def integerValueType(iv: IntegerValue): IntegerType = iv match
  case IntegerValue.bigint(_) => IntegerType.bigint()
  case IntegerValue.int8(_) => IntegerType.int8()
  case IntegerValue.int16(_) => IntegerType.int16()
  case IntegerValue.int32(_) => IntegerType.int32()
  case IntegerValue.int64(_) => IntegerType.int64()
  case IntegerValue.uint8(_) => IntegerType.uint8()
  case IntegerValue.uint16(_) => IntegerType.uint16()
  case IntegerValue.uint32(_) => IntegerType.uint32()
  case IntegerValue.uint64(_) => IntegerType.uint64()

def integerValueVariant(it: IntegerValue): IntegerVariant = it match
  case IntegerValue.bigint(_) => IntegerVariant.bigint()
  case IntegerValue.int8(_) => IntegerVariant.int8()
  case IntegerValue.int16(_) => IntegerVariant.int16()
  case IntegerValue.int32(_) => IntegerVariant.int32()
  case IntegerValue.int64(_) => IntegerVariant.int64()
  case IntegerValue.uint8(_) => IntegerVariant.uint8()
  case IntegerValue.uint16(_) => IntegerVariant.uint16()
  case IntegerValue.uint32(_) => IntegerVariant.uint32()
  case IntegerValue.uint64(_) => IntegerVariant.uint64()

/**
 * Whether a term is closed, i.e. represents a complete program
 */
def termIsClosed(term: Term) : Boolean = freeVariables(term).isEmpty

def termVariant(term: Term): TermVariant = term match
  case Term.application(_) => TermVariant.application()
  case Term.atomic(_) => TermVariant.atomic()
  case Term.cases(_) => TermVariant.cases()
  case Term.compareTo(_) => TermVariant.compareTo()
  case Term.data() => TermVariant.data()
  case Term.element(_) => TermVariant.element()
  case Term.function(_) => TermVariant.function()
  case Term.lambda(_) => TermVariant.lambda()
  case Term.list(_) => TermVariant.list()
  case Term.projection(_) => TermVariant.projection()
  case Term.record(_) => TermVariant.record()
  case Term.union(_) => TermVariant.union()
  case Term.variable(_) => TermVariant.variable()

def typeVariant(typ: Type): TypeVariant = typ match
  case Type.atomic(_) => TypeVariant.atomic()
  case Type.element(_) => TypeVariant.element()
  case Type.function(_) => TypeVariant.function()
  case Type.list(_) => TypeVariant.list()
  case Type.nominal(_) => TypeVariant.nominal()
  case Type.record(_) => TypeVariant.record()
  case Type.union(_) => TypeVariant.union()
