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

def atomicValueVariant(av: AtomicValue): AtomicVariant = atomicTypeVariant(atomicValueType(av))

def floatTypeVariant(ft: FloatType): FloatVariant = ft match
  case FloatType.bigfloat() => FloatVariant.bigfloat()
  case FloatType.float32() => FloatVariant.float32()
  case FloatType.float64() => FloatVariant.float64()

def floatValueType(fv: FloatValue): FloatType = fv match
  case FloatValue.bigfloat(_) => FloatType.bigfloat()
  case FloatValue.float32(_) => FloatType.float32()
  case FloatValue.float64(_) => FloatType.float64()

def floatValueVariant(fv: FloatValue): FloatVariant = floatTypeVariant(floatValueType(fv))

def functionVariant(fun: Function): FunctionVariant = fun match
  case Function.cases(_) => FunctionVariant.cases()
  case Function.compareTo(_) => FunctionVariant.compareTo()
  case Function.data() => FunctionVariant.data()
  case Function.lambda(_) => FunctionVariant.lambda()
  case Function.primitive(_) => FunctionVariant.primitive()
  case Function.projection(_) => FunctionVariant.projection()

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

def integerValueVariant(it: IntegerValue): IntegerVariant = integerTypeVariant(integerValueType(it))

def termVariant(term: Term): TermVariant = term match
  case Term.application(_) => TermVariant.application()
  case Term.atomic(_) => TermVariant.atomic()
  case Term.element(_) => TermVariant.element()
  case Term.function(_) => TermVariant.function()
  case Term.list(_) => TermVariant.list()
  case Term.map(_) => TermVariant.map()
  case Term.record(_) => TermVariant.record()
  case Term.set(_) => TermVariant.set()
  case Term.union(_) => TermVariant.union()
  case Term.variable(_) => TermVariant.variable()

def typeVariant(typ: Type): TypeVariant = typ match
  case Type.atomic(_) => TypeVariant.atomic()
  case Type.element(_) => TypeVariant.element()
  case Type.function(_) => TypeVariant.function()
  case Type.list(_) => TypeVariant.list()
  case Type.map(_) => TypeVariant.map()
  case Type.nominal(_) => TypeVariant.nominal()
  case Type.record(_) => TypeVariant.record()
  case Type.set(_) => TypeVariant.set()
  case Type.union(_) => TypeVariant.union()
