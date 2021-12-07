package hydra.prototyping

import hydra.core.*;


def literalTypeVariant(at: LiteralType): LiteralVariant = at match
  case LiteralType.binary() => LiteralVariant.boolean()
  case LiteralType.boolean() => LiteralVariant.boolean()
  case LiteralType.float(_) => LiteralVariant.float()
  case LiteralType.integer(_) => LiteralVariant.integer()
  case LiteralType.string() => LiteralVariant.string()

def literalType(av: Literal): LiteralType = av match
  case Literal.binary(_) => LiteralType.binary()
  case Literal.boolean(_) => LiteralType.boolean()
  case Literal.float(fv) => LiteralType.float(floatValueType(fv))
  case Literal.integer(iv) => LiteralType.integer(integerValueType(iv))
  case Literal.string(_) => LiteralType.string()

def literalVariant(av: Literal): LiteralVariant = literalTypeVariant(literalType(av))

def floatValueType(fv: FloatValue): FloatType = fv match
  case FloatValue.bigfloat(_) => FloatType.bigfloat()
  case FloatValue.float32(_) => FloatType.float32()
  case FloatValue.float64(_) => FloatType.float64()

def functionVariant[a](fun: Function[a]): FunctionVariant = fun match
  case Function.cases(_) => FunctionVariant.cases()
  case Function.compareTo(_) => FunctionVariant.compareTo()
  case Function.data() => FunctionVariant.data()
  case Function.lambda(_) => FunctionVariant.lambda()
  case Function.primitive(_) => FunctionVariant.primitive()
  case Function.projection(_) => FunctionVariant.projection()

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

def termVariant[a](term: Term[a]): TermVariant = term.data match
  case Expression.application(_) => TermVariant.application()
  case Expression.literal(_) => TermVariant.literal()
  case Expression.element(_) => TermVariant.element()
  case Expression.function(_) => TermVariant.function()
  case Expression.list(_) => TermVariant.list()
  case Expression.map(_) => TermVariant.map()
  case Expression.optional(_) => TermVariant.optional()
  case Expression.record(_) => TermVariant.record()
  case Expression.set(_) => TermVariant.set()
  case Expression.typeAbstraction(_) => TermVariant.typeAbstraction()
  case Expression.typeApplication(_) => TermVariant.typeApplication()
  case Expression.union(_) => TermVariant.union()
  case Expression.variable(_) => TermVariant.variable()

def typeVariant(typ: Type): TypeVariant = typ match
  case Type.literal(_) => TypeVariant.literal()
  case Type.element(_) => TypeVariant.element()
  case Type.function(_) => TypeVariant.function()
  case Type.list(_) => TypeVariant.list()
  case Type.map(_) => TypeVariant.map()
  case Type.nominal(_) => TypeVariant.nominal()
  case Type.optional(_) => TypeVariant.optional()
  case Type.record(_) => TypeVariant.record()
  case Type.set(_) => TypeVariant.set()
  case Type.universal(_) => TypeVariant.universal()
  case Type.union(_) => TypeVariant.union()
