package hydra.basics

import hydra.core.*

import hydra.lib.lists

import hydra.lib.strings

def floatTypePrecision(v: FloatType): Precision = v match
  case FloatType.bigfloat() => Precision.arbitrary()
  case FloatType.float32() => Precision.bits(32)
  case FloatType.float64() => Precision.bits(64)

val floatTypes = Seq(FloatType.bigfloat(), FloatType.float32(), FloatType.float64())

def floatValueType(v: FloatValue): FloatType = v match
  case FloatValue.bigfloat(y) => FloatType.bigfloat()
  case FloatValue.float32(y) => FloatType.float32()
  case FloatValue.float64(y) => FloatType.float64()

def functionVariant[a](v: hydra.core.Function[a]): FunctionVariant = v match
  case hydra.core.Function.cases(y) => FunctionVariant.cases()
  case hydra.core.Function.compareTo(y) => FunctionVariant.compareTo()
  case hydra.core.Function.data() => FunctionVariant.data()
  case hydra.core.Function.lambda(y) => FunctionVariant.lambda()
  case hydra.core.Function.optionalCases(y) => FunctionVariant.optionalCases()
  case hydra.core.Function.primitive(y) => FunctionVariant.primitive()
  case hydra.core.Function.projection(y) => FunctionVariant.projection()

val functionVariants = Seq(FunctionVariant.cases(), FunctionVariant.compareTo(), FunctionVariant.data(), FunctionVariant.lambda(), FunctionVariant.optionalCases(), FunctionVariant.primitive(), FunctionVariant.projection())

def integerTypeIsSigned(v: IntegerType): Boolean = v match
  case IntegerType.bigint() => true
  case IntegerType.int8() => true
  case IntegerType.int16() => true
  case IntegerType.int32() => true
  case IntegerType.int64() => true
  case IntegerType.uint8() => false
  case IntegerType.uint16() => false
  case IntegerType.uint32() => false
  case IntegerType.uint64() => false

def integerTypePrecision(v: IntegerType): Precision = v match
  case IntegerType.bigint() => Precision.arbitrary()
  case IntegerType.int8() => Precision.bits(8)
  case IntegerType.int16() => Precision.bits(16)
  case IntegerType.int32() => Precision.bits(32)
  case IntegerType.int64() => Precision.bits(64)
  case IntegerType.uint8() => Precision.bits(8)
  case IntegerType.uint16() => Precision.bits(16)
  case IntegerType.uint32() => Precision.bits(32)
  case IntegerType.uint64() => Precision.bits(64)

val integerTypes = Seq(IntegerType.bigint(), IntegerType.int8(), IntegerType.int16(), IntegerType.int32(), IntegerType.int64(), IntegerType.uint8(), IntegerType.uint16(), IntegerType.uint32(), IntegerType.uint64())

def integerValueType(v: IntegerValue): IntegerType = v match
  case IntegerValue.bigint(y) => IntegerType.bigint()
  case IntegerValue.int8(y) => IntegerType.int8()
  case IntegerValue.int16(y) => IntegerType.int16()
  case IntegerValue.int32(y) => IntegerType.int32()
  case IntegerValue.int64(y) => IntegerType.int64()
  case IntegerValue.uint8(y) => IntegerType.uint8()
  case IntegerValue.uint16(y) => IntegerType.uint16()
  case IntegerValue.uint32(y) => IntegerType.uint32()
  case IntegerValue.uint64(y) => IntegerType.uint64()

def literalType(v: Literal): LiteralType = v match
  case Literal.binary(y) => LiteralType.binary()
  case Literal.boolean(y) => LiteralType.boolean()
  case Literal.float(y) => LiteralType.float(floatValueType(y))
  case Literal.integer(y) => LiteralType.integer(integerValueType(y))
  case Literal.string(y) => LiteralType.string()

def literalTypeVariant(v: LiteralType): LiteralVariant = v match
  case LiteralType.binary() => LiteralVariant.binary()
  case LiteralType.boolean() => LiteralVariant.boolean()
  case LiteralType.float(y) => LiteralVariant.float()
  case LiteralType.integer(y) => LiteralVariant.integer()
  case LiteralType.string() => LiteralVariant.string()

def literalVariant(x: Literal): LiteralVariant = literalTypeVariant(literalType(x))

val literalVariants = Seq(LiteralVariant.binary(), LiteralVariant.boolean(), LiteralVariant.float(), LiteralVariant.integer(), LiteralVariant.string())

def qname(ns: String): (String => Name) = (name: String) => strings.cat(Seq(ns, ".", name))

def termVariant[a](term: Term[a]): TermVariant = term.data match
  case Expression.application(y) => TermVariant.application()
  case Expression.element(y) => TermVariant.element()
  case Expression.function(y) => TermVariant.function()
  case Expression.list(y) => TermVariant.list()
  case Expression.literal(y) => TermVariant.literal()
  case Expression.map(y) => TermVariant.map()
  case Expression.nominal(y) => TermVariant.nominal()
  case Expression.optional(y) => TermVariant.optional()
  case Expression.record(y) => TermVariant.record()
  case Expression.set(y) => TermVariant.set()
  case Expression.typeAbstraction(y) => TermVariant.typeAbstraction()
  case Expression.typeApplication(y) => TermVariant.typeApplication()
  case Expression.union(y) => TermVariant.union()
  case Expression.variable(y) => TermVariant.variable()

val termVariants = Seq(TermVariant.application(), TermVariant.literal(), TermVariant.element(), TermVariant.function(), TermVariant.list(), TermVariant.map(), TermVariant.nominal(), TermVariant.optional(), TermVariant.record(), TermVariant.set(), TermVariant.union(), TermVariant.variable())

def testLists[a](els: Seq[Seq[a]]): Int = lists.length(lists.concat(els))

def typeVariant(v: Type): TypeVariant = v match
  case Type.element(y) => TypeVariant.element()
  case Type.function(y) => TypeVariant.function()
  case Type.list(y) => TypeVariant.list()
  case Type.literal(y) => TypeVariant.literal()
  case Type.map(y) => TypeVariant.map()
  case Type.nominal(y) => TypeVariant.nominal()
  case Type.optional(y) => TypeVariant.optional()
  case Type.record(y) => TypeVariant.record()
  case Type.set(y) => TypeVariant.set()
  case Type.union(y) => TypeVariant.union()
  case Type.universal(y) => TypeVariant.universal()
  case Type.variable(y) => TypeVariant.variable()

val typeVariants = Seq(TypeVariant.literal(), TypeVariant.element(), TypeVariant.function(), TypeVariant.list(), TypeVariant.map(), TypeVariant.nominal(), TypeVariant.optional(), TypeVariant.record(), TypeVariant.set(), TypeVariant.union(), TypeVariant.universal(), TypeVariant.variable())