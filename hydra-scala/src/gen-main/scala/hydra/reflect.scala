package hydra.reflect

import hydra.core.*

import hydra.util.*

import hydra.variants.*

import hydra.lib.lists

def eliminationVariant(v1: hydra.core.Elimination): hydra.variants.EliminationVariant =
  v1 match
  case hydra.core.Elimination.record(_) => hydra.variants.EliminationVariant.record
  case hydra.core.Elimination.union(_) => hydra.variants.EliminationVariant.union
  case hydra.core.Elimination.wrap(_) => hydra.variants.EliminationVariant.wrap

lazy val eliminationVariants: Seq[hydra.variants.EliminationVariant] = Seq(hydra.variants.EliminationVariant.record, hydra.variants.EliminationVariant.union, hydra.variants.EliminationVariant.wrap)

def floatTypePrecision(v1: hydra.core.FloatType): hydra.util.Precision =
  v1 match
  case hydra.core.FloatType.bigfloat => hydra.util.Precision.arbitrary
  case hydra.core.FloatType.float32 => hydra.util.Precision.bits(32)
  case hydra.core.FloatType.float64 => hydra.util.Precision.bits(64)

lazy val floatTypes: Seq[hydra.core.FloatType] = Seq(hydra.core.FloatType.bigfloat, hydra.core.FloatType.float32, hydra.core.FloatType.float64)

def floatValueType(v1: hydra.core.FloatValue): hydra.core.FloatType =
  v1 match
  case hydra.core.FloatValue.bigfloat(_) => hydra.core.FloatType.bigfloat
  case hydra.core.FloatValue.float32(_) => hydra.core.FloatType.float32
  case hydra.core.FloatValue.float64(_) => hydra.core.FloatType.float64

def functionVariant(v1: hydra.core.Function): hydra.variants.FunctionVariant =
  v1 match
  case hydra.core.Function.elimination(_) => hydra.variants.FunctionVariant.elimination
  case hydra.core.Function.lambda(_) => hydra.variants.FunctionVariant.lambda
  case hydra.core.Function.primitive(_) => hydra.variants.FunctionVariant.primitive

lazy val functionVariants: Seq[hydra.variants.FunctionVariant] = Seq(hydra.variants.FunctionVariant.elimination, hydra.variants.FunctionVariant.lambda, hydra.variants.FunctionVariant.primitive)

def integerTypeIsSigned(v1: hydra.core.IntegerType): Boolean =
  v1 match
  case hydra.core.IntegerType.bigint => true
  case hydra.core.IntegerType.int8 => true
  case hydra.core.IntegerType.int16 => true
  case hydra.core.IntegerType.int32 => true
  case hydra.core.IntegerType.int64 => true
  case hydra.core.IntegerType.uint8 => false
  case hydra.core.IntegerType.uint16 => false
  case hydra.core.IntegerType.uint32 => false
  case hydra.core.IntegerType.uint64 => false

def integerTypePrecision(v1: hydra.core.IntegerType): hydra.util.Precision =
  v1 match
  case hydra.core.IntegerType.bigint => hydra.util.Precision.arbitrary
  case hydra.core.IntegerType.int8 => hydra.util.Precision.bits(8)
  case hydra.core.IntegerType.int16 => hydra.util.Precision.bits(16)
  case hydra.core.IntegerType.int32 => hydra.util.Precision.bits(32)
  case hydra.core.IntegerType.int64 => hydra.util.Precision.bits(64)
  case hydra.core.IntegerType.uint8 => hydra.util.Precision.bits(8)
  case hydra.core.IntegerType.uint16 => hydra.util.Precision.bits(16)
  case hydra.core.IntegerType.uint32 => hydra.util.Precision.bits(32)
  case hydra.core.IntegerType.uint64 => hydra.util.Precision.bits(64)

lazy val integerTypes: Seq[hydra.core.IntegerType] = Seq(hydra.core.IntegerType.bigint, hydra.core.IntegerType.int8, hydra.core.IntegerType.int16, hydra.core.IntegerType.int32, hydra.core.IntegerType.int64, hydra.core.IntegerType.uint8, hydra.core.IntegerType.uint16, hydra.core.IntegerType.uint32, hydra.core.IntegerType.uint64)

def integerValueType(v1: hydra.core.IntegerValue): hydra.core.IntegerType =
  v1 match
  case hydra.core.IntegerValue.bigint(_) => hydra.core.IntegerType.bigint
  case hydra.core.IntegerValue.int8(_) => hydra.core.IntegerType.int8
  case hydra.core.IntegerValue.int16(_) => hydra.core.IntegerType.int16
  case hydra.core.IntegerValue.int32(_) => hydra.core.IntegerType.int32
  case hydra.core.IntegerValue.int64(_) => hydra.core.IntegerType.int64
  case hydra.core.IntegerValue.uint8(_) => hydra.core.IntegerType.uint8
  case hydra.core.IntegerValue.uint16(_) => hydra.core.IntegerType.uint16
  case hydra.core.IntegerValue.uint32(_) => hydra.core.IntegerType.uint32
  case hydra.core.IntegerValue.uint64(_) => hydra.core.IntegerType.uint64

def literalType(v1: hydra.core.Literal): hydra.core.LiteralType =
  v1 match
  case hydra.core.Literal.binary(_) => hydra.core.LiteralType.binary
  case hydra.core.Literal.boolean(_) => hydra.core.LiteralType.boolean
  case hydra.core.Literal.float(v_Literal_float_arg_) => hydra.core.LiteralType.float(hydra.reflect.floatValueType(`v_Literal_float_arg_`))
  case hydra.core.Literal.integer(v_Literal_integer_arg_) => hydra.core.LiteralType.integer(hydra.reflect.integerValueType(`v_Literal_integer_arg_`))
  case hydra.core.Literal.string(_) => hydra.core.LiteralType.string

def literalTypeVariant(v1: hydra.core.LiteralType): hydra.variants.LiteralVariant =
  v1 match
  case hydra.core.LiteralType.binary => hydra.variants.LiteralVariant.binary
  case hydra.core.LiteralType.boolean => hydra.variants.LiteralVariant.boolean
  case hydra.core.LiteralType.float(_) => hydra.variants.LiteralVariant.float
  case hydra.core.LiteralType.integer(_) => hydra.variants.LiteralVariant.integer
  case hydra.core.LiteralType.string => hydra.variants.LiteralVariant.string

lazy val literalTypes: Seq[hydra.core.LiteralType] = hydra.lib.lists.concat[hydra.core.LiteralType](Seq(Seq(hydra.core.LiteralType.binary, hydra.core.LiteralType.boolean), hydra.lib.lists.map[hydra.core.FloatType, hydra.core.LiteralType]((x: hydra.core.FloatType) => hydra.core.LiteralType.float(x))(hydra.reflect.floatTypes), hydra.lib.lists.map[hydra.core.IntegerType, hydra.core.LiteralType]((x: hydra.core.IntegerType) => hydra.core.LiteralType.integer(x))(hydra.reflect.integerTypes), Seq(hydra.core.LiteralType.string)))

def literalVariant(`arg_`: hydra.core.Literal): hydra.variants.LiteralVariant = hydra.reflect.literalTypeVariant(hydra.reflect.literalType(`arg_`))

lazy val literalVariants: Seq[hydra.variants.LiteralVariant] = Seq(hydra.variants.LiteralVariant.binary, hydra.variants.LiteralVariant.boolean, hydra.variants.LiteralVariant.float, hydra.variants.LiteralVariant.integer, hydra.variants.LiteralVariant.string)

def termVariant(v1: hydra.core.Term): hydra.variants.TermVariant =
  v1 match
  case hydra.core.Term.annotated(_) => hydra.variants.TermVariant.annotated
  case hydra.core.Term.application(_) => hydra.variants.TermVariant.application
  case hydra.core.Term.either(_) => hydra.variants.TermVariant.either
  case hydra.core.Term.function(_) => hydra.variants.TermVariant.function
  case hydra.core.Term.let(_) => hydra.variants.TermVariant.let
  case hydra.core.Term.list(_) => hydra.variants.TermVariant.list
  case hydra.core.Term.literal(_) => hydra.variants.TermVariant.literal
  case hydra.core.Term.map(_) => hydra.variants.TermVariant.map
  case hydra.core.Term.maybe(_) => hydra.variants.TermVariant.maybe
  case hydra.core.Term.pair(_) => hydra.variants.TermVariant.pair
  case hydra.core.Term.record(_) => hydra.variants.TermVariant.record
  case hydra.core.Term.set(_) => hydra.variants.TermVariant.set
  case hydra.core.Term.typeApplication(_) => hydra.variants.TermVariant.typeApplication
  case hydra.core.Term.typeLambda(_) => hydra.variants.TermVariant.typeLambda
  case hydra.core.Term.union(_) => hydra.variants.TermVariant.union
  case hydra.core.Term.unit => hydra.variants.TermVariant.unit
  case hydra.core.Term.variable(_) => hydra.variants.TermVariant.variable
  case hydra.core.Term.wrap(_) => hydra.variants.TermVariant.wrap

lazy val termVariants: Seq[hydra.variants.TermVariant] = Seq(hydra.variants.TermVariant.annotated, hydra.variants.TermVariant.application, hydra.variants.TermVariant.either, hydra.variants.TermVariant.function, hydra.variants.TermVariant.list, hydra.variants.TermVariant.literal, hydra.variants.TermVariant.map, hydra.variants.TermVariant.maybe, hydra.variants.TermVariant.pair, hydra.variants.TermVariant.record, hydra.variants.TermVariant.set, hydra.variants.TermVariant.typeLambda, hydra.variants.TermVariant.typeApplication, hydra.variants.TermVariant.union, hydra.variants.TermVariant.unit, hydra.variants.TermVariant.variable, hydra.variants.TermVariant.wrap)

def typeVariant(v1: hydra.core.Type): hydra.variants.TypeVariant =
  v1 match
  case hydra.core.Type.annotated(_) => hydra.variants.TypeVariant.annotated
  case hydra.core.Type.application(_) => hydra.variants.TypeVariant.application
  case hydra.core.Type.either(_) => hydra.variants.TypeVariant.either
  case hydra.core.Type.function(_) => hydra.variants.TypeVariant.function
  case hydra.core.Type.forall(_) => hydra.variants.TypeVariant.forall
  case hydra.core.Type.list(_) => hydra.variants.TypeVariant.list
  case hydra.core.Type.literal(_) => hydra.variants.TypeVariant.literal
  case hydra.core.Type.map(_) => hydra.variants.TypeVariant.map
  case hydra.core.Type.maybe(_) => hydra.variants.TypeVariant.maybe
  case hydra.core.Type.pair(_) => hydra.variants.TypeVariant.pair
  case hydra.core.Type.record(_) => hydra.variants.TypeVariant.record
  case hydra.core.Type.set(_) => hydra.variants.TypeVariant.set
  case hydra.core.Type.union(_) => hydra.variants.TypeVariant.union
  case hydra.core.Type.unit => hydra.variants.TypeVariant.unit
  case hydra.core.Type.variable(_) => hydra.variants.TypeVariant.variable
  case hydra.core.Type.void => hydra.variants.TypeVariant.void
  case hydra.core.Type.wrap(_) => hydra.variants.TypeVariant.wrap

lazy val typeVariants: Seq[hydra.variants.TypeVariant] = Seq(hydra.variants.TypeVariant.annotated, hydra.variants.TypeVariant.application, hydra.variants.TypeVariant.either, hydra.variants.TypeVariant.function, hydra.variants.TypeVariant.forall, hydra.variants.TypeVariant.list, hydra.variants.TypeVariant.literal, hydra.variants.TypeVariant.map, hydra.variants.TypeVariant.wrap, hydra.variants.TypeVariant.maybe, hydra.variants.TypeVariant.pair, hydra.variants.TypeVariant.record, hydra.variants.TypeVariant.set, hydra.variants.TypeVariant.union, hydra.variants.TypeVariant.unit, hydra.variants.TypeVariant.variable, hydra.variants.TypeVariant.void)
