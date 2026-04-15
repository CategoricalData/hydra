package hydra.reflect

import hydra.core.*

import hydra.util.*

import hydra.variants.*

lazy val eliminationVariants: Seq[hydra.variants.EliminationVariant] = Seq(hydra.variants.EliminationVariant.record,
   hydra.variants.EliminationVariant.union, hydra.variants.EliminationVariant.wrap)

def floatTypePrecision(v1: hydra.core.FloatType): hydra.util.Precision =
  v1 match
  case hydra.core.FloatType.bigfloat => hydra.util.Precision.arbitrary
  case hydra.core.FloatType.float32 => hydra.util.Precision.bits(32)
  case hydra.core.FloatType.float64 => hydra.util.Precision.bits(64)

lazy val floatTypes: Seq[hydra.core.FloatType] = Seq(hydra.core.FloatType.bigfloat,
   hydra.core.FloatType.float32, hydra.core.FloatType.float64)

def floatValueType(v1: hydra.core.FloatValue): hydra.core.FloatType =
  v1 match
  case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat__) => hydra.core.FloatType.bigfloat
  case hydra.core.FloatValue.float32(v_FloatValue_float32__) => hydra.core.FloatType.float32
  case hydra.core.FloatValue.float64(v_FloatValue_float64__) => hydra.core.FloatType.float64

lazy val functionVariants: Seq[hydra.variants.FunctionVariant] = Seq(hydra.variants.FunctionVariant.elimination,
   hydra.variants.FunctionVariant.lambda)

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

lazy val integerTypes: Seq[hydra.core.IntegerType] = Seq(hydra.core.IntegerType.bigint,
   hydra.core.IntegerType.int8, hydra.core.IntegerType.int16, hydra.core.IntegerType.int32,
   hydra.core.IntegerType.int64, hydra.core.IntegerType.uint8, hydra.core.IntegerType.uint16,
   hydra.core.IntegerType.uint32, hydra.core.IntegerType.uint64)

def integerValueType(v1: hydra.core.IntegerValue): hydra.core.IntegerType =
  v1 match
  case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint__) => hydra.core.IntegerType.bigint
  case hydra.core.IntegerValue.int8(v_IntegerValue_int8__) => hydra.core.IntegerType.int8
  case hydra.core.IntegerValue.int16(v_IntegerValue_int16__) => hydra.core.IntegerType.int16
  case hydra.core.IntegerValue.int32(v_IntegerValue_int32__) => hydra.core.IntegerType.int32
  case hydra.core.IntegerValue.int64(v_IntegerValue_int64__) => hydra.core.IntegerType.int64
  case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8__) => hydra.core.IntegerType.uint8
  case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16__) => hydra.core.IntegerType.uint16
  case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32__) => hydra.core.IntegerType.uint32
  case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64__) => hydra.core.IntegerType.uint64

def literalType(v1: hydra.core.Literal): hydra.core.LiteralType =
  v1 match
  case hydra.core.Literal.binary(v_Literal_binary__) => hydra.core.LiteralType.binary
  case hydra.core.Literal.boolean(v_Literal_boolean__) => hydra.core.LiteralType.boolean
  case hydra.core.Literal.float(v_Literal_float_arg_) => hydra.core.LiteralType.float(hydra.reflect.floatValueType(`v_Literal_float_arg_`))
  case hydra.core.Literal.integer(v_Literal_integer_arg_) => hydra.core.LiteralType.integer(hydra.reflect.integerValueType(`v_Literal_integer_arg_`))
  case hydra.core.Literal.string(v_Literal_string__) => hydra.core.LiteralType.string

def literalTypeVariant(v1: hydra.core.LiteralType): hydra.variants.LiteralVariant =
  v1 match
  case hydra.core.LiteralType.binary => hydra.variants.LiteralVariant.binary
  case hydra.core.LiteralType.boolean => hydra.variants.LiteralVariant.boolean
  case hydra.core.LiteralType.float(v_LiteralType_float__) => hydra.variants.LiteralVariant.float
  case hydra.core.LiteralType.integer(v_LiteralType_integer__) => hydra.variants.LiteralVariant.integer
  case hydra.core.LiteralType.string => hydra.variants.LiteralVariant.string

lazy val literalTypes: Seq[hydra.core.LiteralType] = hydra.lib.lists.concat[hydra.core.LiteralType](Seq(Seq(hydra.core.LiteralType.binary,
   hydra.core.LiteralType.boolean), hydra.lib.lists.map[hydra.core.FloatType, hydra.core.LiteralType]((x: hydra.core.FloatType) => hydra.core.LiteralType.float(x))(hydra.reflect.floatTypes),
   hydra.lib.lists.map[hydra.core.IntegerType, hydra.core.LiteralType]((x: hydra.core.IntegerType) => hydra.core.LiteralType.integer(x))(hydra.reflect.integerTypes),
   Seq(hydra.core.LiteralType.string)))

def literalVariant(`arg_`: hydra.core.Literal): hydra.variants.LiteralVariant = hydra.reflect.literalTypeVariant(hydra.reflect.literalType(`arg_`))

lazy val literalVariants: Seq[hydra.variants.LiteralVariant] = Seq(hydra.variants.LiteralVariant.binary,
   hydra.variants.LiteralVariant.boolean, hydra.variants.LiteralVariant.float, hydra.variants.LiteralVariant.integer,
   hydra.variants.LiteralVariant.string)

def termVariant(v1: hydra.core.Term): hydra.variants.TermVariant =
  v1 match
  case hydra.core.Term.annotated(v_Term_annotated__) => hydra.variants.TermVariant.annotated
  case hydra.core.Term.application(v_Term_application__) => hydra.variants.TermVariant.application
  case hydra.core.Term.cases(v_Term_cases__) => hydra.variants.TermVariant.cases
  case hydra.core.Term.either(v_Term_either__) => hydra.variants.TermVariant.either
  case hydra.core.Term.lambda(v_Term_lambda__) => hydra.variants.TermVariant.lambda
  case hydra.core.Term.let(v_Term_let__) => hydra.variants.TermVariant.let
  case hydra.core.Term.list(v_Term_list__) => hydra.variants.TermVariant.list
  case hydra.core.Term.literal(v_Term_literal__) => hydra.variants.TermVariant.literal
  case hydra.core.Term.map(v_Term_map__) => hydra.variants.TermVariant.map
  case hydra.core.Term.maybe(v_Term_maybe__) => hydra.variants.TermVariant.maybe
  case hydra.core.Term.pair(v_Term_pair__) => hydra.variants.TermVariant.pair
  case hydra.core.Term.project(v_Term_project__) => hydra.variants.TermVariant.project
  case hydra.core.Term.record(v_Term_record__) => hydra.variants.TermVariant.record
  case hydra.core.Term.set(v_Term_set__) => hydra.variants.TermVariant.set
  case hydra.core.Term.typeApplication(v_Term_typeApplication__) => hydra.variants.TermVariant.typeApplication
  case hydra.core.Term.typeLambda(v_Term_typeLambda__) => hydra.variants.TermVariant.typeLambda
  case hydra.core.Term.inject(v_Term_inject__) => hydra.variants.TermVariant.inject
  case hydra.core.Term.unit => hydra.variants.TermVariant.unit
  case hydra.core.Term.unwrap(v_Term_unwrap__) => hydra.variants.TermVariant.unwrap
  case hydra.core.Term.variable(v_Term_variable__) => hydra.variants.TermVariant.variable
  case hydra.core.Term.wrap(v_Term_wrap__) => hydra.variants.TermVariant.wrap

lazy val termVariants: Seq[hydra.variants.TermVariant] = Seq(hydra.variants.TermVariant.annotated,
   hydra.variants.TermVariant.application, hydra.variants.TermVariant.cases, hydra.variants.TermVariant.either,
   hydra.variants.TermVariant.lambda, hydra.variants.TermVariant.let, hydra.variants.TermVariant.list,
   hydra.variants.TermVariant.literal, hydra.variants.TermVariant.map, hydra.variants.TermVariant.maybe,
   hydra.variants.TermVariant.pair, hydra.variants.TermVariant.project, hydra.variants.TermVariant.record,
   hydra.variants.TermVariant.set, hydra.variants.TermVariant.typeLambda, hydra.variants.TermVariant.typeApplication,
   hydra.variants.TermVariant.inject, hydra.variants.TermVariant.unit, hydra.variants.TermVariant.unwrap,
   hydra.variants.TermVariant.variable, hydra.variants.TermVariant.wrap)

def typeVariant(v1: hydra.core.Type): hydra.variants.TypeVariant =
  v1 match
  case hydra.core.Type.annotated(v_Type_annotated__) => hydra.variants.TypeVariant.annotated
  case hydra.core.Type.application(v_Type_application__) => hydra.variants.TypeVariant.application
  case hydra.core.Type.either(v_Type_either__) => hydra.variants.TypeVariant.either
  case hydra.core.Type.function(v_Type_function__) => hydra.variants.TypeVariant.function
  case hydra.core.Type.forall(v_Type_forall__) => hydra.variants.TypeVariant.forall
  case hydra.core.Type.list(v_Type_list__) => hydra.variants.TypeVariant.list
  case hydra.core.Type.literal(v_Type_literal__) => hydra.variants.TypeVariant.literal
  case hydra.core.Type.map(v_Type_map__) => hydra.variants.TypeVariant.map
  case hydra.core.Type.maybe(v_Type_maybe__) => hydra.variants.TypeVariant.maybe
  case hydra.core.Type.pair(v_Type_pair__) => hydra.variants.TypeVariant.pair
  case hydra.core.Type.record(v_Type_record__) => hydra.variants.TypeVariant.record
  case hydra.core.Type.set(v_Type_set__) => hydra.variants.TypeVariant.set
  case hydra.core.Type.union(v_Type_union__) => hydra.variants.TypeVariant.union
  case hydra.core.Type.unit => hydra.variants.TypeVariant.unit
  case hydra.core.Type.variable(v_Type_variable__) => hydra.variants.TypeVariant.variable
  case hydra.core.Type.void => hydra.variants.TypeVariant.void
  case hydra.core.Type.wrap(v_Type_wrap__) => hydra.variants.TypeVariant.wrap

lazy val typeVariants: Seq[hydra.variants.TypeVariant] = Seq(hydra.variants.TypeVariant.annotated,
   hydra.variants.TypeVariant.application, hydra.variants.TypeVariant.either, hydra.variants.TypeVariant.function,
   hydra.variants.TypeVariant.forall, hydra.variants.TypeVariant.list, hydra.variants.TypeVariant.literal,
   hydra.variants.TypeVariant.map, hydra.variants.TypeVariant.wrap, hydra.variants.TypeVariant.maybe,
   hydra.variants.TypeVariant.pair, hydra.variants.TypeVariant.record, hydra.variants.TypeVariant.set,
   hydra.variants.TypeVariant.union, hydra.variants.TypeVariant.unit, hydra.variants.TypeVariant.variable,
   hydra.variants.TypeVariant.void)
