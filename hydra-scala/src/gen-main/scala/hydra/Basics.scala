package hydra.basics

import hydra.core.*

import hydra.graph.*

import hydra.lib.lists

import hydra.lib.strings

def eliminationVariant[a](v: hydra.core.Elimination[a]): hydra.core.EliminationVariant = v match
  case hydra.core.Elimination.element(y) => hydra.core.EliminationVariant.element()
  case hydra.core.Elimination.nominal(y) => hydra.core.EliminationVariant.nominal()
  case hydra.core.Elimination.optional(y) => hydra.core.EliminationVariant.optional()
  case hydra.core.Elimination.record(y) => hydra.core.EliminationVariant.record()
  case hydra.core.Elimination.union(y) => hydra.core.EliminationVariant.union()

val eliminationVariants = Seq(hydra.core.EliminationVariant.element(), hydra.core.EliminationVariant.nominal(), hydra.core.EliminationVariant.optional(), hydra.core.EliminationVariant.record(), hydra.core.EliminationVariant.union())

def floatTypePrecision(v: hydra.core.FloatType): hydra.core.Precision = v match
  case hydra.core.FloatType.bigfloat() => hydra.core.Precision.arbitrary()
  case hydra.core.FloatType.float32() => hydra.core.Precision.bits(32)
  case hydra.core.FloatType.float64() => hydra.core.Precision.bits(64)

val floatTypes = Seq(hydra.core.FloatType.bigfloat(), hydra.core.FloatType.float32(), hydra.core.FloatType.float64())

def floatValueType(v: hydra.core.FloatValue): hydra.core.FloatType = v match
  case hydra.core.FloatValue.bigfloat(y) => hydra.core.FloatType.bigfloat()
  case hydra.core.FloatValue.float32(y) => hydra.core.FloatType.float32()
  case hydra.core.FloatValue.float64(y) => hydra.core.FloatType.float64()

def functionVariant[m](v: hydra.core.Function[m]): hydra.core.FunctionVariant = v match
  case hydra.core.Function.compareTo(y) => hydra.core.FunctionVariant.compareTo()
  case hydra.core.Function.elimination(y) => hydra.core.FunctionVariant.elimination()
  case hydra.core.Function.lambda(y) => hydra.core.FunctionVariant.lambda()
  case hydra.core.Function.primitive(y) => hydra.core.FunctionVariant.primitive()

val functionVariants = Seq(hydra.core.FunctionVariant.compareTo(), hydra.core.FunctionVariant.elimination(), hydra.core.FunctionVariant.lambda(), hydra.core.FunctionVariant.primitive())

def integerTypeIsSigned(v: hydra.core.IntegerType): Boolean = v match
  case hydra.core.IntegerType.bigint() => true
  case hydra.core.IntegerType.int8() => true
  case hydra.core.IntegerType.int16() => true
  case hydra.core.IntegerType.int32() => true
  case hydra.core.IntegerType.int64() => true
  case hydra.core.IntegerType.uint8() => false
  case hydra.core.IntegerType.uint16() => false
  case hydra.core.IntegerType.uint32() => false
  case hydra.core.IntegerType.uint64() => false

def integerTypePrecision(v: hydra.core.IntegerType): hydra.core.Precision = v match
  case hydra.core.IntegerType.bigint() => hydra.core.Precision.arbitrary()
  case hydra.core.IntegerType.int8() => hydra.core.Precision.bits(8)
  case hydra.core.IntegerType.int16() => hydra.core.Precision.bits(16)
  case hydra.core.IntegerType.int32() => hydra.core.Precision.bits(32)
  case hydra.core.IntegerType.int64() => hydra.core.Precision.bits(64)
  case hydra.core.IntegerType.uint8() => hydra.core.Precision.bits(8)
  case hydra.core.IntegerType.uint16() => hydra.core.Precision.bits(16)
  case hydra.core.IntegerType.uint32() => hydra.core.Precision.bits(32)
  case hydra.core.IntegerType.uint64() => hydra.core.Precision.bits(64)

val integerTypes = Seq(hydra.core.IntegerType.bigint(), hydra.core.IntegerType.int8(), hydra.core.IntegerType.int16(), hydra.core.IntegerType.int32(), hydra.core.IntegerType.int64(), hydra.core.IntegerType.uint8(), hydra.core.IntegerType.uint16(), hydra.core.IntegerType.uint32(), hydra.core.IntegerType.uint64())

def integerValueType(v: hydra.core.IntegerValue): hydra.core.IntegerType = v match
  case hydra.core.IntegerValue.bigint(y) => hydra.core.IntegerType.bigint()
  case hydra.core.IntegerValue.int8(y) => hydra.core.IntegerType.int8()
  case hydra.core.IntegerValue.int16(y) => hydra.core.IntegerType.int16()
  case hydra.core.IntegerValue.int32(y) => hydra.core.IntegerType.int32()
  case hydra.core.IntegerValue.int64(y) => hydra.core.IntegerType.int64()
  case hydra.core.IntegerValue.uint8(y) => hydra.core.IntegerType.uint8()
  case hydra.core.IntegerValue.uint16(y) => hydra.core.IntegerType.uint16()
  case hydra.core.IntegerValue.uint32(y) => hydra.core.IntegerType.uint32()
  case hydra.core.IntegerValue.uint64(y) => hydra.core.IntegerType.uint64()

def literalType(v: hydra.core.Literal): hydra.core.LiteralType = v match
  case hydra.core.Literal.binary(y) => (_: v2) => hydra.core.LiteralType.binary()(y)
  case hydra.core.Literal.boolean(y) => (_: v5) => hydra.core.LiteralType.boolean()(y)
  case hydra.core.Literal.float(y) => (x: v8) => (x: v9) => hydra.core.LiteralType.float(x)(floatValueType(x))(y)
  case hydra.core.Literal.integer(y) => (x: v16) => (x: v17) => hydra.core.LiteralType.integer(x)(integerValueType(x))(y)
  case hydra.core.Literal.string(y) => (_: v24) => hydra.core.LiteralType.string()(y)

def literalTypeVariant(v: hydra.core.LiteralType): hydra.core.LiteralVariant = v match
  case hydra.core.LiteralType.binary() => hydra.core.LiteralVariant.binary()
  case hydra.core.LiteralType.boolean() => hydra.core.LiteralVariant.boolean()
  case hydra.core.LiteralType.float(y) => hydra.core.LiteralVariant.float()
  case hydra.core.LiteralType.integer(y) => hydra.core.LiteralVariant.integer()
  case hydra.core.LiteralType.string() => hydra.core.LiteralVariant.string()

def literalVariant(x: hydra.core.Literal): hydra.core.LiteralVariant = literalTypeVariant(literalType(x))

val literalVariants = Seq(hydra.core.LiteralVariant.binary(), hydra.core.LiteralVariant.boolean(), hydra.core.LiteralVariant.float(), hydra.core.LiteralVariant.integer(), hydra.core.LiteralVariant.string())

def qname(ns: hydra.graph.GraphName): (String => hydra.core.Name) = (name: v2) => strings.cat(Seq(ELIM-NOMINAL(Name {unName = "hydra/graph.GraphName"})(ns), ".", name))

def termVariant[m](term: hydra.core.Term[m]): hydra.core.TermVariant = term.expr match
  case hydra.core.TermExpr.application(y) => hydra.core.TermVariant.application()
  case hydra.core.TermExpr.element(y) => hydra.core.TermVariant.element()
  case hydra.core.TermExpr.function(y) => hydra.core.TermVariant.function()
  case hydra.core.TermExpr.list(y) => hydra.core.TermVariant.list()
  case hydra.core.TermExpr.literal(y) => hydra.core.TermVariant.literal()
  case hydra.core.TermExpr.map(y) => hydra.core.TermVariant.map()
  case hydra.core.TermExpr.nominal(y) => hydra.core.TermVariant.nominal()
  case hydra.core.TermExpr.optional(y) => hydra.core.TermVariant.optional()
  case hydra.core.TermExpr.record(y) => hydra.core.TermVariant.record()
  case hydra.core.TermExpr.set(y) => hydra.core.TermVariant.set()
  case hydra.core.TermExpr.typeAbstraction(y) => hydra.core.TermVariant.typeAbstraction()
  case hydra.core.TermExpr.typeApplication(y) => hydra.core.TermVariant.typeApplication()
  case hydra.core.TermExpr.union(y) => hydra.core.TermVariant.union()
  case hydra.core.TermExpr.variable(y) => hydra.core.TermVariant.variable()

val termVariants = Seq(hydra.core.TermVariant.application(), hydra.core.TermVariant.literal(), hydra.core.TermVariant.element(), hydra.core.TermVariant.function(), hydra.core.TermVariant.list(), hydra.core.TermVariant.map(), hydra.core.TermVariant.nominal(), hydra.core.TermVariant.optional(), hydra.core.TermVariant.record(), hydra.core.TermVariant.set(), hydra.core.TermVariant.union(), hydra.core.TermVariant.variable())

def testLists[a](els: Seq[Seq[a]]): Int = lists.length(lists.concat(els))

def typeVariant[m](typ: hydra.core.Type[m]): hydra.core.TypeVariant = typ.expr match
  case hydra.core.TypeExpr.element(y) => hydra.core.TypeVariant.element()
  case hydra.core.TypeExpr.function(y) => hydra.core.TypeVariant.function()
  case hydra.core.TypeExpr.list(y) => hydra.core.TypeVariant.list()
  case hydra.core.TypeExpr.literal(y) => hydra.core.TypeVariant.literal()
  case hydra.core.TypeExpr.map(y) => hydra.core.TypeVariant.map()
  case hydra.core.TypeExpr.nominal(y) => hydra.core.TypeVariant.nominal()
  case hydra.core.TypeExpr.optional(y) => hydra.core.TypeVariant.optional()
  case hydra.core.TypeExpr.record(y) => hydra.core.TypeVariant.record()
  case hydra.core.TypeExpr.set(y) => hydra.core.TypeVariant.set()
  case hydra.core.TypeExpr.union(y) => hydra.core.TypeVariant.union()
  case hydra.core.TypeExpr.universal(y) => hydra.core.TypeVariant.universal()
  case hydra.core.TypeExpr.variable(y) => hydra.core.TypeVariant.variable()

val typeVariants = Seq(hydra.core.TypeVariant.literal(), hydra.core.TypeVariant.element(), hydra.core.TypeVariant.function(), hydra.core.TypeVariant.list(), hydra.core.TypeVariant.map(), hydra.core.TypeVariant.nominal(), hydra.core.TypeVariant.optional(), hydra.core.TypeVariant.record(), hydra.core.TypeVariant.set(), hydra.core.TypeVariant.union(), hydra.core.TypeVariant.universal(), hydra.core.TypeVariant.variable())