package hydra.adapters.utils

import hydra.basics.*

import hydra.core.*

import hydra.lib.literals

import hydra.lib.strings

def describeFloatType(t: hydra.core.FloatType): String = strings.cat(Seq((x: v3) => describePrecision(floatTypePrecision(x))(t), " floating-point numbers"))

def describeIntegerType(t: hydra.core.IntegerType): String = strings.cat(Seq((x: v3) => describePrecision(integerTypePrecision(x))(t), " integers"))

def describeLiteralType(v: hydra.core.LiteralType): String = v match
  case hydra.core.LiteralType.binary() => (_: v2) => "binary strings"(y)
  case hydra.core.LiteralType.boolean() => (_: v5) => "boolean values"(y)
  case hydra.core.LiteralType.float(y) => describeFloatType(y)
  case hydra.core.LiteralType.integer(y) => describeIntegerType(y)
  case hydra.core.LiteralType.string() => (_: v16) => "character strings"(y)

def describePrecision(v: hydra.core.Precision): String = v match
  case hydra.core.Precision.arbitrary() => (_: v2) => "arbitrary-precision"(y)
  case hydra.core.Precision.bits(y) => (bits: v5) => strings.cat(Seq(literals.showInt32(bits), "-bit"))(y)

def describeType[m](typ: hydra.core.Type[m]): String = typ.expr match
  case hydra.core.TypeExpr.literal(y) => describeLiteralType(y)
  case hydra.core.TypeExpr.element(y) => (t: v7) => strings.cat(Seq("elements containing ", describeType(t)))(y)
  case hydra.core.TypeExpr.function(y) => (ft: v15) => strings.cat(Seq(strings.cat(Seq(strings.cat(Seq("functions from ", describeType(ft.domain))), " to ")), describeType(ft.codomain)))(y)
  case hydra.core.TypeExpr.list(y) => (t: v36) => strings.cat(Seq("lists of ", describeType(t)))(y)
  case hydra.core.TypeExpr.map(y) => (mt: v44) => strings.cat(Seq(strings.cat(Seq(strings.cat(Seq("maps from ", describeType(mt.keys))), " to ")), describeType(mt.values)))(y)
  case hydra.core.TypeExpr.nominal(y) => (name: v65) => strings.cat(Seq("alias for ", ELIM-NOMINAL(Name {unName = "hydra/core.Name"})(name)))(y)
  case hydra.core.TypeExpr.optional(y) => (ot: v71) => strings.cat(Seq("optional ", describeType(ot)))(y)
  case hydra.core.TypeExpr.record(y) => (_: v79) => "records of a particular set of fields"(y)
  case hydra.core.TypeExpr.set(y) => (st: v82) => strings.cat(Seq("sets of ", describeType(st)))(y)
  case hydra.core.TypeExpr.union(y) => (_: v90) => "unions of a particular set of fields"(y)
  case hydra.core.TypeExpr.universal(y) => (_: v93) => "polymorphic terms"(y)
  case hydra.core.TypeExpr.variable(y) => (_: v96) => "unspecified/parametric terms"(y)