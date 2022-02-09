package hydra.adapters.utils

import hydra.basics.*

import hydra.core.*

import hydra.lib.literals

import hydra.lib.strings

def describeFloatType(t: FloatType): String = strings.cat(Seq(describePrecision(floatTypePrecision(t)), " floating-point numbers"))

def describeIntegerType(t: IntegerType): String = strings.cat(Seq(describePrecision(integerTypePrecision(t)), " integers"))

def describeLiteralType(v: LiteralType): String = v match
  case LiteralType.binary() => "binary strings"
  case LiteralType.boolean() => "boolean values"
  case LiteralType.float(y) => describeFloatType(y)
  case LiteralType.integer(y) => describeIntegerType(y)
  case LiteralType.string() => "character strings"

def describePrecision(v: Precision): String = v match
  case Precision.arbitrary() => "arbitrary-precision"
  case Precision.bits(y) => strings.cat(Seq(literals.showInt32(y), "-bit"))

def describeType(v: Type): String = v match
  case Type.literal(y) => describeLiteralType(y)
  case Type.element(y) => strings.cat(Seq("elements containing ", describeType(y)))
  case Type.function(y) => strings.cat(Seq(strings.cat(Seq(strings.cat(Seq("functions from ", describeType(y.domain))), " to ")), describeType(y.codomain)))
  case Type.list(y) => strings.cat(Seq("lists of ", describeType(y)))
  case Type.map(y) => strings.cat(Seq(strings.cat(Seq(strings.cat(Seq("maps from ", describeType(y.keys))), " to ")), describeType(y.values)))
  case Type.nominal(y) => strings.cat(Seq("alias for", y))
  case Type.optional(y) => strings.cat(Seq("optional ", describeType(y)))
  case Type.record(y) => "records of a particular set of fields"
  case Type.set(y) => strings.cat(Seq("sets of ", describeType(y)))
  case Type.union(y) => "unions of a particular set of fields"
  case Type.universal(y) => "polymorphic terms"
  case Type.variable(y) => "unspecified/parametric terms"