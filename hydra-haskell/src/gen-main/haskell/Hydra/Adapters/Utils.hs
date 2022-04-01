module Hydra.Adapters.Utils where

import Hydra.Basics as Basics
import Hydra.Core as Core
import Hydra.Lib.Literals as Literals
import Hydra.Lib.Strings as Strings
import Data.Map
import Data.Set

-- Display a floating-point type as a string
describeFloatType :: (FloatType -> String)
describeFloatType t = (
  Strings.cat [
    describePrecision (floatTypePrecision t),
    " floating-point numbers"])

-- Display an integer type as a string
describeIntegerType :: (IntegerType -> String)
describeIntegerType t = (
  Strings.cat [
    describePrecision (integerTypePrecision t),
    " integers"])

-- Display a literal type as a string
describeLiteralType :: (LiteralType -> String)
describeLiteralType x = case x of
  LiteralTypeBinary -> "binary strings"
  LiteralTypeBoolean -> "boolean values"
  LiteralTypeFloat v -> (describeFloatType v)
  LiteralTypeInteger v -> (describeIntegerType v)
  LiteralTypeString -> "character strings"

-- Display numeric precision as a string
describePrecision :: (Precision -> String)
describePrecision x = case x of
  PrecisionArbitrary -> "arbitrary-precision"
  PrecisionBits v -> (
    Strings.cat [
      Literals.showInt32 v,
      "-bit"])

-- Display a type as a string
describeType :: (Type -> String)
describeType x = case x of
  TypeLiteral v -> (describeLiteralType v)
  TypeElement v -> (
    Strings.cat [
      "elements containing ",
      (describeType v)])
  TypeFunction v -> (
    Strings.cat [
      Strings.cat [
        Strings.cat [
          "functions from ",
          (describeType (functionTypeDomain v))],
        " to "],
      (describeType (functionTypeCodomain v))])
  TypeList v -> (
    Strings.cat [
      "lists of ",
      (describeType v)])
  TypeMap v -> (
    Strings.cat [
      Strings.cat [
        Strings.cat [
          "maps from ",
          (describeType (mapTypeKeys v))],
        " to "],
      (describeType (mapTypeValues v))])
  TypeNominal v -> (
    Strings.cat [
      "alias for ",
      v])
  TypeOptional v -> (
    Strings.cat [
      "optional ",
      (describeType v)])
  TypeRecord _ -> "records of a particular set of fields"
  TypeSet v -> (
    Strings.cat [
      "sets of ",
      (describeType v)])
  TypeUnion _ -> "unions of a particular set of fields"
  TypeUniversal _ -> "polymorphic terms"
  TypeVariable _ -> "unspecified/parametric terms"