module Hydra.Adapters.Utils where

import qualified Hydra.Basics as Basics
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Strings as Strings
import Data.Map
import Data.Set

-- Display a floating-point type as a string
describeFloatType :: (Core.FloatType -> String)
describeFloatType t = (
  Strings.cat [
    describePrecision (Basics.floatTypePrecision t),
    " floating-point numbers"])

-- Display an integer type as a string
describeIntegerType :: (Core.IntegerType -> String)
describeIntegerType t = (
  Strings.cat [
    describePrecision (Basics.integerTypePrecision t),
    " integers"])

-- Display a literal type as a string
describeLiteralType :: (Core.LiteralType -> String)
describeLiteralType x = case x of
  Core.LiteralTypeBinary -> "binary strings"
  Core.LiteralTypeBoolean -> "boolean values"
  Core.LiteralTypeFloat v -> (describeFloatType v)
  Core.LiteralTypeInteger v -> (describeIntegerType v)
  Core.LiteralTypeString -> "character strings"

-- Display numeric precision as a string
describePrecision :: (Core.Precision -> String)
describePrecision x = case x of
  Core.PrecisionArbitrary -> "arbitrary-precision"
  Core.PrecisionBits v -> (
    Strings.cat [
      Literals.showInt32 v,
      "-bit"])

-- Display a type as a string
describeType :: (Core.Type -> String)
describeType x = case x of
  Core.TypeLiteral v -> (describeLiteralType v)
  Core.TypeElement v -> (
    Strings.cat [
      "elements containing ",
      (describeType v)])
  Core.TypeFunction v -> (
    Strings.cat [
      Strings.cat [
        Strings.cat [
          "functions from ",
          (describeType (Core.functionTypeDomain v))],
        " to "],
      (describeType (Core.functionTypeCodomain v))])
  Core.TypeList v -> (
    Strings.cat [
      "lists of ",
      (describeType v)])
  Core.TypeMap v -> (
    Strings.cat [
      Strings.cat [
        Strings.cat [
          "maps from ",
          (describeType (Core.mapTypeKeys v))],
        " to "],
      (describeType (Core.mapTypeValues v))])
  Core.TypeNominal v -> (
    Strings.cat [
      "alias for ",
      v])
  Core.TypeOptional v -> (
    Strings.cat [
      "optional ",
      (describeType v)])
  Core.TypeRecord _ -> "records of a particular set of fields"
  Core.TypeSet v -> (
    Strings.cat [
      "sets of ",
      (describeType v)])
  Core.TypeUnion _ -> "unions of a particular set of fields"
  Core.TypeUniversal _ -> "polymorphic terms"
  Core.TypeVariable _ -> "unspecified/parametric terms"