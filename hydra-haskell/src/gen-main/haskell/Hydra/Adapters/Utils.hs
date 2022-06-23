module Hydra.Adapters.Utils where

import qualified Hydra.Basics as Basics
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Strings as Strings
import Data.Map
import Data.Set

-- Display a floating-point type as a string
describeFloatType :: (Core.FloatType -> String)
describeFloatType t = (Strings.cat [
  (\x -> describePrecision (Basics.floatTypePrecision x)) t,
  " floating-point numbers"])

-- Display an integer type as a string
describeIntegerType :: (Core.IntegerType -> String)
describeIntegerType t = (Strings.cat [
  (\x -> describePrecision (Basics.integerTypePrecision x)) t,
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
  Core.PrecisionBits v -> (Strings.cat [
    Literals.showInt32 v,
    "-bit"])

-- Display a type as a string
describeType :: (Core.Type m -> String)
describeType typ = ((\x -> case x of
  Core.TypeExprApplication _ -> "instances of an application type"
  Core.TypeExprLiteral v -> (describeLiteralType v)
  Core.TypeExprElement v -> (Strings.cat [
    "elements containing ",
    (describeType v)])
  Core.TypeExprFunction v -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "functions from ",
        (describeType (Core.functionTypeDomain v))],
      " to "],
    (describeType (Core.functionTypeCodomain v))])
  Core.TypeExprLambda _ -> "polymorphic terms"
  Core.TypeExprList v -> (Strings.cat [
    "lists of ",
    (describeType v)])
  Core.TypeExprMap v -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "maps from ",
        (describeType (Core.mapTypeKeys v))],
      " to "],
    (describeType (Core.mapTypeValues v))])
  Core.TypeExprNominal v -> (Strings.cat [
    "alias for ",
    (Core.unName v)])
  Core.TypeExprOptional v -> (Strings.cat [
    "optional ",
    (describeType v)])
  Core.TypeExprRecord _ -> "records of a particular set of fields"
  Core.TypeExprSet v -> (Strings.cat [
    "sets of ",
    (describeType v)])
  Core.TypeExprUnion _ -> "unions of a particular set of fields"
  Core.TypeExprVariable _ -> "unspecified/parametric terms") (Core.typeExpr typ))