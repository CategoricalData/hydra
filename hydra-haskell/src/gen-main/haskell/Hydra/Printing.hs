-- | Utilities for use in transformations

module Hydra.Printing where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Variants as Variants
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Display a floating-point type as a string
describeFloatType :: (Core.FloatType -> String)
describeFloatType t = (Strings.cat [
  (\arg_ -> describePrecision (Variants.floatTypePrecision arg_)) t,
  " floating-point numbers"])

-- | Display an integer type as a string
describeIntegerType :: (Core.IntegerType -> String)
describeIntegerType t = (Strings.cat [
  (\arg_ -> describePrecision (Variants.integerTypePrecision arg_)) t,
  " integers"])

-- | Display a literal type as a string
describeLiteralType :: (Core.LiteralType -> String)
describeLiteralType x = case x of
  Core.LiteralTypeBinary -> "binary strings"
  Core.LiteralTypeBoolean -> "boolean values"
  Core.LiteralTypeFloat v1 -> (describeFloatType v1)
  Core.LiteralTypeInteger v1 -> (describeIntegerType v1)
  Core.LiteralTypeString -> "character strings"

-- | Display numeric precision as a string
describePrecision :: (Mantle.Precision -> String)
describePrecision x = case x of
  Mantle.PrecisionArbitrary -> "arbitrary-precision"
  Mantle.PrecisionBits v1 -> (Strings.cat [
    Literals.showInt32 v1,
    "-bit"])

-- | Display a type as a string
describeType :: (Core.Type -> String)
describeType x = case x of
  Core.TypeAnnotated v1 -> (Strings.cat [
    "annotated ",
    (describeType (Core.annotatedTypeSubject v1))])
  Core.TypeApplication _ -> "instances of an application type"
  Core.TypeLiteral v1 -> (describeLiteralType v1)
  Core.TypeFunction v1 -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "functions from ",
        (describeType (Core.functionTypeDomain v1))],
      " to "],
    (describeType (Core.functionTypeCodomain v1))])
  Core.TypeLambda _ -> "polymorphic terms"
  Core.TypeList v1 -> (Strings.cat [
    "lists of ",
    (describeType v1)])
  Core.TypeMap v1 -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "maps from ",
        (describeType (Core.mapTypeKeys v1))],
      " to "],
    (describeType (Core.mapTypeValues v1))])
  Core.TypeOptional v1 -> (Strings.cat [
    "optional ",
    (describeType v1)])
  Core.TypeProduct _ -> "tuples"
  Core.TypeRecord _ -> "records"
  Core.TypeSet v1 -> (Strings.cat [
    "sets of ",
    (describeType v1)])
  Core.TypeSum _ -> "variant tuples"
  Core.TypeUnion _ -> "unions"
  Core.TypeVariable _ -> "instances of a named type"
  Core.TypeWrap v1 -> (Strings.cat [
    "wrapper for ",
    (describeType (Core.wrappedTypeObject v1))])
