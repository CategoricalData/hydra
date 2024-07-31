-- | Utilities for use in transformations

module Hydra.Printing where

import qualified Hydra.Basics as Basics
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | Display a floating-point type as a string
describeFloatType :: (Core.FloatType -> String)
describeFloatType t = (Strings.cat [
  (\x -> describePrecision (Basics.floatTypePrecision x)) t,
  " floating-point numbers"])

-- | Display an integer type as a string
describeIntegerType :: (Core.IntegerType -> String)
describeIntegerType t = (Strings.cat [
  (\x -> describePrecision (Basics.integerTypePrecision x)) t,
  " integers"])

-- | Display a literal type as a string
describeLiteralType :: (Core.LiteralType -> String)
describeLiteralType x = case x of
  Core.LiteralTypeBinary -> "binary strings"
  Core.LiteralTypeBoolean -> "boolean values"
  Core.LiteralTypeFloat v246 -> (describeFloatType v246)
  Core.LiteralTypeInteger v247 -> (describeIntegerType v247)
  Core.LiteralTypeString -> "character strings"

-- | Display numeric precision as a string
describePrecision :: (Mantle.Precision -> String)
describePrecision x = case x of
  Mantle.PrecisionArbitrary -> "arbitrary-precision"
  Mantle.PrecisionBits v250 -> (Strings.cat [
    Literals.showInt32 v250,
    "-bit"])

-- | Display a type as a string
describeType :: (Core.Type -> String)
describeType x = case x of
  Core.TypeAnnotated v251 -> (Strings.cat [
    "annotated ",
    (describeType (Core.annotatedTypeSubject v251))])
  Core.TypeApplication _ -> "instances of an application type"
  Core.TypeLiteral v253 -> (describeLiteralType v253)
  Core.TypeFunction v254 -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "functions from ",
        (describeType (Core.functionTypeDomain v254))],
      " to "],
    (describeType (Core.functionTypeCodomain v254))])
  Core.TypeLambda _ -> "polymorphic terms"
  Core.TypeList v256 -> (Strings.cat [
    "lists of ",
    (describeType v256)])
  Core.TypeMap v257 -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "maps from ",
        (describeType (Core.mapTypeKeys v257))],
      " to "],
    (describeType (Core.mapTypeValues v257))])
  Core.TypeOptional v258 -> (Strings.cat [
    "optional ",
    (describeType v258)])
  Core.TypeProduct _ -> "tuples"
  Core.TypeRecord _ -> "records"
  Core.TypeSet v261 -> (Strings.cat [
    "sets of ",
    (describeType v261)])
  Core.TypeSum _ -> "variant tuples"
  Core.TypeUnion _ -> "unions"
  Core.TypeVariable _ -> "instances of a named type"
  Core.TypeWrap v265 -> (Strings.cat [
    "wrapper for ",
    (describeType (Core.wrappedTypeObject v265))])