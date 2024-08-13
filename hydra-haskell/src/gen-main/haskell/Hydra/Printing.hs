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
  Core.LiteralTypeFloat v289 -> (describeFloatType v289)
  Core.LiteralTypeInteger v290 -> (describeIntegerType v290)
  Core.LiteralTypeString -> "character strings"

-- | Display numeric precision as a string
describePrecision :: (Mantle.Precision -> String)
describePrecision x = case x of
  Mantle.PrecisionArbitrary -> "arbitrary-precision"
  Mantle.PrecisionBits v293 -> (Strings.cat [
    Literals.showInt32 v293,
    "-bit"])

-- | Display a type as a string
describeType :: (Core.Type -> String)
describeType x = case x of
  Core.TypeAnnotated v294 -> (Strings.cat [
    "annotated ",
    (describeType (Core.annotatedTypeSubject v294))])
  Core.TypeApplication _ -> "instances of an application type"
  Core.TypeLiteral v296 -> (describeLiteralType v296)
  Core.TypeFunction v297 -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "functions from ",
        (describeType (Core.functionTypeDomain v297))],
      " to "],
    (describeType (Core.functionTypeCodomain v297))])
  Core.TypeLambda _ -> "polymorphic terms"
  Core.TypeList v299 -> (Strings.cat [
    "lists of ",
    (describeType v299)])
  Core.TypeMap v300 -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "maps from ",
        (describeType (Core.mapTypeKeys v300))],
      " to "],
    (describeType (Core.mapTypeValues v300))])
  Core.TypeOptional v301 -> (Strings.cat [
    "optional ",
    (describeType v301)])
  Core.TypeProduct _ -> "tuples"
  Core.TypeRecord _ -> "records"
  Core.TypeSet v304 -> (Strings.cat [
    "sets of ",
    (describeType v304)])
  Core.TypeSum _ -> "variant tuples"
  Core.TypeUnion _ -> "unions"
  Core.TypeVariable _ -> "instances of a named type"
  Core.TypeWrap v308 -> (Strings.cat [
    "wrapper for ",
    (describeType (Core.wrappedTypeObject v308))])