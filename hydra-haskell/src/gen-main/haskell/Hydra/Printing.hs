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
  Core.LiteralTypeFloat v248 -> (describeFloatType v248)
  Core.LiteralTypeInteger v249 -> (describeIntegerType v249)
  Core.LiteralTypeString -> "character strings"

-- | Display numeric precision as a string
describePrecision :: (Mantle.Precision -> String)
describePrecision x = case x of
  Mantle.PrecisionArbitrary -> "arbitrary-precision"
  Mantle.PrecisionBits v252 -> (Strings.cat [
    Literals.showInt32 v252,
    "-bit"])

-- | Display a type as a string
describeType :: (Core.Type -> String)
describeType x = case x of
  Core.TypeAnnotated v253 -> (Strings.cat [
    "annotated ",
    (describeType (Core.annotatedSubject v253))])
  Core.TypeApplication _ -> "instances of an application type"
  Core.TypeLiteral v255 -> (describeLiteralType v255)
  Core.TypeFunction v256 -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "functions from ",
        (describeType (Core.functionTypeDomain v256))],
      " to "],
    (describeType (Core.functionTypeCodomain v256))])
  Core.TypeLambda _ -> "polymorphic terms"
  Core.TypeList v258 -> (Strings.cat [
    "lists of ",
    (describeType v258)])
  Core.TypeMap v259 -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "maps from ",
        (describeType (Core.mapTypeKeys v259))],
      " to "],
    (describeType (Core.mapTypeValues v259))])
  Core.TypeOptional v260 -> (Strings.cat [
    "optional ",
    (describeType v260)])
  Core.TypeProduct _ -> "tuples"
  Core.TypeRecord _ -> "records"
  Core.TypeSet v263 -> (Strings.cat [
    "sets of ",
    (describeType v263)])
  Core.TypeStream v264 -> (Strings.cat [
    "streams of ",
    (describeType v264)])
  Core.TypeSum _ -> "variant tuples"
  Core.TypeUnion _ -> "unions"
  Core.TypeVariable _ -> "instances of a named type"
  Core.TypeWrap v268 -> (Strings.cat [
    "wrapper for ",
    (describeType (Core.nominalObject v268))])