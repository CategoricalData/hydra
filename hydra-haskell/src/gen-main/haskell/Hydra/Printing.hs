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
  Core.LiteralTypeFloat v250 -> (describeFloatType v250)
  Core.LiteralTypeInteger v251 -> (describeIntegerType v251)
  Core.LiteralTypeString -> "character strings"

-- | Display numeric precision as a string
describePrecision :: (Mantle.Precision -> String)
describePrecision x = case x of
  Mantle.PrecisionArbitrary -> "arbitrary-precision"
  Mantle.PrecisionBits v254 -> (Strings.cat [
    Literals.showInt32 v254,
    "-bit"])

-- | Display a type as a string
describeType :: (Core.Type -> String)
describeType x = case x of
  Core.TypeAnnotated v255 -> (Strings.cat [
    "annotated ",
    (describeType (Core.annotatedTypeSubject v255))])
  Core.TypeApplication _ -> "instances of an application type"
  Core.TypeLiteral v257 -> (describeLiteralType v257)
  Core.TypeFunction v258 -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "functions from ",
        (describeType (Core.functionTypeDomain v258))],
      " to "],
    (describeType (Core.functionTypeCodomain v258))])
  Core.TypeLambda _ -> "polymorphic terms"
  Core.TypeList v260 -> (Strings.cat [
    "lists of ",
    (describeType v260)])
  Core.TypeMap v261 -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "maps from ",
        (describeType (Core.mapTypeKeys v261))],
      " to "],
    (describeType (Core.mapTypeValues v261))])
  Core.TypeOptional v262 -> (Strings.cat [
    "optional ",
    (describeType v262)])
  Core.TypeProduct _ -> "tuples"
  Core.TypeRecord _ -> "records"
  Core.TypeSet v265 -> (Strings.cat [
    "sets of ",
    (describeType v265)])
  Core.TypeStream v266 -> (Strings.cat [
    "streams of ",
    (describeType v266)])
  Core.TypeSum _ -> "variant tuples"
  Core.TypeUnion _ -> "unions"
  Core.TypeVariable _ -> "instances of a named type"
  Core.TypeWrap v270 -> (Strings.cat [
    "wrapper for ",
    (describeType (Core.wrappedTypeObject v270))])