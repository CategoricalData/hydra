-- | Utilities for use in transformations

module Hydra.Describe.Core where

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
floatType :: (Core.FloatType -> String)
floatType t = (Strings.cat [
  (\arg_ -> precision (Variants.floatTypePrecision arg_)) t,
  " floating-point numbers"])

-- | Display an integer type as a string
integerType :: (Core.IntegerType -> String)
integerType t = (Strings.cat [
  (\arg_ -> precision (Variants.integerTypePrecision arg_)) t,
  " integers"])

-- | Display a literal type as a string
literalType :: (Core.LiteralType -> String)
literalType x = case x of
  Core.LiteralTypeBinary -> "binary strings"
  Core.LiteralTypeBoolean -> "boolean values"
  Core.LiteralTypeFloat v1 -> (floatType v1)
  Core.LiteralTypeInteger v1 -> (integerType v1)
  Core.LiteralTypeString -> "character strings"

-- | Display numeric precision as a string
precision :: (Mantle.Precision -> String)
precision x = case x of
  Mantle.PrecisionArbitrary -> "arbitrary-precision"
  Mantle.PrecisionBits v1 -> (Strings.cat [
    Literals.showInt32 v1,
    "-bit"])

-- | Display a type as a string
type_ :: (Core.Type -> String)
type_ x = case x of
  Core.TypeAnnotated v1 -> (Strings.cat [
    "annotated ",
    (type_ (Core.annotatedTypeSubject v1))])
  Core.TypeApplication _ -> "instances of an application type"
  Core.TypeLiteral v1 -> (literalType v1)
  Core.TypeFunction v1 -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "functions from ",
        (type_ (Core.functionTypeDomain v1))],
      " to "],
    (type_ (Core.functionTypeCodomain v1))])
  Core.TypeForall _ -> "polymorphic terms"
  Core.TypeList v1 -> (Strings.cat [
    "lists of ",
    (type_ v1)])
  Core.TypeMap v1 -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "maps from ",
        (type_ (Core.mapTypeKeys v1))],
      " to "],
    (type_ (Core.mapTypeValues v1))])
  Core.TypeOptional v1 -> (Strings.cat [
    "optional ",
    (type_ v1)])
  Core.TypeProduct _ -> "tuples"
  Core.TypeRecord _ -> "records"
  Core.TypeSet v1 -> (Strings.cat [
    "sets of ",
    (type_ v1)])
  Core.TypeSum _ -> "variant tuples"
  Core.TypeUnion _ -> "unions"
  Core.TypeVariable _ -> "instances of a named type"
  Core.TypeWrap v1 -> (Strings.cat [
    "wrapper for ",
    (type_ (Core.wrappedTypeObject v1))])
