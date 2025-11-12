-- | Natural-language descriptions for hydra.core types

module Hydra.Describe.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Describe.Util as Util
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Display a floating-point type as a string
floatType :: (Core.FloatType -> String)
floatType t = (Strings.cat [
  (\arg_ -> Util.precision (Variants.floatTypePrecision arg_)) t,
  " floating-point number"])

-- | Display an integer type as a string
integerType :: (Core.IntegerType -> String)
integerType t = (Strings.cat [
  (\arg_ -> Util.precision (Variants.integerTypePrecision arg_)) t,
  " integer"])

-- | Display a literal type as a string
literalType :: (Core.LiteralType -> String)
literalType x = case x of
  Core.LiteralTypeBinary -> "binary string"
  Core.LiteralTypeBoolean -> "boolean value"
  Core.LiteralTypeFloat v1 -> (floatType v1)
  Core.LiteralTypeInteger v1 -> (integerType v1)
  Core.LiteralTypeString -> "character string"

-- | Display a type as a string
type_ :: (Core.Type -> String)
type_ x = case x of
  Core.TypeAnnotated v1 -> (Strings.cat [
    "annotated ",
    (type_ (Core.annotatedTypeBody v1))])
  Core.TypeApplication v1 -> (Strings.cat [
    type_ (Core.applicationTypeFunction v1),
    " applied to ",
    (type_ (Core.applicationTypeArgument v1))])
  Core.TypeEither v1 -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "either ",
        (type_ (Core.eitherTypeLeft v1))],
      " or "],
    (type_ (Core.eitherTypeRight v1))])
  Core.TypeLiteral v1 -> (literalType v1)
  Core.TypeFunction v1 -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "function from ",
        (type_ (Core.functionTypeDomain v1))],
      " to "],
    (type_ (Core.functionTypeCodomain v1))])
  Core.TypeForall v1 -> (Strings.cat2 "polymorphic " (type_ (Core.forallTypeBody v1)))
  Core.TypeList v1 -> (Strings.cat [
    "list of ",
    (type_ v1)])
  Core.TypeMap v1 -> (Strings.cat [
    Strings.cat [
      Strings.cat [
        "map from ",
        (type_ (Core.mapTypeKeys v1))],
      " to "],
    (type_ (Core.mapTypeValues v1))])
  Core.TypeMaybe v1 -> (Strings.cat [
    "maybe ",
    (type_ v1)])
  Core.TypeProduct _ -> "tuple"
  Core.TypeRecord _ -> "record"
  Core.TypeSet v1 -> (Strings.cat [
    "set of ",
    (type_ v1)])
  Core.TypeSum _ -> "variant tuple"
  Core.TypeUnion _ -> "union"
  Core.TypeUnit -> "unit"
  Core.TypeVariable _ -> "instance of a named type"
  Core.TypeWrap v1 -> (Strings.cat [
    "wrapper for ",
    (type_ (Core.wrappedTypeBody v1))])
