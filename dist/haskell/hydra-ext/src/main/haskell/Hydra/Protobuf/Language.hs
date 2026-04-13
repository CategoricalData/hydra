-- Note: this is an automatically generated file. Do not edit.

-- | Language constraints for Protobuf v3

module Hydra.Protobuf.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Strip as Strip
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Set as S

-- | Language constraints for Protocol Buffers v3
protobufLanguage :: Coders.Language
protobufLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.protobuf"),
      Coders.languageConstraints = Coders.LanguageConstraints {
        Coders.languageConstraintsEliminationVariants = eliminationVariants,
        Coders.languageConstraintsLiteralVariants = literalVariants,
        Coders.languageConstraintsFloatTypes = floatTypes,
        Coders.languageConstraintsFunctionVariants = functionVariants,
        Coders.languageConstraintsIntegerTypes = integerTypes,
        Coders.languageConstraintsTermVariants = termVariants,
        Coders.languageConstraintsTypeVariants = typeVariants,
        Coders.languageConstraintsTypes = typePredicate}}
  where
    eliminationVariants = Sets.empty
    literalVariants =
        Sets.fromList [
          Variants.LiteralVariantBinary,
          Variants.LiteralVariantBoolean,
          Variants.LiteralVariantFloat,
          Variants.LiteralVariantInteger,
          Variants.LiteralVariantString]
    floatTypes =
        Sets.fromList [
          Core.FloatTypeFloat32,
          Core.FloatTypeFloat64]
    functionVariants = Sets.empty
    integerTypes =
        Sets.fromList [
          Core.IntegerTypeInt32,
          Core.IntegerTypeInt64,
          Core.IntegerTypeUint32,
          Core.IntegerTypeUint64]
    termVariants =
        Sets.fromList [
          Variants.TermVariantEither,
          Variants.TermVariantList,
          Variants.TermVariantLiteral,
          Variants.TermVariantMap,
          Variants.TermVariantMaybe,
          Variants.TermVariantPair,
          Variants.TermVariantRecord,
          Variants.TermVariantSet,
          Variants.TermVariantUnion,
          Variants.TermVariantUnit,
          Variants.TermVariantWrap]
    typeVariants =
        Sets.fromList [
          Variants.TypeVariantAnnotated,
          Variants.TypeVariantEither,
          Variants.TypeVariantList,
          Variants.TypeVariantLiteral,
          Variants.TypeVariantMap,
          Variants.TypeVariantMaybe,
          Variants.TypeVariantPair,
          Variants.TypeVariantRecord,
          Variants.TypeVariantSet,
          Variants.TypeVariantUnion,
          Variants.TypeVariantUnit,
          Variants.TypeVariantVariable,
          Variants.TypeVariantVoid,
          Variants.TypeVariantWrap]
    typePredicate =
        \typ -> case typ of
          Core.TypeMap v0 ->
            let valuesType = Core.mapTypeValues v0
                stripped = Strip.deannotateType valuesType
            in case stripped of
              Core.TypeMaybe _ -> False
              _ -> True
          _ -> True

-- | A set of reserved words in Protobuf
protobufReservedWords :: S.Set String
protobufReservedWords = Sets.fromList (Lists.concat [
  fieldNames])
  where
    fieldNames =
        [
          "case",
          "class",
          "data",
          "default",
          "deriving",
          "do",
          "else",
          "foreign",
          "if",
          "import",
          "in",
          "infix",
          "infixl",
          "infixr",
          "instance",
          "let",
          "mdo",
          "module",
          "newtype",
          "of",
          "pattern",
          "proc",
          "rec",
          "then",
          "type",
          "where"]
