-- Note: this is an automatically generated file. Do not edit.
-- | Language constraints for Apache Avro

module Hydra.Avro.Language where
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Strip as Strip
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Language constraints for Apache Avro
avroLanguage :: Coders.Language
avroLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.avro"),
      Coders.languageConstraints = Coders.LanguageConstraints {
        Coders.languageConstraintsLiteralVariants = literalVariants,
        Coders.languageConstraintsFloatTypes = floatTypes,
        Coders.languageConstraintsIntegerTypes = integerTypes,
        Coders.languageConstraintsTermVariants = termVariants,
        Coders.languageConstraintsTypeVariants = typeVariants,
        Coders.languageConstraintsTypes = typePredicate}}
  where
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
    integerTypes =
        Sets.fromList [
          Core.IntegerTypeInt32,
          Core.IntegerTypeInt64]
    termVariants =
        Sets.fromList [
          Variants.TermVariantList,
          Variants.TermVariantLiteral,
          Variants.TermVariantMap,
          Variants.TermVariantMaybe,
          Variants.TermVariantRecord]
    typeVariants =
        Sets.fromList [
          Variants.TypeVariantAnnotated,
          Variants.TypeVariantList,
          Variants.TypeVariantLiteral,
          Variants.TypeVariantMap,
          Variants.TypeVariantWrap,
          Variants.TypeVariantMaybe,
          Variants.TypeVariantRecord]
    typePredicate =
        \typ -> case (Strip.deannotateType typ) of
          Core.TypeMaybe v0 -> case (Strip.deannotateType v0) of
            Core.TypeMaybe _ -> False
            _ -> True
          _ -> True
