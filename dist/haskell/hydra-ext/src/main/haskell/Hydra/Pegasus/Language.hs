-- Note: this is an automatically generated file. Do not edit.
-- | Language constraints for LinkedIn Pegasus (PDL)

module Hydra.Pegasus.Language where
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Language constraints for LinkedIn Pegasus (PDL)
pdlLanguage :: Coders.Language
pdlLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.pegasus.pdl"),
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
          Variants.TermVariantEither,
          Variants.TermVariantList,
          Variants.TermVariantLiteral,
          Variants.TermVariantMap,
          Variants.TermVariantPair,
          Variants.TermVariantSet,
          Variants.TermVariantWrap,
          Variants.TermVariantMaybe,
          Variants.TermVariantRecord,
          Variants.TermVariantInject]
    typeVariants =
        Sets.fromList [
          Variants.TypeVariantAnnotated,
          Variants.TypeVariantEither,
          Variants.TypeVariantList,
          Variants.TypeVariantLiteral,
          Variants.TypeVariantMap,
          Variants.TypeVariantPair,
          Variants.TypeVariantSet,
          Variants.TypeVariantWrap,
          Variants.TypeVariantMaybe,
          Variants.TypeVariantRecord,
          Variants.TypeVariantUnion,
          Variants.TypeVariantVariable]
    typePredicate = \_ -> True
