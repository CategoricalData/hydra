-- Note: this is an automatically generated file. Do not edit.
-- | Language constraints for W3C SHACL

module Hydra.Shacl.Language where
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Language constraints for W3C SHACL
shaclLanguage :: Coders.Language
shaclLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.shacl"),
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
          Variants.TermVariantWrap,
          Variants.TermVariantMaybe,
          Variants.TermVariantRecord,
          Variants.TermVariantSet,
          Variants.TermVariantInject]
    typeVariants =
        Sets.fromList [
          Variants.TypeVariantAnnotated,
          Variants.TypeVariantList,
          Variants.TypeVariantLiteral,
          Variants.TypeVariantMap,
          Variants.TypeVariantWrap,
          Variants.TypeVariantMaybe,
          Variants.TypeVariantRecord,
          Variants.TypeVariantSet,
          Variants.TypeVariantUnion]
    typePredicate = \_ -> True
