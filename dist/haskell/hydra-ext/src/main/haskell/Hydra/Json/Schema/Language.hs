-- Note: this is an automatically generated file. Do not edit.
-- | Language constraints for JSON Schema

module Hydra.Json.Schema.Language where
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Language constraints for JSON Schema
jsonSchemaLanguage :: Coders.Language
jsonSchemaLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.json.schema"),
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
    floatTypes = Sets.fromList [
      Core.FloatTypeFloat64]
    integerTypes = Sets.fromList [
      Core.IntegerTypeBigint]
    termVariants = Sets.fromList Reflect.termVariants
    typeVariants =
        Sets.fromList [
          Variants.TypeVariantAnnotated,
          Variants.TypeVariantApplication,
          Variants.TypeVariantEither,
          Variants.TypeVariantForall,
          Variants.TypeVariantList,
          Variants.TypeVariantLiteral,
          Variants.TypeVariantMap,
          Variants.TypeVariantMaybe,
          Variants.TypeVariantPair,
          Variants.TypeVariantRecord,
          Variants.TypeVariantSet,
          Variants.TypeVariantUnion,
          Variants.TypeVariantVariable,
          Variants.TypeVariantWrap]
    typePredicate = \_ -> True
