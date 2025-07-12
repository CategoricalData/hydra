module Hydra.Staging.Json.Schema.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Variants as Variants

import qualified Data.Set as S


jsonSchemaLanguage :: Coders.Language
jsonSchemaLanguage = Coders.Language (Coders.LanguageName "hydra.ext.json.schema") $ Coders.LanguageConstraints {
  Coders.languageConstraintsEliminationVariants = S.empty,
  Coders.languageConstraintsLiteralVariants = S.fromList [
    Mantle.LiteralVariantBoolean,
    Mantle.LiteralVariantFloat,
    Mantle.LiteralVariantInteger,
    Mantle.LiteralVariantString],
  Coders.languageConstraintsFloatTypes = S.fromList [Core.FloatTypeBigfloat],
  Coders.languageConstraintsFunctionVariants = S.empty,
  Coders.languageConstraintsIntegerTypes = S.fromList [Core.IntegerTypeBigint],
  Coders.languageConstraintsTermVariants = S.fromList Variants.termVariants,
  Coders.languageConstraintsTypeVariants = S.fromList [
    Mantle.TypeVariantAnnotated,
    Mantle.TypeVariantList,
    Mantle.TypeVariantLiteral,
    Mantle.TypeVariantMap,
    Mantle.TypeVariantOptional,
    Mantle.TypeVariantRecord,
    Mantle.TypeVariantUnion,
    Mantle.TypeVariantVariable],
  Coders.languageConstraintsTypes = \typ -> True }