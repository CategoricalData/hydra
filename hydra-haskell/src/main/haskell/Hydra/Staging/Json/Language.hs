module Hydra.Staging.Json.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Rewriting as Rewriting

import qualified Data.Set as S


jsonLanguage :: Coders.Language
jsonLanguage = Coders.Language (Coders.LanguageName "hydra.ext.json") $ Coders.LanguageConstraints {
  Coders.languageConstraintsEliminationVariants = S.empty,
  Coders.languageConstraintsLiteralVariants = S.fromList [
    Mantle.LiteralVariantBoolean, Mantle.LiteralVariantFloat, Mantle.LiteralVariantInteger, Mantle.LiteralVariantString],
  Coders.languageConstraintsFloatTypes = S.fromList [Core.FloatTypeBigfloat],
  Coders.languageConstraintsFunctionVariants = S.empty,
  Coders.languageConstraintsIntegerTypes = S.fromList [Core.IntegerTypeBigint],
  Coders.languageConstraintsTermVariants = S.fromList [
    Mantle.TermVariantList,
    Mantle.TermVariantLiteral,
    Mantle.TermVariantMap,
    Mantle.TermVariantOptional,
    Mantle.TermVariantRecord],
    -- Note: TermVariantUnit is excluded because JSON null is used for optionals
  Coders.languageConstraintsTypeVariants = S.fromList [
    Mantle.TypeVariantList,
    Mantle.TypeVariantLiteral,
    Mantle.TypeVariantMap,
    Mantle.TypeVariantOptional,
    Mantle.TypeVariantRecord],
    -- Note: TypeVariantUnit is excluded because JSON null is used for optionals
  Coders.languageConstraintsTypes = \typ -> case Rewriting.deannotateType typ of
    Core.TypeOptional (Core.TypeOptional _) -> False
    _ -> True }