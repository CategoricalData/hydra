module Hydra.Ext.Json.Language where

import Hydra.Core
import Hydra.Coders
import Hydra.Mantle
import Hydra.Rewriting

import qualified Data.Set as S


jsonLanguage :: Language
jsonLanguage = Language (LanguageName "hydra.ext.json") $ LanguageConstraints {
  languageConstraintsEliminationVariants = S.empty,
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBoolean, LiteralVariantFloat, LiteralVariantInteger, LiteralVariantString],
  languageConstraintsFloatTypes = S.fromList [FloatTypeBigfloat],
  languageConstraintsFunctionVariants = S.empty,
  languageConstraintsIntegerTypes = S.fromList [IntegerTypeBigint],
  languageConstraintsTermVariants = S.fromList [
    TermVariantList,
    TermVariantLiteral,
    TermVariantMap,
    TermVariantOptional,
    TermVariantRecord],
    -- Note: TermVariantUnit is excluded because JSON null is used for optionals
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantList,
    TypeVariantLiteral,
    TypeVariantMap,
    TypeVariantOptional,
    TypeVariantRecord],
    -- Note: TypeVariantUnit is excluded because JSON null is used for optionals
  languageConstraintsTypes = \typ -> case deannotateType typ of
    TypeOptional (TypeOptional _) -> False
    _ -> True }
