module Hydra.Staging.Yaml.Language where

import Hydra.Kernel

import qualified Data.Set as S


yamlLanguage :: Language
yamlLanguage = Language (LanguageName "hydra.ext.yaml") $ LanguageConstraints {
  languageConstraintsEliminationVariants = S.empty,
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBoolean, LiteralVariantFloat, LiteralVariantInteger, LiteralVariantString],
  languageConstraintsFloatTypes = S.fromList [FloatTypeBigfloat],
  languageConstraintsFunctionVariants = S.empty,
  languageConstraintsIntegerTypes = S.fromList [IntegerTypeBigint],
  languageConstraintsTermVariants = S.fromList [
    TermVariantLiteral,
    TermVariantList,
    TermVariantMap,
    TermVariantOptional,
    TermVariantRecord,
    TermVariantUnit],
    -- Note: TermVariantUnit is excluded because YAML null is used for optionals
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantLiteral,
    TypeVariantList,
    TypeVariantMap,
    TypeVariantOptional,
    TypeVariantRecord,
    TypeVariantUnit],
    -- Note: TypeVariantUnit is excluded because YAML null is used for optionals
  languageConstraintsTypes = \typ -> case deannotateType typ of
    TypeOptional (TypeOptional _) -> False
    _ -> True }
