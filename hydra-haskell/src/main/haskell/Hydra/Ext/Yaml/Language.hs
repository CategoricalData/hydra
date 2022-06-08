module Hydra.Ext.Yaml.Language where

import Hydra.Core
import Hydra.Adapter
import Hydra.Basics

import qualified Data.Set as S


yamlLanguage :: Language m
yamlLanguage = Language (LanguageName "hydra/ext/yaml") $ LanguageConstraints {
  languageConstraintsEliminationVariants = S.empty,
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBoolean, LiteralVariantFloat, LiteralVariantInteger, LiteralVariantString],
  languageConstraintsFloatTypes = S.fromList [FloatTypeBigfloat],
  languageConstraintsFunctionVariants = S.empty,
  languageConstraintsIntegerTypes = S.fromList [IntegerTypeBigint],
  languageConstraintsDataVariants = S.fromList termVariants,
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantLiteral, TypeVariantList, TypeVariantMap, TypeVariantOptional, TypeVariantRecord],
  languageConstraintsTypes = \typ -> case typeTerm typ of
    TypeTermOptional (Type (TypeTermOptional _) _) -> False
    _ -> True }
