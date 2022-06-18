module Hydra.Ext.Yaml.Language where

import Hydra.Core
import Hydra.Adapter
import Hydra.Basics

import qualified Data.Set as S


language :: Language m
language = Language (LanguageName "hydra/ext/yaml") $ LanguageConstraints {
  languageConstraintsEliminationVariants = S.empty,
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBoolean, LiteralVariantFloat, LiteralVariantInteger, LiteralVariantString],
  languageConstraintsFloatTypes = S.fromList [FloatTypeBigfloat],
  languageConstraintsFunctionVariants = S.empty,
  languageConstraintsIntegerTypes = S.fromList [IntegerTypeBigint],
  languageConstraintsTermVariants = S.fromList termVariants,
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantLiteral, TypeVariantList, TypeVariantMap, TypeVariantOptional, TypeVariantRecord],
  languageConstraintsTypes = \typ -> case typeExpr typ of
    TypeExprOptional (Type (TypeExprOptional _) _) -> False
    _ -> True }
