module Hydra.Ext.Shacl.Language where

import Hydra.All

import qualified Data.Set as S


shaclLanguage :: Language m
shaclLanguage = Language (LanguageName "hydra/ext/shacl") $ LanguageConstraints {
  languageConstraintsEliminationVariants = S.empty,
  languageConstraintsLiteralVariants = S.fromList literalVariants,
  languageConstraintsFloatTypes = S.fromList floatTypes,
  languageConstraintsFunctionVariants = S.empty,
  languageConstraintsIntegerTypes = S.fromList integerTypes,
  languageConstraintsTermVariants = S.fromList [
    TermVariantElement,
    TermVariantList,
    TermVariantLiteral,
    TermVariantMap,
    TermVariantNominal,
    TermVariantOptional,
    TermVariantRecord,
    TermVariantSet,
    TermVariantUnion],
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantAnnotated,
    TypeVariantElement,
    TypeVariantList,
    TypeVariantLiteral,
    TypeVariantMap,
    TypeVariantNominal,
    TypeVariantOptional,
    TypeVariantRecord,
    TypeVariantSet,
    TypeVariantUnion],
  languageConstraintsTypes = const True }
