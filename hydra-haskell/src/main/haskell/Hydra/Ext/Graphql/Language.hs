module Hydra.Ext.Graphql.Language where

import Hydra.All

import qualified Data.Set as S


language :: Language m
language = Language (LanguageName "hydra/ext/graphql") $ LanguageConstraints {
  -- Note: this language is for schemas and data only; support for queries may be added later
  languageConstraintsEliminationVariants = S.empty,
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBoolean,
    LiteralVariantFloat,
    LiteralVariantInteger,
    LiteralVariantString],
  languageConstraintsFloatTypes = S.fromList [
    FloatTypeBigfloat],
  languageConstraintsFunctionVariants = S.empty,
  languageConstraintsIntegerTypes = S.fromList [
    IntegerTypeBigint],
  languageConstraintsTermVariants = S.fromList [
    TermVariantList,
    TermVariantLiteral,
    TermVariantOptional,
    TermVariantRecord,
    TermVariantUnion],
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantApplication,
    TypeVariantLambda,
    TypeVariantList,
    TypeVariantLiteral,
    TypeVariantNominal,
    TypeVariantOptional,
    TypeVariantRecord,
    TypeVariantUnion,
    TypeVariantVariable],
  languageConstraintsTypes = const True }
