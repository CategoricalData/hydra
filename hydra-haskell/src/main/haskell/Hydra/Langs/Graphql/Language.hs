module Hydra.Langs.Graphql.Language where

import Hydra.Kernel

import qualified Data.Set as S


graphqlLanguage :: Language m
graphqlLanguage = Language (LanguageName "hydra/langs/graphql") $ LanguageConstraints {
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
    TypeVariantWrap,
    TypeVariantOptional,
    TypeVariantRecord,
    TypeVariantUnion,
    TypeVariantVariable],
  languageConstraintsTypes = const True }
