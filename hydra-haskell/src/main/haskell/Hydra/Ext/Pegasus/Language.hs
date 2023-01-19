module Hydra.Ext.Pegasus.Language where

import Hydra.Kernel

import qualified Data.Set as S


pdlLanguage :: Language m
pdlLanguage = Language (LanguageName "hydra/ext/pegasus/pdl") $ LanguageConstraints {
  languageConstraintsEliminationVariants = S.empty,
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBinary,
    LiteralVariantBoolean,
    LiteralVariantFloat,
    LiteralVariantInteger,
    LiteralVariantString],
  languageConstraintsFloatTypes = S.fromList [
    FloatTypeFloat32,
    FloatTypeFloat64],
  languageConstraintsFunctionVariants = S.empty,
  languageConstraintsIntegerTypes = S.fromList [
    IntegerTypeInt32,
    IntegerTypeInt64],
  languageConstraintsTermVariants = S.fromList [
    TermVariantList,
    TermVariantLiteral,
    TermVariantMap,
    TermVariantWrapped,
    TermVariantOptional,
    TermVariantRecord,
    TermVariantUnion],
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantAnnotated,
    TypeVariantElement,
    TypeVariantList,
    TypeVariantLiteral,
    TypeVariantMap,
    TypeVariantWrapped,
    TypeVariantOptional,
    TypeVariantRecord,
    TypeVariantUnion],
  languageConstraintsTypes = const True }
