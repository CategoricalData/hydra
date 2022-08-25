module Hydra.Ext.Pegasus.Language where

import Hydra.Adapter
import Hydra.Core

import qualified Data.Set as S


language :: Language m
language = Language (LanguageName "hydra/ext/pegasus/pdl") $ LanguageConstraints {
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
    TermVariantNominal,
    TermVariantOptional,
    TermVariantRecord,
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
    TypeVariantUnion],
  languageConstraintsTypes = const True }
