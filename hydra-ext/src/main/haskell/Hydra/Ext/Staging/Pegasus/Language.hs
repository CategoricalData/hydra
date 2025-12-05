module Hydra.Ext.Staging.Pegasus.Language where

import Hydra.Kernel

import qualified Data.Set as S


pdlLanguage :: Language
pdlLanguage = Language (LanguageName "hydra.ext.pegasus.pdl") $ LanguageConstraints {
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
    TermVariantEither,
    TermVariantList,
    TermVariantLiteral,
    TermVariantMap,
    TermVariantPair,
    TermVariantSet,
    TermVariantWrap,
    TermVariantMaybe,
    TermVariantRecord,
    TermVariantUnion],
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantAnnotated,
    TypeVariantEither,
    TypeVariantList,
    TypeVariantLiteral,
    TypeVariantMap,
    TypeVariantPair,
    TypeVariantSet,
    TypeVariantWrap,
    TypeVariantMaybe,
    TypeVariantRecord,
    TypeVariantUnion,
    TypeVariantVariable],
  languageConstraintsTypes = const True }
