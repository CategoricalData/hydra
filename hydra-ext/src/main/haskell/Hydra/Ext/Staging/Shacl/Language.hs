module Hydra.Ext.Staging.Shacl.Language where

import Hydra.Kernel

import qualified Data.Set as S


-- Note: the canonical definition is now in Hydra.Ext.Sources.Shacl.Language.
-- This module is kept for backward compatibility.

shaclLanguage :: Language
shaclLanguage = Language (LanguageName "hydra.ext.shacl") $ LanguageConstraints {
  languageConstraintsEliminationVariants = S.empty,
  languageConstraintsLiteralVariants = S.fromList [
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
    TermVariantWrap,
    TermVariantMaybe,
    TermVariantRecord,
    TermVariantSet,
    TermVariantUnion],
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantAnnotated,
    TypeVariantList,
    TypeVariantLiteral,
    TypeVariantMap,
    TypeVariantWrap,
    TypeVariantMaybe,
    TypeVariantRecord,
    TypeVariantSet,
    TypeVariantUnion],
  languageConstraintsTypes = const True }
