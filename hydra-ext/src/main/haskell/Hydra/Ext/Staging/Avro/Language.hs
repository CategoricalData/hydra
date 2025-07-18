module Hydra.Ext.Staging.Avro.Language where

import Hydra.Kernel

import qualified Data.Set as S


avroLanguage :: Language
avroLanguage = Language (LanguageName "hydra.ext.avro") $ LanguageConstraints {
  languageConstraintsEliminationVariants = S.empty,
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBinary, LiteralVariantBoolean, LiteralVariantFloat, LiteralVariantInteger, LiteralVariantString],
  languageConstraintsFloatTypes = S.fromList [FloatTypeFloat32, FloatTypeFloat64],
  languageConstraintsFunctionVariants = S.empty,
  languageConstraintsIntegerTypes = S.fromList [IntegerTypeInt32, IntegerTypeInt64],
  languageConstraintsTermVariants = S.fromList [
    TermVariantList,
    TermVariantLiteral,
    TermVariantMap,
    TermVariantOptional,
    TermVariantRecord],
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantAnnotated,
    TypeVariantList,
    TypeVariantLiteral,
    TypeVariantMap,
    TypeVariantWrap,
    TypeVariantOptional,
    TypeVariantRecord],
  languageConstraintsTypes = \typ -> case deannotateType typ of
    TypeOptional (TypeOptional _) -> False
    _ -> True }
