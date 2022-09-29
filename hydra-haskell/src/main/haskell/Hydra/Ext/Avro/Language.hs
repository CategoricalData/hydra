module Hydra.Ext.Avro.Language where

import Hydra.Core
import Hydra.Adapter
import Hydra.Common
import Hydra.Compute

import qualified Data.Set as S


language :: Context m -> Language m
language cx = Language (LanguageName "hydra/ext/avro") $ LanguageConstraints {
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
    TypeVariantNominal,
    TypeVariantOptional,
    TypeVariantRecord],
  languageConstraintsTypes = \typ -> case stripType typ of
    TypeOptional (TypeOptional _) -> False
    _ -> True }
