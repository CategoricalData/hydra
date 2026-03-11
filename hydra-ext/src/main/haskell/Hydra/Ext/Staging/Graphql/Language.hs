module Hydra.Ext.Staging.Graphql.Language where

import Hydra.Kernel

import qualified Data.Set as S


-- Note: the canonical definition is now in Hydra.Ext.Sources.Graphql.Language.
-- This module is kept for backward compatibility.

graphqlLanguage :: Language
graphqlLanguage = Language (LanguageName "hydra.ext.graphql") $ LanguageConstraints {
  languageConstraintsEliminationVariants = S.empty,
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBoolean, LiteralVariantFloat, LiteralVariantInteger, LiteralVariantString],
  languageConstraintsFloatTypes = S.fromList [FloatTypeFloat64],
  languageConstraintsFunctionVariants = S.empty,
  languageConstraintsIntegerTypes = S.fromList [IntegerTypeInt32],
  languageConstraintsTermVariants = S.fromList [
    TermVariantList, TermVariantLiteral, TermVariantMaybe, TermVariantRecord, TermVariantUnion],
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantEither, TypeVariantList, TypeVariantLiteral, TypeVariantMap, TypeVariantPair,
    TypeVariantSet, TypeVariantWrap, TypeVariantMaybe, TypeVariantRecord, TypeVariantUnion, TypeVariantVariable],
  languageConstraintsTypes = \typ -> case deannotateType typ of
    TypeMaybe et -> case deannotateType et of
      TypeMaybe _ -> False
      _ -> True
    _ -> True}

graphqlReservedWords :: S.Set String
graphqlReservedWords = S.fromList ["true", "false"]
