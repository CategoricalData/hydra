module Hydra.Ext.Staging.Graphql.Language where

import Hydra.Kernel
import qualified Hydra.Encode.Core as EncodeCore

import qualified Data.List as L
import qualified Data.Set as S


graphqlLanguage :: Language
graphqlLanguage = Language (LanguageName "hydra.ext.graphql") $ LanguageConstraints {
  -- Note: this language is for schemas and data only; support for queries may be added later
  languageConstraintsEliminationVariants = S.empty,
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBoolean, -- Boolean
    LiteralVariantFloat,   -- Float
    LiteralVariantInteger, -- Int
    LiteralVariantString], -- String
  languageConstraintsFloatTypes = S.fromList [
    FloatTypeFloat64], -- Float
  languageConstraintsFunctionVariants = S.empty,
  languageConstraintsIntegerTypes = S.fromList [
    IntegerTypeInt32], -- Int
  languageConstraintsTermVariants = S.fromList [
    TermVariantList,
    TermVariantLiteral,
    TermVariantMaybe,
    TermVariantRecord,
    TermVariantUnion], -- Unions are supported only in the form of enums
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantEither, -- Either types are encoded as records with optional left/right fields
    TypeVariantList,
    TypeVariantLiteral,
    TypeVariantMap, -- Maps are encoded as lists of values
    TypeVariantPair, -- Pairs are encoded as records with first/second fields
    TypeVariantSet, -- Sets are encoded as lists
    TypeVariantWrap,
    TypeVariantMaybe,
    TypeVariantRecord,
    TypeVariantUnion,
    TypeVariantVariable],
  languageConstraintsTypes = \typ -> case deannotateType typ of
    -- Union types are supported: enums for unit-type variants, GraphQL unions for non-unit variants
    TypeMaybe et -> case deannotateType et of
      TypeMaybe _ -> False  -- No encoding for optionals within optionals
      _ -> True
    _ -> True}

graphqlReservedWords :: S.Set String
graphqlReservedWords = S.fromList ["true", "false"]
