module Hydra.Ext.Json.Schema.Language where

import Hydra.Core
import Hydra.Coders
import Hydra.Strip
import Hydra.Mantle
import qualified Hydra.Basics as Basics

import qualified Data.Set as S


jsonSchemaLanguage :: Language
jsonSchemaLanguage = Language (LanguageName "hydra/ext/json/schema") $ LanguageConstraints {
  languageConstraintsEliminationVariants = S.empty,
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBoolean, LiteralVariantFloat, LiteralVariantInteger, LiteralVariantString],
  languageConstraintsFloatTypes = S.fromList [FloatTypeBigfloat],
  languageConstraintsFunctionVariants = S.empty,
  languageConstraintsIntegerTypes = S.fromList [IntegerTypeBigint],
  languageConstraintsTermVariants = S.fromList Basics.termVariants,
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantAnnotated,
    TypeVariantList,
    TypeVariantLiteral,
    TypeVariantMap,
    TypeVariantOptional,
    TypeVariantRecord,
    TypeVariantUnion,
    TypeVariantVariable],
  languageConstraintsTypes = \typ -> True }
