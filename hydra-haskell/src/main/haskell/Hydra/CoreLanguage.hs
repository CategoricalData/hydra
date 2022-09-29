module Hydra.CoreLanguage where

import Hydra.Compute
import Hydra.Basics

import qualified Data.Set as S


hydraCoreLanguage :: Language m
hydraCoreLanguage = Language (LanguageName "hydra/core") $ LanguageConstraints {
  languageConstraintsEliminationVariants = S.fromList eliminationVariants,
  languageConstraintsLiteralVariants = S.fromList literalVariants,
  languageConstraintsFloatTypes = S.fromList floatTypes,
  languageConstraintsFunctionVariants = S.fromList functionVariants,
  languageConstraintsIntegerTypes = S.fromList integerTypes,
  languageConstraintsTermVariants = S.fromList termVariants,
  languageConstraintsTypeVariants = S.fromList typeVariants,
  languageConstraintsTypes = const True }
