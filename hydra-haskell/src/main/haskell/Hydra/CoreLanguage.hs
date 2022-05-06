module Hydra.CoreLanguage where

import Hydra.Adapter
import Hydra.Basics

import qualified Data.Set as S


hydraCoreLanguage :: Language m
hydraCoreLanguage = Language "hydra/core" $ Language_Constraints {
  languageConstraintsLiteralVariants = S.fromList literalVariants,
  languageConstraintsFloatTypes = S.fromList floatTypes,
  languageConstraintsFunctionVariants = S.fromList functionVariants,
  languageConstraintsIntegerTypes = S.fromList integerTypes,
  languageConstraintsDataVariants = S.fromList termVariants,
  languageConstraintsTypeVariants = S.fromList typeVariants,
  languageConstraintsTypes = const True }
