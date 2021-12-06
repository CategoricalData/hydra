module Hydra.Prototyping.CoreLanguage where

import Hydra.Adapter
import Hydra.Basics

import qualified Data.Set as S


hydraCoreLanguage :: Language
hydraCoreLanguage = Language "hydra/core" $ Language_Constraints {
  languageConstraintsLiteralVariants = S.fromList literalVariants,
  languageConstraintsFloatTypes = S.fromList floatTypes,
  languageConstraintsFunctionVariants = S.fromList functionVariants,
  languageConstraintsIntegerTypes = S.fromList integerTypes,
  languageConstraintsTermVariants = S.fromList termVariants,
  languageConstraintsTypeVariants = S.fromList typeVariants,
  languageConstraintsTypes = const True }
