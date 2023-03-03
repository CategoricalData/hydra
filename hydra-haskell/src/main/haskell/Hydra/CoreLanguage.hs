-- | Core language constraints of Hydra. These constraints are trivial; all types and all terms are supported.

module Hydra.CoreLanguage where

import Hydra.Compute
import Hydra.Basics
import Hydra.Coders

import qualified Data.Set as S


hydraCoreLanguage :: Language a
hydraCoreLanguage = Language (LanguageName "hydra/core") $ LanguageConstraints {
  languageConstraintsEliminationVariants = S.fromList eliminationVariants,
  languageConstraintsLiteralVariants = S.fromList literalVariants,
  languageConstraintsFloatTypes = S.fromList floatTypes,
  languageConstraintsFunctionVariants = S.fromList functionVariants,
  languageConstraintsIntegerTypes = S.fromList integerTypes,
  languageConstraintsTermVariants = S.fromList termVariants,
  languageConstraintsTypeVariants = S.fromList typeVariants,
  languageConstraintsTypes = const True }
