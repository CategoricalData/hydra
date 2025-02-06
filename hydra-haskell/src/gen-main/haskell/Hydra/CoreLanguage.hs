-- | Language constraints for Hydra Core

module Hydra.CoreLanguage where

import qualified Hydra.Coders as Coders
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Variants as Variants
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | Language constraints for Java
hydraCoreLanguage :: Coders.Language
hydraCoreLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra/core"),
  Coders.languageConstraints = Coders.LanguageConstraints {
    Coders.languageConstraintsEliminationVariants = (Sets.fromList Variants.eliminationVariants),
    Coders.languageConstraintsLiteralVariants = (Sets.fromList Variants.literalVariants),
    Coders.languageConstraintsFloatTypes = (Sets.fromList Variants.floatTypes),
    Coders.languageConstraintsFunctionVariants = (Sets.fromList Variants.functionVariants),
    Coders.languageConstraintsIntegerTypes = (Sets.fromList Variants.integerTypes),
    Coders.languageConstraintsTermVariants = (Sets.fromList Variants.termVariants),
    Coders.languageConstraintsTypeVariants = (Sets.fromList Variants.typeVariants),
    Coders.languageConstraintsTypes = (\_ -> True)}}