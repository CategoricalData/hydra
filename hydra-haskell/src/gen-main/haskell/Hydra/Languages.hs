-- | Language constraints for Hydra Core

module Hydra.Languages where

import qualified Hydra.Coders as Coders
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Language constraints for Hydra Core, i.e. no constraints.
hydraLanguage :: Coders.Language
hydraLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra.core"),
  Coders.languageConstraints = Coders.LanguageConstraints {
    Coders.languageConstraintsEliminationVariants = (Sets.fromList Variants.eliminationVariants),
    Coders.languageConstraintsLiteralVariants = (Sets.fromList Variants.literalVariants),
    Coders.languageConstraintsFloatTypes = (Sets.fromList Variants.floatTypes),
    Coders.languageConstraintsFunctionVariants = (Sets.fromList Variants.functionVariants),
    Coders.languageConstraintsIntegerTypes = (Sets.fromList Variants.integerTypes),
    Coders.languageConstraintsTermVariants = (Sets.fromList Variants.termVariants),
    Coders.languageConstraintsTypeVariants = (Sets.fromList Variants.typeVariants),
    Coders.languageConstraintsTypes = (\_ -> True)}}
