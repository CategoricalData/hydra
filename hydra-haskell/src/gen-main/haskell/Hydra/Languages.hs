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
    Coders.languageConstraintsEliminationVariants = eliminationVariants,
    Coders.languageConstraintsLiteralVariants = literalVariants,
    Coders.languageConstraintsFloatTypes = floatTypes,
    Coders.languageConstraintsFunctionVariants = functionVariants,
    Coders.languageConstraintsIntegerTypes = integerTypes,
    Coders.languageConstraintsTermVariants = termVariants,
    Coders.languageConstraintsTypeVariants = typeVariants,
    Coders.languageConstraintsTypes = types}} 
  where 
    eliminationVariants = (Sets.fromList Variants.eliminationVariants)
    literalVariants = (Sets.fromList Variants.literalVariants)
    floatTypes = (Sets.fromList Variants.floatTypes)
    functionVariants = (Sets.fromList Variants.functionVariants)
    integerTypes = (Sets.fromList Variants.integerTypes)
    termVariants = (Sets.fromList Variants.termVariants)
    typeVariants = (Sets.fromList Variants.typeVariants)
    types = (\_ -> True)
