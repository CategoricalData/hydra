-- Note: this is an automatically generated file. Do not edit.

-- | Language constraints for Hydra Core

module Hydra.Languages where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Reflect as Reflect
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
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
    eliminationVariants = (Sets.fromList Reflect.eliminationVariants)
    literalVariants = (Sets.fromList Reflect.literalVariants)
    floatTypes = (Sets.fromList Reflect.floatTypes)
    functionVariants = (Sets.fromList Reflect.functionVariants)
    integerTypes = (Sets.fromList Reflect.integerTypes)
    termVariants = (Sets.fromList Reflect.termVariants)
    typeVariants = (Sets.fromList Reflect.typeVariants)
    types = (\t -> (\x -> case x of
      _ -> True) t)
