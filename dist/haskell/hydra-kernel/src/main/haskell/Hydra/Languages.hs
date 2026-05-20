-- Note: this is an automatically generated file. Do not edit.
-- | Language constraints for Hydra Core

module Hydra.Languages where
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Reflect as Reflect
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Language constraints for Hydra Core, i.e. no constraints.
hydraLanguage :: Coders.Language
hydraLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.core"),
      Coders.languageConstraints = Coders.LanguageConstraints {
        Coders.languageConstraintsLiteralVariants = literalVariants,
        Coders.languageConstraintsFloatTypes = floatTypes,
        Coders.languageConstraintsIntegerTypes = integerTypes,
        Coders.languageConstraintsTermVariants = termVariants,
        Coders.languageConstraintsTypeVariants = typeVariants,
        Coders.languageConstraintsTypes = types}}
  where
    literalVariants = Sets.fromList Reflect.literalVariants
    floatTypes = Sets.fromList Reflect.floatTypes
    integerTypes = Sets.fromList Reflect.integerTypes
    termVariants = Sets.fromList Reflect.termVariants
    typeVariants = Sets.fromList Reflect.typeVariants
    types =
        \t -> case t of
          _ -> True
