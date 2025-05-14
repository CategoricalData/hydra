module Hydra.Dsl.Coders where

import Hydra.Kernel
import Hydra.Dsl.Phantoms as Phantoms
import qualified Hydra.Dsl.Core as Core
import Hydra.Dsl.Lib.Sets as Sets
import Hydra.Dsl.Mantle as Mantle
import qualified Hydra.Dsl.TTypes as T

language
  :: String
  -> [EliminationVariant]
  -> [LiteralVariant]
  -> [FloatType]
  -> [FunctionVariant]
  -> [IntegerType]
  -> [TermVariant]
  -> [TypeVariant]
  -> TTerm (Type -> Bool)
  -> TTerm Language
language name eliminationVariants literalVariants floatTypes functionVariants integerTypes termVariants typeVariants typePredicate = Phantoms.record _Language [
  _Language_name>>: wrap _LanguageName $ string name,
  _Language_constraints>>: Phantoms.record _LanguageConstraints [
    _LanguageConstraints_eliminationVariants>>: Sets.fromList $ list (Mantle.eliminationVariant <$> eliminationVariants),
    _LanguageConstraints_literalVariants>>: Sets.fromList $ list (Mantle.literalVariant <$> literalVariants),
    _LanguageConstraints_floatTypes>>: Sets.fromList $ list (T.floatType <$> floatTypes),
    _LanguageConstraints_functionVariants>>: Sets.fromList $ list (Mantle.functionVariant <$> functionVariants),
    _LanguageConstraints_integerTypes>>: Sets.fromList $ list (T.integerType <$> integerTypes),
    _LanguageConstraints_termVariants>>: Sets.fromList $ list (Mantle.termVariant <$> termVariants),
    _LanguageConstraints_typeVariants>>: Sets.fromList $ list (Mantle.typeVariant <$> typeVariants),
    _LanguageConstraints_types>>: typePredicate]]
