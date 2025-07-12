-- | Language constraints for JSON

module Hydra.Ext.Org.Json.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Language constraints for JSON
jsonLanguage :: Coders.Language
jsonLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra.ext.json"),
  Coders.languageConstraints = Coders.LanguageConstraints {
    Coders.languageConstraintsEliminationVariants = eliminationVariants,
    Coders.languageConstraintsLiteralVariants = literalVariants,
    Coders.languageConstraintsFloatTypes = floatTypes,
    Coders.languageConstraintsFunctionVariants = functionVariants,
    Coders.languageConstraintsIntegerTypes = integerTypes,
    Coders.languageConstraintsTermVariants = termVariants,
    Coders.languageConstraintsTypeVariants = typeVariants,
    Coders.languageConstraintsTypes = typePredicate}} 
  where 
    eliminationVariants = Sets.empty
    literalVariants = (Sets.fromList [
      Mantle.LiteralVariantBoolean,
      Mantle.LiteralVariantFloat,
      Mantle.LiteralVariantInteger,
      Mantle.LiteralVariantString])
    floatTypes = (Sets.fromList [
      Core.FloatTypeBigfloat])
    functionVariants = Sets.empty
    integerTypes = (Sets.fromList [
      Core.IntegerTypeBigint])
    termVariants = (Sets.fromList [
      Mantle.TermVariantList,
      Mantle.TermVariantLiteral,
      Mantle.TermVariantMap,
      Mantle.TermVariantOptional,
      Mantle.TermVariantRecord])
    typeVariants = (Sets.fromList [
      Mantle.TypeVariantList,
      Mantle.TypeVariantLiteral,
      Mantle.TypeVariantMap,
      Mantle.TypeVariantOptional,
      Mantle.TypeVariantRecord])
    typePredicate = (\typ -> (\x -> case x of
      Core.TypeOptional v1 -> ((\x -> case x of
        Core.TypeOptional _ -> False
        _ -> True) v1)
      _ -> True) (Rewriting.deannotateType typ))
