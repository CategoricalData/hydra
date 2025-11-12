-- | Language constraints for JSON

module Hydra.Ext.Org.Json.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Meta as Meta
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Language constraints for JSON
jsonLanguage :: Coders.Language
jsonLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra.ext.org.json"),
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
      Meta.LiteralVariantBoolean,
      Meta.LiteralVariantFloat,
      Meta.LiteralVariantInteger,
      Meta.LiteralVariantString])
    floatTypes = (Sets.fromList [
      Core.FloatTypeBigfloat])
    functionVariants = Sets.empty
    integerTypes = (Sets.fromList [
      Core.IntegerTypeBigint])
    termVariants = (Sets.fromList [
      Meta.TermVariantList,
      Meta.TermVariantLiteral,
      Meta.TermVariantMap,
      Meta.TermVariantMaybe,
      Meta.TermVariantRecord])
    typeVariants = (Sets.fromList [
      Meta.TypeVariantList,
      Meta.TypeVariantLiteral,
      Meta.TypeVariantMap,
      Meta.TypeVariantMaybe,
      Meta.TypeVariantRecord])
    typePredicate = (\typ -> (\x -> case x of
      Core.TypeMaybe v1 -> ((\x -> case x of
        Core.TypeMaybe _ -> False
        _ -> True) v1)
      _ -> True) (Rewriting.deannotateType typ))
