-- | Language constraints for JSON Schema

module Hydra.Ext.Org.Json.Schema.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Language constraints for JSON Schema
jsonSchemaLanguage :: Coders.Language
jsonSchemaLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra.ext.org.json.schema"),
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
    eliminationVariants = (Sets.fromList [])
    literalVariants = (Sets.fromList [
      Mantle.LiteralVariantBoolean,
      Mantle.LiteralVariantFloat,
      Mantle.LiteralVariantInteger,
      Mantle.LiteralVariantString])
    floatTypes = (Sets.fromList [
      Core.FloatTypeBigfloat])
    functionVariants = (Sets.fromList [])
    integerTypes = (Sets.fromList [
      Core.IntegerTypeBigint])
    termVariants = (Sets.fromList Variants.termVariants)
    typeVariants = (Sets.fromList [
      Mantle.TypeVariantAnnotated,
      Mantle.TypeVariantList,
      Mantle.TypeVariantLiteral,
      Mantle.TypeVariantMap,
      Mantle.TypeVariantMaybe,
      Mantle.TypeVariantRecord,
      Mantle.TypeVariantUnion,
      Mantle.TypeVariantVariable])
    typePredicate = (\_ -> True)
