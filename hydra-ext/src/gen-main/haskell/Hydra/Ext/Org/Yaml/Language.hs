-- Note: this is an automatically generated file. Do not edit.

-- | Language constraints for YAML

module Hydra.Ext.Org.Yaml.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Language constraints for YAML
yamlLanguage :: Coders.Language
yamlLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.ext.yaml"),
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
    literalVariants =
        Sets.fromList [
          Variants.LiteralVariantBoolean,
          Variants.LiteralVariantFloat,
          Variants.LiteralVariantInteger,
          Variants.LiteralVariantString]
    floatTypes = Sets.fromList [
      Core.FloatTypeBigfloat]
    functionVariants = Sets.empty
    integerTypes = Sets.fromList [
      Core.IntegerTypeBigint]
    termVariants =
        Sets.fromList [
          Variants.TermVariantLiteral,
          Variants.TermVariantList,
          Variants.TermVariantMap,
          Variants.TermVariantMaybe,
          Variants.TermVariantRecord,
          Variants.TermVariantUnit]
    typeVariants =
        Sets.fromList [
          Variants.TypeVariantLiteral,
          Variants.TypeVariantList,
          Variants.TypeVariantMap,
          Variants.TypeVariantMaybe,
          Variants.TypeVariantRecord,
          Variants.TypeVariantUnit]
    typePredicate =
        \typ -> case (Rewriting.deannotateType typ) of
          Core.TypeMaybe v0 -> case v0 of
            Core.TypeMaybe _ -> False
            _ -> True
          _ -> True
