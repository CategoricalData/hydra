-- Note: this is an automatically generated file. Do not edit.

-- | Language constraints and reserved words for GraphQL

module Hydra.Ext.Graphql.Language where

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

-- | Language constraints for GraphQL
graphqlLanguage :: Coders.Language
graphqlLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.ext.graphql"),
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
      Core.FloatTypeFloat64]
    functionVariants = Sets.empty
    integerTypes = Sets.fromList [
      Core.IntegerTypeInt32]
    termVariants =
        Sets.fromList [
          Variants.TermVariantList,
          Variants.TermVariantLiteral,
          Variants.TermVariantMaybe,
          Variants.TermVariantRecord,
          Variants.TermVariantUnion]
    typeVariants =
        Sets.fromList [
          Variants.TypeVariantApplication,
          Variants.TypeVariantEither,
          Variants.TypeVariantForall,
          Variants.TypeVariantFunction,
          Variants.TypeVariantList,
          Variants.TypeVariantLiteral,
          Variants.TypeVariantMap,
          Variants.TypeVariantPair,
          Variants.TypeVariantSet,
          Variants.TypeVariantUnit,
          Variants.TypeVariantWrap,
          Variants.TypeVariantMaybe,
          Variants.TypeVariantRecord,
          Variants.TypeVariantUnion,
          Variants.TypeVariantVariable]
    typePredicate =
        \typ -> case (Rewriting.deannotateType typ) of
          Core.TypeMaybe v0 -> case (Rewriting.deannotateType v0) of
            Core.TypeMaybe _ -> False
            _ -> True
          _ -> True

-- | A set of reserved words in GraphQL
graphqlReservedWords :: S.Set String
graphqlReservedWords =
    Sets.fromList [
      "true",
      "false"]
