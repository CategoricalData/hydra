-- Note: this is an automatically generated file. Do not edit.

-- | Language constraints and reserved words for Haskell

module Hydra.Ext.Haskell.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Language constraints for Haskell
haskellLanguage :: Coders.Language
haskellLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra.ext.haskell"),
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
    eliminationVariants = (Sets.fromList [
      Variants.EliminationVariantRecord,
      Variants.EliminationVariantUnion,
      Variants.EliminationVariantWrap])
    literalVariants = (Sets.fromList [
      Variants.LiteralVariantBinary,
      Variants.LiteralVariantBoolean,
      Variants.LiteralVariantFloat,
      Variants.LiteralVariantInteger,
      Variants.LiteralVariantString])
    floatTypes = (Sets.fromList [
      Core.FloatTypeFloat32,
      Core.FloatTypeFloat64])
    functionVariants = (Sets.fromList [
      Variants.FunctionVariantElimination,
      Variants.FunctionVariantLambda,
      Variants.FunctionVariantPrimitive])
    integerTypes = (Sets.fromList [
      Core.IntegerTypeBigint,
      Core.IntegerTypeInt8,
      Core.IntegerTypeInt16,
      Core.IntegerTypeInt32,
      Core.IntegerTypeInt64])
    termVariants = (Sets.fromList [
      Variants.TermVariantAnnotated,
      Variants.TermVariantApplication,
      Variants.TermVariantEither,
      Variants.TermVariantFunction,
      Variants.TermVariantLet,
      Variants.TermVariantList,
      Variants.TermVariantLiteral,
      Variants.TermVariantMap,
      Variants.TermVariantMaybe,
      Variants.TermVariantPair,
      Variants.TermVariantRecord,
      Variants.TermVariantSet,
      Variants.TermVariantUnion,
      Variants.TermVariantUnit,
      Variants.TermVariantVariable,
      Variants.TermVariantWrap])
    typeVariants = (Sets.fromList [
      Variants.TypeVariantAnnotated,
      Variants.TypeVariantApplication,
      Variants.TypeVariantEither,
      Variants.TypeVariantFunction,
      Variants.TypeVariantForall,
      Variants.TypeVariantList,
      Variants.TypeVariantLiteral,
      Variants.TypeVariantMap,
      Variants.TypeVariantMaybe,
      Variants.TypeVariantPair,
      Variants.TypeVariantRecord,
      Variants.TypeVariantSet,
      Variants.TypeVariantUnion,
      Variants.TypeVariantUnit,
      Variants.TypeVariantVariable,
      Variants.TypeVariantWrap])
    typePredicate = (\_ -> True)

-- | Created on 2025-02-28 using GHCi 9.6.6
-- | 
-- | You can reproduce these lists of symbols by issuing the command `:browse Prelude` in GHCi, pasting the results into
-- | /tmp/browse_Prelude.txt, and then running the Bash command provided with each list.
-- | 
-- | See also https://www.haskell.org/onlinereport/standard-prelude.html
reservedWords :: (S.Set String)
reservedWords = (Sets.fromList (Lists.concat2 keywordSymbols reservedSymbols)) 
  where 
    keywordSymbols = [
      "case",
      "class",
      "data",
      "default",
      "deriving",
      "do",
      "else",
      "forall",
      "foreign",
      "if",
      "import",
      "in",
      "infix",
      "infixl",
      "infixr",
      "instance",
      "let",
      "module",
      "newtype",
      "of",
      "then",
      "type",
      "where"]
    reservedSymbols = [
      "Bool",
      "Double",
      "False",
      "Float",
      "Int",
      "Integer",
      "Just",
      "Maybe",
      "Nothing",
      "Ord",
      "Show",
      "String",
      "True"]
