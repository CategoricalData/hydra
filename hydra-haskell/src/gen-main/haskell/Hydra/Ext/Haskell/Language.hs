-- | Language constraints and reserved words for Haskell

module Hydra.Ext.Haskell.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Mantle as Mantle
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
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
      Mantle.EliminationVariantProduct,
      Mantle.EliminationVariantRecord,
      Mantle.EliminationVariantUnion,
      Mantle.EliminationVariantWrap])
    literalVariants = (Sets.fromList [
      Mantle.LiteralVariantBoolean,
      Mantle.LiteralVariantFloat,
      Mantle.LiteralVariantInteger,
      Mantle.LiteralVariantString])
    floatTypes = (Sets.fromList [
      Core.FloatTypeFloat32,
      Core.FloatTypeFloat64])
    functionVariants = (Sets.fromList [
      Mantle.FunctionVariantElimination,
      Mantle.FunctionVariantLambda,
      Mantle.FunctionVariantPrimitive])
    integerTypes = (Sets.fromList [
      Core.IntegerTypeBigint,
      Core.IntegerTypeInt8,
      Core.IntegerTypeInt16,
      Core.IntegerTypeInt32,
      Core.IntegerTypeInt64])
    termVariants = (Sets.fromList [
      Mantle.TermVariantApplication,
      Mantle.TermVariantFunction,
      Mantle.TermVariantLet,
      Mantle.TermVariantList,
      Mantle.TermVariantLiteral,
      Mantle.TermVariantMap,
      Mantle.TermVariantMaybe,
      Mantle.TermVariantProduct,
      Mantle.TermVariantRecord,
      Mantle.TermVariantSet,
      Mantle.TermVariantUnion,
      Mantle.TermVariantUnit,
      Mantle.TermVariantVariable,
      Mantle.TermVariantWrap])
    typeVariants = (Sets.fromList [
      Mantle.TypeVariantAnnotated,
      Mantle.TypeVariantApplication,
      Mantle.TypeVariantEither,
      Mantle.TypeVariantFunction,
      Mantle.TypeVariantForall,
      Mantle.TypeVariantList,
      Mantle.TypeVariantLiteral,
      Mantle.TypeVariantMap,
      Mantle.TypeVariantMaybe,
      Mantle.TypeVariantProduct,
      Mantle.TypeVariantRecord,
      Mantle.TypeVariantSet,
      Mantle.TypeVariantUnion,
      Mantle.TypeVariantUnit,
      Mantle.TypeVariantVariable,
      Mantle.TypeVariantWrap])
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
