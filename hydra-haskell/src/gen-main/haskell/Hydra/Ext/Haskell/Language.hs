-- | Language constraints and reserved words for Haskell

module Hydra.Ext.Haskell.Language where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Meta as Meta
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
      Meta.EliminationVariantProduct,
      Meta.EliminationVariantRecord,
      Meta.EliminationVariantUnion,
      Meta.EliminationVariantWrap])
    literalVariants = (Sets.fromList [
      Meta.LiteralVariantBoolean,
      Meta.LiteralVariantFloat,
      Meta.LiteralVariantInteger,
      Meta.LiteralVariantString])
    floatTypes = (Sets.fromList [
      Core.FloatTypeFloat32,
      Core.FloatTypeFloat64])
    functionVariants = (Sets.fromList [
      Meta.FunctionVariantElimination,
      Meta.FunctionVariantLambda,
      Meta.FunctionVariantPrimitive])
    integerTypes = (Sets.fromList [
      Core.IntegerTypeBigint,
      Core.IntegerTypeInt8,
      Core.IntegerTypeInt16,
      Core.IntegerTypeInt32,
      Core.IntegerTypeInt64])
    termVariants = (Sets.fromList [
      Meta.TermVariantApplication,
      Meta.TermVariantFunction,
      Meta.TermVariantLet,
      Meta.TermVariantList,
      Meta.TermVariantLiteral,
      Meta.TermVariantMap,
      Meta.TermVariantMaybe,
      Meta.TermVariantPair,
      Meta.TermVariantProduct,
      Meta.TermVariantRecord,
      Meta.TermVariantSet,
      Meta.TermVariantUnion,
      Meta.TermVariantUnit,
      Meta.TermVariantVariable,
      Meta.TermVariantWrap])
    typeVariants = (Sets.fromList [
      Meta.TypeVariantAnnotated,
      Meta.TypeVariantApplication,
      Meta.TypeVariantEither,
      Meta.TypeVariantFunction,
      Meta.TypeVariantForall,
      Meta.TypeVariantList,
      Meta.TypeVariantLiteral,
      Meta.TypeVariantMap,
      Meta.TypeVariantMaybe,
      Meta.TypeVariantPair,
      Meta.TypeVariantProduct,
      Meta.TypeVariantRecord,
      Meta.TypeVariantSet,
      Meta.TypeVariantUnion,
      Meta.TypeVariantUnit,
      Meta.TypeVariantVariable,
      Meta.TypeVariantWrap])
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
