-- | Language constraints and reserved words for Python 3

module Hydra.Ext.Python.Language where

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

-- | Language constraints for Python 3
pythonLanguage :: Coders.Language
pythonLanguage = Coders.Language {
  Coders.languageName = (Coders.LanguageName "hydra.ext.python"),
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
      Mantle.LiteralVariantBinary,
      Mantle.LiteralVariantBoolean,
      Mantle.LiteralVariantFloat,
      Mantle.LiteralVariantInteger,
      Mantle.LiteralVariantString])
    floatTypes = (Sets.fromList [
      Core.FloatTypeFloat64])
    functionVariants = (Sets.fromList [
      Mantle.FunctionVariantElimination,
      Mantle.FunctionVariantLambda,
      Mantle.FunctionVariantPrimitive])
    integerTypes = (Sets.fromList [
      Core.IntegerTypeBigint])
    termVariants = (Sets.fromList [
      Mantle.TermVariantApplication,
      Mantle.TermVariantFunction,
      Mantle.TermVariantLet,
      Mantle.TermVariantList,
      Mantle.TermVariantLiteral,
      Mantle.TermVariantMap,
      Mantle.TermVariantOptional,
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
      Mantle.TypeVariantFunction,
      Mantle.TypeVariantForall,
      Mantle.TypeVariantList,
      Mantle.TypeVariantLiteral,
      Mantle.TypeVariantMap,
      Mantle.TypeVariantOptional,
      Mantle.TypeVariantProduct,
      Mantle.TypeVariantRecord,
      Mantle.TypeVariantSet,
      Mantle.TypeVariantUnion,
      Mantle.TypeVariantUnit,
      Mantle.TypeVariantVariable,
      Mantle.TypeVariantWrap])
    typePredicate = (\_ -> True)

-- | A set of reserved words in Python
pythonReservedWords :: (S.Set String)
pythonReservedWords = (Sets.fromList (Lists.concat2 pythonKeywords hydraPythonKeywords)) 
  where 
    pythonKeywords = [
      "False",
      "None",
      "True",
      "and",
      "as",
      "assert",
      "async",
      "await",
      "break",
      "class",
      "continue",
      "def",
      "del",
      "elif",
      "else",
      "except",
      "finally",
      "for",
      "from",
      "global",
      "if",
      "import",
      "in",
      "is",
      "lambda",
      "nonlocal",
      "not",
      "or",
      "pass",
      "raise",
      "return",
      "try",
      "while",
      "with",
      "yield"]
    hydraPythonKeywords = [
      "Node",
      "FrozenDict"]
