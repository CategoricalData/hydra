-- | Language constraints and reserved words for Python 3

module Hydra.Ext.Python.Language where

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
      Meta.EliminationVariantProduct,
      Meta.EliminationVariantRecord,
      Meta.EliminationVariantUnion,
      Meta.EliminationVariantWrap])
    literalVariants = (Sets.fromList [
      Meta.LiteralVariantBinary,
      Meta.LiteralVariantBoolean,
      Meta.LiteralVariantFloat,
      Meta.LiteralVariantInteger,
      Meta.LiteralVariantString])
    floatTypes = (Sets.fromList [
      Core.FloatTypeBigfloat,
      Core.FloatTypeFloat64])
    functionVariants = (Sets.fromList [
      Meta.FunctionVariantElimination,
      Meta.FunctionVariantLambda,
      Meta.FunctionVariantPrimitive])
    integerTypes = (Sets.fromList [
      Core.IntegerTypeBigint])
    termVariants = (Sets.fromList [
      Meta.TermVariantAnnotated,
      Meta.TermVariantApplication,
      Meta.TermVariantEither,
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
      Meta.TermVariantTypeApplication,
      Meta.TermVariantTypeLambda,
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

-- | A set of reserved words in Python
pythonReservedWords :: (S.Set String)
pythonReservedWords = (Sets.fromList (Lists.concat [
  pythonKeywords,
  pythonBuiltInFunctions,
  hydraPythonKeywords])) 
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
    pythonBuiltInFunctions = [
      "range"]
    hydraPythonKeywords = [
      "Node",
      "FrozenDict"]
