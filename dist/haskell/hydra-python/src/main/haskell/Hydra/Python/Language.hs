-- Note: this is an automatically generated file. Do not edit.
-- | Language constraints and reserved words for Python 3

module Hydra.Python.Language where
import qualified Hydra.Ast as Ast
import qualified Hydra.Classes as Classes
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
-- | Language constraints for Python 3
pythonLanguage :: Coders.Language
pythonLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.python"),
      Coders.languageConstraints = Coders.LanguageConstraints {
        Coders.languageConstraintsLiteralVariants = literalVariants,
        Coders.languageConstraintsFloatTypes = floatTypes,
        Coders.languageConstraintsIntegerTypes = integerTypes,
        Coders.languageConstraintsTermVariants = termVariants,
        Coders.languageConstraintsTypeVariants = typeVariants,
        Coders.languageConstraintsTypes = typePredicate},
      Coders.languageSupportedFeatures = (Sets.fromList [
        Coders.LanguageFeatureNestedPolymorphicLetBindings]),
      Coders.languageCaseConventions = Coders.CaseConventions {
        Coders.caseConventionsConstant = Util.CaseConventionUpperSnake,
        Coders.caseConventionsDirectory = Util.CaseConventionLowerSnake,
        Coders.caseConventionsEnumValue = Util.CaseConventionUpperSnake,
        Coders.caseConventionsField = Util.CaseConventionLowerSnake,
        Coders.caseConventionsFile = Util.CaseConventionLowerSnake,
        Coders.caseConventionsModule = Util.CaseConventionLowerSnake,
        Coders.caseConventionsTerm = Util.CaseConventionLowerSnake,
        Coders.caseConventionsTermVariable = Util.CaseConventionLowerSnake,
        Coders.caseConventionsType = Util.CaseConventionPascal,
        Coders.caseConventionsTypeVariable = Util.CaseConventionPascal},
      Coders.languageDefaultFileExtension = (Util.FileExtension "py")}
  where
    literalVariants =
        Sets.fromList [
          Variants.LiteralVariantBinary,
          Variants.LiteralVariantBoolean,
          Variants.LiteralVariantDecimal,
          Variants.LiteralVariantFloat,
          Variants.LiteralVariantInteger,
          Variants.LiteralVariantString]
    floatTypes = Sets.fromList [
      Core.FloatTypeFloat64]
    integerTypes = Sets.fromList [
      Core.IntegerTypeBigint]
    termVariants =
        Sets.fromList [
          Variants.TermVariantAnnotated,
          Variants.TermVariantApplication,
          Variants.TermVariantEither,
          Variants.TermVariantCases,
          Variants.TermVariantLambda,
          Variants.TermVariantProject,
          Variants.TermVariantUnwrap,
          Variants.TermVariantLet,
          Variants.TermVariantList,
          Variants.TermVariantLiteral,
          Variants.TermVariantMap,
          Variants.TermVariantOptional,
          Variants.TermVariantPair,
          Variants.TermVariantRecord,
          Variants.TermVariantSet,
          Variants.TermVariantTypeApplication,
          Variants.TermVariantTypeLambda,
          Variants.TermVariantInject,
          Variants.TermVariantUnit,
          Variants.TermVariantVariable,
          Variants.TermVariantWrap]
    typeVariants =
        Sets.fromList [
          Variants.TypeVariantAnnotated,
          Variants.TypeVariantApplication,
          Variants.TypeVariantEither,
          Variants.TypeVariantFunction,
          Variants.TypeVariantForall,
          Variants.TypeVariantList,
          Variants.TypeVariantLiteral,
          Variants.TypeVariantMap,
          Variants.TypeVariantOptional,
          Variants.TypeVariantPair,
          Variants.TypeVariantRecord,
          Variants.TypeVariantSet,
          Variants.TypeVariantUnion,
          Variants.TypeVariantUnit,
          Variants.TypeVariantVariable,
          Variants.TypeVariantVoid,
          Variants.TypeVariantWrap]
    typePredicate = \_ -> True
-- | A set of reserved words in Python
pythonReservedWords :: S.Set String
pythonReservedWords =
    Sets.fromList (Lists.concat [
      pythonKeywords,
      pythonBuiltInFunctions,
      hydraPythonKeywords])
  where
    pythonKeywords =
        [
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
    hydraPythonKeywords =
        [
          "Node",
          "FrozenDict"]
