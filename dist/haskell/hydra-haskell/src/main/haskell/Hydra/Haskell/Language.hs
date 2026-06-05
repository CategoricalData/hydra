-- Note: this is an automatically generated file. Do not edit.
-- | Language constraints and reserved words for Haskell

module Hydra.Haskell.Language where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
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
-- | Language constraints for Haskell
haskellLanguage :: Coders.Language
haskellLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.haskell"),
      Coders.languageConstraints = Coders.LanguageConstraints {
        Coders.languageConstraintsLiteralVariants = literalVariants,
        Coders.languageConstraintsFloatTypes = floatTypes,
        Coders.languageConstraintsIntegerTypes = integerTypes,
        Coders.languageConstraintsTermVariants = termVariants,
        Coders.languageConstraintsTypeVariants = typeVariants,
        Coders.languageConstraintsTypes = typePredicate},
      Coders.languageSupportedFeatures = (Sets.fromList [
        Coders.LanguageFeaturePartialApplication,
        Coders.LanguageFeatureNestedCaseStatements,
        Coders.LanguageFeatureNestedPolymorphicLetBindings]),
      Coders.languageCaseConventions = Coders.CaseConventions {
        Coders.caseConventionsConstant = Util.CaseConventionUpperSnake,
        Coders.caseConventionsDirectory = Util.CaseConventionPascal,
        Coders.caseConventionsEnumValue = Util.CaseConventionPascal,
        Coders.caseConventionsField = Util.CaseConventionCamel,
        Coders.caseConventionsFile = Util.CaseConventionPascal,
        Coders.caseConventionsModule = Util.CaseConventionPascal,
        Coders.caseConventionsTerm = Util.CaseConventionCamel,
        Coders.caseConventionsTermVariable = Util.CaseConventionCamel,
        Coders.caseConventionsType = Util.CaseConventionPascal,
        Coders.caseConventionsTypeVariable = Util.CaseConventionCamel},
      Coders.languageDefaultFileExtension = (Util.FileExtension "hs")}
  where
    literalVariants =
        Sets.fromList [
          Variants.LiteralVariantBinary,
          Variants.LiteralVariantBoolean,
          Variants.LiteralVariantDecimal,
          Variants.LiteralVariantFloat,
          Variants.LiteralVariantInteger,
          Variants.LiteralVariantString]
    floatTypes =
        Sets.fromList [
          Core.FloatTypeFloat32,
          Core.FloatTypeFloat64]
    integerTypes =
        Sets.fromList [
          Core.IntegerTypeBigint,
          Core.IntegerTypeInt8,
          Core.IntegerTypeInt16,
          Core.IntegerTypeInt32,
          Core.IntegerTypeInt64]
    termVariants =
        Sets.fromList [
          Variants.TermVariantAnnotated,
          Variants.TermVariantApplication,
          Variants.TermVariantCases,
          Variants.TermVariantEither,
          Variants.TermVariantLambda,
          Variants.TermVariantLet,
          Variants.TermVariantList,
          Variants.TermVariantLiteral,
          Variants.TermVariantMap,
          Variants.TermVariantMaybe,
          Variants.TermVariantPair,
          Variants.TermVariantProject,
          Variants.TermVariantRecord,
          Variants.TermVariantSet,
          Variants.TermVariantTypeApplication,
          Variants.TermVariantTypeLambda,
          Variants.TermVariantInject,
          Variants.TermVariantUnit,
          Variants.TermVariantUnwrap,
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
          Variants.TypeVariantMaybe,
          Variants.TypeVariantPair,
          Variants.TypeVariantRecord,
          Variants.TypeVariantSet,
          Variants.TypeVariantUnion,
          Variants.TypeVariantUnit,
          Variants.TypeVariantVariable,
          Variants.TypeVariantVoid,
          Variants.TypeVariantWrap]
    typePredicate = \_ -> True
-- | Created on 2025-02-28 using GHCi 9.6.6
-- |
-- | You can reproduce these lists of symbols by issuing the command `:browse Prelude` in GHCi, pasting the results into
-- | /tmp/browse_Prelude.txt, and then running the Bash command provided with each list.
-- |
-- | See also https://www.haskell.org/onlinereport/standard-prelude.html
reservedWords :: S.Set String
reservedWords = Sets.fromList (Lists.concat2 keywordSymbols reservedSymbols)
  where
    keywordSymbols =
        [
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
    reservedSymbols =
        [
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
