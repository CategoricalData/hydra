-- Note: this is an automatically generated file. Do not edit.
-- | Language constraints and reserved words for Scala

module Hydra.Scala.Language where
import qualified Hydra.Ast as Ast
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
-- | Language constraints for Scala
scalaLanguage :: Coders.Language
scalaLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.scala"),
      Coders.languageConstraints = Coders.LanguageConstraints {
        Coders.languageConstraintsLiteralVariants = literalVariants,
        Coders.languageConstraintsFloatTypes = floatTypes,
        Coders.languageConstraintsIntegerTypes = integerTypes,
        Coders.languageConstraintsTermVariants = termVariants,
        Coders.languageConstraintsTypeVariants = typeVariants,
        Coders.languageConstraintsTypes = typePredicate},
      Coders.languageSupportedFeatures = (Sets.fromList [
        Coders.LanguageFeatureNestedCaseStatements,
        Coders.LanguageFeatureNestedPolymorphicLetBindings]),
      Coders.languageCaseConventions = Coders.CaseConventions {
        Coders.caseConventionsConstant = Util.CaseConventionUpperSnake,
        Coders.caseConventionsDirectory = Util.CaseConventionCamel,
        Coders.caseConventionsEnumValue = Util.CaseConventionPascal,
        Coders.caseConventionsField = Util.CaseConventionCamel,
        Coders.caseConventionsFile = Util.CaseConventionCamel,
        Coders.caseConventionsModule = Util.CaseConventionCamel,
        Coders.caseConventionsTerm = Util.CaseConventionCamel,
        Coders.caseConventionsTermVariable = Util.CaseConventionCamel,
        Coders.caseConventionsType = Util.CaseConventionPascal,
        Coders.caseConventionsTypeVariable = Util.CaseConventionPascal},
      Coders.languageDefaultFileExtension = (Util.FileExtension "scala")}
  where
    literalVariants =
        Sets.fromList [
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
          Core.IntegerTypeInt64,
          Core.IntegerTypeUint8,
          Core.IntegerTypeUint16,
          Core.IntegerTypeUint32,
          Core.IntegerTypeUint64]
    termVariants =
        Sets.fromList [
          Variants.TermVariantApplication,
          Variants.TermVariantEither,
          Variants.TermVariantCases,
          Variants.TermVariantLambda,
          Variants.TermVariantProject,
          Variants.TermVariantUnwrap,
          Variants.TermVariantTypeApplication,
          Variants.TermVariantTypeLambda,
          Variants.TermVariantLet,
          Variants.TermVariantList,
          Variants.TermVariantLiteral,
          Variants.TermVariantMap,
          Variants.TermVariantMaybe,
          Variants.TermVariantPair,
          Variants.TermVariantRecord,
          Variants.TermVariantSet,
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
          Variants.TypeVariantList,
          Variants.TypeVariantLiteral,
          Variants.TypeVariantMap,
          Variants.TypeVariantMaybe,
          Variants.TypeVariantPair,
          Variants.TypeVariantRecord,
          Variants.TypeVariantSet,
          Variants.TypeVariantUnion,
          Variants.TypeVariantUnit,
          Variants.TypeVariantForall,
          Variants.TypeVariantVariable,
          Variants.TypeVariantVoid,
          Variants.TypeVariantWrap]
    typePredicate = \_ -> True
-- | A set of reserved words in Scala
scalaReservedWords :: S.Set String
scalaReservedWords =
    Sets.fromList (Lists.concat [
      keywords,
      classNames,
      hydraScalaKeywords])
  where
    keywords =
        [
          "abstract",
          "case",
          "catch",
          "class",
          "def",
          "do",
          "else",
          "end",
          "enum",
          "export",
          "extends",
          "false",
          "final",
          "finally",
          "for",
          "forSome",
          "given",
          "if",
          "implicit",
          "import",
          "lazy",
          "macro",
          "match",
          "new",
          "null",
          "object",
          "override",
          "package",
          "private",
          "protected",
          "return",
          "sealed",
          "super",
          "then",
          "this",
          "throw",
          "trait",
          "true",
          "try",
          "type",
          "val",
          "var",
          "while",
          "with",
          "yield"]
    classNames =
        [
          "Any",
          "AnyVal",
          "App",
          "Array",
          "Boolean",
          "Byte",
          "Char",
          "Console",
          "DelayedInit",
          "Double",
          "DummyExplicit",
          "Dynamic",
          "Enumeration",
          "Equals",
          "Float",
          "Function",
          "Int",
          "Long",
          "MatchError",
          "None",
          "Nothing",
          "Null",
          "Option",
          "PartialFunction",
          "Predef",
          "Product",
          "Proxy",
          "SerialVersionUID",
          "Short",
          "Singleton",
          "Some",
          "Specializable",
          "StringContext",
          "Symbol",
          "Unit",
          "ValueOf"]
    hydraScalaKeywords = []
