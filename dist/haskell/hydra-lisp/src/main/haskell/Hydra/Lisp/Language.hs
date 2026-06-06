-- Note: this is an automatically generated file. Do not edit.
-- | Language constraints and reserved words for Lisp (covering Clojure, Emacs Lisp, Common Lisp, and Scheme)

module Hydra.Lisp.Language where
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
-- | Language constraints for Lisp
lispLanguage :: Coders.Language
lispLanguage =
    Coders.Language {
      Coders.languageName = (Coders.LanguageName "hydra.lisp"),
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
        Coders.caseConventionsDirectory = Util.CaseConventionLowerSnake,
        Coders.caseConventionsEnumValue = Util.CaseConventionLowerSnake,
        Coders.caseConventionsField = Util.CaseConventionLowerSnake,
        Coders.caseConventionsFile = Util.CaseConventionLowerSnake,
        Coders.caseConventionsModule = Util.CaseConventionLowerSnake,
        Coders.caseConventionsTerm = Util.CaseConventionLowerSnake,
        Coders.caseConventionsTermVariable = Util.CaseConventionLowerSnake,
        Coders.caseConventionsType = Util.CaseConventionLowerSnake,
        Coders.caseConventionsTypeVariable = Util.CaseConventionLowerSnake},
      Coders.languageDefaultFileExtension = (Util.FileExtension "clj")}
  where
    literalVariants =
        Sets.fromList [
          Variants.LiteralVariantBinary,
          Variants.LiteralVariantBoolean,
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
-- | A set of reserved words across all four Lisp dialects
lispReservedWords :: S.Set String
lispReservedWords =
    Sets.fromList (Lists.concat [
      clojureKeywords,
      emacsLispKeywords,
      commonLispKeywords,
      schemeKeywords,
      hydraLispKeywords])
  where
    clojureKeywords =
        [
          "def",
          "defn",
          "defn-",
          "defmacro",
          "defrecord",
          "deftype",
          "defprotocol",
          "defmulti",
          "defmethod",
          "fn",
          "let",
          "loop",
          "recur",
          "if",
          "do",
          "cond",
          "case",
          "when",
          "when-not",
          "when-let",
          "if-let",
          "and",
          "or",
          "not",
          "nil",
          "true",
          "false",
          "throw",
          "try",
          "catch",
          "finally",
          "quote",
          "var",
          "ns",
          "require",
          "import",
          "use",
          "in-ns",
          "refer",
          "new",
          "set!",
          "monitor-enter",
          "monitor-exit"]
    emacsLispKeywords =
        [
          "defun",
          "defvar",
          "defconst",
          "defmacro",
          "defsubst",
          "defadvice",
          "defcustom",
          "defgroup",
          "lambda",
          "let",
          "let*",
          "if",
          "cond",
          "progn",
          "prog1",
          "prog2",
          "while",
          "dolist",
          "dotimes",
          "and",
          "or",
          "not",
          "nil",
          "t",
          "quote",
          "function",
          "setq",
          "setq-default",
          "require",
          "provide",
          "condition-case",
          "unwind-protect",
          "save-excursion",
          "save-restriction",
          "catch",
          "throw",
          "interactive",
          "cl-defstruct",
          "cl-case",
          "cl-loop",
          "cl-labels",
          "cl-flet",
          "pcase",
          "pcase-let",
          "seq-let"]
    commonLispKeywords =
        [
          "defun",
          "defvar",
          "defparameter",
          "defconstant",
          "defmacro",
          "defgeneric",
          "defmethod",
          "defclass",
          "defstruct",
          "deftype",
          "defpackage",
          "defsetf",
          "lambda",
          "let",
          "let*",
          "flet",
          "labels",
          "macrolet",
          "symbol-macrolet",
          "if",
          "cond",
          "case",
          "typecase",
          "etypecase",
          "ecase",
          "progn",
          "prog1",
          "prog2",
          "block",
          "return-from",
          "tagbody",
          "go",
          "and",
          "or",
          "not",
          "nil",
          "t",
          "quote",
          "function",
          "setq",
          "setf",
          "do",
          "do*",
          "dolist",
          "dotimes",
          "loop",
          "values",
          "multiple-value-bind",
          "multiple-value-list",
          "the",
          "declare",
          "declaim",
          "proclaim",
          "in-package",
          "use-package",
          "export",
          "import",
          "intern",
          "handler-case",
          "handler-bind",
          "restart-case",
          "condition",
          "unwind-protect",
          "catch",
          "throw",
          "eval-when",
          "load-time-value",
          "locally",
          "the",
          "pi"]
    schemeKeywords =
        [
          "define",
          "define-syntax",
          "define-record-type",
          "define-library",
          "define-values",
          "lambda",
          "let",
          "let*",
          "letrec",
          "letrec*",
          "let-values",
          "let*-values",
          "if",
          "cond",
          "case",
          "when",
          "unless",
          "and",
          "or",
          "not",
          "begin",
          "do",
          "quote",
          "quasiquote",
          "unquote",
          "unquote-splicing",
          "set!",
          "import",
          "export",
          "include",
          "include-ci",
          "syntax-rules",
          "syntax-case",
          "with-syntax",
          "call-with-current-continuation",
          "call/cc",
          "call-with-values",
          "values",
          "dynamic-wind",
          "guard",
          "parameterize",
          "else"]
    hydraLispKeywords = [
      "Node"]
