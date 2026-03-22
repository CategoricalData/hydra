module Hydra.Ext.Sources.Lisp.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Accessors                       as Accessors
import qualified Hydra.Dsl.Ast                             as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                          as Coders
import qualified Hydra.Dsl.Util                            as Compute
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Grammar                         as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                      as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Module                          as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                        as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                          as Typing
import qualified Hydra.Dsl.Util                            as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                    as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y


lispLanguageDefinition :: String -> TTerm a -> TBinding a
lispLanguageDefinition = definitionInModule module_

module_ :: Module
module_ = Module (Namespace "hydra.ext.lisp.language")
  [toTermDefinition lispLanguage, toTermDefinition lispReservedWords]
  [Lexical.ns]
  KernelTypes.kernelTypesNamespaces $
  Just "Language constraints and reserved words for Lisp (covering Clojure, Emacs Lisp, Common Lisp, and Scheme)"

-- | Language constraints for Lisp.
--
-- Lisp is dynamically typed, so all Hydra types must be represented at the term level
-- (as runtime data structures) rather than through a static type system.
--
-- Type mapping strategy:
--   Hydra Type          -> Lisp representation
--   -----------------------------------------------
--   Record              -> defrecord (Clojure), defstruct (CL/Elisp), define-record-type (Scheme)
--   Union               -> Tagged data: keyword + value, dispatched with cond/case
--   List                -> Native list (cons cells or persistent list)
--   Map                 -> Hash map literal (Clojure), alist or hash-table (CL/Elisp/Scheme)
--   Set                 -> Hash set literal (Clojure), list-as-set (others)
--   Maybe               -> nil for Nothing, value for Just (with nil-punning caveats in Scheme)
--   Either              -> Tagged pair: (:left value) or (:right value)
--   Pair                -> Two-element list or vector
--   Wrap (newtype)      -> Transparent wrapper; unwrap at construction/access
--   Literal             -> Native literal (integer, float, string, boolean, character)
--   Function (A -> B)   -> Lambda / defun
--   Unit                -> nil or '()
--
-- Term mapping strategy:
--   Hydra Term          -> Lisp expression
--   -----------------------------------------------
--   Application          -> (f arg1 arg2 ...)
--   Lambda               -> (fn [params] body) / (lambda (params) body)
--   Variable             -> Symbol reference
--   Let                  -> (let [bindings] body) / (let ((bindings)) body)
--   Literal              -> Native literal
--   List                 -> '(elements) or (list elements)
--   Record construction  -> (make-RecordName field1 field2 ...) or (->RecordName field1 field2 ...)
--   Union injection      -> '(:variant value) or (list :variant value)
--   Elimination (record) -> (record-field instance) or (:field instance)
--   Elimination (union)  -> (cond (eq? tag :v1) ... (eq? tag :v2) ...) or case dispatch
--   Map construction     -> {:k1 v1 :k2 v2} (Clojure) or alist (others)
--   Set construction     -> #{elements} (Clojure) or list-to-set (others)
--   Maybe (Just v)       -> v
--   Maybe (Nothing)      -> nil / '()
--   Either (Left v)      -> '(:left v)
--   Either (Right v)     -> '(:right v)
--   Pair (a, b)          -> (list a b) or [a b]
--   Wrap                 -> value (transparent)
--   Unit                 -> nil / '()

lispLanguage :: TBinding Language
lispLanguage = lispLanguageDefinition "lispLanguage" $
    doc "Language constraints for Lisp" $ lets [
    "eliminationVariants">: Sets.fromList $ list [
      Variants.eliminationVariantRecord, -- field access: (:field record) or (record-field instance)
      Variants.eliminationVariantUnion, -- case dispatch on tagged unions
      Variants.eliminationVariantWrap], -- newtype unwrapping (transparent)
    "literalVariants">: Sets.fromList $ list [
      Variants.literalVariantBinary, -- byte arrays / byte strings
      Variants.literalVariantBoolean, -- true/false, t/nil, #t/#f
      Variants.literalVariantFloat, -- floating-point numbers
      Variants.literalVariantInteger, -- integers (all dialects have arbitrary-precision)
      Variants.literalVariantString], -- strings
    "floatTypes">: Sets.fromList $ list [
      Core.floatTypeBigfloat, -- all dialects support arbitrary-precision floats via libraries
      Core.floatTypeFloat64], -- double-precision float (native in all dialects)
    "functionVariants">: Sets.fromList $ list [
      Variants.functionVariantElimination, -- case/cond dispatch, field projection
      Variants.functionVariantLambda, -- fn/lambda
      Variants.functionVariantPrimitive], -- calls to primitive functions
    "integerTypes">: Sets.fromList $ list [
      Core.integerTypeBigint], -- all four dialects have arbitrary-precision integers
    "termVariants">: Sets.fromList $ list [
      Variants.termVariantAnnotated,
      Variants.termVariantApplication,
      Variants.termVariantEither,
      Variants.termVariantFunction,
      Variants.termVariantLet,
      Variants.termVariantList,
      Variants.termVariantLiteral,
      Variants.termVariantMap,
      Variants.termVariantMaybe,
      Variants.termVariantPair,
      Variants.termVariantRecord,
      Variants.termVariantSet,
      Variants.termVariantUnion,
      Variants.termVariantUnit,
      Variants.termVariantVariable,
      Variants.termVariantWrap],
    "typeVariants">: Sets.fromList $ list [
      Variants.typeVariantAnnotated,
      Variants.typeVariantApplication,
      Variants.typeVariantEither,
      Variants.typeVariantFunction,
      Variants.typeVariantForall,
      Variants.typeVariantList,
      Variants.typeVariantLiteral,
      Variants.typeVariantMap,
      Variants.typeVariantMaybe,
      Variants.typeVariantPair,
      Variants.typeVariantRecord,
      Variants.typeVariantSet,
      Variants.typeVariantUnion,
      Variants.typeVariantUnit,
      Variants.typeVariantVariable,
      Variants.typeVariantVoid,
      Variants.typeVariantWrap],
    "typePredicate">: constant true] $
    Coders.language
      (Coders.languageName_ $ string "hydra.ext.lisp")
      (Coders.languageConstraints_
        (var "eliminationVariants")
        (var "literalVariants")
        (var "floatTypes")
        (var "functionVariants")
        (var "integerTypes")
        (var "termVariants")
        (var "typeVariants")
        (var "typePredicate"))

lispReservedWords :: TBinding (S.Set String)
lispReservedWords = lispLanguageDefinition "lispReservedWords" $
  doc "A set of reserved words across all four Lisp dialects" $
  lets [
    -- Clojure keywords
    "clojureKeywords">:
      doc "Clojure special forms and reserved symbols" $
      list $ string <$> [
        "def", "defn", "defn-", "defmacro", "defrecord", "deftype", "defprotocol", "defmulti",
        "defmethod", "fn", "let", "loop", "recur", "if", "do", "cond", "case", "when", "when-not",
        "when-let", "if-let", "and", "or", "not", "nil", "true", "false", "throw", "try", "catch",
        "finally", "quote", "var", "ns", "require", "import", "use", "in-ns", "refer",
        "new", "set!", "monitor-enter", "monitor-exit"],

    -- Emacs Lisp keywords
    "emacsLispKeywords">:
      doc "Emacs Lisp special forms and reserved symbols" $
      list $ string <$> [
        "defun", "defvar", "defconst", "defmacro", "defsubst", "defadvice", "defcustom", "defgroup",
        "lambda", "let", "let*", "if", "cond", "progn", "prog1", "prog2", "while", "dolist",
        "dotimes", "and", "or", "not", "nil", "t", "quote", "function", "setq", "setq-default",
        "require", "provide", "condition-case", "unwind-protect", "save-excursion",
        "save-restriction", "catch", "throw", "interactive", "cl-defstruct",
        "cl-case", "cl-loop", "cl-labels", "cl-flet", "pcase", "pcase-let", "seq-let"],

    -- Common Lisp keywords
    "commonLispKeywords">:
      doc "Common Lisp special operators, macros, and reserved symbols" $
      list $ string <$> [
        "defun", "defvar", "defparameter", "defconstant", "defmacro", "defgeneric", "defmethod",
        "defclass", "defstruct", "deftype", "defpackage", "defsetf",
        "lambda", "let", "let*", "flet", "labels", "macrolet", "symbol-macrolet",
        "if", "cond", "case", "typecase", "etypecase", "ecase",
        "progn", "prog1", "prog2", "block", "return-from", "tagbody", "go",
        "and", "or", "not", "nil", "t",
        "quote", "function", "setq", "setf",
        "do", "do*", "dolist", "dotimes", "loop",
        "values", "multiple-value-bind", "multiple-value-list",
        "the", "declare", "declaim", "proclaim",
        "in-package", "use-package", "export", "import", "intern",
        "handler-case", "handler-bind", "restart-case", "condition",
        "unwind-protect", "catch", "throw",
        "eval-when", "load-time-value", "locally", "the"],

    -- Scheme R7RS keywords
    "schemeKeywords">:
      doc "Scheme R7RS keywords and special forms" $
      list $ string <$> [
        "define", "define-syntax", "define-record-type", "define-library", "define-values",
        "lambda", "let", "let*", "letrec", "letrec*", "let-values", "let*-values",
        "if", "cond", "case", "when", "unless",
        "and", "or", "not",
        "begin", "do",
        "quote", "quasiquote", "unquote", "unquote-splicing",
        "set!", "import", "export", "include", "include-ci",
        "syntax-rules", "syntax-case", "with-syntax",
        "call-with-current-continuation", "call/cc",
        "call-with-values", "values", "dynamic-wind",
        "guard", "parameterize",
        "else"],

    -- Words reserved by Hydra across all dialects
    "hydraLispKeywords">:
      doc "Reserved words specific to Hydra-Lisp code generation" $
      list $ string <$> ["Node"]] $

    Sets.fromList $ Lists.concat $ list [
      var "clojureKeywords",
      var "emacsLispKeywords",
      var "commonLispKeywords",
      var "schemeKeywords",
      var "hydraLispKeywords"]
