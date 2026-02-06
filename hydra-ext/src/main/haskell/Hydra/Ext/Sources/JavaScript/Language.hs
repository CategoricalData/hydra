-- | Language constraints and reserved words for JavaScript/ECMAScript

module Hydra.Ext.Sources.JavaScript.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import qualified Hydra.Sources.Kernel.Terms.All             as KernelTerms
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules   as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple    as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking        as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util    as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting      as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars        as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages       as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals        as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads          as Monads
import qualified Hydra.Sources.Kernel.Terms.Names           as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction       as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect         as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas         as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors  as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph      as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta       as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing     as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting         as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution    as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan          as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates       as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification     as Unification
import           Prelude hiding ((++))
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

module_ :: Module
module_ = Module (Namespace "hydra.ext.javaScript.language")
  [toBinding javaScriptLanguage, toBinding javaScriptReservedWords]
  [Lexical.ns]
  KernelTypes.kernelTypesNamespaces $
  Just "Language constraints and reserved words for JavaScript (ECMAScript 2024)"

javaScriptLanguage :: TBinding Language
javaScriptLanguage = define "javaScriptLanguage" $
    doc "Language constraints for JavaScript (ECMAScript 2024)" $ lets [
    "eliminationVariants">: Sets.fromList $ list [
      Variants.eliminationVariantRecord,
      Variants.eliminationVariantUnion,
      Variants.eliminationVariantWrap],
    "literalVariants">: Sets.fromList $ list [
      Variants.literalVariantBinary, -- Uint8Array
      Variants.literalVariantBoolean, -- boolean
      Variants.literalVariantFloat, -- number (see float types)
      Variants.literalVariantInteger, -- number/BigInt (see integer types)
      Variants.literalVariantString], -- string
    "floatTypes">: Sets.fromList $ list [
      -- JavaScript's number is IEEE 754 double precision (float64)
      Core.floatTypeFloat64],
    "functionVariants">: Sets.fromList $ list [
      Variants.functionVariantElimination,
      Variants.functionVariantLambda,
      Variants.functionVariantPrimitive],
    "integerTypes">: Sets.fromList $ list [
      -- JavaScript number can safely represent integers up to 2^53-1
      -- For larger integers, BigInt must be used
      Core.integerTypeInt32, -- Most integer operations use 32-bit
      Core.integerTypeBigint], -- BigInt for arbitrary precision
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
      Variants.termVariantTypeApplication,
      Variants.termVariantTypeLambda,
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
      Variants.typeVariantWrap],
    "typePredicate">: constant true] $ -- All types supported via runtime representation
    Coders.language
      (Coders.languageName $ string "hydra.ext.javaScript")
      (Coders.languageConstraints
        (var "eliminationVariants")
        (var "literalVariants")
        (var "floatTypes")
        (var "functionVariants")
        (var "integerTypes")
        (var "termVariants")
        (var "typeVariants")
        (var "typePredicate"))

javaScriptReservedWords :: TBinding (S.Set String)
javaScriptReservedWords = define "javaScriptReservedWords" $
  doc "A set of reserved words in JavaScript" $
  lets [
    "keywords">:
      doc "JavaScript keywords per ECMAScript 2024 specification" $
      list $ string <$> [
        -- Keywords
        "await", "break", "case", "catch", "class", "const", "continue",
        "debugger", "default", "delete", "do", "else", "enum", "export",
        "extends", "false", "finally", "for", "function", "if", "import",
        "in", "instanceof", "let", "new", "null", "return", "super",
        "switch", "this", "throw", "true", "try", "typeof", "undefined",
        "var", "void", "while", "with", "yield"],
    "futureReserved">:
      doc "Future reserved words (strict mode)" $
      list $ string <$> [
        "implements", "interface", "package", "private", "protected", "public"],
    "strictModeReserved">:
      doc "Additional words reserved in strict mode" $
      list $ string <$> [
        "arguments", "eval"],
    "builtIns">:
      doc "Common built-in identifiers we should avoid" $
      list $ string <$> [
        -- Global objects
        "Array", "ArrayBuffer", "BigInt", "Boolean", "DataView", "Date",
        "Error", "Float32Array", "Float64Array", "Function", "Int8Array",
        "Int16Array", "Int32Array", "JSON", "Map", "Math", "Number",
        "Object", "Promise", "Proxy", "Reflect", "RegExp", "Set", "String",
        "Symbol", "Uint8Array", "Uint8ClampedArray", "Uint16Array",
        "Uint32Array", "WeakMap", "WeakSet",
        -- Global functions
        "decodeURI", "decodeURIComponent", "encodeURI", "encodeURIComponent",
        "eval", "isFinite", "isNaN", "parseFloat", "parseInt",
        -- Common identifiers in Node.js and browsers
        "console", "document", "global", "globalThis", "module", "process",
        "require", "window"],
    "hydraJavaScriptKeywords">:
      doc "Reserved words specific to Hydra-JavaScript" $
      list $ string <$> [
        -- Core utilities
        "Name", "FrozenMap",
        -- Tag constants
        "TERM_ANNOTATED", "TERM_APPLICATION", "TERM_EITHER", "TERM_FUNCTION",
        "TERM_LET", "TERM_LIST", "TERM_LITERAL", "TERM_MAP", "TERM_MAYBE",
        "TERM_PAIR", "TERM_RECORD", "TERM_SET", "TERM_TYPE_APPLICATION",
        "TERM_TYPE_LAMBDA", "TERM_UNION", "TERM_UNIT", "TERM_VARIABLE", "TERM_WRAP"]] $
    Sets.fromList $ Lists.concat $ list [
      var "keywords",
      var "futureReserved",
      var "strictModeReserved",
      var "builtIns",
      var "hydraJavaScriptKeywords"]
