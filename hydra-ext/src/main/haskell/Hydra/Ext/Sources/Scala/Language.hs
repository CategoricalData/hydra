module Hydra.Ext.Sources.Scala.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
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
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
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
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
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


define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

module_ :: Module
module_ = Module (Namespace "hydra.ext.scala.language")
  [toDefinition scalaLanguage, toDefinition scalaReservedWords]
  [Lexical.ns]
  KernelTypes.kernelTypesNamespaces $
  Just "Language constraints and reserved words for Scala"

scalaLanguage :: TTermDefinition Language
scalaLanguage = define "scalaLanguage" $
  doc "Language constraints for Scala" $ lets [
  "eliminationVariants">: Sets.fromList $ list [
    Variants.eliminationVariantRecord,
    Variants.eliminationVariantUnion,
    Variants.eliminationVariantWrap],
  "literalVariants">: Sets.fromList $ list [
    Variants.literalVariantBoolean,
    Variants.literalVariantFloat,
    Variants.literalVariantInteger,
    Variants.literalVariantString],
  "floatTypes">: Sets.fromList $ list [
    Core.floatTypeBigfloat,
    Core.floatTypeFloat32,
    Core.floatTypeFloat64],
  "functionVariants">: Sets.fromList $ list [
    Variants.functionVariantElimination,
    Variants.functionVariantLambda,
    Variants.functionVariantPrimitive],
  "integerTypes">: Sets.fromList $ list [
    Core.integerTypeBigint,
    Core.integerTypeInt8,
    Core.integerTypeInt16,
    Core.integerTypeInt32,
    Core.integerTypeInt64,
    Core.integerTypeUint8,
    Core.integerTypeUint16,
    Core.integerTypeUint32,
    Core.integerTypeUint64],
  "termVariants">: Sets.fromList $ list [
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
    Variants.typeVariantList,
    Variants.typeVariantLiteral,
    Variants.typeVariantMap,
    Variants.typeVariantMaybe,
    Variants.typeVariantPair,
    Variants.typeVariantRecord,
    Variants.typeVariantSet,
    Variants.typeVariantUnion,
    Variants.typeVariantUnit,
    Variants.typeVariantForall,
    Variants.typeVariantVariable,
    Variants.typeVariantVoid,
    Variants.typeVariantWrap],
  "typePredicate">: constant true] $
  Coders.language
    (Coders.languageName_ $ string "hydra.ext.scala")
    (Coders.languageConstraints_
      (var "eliminationVariants")
      (var "literalVariants")
      (var "floatTypes")
      (var "functionVariants")
      (var "integerTypes")
      (var "termVariants")
      (var "typeVariants")
      (var "typePredicate"))

scalaReservedWords :: TTermDefinition (S.Set String)
scalaReservedWords = define "scalaReservedWords" $
  doc "A set of reserved words in Scala" $ lets [
  "keywords">:
    doc "Scala keywords" $
    list $ string <$> [
      "abstract", "case", "catch", "class", "def", "do", "else", "end", "enum", "export", "extends", "false", "final", "finally", "for",
      "forSome", "given", "if", "implicit", "import", "lazy", "macro", "match", "new", "null", "object", "override", "package", "private",
      "protected", "return", "sealed", "super", "then", "this", "throw", "trait", "true", "try", "type", "val", "var", "while",
      "with", "yield"],
  "classNames">:
    doc "Classes in the Scala Standard Library 2.13.8" $
    list $ string <$> [
      "Any", "AnyVal", "App", "Array", "Boolean", "Byte", "Char", "Console", "DelayedInit", "Double", "DummyExplicit",
      "Dynamic", "Enumeration", "Equals", "Float", "Function", "Int", "Long", "MatchError", "None",
      "Nothing", "Null", "Option", "PartialFunction", "Predef", "Product", "Proxy",
      "SerialVersionUID", "Short", "Singleton", "Some", "Specializable", "StringContext",
      "Symbol", "Unit", "ValueOf"],
  "hydraScalaKeywords">:
    doc "Reserved words which are specific to Hydra" $
    list $ string <$> []] $
  Sets.fromList $ Lists.concat $ list [
    var "keywords",
    var "classNames",
    var "hydraScalaKeywords"]
