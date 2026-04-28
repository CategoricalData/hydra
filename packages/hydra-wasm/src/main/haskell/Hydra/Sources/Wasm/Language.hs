module Hydra.Sources.Wasm.Language where

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
import qualified Hydra.Dsl.Packaging                     as Packaging
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
module_ = Module {
            moduleNamespace = (Namespace "hydra.wasm.language"),
            moduleDefinitions = [toDefinition wasmLanguage, toDefinition wasmReservedWords],
            moduleTermDependencies = [Lexical.ns],
            moduleTypeDependencies = KernelTypes.kernelTypesNamespaces,
            moduleDescription = Just "Language constraints and reserved words for WebAssembly (WAT text format)"}
wasmLanguage :: TTermDefinition Language
wasmLanguage = define "wasmLanguage" $
    doc "Language constraints for WebAssembly" $ lets [
    "eliminationVariants">: Sets.fromList $ list [
      Variants.eliminationVariantRecord,
      Variants.eliminationVariantUnion],
    "literalVariants">: Sets.fromList $ list [
      Variants.literalVariantBoolean, -- i32 (0/1)
      Variants.literalVariantFloat, -- f32, f64
      Variants.literalVariantInteger, -- i32, i64
      Variants.literalVariantString], -- i32 (memory pointer)
    "floatTypes">: Sets.fromList $ list [
      Core.floatTypeFloat32, -- f32
      Core.floatTypeFloat64], -- f64
    "functionVariants">: Sets.fromList $ list [
      Variants.functionVariantElimination,
      Variants.functionVariantLambda],
    "integerTypes">: Sets.fromList $ list [
      Core.integerTypeInt8,   -- i32
      Core.integerTypeInt16,  -- i32
      Core.integerTypeInt32,  -- i32
      Core.integerTypeInt64,  -- i64
      Core.integerTypeUint8,  -- i32
      Core.integerTypeUint16, -- i32
      Core.integerTypeUint32, -- i32
      Core.integerTypeUint64], -- i64
    "termVariants">: Sets.fromList $ list [
      Variants.termVariantAnnotated,
      Variants.termVariantApplication,
      Variants.termVariantCases,
      Variants.termVariantEither,
      Variants.termVariantInject,
      Variants.termVariantLambda,
      Variants.termVariantLet,
      Variants.termVariantList,
      Variants.termVariantLiteral,
      Variants.termVariantMap,
      Variants.termVariantMaybe,
      Variants.termVariantPair,
      Variants.termVariantProject,
      Variants.termVariantRecord,
      Variants.termVariantSet,
      Variants.termVariantTypeApplication,
      Variants.termVariantTypeLambda,
      Variants.termVariantUnit,
      Variants.termVariantUnwrap,
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
      (Coders.languageName_ $ string "hydra.wasm")
      (Coders.languageConstraints_
        (var "eliminationVariants")
        (var "literalVariants")
        (var "floatTypes")
        (var "functionVariants")
        (var "integerTypes")
        (var "termVariants")
        (var "typeVariants")
        (var "typePredicate"))

wasmReservedWords :: TTermDefinition (S.Set String)
wasmReservedWords = define "wasmReservedWords" $
  doc "A set of reserved words in WebAssembly text format" $
  lets [
    "keywords">:
      doc "WAT keywords (section and instruction keywords)" $
      list $ string <$> [
        "module", "type", "func", "param", "result", "local", "global",
        "table", "memory", "elem", "data", "start", "import", "export",
        "block", "loop", "if", "then", "else", "end", "br", "br_if",
        "br_table", "return", "call", "call_indirect", "drop", "select",
        "unreachable", "nop", "mut", "offset", "align"],
    "valueTypes">:
      doc "WAT value type keywords" $
      list $ string <$> [
        "i32", "i64", "f32", "f64", "v128", "funcref", "externref"],
    "instructionPrefixes">:
      doc "Common WAT instruction prefixes" $
      list $ string <$> [
        "i32", "i64", "f32", "f64",
        "local.get", "local.set", "local.tee",
        "global.get", "global.set",
        "memory.size", "memory.grow"]] $
    Sets.fromList $ Lists.concat $ list [
      var "keywords",
      var "valueTypes",
      var "instructionPrefixes"]
