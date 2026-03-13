module Hydra.Ext.Sources.Tinkerpop.Language where

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
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Meta.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Compute                    as Compute
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Meta.Json                       as Json
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
import qualified Hydra.Dsl.Meta.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Meta.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Meta.Typing                     as Typing
import qualified Hydra.Dsl.Meta.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules  as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple   as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms    as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils    as AdaptUtils
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
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import qualified Hydra.Ext.Org.Apache.Tinkerpop.Features as TF
import qualified Hydra.Ext.Sources.Tinkerpop.Features as TinkerpopFeatures


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.tinkerpop.language"

module_ :: Module
module_ = Module ns elements
    [Rewriting.ns]
    (TinkerpopFeatures.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Language constraints based on TinkerPop Graph.Features"
  where
    elements = [
      toBinding tinkerpopLanguage]


-- | Populate language constraints based on TinkerPop Graph.Features.
-- Note: although Graph.Features is phrased such that it defaults to supporting features not explicitly mentioned,
--       for Hydra we cannot support a term or type pattern unless it is provably safe in the target environment.
--       Otherwise, generated expressions could cause failure during runtime operations.
-- Also note that extra features are required on top of Graph.Features, again for reasons of completeness.
tinkerpopLanguage :: TBinding (LanguageName -> TF.Features -> TF.ExtraFeatures a -> Language)
tinkerpopLanguage = define "tinkerpopLanguage" $
  doc "Populate language constraints based on TinkerPop Graph.Features" $
  lambda "name" $ lambda "features" $ lambda "extras" $ lets [
    -- Extract vertex property data type features
    "vpFeatures">:
      project TF._VertexPropertyFeatures TF._VertexPropertyFeatures_dataTypeFeatures @@
        (project TF._VertexFeatures TF._VertexFeatures_properties @@
          (project TF._Features TF._Features_vertex @@ var "features")),

    -- Helper: conditional inclusion
    "cond">: lambda "v" $ lambda "b" $
      Logic.ifElse (var "b") (Maybes.pure (var "v")) nothing,

    -- Whether lists are supported (any array type is supported)
    "supportsLists">:
      Logic.or (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsBooleanArrayValues @@ var "vpFeatures")
        (Logic.or (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsByteArrayValues @@ var "vpFeatures")
        (Logic.or (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsDoubleArrayValues @@ var "vpFeatures")
        (Logic.or (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsFloatArrayValues @@ var "vpFeatures")
        (Logic.or (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsIntegerArrayValues @@ var "vpFeatures")
        (Logic.or (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsLongArrayValues @@ var "vpFeatures")
          (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsStringArrayValues @@ var "vpFeatures")))))),

    -- Support for at least one of the Graph.Features literal types is assumed.
    "supportsLiterals">: true,

    -- Note: additional constraints are required, beyond Graph.Features, if maps are supported
    "supportsMaps">: project TF._DataTypeFeatures TF._DataTypeFeatures_supportsMapValues @@ var "vpFeatures",

    "eliminationVariants">: Sets.empty,

    "literalVariants">: Sets.fromList (Maybes.cat $ list [
      var "cond" @@ Variants.literalVariantBinary
        @@ (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsByteArrayValues @@ var "vpFeatures"),
      var "cond" @@ Variants.literalVariantBoolean
        @@ (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsBooleanValues @@ var "vpFeatures"),
      var "cond" @@ Variants.literalVariantFloat
        @@ (Logic.or (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsFloatValues @@ var "vpFeatures")
                     (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsDoubleValues @@ var "vpFeatures")),
      var "cond" @@ Variants.literalVariantInteger
        @@ (Logic.or (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsIntegerValues @@ var "vpFeatures")
                     (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsLongValues @@ var "vpFeatures")),
      var "cond" @@ Variants.literalVariantString
        @@ (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsStringValues @@ var "vpFeatures")]),

    "floatTypes">: Sets.fromList (Maybes.cat $ list [
      var "cond" @@ Core.floatTypeFloat32
        @@ (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsFloatValues @@ var "vpFeatures"),
      var "cond" @@ Core.floatTypeFloat64
        @@ (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsDoubleValues @@ var "vpFeatures")]),

    "functionVariants">: Sets.empty,

    "integerTypes">: Sets.fromList (Maybes.cat $ list [
      var "cond" @@ Core.integerTypeInt32
        @@ (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsIntegerValues @@ var "vpFeatures"),
      var "cond" @@ Core.integerTypeInt64
        @@ (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsLongValues @@ var "vpFeatures")]),

    -- Only lists and literal values may be explicitly supported via Graph.Features.
    "termVariants">: Sets.fromList (Maybes.cat $ list [
      var "cond" @@ Variants.termVariantList @@ var "supportsLists",
      var "cond" @@ Variants.termVariantLiteral @@ var "supportsLiterals",
      var "cond" @@ Variants.termVariantMap @@ var "supportsMaps",
      -- An optional value translates to an absent vertex property
      Maybes.pure Variants.termVariantMaybe]),

    "typeVariants">: Sets.fromList (Maybes.cat $ list [
      var "cond" @@ Variants.typeVariantList @@ var "supportsLists",
      var "cond" @@ Variants.typeVariantLiteral @@ var "supportsLiterals",
      var "cond" @@ Variants.typeVariantMap @@ var "supportsMaps",
      Maybes.pure Variants.typeVariantMaybe,
      Maybes.pure Variants.typeVariantWrap]),

    "typePredicate">: "typ" ~> lets [
      "dt">: Rewriting.deannotateType @@ var "typ"] $
      cases _Type (var "dt") (Just true) [
        -- Only lists of literal values are supported, as nothing else is mentioned in Graph.Features
        _Type_list>>: "t" ~> cases _Type (Rewriting.deannotateType @@ var "t") (Just false) [
          _Type_literal>>: "lt" ~> cases _LiteralType (var "lt") (Just false) [
            _LiteralType_boolean>>: constant
              (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsBooleanArrayValues @@ var "vpFeatures"),
            _LiteralType_float>>: "ft" ~> cases _FloatType (var "ft") (Just false) [
              _FloatType_float64>>: constant
                (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsDoubleArrayValues @@ var "vpFeatures"),
              _FloatType_float32>>: constant
                (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsFloatArrayValues @@ var "vpFeatures")],
            _LiteralType_integer>>: "it" ~> cases _IntegerType (var "it") (Just false) [
              _IntegerType_uint8>>: constant
                (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsByteArrayValues @@ var "vpFeatures"),
              _IntegerType_int32>>: constant
                (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsIntegerArrayValues @@ var "vpFeatures"),
              _IntegerType_int64>>: constant
                (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsLongArrayValues @@ var "vpFeatures")],
            _LiteralType_string>>: constant
              (project TF._DataTypeFeatures TF._DataTypeFeatures_supportsStringArrayValues @@ var "vpFeatures")]],
        _Type_literal>>: constant true,
        _Type_map>>: "mt" ~>
          project TF._ExtraFeatures TF._ExtraFeatures_supportsMapKey @@ var "extras"
            @@ (project _MapType _MapType_keys @@ var "mt"),
        _Type_wrap>>: constant true,
        _Type_maybe>>: "ot" ~> cases _Type (Rewriting.deannotateType @@ var "ot") (Just false) [
          _Type_literal>>: constant true]]] $

  Coders.language
    (var "name")
    (Coders.languageConstraints
      (var "eliminationVariants")
      (var "literalVariants")
      (var "floatTypes")
      (var "functionVariants")
      (var "integerTypes")
      (var "termVariants")
      (var "typeVariants")
      (var "typePredicate"))
