
module Hydra.Sources.Kernel.Terms.Adapt where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  adaptFloatType, adaptDataGraph, adaptGraphSchema, adaptIntegerType, adaptLambdaDomains, adaptLiteral,
  adaptLiteralType, adaptLiteralTypesMap, adaptLiteralValue, adaptNestedTypes, adaptPrimitive,
  adaptTerm, adaptTermForLanguage, adaptType, adaptTypeForLanguage, adaptTypeScheme,
  composeCoders, dataGraphToDefinitions, literalTypeSupported,
  prepareFloatType, prepareIntegerType, prepareLiteralType, prepareType, prepareSame,
  pushTypeAppsInward, schemaGraphToDefinitions,
  simpleLanguageAdapter, termAlternatives, typeAlternatives)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Errors       as Error
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Hoisting    as Hoisting
import qualified Hydra.Sources.Kernel.Terms.Inference   as Inference
import qualified Hydra.Sources.Kernel.Terms.Literals    as Literals
import qualified Hydra.Sources.Kernel.Terms.Names       as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction   as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect     as Reflect
import qualified Hydra.Sources.Kernel.Terms.Lexical      as Lexical

import qualified Hydra.Sources.Kernel.Terms.Dependencies as Dependencies
import qualified Hydra.Sources.Kernel.Terms.Rewriting   as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Environment  as Environment
import qualified Hydra.Sources.Kernel.Terms.Resolution   as Resolution
import qualified Hydra.Sources.Kernel.Terms.Scoping     as Scoping
import qualified Hydra.Sources.Kernel.Terms.Strip       as Strip
import qualified Hydra.Sources.Kernel.Terms.Variables   as Variables
import qualified Hydra.Sources.Kernel.Terms.Show.Core   as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Errors as ShowError
import qualified Hydra.Sources.Kernel.Terms.Show.Graph  as ShowGraph


ns :: Namespace
ns = Namespace "hydra.adapt"

module_ :: Module
module_ = Module ns elements
    [Dependencies.ns, Hoisting.ns, Inference.ns, Lexical.ns, Literals.ns, Names.ns, Reduction.ns, Reflect.ns, Rewriting.ns,
      Scoping.ns, Environment.ns, Resolution.ns, ShowCore.ns, ShowError.ns, ShowGraph.ns, Strip.ns, Variables.ns]
    kernelTypesNamespaces $
    Just "Simple, one-way adapters for types and terms"
  where
    elements = [
      toDefinition adaptFloatType,
      toDefinition adaptDataGraph,
      toDefinition adaptGraphSchema,
      toDefinition adaptIntegerType,
      toDefinition adaptLambdaDomains,
      toDefinition adaptLiteral,
      toDefinition adaptLiteralType,
      toDefinition adaptLiteralTypesMap,
      toDefinition adaptLiteralValue,
      toDefinition adaptNestedTypes,
      toDefinition adaptPrimitive,
      toDefinition adaptTerm,
      toDefinition adaptTermForLanguage,
      toDefinition adaptType,
      toDefinition adaptTypeForLanguage,
      toDefinition adaptTypeScheme,
      toDefinition composeCoders,
      toDefinition dataGraphToDefinitions,
      toDefinition literalTypeSupported,
      -- TODO: the prepare* functions below duplicate logic already in adaptFloatType, adaptIntegerType,
      -- adaptLiteralType, etc. They should be simplified or eliminated in favor of those functions.
      -- They were moved here from hydra.ext.scala.prepare as part of the coder standardization effort.
      toDefinition prepareFloatType,
      toDefinition prepareIntegerType,
      toDefinition prepareLiteralType,
      toDefinition prepareType,
      toDefinition prepareSame,
      toDefinition pushTypeAppsInward,
      toDefinition schemaGraphToDefinitions,
      toDefinition simpleLanguageAdapter,
      toDefinition termAlternatives,
      toDefinition typeAlternatives]

formatError :: TTerm (InContext Error -> String)
formatError = "ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic")

formatDecodingError :: TTerm (InContext DecodingError -> String)
formatDecodingError = "ic" ~> unwrap _DecodingError @@ Ctx.inContextObject (var "ic")

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

adaptFloatType :: TTermDefinition (LanguageConstraints -> FloatType -> Maybe FloatType)
adaptFloatType = define "adaptFloatType" $
  doc "Attempt to adapt a floating-point type using the given language constraints" $
  "constraints" ~> "ft" ~>
  "supported" <~ Sets.member (var "ft") (Coders.languageConstraintsFloatTypes $ var "constraints") $
  "alt" <~ (adaptFloatType @@ var "constraints") $
  "forUnsupported" <~ ("ft" ~> cases _FloatType (var "ft")
    Nothing [
--    _FloatType_bigfloat>>: constant nothing,
    _FloatType_bigfloat>>: constant $ var "alt" @@ Core.floatTypeFloat64, -- TODO: temporary; the only non-lossy alternative for bigfloat is string, but some migration is needed
    _FloatType_float32>>: constant $ var "alt" @@ Core.floatTypeFloat64,
    _FloatType_float64>>: constant $ var "alt" @@ Core.floatTypeBigfloat]) $
  Logic.ifElse (var "supported")
    (just $ var "ft")
    (var "forUnsupported" @@ var "ft")

adaptDataGraph :: TTermDefinition (LanguageConstraints -> Bool -> [Binding] -> Context -> Graph -> Prelude.Either String (Graph, [Binding]))
adaptDataGraph = define "adaptDataGraph" $
  doc ("Adapt a graph and its schema to the given language constraints."
    <> " The doExpand flag controls eta expansion of partial applications."
    <> " Adaptation is type-preserving: binding-level TypeSchemes are adapted (not stripped)."
    <> " Note: case statement hoisting is done separately, prior to adaptation."
    <> " The els0 parameter provides the original ordered bindings."
    <> " Returns both the adapted graph and the ordered adapted bindings.") $
  "constraints" ~> "doExpand" ~> "els0" ~> "cx" ~> "graph0" ~>
  -- Transform a single term: push type apps, eta expand, lift lambdas.
  -- Applied per-binding to avoid O(n²) behavior when processing all bindings as one let.
  "transformTerm" <~ ("g" ~> "term" ~>
    "tx" <~ var "g" $
    "t1" <~ Variables.unshadowVariables @@ (pushTypeAppsInward @@ var "term") $
    "t2" <~ Variables.unshadowVariables @@ (Logic.ifElse (var "doExpand")
      (pushTypeAppsInward @@ (Reduction.etaExpandTerm @@ var "tx" @@ var "t1"))
      (var "t1")) $
    Dependencies.liftLambdaAboveLet @@ var "t2") $
  -- Transform each binding's term individually, preserving name and type
  "transformBinding" <~ ("g" ~> "el" ~>
    Core.binding
      (Core.bindingName $ var "el")
      (var "transformTerm" @@ var "g" @@ (Core.bindingTerm $ var "el"))
      (Core.bindingType $ var "el")) $
  "litmap" <~ adaptLiteralTypesMap @@ var "constraints" $
  "prims0" <~ Graph.graphPrimitives (var "graph0") $
  "schemaTypes0" <~ Graph.graphSchemaTypes (var "graph0") $
  -- Adapt schema types
  "schemaBindings" <~ Environment.typesToDefinitions @@ Maps.map ("ts" ~> Scoping.typeSchemeToFType @@ var "ts") (var "schemaTypes0") $
  "schemaResult" <<~ Logic.ifElse (Maps.null (var "schemaTypes0"))
    (right (Maps.empty :: TTerm (M.Map Name TypeScheme)))
    ("tmap0" <<~ Eithers.bimap formatDecodingError ("x" ~> var "x") (Environment.graphAsTypes @@ var "cx" @@ var "graph0" @@ var "schemaBindings") $
      "tmap1" <<~ adaptGraphSchema @@ var "constraints" @@ var "litmap" @@ var "tmap0" $
      right $ Maps.map ("t" ~> Resolution.typeToTypeScheme @@ var "t") (var "tmap1")) $
  "adaptedSchemaTypes" <~ var "schemaResult" $
  -- Process each binding individually: transform, wrap in single-binding let,
  -- adapt, rewrite lambda domains, then extract. This avoids building one giant
  -- let with all bindings, which causes bytecode stack overflow in EL.
  "adaptBinding" <~ ("el" ~>
    "transformed" <~ var "transformBinding" @@ var "graph0" @@ var "el" $
    "wrapped" <~ Core.termLet (Core.let_ (Lists.pure (var "transformed")) Core.termUnit) $
    "adapted" <<~ adaptTerm @@ var "constraints" @@ var "litmap" @@ var "cx" @@ var "graph0" @@ var "wrapped" $
    Rewriting.rewriteTermM @@ (adaptLambdaDomains @@ var "constraints" @@ var "litmap") @@ var "adapted") $
  "adaptedTerms" <<~ Eithers.mapList (var "adaptBinding") (var "els0") $
  "els1Raw" <~ Lists.concat (Lists.map Environment.termAsBindings (var "adaptedTerms")) $

  -- Adapt nested let binding TypeSchemes within each top-level binding's term.
  -- These TypeSchemes may carry stale types from JSON modules (e.g. bigfloat→float64).
  -- Applied per-binding AFTER termAsBindings so that top-level binding TypeSchemes
  -- (which carry type-class constraints like Ord) are preserved.
  -- Process each binding: adapt nested let TypeSchemes AND adapt top-level TypeSchemes.
  -- Adapting (rather than stripping) TypeSchemes converts stale types like bigfloat→float64
  -- while preserving type-class constraints like Ord needed by decodeSet.
  "processBinding" <~ ("el" ~>
    "newTerm" <<~ Rewriting.rewriteTermM @@ (adaptNestedTypes @@ var "constraints" @@ var "litmap") @@ (Core.bindingTerm $ var "el") $
    "adaptedType" <<~ optCases (Core.bindingType $ var "el")
      (right nothing)
      ("ts" ~>
        "ts1" <<~ adaptTypeScheme @@ var "constraints" @@ var "litmap" @@ var "ts" $
        right $ just $ var "ts1") $
    right $ Core.binding
      (Core.bindingName $ var "el")
      (var "newTerm")
      (var "adaptedType")) $
  "els1" <<~ Eithers.mapList (var "processBinding") (var "els1Raw") $
  "primPairs" <<~ Eithers.mapList ("kv" ~>
    "prim1" <<~ adaptPrimitive @@ var "constraints" @@ var "litmap" @@ (Pairs.second $ var "kv") $
    right $ pair (Pairs.first $ var "kv") (var "prim1")) (Maps.toList (var "prims0")) $
  "prims1" <~ Maps.fromList (var "primPairs") $
  "adaptedGraphRaw" <~ (Lexical.buildGraph @@ var "els1" @@ Maps.empty @@ var "prims1") $
  "adaptedGraph" <~ Graph.graphWithSchemaTypes (var "adaptedGraphRaw") (var "adaptedSchemaTypes") $
  right $ pair (var "adaptedGraph") (var "els1")

-- | Rewrite callback for adapting lambda domains in a term.
-- Dispatches on Term variants: for TermFunction, adapts the lambda domain type;
-- for all other variants, returns the term unchanged.
-- This is a top-level function (not inline) so the Python code generator can emit match statements.
adaptLambdaDomains :: TTermDefinition (LanguageConstraints -> M.Map LiteralType LiteralType -> (Term -> Prelude.Either String Term) -> Term -> Prelude.Either String Term)
adaptLambdaDomains = define "adaptLambdaDomains" $
  doc "Rewrite callback for adapting lambda domain types in a term" $
  "constraints" ~> "litmap" ~> "recurse" ~> "term" ~>
  "rewritten" <<~ var "recurse" @@ var "term" $
  cases _Term (var "rewritten")
    (Just $ right $ var "rewritten") [
    _Term_function>>: "f" ~>
      cases _Function (var "f")
        (Just $ right $ Core.termFunction $ var "f") [
        _Function_lambda>>: "l" ~>
          "adaptedDomain" <<~ optCases (Core.lambdaDomain $ var "l")
            (right nothing)
            ("dom" ~>
              "dom1" <<~ adaptType @@ var "constraints" @@ var "litmap" @@ var "dom" $
              right $ just $ var "dom1") $
          right $ Core.termFunction $ Core.functionLambda $ Core.lambda
            (Core.lambdaParameter $ var "l")
            (var "adaptedDomain")
            (Core.lambdaBody $ var "l")]]

-- | Rewrite callback for adapting nested let binding TypeSchemes in a term.
-- Dispatches on Term variants: for TermLet, adapts the binding TypeSchemes;
-- for all other variants, returns the term unchanged.
-- This is a top-level function (not inline) so the Python code generator can emit match statements.
adaptNestedTypes :: TTermDefinition (LanguageConstraints -> M.Map LiteralType LiteralType -> (Term -> Prelude.Either String Term) -> Term -> Prelude.Either String Term)
adaptNestedTypes = define "adaptNestedTypes" $
  doc "Rewrite callback for adapting nested let binding TypeSchemes in a term" $
  "constraints" ~> "litmap" ~> "recurse" ~> "term" ~>
  "rewritten" <<~ var "recurse" @@ var "term" $
  cases _Term (var "rewritten")
    (Just $ right $ var "rewritten") [
    _Term_let>>: "lt" ~>
      "adaptB" <~ ("b" ~>
        "adaptedBType" <<~ optCases (Core.bindingType $ var "b")
          (right nothing)
          ("ts" ~>
            "ts1" <<~ adaptTypeScheme @@ var "constraints" @@ var "litmap" @@ var "ts" $
            right $ just $ var "ts1") $
        right $ Core.binding
          (Core.bindingName $ var "b")
          (Core.bindingTerm $ var "b")
          (var "adaptedBType")) $
      "adaptedBindings" <<~ Eithers.mapList (var "adaptB") (Core.letBindings $ var "lt") $
      right $ Core.termLet $ Core.let_
        (var "adaptedBindings")
        (Core.letBody $ var "lt")]

adaptGraphSchema :: TTermDefinition (LanguageConstraints -> M.Map LiteralType LiteralType -> M.Map Name Type -> Prelude.Either String (M.Map Name Type))
adaptGraphSchema = define "adaptGraphSchema" $
  doc "Adapt a schema graph to the given language constraints" $
  "constraints" ~> "litmap" ~> "types0" ~>
  "mapPair" <~ ("pair" ~>
    "name" <~ Pairs.first (var "pair") $
    "typ" <~ Pairs.second (var "pair") $
    "typ1" <<~ adaptType @@ var "constraints" @@ var "litmap" @@ var "typ" $
    right $ pair (var "name") (var "typ1")) $
  "pairs" <<~ Eithers.mapList (var "mapPair") (Maps.toList $ var "types0") $
  right $ Maps.fromList (var "pairs")

adaptIntegerType :: TTermDefinition (LanguageConstraints -> IntegerType -> Maybe IntegerType)
adaptIntegerType = define "adaptIntegerType" $
  doc "Attempt to adapt an integer type using the given language constraints" $
  "constraints" ~> "it" ~>
  "supported" <~ Sets.member (var "it") (Coders.languageConstraintsIntegerTypes $ var "constraints") $
  "alt" <~ (adaptIntegerType @@ var "constraints") $
  "forUnsupported" <~ ("it" ~> cases _IntegerType (var "it")
    Nothing [
    _IntegerType_bigint>>: constant nothing,
    _IntegerType_int8>>: constant $ var "alt" @@ Core.integerTypeUint16,
    _IntegerType_int16>>: constant $ var "alt" @@ Core.integerTypeUint32,
    _IntegerType_int32>>: constant $ var "alt" @@ Core.integerTypeUint64,
    _IntegerType_int64>>: constant $ var "alt" @@ Core.integerTypeBigint,
    _IntegerType_uint8>>: constant $ var "alt" @@ Core.integerTypeInt16,
    _IntegerType_uint16>>: constant $ var "alt" @@ Core.integerTypeInt32,
    _IntegerType_uint32>>: constant $ var "alt" @@ Core.integerTypeInt64,
    _IntegerType_uint64>>: constant $ var "alt" @@ Core.integerTypeBigint]) $
  Logic.ifElse (var "supported")
    (just $ var "it")
    (var "forUnsupported" @@ var "it")

adaptLiteral :: TTermDefinition (LiteralType -> Literal -> Literal)
adaptLiteral = define "adaptLiteral" $
  doc "Convert a literal to a different type" $
  "lt" ~> "l" ~>
  cases _Literal (var "l")
    Nothing [
    _Literal_binary>>: "b" ~> cases _LiteralType (var "lt")
      Nothing [
      _LiteralType_string>>: constant $ Core.literalString $ Literals.binaryToString $ var "b"],
    _Literal_boolean>>: "b" ~> cases _LiteralType (var "lt")
      Nothing [
      _LiteralType_integer>>: "it" ~> Core.literalInteger $
        Literals.bigintToIntegerValue @@ var "it" @@ Logic.ifElse (var "b") (bigint 1) (bigint 0)],
    _Literal_float>>: "f" ~> cases _LiteralType (var "lt")
      Nothing [
      _LiteralType_float>>: "ft" ~> Core.literalFloat $
        Literals.bigfloatToFloatValue @@ var "ft" @@ (Literals.floatValueToBigfloat @@ var "f")],
    _Literal_integer>>: "i" ~> cases _LiteralType (var "lt")
      Nothing [
      _LiteralType_integer>>: "it" ~> Core.literalInteger $
        Literals.bigintToIntegerValue @@ var "it" @@ (Literals.integerValueToBigint @@ var "i")]]

adaptLiteralType :: TTermDefinition (LanguageConstraints -> LiteralType -> Maybe LiteralType)
adaptLiteralType = define "adaptLiteralType" $
  doc "Attempt to adapt a literal type using the given language constraints" $
  "constraints" ~> "lt" ~>
  "forUnsupported" <~ ("lt" ~> cases _LiteralType (var "lt")
    (Just nothing) [
    _LiteralType_binary>>: constant $ just Core.literalTypeString,
    _LiteralType_boolean>>: constant $ Maybes.map (unaryFunction Core.literalTypeInteger) $
      adaptIntegerType @@ var "constraints" @@ Core.integerTypeInt8,
    _LiteralType_float>>: "ft" ~> Maybes.map (unaryFunction Core.literalTypeFloat) $
      adaptFloatType @@ var "constraints" @@ var "ft",
    _LiteralType_integer>>: "it" ~> Maybes.map (unaryFunction Core.literalTypeInteger) $
      adaptIntegerType @@ var "constraints" @@ var "it"]) $
  Logic.ifElse (literalTypeSupported @@ var "constraints" @@ var "lt")
    nothing
    (var "forUnsupported" @@ var "lt")

adaptLiteralTypesMap :: TTermDefinition (LanguageConstraints -> M.Map LiteralType LiteralType)
adaptLiteralTypesMap = define "adaptLiteralTypesMap" $
  doc "Derive a map of adapted literal types for the given language constraints" $
  "constraints" ~>
  "tryType" <~ ("lt" ~> optCases (adaptLiteralType @@ var "constraints" @@ var "lt")
    nothing
    ("lt2" ~> just $ pair (var "lt") (var "lt2"))) $
  Maps.fromList $ Maybes.cat $ Lists.map (var "tryType") (Reflect.literalTypes)

adaptLiteralValue :: TTermDefinition (M.Map LiteralType LiteralType -> LiteralType -> Literal -> Literal)
adaptLiteralValue = define "adaptLiteralValue" $
  doc "Adapt a literal value using the given language constraints" $
  "litmap" ~> "lt" ~> "l" ~> optCases (Maps.lookup (var "lt") (var "litmap"))
    (Core.literalString $ ShowCore.literal @@ var "l")
    ("lt2" ~> adaptLiteral @@ var "lt2" @@ var "l")

adaptPrimitive :: TTermDefinition (LanguageConstraints -> M.Map LiteralType LiteralType -> Primitive -> Prelude.Either String Primitive)
adaptPrimitive = define "adaptPrimitive" $
  doc "Adapt a primitive to the given language constraints, prior to inference" $
  "constraints" ~> "litmap" ~> "prim0" ~>
  "ts0" <~ Graph.primitiveType (var "prim0") $
  "ts1" <<~ adaptTypeScheme @@ var "constraints" @@ var "litmap" @@ var "ts0" $
  right $ Graph.primitiveWithType (var "prim0") (var "ts1")

-- Note: this function could be made more efficient through precomputation of alternatives,
--       similar to what is done for literals.
adaptTerm :: TTermDefinition (LanguageConstraints -> M.Map LiteralType LiteralType -> Context -> Graph -> Term -> Prelude.Either String Term)
adaptTerm = define "adaptTerm" $
  doc "Adapt a term using the given language constraints" $
  "constraints" ~> "litmap" ~> "cx" ~> "graph" ~> "term0" ~>
  "rewrite" <~ ("recurse" ~> "term0" ~> lets [
    "forSupported">: ("term" ~> cases _Term (var "term")
      (Just $ right $ just $ var "term") [
      _Term_literal>>: "l" ~>
        "lt" <~ Reflect.literalType @@ var "l" $
        right $ just $ Logic.ifElse (literalTypeSupported @@ var "constraints" @@ var "lt")
          (var "term")
          (Core.termLiteral $ adaptLiteralValue @@ var "litmap" @@ var "lt" @@ var "l")]),
    "forUnsupported">: ("term" ~> lets [
      "forNonNull">: ("alts" ~>
        "mterm" <<~ var "tryTerm" @@ Lists.head (var "alts") $
        optCases (var "mterm")
          (var "tryAlts" @@ Lists.tail (var "alts"))
          ("t" ~> right $ just $ var "t")),
      "tryAlts">: ("alts" ~> Logic.ifElse (Lists.null $ var "alts")
        (right nothing)
        (var "forNonNull" @@ var "alts"))] $
      "alts0" <<~ termAlternatives @@ var "cx" @@ var "graph" @@ var "term" $
      var "tryAlts" @@ var "alts0"),
    "tryTerm">: ("term" ~>
      "supportedVariant" <~ Sets.member
        (Reflect.termVariant @@ var "term")
        (Coders.languageConstraintsTermVariants $ var "constraints") $
      Logic.ifElse (var "supportedVariant")
        (var "forSupported" @@ var "term")
        (var "forUnsupported" @@ var "term"))] $
    "term1" <<~ var "recurse" @@ var "term0" $
    -- Type application/lambda wrappers pass through unconditionally.
    -- fsub already recursed into their bodies; we must not strip the wrappers
    -- because they carry type information needed by typeOf in the coders.
    cases _Term (var "term1")
      (Just $
        "mterm" <<~ var "tryTerm" @@ var "term1" $
        optCases (var "mterm")
          (left $ (string "no alternatives for term: ") ++ (ShowCore.term @@ var "term1"))
          ("term2" ~> right $ var "term2"))
      [_Term_typeApplication>>: "ta" ~>
         "atyp" <<~ adaptType @@ var "constraints" @@ var "litmap" @@ (Core.typeApplicationTermType $ var "ta") $
         right $ Core.termTypeApplication $ Core.typeApplicationTerm
           (Core.typeApplicationTermBody $ var "ta")
           (var "atyp"),
       _Term_typeLambda>>:      "_" ~> right $ var "term1"]) $
  Rewriting.rewriteTermM @@ var "rewrite" @@ var "term0"

adaptType :: TTermDefinition (LanguageConstraints -> M.Map LiteralType LiteralType -> Type -> Prelude.Either String Type)
adaptType = define "adaptType" $
  doc "Adapt a type using the given language constraints" $
  "constraints" ~> "litmap" ~> "type0" ~>
  lets [
  "forSupported">: ("typ" ~> cases _Type (var "typ")
    (Just $ just $ var "typ") [
    _Type_literal>>: "lt" ~> Logic.ifElse (literalTypeSupported @@ var "constraints" @@ var "lt")
      (just $ var "typ")
      (optCases (Maps.lookup (var "lt") (var "litmap"))
        (just $ Core.typeLiteral Core.literalTypeString)
        ("lt2" ~> just $ Core.typeLiteral $ var "lt2"))]),
  "forUnsupported">: ("typ" ~>
    "tryAlts" <~ ("alts" ~> Logic.ifElse (Lists.null $ var "alts")
      nothing
      (optCases (var "tryType" @@ Lists.head (var "alts"))
        (var "tryAlts" @@ Lists.tail (var "alts"))
        ("t" ~> just $ var "t"))) $
    "alts0" <~ typeAlternatives @@ var "typ" $
    var "tryAlts" @@ var "alts0"),
  "tryType">: ("typ" ~>
    "supportedVariant" <~ Sets.member
      (Reflect.typeVariant @@ var "typ")
      (Coders.languageConstraintsTypeVariants $ var "constraints") $
    Logic.ifElse (var "supportedVariant")
      (var "forSupported" @@ var "typ")
      (var "forUnsupported" @@ var "typ"))] $
  "rewrite" <~ ("recurse" ~> "typ" ~>
    "type1" <<~ var "recurse" @@ var "typ" $
    optCases (var "tryType" @@ var "type1")
      (left $ (string "no alternatives for type: ") ++ (ShowCore.type_ @@ var "typ"))
      ("type2" ~> right $ var "type2")) $
  Rewriting.rewriteTypeM @@ var "rewrite" @@ var "type0"

adaptTypeScheme :: TTermDefinition (LanguageConstraints -> M.Map LiteralType LiteralType -> TypeScheme -> Prelude.Either String TypeScheme)
adaptTypeScheme = define "adaptTypeScheme" $
  doc "Adapt a type scheme to the given language constraints, prior to inference" $
  "constraints" ~> "litmap" ~> "ts0" ~>
  "vars0" <~ Core.typeSchemeVariables (var "ts0") $
  "t0" <~ Core.typeSchemeType (var "ts0") $
  "t1" <<~ adaptType @@ var "constraints" @@ var "litmap" @@ var "t0" $
  right $ Core.typeScheme (var "vars0") (var "t1") (Core.typeSchemeConstraints (var "ts0"))

pushTypeAppsInward :: TTermDefinition (Term -> Term)
pushTypeAppsInward = define "pushTypeAppsInward" $
  doc ("Normalize a term by pushing TermTypeApplication inward past TermApplication and"
    <> " TermFunction (Lambda). This corrects structures produced by poly-let hoisting and"
    <> " eta expansion, where type applications from inference end up wrapping term"
    <> " applications or lambda abstractions instead of being directly on the polymorphic variable.") $
  "term" ~>
  lets [
  "push">: ("body" ~> "typ" ~> cases _Term (var "body")
    -- Default: keep TypeApp as-is
    (Just $ Core.termTypeApplication $ Core.typeApplicationTerm (var "body") (var "typ")) [
    -- TypeApp(App(f, arg), τ) → go(App(TypeApp(f, τ), arg))
    _Term_application>>: "a" ~> var "go" @@
      (Core.termApplication $ Core.application
        (Core.termTypeApplication $ Core.typeApplicationTerm
          (Core.applicationFunction $ var "a")
          (var "typ"))
        (Core.applicationArgument $ var "a")),
    -- TypeApp(Lambda(v, d, body), τ) → go(Lambda(v, d, TypeApp(body, τ)))
    _Term_function>>: "f" ~> cases _Function (var "f")
      (Just $ Core.termTypeApplication $ Core.typeApplicationTerm
        (Core.termFunction $ var "f") (var "typ")) [
      _Function_lambda>>: "l" ~> var "go" @@
        (Core.termFunction $ Core.functionLambda $ Core.lambda
          (Core.lambdaParameter $ var "l")
          (Core.lambdaDomain $ var "l")
          (Core.termTypeApplication $ Core.typeApplicationTerm
            (Core.lambdaBody $ var "l")
            (var "typ")))],
    -- TypeApp(Let(bindings, body), τ) → go(Let(bindings, TypeApp(body, τ)))
    _Term_let>>: "lt" ~> var "go" @@
      (Core.termLet $ Core.let_
        (Core.letBindings $ var "lt")
        (Core.termTypeApplication $ Core.typeApplicationTerm
          (Core.letBody $ var "lt")
          (var "typ")))]),
  "go">: ("t" ~>
    "forField" <~ ("fld" ~> Core.fieldWithTerm (var "fld") (var "go" @@ (Core.fieldTerm $ var "fld"))) $
    "forElimination" <~ ("elm" ~> cases _Elimination (var "elm") Nothing [
      _Elimination_record>>: "p" ~> Core.eliminationRecord (var "p"),
      _Elimination_union>>: "cs" ~> Core.eliminationUnion $ Core.caseStatement
        (Core.caseStatementTypeName $ var "cs")
        (Maybes.map (var "go") (Core.caseStatementDefault $ var "cs"))
        (Lists.map (var "forField") (Core.caseStatementCases $ var "cs")),
      _Elimination_wrap>>: "name" ~> Core.eliminationWrap $ var "name"]) $
    "forFunction" <~ ("fun" ~> cases _Function (var "fun") Nothing [
      _Function_elimination>>: "elm" ~> Core.functionElimination $ var "forElimination" @@ var "elm",
      _Function_lambda>>: "l" ~> Core.functionLambda $ Core.lambda
        (Core.lambdaParameter $ var "l")
        (Core.lambdaDomain $ var "l")
        (var "go" @@ (Core.lambdaBody $ var "l")),
      _Function_primitive>>: "name" ~> Core.functionPrimitive $ var "name"]) $
    "forLet" <~ ("lt" ~>
      "mapBinding" <~ ("b" ~> Core.binding
        (Core.bindingName $ var "b")
        (var "go" @@ (Core.bindingTerm $ var "b"))
        (Core.bindingType $ var "b")) $
      Core.let_
        (Lists.map (var "mapBinding") (Core.letBindings $ var "lt"))
        (var "go" @@ (Core.letBody $ var "lt"))) $
    "forMap" <~ ("m" ~>
      "forPair" <~ ("p" ~> pair (var "go" @@ (Pairs.first $ var "p")) (var "go" @@ (Pairs.second $ var "p"))) $
      Maps.fromList $ Lists.map (var "forPair") $ Maps.toList $ var "m") $
    cases _Term (var "t") Nothing [
      _Term_annotated>>: "at" ~> Core.termAnnotated $ Core.annotatedTerm
        (var "go" @@ (Core.annotatedTermBody $ var "at"))
        (Core.annotatedTermAnnotation $ var "at"),
      _Term_application>>: "a" ~> Core.termApplication $ Core.application
        (var "go" @@ (Core.applicationFunction $ var "a"))
        (var "go" @@ (Core.applicationArgument $ var "a")),
      _Term_either>>: "e" ~> Core.termEither $ Eithers.either_
        ("l" ~> left $ var "go" @@ var "l")
        ("r" ~> right $ var "go" @@ var "r")
        (var "e"),
      _Term_function>>: "fun" ~> Core.termFunction $ var "forFunction" @@ var "fun",
      _Term_let>>: "lt" ~> Core.termLet $ var "forLet" @@ var "lt",
      _Term_list>>: "els" ~> Core.termList $ Lists.map (var "go") (var "els"),
      _Term_literal>>: "v" ~> Core.termLiteral $ var "v",
      _Term_map>>: "m" ~> Core.termMap $ var "forMap" @@ var "m",
      _Term_maybe>>: "m" ~> Core.termMaybe $ Maybes.map (var "go") (var "m"),
      _Term_pair>>: "p" ~> Core.termPair $ pair
        (var "go" @@ (Pairs.first $ var "p"))
        (var "go" @@ (Pairs.second $ var "p")),
      _Term_record>>: "r" ~> Core.termRecord $ Core.record
        (Core.recordTypeName $ var "r")
        (Lists.map (var "forField") (Core.recordFields $ var "r")),
      _Term_set>>: "s" ~> Core.termSet $ Sets.fromList $ Lists.map (var "go") $ Sets.toList (var "s"),
      _Term_typeApplication>>: "tt" ~>
        "body1" <~ var "go" @@ (Core.typeApplicationTermBody $ var "tt") $
        var "push" @@ var "body1" @@ (Core.typeApplicationTermType $ var "tt"),
      _Term_typeLambda>>: "ta" ~> Core.termTypeLambda $ Core.typeLambda
        (Core.typeLambdaParameter $ var "ta")
        (var "go" @@ (Core.typeLambdaBody $ var "ta")),
      _Term_union>>: "i" ~> Core.termUnion $ Core.injection
        (Core.injectionTypeName $ var "i")
        (var "forField" @@ (Core.injectionField $ var "i")),
      _Term_unit>>: constant Core.termUnit,
      _Term_variable>>: "v" ~> Core.termVariable $ var "v",
      _Term_wrap>>: "wt" ~> Core.termWrap $ Core.wrappedTerm
        (Core.wrappedTermTypeName $ var "wt")
        (var "go" @@ (Core.wrappedTermBody $ var "wt"))])] $
  var "go" @@ var "term"

dataGraphToDefinitions :: TTermDefinition (LanguageConstraints -> Bool -> Bool -> Bool -> Bool -> [Binding] -> Graph -> [Namespace] -> Context -> Prelude.Either String (Graph, [[TermDefinition]]))
dataGraphToDefinitions = define "dataGraphToDefinitions" $
  doc ("Given a data graph along with language constraints, original ordered bindings, and a designated list of namespaces,"
    <> " adapt the graph to the language constraints,"
    <> " then return the processed graph along with term definitions grouped by namespace (in the order of the input namespaces)."
    <> " Inference is performed before adaptation if bindings lack type annotations."
    <> " Hoisting must preserve type schemes; if any binding loses its type scheme after hoisting, the pipeline fails."
    <> " Adaptation preserves type application/lambda wrappers and adapts embedded types."
    <> " Post-adaptation inference is performed to ensure binding TypeSchemes are fully consistent."
    <> " The doExpand flag controls eta expansion."
    <> " The doHoistCaseStatements flag controls case statement hoisting (needed for Python)."
    <> " The doHoistPolymorphicLetBindings flag controls polymorphic let binding hoisting (needed for Java)."
    <> " The originalBindings parameter provides the original ordered bindings (from module elements).") $
  "constraints" ~>
  "doInfer" ~> "doExpand" ~> "doHoistCaseStatements" ~> "doHoistPolymorphicLetBindings" ~>
  "originalBindings" ~> "graph0" ~> "namespaces" ~> "cx" ~>

  "namespacesSet" <~ Sets.fromList (var "namespaces") $

  "isParentBinding" <~ ("b" ~> optCases (Names.namespaceOf @@ (Core.bindingName $ var "b"))
    false
    ("ns" ~> Sets.member (var "ns") (var "namespacesSet"))) $

  -- Steps 0a-2: Case statement hoisting pipeline (only for Python target, currently).
  -- 0a: Strip type lambdas so case expressions are visible to the hoister
  --     (the hoister does not traverse into type lambdas).
  -- 0b: Unshadow variables to prevent capture issues after hoisting.
  -- 1:  Hoist case statements before inference.
  -- 2:  Unshadow again after hoisting (hoisting introduces new lambda wrappers).
  "hoistCases" <~ ("bindings" ~>
    -- 0a: Strip type lambdas from bindings
    "stripped" <~ Lists.map ("b" ~>
        Core.binding (Core.bindingName $ var "b")
          (Strip.stripTypeLambdas @@ (Core.bindingTerm $ var "b"))
          (Core.bindingType $ var "b"))
        (var "bindings") $
    -- 0b: Unshadow variables
    "term0" <~ Core.termLet (Core.let_ (var "stripped") Core.termUnit) $
    "unshadowed0" <~ Environment.termAsBindings @@ (Variables.unshadowVariables @@ var "term0") $
    -- 1: Hoist case statements
    "hoisted" <~ Hoisting.hoistCaseStatementsInGraph @@ var "unshadowed0" $
    -- 2: Unshadow again after hoisting
    "term1" <~ Core.termLet (Core.let_ (var "hoisted") Core.termUnit) $
    Environment.termAsBindings @@ (Variables.unshadowVariables @@ var "term1")) $

  "hoistPoly" <~ ("bindings" ~>
    "letBefore" <~ Core.let_ (var "bindings") Core.termUnit $
    "letAfter" <~ Hoisting.hoistPolymorphicLetBindings @@ var "isParentBinding" @@ var "letBefore" $
    Core.letBindings $ var "letAfter") $

  -- Note: this is a rough test of typedness, as it only checks that the top-level bindings are typed.
  "checkBindingsTyped" <~ ("debugLabel" ~> "bindings" ~>
    "untypedBindings" <~ Lists.map ("b" ~> Core.unName (Core.bindingName $ var "b"))
      (Lists.filter ("b" ~> Logic.not $ Maybes.isJust (Core.bindingType $ var "b")) (var "bindings")) $
    Logic.ifElse (Lists.null $ var "untypedBindings")
      (right $ var "bindings")
      (left $ Strings.concat [
        string "Found untyped bindings (", var "debugLabel", string "): ",
        Strings.intercalate (string ", ") (var "untypedBindings")])) $

  -- Normalize: push type applications inward past applications and lambdas.
  -- This corrects structures where TypeApp wraps App/Lambda after adaptation and eta expansion.
  "normalizeBindings" <~ ("bindings" ~>
    Lists.map ("b" ~> Core.binding
      (Core.bindingName $ var "b")
      (pushTypeAppsInward @@ (Core.bindingTerm $ var "b"))
      (Core.bindingType $ var "b"))
    (var "bindings")) $

  -- Helper to rebuild a Graph from bindings, reusing graph0's context
  "rebuildGraph" <~ ("bindings" ~>
    "g" <~ (Lexical.buildGraph @@ var "bindings" @@ Maps.empty @@ Graph.graphPrimitives (var "graph0")) $
    Graph.graphWithSchemaTypes (var "g") (Graph.graphSchemaTypes (var "graph0"))) $

  -- Step 1: hoist case statements if needed (currently, for the Python target)
  -- Use original ordered bindings instead of graphToBindings (which loses order)
  "bins0" <~ var "originalBindings" $
  "bins1" <~ Logic.ifElse (var "doHoistCaseStatements")
    (var "hoistCases" @@ var "bins0")
    (var "bins0") $

  -- Step 2: infer types if necessary
  -- inferGraphTypes now takes ordered bindings and returns (Graph, [Binding])
  "bins2" <<~ Logic.ifElse (var "doInfer")
     (Eithers.map ("result" ~> Pairs.second (Pairs.first (var "result")))
       (Eithers.bimap formatError ("x" ~> var "x")
         (Inference.inferGraphTypes @@ var "cx" @@ var "bins1" @@ (var "rebuildGraph" @@ var "bins1"))))
     (var "checkBindingsTyped" @@ string "after case hoisting" @@ var "bins1") $

  -- Step 3: hoist let bindings if necessary (currently, for the Java target)
  "bins3" <<~ Logic.ifElse (var "doHoistPolymorphicLetBindings")
    (var "checkBindingsTyped" @@ string "after let hoisting"
      @@ (var "hoistPoly" @@ var "bins2"))
    (right $ var "bins2") $

  -- Step 4: adapt the graph (includes eta expansion if enabled).
  -- Adaptation preserves type application/lambda wrappers and adapts embedded types
  -- (literal types, lambda domains, TypeSchemes).
  -- Pass ordered bins3 to adaptDataGraph so it uses them instead of graphToBindings.
  -- adaptDataGraph returns (Graph, [Binding]) preserving binding order.
  "adaptResult" <<~ adaptDataGraph @@ var "constraints" @@ var "doExpand" @@ var "bins3" @@ var "cx" @@ (var "rebuildGraph" @@ var "bins3") $
  "adapted" <~ Pairs.first (var "adaptResult") $
  "adaptedBindings" <~ Pairs.second (var "adaptResult") $
  "bins4" <<~ var "checkBindingsTyped" @@ (string "after adaptation") @@ var "adaptedBindings" $

  -- Step 5: normalize bindings
  "bins5" <~ var "normalizeBindings" @@ var "bins4" $

  -- Construct term definitions grouped by namespace
  "toDef" <~ ("el" ~>
    Maybes.map
      ("ts" ~> Packaging.termDefinition
        (Core.bindingName $ var "el")
        (Core.bindingTerm $ var "el")
        (just $ var "ts"))
      (Core.bindingType $ var "el")) $
  -- Filter to elements in the requested namespaces
  "selectedElements" <~ Lists.filter
    ("el" ~> optCases (Names.namespaceOf @@ (Core.bindingName $ var "el"))
      false
      ("ns" ~> Sets.member (var "ns") (var "namespacesSet")))
    (var "bins5") $
  -- Group elements by namespace
  "elementsByNamespace" <~ Lists.foldl
    ("acc" ~> "el" ~>
      optCases (Names.namespaceOf @@ (Core.bindingName $ var "el"))
        (var "acc")
        ("ns" ~>
          "existing" <~ Maybes.maybe (list ([] :: [TTerm Binding])) (unaryFunction Equality.identity) (Maps.lookup (var "ns") (var "acc")) $
          Maps.insert (var "ns") (Lists.concat2 (var "existing") (list [var "el"])) (var "acc")))
    Maps.empty
    (var "selectedElements") $
  -- Produce definitions in the order of the input namespaces
  "defsGrouped" <~ Lists.map
    ("ns" ~>
      "elsForNs" <~ Maybes.maybe (list ([] :: [TTerm Binding])) (unaryFunction Equality.identity) (Maps.lookup (var "ns") (var "elementsByNamespace")) $
      Maybes.cat (Lists.map (var "toDef") (var "elsForNs")))
    (var "namespaces") $

  "g" <~ (Lexical.buildGraph @@ var "bins5" @@ Maps.empty @@ Graph.graphPrimitives (var "adapted")) $
  right $ pair
    (Graph.graphWithSchemaTypes (var "g") (Graph.graphSchemaTypes (var "adapted")))
    (var "defsGrouped")

literalTypeSupported :: TTermDefinition (LanguageConstraints -> LiteralType -> Bool)
literalTypeSupported = define "literalTypeSupported" $
  doc "Check if a literal type is supported by the given language constraints" $
  "constraints" ~> "lt" ~>
  "forType" <~ ("lt" ~> cases _LiteralType (var "lt")
    (Just true) [
      _LiteralType_float>>: "ft" ~> Sets.member (var "ft") (Coders.languageConstraintsFloatTypes $ var "constraints"),
      _LiteralType_integer>>: "it" ~> Sets.member (var "it") (Coders.languageConstraintsIntegerTypes $ var "constraints")]) $
  Logic.ifElse
    (Sets.member
      (Reflect.literalTypeVariant @@ var "lt")
      (Coders.languageConstraintsLiteralVariants $ var "constraints"))
    (var "forType" @@ var "lt")
    false

schemaGraphToDefinitions :: TTermDefinition (LanguageConstraints -> Graph -> [[Name]] -> Context -> Prelude.Either String (M.Map Name Type, [[TypeDefinition]]))
schemaGraphToDefinitions = define "schemaGraphToDefinitions" $
  doc ("Given a schema graph along with language constraints and a designated list of element names,"
    <> " adapt the graph to the language constraints,"
    <> " then return a corresponding type definition for each element name.") $
  "constraints" ~> "graph" ~> "nameLists" ~> "cx" ~>
  "litmap" <~ adaptLiteralTypesMap @@ var "constraints" $
  "tmap0" <<~ Eithers.bimap formatDecodingError ("x" ~> var "x") (Environment.graphAsTypes @@ var "cx" @@ var "graph" @@ (Lexical.graphToBindings @@ var "graph")) $
  "tmap1" <<~ adaptGraphSchema @@ var "constraints" @@ var "litmap" @@ var "tmap0" $
  "toDef" <~ ("pair" ~> Packaging.typeDefinition (Pairs.first $ var "pair") (Core.typeScheme (list ([] :: [TTerm Name])) (Pairs.second $ var "pair") nothing)) $
  right $ pair
    (var "tmap1")
    (Lists.map
      ("names" ~> Lists.map (var "toDef") $
        Lists.map ("n" ~> pair (var "n") (Maybes.fromJust $ Maps.lookup (var "n") (var "tmap1"))) (var "names"))
      (var "nameLists"))

termAlternatives :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either String [Term])
termAlternatives = define "termAlternatives" $
  doc "Find a list of alternatives for a given term, if any" $
  "cx" ~> "graph" ~> "term" ~> cases _Term (var "term")
    (Just $ right $ list ([] :: [TTerm Term])) [
    _Term_annotated>>: "at" ~>
      "term2" <~ Core.annotatedTermBody (var "at") $
      right $ list [
        var "term2"], -- TODO: lossy
    _Term_maybe>>: "ot" ~> right $ list [
      Core.termList $ optCases (var "ot")
        (list ([] :: [TTerm Term]))
        ("term2" ~> list [var "term2"])],
    _Term_typeLambda>>: "abs" ~>
      "term2" <~ Core.typeLambdaBody (var "abs") $
      right $ list [var "term2"],
    _Term_typeApplication>>: "ta" ~>
      "term2" <~ Core.typeApplicationTermBody (var "ta") $
      right $ list [var "term2"],
    _Term_union>>: "inj" ~>
      "tname" <~ Core.injectionTypeName (var "inj") $
      "field" <~ Core.injectionField (var "inj") $
      "fname" <~ Core.fieldName (var "field") $
      "fterm" <~ Core.fieldTerm (var "field") $
      "forFieldType" <~ ("ft" ~>
        "ftname" <~ Core.fieldTypeName (var "ft") $
        Core.field (var "fname") $ Core.termMaybe $ Logic.ifElse (Equality.equal (var "ftname") (var "fname"))
          (just $ var "fterm")
          (nothing)) $
      "rt" <<~ Eithers.bimap formatError ("x" ~> var "x") (Resolution.requireUnionType @@ var "cx" @@ var "graph" @@ var "tname") $
      right $ list [
        Core.termRecord $ Core.record (var "tname") (Lists.map (var "forFieldType") (var "rt"))],
    _Term_unit>>: constant $ right $ list [
      Core.termLiteral $ Core.literalBoolean true],
    _Term_wrap>>: "wt" ~>
      "term2" <~ Core.wrappedTermBody (var "wt") $
      right $ list [
         var "term2"]]

typeAlternatives :: TTermDefinition (Type -> [Type])
typeAlternatives = define "typeAlternatives" $
  doc "Find a list of alternatives for a given type, if any" $
  "type" ~> cases _Type (var "type")
    (Just $ list ([] :: [TTerm Type])) [
    _Type_annotated>>: "at" ~>
      "type2" <~ Core.annotatedTypeBody (var "at") $
       list [var "type2"], -- TODO: lossy
    _Type_maybe>>: "ot" ~> list [
      Core.typeList $ var "ot"],
    _Type_union>>: "rt" ~>
      "toOptField" <~ ("f" ~> Core.fieldType (Core.fieldTypeName $ var "f") (MetaTypes.optional $ Core.fieldTypeType $ var "f")) $
      "optFields" <~ Lists.map (var "toOptField") (var "rt") $
      list [
        Core.typeRecord (var "optFields")],
    _Type_unit>>: constant $ list [
      Core.typeLiteral $ Core.literalTypeBoolean],
    _Type_void>>: constant $ list [
      Core.typeUnit]]

adaptTypeForLanguage :: TTermDefinition (Language -> Type -> Prelude.Either String Type)
adaptTypeForLanguage = define "adaptTypeForLanguage" $
  doc "Adapt a type using the constraints of a given language" $
  "lang" ~> "typ" ~>
  "constraints" <~ Coders.languageConstraints (var "lang") $
  "litmap" <~ adaptLiteralTypesMap @@ var "constraints" $
  adaptType @@ var "constraints" @@ var "litmap" @@ var "typ"

adaptTermForLanguage :: TTermDefinition (Language -> Context -> Graph -> Term -> Prelude.Either String Term)
adaptTermForLanguage = define "adaptTermForLanguage" $
  doc "Adapt a term using the constraints of a given language" $
  "lang" ~> "cx" ~> "g" ~> "term" ~>
  "constraints" <~ Coders.languageConstraints (var "lang") $
  "litmap" <~ adaptLiteralTypesMap @@ var "constraints" $
  adaptTerm @@ var "constraints" @@ var "litmap" @@ var "cx" @@ var "g" @@ var "term"

composeCoders :: TTermDefinition (Coder a b -> Coder b c -> Coder a c)
composeCoders = define "composeCoders" $
  doc "Compose two coders into a single coder" $
  "c1" ~> "c2" ~>
  Coders.coder
    ("cx" ~> "a" ~>
      "b1" <<~ Coders.coderEncode (var "c1") @@ var "cx" @@ var "a" $
      Coders.coderEncode (var "c2") @@ var "cx" @@ var "b1")
    ("cx" ~> "c" ~>
      "b2" <<~ Coders.coderDecode (var "c2") @@ var "cx" @@ var "c" $
      Coders.coderDecode (var "c1") @@ var "cx" @@ var "b2")

simpleLanguageAdapter :: TTermDefinition (Language -> Context -> Graph -> Type -> Prelude.Either String (Adapter Type Type Term Term))
simpleLanguageAdapter = define "simpleLanguageAdapter" $
  doc "Given a target language and a source type, produce an adapter which rewrites the type and its terms according to the language's constraints. The encode direction adapts terms; the decode direction is identity." $
  "lang" ~> "cx" ~> "g" ~> "typ" ~>
  "constraints" <~ Coders.languageConstraints (var "lang") $
  "litmap" <~ adaptLiteralTypesMap @@ var "constraints" $
  "adaptedType" <<~ adaptType @@ var "constraints" @@ var "litmap" @@ var "typ" $
  right $ Coders.adapter
    false
    (var "typ")
    (var "adaptedType")
    (Coders.coder
      ("cx" ~> "term" ~>
        Eithers.bimap ("_s" ~> Ctx.inContext (Error.errorOther $ Error.otherError $ var "_s") (var "cx")) ("_x" ~> var "_x")
          (adaptTerm @@ var "constraints" @@ var "litmap" @@ var "cx" @@ var "g" @@ var "term"))
      ("cx" ~> "term" ~> right $ var "term"))


--------------------------------------------------------------------------------
-- Type preparation functions
-- TODO: these functions duplicate logic already in adaptFloatType, adaptIntegerType,
-- adaptLiteralType, etc. above. They differ in that they return a triple of
-- (adapted type, term transformer, diagnostic messages) rather than just the adapted type.
-- They should be simplified or eliminated in a future refactoring pass.
--------------------------------------------------------------------------------

-- | Prepare a literal type, substituting unsupported types.
-- Returns (adapted literal type, literal value transformer, diagnostic messages).
prepareLiteralType :: TTermDefinition (LiteralType -> (LiteralType, Literal -> Literal, S.Set String))
prepareLiteralType = define "prepareLiteralType" $
  doc "Prepare a literal type, substituting unsupported types" $
  lambda "at" $
    (cases _LiteralType (var "at") (Just (prepareSame @@ var "at")) [
      _LiteralType_binary>>: (constant $
        triple
          (Core.literalTypeString)
          ("v" ~> cases _Literal (var "v") (Just (var "v")) [
            _Literal_binary>>: ("b" ~> inject _Literal _Literal_string (Literals.binaryToString (var "b")))])
          (Sets.fromList $ list [string "replace binary strings with character strings"])),
      _LiteralType_float>>: ("ft" ~> lets [
        "result">: prepareFloatType @@ var "ft",
        "rtyp">: Pairs.first (var "result"),
        "rep">: Pairs.first (Pairs.second (var "result")),
        "msgs">: Pairs.second (Pairs.second (var "result"))] $
        triple
          (Core.literalTypeFloat (var "rtyp"))
          ("v" ~> cases _Literal (var "v") (Just (var "v")) [
            _Literal_float>>: ("fv" ~> inject _Literal _Literal_float (var "rep" @@ var "fv"))])
          (var "msgs")),
      _LiteralType_integer>>: ("it" ~> lets [
        "result">: prepareIntegerType @@ var "it",
        "rtyp">: Pairs.first (var "result"),
        "rep">: Pairs.first (Pairs.second (var "result")),
        "msgs">: Pairs.second (Pairs.second (var "result"))] $
        triple
          (Core.literalTypeInteger (var "rtyp"))
          ("v" ~> cases _Literal (var "v") (Just (var "v")) [
            _Literal_integer>>: ("iv" ~> inject _Literal _Literal_integer (var "rep" @@ var "iv"))])
          (var "msgs"))])

-- | Prepare a float type, substituting unsupported types.
prepareFloatType :: TTermDefinition (FloatType -> (FloatType, FloatValue -> FloatValue, S.Set String))
prepareFloatType = define "prepareFloatType" $
  doc "Prepare a float type, substituting unsupported types" $
  lambda "ft" $
    (cases _FloatType (var "ft") (Just (prepareSame @@ var "ft")) [
      _FloatType_bigfloat>>: (constant $
        triple
          Core.floatTypeFloat64
          ("v" ~> cases _FloatValue (var "v") (Just (var "v")) [
            _FloatValue_bigfloat>>: ("d" ~> inject _FloatValue _FloatValue_float64 (Literals.bigfloatToFloat64 (var "d")))])
          (Sets.fromList $ list [string "replace arbitrary-precision floating-point numbers with 64-bit floating-point numbers (doubles)"]))])

-- | Prepare an integer type, substituting unsupported types.
prepareIntegerType :: TTermDefinition (IntegerType -> (IntegerType, IntegerValue -> IntegerValue, S.Set String))
prepareIntegerType = define "prepareIntegerType" $
  doc "Prepare an integer type, substituting unsupported types" $
  lambda "it" $
    (cases _IntegerType (var "it") (Just (prepareSame @@ var "it")) [
      _IntegerType_bigint>>: (constant $
        triple
          Core.integerTypeInt64
          ("v" ~> cases _IntegerValue (var "v") (Just (var "v")) [
            _IntegerValue_bigint>>: ("i" ~> inject _IntegerValue _IntegerValue_int64 (Literals.bigintToInt64 (var "i")))])
          (Sets.fromList $ list [string "replace arbitrary-precision integers with 64-bit integers"])),
      _IntegerType_uint8>>: (constant $
        triple
          Core.integerTypeInt8
          ("v" ~> cases _IntegerValue (var "v") (Just (var "v")) [
            _IntegerValue_uint8>>: ("i" ~> inject _IntegerValue _IntegerValue_int8 (Literals.bigintToInt8 (Literals.uint8ToBigint (var "i"))))])
          (Sets.fromList $ list [string "replace unsigned 8-bit integers with signed 8-bit integers"])),
      _IntegerType_uint32>>: (constant $
        triple
          Core.integerTypeInt32
          ("v" ~> cases _IntegerValue (var "v") (Just (var "v")) [
            _IntegerValue_uint32>>: ("i" ~> inject _IntegerValue _IntegerValue_int32 (Literals.bigintToInt32 (Literals.uint32ToBigint (var "i"))))])
          (Sets.fromList $ list [string "replace unsigned 32-bit integers with signed 32-bit integers"])),
      _IntegerType_uint64>>: (constant $
        triple
          Core.integerTypeInt64
          ("v" ~> cases _IntegerValue (var "v") (Just (var "v")) [
            _IntegerValue_uint64>>: ("i" ~> inject _IntegerValue _IntegerValue_int64 (Literals.bigintToInt64 (Literals.uint64ToBigint (var "i"))))])
          (Sets.fromList $ list [string "replace unsigned 64-bit integers with signed 64-bit integers"]))])

-- | Prepare a type, substituting unsupported literal types.
prepareType :: TTermDefinition (Graph -> Type -> (Type, Term -> Term, S.Set String))
prepareType = define "prepareType" $
  doc "Prepare a type, substituting unsupported literal types" $
  lambda "cx" $ lambda "typ" $
    (cases _Type (Strip.deannotateType @@ var "typ") (Just (prepareSame @@ var "typ")) [
      _Type_literal>>: ("at" ~> lets [
        "result">: prepareLiteralType @@ var "at",
        "rtyp">: Pairs.first (var "result"),
        "rep">: Pairs.first (Pairs.second (var "result")),
        "msgs">: Pairs.second (Pairs.second (var "result"))] $
        triple
          (MetaTypes.literal (var "rtyp"))
          ("v" ~> cases _Term (var "v") (Just (var "v")) [
            _Term_literal>>: ("av" ~> inject _Term _Term_literal (var "rep" @@ var "av"))])
          (var "msgs"))])

-- | Return a value unchanged with identity transform and no messages.
prepareSame :: TTermDefinition (a -> (a, b -> b, S.Set c))
prepareSame = define "prepareSame" $
  doc "Return a value unchanged with identity transform and no messages" $
  lambda "x" $
    triple (var "x") ("y" ~> var "y") (Sets.empty)
