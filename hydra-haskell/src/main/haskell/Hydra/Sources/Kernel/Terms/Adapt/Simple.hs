{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Adapt.Simple where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Decode.Core as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Inference as Inference
import qualified Hydra.Sources.Kernel.Terms.Literals as Lits
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Reduction as Reduction
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Variants as Variants


module_ :: Module
module_ = Module (Namespace "hydra.adapt.simple") elements
    [DecodeCore.module_, Inference.module_, Lits.module_, Monads.module_, Reduction.module_, Rewriting.module_, Schemas.module_,
      ShowCore.module_, ShowGraph.module_, Variants.module_]
    kernelTypesModules $
    Just "Simple, one-way adapters for types and terms"
  where
    elements = [
      el adaptFloatTypeDef,
      el adaptDataGraphDef,
      el adaptGraphSchemaDef,
      el adaptIntegerTypeDef,
      el adaptLiteralDef,
      el adaptLiteralTypeDef,
      el adaptLiteralTypesMapDef,
      el adaptLiteralValueDef,
      el adaptPrimitiveDef,
      el adaptTermDef,
      el adaptTypeDef,
      el adaptTypeSchemeDef,
      el dataGraphToDefinitionsDef,
      el literalTypeSupportedDef,
      el schemaGraphToDefinitionsDef,
      el termAlternativesDef,
      el typeAlternativesDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

adaptFloatTypeDef :: TBinding (LanguageConstraints -> FloatType -> Maybe FloatType)
adaptFloatTypeDef = define "adaptFloatType" $
  doc "Attempt to adapt a floating-point type using the given language constraints" $
  "constraints" ~> "ft" ~>
  "supported" <~ Sets.member (var "ft") (Coders.languageConstraintsFloatTypes $ var "constraints") $
  "alt" <~ (ref adaptFloatTypeDef @@ var "constraints") $
  Logic.ifElse (var "supported")
    (just $ var "ft")
    (cases _FloatType (var "ft")
      Nothing [
      _FloatType_bigfloat>>: constant nothing,
      _FloatType_float32>>: constant $ var "alt" @@ Core.floatTypeFloat64,
      _FloatType_float64>>: constant $ var "alt" @@ Core.floatTypeBigfloat])

adaptDataGraphDef :: TBinding (LanguageConstraints -> Bool -> Graph -> Flow s Graph)
adaptDataGraphDef = define "adaptDataGraph" $
  doc "Adapt a graph and its schema to the given language constraints, prior to inference" $
  "constraints" ~> "doExpand" ~> "graph0" ~>
  "litmap" <~ ref adaptLiteralTypesMapDef @@ var "constraints" $
  "els0" <~ Graph.graphElements (var "graph0") $
  "env0" <~ Graph.graphEnvironment (var "graph0") $
  "body0" <~ Graph.graphBody (var "graph0") $
  "prims0" <~ Graph.graphPrimitives (var "graph0") $
  "schema0" <~ Graph.graphSchema (var "graph0") $
  "schema1" <<~ optCases (var "schema0")
    (produce nothing)
--    ("sg" ~> Flows.fail $ "schema graph: " ++ (ref ShowGraph.graphDef @@ var "sg")) $
    ( "sg" ~>
      "tmap0" <<~ ref Schemas.graphAsTypesDef @@ var "sg" $
      "tmap1" <<~ ref adaptGraphSchemaDef @@ var "constraints" @@ var "litmap" @@ var "tmap0" $
      "emap" <~ ref Schemas.typesToElementsDef @@ var "tmap1" $
      produce $ just $ Graph.graphWithElements (var "sg") (var "emap")) $
  "gterm0" <~ ref Schemas.graphAsTermDef @@ var "graph0" $
  "gterm1" <~ Logic.ifElse (var "doExpand")
    (ref Reduction.expandLambdasDef @@ var "graph0" @@ var "gterm0")
    (var "gterm0") $
  "gterm2" <<~ ref adaptTermDef @@ var "constraints" @@ var "litmap" @@ var "gterm1" $
  "els1" <~ ref Schemas.termAsGraphDef @@ var "gterm2" $
  "prims1" <<~ Flows.mapElems (ref adaptPrimitiveDef @@ var "constraints" @@ var "litmap") (var "prims0") $
--  Flows.fail $ "adapted data graph: " ++ (ref ShowCore.termDef @@ var "gterm2")
--  Flows.fail $ "schema graph: " ++ (optCases (var "schema1")
--    ("none")
--    ("sg" ~> ref ShowGraph.graphDef @@ var "sg"))
  produce $ Graph.graph
    (var "els1")
    (var "env0")
    Maps.empty
    Core.termUnit
    (var "prims1")
    (var "schema1")

adaptGraphSchemaDef :: TBinding (LanguageConstraints -> M.Map LiteralType LiteralType -> M.Map Name Type -> Flow s (M.Map Name Type))
adaptGraphSchemaDef = define "adaptGraphSchema" $
  doc "Adapt a schema graph to the given language constraints" $
  "constraints" ~> "litmap" ~> "types0" ~>
  "mapPair" <~ ("pair" ~>
    "name" <~ first (var "pair") $
    "typ" <~ second (var "pair") $
    "typ1" <<~ ref adaptTypeDef @@ var "constraints" @@ var "litmap" @@ var "typ" $
    produce $ pair (var "name") (var "typ1")) $
  "pairs" <<~ Flows.mapList (var "mapPair") (Maps.toList $ var "types0") $
  produce $ Maps.fromList (var "pairs")

adaptIntegerTypeDef :: TBinding (LanguageConstraints -> IntegerType -> Maybe IntegerType)
adaptIntegerTypeDef = define "adaptIntegerType" $
  doc "Attempt to adapt an integer type using the given language constraints" $
  "constraints" ~> "it" ~>
  "supported" <~ Sets.member (var "it") (Coders.languageConstraintsIntegerTypes $ var "constraints") $
  "alt" <~ (ref adaptIntegerTypeDef @@ var "constraints") $
  Logic.ifElse (var "supported")
    (just $ var "it")
    (cases _IntegerType (var "it")
      Nothing [
      _IntegerType_bigint>>: constant nothing,
      _IntegerType_int8>>: constant $ var "alt" @@ Core.integerTypeUint16,
      _IntegerType_int16>>: constant $ var "alt" @@ Core.integerTypeUint32,
      _IntegerType_int32>>: constant $ var "alt" @@ Core.integerTypeUint64,
      _IntegerType_int64>>: constant $ var "alt" @@ Core.integerTypeBigint,
      _IntegerType_uint8>>: constant $ var "alt" @@ Core.integerTypeInt16,
      _IntegerType_uint16>>: constant $ var "alt" @@ Core.integerTypeInt32,
      _IntegerType_uint32>>: constant $ var "alt" @@ Core.integerTypeInt64,
      _IntegerType_uint64>>: constant $ var "alt" @@ Core.integerTypeBigint])

adaptLiteralDef :: TBinding (LiteralType -> Literal -> Literal)
adaptLiteralDef = define "adaptLiteral" $
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
        ref Lits.bigintToIntegerValueDef @@ var "it" @@ Logic.ifElse (var "b") (bigint 1) (bigint 0)],
    _Literal_float>>: "f" ~> cases _LiteralType (var "lt")
      Nothing [
      _LiteralType_float>>: "ft" ~> Core.literalFloat $
        ref Lits.bigfloatToFloatValueDef @@ var "ft" @@ (ref Lits.floatValueToBigfloatDef @@ var "f")],
    _Literal_integer>>: "i" ~> cases _LiteralType (var "lt")
      Nothing [
      _LiteralType_integer>>: "it" ~> Core.literalInteger $
        ref Lits.bigintToIntegerValueDef @@ var "it" @@ (ref Lits.integerValueToBigintDef @@ var "i")]]

adaptLiteralTypeDef :: TBinding (LanguageConstraints -> LiteralType -> Maybe LiteralType)
adaptLiteralTypeDef = define "adaptLiteralType" $
  doc "Attempt to adapt a literal type using the given language constraints" $
  "constraints" ~> "lt" ~>
  Logic.ifElse (ref literalTypeSupportedDef @@ var "constraints" @@ var "lt")
    nothing
    (cases _LiteralType (var "lt")
      (Just nothing) [
      _LiteralType_binary>>: constant $ just Core.literalTypeString,
      _LiteralType_boolean>>: constant $ Optionals.map (unaryFunction Core.literalTypeInteger) $
          ref adaptIntegerTypeDef @@ var "constraints" @@ Core.integerTypeInt8,
      _LiteralType_float>>: "ft" ~> Optionals.map (unaryFunction Core.literalTypeFloat) $
        ref adaptFloatTypeDef @@ var "constraints" @@ var "ft",
      _LiteralType_integer>>: "it" ~> Optionals.map (unaryFunction Core.literalTypeInteger) $
        ref adaptIntegerTypeDef @@ var "constraints" @@ var "it"])

adaptLiteralTypesMapDef :: TBinding (LanguageConstraints -> M.Map LiteralType LiteralType)
adaptLiteralTypesMapDef = define "adaptLiteralTypesMap" $
  doc "Derive a map of adapted literal types for the given language constraints" $
  "constraints" ~>
  "tryType" <~ ("lt" ~> optCases (ref adaptLiteralTypeDef @@ var "constraints" @@ var "lt")
    nothing
    ("lt2" ~> just $ pair (var "lt") (var "lt2"))) $
  Maps.fromList $ Optionals.cat $ Lists.map (var "tryType") (ref Variants.literalTypesDef)

adaptLiteralValueDef :: TBinding (M.Map LiteralType LiteralType -> LiteralType -> Literal -> Literal)
adaptLiteralValueDef = define "adaptLiteralValue" $
  doc "Adapt a literal value using the given language constraints" $
  "litmap" ~> "lt" ~> "l" ~> optCases (Maps.lookup (var "lt") (var "litmap"))
    (Core.literalString $ ref ShowCore.literalDef @@ var "l")
    ("lt2" ~> ref adaptLiteralDef @@ var "lt2" @@ var "l")

adaptPrimitiveDef :: TBinding (LanguageConstraints -> M.Map LiteralType LiteralType -> Primitive -> Flow s Primitive)
adaptPrimitiveDef = define "adaptPrimitive" $
  doc "Adapt a primitive to the given language constraints, prior to inference" $
  "constraints" ~> "litmap" ~> "prim0" ~>
  "ts0" <~ Graph.primitiveType (var "prim0") $
  "ts1" <<~ ref adaptTypeSchemeDef @@ var "constraints" @@ var "litmap" @@ var "ts0" $
  produce $ Graph.primitiveWithType (var "prim0") (var "ts1")

-- Note: this function could be made more efficient through precomputation of alternatives,
--       similar to what is done for literals.
adaptTermDef :: TBinding (LanguageConstraints -> M.Map LiteralType LiteralType -> Term -> Flow Graph Term)
adaptTermDef = define "adaptTerm" $
  doc "Adapt a term using the given language constraints" $
  "constraints" ~> "litmap" ~> "term0" ~>
  "rewrite" <~ ("recurse" ~> "term0" ~>
    "term1" <<~ var "recurse" @@ var "term0" $
    "tryTerm" <~ ("term" ~>
      "supportedVariant" <~ Sets.member
        (ref Variants.termVariantDef @@ var "term")
        (Coders.languageConstraintsTermVariants $ var "constraints") $
      Logic.ifElse (var "supportedVariant")
        (cases _Term (var "term")
          (Just $ produce $ just $ var "term") [
          _Term_literal>>: "l" ~>
            "lt" <~ ref Variants.literalTypeDef @@ var "l" $
            produce $ just $ Logic.ifElse (ref literalTypeSupportedDef @@ var "constraints" @@ var "lt")
              (var "term")
              (Core.termLiteral $ ref adaptLiteralValueDef @@ var "litmap" @@ var "lt" @@ var "l")])
        ("tryAlts" <~ ("alts" ~> Logic.ifElse (Lists.null $ var "alts")
          (produce nothing)
          ( "mterm" <<~ var "tryTerm" @@ Lists.head (var "alts") $
            optCases (var "mterm")
              (var "tryAlts" @@ Lists.tail (var "alts"))
              ("t" ~> produce $ just $ var "t"))) $
           "alts" <<~ ref termAlternativesDef @@ var "term1" $
           var "tryAlts" @@ var "alts")) $
    "mterm" <<~ var "tryTerm" @@ var "term1" $
    optCases (var "mterm")
      (Flows.fail $ "no alternatives for term: " ++ (ref ShowCore.termDef @@ var "term1"))
      ("term2" ~> produce $ var "term2")) $
  ref Rewriting.rewriteTermMDef @@ var "rewrite" @@ var "term0"

adaptTypeDef :: TBinding (LanguageConstraints -> M.Map LiteralType LiteralType -> Type -> Flow s Type)
adaptTypeDef = define "adaptType" $
  doc "Adapt a type using the given language constraints" $
  "constraints" ~> "litmap" ~> "type0" ~>
  "rewrite" <~ ("recurse" ~> "typ" ~>
    "type1" <<~ var "recurse" @@ var "typ" $
    "tryType" <~ ("typ" ~>
      "supportedVariant" <~ Sets.member
        (ref Variants.typeVariantDef @@ var "typ")
        (Coders.languageConstraintsTypeVariants $ var "constraints") $
      Logic.ifElse (var "supportedVariant")
        (cases _Type (var "typ")
          (Just $ just $ var "typ") [
          _Type_literal>>: "lt" ~> Logic.ifElse (ref literalTypeSupportedDef @@ var "constraints" @@ var "lt")
            (just $ var "typ")
            (optCases (Maps.lookup (var "lt") (var "litmap"))
              (just $ Core.typeLiteral Core.literalTypeString)
              ("lt2" ~> just $ Core.typeLiteral $ var "lt2"))])
        ("tryAlts" <~ ("alts" ~> Logic.ifElse (Lists.null $ var "alts")
          nothing
          ( optCases (var "tryType" @@ Lists.head (var "alts"))
              (var "tryAlts" @@ Lists.tail (var "alts"))
              ("t" ~> just $ var "t"))) $
           "alts" <~ ref typeAlternativesDef @@ var "type1" $
           var "tryAlts" @@ var "alts")) $
    optCases (var "tryType" @@ var "type1")
      (Flows.fail $ "no alternatives for type: " ++ (ref ShowCore.typeDef @@ var "typ"))
      ("type2" ~> produce $ var "type2")) $
  ref Rewriting.rewriteTypeMDef @@ var "rewrite" @@ var "type0"

adaptTypeSchemeDef :: TBinding (LanguageConstraints -> M.Map LiteralType LiteralType -> TypeScheme -> Flow s TypeScheme)
adaptTypeSchemeDef = define "adaptTypeScheme" $
  doc "Adapt a type scheme to the given language constraints, prior to inference" $
  "constraints" ~> "litmap" ~> "ts0" ~>
  "vars0" <~ Core.typeSchemeVariables (var "ts0") $
  "t0" <~ Core.typeSchemeType (var "ts0") $
  "t1" <<~ ref adaptTypeDef @@ var "constraints" @@ var "litmap" @@ var "t0" $
  produce $ Core.typeScheme (var "vars0") (var "t1")

dataGraphToDefinitionsDef :: TBinding (LanguageConstraints -> Bool -> Graph -> [[Name]] -> Flow s (Graph, [[TermDefinition]]))
dataGraphToDefinitionsDef = define "dataGraphToDefinitions" $
  doc ("Given a data graph along with language constraints and a designated list of element names,"
    <> " adapt the graph to the language constraints, perform inference,"
    <> " then return a corresponding term definition for each element name.") $
  "constraints" ~> "doExpand" ~> "graph" ~> "nameLists" ~>

  -- This extra, early inference step is necessary so that elements are annotated with correct types,
  -- as needed for lambda expansion.
  "graphi" <<~ Logic.ifElse (var "doExpand")
    (ref Inference.inferGraphTypesDef @@ var "graph")
    (produce $ var "graph") $
--  "graphi" <~ var "graph" $

  "graph1" <<~ ref adaptDataGraphDef @@ var "constraints" @@ var "doExpand" @@ var "graphi" $
--  "graph1" <<~ ref adaptDataGraphDef @@ var "constraints" @@ false @@ var "graphi" $

--  Flows.fail ("adapted graph: " ++ (ref ShowGraph.graphDef @@ var "graph1"))
  "graph2" <<~ ref Inference.inferGraphTypesDef @@ var "graph1" $
  "toDef" <~ ("el" ~>
    "ts" <~ Optionals.fromJust (Core.bindingType $ var "el") $
    Module.termDefinition
      (Core.bindingName $ var "el")
      (Core.bindingTerm $ var "el")
      (ref Schemas.typeSchemeToFTypeDef @@ var "ts")) $
  produce $ pair
    (var "graph2")
    (Lists.map
      ("names" ~> Lists.map (var "toDef") $
        Lists.map ("n" ~> Optionals.fromJust $ Maps.lookup (var"n") (Graph.graphElements $ var "graph2")) (var "names"))
      (var "nameLists"))

literalTypeSupportedDef :: TBinding (LanguageConstraints -> LiteralType -> Bool)
literalTypeSupportedDef = define "literalTypeSupported" $
  doc "Check if a literal type is supported by the given language constraints" $
  "constraints" ~> "lt" ~>
  Logic.ifElse
    (Sets.member
      (ref Variants.literalTypeVariantDef @@ var "lt")
      (Coders.languageConstraintsLiteralVariants $ var "constraints"))
    (cases _LiteralType (var "lt")
      (Just true) [
        _LiteralType_float>>: "ft" ~> Sets.member (var "ft") (Coders.languageConstraintsFloatTypes $ var "constraints"),
        _LiteralType_integer>>: "it" ~> Sets.member (var "it") (Coders.languageConstraintsIntegerTypes $ var "constraints")])
    false

schemaGraphToDefinitionsDef :: TBinding (LanguageConstraints -> Graph -> [[Name]] -> Flow s (M.Map Name Type, [[TypeDefinition]]))
schemaGraphToDefinitionsDef = define "schemaGraphToDefinitions" $
  doc ("Given a schema graph along with language constraints and a designated list of element names,"
    <> " adapt the graph to the language constraints,"
    <> " then return a corresponding type definition for each element name.") $
  "constraints" ~> "graph" ~> "nameLists" ~>
  "litmap" <~ ref adaptLiteralTypesMapDef @@ var "constraints" $
  "tmap0" <<~ ref Schemas.graphAsTypesDef @@ var "graph" $
  "tmap1" <<~ ref adaptGraphSchemaDef @@ var "constraints" @@ var "litmap" @@ var "tmap0" $
  "toDef" <~ ("pair" ~> Module.typeDefinition (first $ var "pair") (second $ var "pair")) $
  produce $ pair
    (var "tmap1")
    (Lists.map
      ("names" ~> Lists.map (var "toDef") $
        Lists.map ("n" ~> pair (var "n") (Optionals.fromJust $ Maps.lookup (var "n") (var "tmap1"))) (var "names"))
      (var "nameLists"))

termAlternativesDef :: TBinding (Term -> Flow Graph [Term])
termAlternativesDef = define "termAlternatives" $
  doc "Find a list of alternatives for a given term, if any" $
  "term" ~> cases _Term (var "term")
    (Just $ produce $ list []) [
    _Term_annotated>>: "at" ~>
      "term2" <~ Core.annotatedTermSubject (var "at") $
      produce $ list [
        var "term2"], -- TODO: lossy
    _Term_optional>>: "ot" ~> produce $ list [
      Core.termList $ optCases (var "ot")
        (list [])
        ("term2" ~> list [var "term2"])],
    -- Note: no type abstractions or type applications, as we are not expecting System F terms here
    _Term_union>>: "inj" ~>
      "tname" <~ Core.injectionTypeName (var "inj") $
      "field" <~ Core.injectionField (var "inj") $
      "fname" <~ Core.fieldName (var "field") $
      "fterm" <~ Core.fieldTerm (var "field") $
      "rt" <<~ ref Schemas.requireUnionTypeDef @@ var "tname" $
      produce $ list [
        "forFieldType" <~ ("ft" ~>
          "ftname" <~ Core.fieldTypeName (var "ft") $
          Core.field (var "fname") $ Core.termOptional $ Logic.ifElse (Equality.equal (var "ftname") (var "fname"))
            (just $ var "fterm")
            (nothing)) $
        "fields" <~ Lists.map (var "forFieldType") (Core.rowTypeFields $ var "rt") $
        Core.termRecord $ Core.record (var "tname") (var "fields")],
    _Term_unit>>: constant $ produce $ list [
      Core.termLiteral $ Core.literalBoolean true],
    _Term_wrap>>: "wt" ~>
      "term2" <~ Core.wrappedTermObject (var "wt") $
      produce $ list [
         var "term2"]]

typeAlternativesDef :: TBinding (Type -> [Type])
typeAlternativesDef = define "typeAlternatives" $
  doc "Find a list of alternatives for a given type, if any" $
  "type" ~> cases _Type (var "type")
    (Just $ list []) [
    _Type_annotated>>: "at" ~>
      "type2" <~ Core.annotatedTypeSubject (var "at") $
       list [var "type2"], -- TODO: lossy
    _Type_optional>>: "ot" ~> list [
      Core.typeList $ var "ot"],
    _Type_union>>: "rt" ~>
      "tname" <~ Core.rowTypeTypeName (var "rt") $
      "fields" <~ Core.rowTypeFields (var "rt") $
      list [Core.typeRecord $ Core.rowType (var "tname") (var "fields")],
    _Type_unit>>: constant $ list [
      Core.typeLiteral $ Core.literalTypeBoolean]]
