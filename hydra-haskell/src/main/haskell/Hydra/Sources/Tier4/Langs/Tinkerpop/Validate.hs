{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier4.Langs.Tinkerpop.Validate where

import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y
import           Hydra.Dsl.Base            as Base
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types

import Hydra.Kernel hiding (Edge(..), _Edge, _Edge_in, _Edge_out, Element(..), _Element, Graph(..), _Graph)

import Hydra.Langs.Tinkerpop.PropertyGraph as PG
import Hydra.Sources.Tier4.Langs.Tinkerpop.PropertyGraph


validateDefinition :: String -> Datum a -> Definition a
validateDefinition = definitionInModule tinkerpopValidateModule

tinkerpopValidateModule :: Module Kv
tinkerpopValidateModule = Module (Namespace "hydra/langs/tinkerpop/validate") elements [] [tinkerpopPropertyGraphModule] $
    Just "Utilities for validating property graphs against property graph schemas"
  where
   elements = [
     el validateEdgeDef,
     el validateElementDef,
     el validateGraphDef,
     el validatePropertiesDef,
     el validateVertexDef,
     ----
     el checkAllDef,
     el edgeErrorDef,
     el edgeLabelMismatchDef,
     el prependDef,
     el verifyDef,
     el vertexErrorDef,
     el vertexLabelMismatchDef]

validateEdgeDef :: Definition (
     (t -> v -> Maybe String)
  -> (v -> String)
  -> Y.Maybe (v -> Y.Maybe PG.VertexLabel)
  -> PG.EdgeType t
  -> PG.Edge v
  -> Y.Maybe String)
validateEdgeDef = validateDefinition "validateEdge" $
  functionN [
    functionT tT (functionT vT (optionalT stringT)),
    functionT vT stringT,
    optionalT (functionT vT (optionalT vertexLabelT)),
    edgeTypeTT,
    edgeVT,
    optionalT stringT] $
  lambda "checkValue" $ lambda "showValue" $ lambda "labelForVertexId" $ lambda "typ" $ lambda "el" $
    (ref checkAllDef @@ list [var "checkLabel", var "checkId", var "checkProperties", var "checkOut", var "checkIn"]
    `with` [
      "failWith">: typed (functionT stringT stringT) $
        ref edgeErrorDef @@ var "showValue" @@ var "el",
      "checkLabel">: (ref verifyDef
        @@ (Equality.equalString
          @@ (unwrap _EdgeLabel @@ var "actual")
          @@ (unwrap _EdgeLabel @@ var "expected"))
        @@ (var "failWith" @@ (ref prependDef @@ "Wrong label" @@ (ref edgeLabelMismatchDef @@ var "expected" @@ var "actual"))))
        `with` [
          "expected">: project _EdgeType _EdgeType_label @@ var "typ",
          "actual">: project _Edge _Edge_label @@ var "el"],
      "checkId">: Optionals.map
        @@ (var "failWith" <.> (ref prependDef @@ "Invalid id"))
        @@ (var "checkValue" @@ (project _EdgeType _EdgeType_id @@ var "typ") @@ (project _Edge _Edge_id @@ var "el")),
      "checkProperties">: Optionals.map
        @@ (var "failWith" <.> (ref prependDef @@ "Invalid property"))
        @@ (ref validatePropertiesDef
          @@ var "checkValue"
          @@ (project _EdgeType _EdgeType_properties @@ var "typ")
          @@ (project _Edge _Edge_properties @@ var "el")),
      "checkOut">: ifOpt (var "labelForVertexId")
        nothing
        (lambda "f" $ ifOpt (var "f" @@ (project _Edge _Edge_out @@ var "el"))
          (just (var "failWith" @@ (ref prependDef @@ "Out-vertex does not exist" @@ (var "showValue" @@ (project _Edge _Edge_out @@ var "el")))))
          (lambda "label" $ ref verifyDef
            @@ (Equality.equalString
              @@ (unwrap _VertexLabel @@ var "label")
              @@ (unwrap _VertexLabel @@ (project _EdgeType _EdgeType_out @@ var "typ")))
            @@ (var "failWith" @@ (ref prependDef @@ "Wrong out-vertex label" @@ (ref vertexLabelMismatchDef @@ (project _EdgeType _EdgeType_out @@ var "typ") @@ var "label"))))),
      "checkIn">: ifOpt (var "labelForVertexId")
        nothing
        (lambda "f" $ ifOpt (var "f" @@ (project _Edge _Edge_in @@ var "el"))
          (just (var "failWith" @@ (ref prependDef @@ "In-vertex does not exist" @@ (var "showValue" @@ (project _Edge _Edge_in @@ var "el")))))
          (lambda "label" $ ref verifyDef
            @@ (Equality.equalString
              @@ (unwrap _VertexLabel @@ var "label")
              @@ (unwrap _VertexLabel @@ (project _EdgeType _EdgeType_in @@ var "typ")))
            @@ (var "failWith" @@ (ref prependDef @@ "Wrong in-vertex label" @@ (ref vertexLabelMismatchDef @@ (project _EdgeType _EdgeType_in @@ var "typ") @@ var "label")))))])

validateElementDef :: Definition (
     (t -> v -> Maybe String)
  -> (v -> String)
  -> Y.Maybe (v -> Y.Maybe PG.VertexLabel)
  -> PG.ElementType t
  -> PG.Element v
  -> Y.Maybe String)
validateElementDef = validateDefinition "validateElement" $
  functionN [
    functionT tT (functionT vT (optionalT stringT)),
    functionT vT stringT,
    optionalT (functionT vT (optionalT vertexLabelT)),
    elementTypeTT,
    elementVT,
    optionalT stringT] $
  lambda "checkValue" $ lambda "showValue" $ lambda "labelForVertexId" $ lambda "typ" $ lambda "el" $
    (match _ElementType Nothing [
        _ElementType_vertex>>: lambda "vt" $ (match _Element Nothing [
            _Element_edge>>: lambda "e" $ just (ref prependDef @@ "Edge instead of vertex" @@ (var "showValue" @@ (project _Edge _Edge_id @@ var "e"))),
            _Element_vertex>>: lambda "vertex" $ ref validateVertexDef
              @@ var "checkValue"
              @@ var "showValue"
              @@ var "vt"
              @@ var "vertex"]) @@ var "el",
--        _ElementType_edge>>: constant nothing]) @@ var "typ"
        _ElementType_edge>>: lambda "et" $ (match _Element Nothing [
            _Element_vertex>>: lambda "v" $ just (ref prependDef @@ "Vertex instead of edge" @@ (var "showValue" @@ (project _Vertex _Vertex_id @@ var "v"))),
            _Element_edge>>: lambda "edge" $ ref validateEdgeDef
              @@ var "checkValue"
              @@ var "showValue"
              @@ var "labelForVertexId"
              @@ var "et"
              @@ var "edge"]) @@ var "el"]) @@ var "typ"

validateGraphDef :: Definition (
     (t -> v -> Maybe String)
  -> (v -> String)
  -> PG.GraphSchema t
  -> PG.Graph v
  -> Y.Maybe String)
validateGraphDef = validateDefinition "validateGraph" $
  functionNWithClasses [
    functionT tT (functionT vT (optionalT stringT)),
    functionT vT stringT,
    graphSchemaTT,
    graphVT,
    optionalT stringT] ordV $
  lambda "checkValue" $ lambda "showValue" $ lambda "schema" $ lambda "graph" $
    (ref checkAllDef @@ list [var "checkVertices", var "checkEdges"])
    `with` [
      "checkVertices">: (ref checkAllDef
          @@ (Lists.map @@ var "checkVertex" @@ (Maps.values @@ (project _Graph _Graph_vertices @@ var "graph"))))
        `with` [
          "checkVertex">: lambda "el" $ ifOpt (Maps.lookup
              @@ (project _Vertex _Vertex_label @@ var "el")
              @@ (project _GraphSchema _GraphSchema_vertices @@ var "schema"))
            (just (ref vertexErrorDef @@ var "showValue" @@ var "el"
              @@ (ref prependDef @@ "Unexpected label" @@ (unwrap _VertexLabel @@ (project _Vertex _Vertex_label @@ var "el")))))
            (lambda "t" $ ref validateVertexDef
              @@ var "checkValue"
              @@ var "showValue"
              @@ var "t"
              @@ var "el")],
      "checkEdges">: (ref checkAllDef
          @@ (Lists.map @@ var "checkEdge" @@ (Maps.values @@ (project _Graph _Graph_edges @@ var "graph"))))
        `with` [
          "checkEdge">: lambda "el" $ ifOpt (Maps.lookup
              @@ (project _Edge _Edge_label @@ var "el")
              @@ (project _GraphSchema _GraphSchema_edges @@ var "schema"))
            (just (ref edgeErrorDef @@ var "showValue" @@ var "el"
              @@ (ref prependDef @@ "Unexpected label" @@ (unwrap _EdgeLabel @@ (project _Edge _Edge_label @@ var "el")))))
            (lambda "t" $ ref validateEdgeDef
              @@ var "checkValue"
              @@ var "showValue"
              @@ var "labelForVertexId"
              @@ var "t"
              @@ var "el"),
          "labelForVertexId">: typed (optionalT (functionT vT (optionalT vertexLabelT))) $
            just $ lambda "i" $ Optionals.map @@ (project _Vertex _Vertex_label) @@ (Maps.lookup @@ var "i" @@ (project _Graph _Graph_vertices @@ var "graph"))]]

validatePropertiesDef :: Definition (
     (t -> v -> Maybe String)
  -> [PG.PropertyType t]
  -> M.Map PG.PropertyKey v
  -> Y.Maybe String)
validatePropertiesDef = validateDefinition "validateProperties" $
  functionN [
    functionT tT (functionT vT (optionalT stringT)),
    listT propertyTypeTT,
    mapT propertyKeyT vT,
    optionalT stringT] $
  lambda "checkValue" $ lambda "types" $ lambda "props" $
    ((ref checkAllDef @@ list [var "checkTypes", var "checkValues"])
    `with` [
      "checkTypes">: ref checkAllDef @@ (Lists.map @@ var "checkType" @@ var "types"),
      "checkType">:
        typed (functionT propertyTypeTT $ optionalT stringT) $
        lambda "t" $ ifElse (project _PropertyType _PropertyType_required @@ var "t")
          (ifOpt (Maps.lookup @@ (project _PropertyType _PropertyType_key @@ var "t") @@ var "props")
            (just (ref prependDef @@ "Missing value for " @@ (unwrap _PropertyKey @@ (project _PropertyType _PropertyType_key @@ var "t"))))
            (constant nothing))
          nothing,
      "checkValues">: ((ref checkAllDef @@ (Lists.map @@ var "checkPair" @@ (Maps.toList @@ var "props")))
        `with` [
          "m">: typed (mapT propertyKeyT tT) $
            Maps.fromList @@ (Lists.map
              @@ (lambda "p" $ pair (
                project _PropertyType _PropertyType_key @@ var "p",
                project _PropertyType _PropertyType_value @@ var "p"))
              @@ var "types"),
          "checkPair">: lambda "pair" $ ((ifOpt (Maps.lookup @@ var "key" @@ var "m")
              (just (ref prependDef @@ "Unexpected key" @@ (unwrap _PropertyKey @@ var "key")))
              (lambda "typ" $ Optionals.map
                @@ (ref prependDef @@ "Invalid value")
                @@ (var "checkValue" @@ var "typ" @@ var "val")))
            `with` [
              "key">: first @@ var "pair",
              "val">: second @@ var "pair"])])])

validateVertexDef :: Definition (
     (t -> v -> Maybe String)
  -> (v -> String)
  -> PG.VertexType t
  -> PG.Vertex v
  -> Y.Maybe String)
validateVertexDef = validateDefinition "validateVertex" $
  functionN [
    functionT tT (functionT vT (optionalT stringT)),
    functionT vT stringT,
    vertexTypeTT,
    vertexVT,
    optionalT stringT] $
  lambda "checkValue" $ lambda "showValue" $ lambda "typ" $ lambda "el" $
    ((ref checkAllDef @@ list [var "checkLabel", var "checkId", var "checkProperties"])
    `with` [
      "failWith">: typed (functionT stringT stringT) $
        ref vertexErrorDef @@ var "showValue" @@ var "el",
      "checkLabel">: (ref verifyDef
        @@ (Equality.equalString
          @@ (unwrap _VertexLabel @@ var "actual")
          @@ (unwrap _VertexLabel @@ var "expected"))
        @@ (var "failWith" @@ (ref prependDef @@ "Wrong label" @@ (ref vertexLabelMismatchDef @@ var "expected" @@ var "actual"))))
        `with` [
          "expected">: project _VertexType _VertexType_label @@ var "typ",
          "actual">: project _Vertex _Vertex_label @@ var "el"],
      "checkId">: Optionals.map
        @@ (var "failWith" <.> (ref prependDef @@ "Invalid id"))
        @@ (var "checkValue" @@ (project _VertexType _VertexType_id @@ var "typ") @@ (project _Vertex _Vertex_id @@ var "el")),
      "checkProperties">: Optionals.map
        @@ (var "failWith" <.> (ref prependDef @@ "Invalid property"))
        @@ (ref validatePropertiesDef
          @@ var "checkValue"
          @@ (project _VertexType _VertexType_properties @@ var "typ")
          @@ (project _Vertex _Vertex_properties @@ var "el"))])

----

checkAllDef :: Definition ([Y.Maybe a] -> Y.Maybe a)
checkAllDef = validateDefinition "checkAll" $
  function (listT $ optionalT aT) (optionalT aT) $
  lambda "checks" (
    (ifElse (Lists.null @@ var "errors")
      nothing
      (just (Lists.head @@ var "errors")))
    `with` [
      "errors">: Optionals.cat @@ var "checks"])

edgeErrorDef :: Definition ((v -> String) -> PG.Edge v -> String -> String)
edgeErrorDef = validateDefinition "edgeError" $
  functionN [functionT vT stringT, edgeVT, stringT, stringT] $
  lambda "showValue" $ lambda "e" $
    ref prependDef @@ ("Invalid edge with id " ++ (var "showValue" @@ (project _Edge _Edge_id @@ var "e")))

edgeLabelMismatchDef :: Definition (PG.EdgeLabel -> PG.EdgeLabel -> String)
edgeLabelMismatchDef = validateDefinition "edgeLabelMismatch" $
  functionN [edgeLabelT, edgeLabelT, stringT] $
  lambda "expected" $ lambda "actual" $
    "expected " ++ (unwrap _EdgeLabel @@ var "expected") ++ ", found " ++ (unwrap _EdgeLabel @@ var "actual")

prependDef :: Definition (String -> String -> String)
prependDef = validateDefinition "prepend" $
  functionN [stringT, stringT, stringT] $
  lambda "prefix" $ lambda "msg" $
    (var "prefix") ++ ": " ++ (var "msg")

verifyDef :: Definition (Bool -> String -> Maybe String)
verifyDef = validateDefinition "verify" $
  functionN [booleanT, stringT, optionalT stringT] $
  lambda "b" $ lambda "err" $
    ifElse (var "b")
      nothing
      (just $ var "err")

vertexErrorDef :: Definition ((v -> String) -> PG.Vertex v -> String -> String)
vertexErrorDef = validateDefinition "vertexError" $
  functionN [functionT vT stringT, vertexVT, stringT, stringT] $
  lambda "showValue" $ lambda "v" $
    ref prependDef @@ ("Invalid vertex with id " ++ (var "showValue" @@ (project _Vertex _Vertex_id @@ var "v")))

vertexLabelMismatchDef :: Definition (PG.VertexLabel -> PG.VertexLabel -> String)
vertexLabelMismatchDef = validateDefinition "vertexLabelMismatch" $
  functionN [vertexLabelT, vertexLabelT, stringT] $
  lambda "expected" $ lambda "actual" $
    "expected " ++ (unwrap _VertexLabel @@ var "expected") ++ ", found " ++ (unwrap _VertexLabel @@ var "actual")

ordV = (M.fromList [(Name "v", S.fromList [TypeClassOrdering])])

edgeLabelT = TypeVariable _EdgeLabel
edgeTypeTT = Types.apply (TypeVariable _EdgeType) tT
edgeVT = Types.apply (TypeVariable _Edge) vT
elementTypeTT = Types.apply (TypeVariable _ElementType) tT
elementVT = Types.apply (TypeVariable _Element) vT
graphSchemaTT = Types.apply (TypeVariable _GraphSchema) tT
graphVT = Types.apply (TypeVariable _Graph) vT
propertyKeyT = TypeVariable _PropertyKey
propertyTypeTT = Types.apply (TypeVariable _PropertyType) tT
vertexLabelT = TypeVariable _VertexLabel
vertexTypeTT = Types.apply (TypeVariable _VertexType) tT
vertexVT = Types.apply (TypeVariable _Vertex) vT
tT = Types.var "t"
vT = Types.var "v"
