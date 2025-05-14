{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier3.Ext.Pg.Validation where

import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y
import           Hydra.Dsl.Phantoms            as Base
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Chars       as Chars
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

import Hydra.Pg.Model as PG
import Hydra.Sources.Tier3.Ext.Pg.Model
import Hydra.Sources.Libraries


validationDefinition :: String -> TTerm a -> TElement a
validationDefinition = definitionInModule pgValidationModule

pgValidationModule :: Module
pgValidationModule = Module (Namespace "hydra.pg.validation") elements
    [] [pgModelModule] $
    Just "Utilities for validating property graphs against property graph schemas"
  where
   elements = [
     el validateEdgeDef,
     el validateElementDef,
     el validateGraphDef,
     el validatePropertiesDef,
     el validateVertexDef,
     --
     el checkAllDef,
     el edgeErrorDef,
     el edgeLabelMismatchDef,
     el prependDef,
     el verifyDef,
     el vertexErrorDef,
     el vertexLabelMismatchDef]

validateEdgeDef :: TElement (
     (t -> v -> Maybe String)
  -> (v -> String)
  -> Y.Maybe (v -> Y.Maybe PG.VertexLabel)
  -> PG.EdgeType t
  -> PG.Edge v
  -> Y.Maybe String)
validateEdgeDef = validationDefinition "validateEdge" $
  lambda "checkValue" $ lambda "showValue" $ lambda "labelForVertexId" $ lambda "typ" $ lambda "el" $
    lets [
      "failWith">: ref edgeErrorDef @@ var "showValue" @@ var "el",
      "checkLabel">: lets [
        "expected">: project _EdgeType _EdgeType_label @@ var "typ",
        "actual">: project _Edge _Edge_label @@ var "el"]
        $ ref verifyDef
          @@ (Equality.equalString
            (unwrap _EdgeLabel @@ var "actual")
            (unwrap _EdgeLabel @@ var "expected"))
          @@ (var "failWith" @@ (ref prependDef @@ "Wrong label" @@ (ref edgeLabelMismatchDef @@ var "expected" @@ var "actual"))),
      "checkId">: Optionals.map
        (var "failWith" <.> (ref prependDef @@ "Invalid id"))
        (var "checkValue" @@ (project _EdgeType _EdgeType_id @@ var "typ") @@ (project _Edge _Edge_id @@ var "el")),
      "checkProperties">: Optionals.map
        (var "failWith" <.> (ref prependDef @@ "Invalid property"))
        (ref validatePropertiesDef
          @@ var "checkValue"
          @@ (project _EdgeType _EdgeType_properties @@ var "typ")
          @@ (project _Edge _Edge_properties @@ var "el")),
      "checkOut">: Optionals.maybe
        nothing
        (lambda "f" $ Optionals.maybe
          (just (var "failWith" @@ (ref prependDef @@ "Out-vertex does not exist" @@ (var "showValue" @@ (project _Edge _Edge_out @@ var "el")))))
          (lambda "label" $ ref verifyDef
            @@ (Equality.equalString
              (unwrap _VertexLabel @@ var "label")
              (unwrap _VertexLabel @@ (project _EdgeType _EdgeType_out @@ var "typ")))
            @@ (var "failWith" @@ (ref prependDef @@ "Wrong out-vertex label" @@ (ref vertexLabelMismatchDef @@ (project _EdgeType _EdgeType_out @@ var "typ") @@ var "label"))))
          (var "f" @@ (project _Edge _Edge_out @@ var "el")))
          (var "labelForVertexId"),
      "checkIn">: Optionals.maybe
        nothing
        (lambda "f" $ Optionals.maybe
          (just (var "failWith" @@ (ref prependDef @@ "In-vertex does not exist" @@ (var "showValue" @@ (project _Edge _Edge_in @@ var "el")))))
          (lambda "label" $ ref verifyDef
            @@ (Equality.equalString
              (unwrap _VertexLabel @@ var "label")
              (unwrap _VertexLabel @@ (project _EdgeType _EdgeType_in @@ var "typ")))
            @@ (var "failWith" @@ (ref prependDef @@ "Wrong in-vertex label" @@ (ref vertexLabelMismatchDef @@ (project _EdgeType _EdgeType_in @@ var "typ") @@ var "label"))))
          (var "f" @@ (project _Edge _Edge_in @@ var "el")))
        (var "labelForVertexId")]
      $ ref checkAllDef @@ list [var "checkLabel", var "checkId", var "checkProperties", var "checkOut", var "checkIn"]

validateElementDef :: TElement (
     (t -> v -> Maybe String)
  -> (v -> String)
  -> Y.Maybe (v -> Y.Maybe PG.VertexLabel)
  -> PG.ElementType t
  -> PG.Element v
  -> Y.Maybe String)
validateElementDef = validationDefinition "validateElement" $
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

validateGraphDef :: TElement (
     (t -> v -> Maybe String)
  -> (v -> String)
  -> PG.GraphSchema t
  -> PG.Graph v
  -> Y.Maybe String)
validateGraphDef = validationDefinition "validateGraph" $
  withTypeClasses ordT0 $
  lambda "checkValue" $ lambda "showValue" $ lambda "schema" $ lambda "graph" $ lets [
    "checkVertices">: lets [
      "checkVertex">: lambda "el" $ Optionals.maybe
        (just (ref vertexErrorDef @@ var "showValue" @@ var "el"
          @@ (ref prependDef @@ "Unexpected label" @@ (unwrap _VertexLabel @@ (project _Vertex _Vertex_label @@ var "el")))))
        (lambda "t" $ ref validateVertexDef
          @@ var "checkValue"
          @@ var "showValue"
          @@ var "t"
          @@ var "el")
        (Maps.lookup
          (project _Vertex _Vertex_label @@ var "el")
          (project _GraphSchema _GraphSchema_vertices @@ var "schema"))]
      $ ref checkAllDef
          @@ (Lists.map (var "checkVertex") $ Maps.values $ project _Graph _Graph_vertices @@ var "graph"),
    "checkEdges">: lets [
        "checkEdge">: lambda "el" $ Optionals.maybe
          (just (ref edgeErrorDef @@ var "showValue" @@ var "el"
            @@ (ref prependDef @@ "Unexpected label" @@ (unwrap _EdgeLabel @@ (project _Edge _Edge_label @@ var "el")))))
          (lambda "t" $ ref validateEdgeDef
            @@ var "checkValue"
            @@ var "showValue"
            @@ var "labelForVertexId"
            @@ var "t"
            @@ var "el")
          (Maps.lookup
            (project _Edge _Edge_label @@ var "el")
            (project _GraphSchema _GraphSchema_edges @@ var "schema")),
        "labelForVertexId">: just $ lambda "i" $
          Optionals.map (project _Vertex _Vertex_label) (Maps.lookup (var "i") (project _Graph _Graph_vertices @@ var "graph"))]
      $ ref checkAllDef
          @@ (Lists.map (var "checkEdge") $ Maps.values $ project _Graph _Graph_edges @@ var "graph")]
    $ ref checkAllDef @@ list [var "checkVertices", var "checkEdges"]

validatePropertiesDef :: TElement (
     (t -> v -> Maybe String)
  -> [PG.PropertyType t]
  -> M.Map PG.PropertyKey v
  -> Y.Maybe String)
validatePropertiesDef = validationDefinition "validateProperties" $
  lambda "checkValue" $ lambda "types" $ lambda "props" $ lets [
    "checkTypes">: ref checkAllDef @@ (Lists.map (var "checkType") (var "types")),
    "checkType">:
      lambda "t" $ Logic.ifElse (project _PropertyType _PropertyType_required @@ var "t")
        (Optionals.maybe
          (just (ref prependDef @@ "Missing value for " @@ (unwrap _PropertyKey @@ (project _PropertyType _PropertyType_key @@ var "t"))))
          (constant nothing)
          (Maps.lookup (project _PropertyType _PropertyType_key @@ var "t") $ var "props"))
        nothing,
    "checkValues">: lets [
      "m">: Maps.fromList (Lists.map
          (lambda "p" $ pair
            (project _PropertyType _PropertyType_key @@ var "p")
            (project _PropertyType _PropertyType_value @@ var "p"))
          (var "types")),
      "checkPair">: lambda "pair" $ lets [
        "key">: first @@ var "pair",
        "val">: second @@ var "pair"]
        $ Optionals.maybe
          (just (ref prependDef @@ "Unexpected key" @@ (unwrap _PropertyKey @@ var "key")))
          (lambda "typ" $ Optionals.map
            (ref prependDef @@ "Invalid value")
            (var "checkValue" @@ var "typ" @@ var "val"))
          (Maps.lookup (var "key") (var "m"))]
      $ ref checkAllDef @@ (Lists.map (var "checkPair") (Maps.toList $ var "props"))]
    $ ref checkAllDef @@ list [var "checkTypes", var "checkValues"]

validateVertexDef :: TElement (
     (t -> v -> Maybe String)
  -> (v -> String)
  -> PG.VertexType t
  -> PG.Vertex v
  -> Y.Maybe String)
validateVertexDef = validationDefinition "validateVertex" $
  lambda "checkValue" $ lambda "showValue" $ lambda "typ" $ lambda "el" $ lets [
    "failWith">: ref vertexErrorDef @@ var "showValue" @@ var "el",
    "checkLabel">: lets [
      "expected">: project _VertexType _VertexType_label @@ var "typ",
      "actual">: project _Vertex _Vertex_label @@ var "el"]
      $ ref verifyDef
        @@ (Equality.equalString
          (unwrap _VertexLabel @@ var "actual")
          (unwrap _VertexLabel @@ var "expected"))
        @@ (var "failWith" @@ (ref prependDef @@ "Wrong label" @@ (ref vertexLabelMismatchDef @@ var "expected" @@ var "actual"))),
    "checkId">: Optionals.map
      (var "failWith" <.> (ref prependDef @@ "Invalid id"))
      (var "checkValue" @@ (project _VertexType _VertexType_id @@ var "typ") @@ (project _Vertex _Vertex_id @@ var "el")),
    "checkProperties">: Optionals.map
      (var "failWith" <.> (ref prependDef @@ "Invalid property"))
      (ref validatePropertiesDef
        @@ var "checkValue"
        @@ (project _VertexType _VertexType_properties @@ var "typ")
        @@ (project _Vertex _Vertex_properties @@ var "el"))]
    $ ref checkAllDef @@ list [var "checkLabel", var "checkId", var "checkProperties"]

----

checkAllDef :: TElement ([Y.Maybe a] -> Y.Maybe a)
checkAllDef = validationDefinition "checkAll" $
  lambda "checks" $ lets [
    "errors">: Optionals.cat $ var "checks"]
    $ Lists.safeHead $ var "errors"

edgeErrorDef :: TElement ((v -> String) -> PG.Edge v -> String -> String)
edgeErrorDef = validationDefinition "edgeError" $
  lambda "showValue" $ lambda "e" $
    ref prependDef @@ ("Invalid edge with id " ++ (var "showValue" @@ (project _Edge _Edge_id @@ var "e")))

edgeLabelMismatchDef :: TElement (PG.EdgeLabel -> PG.EdgeLabel -> String)
edgeLabelMismatchDef = validationDefinition "edgeLabelMismatch" $
  lambda "expected" $ lambda "actual" $
    "expected " ++ (unwrap _EdgeLabel @@ var "expected") ++ ", found " ++ (unwrap _EdgeLabel @@ var "actual")

prependDef :: TElement (String -> String -> String)
prependDef = validationDefinition "prepend" $
  lambda "prefix" $ lambda "msg" $
    (var "prefix") ++ ": " ++ (var "msg")

verifyDef :: TElement (Bool -> String -> Maybe String)
verifyDef = validationDefinition "verify" $
  lambda "b" $ lambda "err" $
    Logic.ifElse (var "b")
      nothing
      (just $ var "err")

vertexErrorDef :: TElement ((v -> String) -> PG.Vertex v -> String -> String)
vertexErrorDef = validationDefinition "vertexError" $
  lambda "showValue" $ lambda "v" $
    ref prependDef @@ ("Invalid vertex with id " ++ (var "showValue" @@ (project _Vertex _Vertex_id @@ var "v")))

vertexLabelMismatchDef :: TElement (PG.VertexLabel -> PG.VertexLabel -> String)
vertexLabelMismatchDef = validationDefinition "vertexLabelMismatch" $
  lambda "expected" $ lambda "actual" $
    "expected " ++ (unwrap _VertexLabel @@ var "expected") ++ ", found " ++ (unwrap _VertexLabel @@ var "actual")

-- TODO: this is a hack
ordT0 = (M.fromList [(Name "t0", S.fromList [TypeClassOrdering])])
