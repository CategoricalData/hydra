{-# LANGUAGE OverloadedStrings #-}

module Hydra.Ext.Sources.Pg.Validation where

-- Standard imports for term-level sources outside of the kernel
--import Hydra.Kernel
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
import qualified Hydra.Dsl.Meta.Testing       as Testing
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
import qualified Hydra.Sources.Kernel.Terms.Decode.Core     as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Encode.Core     as EncodeCore
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


import Hydra.Kernel hiding (Edge(..), _Edge, _Edge_in, _Edge_out, Element(..), _Element, Graph(..), _Graph)
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes

import Hydra.Pg.Model as PG
import Hydra.Ext.Sources.Pg.Model


validationDefinition :: String -> TTerm a -> TBinding a
validationDefinition = definitionInModule pgValidationModule

pgValidationModule :: Module
pgValidationModule = Module (Namespace "hydra.pg.validation") elements
    []
    [pgModelModule] $
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

validateEdgeDef :: TBinding (
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
          @@ (Equality.equal
            (unwrap _EdgeLabel @@ var "actual")
            (unwrap _EdgeLabel @@ var "expected"))
          @@ (var "failWith" @@ (ref prependDef @@ "Wrong label" @@ (ref edgeLabelMismatchDef @@ var "expected" @@ var "actual"))),
      "checkId">: Maybes.map
        (var "failWith" <.> (ref prependDef @@ "Invalid id"))
        (var "checkValue" @@ (project _EdgeType _EdgeType_id @@ var "typ") @@ (project _Edge _Edge_id @@ var "el")),
      "checkProperties">: Maybes.map
        (var "failWith" <.> (ref prependDef @@ "Invalid property"))
        (ref validatePropertiesDef
          @@ var "checkValue"
          @@ (project _EdgeType _EdgeType_properties @@ var "typ")
          @@ (project _Edge _Edge_properties @@ var "el")),
      "checkOut">: Maybes.maybe
        nothing
        (lambda "f" $ Maybes.maybe
          (just (var "failWith" @@ (ref prependDef @@ "Out-vertex does not exist" @@ (var "showValue" @@ (project _Edge _Edge_out @@ var "el")))))
          (lambda "label" $ ref verifyDef
            @@ (Equality.equal
              (unwrap _VertexLabel @@ var "label")
              (unwrap _VertexLabel @@ (project _EdgeType _EdgeType_out @@ var "typ")))
            @@ (var "failWith" @@ (ref prependDef @@ "Wrong out-vertex label" @@ (ref vertexLabelMismatchDef @@ (project _EdgeType _EdgeType_out @@ var "typ") @@ var "label"))))
          (var "f" @@ (project _Edge _Edge_out @@ var "el")))
          (var "labelForVertexId"),
      "checkIn">: Maybes.maybe
        nothing
        (lambda "f" $ Maybes.maybe
          (just (var "failWith" @@ (ref prependDef @@ "In-vertex does not exist" @@ (var "showValue" @@ (project _Edge _Edge_in @@ var "el")))))
          (lambda "label" $ ref verifyDef
            @@ (Equality.equal
              (unwrap _VertexLabel @@ var "label")
              (unwrap _VertexLabel @@ (project _EdgeType _EdgeType_in @@ var "typ")))
            @@ (var "failWith" @@ (ref prependDef @@ "Wrong in-vertex label" @@ (ref vertexLabelMismatchDef @@ (project _EdgeType _EdgeType_in @@ var "typ") @@ var "label"))))
          (var "f" @@ (project _Edge _Edge_in @@ var "el")))
        (var "labelForVertexId")]
      $ ref checkAllDef @@ list [var "checkLabel", var "checkId", var "checkProperties", var "checkOut", var "checkIn"]

validateElementDef :: TBinding (
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

validateGraphDef :: TBinding (
     (t -> v -> Maybe String)
  -> (v -> String)
  -> PG.GraphSchema t
  -> PG.Graph v
  -> Y.Maybe String)
validateGraphDef = validationDefinition "validateGraph" $
  withOrds ["t0", "t1"] $
  lambda "checkValue" $ lambda "showValue" $ lambda "schema" $ lambda "graph" $ lets [
    "checkVertices">: lets [
      "checkVertex">: lambda "el" $ Maybes.maybe
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
          @@ (Lists.map (var "checkVertex") $ Maps.elems $ project _Graph _Graph_vertices @@ var "graph"),
    "checkEdges">: lets [
        "checkEdge">: lambda "el" $ Maybes.maybe
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
          Maybes.map (project _Vertex _Vertex_label) (Maps.lookup (var "i") (project _Graph _Graph_vertices @@ var "graph"))]
      $ ref checkAllDef
          @@ (Lists.map (var "checkEdge") $ Maps.elems $ project _Graph _Graph_edges @@ var "graph")]
    $ ref checkAllDef @@ list [var "checkVertices", var "checkEdges"]

validatePropertiesDef :: TBinding (
     (t -> v -> Maybe String)
  -> [PG.PropertyType t]
  -> M.Map PG.PropertyKey v
  -> Y.Maybe String)
validatePropertiesDef = validationDefinition "validateProperties" $
  lambda "checkValue" $ lambda "types" $ lambda "props" $ lets [
    "checkTypes">: ref checkAllDef @@ (Lists.map (var "checkType") (var "types")),
    "checkType">:
      lambda "t" $ Logic.ifElse (project _PropertyType _PropertyType_required @@ var "t")
        (Maybes.maybe
          (just (ref prependDef @@ "Missing value for " @@ (unwrap _PropertyKey @@ (project _PropertyType _PropertyType_key @@ var "t"))))
          (constant nothing)
          (Maps.lookup (project _PropertyType _PropertyType_key @@ var "t") $ var "props"))
        nothing,
    "checkValues">: lets [
      "m">: Maps.fromList (Lists.map
          (lambda "p" $ tuple2
            (project _PropertyType _PropertyType_key @@ var "p")
            (project _PropertyType _PropertyType_value @@ var "p"))
          (var "types")),
      "checkPair">: lambda "pair" $ lets [
        "key">: first $ var "pair",
        "val">: second $ var "pair"]
        $ Maybes.maybe
          (just (ref prependDef @@ "Unexpected key" @@ (unwrap _PropertyKey @@ var "key")))
          (lambda "typ" $ Maybes.map
            (ref prependDef @@ "Invalid value")
            (var "checkValue" @@ var "typ" @@ var "val"))
          (Maps.lookup (var "key") (var "m"))]
      $ ref checkAllDef @@ (Lists.map (var "checkPair") (Maps.toList $ var "props"))]
    $ ref checkAllDef @@ list [var "checkTypes", var "checkValues"]

validateVertexDef :: TBinding (
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
        @@ (Equality.equal
          (unwrap _VertexLabel @@ var "actual")
          (unwrap _VertexLabel @@ var "expected"))
        @@ (var "failWith" @@ (ref prependDef @@ "Wrong label" @@ (ref vertexLabelMismatchDef @@ var "expected" @@ var "actual"))),
    "checkId">: Maybes.map
      (var "failWith" <.> (ref prependDef @@ "Invalid id"))
      (var "checkValue" @@ (project _VertexType _VertexType_id @@ var "typ") @@ (project _Vertex _Vertex_id @@ var "el")),
    "checkProperties">: Maybes.map
      (var "failWith" <.> (ref prependDef @@ "Invalid property"))
      (ref validatePropertiesDef
        @@ var "checkValue"
        @@ (project _VertexType _VertexType_properties @@ var "typ")
        @@ (project _Vertex _Vertex_properties @@ var "el"))]
    $ ref checkAllDef @@ list [var "checkLabel", var "checkId", var "checkProperties"]

----

checkAllDef :: TBinding ([Y.Maybe a] -> Y.Maybe a)
checkAllDef = validationDefinition "checkAll" $
  lambda "checks" $ lets [
    "errors">: Maybes.cat $ var "checks"]
    $ Lists.safeHead $ var "errors"

edgeErrorDef :: TBinding ((v -> String) -> PG.Edge v -> String -> String)
edgeErrorDef = validationDefinition "edgeError" $
  lambda "showValue" $ lambda "e" $
    ref prependDef @@ ("Invalid edge with id " ++ (var "showValue" @@ (project _Edge _Edge_id @@ var "e")))

edgeLabelMismatchDef :: TBinding (PG.EdgeLabel -> PG.EdgeLabel -> String)
edgeLabelMismatchDef = validationDefinition "edgeLabelMismatch" $
  lambda "expected" $ lambda "actual" $
    "expected " ++ (unwrap _EdgeLabel @@ var "expected") ++ ", found " ++ (unwrap _EdgeLabel @@ var "actual")

prependDef :: TBinding (String -> String -> String)
prependDef = validationDefinition "prepend" $
  lambda "prefix" $ lambda "msg" $
    (var "prefix") ++ ": " ++ (var "msg")

verifyDef :: TBinding (Bool -> String -> Maybe String)
verifyDef = validationDefinition "verify" $
  lambda "b" $ lambda "err" $
    Logic.ifElse (var "b")
      nothing
      (just $ var "err")

vertexErrorDef :: TBinding ((v -> String) -> PG.Vertex v -> String -> String)
vertexErrorDef = validationDefinition "vertexError" $
  lambda "showValue" $ lambda "v" $
    ref prependDef @@ ("Invalid vertex with id " ++ (var "showValue" @@ (project _Vertex _Vertex_id @@ var "v")))

vertexLabelMismatchDef :: TBinding (PG.VertexLabel -> PG.VertexLabel -> String)
vertexLabelMismatchDef = validationDefinition "vertexLabelMismatch" $
  lambda "expected" $ lambda "actual" $
    "expected " ++ (unwrap _VertexLabel @@ var "expected") ++ ", found " ++ (unwrap _VertexLabel @@ var "actual")

-- TODO: this is a hack
ordT0 = (M.fromList [(Name "t0", S.fromList [TypeClassOrdering])])
