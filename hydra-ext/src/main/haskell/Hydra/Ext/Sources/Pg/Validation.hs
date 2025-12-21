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
import qualified Hydra.Ext.Sources.Pg.Model as PgModel


validationDefinition :: String -> TTerm a -> TBinding a
validationDefinition = definitionInModule pgValidationModule

pgValidationModule :: Module
pgValidationModule = Module (Namespace "hydra.pg.validation") elements
    []
    [PgModel.ns] $
    Just "Utilities for validating property graphs against property graph schemas"
  where
   elements = [
     toBinding validateEdge,
     toBinding validateElement,
     toBinding validateGraph,
     toBinding validateProperties,
     toBinding validateVertex,
     --
     toBinding checkAll,
     toBinding edgeError,
     toBinding edgeLabelMismatch,
     toBinding prepend,
     toBinding verify,
     toBinding vertexError,
     toBinding vertexLabelMismatch]

validateEdge :: TBinding (
     (t -> v -> Maybe String)
  -> (v -> String)
  -> Y.Maybe (v -> Y.Maybe PG.VertexLabel)
  -> PG.EdgeType t
  -> PG.Edge v
  -> Y.Maybe String)
validateEdge = validationDefinition "validateEdge" $
  "checkValue" ~> "showValue" ~> "labelForVertexId" ~> "typ" ~> "el" ~>
    lets [
      "failWith">: edgeError @@ var "showValue" @@ var "el",
      "checkLabel">: lets [
        "expected">: project _EdgeType _EdgeType_label @@ var "typ",
        "actual">: project _Edge _Edge_label @@ var "el"]
        $ verify
          @@ (Equality.equal
            (unwrap _EdgeLabel @@ var "actual")
            (unwrap _EdgeLabel @@ var "expected"))
          @@ (var "failWith" @@ (prepend @@ (string "Wrong label") @@ (edgeLabelMismatch @@ var "expected" @@ var "actual"))),
      "checkId">: Maybes.map
        (var "failWith" <.> (prepend @@ (string "Invalid id")))
        (var "checkValue" @@ (project _EdgeType _EdgeType_id @@ var "typ") @@ (project _Edge _Edge_id @@ var "el")),
      "checkProperties">: Maybes.map
        (var "failWith" <.> (prepend @@ (string "Invalid property")))
        (validateProperties
          @@ var "checkValue"
          @@ (project _EdgeType _EdgeType_properties @@ var "typ")
          @@ (project _Edge _Edge_properties @@ var "el")),
      "checkOut">: Maybes.maybe
        nothing
        ("f" ~> Maybes.maybe
          (just (var "failWith" @@ (prepend @@ (string "Out-vertex does not exist") @@ (var "showValue" @@ (project _Edge _Edge_out @@ var "el")))))
          ("label" ~> verify
            @@ (Equality.equal
              (unwrap _VertexLabel @@ var "label")
              (unwrap _VertexLabel @@ (project _EdgeType _EdgeType_out @@ var "typ")))
            @@ (var "failWith" @@ (prepend @@ (string "Wrong out-vertex label") @@ (vertexLabelMismatch @@ (project _EdgeType _EdgeType_out @@ var "typ") @@ var "label"))))
          (var "f" @@ (project _Edge _Edge_out @@ var "el")))
          (var "labelForVertexId"),
      "checkIn">: Maybes.maybe
        nothing
        ("f" ~> Maybes.maybe
          (just (var "failWith" @@ (prepend @@ (string "In-vertex does not exist") @@ (var "showValue" @@ (project _Edge _Edge_in @@ var "el")))))
          ("label" ~> verify
            @@ (Equality.equal
              (unwrap _VertexLabel @@ var "label")
              (unwrap _VertexLabel @@ (project _EdgeType _EdgeType_in @@ var "typ")))
            @@ (var "failWith" @@ (prepend @@ (string "Wrong in-vertex label") @@ (vertexLabelMismatch @@ (project _EdgeType _EdgeType_in @@ var "typ") @@ var "label"))))
          (var "f" @@ (project _Edge _Edge_in @@ var "el")))
        (var "labelForVertexId")]
      $ checkAll @@ list [var "checkLabel", var "checkId", var "checkProperties", var "checkOut", var "checkIn"]

validateElement :: TBinding (
     (t -> v -> Maybe String)
  -> (v -> String)
  -> Y.Maybe (v -> Y.Maybe PG.VertexLabel)
  -> PG.ElementType t
  -> PG.Element v
  -> Y.Maybe String)
validateElement = validationDefinition "validateElement" $
  "checkValue" ~> "showValue" ~> "labelForVertexId" ~> "typ" ~> "el" ~>
    (match _ElementType Nothing [
        _ElementType_vertex>>: "vt" ~> (match _Element Nothing [
            _Element_edge>>: "e" ~> just (prepend @@ (string "Edge instead of vertex") @@ (var "showValue" @@ (project _Edge _Edge_id @@ var "e"))),
            _Element_vertex>>: "vertex" ~> validateVertex
              @@ var "checkValue"
              @@ var "showValue"
              @@ var "vt"
              @@ var "vertex"]) @@ var "el",
--        _ElementType_edge>>: constant nothing]) @@ var "typ"
        _ElementType_edge>>: "et" ~> (match _Element Nothing [
            _Element_vertex>>: "v" ~> just (prepend @@ (string "Vertex instead of edge") @@ (var "showValue" @@ (project _Vertex _Vertex_id @@ var "v"))),
            _Element_edge>>: "edge" ~> validateEdge
              @@ var "checkValue"
              @@ var "showValue"
              @@ var "labelForVertexId"
              @@ var "et"
              @@ var "edge"]) @@ var "el"]) @@ var "typ"

validateGraph :: TBinding (
     (t -> v -> Maybe String)
  -> (v -> String)
  -> PG.GraphSchema t
  -> PG.Graph v
  -> Y.Maybe String)
validateGraph = validationDefinition "validateGraph" $
  withOrds ["t0", "t1"] $
  "checkValue" ~> "showValue" ~> "schema" ~> "graph" ~> lets [
    "checkVertices">: lets [
      "checkVertex">: "el" ~> Maybes.maybe
        (just (vertexError @@ var "showValue" @@ var "el"
          @@ (prepend @@ (string "Unexpected label") @@ (unwrap _VertexLabel @@ (project _Vertex _Vertex_label @@ var "el")))))
        ("t" ~> validateVertex
          @@ var "checkValue"
          @@ var "showValue"
          @@ var "t"
          @@ var "el")
        (Maps.lookup
          (project _Vertex _Vertex_label @@ var "el")
          (project _GraphSchema _GraphSchema_vertices @@ var "schema"))]
      $ checkAll
          @@ (Lists.map (var "checkVertex") $ Maps.elems $ project _Graph _Graph_vertices @@ var "graph"),
    "checkEdges">: lets [
        "checkEdge">: "el" ~> Maybes.maybe
          (just (edgeError @@ var "showValue" @@ var "el"
            @@ (prepend @@ (string "Unexpected label") @@ (unwrap _EdgeLabel @@ (project _Edge _Edge_label @@ var "el")))))
          ("t" ~> validateEdge
            @@ var "checkValue"
            @@ var "showValue"
            @@ var "labelForVertexId"
            @@ var "t"
            @@ var "el")
          (Maps.lookup
            (project _Edge _Edge_label @@ var "el")
            (project _GraphSchema _GraphSchema_edges @@ var "schema")),
        "labelForVertexId">: just $ "i" ~>
          Maybes.map (project _Vertex _Vertex_label) (Maps.lookup (var "i") (project _Graph _Graph_vertices @@ var "graph"))]
      $ checkAll
          @@ (Lists.map (var "checkEdge") $ Maps.elems $ project _Graph _Graph_edges @@ var "graph")]
    $ checkAll @@ list [var "checkVertices", var "checkEdges"]

validateProperties :: TBinding (
     (t -> v -> Maybe String)
  -> [PG.PropertyType t]
  -> M.Map PG.PropertyKey v
  -> Y.Maybe String)
validateProperties = validationDefinition "validateProperties" $
  "checkValue" ~> "types" ~> "props" ~> lets [
    "checkTypes">: checkAll @@ (Lists.map (var "checkType") (var "types")),
    "checkType">:
      "t" ~> Logic.ifElse (project _PropertyType _PropertyType_required @@ var "t")
        (Maybes.maybe
          (just (prepend @@ (string "Missing value for ") @@ (unwrap _PropertyKey @@ (project _PropertyType _PropertyType_key @@ var "t"))))
          (constant nothing)
          (Maps.lookup (project _PropertyType _PropertyType_key @@ var "t") $ var "props"))
        nothing,
    "checkValues">: lets [
      "m">: Maps.fromList (Lists.map
          ("p" ~> pair
            (project _PropertyType _PropertyType_key @@ var "p")
            (project _PropertyType _PropertyType_value @@ var "p"))
          (var "types")),
      "checkPair">: "pair" ~> lets [
        "key">: Pairs.first $ var "pair",
        "val">: Pairs.second $ var "pair"]
        $ Maybes.maybe
          (just (prepend @@ (string "Unexpected key") @@ (unwrap _PropertyKey @@ var "key")))
          ("typ" ~> Maybes.map
            (prepend @@ (string "Invalid value"))
            (var "checkValue" @@ var "typ" @@ var "val"))
          (Maps.lookup (var "key") (var "m"))]
      $ checkAll @@ (Lists.map (var "checkPair") (Maps.toList $ var "props"))]
    $ checkAll @@ list [var "checkTypes", var "checkValues"]

validateVertex :: TBinding (
     (t -> v -> Maybe String)
  -> (v -> String)
  -> PG.VertexType t
  -> PG.Vertex v
  -> Y.Maybe String)
validateVertex = validationDefinition "validateVertex" $
  "checkValue" ~> "showValue" ~> "typ" ~> "el" ~> lets [
    "failWith">: vertexError @@ var "showValue" @@ var "el",
    "checkLabel">: lets [
      "expected">: project _VertexType _VertexType_label @@ var "typ",
      "actual">: project _Vertex _Vertex_label @@ var "el"]
      $ verify
        @@ (Equality.equal
          (unwrap _VertexLabel @@ var "actual")
          (unwrap _VertexLabel @@ var "expected"))
        @@ (var "failWith" @@ (prepend @@ (string "Wrong label") @@ (vertexLabelMismatch @@ var "expected" @@ var "actual"))),
    "checkId">: Maybes.map
      (var "failWith" <.> (prepend @@ (string "Invalid id")))
      (var "checkValue" @@ (project _VertexType _VertexType_id @@ var "typ") @@ (project _Vertex _Vertex_id @@ var "el")),
    "checkProperties">: Maybes.map
      (var "failWith" <.> (prepend @@ (string "Invalid property")))
      (validateProperties
        @@ var "checkValue"
        @@ (project _VertexType _VertexType_properties @@ var "typ")
        @@ (project _Vertex _Vertex_properties @@ var "el"))]
    $ checkAll @@ list [var "checkLabel", var "checkId", var "checkProperties"]

----

checkAll :: TBinding ([Y.Maybe a] -> Y.Maybe a)
checkAll = validationDefinition "checkAll" $
  "checks" ~> lets [
    "errors">: Maybes.cat $ var "checks"]
    $ Lists.safeHead $ var "errors"

edgeError :: TBinding ((v -> String) -> PG.Edge v -> String -> String)
edgeError = validationDefinition "edgeError" $
  "showValue" ~> "e" ~>
    prepend @@ (string "Invalid edge with id " ++ (var "showValue" @@ (project _Edge _Edge_id @@ var "e")))

edgeLabelMismatch :: TBinding (PG.EdgeLabel -> PG.EdgeLabel -> String)
edgeLabelMismatch = validationDefinition "edgeLabelMismatch" $
  "expected" ~> "actual" ~>
    string "expected " ++ (unwrap _EdgeLabel @@ var "expected") ++ string ", found " ++ (unwrap _EdgeLabel @@ var "actual")

prepend :: TBinding (String -> String -> String)
prepend = validationDefinition "prepend" $
  "prefix" ~> "msg" ~>
    (var "prefix") ++ string ": " ++ (var "msg")

verify :: TBinding (Bool -> String -> Maybe String)
verify = validationDefinition "verify" $
  "b" ~> "err" ~>
    Logic.ifElse (var "b")
      nothing
      (just $ var "err")

vertexError :: TBinding ((v -> String) -> PG.Vertex v -> String -> String)
vertexError = validationDefinition "vertexError" $
  "showValue" ~> "v" ~>
    prepend @@ (string "Invalid vertex with id " ++ (var "showValue" @@ (project _Vertex _Vertex_id @@ var "v")))

vertexLabelMismatch :: TBinding (PG.VertexLabel -> PG.VertexLabel -> String)
vertexLabelMismatch = validationDefinition "vertexLabelMismatch" $
  "expected" ~> "actual" ~> Strings.cat $ list [
    string "expected ", unwrap _VertexLabel @@ var "expected", string ", found ", unwrap _VertexLabel @@ var "actual"]

-- TODO: this is a hack
ordT0 = (M.fromList [(Name "t0", S.fromList [TypeClassOrdering])])
