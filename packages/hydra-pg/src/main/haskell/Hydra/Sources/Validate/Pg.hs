module Hydra.Sources.Validate.Pg where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (Edge(..), _Edge, _Edge_in, _Edge_out, Element(..), _Element, Graph(..), _Graph)
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import           Prelude hiding ((++))
import qualified Data.Map                                  as M
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Pg.Model as PG
import Hydra.Error.Pg as Err
import qualified Hydra.Sources.Pg.Model as PgModel
import qualified Hydra.Sources.Error.Pg as ErrorPg


validationDefinition :: String -> TTerm a -> TTermDefinition a
validationDefinition = definitionInModule module_

module_ :: Module
module_ = Module {
            moduleNamespace = (Namespace "hydra.validate.pg"),
            moduleDefinitions = definitions,
            moduleTermDependencies = [],
            moduleTypeDependencies = [PgModel.ns, ErrorPg.ns],
            moduleDescription = Just "Validation functions for property graphs"}
  where
   definitions = [
     toDefinition checkAll,
     toDefinition validateEdge,
     toDefinition validateGraph,
     toDefinition validateProperties,
     toDefinition validateVertex]

checkAll :: TTermDefinition ([Y.Maybe a] -> Y.Maybe a)
checkAll = validationDefinition "checkAll" $
  "checks" ~> lets [
    "errors">: Maybes.cat $ var "checks"]
    $ Lists.maybeHead $ var "errors"

validateVertex :: TTermDefinition (
     (t -> v -> Maybe InvalidValueError)
  -> PG.VertexType t
  -> PG.Vertex v
  -> Y.Maybe InvalidVertexError)
validateVertex = validationDefinition "validateVertex" $
  "checkValue" ~> "typ" ~> "el" ~> lets [
    "checkLabel">: lets [
      "expected">: project _VertexType _VertexType_label @@ var "typ",
      "actual">: project _Vertex _Vertex_label @@ var "el"]
      $ Logic.ifElse
          (Equality.equal
            (unwrap _VertexLabel @@ var "actual")
            (unwrap _VertexLabel @@ var "expected"))
          nothing
          (just $ inject _InvalidVertexError _InvalidVertexError_label $
            record _NoSuchVertexLabelError [
              _NoSuchVertexLabelError_label>>: var "actual"]),
    "checkId">: Maybes.map
      ("err" ~> inject _InvalidVertexError _InvalidVertexError_id $ var "err")
      (var "checkValue" @@ (project _VertexType _VertexType_id @@ var "typ") @@ (project _Vertex _Vertex_id @@ var "el")),
    "checkProperties">: Maybes.map
      ("err" ~> inject _InvalidVertexError _InvalidVertexError_property $ var "err")
      (validateProperties
        @@ var "checkValue"
        @@ (project _VertexType _VertexType_properties @@ var "typ")
        @@ (project _Vertex _Vertex_properties @@ var "el"))]
    $ checkAll @@ list [var "checkLabel", var "checkId", var "checkProperties"]

validateEdge :: TTermDefinition (
     (t -> v -> Maybe InvalidValueError)
  -> Y.Maybe (v -> Y.Maybe PG.VertexLabel)
  -> PG.EdgeType t
  -> PG.Edge v
  -> Y.Maybe InvalidEdgeError)
validateEdge = validationDefinition "validateEdge" $
  "checkValue" ~> "labelForVertexId" ~> "typ" ~> "el" ~>
    lets [
      "checkLabel">: lets [
        "expected">: project _EdgeType _EdgeType_label @@ var "typ",
        "actual">: project _Edge _Edge_label @@ var "el"]
        $ Logic.ifElse
            (Equality.equal
              (unwrap _EdgeLabel @@ var "actual")
              (unwrap _EdgeLabel @@ var "expected"))
            nothing
            (just $ inject _InvalidEdgeError _InvalidEdgeError_label $
              record _NoSuchEdgeLabelError [
                _NoSuchEdgeLabelError_label>>: var "actual"]),
      "checkId">: Maybes.map
        ("err" ~> inject _InvalidEdgeError _InvalidEdgeError_id $ var "err")
        (var "checkValue" @@ (project _EdgeType _EdgeType_id @@ var "typ") @@ (project _Edge _Edge_id @@ var "el")),
      "checkProperties">: Maybes.map
        ("err" ~> inject _InvalidEdgeError _InvalidEdgeError_property $ var "err")
        (validateProperties
          @@ var "checkValue"
          @@ (project _EdgeType _EdgeType_properties @@ var "typ")
          @@ (project _Edge _Edge_properties @@ var "el")),
      "checkOut">: Maybes.maybe
        nothing
        ("f" ~> Maybes.maybe
          (just (inject _InvalidEdgeError _InvalidEdgeError_outVertexNotFound $ unit))
          ("label" ~> Logic.ifElse
            (Equality.equal
              (unwrap _VertexLabel @@ var "label")
              (unwrap _VertexLabel @@ (project _EdgeType _EdgeType_out @@ var "typ")))
            nothing
            (just $ inject _InvalidEdgeError _InvalidEdgeError_outVertexLabel $
              record _WrongVertexLabelError [
                _WrongVertexLabelError_expected>>: project _EdgeType _EdgeType_out @@ var "typ",
                _WrongVertexLabelError_actual>>: var "label"]))
          (var "f" @@ (project _Edge _Edge_out @@ var "el")))
          (var "labelForVertexId"),
      "checkIn">: Maybes.maybe
        nothing
        ("f" ~> Maybes.maybe
          (just (inject _InvalidEdgeError _InvalidEdgeError_inVertexNotFound $ unit))
          ("label" ~> Logic.ifElse
            (Equality.equal
              (unwrap _VertexLabel @@ var "label")
              (unwrap _VertexLabel @@ (project _EdgeType _EdgeType_in @@ var "typ")))
            nothing
            (just $ inject _InvalidEdgeError _InvalidEdgeError_inVertexLabel $
              record _WrongVertexLabelError [
                _WrongVertexLabelError_expected>>: project _EdgeType _EdgeType_in @@ var "typ",
                _WrongVertexLabelError_actual>>: var "label"]))
          (var "f" @@ (project _Edge _Edge_in @@ var "el")))
        (var "labelForVertexId")]
      $ checkAll @@ list [var "checkLabel", var "checkId", var "checkProperties", var "checkOut", var "checkIn"]

validateProperties :: TTermDefinition (
     (t -> v -> Maybe InvalidValueError)
  -> [PG.PropertyType t]
  -> M.Map PG.PropertyKey v
  -> Y.Maybe InvalidElementPropertyError)
validateProperties = validationDefinition "validateProperties" $
  "checkValue" ~> "types" ~> "props" ~> lets [
    "checkTypes">: checkAll @@ (Lists.map (var "checkType") (var "types")),
    "checkType">:
      "t" ~> Logic.ifElse (project _PropertyType _PropertyType_required @@ var "t")
        (Maybes.maybe
          (just (record _InvalidElementPropertyError [
            _InvalidElementPropertyError_key>>: project _PropertyType _PropertyType_key @@ var "t",
            _InvalidElementPropertyError_error>>:
              inject _InvalidPropertyError _InvalidPropertyError_missingRequired $
                project _PropertyType _PropertyType_key @@ var "t"]))
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
          (just (record _InvalidElementPropertyError [
            _InvalidElementPropertyError_key>>: var "key",
            _InvalidElementPropertyError_error>>:
              inject _InvalidPropertyError _InvalidPropertyError_unexpectedKey $ var "key"]))
          ("typ" ~> Maybes.map
            ("err" ~> record _InvalidElementPropertyError [
              _InvalidElementPropertyError_key>>: var "key",
              _InvalidElementPropertyError_error>>:
                inject _InvalidPropertyError _InvalidPropertyError_invalidValue $ var "err"])
            (var "checkValue" @@ var "typ" @@ var "val"))
          (Maps.lookup (var "key") (var "m"))]
      $ checkAll @@ (Lists.map (var "checkPair") (Maps.toList $ var "props"))]
    $ checkAll @@ list [var "checkTypes", var "checkValues"]

validateGraph :: TTermDefinition (
     (t -> v -> Maybe InvalidValueError)
  -> PG.GraphSchema t
  -> PG.Graph v
  -> Y.Maybe (InvalidGraphError v))
validateGraph = validationDefinition "validateGraph" $
  "checkValue" ~> "schema" ~> "graph" ~> lets [
    "checkVertices">: lets [
      "checkVertex">: "el" ~> Maybes.maybe
        (just (inject _InvalidGraphError _InvalidGraphError_vertex $
          record _InvalidGraphVertexError [
            _InvalidGraphVertexError_id>>: project _Vertex _Vertex_id @@ var "el",
            _InvalidGraphVertexError_error>>:
              inject _InvalidVertexError _InvalidVertexError_label $
                record _NoSuchVertexLabelError [
                  _NoSuchVertexLabelError_label>>: project _Vertex _Vertex_label @@ var "el"]]))
        ("t" ~> Maybes.map
          ("err" ~> inject _InvalidGraphError _InvalidGraphError_vertex $
            record _InvalidGraphVertexError [
              _InvalidGraphVertexError_id>>: project _Vertex _Vertex_id @@ var "el",
              _InvalidGraphVertexError_error>>: var "err"])
          (validateVertex
            @@ var "checkValue"
            @@ var "t"
            @@ var "el"))
        (Maps.lookup
          (project _Vertex _Vertex_label @@ var "el")
          (project _GraphSchema _GraphSchema_vertices @@ var "schema"))]
      $ checkAll
          @@ (Lists.map (var "checkVertex") $ Maps.elems $ project _Graph _Graph_vertices @@ var "graph"),
    "checkEdges">: lets [
        "checkEdge">: "el" ~> Maybes.maybe
          (just (inject _InvalidGraphError _InvalidGraphError_edge $
            record _InvalidGraphEdgeError [
              _InvalidGraphEdgeError_id>>: project _Edge _Edge_id @@ var "el",
              _InvalidGraphEdgeError_error>>:
                inject _InvalidEdgeError _InvalidEdgeError_label $
                  record _NoSuchEdgeLabelError [
                    _NoSuchEdgeLabelError_label>>: project _Edge _Edge_label @@ var "el"]]))
          ("t" ~> Maybes.map
            ("err" ~> inject _InvalidGraphError _InvalidGraphError_edge $
              record _InvalidGraphEdgeError [
                _InvalidGraphEdgeError_id>>: project _Edge _Edge_id @@ var "el",
                _InvalidGraphEdgeError_error>>: var "err"])
            (validateEdge
              @@ var "checkValue"
              @@ var "labelForVertexId"
              @@ var "t"
              @@ var "el"))
          (Maps.lookup
            (project _Edge _Edge_label @@ var "el")
            (project _GraphSchema _GraphSchema_edges @@ var "schema")),
        "labelForVertexId">: just $ "i" ~>
          Maybes.map (project _Vertex _Vertex_label) (Maps.lookup (var "i") (project _Graph _Graph_vertices @@ var "graph"))]
      $ checkAll
          @@ (Lists.map (var "checkEdge") $ Maps.elems $ project _Graph _Graph_edges @@ var "graph")]
    $ checkAll @@ list [var "checkVertices", var "checkEdges"]
