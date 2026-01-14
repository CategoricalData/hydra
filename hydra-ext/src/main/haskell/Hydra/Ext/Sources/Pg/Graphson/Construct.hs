module Hydra.Ext.Sources.Pg.Graphson.Construct where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (
  aggregateMap, adjacentEdgeToGraphson, edgePropertyToGraphson,
  graphsonVertexToJsonCoder, pgVertexWithAdjacentEdgesToGraphsonVertex,
  pgVertexWithAdjacentEdgesToJson, vertexPropertyToGraphson)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Meta.Ast           as Ast
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
import qualified Hydra.Dsl.Meta.Lib.Strings   as Strings
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
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Ext.Sources.Pg.Graphson.Syntax as GraphsonSyntax
import qualified Hydra.Ext.Sources.Pg.Graphson.Coder as GraphsonCoder
import qualified Hydra.Ext.Sources.Pg.Graphson.Coder as Coder (vertexToJson)
import qualified Hydra.Ext.Sources.Pg.Model as PgModel
import qualified Hydra.Sources.Json.Model as JsonModel
import qualified Hydra.Pg.Graphson.Syntax as G  -- Generated phantom types
import qualified Hydra.Pg.Model as PG            -- Generated PG model types
import qualified Hydra.Json.Model as JM          -- Generated JSON types
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


ns :: Namespace
ns = Namespace "hydra.pg.graphson.construct"

module_ :: Module
module_ = Module ns elements
    [GraphsonCoder.ns]  -- term dependencies
    (kernelTypesNamespaces L.++ [GraphsonSyntax.ns, PgModel.ns, JsonModel.ns]) $  -- type dependencies
    Just "Functions for constructing GraphSON vertices from property graph vertices."
  where
    elements = [
      toBinding aggregateMap,
      toBinding adjacentEdgeToGraphson,
      toBinding edgePropertyToGraphson,
      toBinding graphsonVertexToJsonCoder,
      toBinding pgVertexWithAdjacentEdgesToGraphsonVertex,
      toBinding pgVertexWithAdjacentEdgesToJson,
      toBinding vertexPropertyToGraphson]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- Type references
gson :: String -> Type
gson = Bootstrap.typeref GraphsonSyntax.ns

pg :: String -> Type
pg = Bootstrap.typeref PgModel.ns

jsonValue :: Type
jsonValue = Bootstrap.typeref JsonModel.ns "Value"

-- | Aggregate a list of key-value pairs into a map of lists
aggregateMap :: TBinding ([(k, v)] -> M.Map k [v])
aggregateMap = define "aggregateMap" $
  doc "Aggregate a list of key-value pairs into a map where each key maps to a list of values" $
  "pairs" ~>
    Lists.foldl
      ("m" ~> "p" ~>
        "k" <~ (Pairs.first $ var "p") $
        "v" <~ (Pairs.second $ var "p") $
        "existing" <~ (Maps.lookup (var "k") (var "m")) $
        Maps.insert (var "k")
          (Maybes.maybe
            (Lists.pure $ var "v")
            ("vs" ~> Lists.cons (var "v") (var "vs"))
            (var "existing"))
          (var "m"))
      Maps.empty
      (var "pairs")

-- | Convert a PG edge property to GraphSON format
edgePropertyToGraphson :: TBinding ((v -> Flow s G.Value) -> (PG.PropertyKey, v) -> Flow s (G.PropertyKey, G.Value))
edgePropertyToGraphson = define "edgePropertyToGraphson" $
  doc "Convert a property graph edge property to a GraphSON property" $
  "encodeValue" ~> "prop" ~>
    Flows.map
      ("gv" ~>
        pair
          (wrap G._PropertyKey (unwrap PG._PropertyKey @@ (Pairs.first $ var "prop")))
          (var "gv"))
      (var "encodeValue" @@ (Pairs.second $ var "prop"))

-- | Convert a PG vertex property to GraphSON format
vertexPropertyToGraphson :: TBinding ((v -> Flow s G.Value) -> (PG.PropertyKey, v) -> Flow s (G.PropertyKey, G.VertexPropertyValue))
vertexPropertyToGraphson = define "vertexPropertyToGraphson" $
  doc "Convert a property graph vertex property to a GraphSON vertex property" $
  "encodeValue" ~> "prop" ~>
    Flows.map
      ("gv" ~>
        pair
          (wrap G._PropertyKey (unwrap PG._PropertyKey @@ (Pairs.first $ var "prop")))
          (record G._VertexPropertyValue [
            G._VertexPropertyValue_id>>: nothing,
            G._VertexPropertyValue_value>>: var "gv"]))
      (var "encodeValue" @@ (Pairs.second $ var "prop"))

-- | Convert a PG adjacent edge to GraphSON format
adjacentEdgeToGraphson :: TBinding ((v -> Flow s G.Value) -> PG.AdjacentEdge v -> Flow s (G.EdgeLabel, G.AdjacentEdge))
adjacentEdgeToGraphson = define "adjacentEdgeToGraphson" $
  doc "Convert a property graph adjacent edge to a GraphSON adjacent edge" $
  "encodeValue" ~> "edge" ~>
    "label" <~ (project PG._AdjacentEdge PG._AdjacentEdge_label @@ var "edge") $
    "edgeId" <~ (project PG._AdjacentEdge PG._AdjacentEdge_id @@ var "edge") $
    "vertexId" <~ (project PG._AdjacentEdge PG._AdjacentEdge_vertex @@ var "edge") $
    "props" <~ (project PG._AdjacentEdge PG._AdjacentEdge_properties @@ var "edge") $
    Flows.bind
      (var "encodeValue" @@ var "edgeId")
      ("gid" ~>
        Flows.bind
          (var "encodeValue" @@ var "vertexId")
          ("gv" ~>
            Flows.bind
              (Flows.mapList (edgePropertyToGraphson @@ var "encodeValue") (Maps.toList $ var "props"))
              ("propPairs" ~>
                Flows.pure $
                  pair
                    (wrap G._EdgeLabel (unwrap PG._EdgeLabel @@ var "label"))
                    (record G._AdjacentEdge [
                      G._AdjacentEdge_id>>: var "gid",
                      G._AdjacentEdge_vertexId>>: var "gv",
                      G._AdjacentEdge_properties>>: Maps.fromList (var "propPairs")]))))

-- | Convert a PG vertex with adjacent edges to a GraphSON vertex
pgVertexWithAdjacentEdgesToGraphsonVertex :: TBinding ((v -> Flow s G.Value) -> PG.VertexWithAdjacentEdges v -> Flow s G.Vertex)
pgVertexWithAdjacentEdgesToGraphsonVertex = define "pgVertexWithAdjacentEdgesToGraphsonVertex" $
  doc "Convert a property graph vertex with adjacent edges to a GraphSON vertex" $
  "encodeValue" ~> "vae" ~>
    "vertex" <~ (project PG._VertexWithAdjacentEdges PG._VertexWithAdjacentEdges_vertex @@ var "vae") $
    "ins" <~ (project PG._VertexWithAdjacentEdges PG._VertexWithAdjacentEdges_ins @@ var "vae") $
    "outs" <~ (project PG._VertexWithAdjacentEdges PG._VertexWithAdjacentEdges_outs @@ var "vae") $
    "label" <~ (project PG._Vertex PG._Vertex_label @@ var "vertex") $
    "vertexId" <~ (project PG._Vertex PG._Vertex_id @@ var "vertex") $
    "props" <~ (project PG._Vertex PG._Vertex_properties @@ var "vertex") $
    Flows.bind
      (var "encodeValue" @@ var "vertexId")
      ("gid" ~>
        Flows.bind
          (Flows.mapList (vertexPropertyToGraphson @@ var "encodeValue") (Maps.toList $ var "props"))
          ("propPairs" ~>
            Flows.bind
              (Flows.mapList (adjacentEdgeToGraphson @@ var "encodeValue") (var "ins"))
              ("inPairs" ~>
                Flows.bind
                  (Flows.mapList (adjacentEdgeToGraphson @@ var "encodeValue") (var "outs"))
                  ("outPairs" ~>
                    Flows.pure $
                      record G._Vertex [
                        G._Vertex_id>>: var "gid",
                        G._Vertex_label>>: just (wrap G._VertexLabel (unwrap PG._VertexLabel @@ var "label")),
                        G._Vertex_inEdges>>: aggregateMap @@ var "inPairs",
                        G._Vertex_outEdges>>: aggregateMap @@ var "outPairs",
                        G._Vertex_properties>>: aggregateMap @@ var "propPairs"]))))

-- | A coder that converts GraphSON vertices to JSON
graphsonVertexToJsonCoder :: TBinding (Coder s s G.Vertex JM.Value)
graphsonVertexToJsonCoder = define "graphsonVertexToJsonCoder" $
  doc "A coder that converts GraphSON vertices to JSON. Decoding is not supported." $
  Compute.coder
    ("v" ~> Flows.pure $ Coder.vertexToJson @@ var "v")
    ("_" ~> Flows.fail $ string "decoding GraphSON JSON is currently unsupported")

-- | Convert a PG vertex with adjacent edges directly to JSON
pgVertexWithAdjacentEdgesToJson :: TBinding ((v -> Flow s G.Value) -> PG.VertexWithAdjacentEdges v -> Flow s JM.Value)
pgVertexWithAdjacentEdgesToJson = define "pgVertexWithAdjacentEdgesToJson" $
  doc "Convert a property graph vertex with adjacent edges to JSON" $
  "encodeValue" ~> "vertex" ~>
    Flows.bind
      (pgVertexWithAdjacentEdgesToGraphsonVertex @@ var "encodeValue" @@ var "vertex")
      ("gVertex" ~>
        Flows.pure $ Coder.vertexToJson @@ var "gVertex")
