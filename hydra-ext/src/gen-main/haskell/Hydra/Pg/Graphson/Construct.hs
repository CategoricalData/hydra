-- Note: this is an automatically generated file. Do not edit.

-- | Functions for constructing GraphSON vertices from property graph vertices.

module Hydra.Pg.Graphson.Construct where

import qualified Hydra.Context as Context
import qualified Hydra.Errors as Errors
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Pg.Graphson.Coder as Coder
import qualified Hydra.Pg.Graphson.Syntax as Syntax
import qualified Hydra.Pg.Model as Model_
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert a property graph adjacent edge to a GraphSON adjacent edge
adjacentEdgeToGraphson :: (t0 -> Either t1 Syntax.Value) -> Model_.AdjacentEdge t0 -> Either t1 (Syntax.EdgeLabel, Syntax.AdjacentEdge)
adjacentEdgeToGraphson encodeValue edge =

      let label = Model_.adjacentEdgeLabel edge
          edgeId = Model_.adjacentEdgeId edge
          vertexId = Model_.adjacentEdgeVertex edge
          props = Model_.adjacentEdgeProperties edge
      in (Eithers.bind (encodeValue edgeId) (\gid -> Eithers.bind (encodeValue vertexId) (\gv -> Eithers.bind (Eithers.mapList (edgePropertyToGraphson encodeValue) (Maps.toList props)) (\propPairs -> Right (Syntax.EdgeLabel (Model_.unEdgeLabel label), Syntax.AdjacentEdge {
        Syntax.adjacentEdgeId = gid,
        Syntax.adjacentEdgeVertexId = gv,
        Syntax.adjacentEdgeProperties = (Maps.fromList propPairs)})))))

-- | Aggregate a list of key-value pairs into a map where each key maps to a list of values
aggregateMap :: Ord t0 => ([(t0, t1)] -> M.Map t0 [t1])
aggregateMap pairs =
    Lists.foldl (\m -> \p ->
      let k = Pairs.first p
          v = Pairs.second p
          existing = Maps.lookup k m
      in (Maps.insert k (Maybes.maybe (Lists.pure v) (\vs -> Lists.cons v vs) existing) m)) Maps.empty pairs

-- | Convert a property graph edge property to a GraphSON property
edgePropertyToGraphson :: (t0 -> Either t1 t2) -> (Model_.PropertyKey, t0) -> Either t1 (Syntax.PropertyKey, t2)
edgePropertyToGraphson encodeValue prop =
    Eithers.map (\gv -> (Syntax.PropertyKey (Model_.unPropertyKey (Pairs.first prop)), gv)) (encodeValue (Pairs.second prop))

-- | A coder that converts GraphSON vertices to JSON. Decoding is not supported.
graphsonVertexToJsonCoder :: Util.Coder Syntax.Vertex Model.Value
graphsonVertexToJsonCoder =
    Util.Coder {
      Util.coderEncode = (\_cx -> \v -> Right (Coder.vertexToJson v)),
      Util.coderDecode = (\_cx -> \_ -> Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "decoding GraphSON JSON is currently unsupported")),
        Context.inContextContext = _cx}))}

-- | Convert a property graph vertex with adjacent edges to a GraphSON vertex
pgVertexWithAdjacentEdgesToGraphsonVertex :: (t0 -> Either t1 Syntax.Value) -> Model_.VertexWithAdjacentEdges t0 -> Either t1 Syntax.Vertex
pgVertexWithAdjacentEdgesToGraphsonVertex encodeValue vae =

      let vertex = Model_.vertexWithAdjacentEdgesVertex vae
          ins = Model_.vertexWithAdjacentEdgesIns vae
          outs = Model_.vertexWithAdjacentEdgesOuts vae
          label = Model_.vertexLabel vertex
          vertexId = Model_.vertexId vertex
          props = Model_.vertexProperties vertex
      in (Eithers.bind (encodeValue vertexId) (\gid -> Eithers.bind (Eithers.mapList (vertexPropertyToGraphson encodeValue) (Maps.toList props)) (\propPairs -> Eithers.bind (Eithers.mapList (adjacentEdgeToGraphson encodeValue) ins) (\inPairs -> Eithers.bind (Eithers.mapList (adjacentEdgeToGraphson encodeValue) outs) (\outPairs -> Right (Syntax.Vertex {
        Syntax.vertexId = gid,
        Syntax.vertexLabel = (Just (Syntax.VertexLabel (Model_.unVertexLabel label))),
        Syntax.vertexInEdges = (aggregateMap inPairs),
        Syntax.vertexOutEdges = (aggregateMap outPairs),
        Syntax.vertexProperties = (aggregateMap propPairs)}))))))

-- | Convert a property graph vertex with adjacent edges to JSON
pgVertexWithAdjacentEdgesToJson :: (t0 -> Either t1 Syntax.Value) -> Model_.VertexWithAdjacentEdges t0 -> Either t1 Model.Value
pgVertexWithAdjacentEdgesToJson encodeValue vertex =
    Eithers.bind (pgVertexWithAdjacentEdgesToGraphsonVertex encodeValue vertex) (\gVertex -> Right (Coder.vertexToJson gVertex))

-- | Convert a property graph vertex property to a GraphSON vertex property
vertexPropertyToGraphson :: (t0 -> Either t1 Syntax.Value) -> (Model_.PropertyKey, t0) -> Either t1 (Syntax.PropertyKey, Syntax.VertexPropertyValue)
vertexPropertyToGraphson encodeValue prop =
    Eithers.map (\gv -> (Syntax.PropertyKey (Model_.unPropertyKey (Pairs.first prop)), Syntax.VertexPropertyValue {
      Syntax.vertexPropertyValueId = Nothing,
      Syntax.vertexPropertyValueValue = gv})) (encodeValue (Pairs.second prop))
