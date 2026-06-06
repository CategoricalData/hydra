-- Note: this is an automatically generated file. Do not edit.
-- | Functions for constructing GraphSON vertices from property graph vertices.

module Hydra.Pg.Graphson.Construct where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as JsonModel
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Pg.Graphson.Coder as Coder
import qualified Hydra.Pg.Graphson.Syntax as Syntax
import qualified Hydra.Pg.Model as PgModel
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | Convert a property graph adjacent edge to a GraphSON adjacent edge
adjacentEdgeToGraphson :: (t0 -> Either t1 Syntax.Value) -> PgModel.AdjacentEdge t0 -> Either t1 (Syntax.EdgeLabel, Syntax.AdjacentEdge)
adjacentEdgeToGraphson encodeValue edge =

      let label = PgModel.adjacentEdgeLabel edge
          edgeId = PgModel.adjacentEdgeId edge
          vertexId = PgModel.adjacentEdgeVertex edge
          props = PgModel.adjacentEdgeProperties edge
      in (Eithers.bind (encodeValue edgeId) (\gid -> Eithers.bind (encodeValue vertexId) (\gv -> Eithers.bind (Eithers.mapList (edgePropertyToGraphson encodeValue) (Maps.toList props)) (\propPairs -> Right (
        Syntax.EdgeLabel (PgModel.unEdgeLabel label),
        Syntax.AdjacentEdge {
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
      in (Maps.insert k (Optionals.cases existing (Lists.pure v) (\vs -> Lists.cons v vs)) m)) Maps.empty pairs
-- | Convert a property graph edge property to a GraphSON property
edgePropertyToGraphson :: (t0 -> Either t1 t2) -> (PgModel.PropertyKey, t0) -> Either t1 (Syntax.PropertyKey, t2)
edgePropertyToGraphson encodeValue prop =
    Eithers.map (\gv -> (Syntax.PropertyKey (PgModel.unPropertyKey (Pairs.first prop)), gv)) (encodeValue (Pairs.second prop))
-- | A coder that converts GraphSON vertices to JSON. Decoding is not supported.
graphsonVertexToJsonCoder :: Coders.Coder Syntax.Vertex JsonModel.Value
graphsonVertexToJsonCoder =
    Coders.Coder {
      Coders.coderEncode = (\_cx -> \v -> Right (Coder.vertexToJson v)),
      Coders.coderDecode = (\_cx -> \_ -> Left (Errors.ErrorOther (Errors.OtherError "decoding GraphSON JSON is currently unsupported")))}
-- | Convert a property graph vertex with adjacent edges to a GraphSON vertex
pgVertexWithAdjacentEdgesToGraphsonVertex :: (t0 -> Either t1 Syntax.Value) -> PgModel.VertexWithAdjacentEdges t0 -> Either t1 Syntax.Vertex
pgVertexWithAdjacentEdgesToGraphsonVertex encodeValue vae =

      let vertex = PgModel.vertexWithAdjacentEdgesVertex vae
          ins = PgModel.vertexWithAdjacentEdgesIns vae
          outs = PgModel.vertexWithAdjacentEdgesOuts vae
          label = PgModel.vertexLabel vertex
          vertexId = PgModel.vertexId vertex
          props = PgModel.vertexProperties vertex
      in (Eithers.bind (encodeValue vertexId) (\gid -> Eithers.bind (Eithers.mapList (vertexPropertyToGraphson encodeValue) (Maps.toList props)) (\propPairs -> Eithers.bind (Eithers.mapList (adjacentEdgeToGraphson encodeValue) ins) (\inPairs -> Eithers.bind (Eithers.mapList (adjacentEdgeToGraphson encodeValue) outs) (\outPairs -> Right (Syntax.Vertex {
        Syntax.vertexId = gid,
        Syntax.vertexLabel = (Just (Syntax.VertexLabel (PgModel.unVertexLabel label))),
        Syntax.vertexInEdges = (aggregateMap inPairs),
        Syntax.vertexOutEdges = (aggregateMap outPairs),
        Syntax.vertexProperties = (aggregateMap propPairs)}))))))
-- | Convert a property graph vertex with adjacent edges to JSON
pgVertexWithAdjacentEdgesToJson :: (t0 -> Either t1 Syntax.Value) -> PgModel.VertexWithAdjacentEdges t0 -> Either t1 JsonModel.Value
pgVertexWithAdjacentEdgesToJson encodeValue vertex =
    Eithers.bind (pgVertexWithAdjacentEdgesToGraphsonVertex encodeValue vertex) (\gVertex -> Right (Coder.vertexToJson gVertex))
-- | Convert a property graph vertex property to a GraphSON vertex property
vertexPropertyToGraphson :: (t0 -> Either t1 Syntax.Value) -> (PgModel.PropertyKey, t0) -> Either t1 (Syntax.PropertyKey, Syntax.VertexPropertyValue)
vertexPropertyToGraphson encodeValue prop =
    Eithers.map (\gv -> (
      Syntax.PropertyKey (PgModel.unPropertyKey (Pairs.first prop)),
      Syntax.VertexPropertyValue {
        Syntax.vertexPropertyValueId = Nothing,
        Syntax.vertexPropertyValueValue = gv})) (encodeValue (Pairs.second prop))
