-- Note: this is an automatically generated file. Do not edit.

-- | Functions for constructing GraphSON vertices from property graph vertices.

module Hydra.Pg.Graphson.Construct where

import qualified Hydra.Compute as Compute
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Pg.Graphson.Coder as Coder
import qualified Hydra.Pg.Graphson.Syntax as Syntax
import qualified Hydra.Pg.Model as Model_
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

aggregateMap :: (Ord t0) => ([(t0, t1)] -> M.Map t0 [t1])
aggregateMap pairs = (Lists.foldl (\m -> \p ->  
  let k = (Pairs.first p)
  in  
    let v = (Pairs.second p)
    in  
      let existing = (Maps.lookup k m)
      in (Maps.insert k (Maybes.maybe (Lists.pure v) (\vs -> Lists.cons v vs) existing) m)) Maps.empty pairs)

adjacentEdgeToGraphson :: ((t0 -> Compute.Flow t1 Syntax.Value) -> Model_.AdjacentEdge t0 -> Compute.Flow t1 (Syntax.EdgeLabel, Syntax.AdjacentEdge))
adjacentEdgeToGraphson encodeValue edge =  
  let label = (Model_.adjacentEdgeLabel edge)
  in  
    let edgeId = (Model_.adjacentEdgeId edge)
    in  
      let vertexId = (Model_.adjacentEdgeVertex edge)
      in  
        let props = (Model_.adjacentEdgeProperties edge)
        in (Flows.bind (encodeValue edgeId) (\gid -> Flows.bind (encodeValue vertexId) (\gv -> Flows.bind (Flows.mapList (edgePropertyToGraphson encodeValue) (Maps.toList props)) (\propPairs -> Flows.pure (Syntax.EdgeLabel (Model_.unEdgeLabel label), Syntax.AdjacentEdge {
          Syntax.adjacentEdgeId = gid,
          Syntax.adjacentEdgeVertexId = gv,
          Syntax.adjacentEdgeProperties = (Maps.fromList propPairs)})))))

edgePropertyToGraphson :: ((t0 -> Compute.Flow t1 t2) -> (Model_.PropertyKey, t0) -> Compute.Flow t1 (Syntax.PropertyKey, t2))
edgePropertyToGraphson encodeValue prop = (Flows.map (\gv -> (Syntax.PropertyKey (Model_.unPropertyKey (Pairs.first prop)), gv)) (encodeValue (Pairs.second prop)))

graphsonVertexToJsonCoder :: (Compute.Coder t0 t1 Syntax.Vertex Model.Value)
graphsonVertexToJsonCoder = Compute.Coder {
  Compute.coderEncode = (\v -> Flows.pure (Coder.vertexToJson v)),
  Compute.coderDecode = (\_ -> Flows.fail "decoding GraphSON JSON is currently unsupported")}

pgVertexWithAdjacentEdgesToGraphsonVertex :: ((t0 -> Compute.Flow t1 Syntax.Value) -> Model_.VertexWithAdjacentEdges t0 -> Compute.Flow t1 Syntax.Vertex)
pgVertexWithAdjacentEdgesToGraphsonVertex encodeValue vae =  
  let vertex = (Model_.vertexWithAdjacentEdgesVertex vae)
  in  
    let ins = (Model_.vertexWithAdjacentEdgesIns vae)
    in  
      let outs = (Model_.vertexWithAdjacentEdgesOuts vae)
      in  
        let label = (Model_.vertexLabel vertex)
        in  
          let vertexId = (Model_.vertexId vertex)
          in  
            let props = (Model_.vertexProperties vertex)
            in (Flows.bind (encodeValue vertexId) (\gid -> Flows.bind (Flows.mapList (vertexPropertyToGraphson encodeValue) (Maps.toList props)) (\propPairs -> Flows.bind (Flows.mapList (adjacentEdgeToGraphson encodeValue) ins) (\inPairs -> Flows.bind (Flows.mapList (adjacentEdgeToGraphson encodeValue) outs) (\outPairs -> Flows.pure (Syntax.Vertex {
              Syntax.vertexId = gid,
              Syntax.vertexLabel = (Just (Syntax.VertexLabel (Model_.unVertexLabel label))),
              Syntax.vertexInEdges = (aggregateMap inPairs),
              Syntax.vertexOutEdges = (aggregateMap outPairs),
              Syntax.vertexProperties = (aggregateMap propPairs)}))))))

pgVertexWithAdjacentEdgesToJson :: ((t0 -> Compute.Flow t1 Syntax.Value) -> Model_.VertexWithAdjacentEdges t0 -> Compute.Flow t1 Model.Value)
pgVertexWithAdjacentEdgesToJson encodeValue vertex = (Flows.bind (pgVertexWithAdjacentEdgesToGraphsonVertex encodeValue vertex) (\gVertex -> Flows.pure (Coder.vertexToJson gVertex)))

vertexPropertyToGraphson :: ((t0 -> Compute.Flow t1 Syntax.Value) -> (Model_.PropertyKey, t0) -> Compute.Flow t1 (Syntax.PropertyKey, Syntax.VertexPropertyValue))
vertexPropertyToGraphson encodeValue prop = (Flows.map (\gv -> (Syntax.PropertyKey (Model_.unPropertyKey (Pairs.first prop)), Syntax.VertexPropertyValue {
  Syntax.vertexPropertyValueId = Nothing,
  Syntax.vertexPropertyValueValue = gv})) (encodeValue (Pairs.second prop)))
