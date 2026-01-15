-- Note: this is an automatically generated file. Do not edit.

-- | Utility functions for GraphSON encoding and property graph conversion.

module Hydra.Pg.Graphson.Utils where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Pg.Graphson.Construct as Construct
import qualified Hydra.Pg.Graphson.Syntax as Syntax
import qualified Hydra.Pg.Model as Model_
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

elementsToVerticesWithAdjacentEdges :: (Ord t0) => ([Model_.Element t0] -> [Model_.VertexWithAdjacentEdges t0])
elementsToVerticesWithAdjacentEdges els =  
  let partitioned = (Lists.foldl (\acc -> \el -> (\x -> case x of
          Model_.ElementVertex v1 -> (Lists.cons v1 (Pairs.first acc), (Pairs.second acc))
          Model_.ElementEdge v1 -> (Pairs.first acc, (Lists.cons v1 (Pairs.second acc)))) el) ([], []) els)
  in  
    let vertices = (Lists.reverse (Pairs.first partitioned))
    in  
      let edges = (Lists.reverse (Pairs.second partitioned))
      in  
        let vertexMap0 = (Maps.fromList (Lists.map (\v -> (Model_.vertexId v, Model_.VertexWithAdjacentEdges {
                Model_.vertexWithAdjacentEdgesVertex = v,
                Model_.vertexWithAdjacentEdgesIns = [],
                Model_.vertexWithAdjacentEdgesOuts = []})) vertices))
        in  
          let vertexMap1 = (Lists.foldl (\vmap -> \edge ->  
                  let label = (Model_.edgeLabel edge)
                  in  
                    let edgeId = (Model_.edgeId edge)
                    in  
                      let outV = (Model_.edgeOut edge)
                      in  
                        let inV = (Model_.edgeIn edge)
                        in  
                          let props = (Model_.edgeProperties edge)
                          in  
                            let adjEdgeOut = Model_.AdjacentEdge {
                                    Model_.adjacentEdgeLabel = label,
                                    Model_.adjacentEdgeId = edgeId,
                                    Model_.adjacentEdgeVertex = inV,
                                    Model_.adjacentEdgeProperties = props}
                            in  
                              let adjEdgeIn = Model_.AdjacentEdge {
                                      Model_.adjacentEdgeLabel = label,
                                      Model_.adjacentEdgeId = edgeId,
                                      Model_.adjacentEdgeVertex = outV,
                                      Model_.adjacentEdgeProperties = props}
                              in  
                                let vmap1 = (Maybes.maybe vmap (\vae -> Maps.insert outV (Model_.VertexWithAdjacentEdges {
                                        Model_.vertexWithAdjacentEdgesVertex = (Model_.vertexWithAdjacentEdgesVertex vae),
                                        Model_.vertexWithAdjacentEdgesIns = (Model_.vertexWithAdjacentEdgesIns vae),
                                        Model_.vertexWithAdjacentEdgesOuts = (Lists.cons adjEdgeOut (Model_.vertexWithAdjacentEdgesOuts vae))}) vmap) (Maps.lookup outV vmap))
                                in (Maybes.maybe vmap1 (\vae -> Maps.insert inV (Model_.VertexWithAdjacentEdges {
                                  Model_.vertexWithAdjacentEdgesVertex = (Model_.vertexWithAdjacentEdgesVertex vae),
                                  Model_.vertexWithAdjacentEdgesIns = (Lists.cons adjEdgeIn (Model_.vertexWithAdjacentEdgesIns vae)),
                                  Model_.vertexWithAdjacentEdgesOuts = (Model_.vertexWithAdjacentEdgesOuts vae)}) vmap1) (Maps.lookup inV vmap1))) vertexMap0 edges)
          in (Maps.elems vertexMap1)

encodeStringValue :: (String -> Compute.Flow t0 Syntax.Value)
encodeStringValue s = (Flows.pure (Syntax.ValueString s))

encodeTermValue :: (Core.Term -> Compute.Flow t0 Syntax.Value)
encodeTermValue term = ((\x -> case x of
  Core.TermLiteral v1 -> ((\x -> case x of
    Core.LiteralBinary v2 -> (Flows.pure (Syntax.ValueBinary (Literals.binaryToString v2)))
    Core.LiteralBoolean v2 -> (Flows.pure (Syntax.ValueBoolean v2))
    Core.LiteralFloat v2 -> ((\x -> case x of
      Core.FloatValueBigfloat v3 -> (Flows.pure (Syntax.ValueBigDecimal (Syntax.BigDecimalValue (Literals.showBigfloat v3))))
      Core.FloatValueFloat32 v3 -> (Flows.pure (Syntax.ValueFloat (Syntax.FloatValueFinite v3)))
      Core.FloatValueFloat64 v3 -> (Flows.pure (Syntax.ValueDouble (Syntax.DoubleValueFinite v3)))
      _ -> (Flows.fail "unsupported float type")) v2)
    Core.LiteralInteger v2 -> ((\x -> case x of
      Core.IntegerValueBigint v3 -> (Flows.pure (Syntax.ValueBigInteger v3))
      Core.IntegerValueInt32 v3 -> (Flows.pure (Syntax.ValueInteger v3))
      Core.IntegerValueInt64 v3 -> (Flows.pure (Syntax.ValueLong v3))
      _ -> (Flows.fail "unsupported integer type")) v2)
    Core.LiteralString v2 -> (Flows.pure (Syntax.ValueString v2))
    _ -> (Flows.fail "unsupported literal type for GraphSON encoding")) v1)
  Core.TermUnit -> (Flows.pure Syntax.ValueNull)
  _ -> (Flows.fail "unsupported term variant for GraphSON encoding")) (Rewriting.deannotateTerm term))

pgElementsToGraphson :: (Ord t0) => ((t0 -> Compute.Flow t1 Syntax.Value) -> [Model_.Element t0] -> Compute.Flow t1 [Model.Value])
pgElementsToGraphson encodeValue els = (Flows.mapList (Construct.pgVertexWithAdjacentEdgesToJson encodeValue) (elementsToVerticesWithAdjacentEdges els))
