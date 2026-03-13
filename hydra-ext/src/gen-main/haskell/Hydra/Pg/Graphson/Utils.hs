-- Note: this is an automatically generated file. Do not edit.

-- | Utility functions for GraphSON encoding and property graph conversion.

module Hydra.Pg.Graphson.Utils where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
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

-- | Convert a list of property graph elements to a list of vertices with their adjacent edges
elementsToVerticesWithAdjacentEdges :: Ord t0 => ([Model_.Element t0] -> [Model_.VertexWithAdjacentEdges t0])
elementsToVerticesWithAdjacentEdges els =  
  let partitioned = (Lists.foldl (\acc -> \el -> (\x -> case x of
          Model_.ElementVertex v0 -> (Lists.cons v0 (Pairs.first acc), (Pairs.second acc))
          Model_.ElementEdge v0 -> (Pairs.first acc, (Lists.cons v0 (Pairs.second acc)))) el) ([], []) els)
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

-- | Encode a String value as a GraphSON Value
encodeStringValue :: (String -> Either t0 Syntax.Value)
encodeStringValue s = (Right (Syntax.ValueString s))

-- | Encode a Hydra Term as a GraphSON Value. Supports literals and unit values.
encodeTermValue :: (Core.Term -> Either (Context.InContext Error.OtherError) Syntax.Value)
encodeTermValue term = ((\x -> case x of
  Core.TermLiteral v0 -> ((\x -> case x of
    Core.LiteralBinary v1 -> (Right (Syntax.ValueBinary (Literals.binaryToString v1)))
    Core.LiteralBoolean v1 -> (Right (Syntax.ValueBoolean v1))
    Core.LiteralFloat v1 -> ((\x -> case x of
      Core.FloatValueBigfloat v2 -> (Right (Syntax.ValueBigDecimal (Syntax.BigDecimalValue (Literals.showBigfloat v2))))
      Core.FloatValueFloat32 v2 -> (Right (Syntax.ValueFloat (Syntax.FloatValueFinite v2)))
      Core.FloatValueFloat64 v2 -> (Right (Syntax.ValueDouble (Syntax.DoubleValueFinite v2)))
      _ -> (Left (Context.InContext {
        Context.inContextObject = (Error.OtherError "unsupported float type"),
        Context.inContextContext = Lexical.emptyContext}))) v1)
    Core.LiteralInteger v1 -> ((\x -> case x of
      Core.IntegerValueBigint v2 -> (Right (Syntax.ValueBigInteger v2))
      Core.IntegerValueInt32 v2 -> (Right (Syntax.ValueInteger v2))
      Core.IntegerValueInt64 v2 -> (Right (Syntax.ValueLong v2))
      _ -> (Left (Context.InContext {
        Context.inContextObject = (Error.OtherError "unsupported integer type"),
        Context.inContextContext = Lexical.emptyContext}))) v1)
    Core.LiteralString v1 -> (Right (Syntax.ValueString v1))
    _ -> (Left (Context.InContext {
      Context.inContextObject = (Error.OtherError "unsupported literal type for GraphSON encoding"),
      Context.inContextContext = Lexical.emptyContext}))) v0)
  Core.TermUnit -> (Right Syntax.ValueNull)
  _ -> (Left (Context.InContext {
    Context.inContextObject = (Error.OtherError "unsupported term variant for GraphSON encoding"),
    Context.inContextContext = Lexical.emptyContext}))) (Rewriting.deannotateTerm term))

-- | Convert property graph elements to a list of GraphSON JSON values
pgElementsToGraphson :: Ord t0 => ((t0 -> Either t1 Syntax.Value) -> [Model_.Element t0] -> Either t1 [Model.Value])
pgElementsToGraphson encodeValue els = (Eithers.mapList (Construct.pgVertexWithAdjacentEdgesToJson encodeValue) (elementsToVerticesWithAdjacentEdges els))
