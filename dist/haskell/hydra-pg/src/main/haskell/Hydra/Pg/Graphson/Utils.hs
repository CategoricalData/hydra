-- Note: this is an automatically generated file. Do not edit.

-- | Utility functions for GraphSON encoding and property graph conversion.

module Hydra.Pg.Graphson.Utils where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Json.Model as JsonModel
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Pg.Graphson.Construct as Construct
import qualified Hydra.Pg.Graphson.Syntax as Syntax
import qualified Hydra.Pg.Model as PgModel
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Convert a list of property graph elements to a list of vertices with their adjacent edges
elementsToVerticesWithAdjacentEdges :: Ord t0 => ([PgModel.Element t0] -> [PgModel.VertexWithAdjacentEdges t0])
elementsToVerticesWithAdjacentEdges els =

      let partitioned =
              Lists.foldl (\acc -> \el -> (\x -> case x of
                PgModel.ElementVertex v0 -> (Lists.cons v0 (Pairs.first acc), (Pairs.second acc))
                PgModel.ElementEdge v0 -> (Pairs.first acc, (Lists.cons v0 (Pairs.second acc)))) el) ([], []) els
          vertices = Lists.reverse (Pairs.first partitioned)
          edges = Lists.reverse (Pairs.second partitioned)
          vertexMap0 =
                  Maps.fromList (Lists.map (\v -> (PgModel.vertexId v, PgModel.VertexWithAdjacentEdges {
                    PgModel.vertexWithAdjacentEdgesVertex = v,
                    PgModel.vertexWithAdjacentEdgesIns = [],
                    PgModel.vertexWithAdjacentEdgesOuts = []})) vertices)
          vertexMap1 =
                  Lists.foldl (\vmap -> \edge ->
                    let label = PgModel.edgeLabel edge
                        edgeId = PgModel.edgeId edge
                        outV = PgModel.edgeOut edge
                        inV = PgModel.edgeIn edge
                        props = PgModel.edgeProperties edge
                        adjEdgeOut =
                                PgModel.AdjacentEdge {
                                  PgModel.adjacentEdgeLabel = label,
                                  PgModel.adjacentEdgeId = edgeId,
                                  PgModel.adjacentEdgeVertex = inV,
                                  PgModel.adjacentEdgeProperties = props}
                        adjEdgeIn =
                                PgModel.AdjacentEdge {
                                  PgModel.adjacentEdgeLabel = label,
                                  PgModel.adjacentEdgeId = edgeId,
                                  PgModel.adjacentEdgeVertex = outV,
                                  PgModel.adjacentEdgeProperties = props}
                        vmap1 =
                                Maybes.maybe vmap (\vae -> Maps.insert outV (PgModel.VertexWithAdjacentEdges {
                                  PgModel.vertexWithAdjacentEdgesVertex = (PgModel.vertexWithAdjacentEdgesVertex vae),
                                  PgModel.vertexWithAdjacentEdgesIns = (PgModel.vertexWithAdjacentEdgesIns vae),
                                  PgModel.vertexWithAdjacentEdgesOuts = (Lists.cons adjEdgeOut (PgModel.vertexWithAdjacentEdgesOuts vae))}) vmap) (Maps.lookup outV vmap)
                    in (Maybes.maybe vmap1 (\vae -> Maps.insert inV (PgModel.VertexWithAdjacentEdges {
                      PgModel.vertexWithAdjacentEdgesVertex = (PgModel.vertexWithAdjacentEdgesVertex vae),
                      PgModel.vertexWithAdjacentEdgesIns = (Lists.cons adjEdgeIn (PgModel.vertexWithAdjacentEdgesIns vae)),
                      PgModel.vertexWithAdjacentEdgesOuts = (PgModel.vertexWithAdjacentEdgesOuts vae)}) vmap1) (Maps.lookup inV vmap1))) vertexMap0 edges
      in (Maps.elems vertexMap1)

-- | Encode a String value as a GraphSON Value
encodeStringValue :: String -> Either t0 Syntax.Value
encodeStringValue s = Right (Syntax.ValueString s)

-- | Encode a Hydra Term as a GraphSON Value. Supports literals and unit values.
encodeTermValue :: Core.Term -> Either Errors.Error Syntax.Value
encodeTermValue term =
    case (Strip.deannotateTerm term) of
      Core.TermLiteral v0 -> case v0 of
        Core.LiteralBinary v1 -> Right (Syntax.ValueBinary (Literals.binaryToString v1))
        Core.LiteralBoolean v1 -> Right (Syntax.ValueBoolean v1)
        Core.LiteralFloat v1 -> case v1 of
          Core.FloatValueBigfloat v2 -> Right (Syntax.ValueBigDecimal (Syntax.BigDecimalValue (Literals.showBigfloat v2)))
          Core.FloatValueFloat32 v2 -> Right (Syntax.ValueFloat (Syntax.FloatValueFinite v2))
          Core.FloatValueFloat64 v2 -> Right (Syntax.ValueDouble (Syntax.DoubleValueFinite v2))
          _ -> Left (Errors.ErrorOther (Errors.OtherError "unsupported float type"))
        Core.LiteralInteger v1 -> case v1 of
          Core.IntegerValueBigint v2 -> Right (Syntax.ValueBigInteger v2)
          Core.IntegerValueInt32 v2 -> Right (Syntax.ValueInteger v2)
          Core.IntegerValueInt64 v2 -> Right (Syntax.ValueLong v2)
          _ -> Left (Errors.ErrorOther (Errors.OtherError "unsupported integer type"))
        Core.LiteralString v1 -> Right (Syntax.ValueString v1)
        _ -> Left (Errors.ErrorOther (Errors.OtherError "unsupported literal type for GraphSON encoding"))
      Core.TermUnit -> Right Syntax.ValueNull
      _ -> Left (Errors.ErrorOther (Errors.OtherError "unsupported term variant for GraphSON encoding"))

-- | Convert property graph elements to a list of GraphSON JSON values
pgElementsToGraphson :: Ord t0 => ((t0 -> Either t1 Syntax.Value) -> [PgModel.Element t0] -> Either t1 [JsonModel.Value])
pgElementsToGraphson encodeValue els =
    Eithers.mapList (Construct.pgVertexWithAdjacentEdgesToJson encodeValue) (elementsToVerticesWithAdjacentEdges els)
