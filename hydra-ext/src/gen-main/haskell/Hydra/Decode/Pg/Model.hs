-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.pg.model

module Hydra.Decode.Pg.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Pg.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

adjacentEdge :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Model.AdjacentEdge t0)
adjacentEdge v cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "label" edgeLabel fieldMap cx) (\field_label -> Eithers.bind (Core_.requireField "id" v fieldMap cx) (\field_id -> Eithers.bind (Core_.requireField "vertex" v fieldMap cx) (\field_vertex -> Eithers.bind (Core_.requireField "properties" (Core_.decodeMap propertyKey v) fieldMap cx) (\field_properties -> Right (Model.AdjacentEdge {
          Model.adjacentEdgeLabel = field_label,
          Model.adjacentEdgeId = field_id,
          Model.adjacentEdgeVertex = field_vertex,
          Model.adjacentEdgeProperties = field_properties}))))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

direction :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.Direction
direction cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "out", (\input -> Eithers.map (\t -> Model.DirectionOut) (Core_.decodeUnit cx input))),
                      (Core.Name "in", (\input -> Eithers.map (\t -> Model.DirectionIn) (Core_.decodeUnit cx input))),
                      (Core.Name "both", (\input -> Eithers.map (\t -> Model.DirectionBoth) (Core_.decodeUnit cx input))),
                      (Core.Name "undirected", (\input -> Eithers.map (\t -> Model.DirectionUndirected) (Core_.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

edge :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Model.Edge t0)
edge v cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "label" edgeLabel fieldMap cx) (\field_label -> Eithers.bind (Core_.requireField "id" v fieldMap cx) (\field_id -> Eithers.bind (Core_.requireField "out" v fieldMap cx) (\field_out -> Eithers.bind (Core_.requireField "in" v fieldMap cx) (\field_in -> Eithers.bind (Core_.requireField "properties" (Core_.decodeMap propertyKey v) fieldMap cx) (\field_properties -> Right (Model.Edge {
          Model.edgeLabel = field_label,
          Model.edgeId = field_id,
          Model.edgeOut = field_out,
          Model.edgeIn = field_in,
          Model.edgeProperties = field_properties})))))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

edgeLabel :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.EdgeLabel
edgeLabel cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Model.EdgeLabel b) ((\raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

edgeType :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Model.EdgeType t0)
edgeType t cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "label" edgeLabel fieldMap cx) (\field_label -> Eithers.bind (Core_.requireField "id" t fieldMap cx) (\field_id -> Eithers.bind (Core_.requireField "out" vertexLabel fieldMap cx) (\field_out -> Eithers.bind (Core_.requireField "in" vertexLabel fieldMap cx) (\field_in -> Eithers.bind (Core_.requireField "properties" (Core_.decodeList (propertyType t)) fieldMap cx) (\field_properties -> Right (Model.EdgeType {
          Model.edgeTypeLabel = field_label,
          Model.edgeTypeId = field_id,
          Model.edgeTypeOut = field_out,
          Model.edgeTypeIn = field_in,
          Model.edgeTypeProperties = field_properties})))))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

element :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Model.Element t0)
element v cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "vertex", (\input -> Eithers.map (\t -> Model.ElementVertex t) (vertex v cx input))),
                      (Core.Name "edge", (\input -> Eithers.map (\t -> Model.ElementEdge t) (edge v cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

elementKind :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.ElementKind
elementKind cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "vertex", (\input -> Eithers.map (\t -> Model.ElementKindVertex) (Core_.decodeUnit cx input))),
                      (Core.Name "edge", (\input -> Eithers.map (\t -> Model.ElementKindEdge) (Core_.decodeUnit cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

elementTree :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Model.ElementTree t0)
elementTree v cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "self" (element v) fieldMap cx) (\field_self -> Eithers.bind (Core_.requireField "dependencies" (Core_.decodeList (elementTree v)) fieldMap cx) (\field_dependencies -> Right (Model.ElementTree {
          Model.elementTreeSelf = field_self,
          Model.elementTreeDependencies = field_dependencies}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

elementType :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Model.ElementType t0)
elementType t cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "vertex", (\input -> Eithers.map (\t2 -> Model.ElementTypeVertex t2) (vertexType t cx input))),
                      (Core.Name "edge", (\input -> Eithers.map (\t2 -> Model.ElementTypeEdge t2) (edgeType t cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

elementTypeTree :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Model.ElementTypeTree t0)
elementTypeTree t cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "self" (elementType t) fieldMap cx) (\field_self -> Eithers.bind (Core_.requireField "dependencies" (Core_.decodeList (elementTypeTree t)) fieldMap cx) (\field_dependencies -> Right (Model.ElementTypeTree {
          Model.elementTypeTreeSelf = field_self,
          Model.elementTypeTreeDependencies = field_dependencies}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

graph :: Ord t0 => ((Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Model.Graph t0))
graph v cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "vertices" (Core_.decodeMap v (vertex v)) fieldMap cx) (\field_vertices -> Eithers.bind (Core_.requireField "edges" (Core_.decodeMap v (edge v)) fieldMap cx) (\field_edges -> Right (Model.Graph {
          Model.graphVertices = field_vertices,
          Model.graphEdges = field_edges}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

graphSchema :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Model.GraphSchema t0)
graphSchema t cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "vertices" (Core_.decodeMap vertexLabel (vertexType t)) fieldMap cx) (\field_vertices -> Eithers.bind (Core_.requireField "edges" (Core_.decodeMap edgeLabel (edgeType t)) fieldMap cx) (\field_edges -> Right (Model.GraphSchema {
          Model.graphSchemaVertices = field_vertices,
          Model.graphSchemaEdges = field_edges}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

label :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.Label
label cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermUnion v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "vertex", (\input -> Eithers.map (\t -> Model.LabelVertex t) (vertexLabel cx input))),
                      (Core.Name "edge", (\input -> Eithers.map (\t -> Model.LabelEdge t) (edgeLabel cx input)))]
        in (Maybes.maybe (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
      _ -> Left (Errors.DecodingError "expected union")) (Lexical.stripAndDereferenceTermEither cx raw)

lazyGraph :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Model.LazyGraph t0)
lazyGraph v cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "vertices" (Core_.decodeList (vertex v)) fieldMap cx) (\field_vertices -> Eithers.bind (Core_.requireField "edges" (Core_.decodeList (edge v)) fieldMap cx) (\field_edges -> Right (Model.LazyGraph {
          Model.lazyGraphVertices = field_vertices,
          Model.lazyGraphEdges = field_edges}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

property :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Model.Property t0)
property v cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "key" propertyKey fieldMap cx) (\field_key -> Eithers.bind (Core_.requireField "value" v fieldMap cx) (\field_value -> Right (Model.Property {
          Model.propertyKey = field_key,
          Model.propertyValue = field_value}))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

propertyKey :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.PropertyKey
propertyKey cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Model.PropertyKey b) ((\raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

propertyType :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Model.PropertyType t0)
propertyType t cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "key" propertyKey fieldMap cx) (\field_key -> Eithers.bind (Core_.requireField "value" t fieldMap cx) (\field_value -> Eithers.bind (Core_.requireField "required" (\cx2 -> \raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBoolean v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected boolean literal")
          _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx2 raw2)) fieldMap cx) (\field_required -> Right (Model.PropertyType {
          Model.propertyTypeKey = field_key,
          Model.propertyTypeValue = field_value,
          Model.propertyTypeRequired = field_required})))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

vertex :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Model.Vertex t0)
vertex v cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "label" vertexLabel fieldMap cx) (\field_label -> Eithers.bind (Core_.requireField "id" v fieldMap cx) (\field_id -> Eithers.bind (Core_.requireField "properties" (Core_.decodeMap propertyKey v) fieldMap cx) (\field_properties -> Right (Model.Vertex {
          Model.vertexLabel = field_label,
          Model.vertexId = field_id,
          Model.vertexProperties = field_properties})))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

vertexLabel :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Model.VertexLabel
vertexLabel cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Model.VertexLabel b) ((\raw2 -> Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (Lexical.stripAndDereferenceTermEither cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (Lexical.stripAndDereferenceTermEither cx raw)

vertexType :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Model.VertexType t0)
vertexType t cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "label" vertexLabel fieldMap cx) (\field_label -> Eithers.bind (Core_.requireField "id" t fieldMap cx) (\field_id -> Eithers.bind (Core_.requireField "properties" (Core_.decodeList (propertyType t)) fieldMap cx) (\field_properties -> Right (Model.VertexType {
          Model.vertexTypeLabel = field_label,
          Model.vertexTypeId = field_id,
          Model.vertexTypeProperties = field_properties})))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)

vertexWithAdjacentEdges :: (Graph.Graph -> Core.Term -> Either Errors.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Errors.DecodingError (Model.VertexWithAdjacentEdges t0)
vertexWithAdjacentEdges v cx raw =
    Eithers.either (\err -> Left (Errors.DecodingError err)) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = Core_.toFieldMap v0
        in (Eithers.bind (Core_.requireField "vertex" (vertex v) fieldMap cx) (\field_vertex -> Eithers.bind (Core_.requireField "ins" (Core_.decodeList (adjacentEdge v)) fieldMap cx) (\field_ins -> Eithers.bind (Core_.requireField "outs" (Core_.decodeList (adjacentEdge v)) fieldMap cx) (\field_outs -> Right (Model.VertexWithAdjacentEdges {
          Model.vertexWithAdjacentEdgesVertex = field_vertex,
          Model.vertexWithAdjacentEdgesIns = field_ins,
          Model.vertexWithAdjacentEdgesOuts = field_outs})))))
      _ -> Left (Errors.DecodingError "expected record")) (Lexical.stripAndDereferenceTermEither cx raw)
