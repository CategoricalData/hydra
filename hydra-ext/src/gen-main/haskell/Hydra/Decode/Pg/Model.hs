-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.pg.model

module Hydra.Decode.Pg.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

adjacentEdge :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Model.AdjacentEdge t0))
adjacentEdge v cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "label" edgeLabel fieldMap cx) (\field_label -> Eithers.bind (Helpers.requireField "id" v fieldMap cx) (\field_id -> Eithers.bind (Helpers.requireField "vertex" v fieldMap cx) (\field_vertex -> Eithers.bind (Helpers.requireField "properties" (Helpers.decodeMap propertyKey v) fieldMap cx) (\field_properties -> Right (Model.AdjacentEdge {
      Model.adjacentEdgeLabel = field_label,
      Model.adjacentEdgeId = field_id,
      Model.adjacentEdgeVertex = field_vertex,
      Model.adjacentEdgeProperties = field_properties}))))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.model.AdjacentEdge"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

direction :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Model.Direction)
direction cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "out", (\input -> Eithers.map (\t -> Model.DirectionOut) (Helpers.decodeUnit cx input))),
                (Core.Name "in", (\input -> Eithers.map (\t -> Model.DirectionIn) (Helpers.decodeUnit cx input))),
                (Core.Name "both", (\input -> Eithers.map (\t -> Model.DirectionBoth) (Helpers.decodeUnit cx input))),
                (Core.Name "undirected", (\input -> Eithers.map (\t -> Model.DirectionUndirected) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.pg.model.Direction"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

edge :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Model.Edge t0))
edge v cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "label" edgeLabel fieldMap cx) (\field_label -> Eithers.bind (Helpers.requireField "id" v fieldMap cx) (\field_id -> Eithers.bind (Helpers.requireField "out" v fieldMap cx) (\field_out -> Eithers.bind (Helpers.requireField "in" v fieldMap cx) (\field_in -> Eithers.bind (Helpers.requireField "properties" (Helpers.decodeMap propertyKey v) fieldMap cx) (\field_properties -> Right (Model.Edge {
      Model.edgeLabel = field_label,
      Model.edgeId = field_id,
      Model.edgeOut = field_out,
      Model.edgeIn = field_in,
      Model.edgeProperties = field_properties})))))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.model.Edge"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

edgeLabel :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Model.EdgeLabel)
edgeLabel cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Model.EdgeLabel b) ((\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
    Core.TermLiteral v2 -> ((\x -> case x of
      Core.LiteralString v3 -> (Right v3)
      _ -> (Left (Util.DecodingError "expected string literal"))) v2)
    _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.pg.model.EdgeLabel"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

edgeType :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Model.EdgeType t0))
edgeType t cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "label" edgeLabel fieldMap cx) (\field_label -> Eithers.bind (Helpers.requireField "id" t fieldMap cx) (\field_id -> Eithers.bind (Helpers.requireField "out" vertexLabel fieldMap cx) (\field_out -> Eithers.bind (Helpers.requireField "in" vertexLabel fieldMap cx) (\field_in -> Eithers.bind (Helpers.requireField "properties" (Helpers.decodeList (propertyType t)) fieldMap cx) (\field_properties -> Right (Model.EdgeType {
      Model.edgeTypeLabel = field_label,
      Model.edgeTypeId = field_id,
      Model.edgeTypeOut = field_out,
      Model.edgeTypeIn = field_in,
      Model.edgeTypeProperties = field_properties})))))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.model.EdgeType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

element :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Model.Element t0))
element v cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "vertex", (\input -> Eithers.map (\t -> Model.ElementVertex t) (vertex v cx input))),
                (Core.Name "edge", (\input -> Eithers.map (\t -> Model.ElementEdge t) (edge v cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.pg.model.Element"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

elementKind :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Model.ElementKind)
elementKind cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "vertex", (\input -> Eithers.map (\t -> Model.ElementKindVertex) (Helpers.decodeUnit cx input))),
                (Core.Name "edge", (\input -> Eithers.map (\t -> Model.ElementKindEdge) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.pg.model.ElementKind"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

elementTree :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Model.ElementTree t0))
elementTree v cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "self" (element v) fieldMap cx) (\field_self -> Eithers.bind (Helpers.requireField "dependencies" (Helpers.decodeList (elementTree v)) fieldMap cx) (\field_dependencies -> Right (Model.ElementTree {
      Model.elementTreeSelf = field_self,
      Model.elementTreeDependencies = field_dependencies}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.model.ElementTree"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

elementType :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Model.ElementType t0))
elementType t cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "vertex", (\input -> Eithers.map (\t -> Model.ElementTypeVertex t) (vertexType t cx input))),
                (Core.Name "edge", (\input -> Eithers.map (\t -> Model.ElementTypeEdge t) (edgeType t cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.pg.model.ElementType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

elementTypeTree :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Model.ElementTypeTree t0))
elementTypeTree t cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "self" (elementType t) fieldMap cx) (\field_self -> Eithers.bind (Helpers.requireField "dependencies" (Helpers.decodeList (elementTypeTree t)) fieldMap cx) (\field_dependencies -> Right (Model.ElementTypeTree {
      Model.elementTypeTreeSelf = field_self,
      Model.elementTypeTreeDependencies = field_dependencies}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.model.ElementTypeTree"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

graph :: Ord t0 => ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Model.Graph t0))
graph v cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "vertices" (Helpers.decodeMap v (vertex v)) fieldMap cx) (\field_vertices -> Eithers.bind (Helpers.requireField "edges" (Helpers.decodeMap v (edge v)) fieldMap cx) (\field_edges -> Right (Model.Graph {
      Model.graphVertices = field_vertices,
      Model.graphEdges = field_edges}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.model.Graph"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

graphSchema :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Model.GraphSchema t0))
graphSchema t cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "vertices" (Helpers.decodeMap vertexLabel (vertexType t)) fieldMap cx) (\field_vertices -> Eithers.bind (Helpers.requireField "edges" (Helpers.decodeMap edgeLabel (edgeType t)) fieldMap cx) (\field_edges -> Right (Model.GraphSchema {
      Model.graphSchemaVertices = field_vertices,
      Model.graphSchemaEdges = field_edges}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.model.GraphSchema"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

label :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Model.Label)
label cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v1 ->  
    let tname = (Core.injectionTypeName v1) 
        field = (Core.injectionField v1)
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "vertex", (\input -> Eithers.map (\t -> Model.LabelVertex t) (vertexLabel cx input))),
                (Core.Name "edge", (\input -> Eithers.map (\t -> Model.LabelEdge t) (edgeLabel cx input)))])
    in (Maybes.maybe (Left (Util.DecodingError (Strings.cat [
      "no such field ",
      Core.unName fname,
      " in union type ",
      (Core.unName tname)]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Util.DecodingError "expected union of type hydra.pg.model.Label"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

lazyGraph :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Model.LazyGraph t0))
lazyGraph v cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "vertices" (Helpers.decodeList (vertex v)) fieldMap cx) (\field_vertices -> Eithers.bind (Helpers.requireField "edges" (Helpers.decodeList (edge v)) fieldMap cx) (\field_edges -> Right (Model.LazyGraph {
      Model.lazyGraphVertices = field_vertices,
      Model.lazyGraphEdges = field_edges}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.model.LazyGraph"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

property :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Model.Property t0))
property v cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "key" propertyKey fieldMap cx) (\field_key -> Eithers.bind (Helpers.requireField "value" v fieldMap cx) (\field_value -> Right (Model.Property {
      Model.propertyKey = field_key,
      Model.propertyValue = field_value}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.model.Property"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

propertyKey :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Model.PropertyKey)
propertyKey cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Model.PropertyKey b) ((\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
    Core.TermLiteral v2 -> ((\x -> case x of
      Core.LiteralString v3 -> (Right v3)
      _ -> (Left (Util.DecodingError "expected string literal"))) v2)
    _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.pg.model.PropertyKey"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

propertyType :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Model.PropertyType t0))
propertyType t cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "key" propertyKey fieldMap cx) (\field_key -> Eithers.bind (Helpers.requireField "value" t fieldMap cx) (\field_value -> Eithers.bind (Helpers.requireField "required" (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
      Core.TermLiteral v2 -> ((\x -> case x of
        Core.LiteralBoolean v3 -> (Right v3)
        _ -> (Left (Util.DecodingError "expected boolean literal"))) v2)
      _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) fieldMap cx) (\field_required -> Right (Model.PropertyType {
      Model.propertyTypeKey = field_key,
      Model.propertyTypeValue = field_value,
      Model.propertyTypeRequired = field_required})))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.model.PropertyType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

vertex :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Model.Vertex t0))
vertex v cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "label" vertexLabel fieldMap cx) (\field_label -> Eithers.bind (Helpers.requireField "id" v fieldMap cx) (\field_id -> Eithers.bind (Helpers.requireField "properties" (Helpers.decodeMap propertyKey v) fieldMap cx) (\field_properties -> Right (Model.Vertex {
      Model.vertexLabel = field_label,
      Model.vertexId = field_id,
      Model.vertexProperties = field_properties})))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.model.Vertex"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

vertexLabel :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Model.VertexLabel)
vertexLabel cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Model.VertexLabel b) ((\raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
    Core.TermLiteral v2 -> ((\x -> case x of
      Core.LiteralString v3 -> (Right v3)
      _ -> (Left (Util.DecodingError "expected string literal"))) v2)
    _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.pg.model.VertexLabel"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

vertexType :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Model.VertexType t0))
vertexType t cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "label" vertexLabel fieldMap cx) (\field_label -> Eithers.bind (Helpers.requireField "id" t fieldMap cx) (\field_id -> Eithers.bind (Helpers.requireField "properties" (Helpers.decodeList (propertyType t)) fieldMap cx) (\field_properties -> Right (Model.VertexType {
      Model.vertexTypeLabel = field_label,
      Model.vertexTypeId = field_id,
      Model.vertexTypeProperties = field_properties})))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.model.VertexType"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

vertexWithAdjacentEdges :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Model.VertexWithAdjacentEdges t0))
vertexWithAdjacentEdges v cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "vertex" (vertex v) fieldMap cx) (\field_vertex -> Eithers.bind (Helpers.requireField "ins" (Helpers.decodeList (adjacentEdge v)) fieldMap cx) (\field_ins -> Eithers.bind (Helpers.requireField "outs" (Helpers.decodeList (adjacentEdge v)) fieldMap cx) (\field_outs -> Right (Model.VertexWithAdjacentEdges {
      Model.vertexWithAdjacentEdgesVertex = field_vertex,
      Model.vertexWithAdjacentEdgesIns = field_ins,
      Model.vertexWithAdjacentEdgesOuts = field_outs})))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.pg.model.VertexWithAdjacentEdges"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
