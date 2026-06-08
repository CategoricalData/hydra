-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.pg.model

module Hydra.Encode.Pg.Model where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Pg.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.pg.model.AdjacentEdge
adjacentEdge :: (t0 -> Core.Term) -> Model.AdjacentEdge t0 -> Core.Term
adjacentEdge v x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.AdjacentEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (edgeLabel (Model.adjacentEdgeLabel x))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (v (Model.adjacentEdgeId x))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (v (Model.adjacentEdgeVertex x))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap propertyKey v m)) (Model.adjacentEdgeProperties x))}]})
-- | Encoder for hydra.pg.model.Direction
direction :: Model.Direction -> Core.Term
direction x =
    case x of
      Model.DirectionOut -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = Core.TermUnit}})
      Model.DirectionIn -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = Core.TermUnit}})
      Model.DirectionBoth -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "both"),
          Core.fieldTerm = Core.TermUnit}})
      Model.DirectionUndirected -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.pg.model.Direction"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "undirected"),
          Core.fieldTerm = Core.TermUnit}})
-- | Encoder for hydra.pg.model.Edge
edge :: (t0 -> Core.Term) -> Model.Edge t0 -> Core.Term
edge v x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Edge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (edgeLabel (Model.edgeLabel x))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (v (Model.edgeId x))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (v (Model.edgeOut x))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (v (Model.edgeIn x))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap propertyKey v m)) (Model.edgeProperties x))}]})
-- | Encoder for hydra.pg.model.EdgeLabel
edgeLabel :: Model.EdgeLabel -> Core.Term
edgeLabel x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.model.EdgeLabel"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Model.unEdgeLabel x))})
-- | Encoder for hydra.pg.model.EdgeType
edgeType :: (t0 -> Core.Term) -> Model.EdgeType t0 -> Core.Term
edgeType t x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.EdgeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (edgeLabel (Model.edgeTypeLabel x))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (t (Model.edgeTypeId x))},
        Core.Field {
          Core.fieldName = (Core.Name "out"),
          Core.fieldTerm = (vertexLabel (Model.edgeTypeOut x))},
        Core.Field {
          Core.fieldName = (Core.Name "in"),
          Core.fieldTerm = (vertexLabel (Model.edgeTypeIn x))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (propertyType t) xs)) (Model.edgeTypeProperties x))}]})
-- | Encoder for hydra.pg.model.Element
element :: (t0 -> Core.Term) -> Model.Element t0 -> Core.Term
element v x =
    case x of
      Model.ElementVertex v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.pg.model.Element"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (vertex v v0)}})
      Model.ElementEdge v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.pg.model.Element"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (edge v v0)}})
-- | Encoder for hydra.pg.model.ElementKind
elementKind :: Model.ElementKind -> Core.Term
elementKind x =
    case x of
      Model.ElementKindVertex -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementKind"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = Core.TermUnit}})
      Model.ElementKindEdge -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementKind"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = Core.TermUnit}})
-- | Encoder for hydra.pg.model.ElementTree
elementTree :: (t0 -> Core.Term) -> Model.ElementTree t0 -> Core.Term
elementTree v x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (element v (Model.elementTreeSelf x))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (elementTree v) xs)) (Model.elementTreeDependencies x))}]})
-- | Encoder for hydra.pg.model.ElementType
elementType :: (t0 -> Core.Term) -> Model.ElementType t0 -> Core.Term
elementType t x =
    case x of
      Model.ElementTypeVertex v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (vertexType t v0)}})
      Model.ElementTypeEdge v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.pg.model.ElementType"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (edgeType t v0)}})
-- | Encoder for hydra.pg.model.ElementTypeTree
elementTypeTree :: (t0 -> Core.Term) -> Model.ElementTypeTree t0 -> Core.Term
elementTypeTree t x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.ElementTypeTree"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "self"),
          Core.fieldTerm = (elementType t (Model.elementTypeTreeSelf x))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (elementTypeTree t) xs)) (Model.elementTypeTreeDependencies x))}]})
-- | Encoder for hydra.pg.model.Graph
graph :: Ord t0 => ((t0 -> Core.Term) -> Model.Graph t0 -> Core.Term)
graph v x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap v (vertex v) m)) (Model.graphVertices x))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap v (edge v) m)) (Model.graphEdges x))}]})
-- | Encoder for hydra.pg.model.GraphSchema
graphSchema :: (t0 -> Core.Term) -> Model.GraphSchema t0 -> Core.Term
graphSchema t x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.GraphSchema"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap vertexLabel (vertexType t) m)) (Model.graphSchemaVertices x))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap edgeLabel (edgeType t) m)) (Model.graphSchemaEdges x))}]})
-- | Encoder for hydra.pg.model.Label
label :: Model.Label -> Core.Term
label x =
    case x of
      Model.LabelVertex v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.pg.model.Label"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (vertexLabel v0)}})
      Model.LabelEdge v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.pg.model.Label"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (edgeLabel v0)}})
-- | Encoder for hydra.pg.model.LazyGraph
lazyGraph :: (t0 -> Core.Term) -> Model.LazyGraph t0 -> Core.Term
lazyGraph v x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.LazyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertices"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (vertex v) xs)) (Model.lazyGraphVertices x))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (edge v) xs)) (Model.lazyGraphEdges x))}]})
-- | Encoder for hydra.pg.model.Property
property :: (t0 -> Core.Term) -> Model.Property t0 -> Core.Term
property v x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (propertyKey (Model.propertyKey x))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (v (Model.propertyValue x))}]})
-- | Encoder for hydra.pg.model.PropertyKey
propertyKey :: Model.PropertyKey -> Core.Term
propertyKey x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.model.PropertyKey"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Model.unPropertyKey x))})
-- | Encoder for hydra.pg.model.PropertyType
propertyType :: (t0 -> Core.Term) -> Model.PropertyType t0 -> Core.Term
propertyType t x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.PropertyType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (propertyKey (Model.propertyTypeKey x))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (t (Model.propertyTypeValue x))},
        Core.Field {
          Core.fieldName = (Core.Name "required"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralBoolean x2)) (Model.propertyTypeRequired x))}]})
-- | Encoder for hydra.pg.model.Vertex
vertex :: (t0 -> Core.Term) -> Model.Vertex t0 -> Core.Term
vertex v x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.Vertex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (vertexLabel (Model.vertexLabel x))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (v (Model.vertexId x))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap propertyKey v m)) (Model.vertexProperties x))}]})
-- | Encoder for hydra.pg.model.VertexLabel
vertexLabel :: Model.VertexLabel -> Core.Term
vertexLabel x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.model.VertexLabel"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Model.unVertexLabel x))})
-- | Encoder for hydra.pg.model.VertexType
vertexType :: (t0 -> Core.Term) -> Model.VertexType t0 -> Core.Term
vertexType t x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (vertexLabel (Model.vertexTypeLabel x))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (t (Model.vertexTypeId x))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (propertyType t) xs)) (Model.vertexTypeProperties x))}]})
-- | Encoder for hydra.pg.model.VertexWithAdjacentEdges
vertexWithAdjacentEdges :: (t0 -> Core.Term) -> Model.VertexWithAdjacentEdges t0 -> Core.Term
vertexWithAdjacentEdges v x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.model.VertexWithAdjacentEdges"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (vertex v (Model.vertexWithAdjacentEdgesVertex x))},
        Core.Field {
          Core.fieldName = (Core.Name "ins"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (adjacentEdge v) xs)) (Model.vertexWithAdjacentEdgesIns x))},
        Core.Field {
          Core.fieldName = (Core.Name "outs"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (adjacentEdge v) xs)) (Model.vertexWithAdjacentEdgesOuts x))}]})
