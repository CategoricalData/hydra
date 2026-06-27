-- Note: this is an automatically generated file. Do not edit.
-- | The TinkerPop "Modern" property graph (schema and data), encoded as Hydra terms. The canonical example graph used throughout Apache TinkerPop documentation.

module Hydra.Tinkerpop.Examples.Modern where
import qualified Hydra.Core as Core
import qualified Hydra.Pg.Model as Model
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | The TinkerPop Modern property graph data, with int32 Literal ids
modernGraph :: Model.Graph Core.Literal
modernGraph =
    Model.Graph {
      Model.graphVertices = (M.fromList [
        (
          Core.LiteralInteger (Core.IntegerValueInt32 1),
          Model.Vertex {
            Model.vertexLabel = (Model.VertexLabel "person"),
            Model.vertexId = (Core.LiteralInteger (Core.IntegerValueInt32 1)),
            Model.vertexProperties = (M.fromList [
              (Model.PropertyKey "age", (Core.LiteralInteger (Core.IntegerValueInt32 29))),
              (Model.PropertyKey "name", (Core.LiteralString "marko"))])}),
        (
          Core.LiteralInteger (Core.IntegerValueInt32 2),
          Model.Vertex {
            Model.vertexLabel = (Model.VertexLabel "person"),
            Model.vertexId = (Core.LiteralInteger (Core.IntegerValueInt32 2)),
            Model.vertexProperties = (M.fromList [
              (Model.PropertyKey "age", (Core.LiteralInteger (Core.IntegerValueInt32 27))),
              (Model.PropertyKey "name", (Core.LiteralString "vadas"))])}),
        (
          Core.LiteralInteger (Core.IntegerValueInt32 3),
          Model.Vertex {
            Model.vertexLabel = (Model.VertexLabel "software"),
            Model.vertexId = (Core.LiteralInteger (Core.IntegerValueInt32 3)),
            Model.vertexProperties = (M.fromList [
              (Model.PropertyKey "lang", (Core.LiteralString "java")),
              (Model.PropertyKey "name", (Core.LiteralString "lop"))])}),
        (
          Core.LiteralInteger (Core.IntegerValueInt32 4),
          Model.Vertex {
            Model.vertexLabel = (Model.VertexLabel "person"),
            Model.vertexId = (Core.LiteralInteger (Core.IntegerValueInt32 4)),
            Model.vertexProperties = (M.fromList [
              (Model.PropertyKey "age", (Core.LiteralInteger (Core.IntegerValueInt32 32))),
              (Model.PropertyKey "name", (Core.LiteralString "josh"))])}),
        (
          Core.LiteralInteger (Core.IntegerValueInt32 5),
          Model.Vertex {
            Model.vertexLabel = (Model.VertexLabel "software"),
            Model.vertexId = (Core.LiteralInteger (Core.IntegerValueInt32 5)),
            Model.vertexProperties = (M.fromList [
              (Model.PropertyKey "lang", (Core.LiteralString "java")),
              (Model.PropertyKey "name", (Core.LiteralString "ripple"))])}),
        (
          Core.LiteralInteger (Core.IntegerValueInt32 6),
          Model.Vertex {
            Model.vertexLabel = (Model.VertexLabel "person"),
            Model.vertexId = (Core.LiteralInteger (Core.IntegerValueInt32 6)),
            Model.vertexProperties = (M.fromList [
              (Model.PropertyKey "age", (Core.LiteralInteger (Core.IntegerValueInt32 35))),
              (Model.PropertyKey "name", (Core.LiteralString "peter"))])})]),
      Model.graphEdges = (M.fromList [
        (
          Core.LiteralInteger (Core.IntegerValueInt32 7),
          Model.Edge {
            Model.edgeLabel = (Model.EdgeLabel "knows"),
            Model.edgeId = (Core.LiteralInteger (Core.IntegerValueInt32 7)),
            Model.edgeOut = (Core.LiteralInteger (Core.IntegerValueInt32 1)),
            Model.edgeIn = (Core.LiteralInteger (Core.IntegerValueInt32 2)),
            Model.edgeProperties = (M.fromList [
              (Model.PropertyKey "weight", (Core.LiteralFloat (Core.FloatValueFloat64 0.5)))])}),
        (
          Core.LiteralInteger (Core.IntegerValueInt32 8),
          Model.Edge {
            Model.edgeLabel = (Model.EdgeLabel "knows"),
            Model.edgeId = (Core.LiteralInteger (Core.IntegerValueInt32 8)),
            Model.edgeOut = (Core.LiteralInteger (Core.IntegerValueInt32 1)),
            Model.edgeIn = (Core.LiteralInteger (Core.IntegerValueInt32 4)),
            Model.edgeProperties = (M.fromList [
              (Model.PropertyKey "weight", (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))])}),
        (
          Core.LiteralInteger (Core.IntegerValueInt32 9),
          Model.Edge {
            Model.edgeLabel = (Model.EdgeLabel "created"),
            Model.edgeId = (Core.LiteralInteger (Core.IntegerValueInt32 9)),
            Model.edgeOut = (Core.LiteralInteger (Core.IntegerValueInt32 1)),
            Model.edgeIn = (Core.LiteralInteger (Core.IntegerValueInt32 3)),
            Model.edgeProperties = (M.fromList [
              (Model.PropertyKey "weight", (Core.LiteralFloat (Core.FloatValueFloat64 0.4)))])}),
        (
          Core.LiteralInteger (Core.IntegerValueInt32 10),
          Model.Edge {
            Model.edgeLabel = (Model.EdgeLabel "created"),
            Model.edgeId = (Core.LiteralInteger (Core.IntegerValueInt32 10)),
            Model.edgeOut = (Core.LiteralInteger (Core.IntegerValueInt32 4)),
            Model.edgeIn = (Core.LiteralInteger (Core.IntegerValueInt32 5)),
            Model.edgeProperties = (M.fromList [
              (Model.PropertyKey "weight", (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))])}),
        (
          Core.LiteralInteger (Core.IntegerValueInt32 11),
          Model.Edge {
            Model.edgeLabel = (Model.EdgeLabel "created"),
            Model.edgeId = (Core.LiteralInteger (Core.IntegerValueInt32 11)),
            Model.edgeOut = (Core.LiteralInteger (Core.IntegerValueInt32 4)),
            Model.edgeIn = (Core.LiteralInteger (Core.IntegerValueInt32 3)),
            Model.edgeProperties = (M.fromList [
              (Model.PropertyKey "weight", (Core.LiteralFloat (Core.FloatValueFloat64 0.4)))])}),
        (
          Core.LiteralInteger (Core.IntegerValueInt32 12),
          Model.Edge {
            Model.edgeLabel = (Model.EdgeLabel "created"),
            Model.edgeId = (Core.LiteralInteger (Core.IntegerValueInt32 12)),
            Model.edgeOut = (Core.LiteralInteger (Core.IntegerValueInt32 6)),
            Model.edgeIn = (Core.LiteralInteger (Core.IntegerValueInt32 3)),
            Model.edgeProperties = (M.fromList [
              (Model.PropertyKey "weight", (Core.LiteralFloat (Core.FloatValueFloat64 0.2)))])})])}
-- | The TinkerPop Modern property graph schema, with LiteralType property/id types
modernSchema :: Model.GraphSchema Core.LiteralType
modernSchema =
    Model.GraphSchema {
      Model.graphSchemaVertices = (M.fromList [
        (
          Model.VertexLabel "person",
          Model.VertexType {
            Model.vertexTypeLabel = (Model.VertexLabel "person"),
            Model.vertexTypeId = (Core.LiteralTypeInteger Core.IntegerTypeInt32),
            Model.vertexTypeProperties = [
              Model.PropertyType {
                Model.propertyTypeKey = (Model.PropertyKey "name"),
                Model.propertyTypeValue = Core.LiteralTypeString,
                Model.propertyTypeRequired = True},
              Model.PropertyType {
                Model.propertyTypeKey = (Model.PropertyKey "age"),
                Model.propertyTypeValue = (Core.LiteralTypeInteger Core.IntegerTypeInt32),
                Model.propertyTypeRequired = False}]}),
        (
          Model.VertexLabel "software",
          Model.VertexType {
            Model.vertexTypeLabel = (Model.VertexLabel "software"),
            Model.vertexTypeId = (Core.LiteralTypeInteger Core.IntegerTypeInt32),
            Model.vertexTypeProperties = [
              Model.PropertyType {
                Model.propertyTypeKey = (Model.PropertyKey "name"),
                Model.propertyTypeValue = Core.LiteralTypeString,
                Model.propertyTypeRequired = True},
              Model.PropertyType {
                Model.propertyTypeKey = (Model.PropertyKey "lang"),
                Model.propertyTypeValue = Core.LiteralTypeString,
                Model.propertyTypeRequired = True}]})]),
      Model.graphSchemaEdges = (M.fromList [
        (
          Model.EdgeLabel "created",
          Model.EdgeType {
            Model.edgeTypeLabel = (Model.EdgeLabel "created"),
            Model.edgeTypeId = (Core.LiteralTypeInteger Core.IntegerTypeInt32),
            Model.edgeTypeOut = (Model.VertexLabel "person"),
            Model.edgeTypeIn = (Model.VertexLabel "software"),
            Model.edgeTypeProperties = [
              Model.PropertyType {
                Model.propertyTypeKey = (Model.PropertyKey "weight"),
                Model.propertyTypeValue = (Core.LiteralTypeFloat Core.FloatTypeFloat64),
                Model.propertyTypeRequired = True}]}),
        (
          Model.EdgeLabel "knows",
          Model.EdgeType {
            Model.edgeTypeLabel = (Model.EdgeLabel "knows"),
            Model.edgeTypeId = (Core.LiteralTypeInteger Core.IntegerTypeInt32),
            Model.edgeTypeOut = (Model.VertexLabel "person"),
            Model.edgeTypeIn = (Model.VertexLabel "person"),
            Model.edgeTypeProperties = [
              Model.PropertyType {
                Model.propertyTypeKey = (Model.PropertyKey "weight"),
                Model.propertyTypeValue = (Core.LiteralTypeFloat Core.FloatTypeFloat64),
                Model.propertyTypeRequired = True}]})])}
