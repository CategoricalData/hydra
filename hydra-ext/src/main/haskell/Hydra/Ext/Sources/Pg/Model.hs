module Hydra.Ext.Sources.Pg.Model where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel                    hiding (edgeType, graphSchema)
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:), (@@))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: Namespace
ns = Namespace "hydra.pg.model"

define :: String -> Type -> Binding
define = defineType ns

pg :: String -> Type
pg = typeref ns

module_ :: Module
module_ = Module ns elements [] [Core.ns] $
    Just ("A typed property graph data model. " ++
      "Property graphs are parameterized a type for property and id values, " ++
      "while property graph schemas are parameterized by a type for property and id types")
  where
    elements = [
      adjacentEdge,
      direction,
      edge,
      edgeLabel,
      edgeType,
      element,
      elementKind,
      elementTree,
      elementType,
      elementTypeTree,
      graph,
      graphSchema,
      label,
      lazyGraph,
      property_,
      propertyKey,
      propertyType,
      vertex,
      vertexLabel,
      vertexType,
      vertexWithAdjacentEdges]

adjacentEdge :: Binding
adjacentEdge = define "AdjacentEdge" $
  doc "An edge which is adjacent to a given vertex. Only the other endpoint of the edge is provided." $
  T.forAll "v" $ T.record [
    "label">:
      doc "The label of the edge" $
      pg "EdgeLabel",
    "id">:
      doc "The unique identifier of the edge"
      "v",
    "vertex">:
      doc "The id of the other vertex adjacent to the edge"
      "v",
    "properties">:
      doc "A key/value map of edge properties" $
      T.map (pg "PropertyKey") "v"]

direction :: Binding
direction = define "Direction" $
  doc "The direction of an edge or edge pattern" $
    T.enum ["out", "in", "both", "undirected"]

edge :: Binding
edge = define "Edge" $
  doc "An edge" $
  T.forAll "v" $ T.record [
    "label">:
      doc "The label of the edge" $
      pg "EdgeLabel",
    "id">:
      doc "The unique identifier of the edge"
      "v",
    "out">:
      doc "The id of the out-vertex (tail) of the edge"
      "v",
    "in">:
      doc "The id of the in-vertex (head) of the edge"
      "v",
    "properties">:
      doc "A key/value map of edge properties" $
      T.map (pg "PropertyKey") "v"]

edgeLabel :: Binding
edgeLabel = define "EdgeLabel" $
  doc "The label of an edge" $
  T.wrap T.string

edgeType :: Binding
edgeType = define "EdgeType" $
  doc "The type of an edge" $
  T.forAll "t" $ T.record [
    "label">:
      doc "The label of any edge of this edge type" $
      pg "EdgeLabel",
    "id">:
      doc "The type of the id of any edge of this edge type"
      "t",
    "out">:
      doc "The label of the out-vertex (tail) of any edge of this edge type" $
      pg "VertexLabel",
    "in">:
      doc "The label of the in-vertex (head) of any edge of this edge type" $
      pg "VertexLabel",
    "properties">:
      doc "A list of property types. The types are ordered for the sake of applications in which property order is significant." $
      T.list (pg "PropertyType" @@ "t")]

element :: Binding
element = define "Element" $
  doc "Either a vertex or an edge" $
  T.forAll "v" $ T.union [
    "vertex">: pg "Vertex" @@ "v",
    "edge">: pg "Edge" @@ "v"]

elementKind :: Binding
elementKind = define "ElementKind" $
  doc "The kind of an element: vertex or edge" $
  T.enum ["vertex", "edge"]

elementTree :: Binding
elementTree = define "ElementTree" $
  doc "An element together with its dependencies in some context" $
  T.forAll "v" $ T.record [
    "self">: pg "Element" @@ "v",
    "dependencies">: T.list $ pg "ElementTree" @@ "v"]

elementType :: Binding
elementType = define "ElementType" $
  doc "The type of a vertex or edge" $
  T.forAll "t" $ T.union [
    "vertex">: pg "VertexType" @@ "t",
    "edge">: pg "EdgeType" @@ "t"]

elementTypeTree :: Binding
elementTypeTree = define "ElementTypeTree" $
  doc "An element type together with its dependencies in some context" $
  T.forAll "t" $ T.record [
    "self">: pg "ElementType" @@ "t",
    "dependencies">: T.list $ pg "ElementTypeTree" @@ "t"]

graph :: Binding
graph = define "Graph" $
  doc "A graph; a self-contained collection of vertices and edges" $
  T.forAll "v" $ T.record [
    "vertices">: T.map "v" $ pg "Vertex" @@ "v",
    "edges">: T.map "v" $ pg "Edge" @@ "v"]

graphSchema :: Binding
graphSchema = define "GraphSchema" $
  doc "A graph schema; a vertex and edge types for the vertices and edges of a graph conforming to the schema" $
  T.forAll "t" $ T.record [
    "vertices">:
      doc "A unique vertex type for each vertex label which may occur in a graph" $
      T.map (pg "VertexLabel") (pg "VertexType" @@ "t"),
    "edges">:
      doc "A unique edge type for each edge label which may occur in a graph" $
      T.map (pg "EdgeLabel") (pg "EdgeType" @@ "t")]

label :: Binding
label = define "Label" $
  doc "Either a vertex or edge label" $
  T.union [
    "vertex">: pg "VertexLabel",
    "edge">: pg "EdgeLabel"]

lazyGraph :: Binding
lazyGraph = define "LazyGraph" $
  doc ("A graph which does not assume that vertex or edge ids are unique."
    <> " This is useful in mappings because the id specifications for vertices and/or edges may be non-unique.") $
  T.forAll "v" $ T.record [
    "vertices">: T.list $ pg "Vertex" @@ "v",
    "edges">: T.list $ pg "Edge" @@ "v"]

property_ :: Binding
property_ = define "Property" $
  doc "A key/value property" $
  T.forAll "v" $ T.record [
    "key">:
      doc "They key of the property" $
      pg "PropertyKey",
    "value">:
      doc "The value of the property"
      "v"]

propertyKey :: Binding
propertyKey = define "PropertyKey" $
  doc "A property key" $
  T.wrap T.string

propertyType :: Binding
propertyType = define "PropertyType" $
  doc "The type of a property" $
  T.forAll "t" $ T.record [
    "key">:
      doc "A property's key" $
      pg "PropertyKey",
    "value">:
      doc "The type of a property's value"
      "t",
    "required">:
      doc "Whether the property is required; values may be omitted from a property map otherwise"
      T.boolean]

vertex :: Binding
vertex = define "Vertex" $
  doc "A vertex" $
  T.forAll "v" $ T.record [
    "label">:
      doc "The label of the vertex" $
      pg "VertexLabel",
    "id">:
      doc "The unique identifier of the vertex"
      "v",
    "properties">:
      doc "A key/value map of vertex properties" $
      T.map (pg "PropertyKey") "v"]

vertexLabel :: Binding
vertexLabel = define "VertexLabel" $
  doc "The label of a vertex. The default (null) vertex is represented by the empty string" $
  T.wrap $ T.string

vertexType :: Binding
vertexType = define "VertexType" $
  doc "The type of a vertex" $
  T.forAll "t" $ T.record [
    "label">:
      doc "The label of any vertex of this vertex type" $
      pg "VertexLabel",
    "id">:
      doc "The type of the id of any vertex of this vertex type"
      "t",
    "properties">:
      doc "A list of property types. The types are ordered for the sake of applications in which property order is significant." $
      T.list (pg "PropertyType" @@ "t")]

vertexWithAdjacentEdges :: Binding
vertexWithAdjacentEdges = define "VertexWithAdjacentEdges" $
  doc "A vertex together with any outgoing and/or incoming edges; a vertex object" $
  T.forAll "v" $ T.record [
    "vertex">:
      doc "The focus vertex" $
      pg "Vertex" @@ "v",
    "ins">:
      doc "An adjacency list of edges in which the focus vertex is the head (in-vertex) of the edge" $
      T.list (pg "AdjacentEdge" @@ "v"),
    "outs">:
      doc "An adjacency list of edges in which the focus vertex is the tail (out-vertex) of the edge" $
      T.list (pg "AdjacentEdge" @@ "v")]
