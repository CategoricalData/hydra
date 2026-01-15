# Note: this is an automatically generated file. Do not edit.

r"""A typed property graph data model. Property graphs are parameterized a type for property and id values, while property graph schemas are parameterized by a type for property and id types."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from hydra.dsl.python import FrozenDict, Node, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar
import hydra.core

T = TypeVar("T")
V = TypeVar("V")

@dataclass(frozen=True)
class AdjacentEdge(Generic[V]):
    r"""An edge which is adjacent to a given vertex. Only the other endpoint of the edge is provided."""
    
    label: Annotated[EdgeLabel, "The label of the edge"]
    id: Annotated[V, "The unique identifier of the edge"]
    vertex: Annotated[V, "The id of the other vertex adjacent to the edge"]
    properties: Annotated[FrozenDict[PropertyKey, V], "A key/value map of edge properties"]

ADJACENT_EDGE__NAME = hydra.core.Name("hydra.pg.model.AdjacentEdge")
ADJACENT_EDGE__LABEL__NAME = hydra.core.Name("label")
ADJACENT_EDGE__ID__NAME = hydra.core.Name("id")
ADJACENT_EDGE__VERTEX__NAME = hydra.core.Name("vertex")
ADJACENT_EDGE__PROPERTIES__NAME = hydra.core.Name("properties")

class Direction(Enum):
    r"""The direction of an edge or edge pattern."""
    
    OUT = "out"
    
    IN = "in"
    
    BOTH = "both"
    
    UNDIRECTED = "undirected"

DIRECTION__NAME = hydra.core.Name("hydra.pg.model.Direction")
DIRECTION__OUT__NAME = hydra.core.Name("out")
DIRECTION__IN__NAME = hydra.core.Name("in")
DIRECTION__BOTH__NAME = hydra.core.Name("both")
DIRECTION__UNDIRECTED__NAME = hydra.core.Name("undirected")

@dataclass(frozen=True)
class Edge(Generic[V]):
    r"""An edge."""
    
    label: Annotated[EdgeLabel, "The label of the edge"]
    id: Annotated[V, "The unique identifier of the edge"]
    out: Annotated[V, "The id of the out-vertex (tail) of the edge"]
    in_: Annotated[V, "The id of the in-vertex (head) of the edge"]
    properties: Annotated[FrozenDict[PropertyKey, V], "A key/value map of edge properties"]

EDGE__NAME = hydra.core.Name("hydra.pg.model.Edge")
EDGE__LABEL__NAME = hydra.core.Name("label")
EDGE__ID__NAME = hydra.core.Name("id")
EDGE__OUT__NAME = hydra.core.Name("out")
EDGE__IN__NAME = hydra.core.Name("in")
EDGE__PROPERTIES__NAME = hydra.core.Name("properties")

class EdgeLabel(Node[str]):
    r"""The label of an edge."""

EDGE_LABEL__NAME = hydra.core.Name("hydra.pg.model.EdgeLabel")

@dataclass(frozen=True)
class EdgeType(Generic[T]):
    r"""The type of an edge."""
    
    label: Annotated[EdgeLabel, "The label of any edge of this edge type"]
    id: Annotated[T, "The type of the id of any edge of this edge type"]
    out: Annotated[VertexLabel, "The label of the out-vertex (tail) of any edge of this edge type"]
    in_: Annotated[VertexLabel, "The label of the in-vertex (head) of any edge of this edge type"]
    properties: Annotated[frozenlist[PropertyType[T]], "A list of property types. The types are ordered for the sake of applications in which property order is significant."]

EDGE_TYPE__NAME = hydra.core.Name("hydra.pg.model.EdgeType")
EDGE_TYPE__LABEL__NAME = hydra.core.Name("label")
EDGE_TYPE__ID__NAME = hydra.core.Name("id")
EDGE_TYPE__OUT__NAME = hydra.core.Name("out")
EDGE_TYPE__IN__NAME = hydra.core.Name("in")
EDGE_TYPE__PROPERTIES__NAME = hydra.core.Name("properties")

class ElementVertex(Node["Vertex[V]"]): ...

class ElementEdge(Node["Edge[V]"]): ...

class _ElementMeta(type):
    def __getitem__(cls, item):
        return object

# Either a vertex or an edge.
class Element(metaclass=_ElementMeta):
    r"""ElementVertex[V] | ElementEdge[V]"""
    
    pass

ELEMENT__NAME = hydra.core.Name("hydra.pg.model.Element")
ELEMENT__VERTEX__NAME = hydra.core.Name("vertex")
ELEMENT__EDGE__NAME = hydra.core.Name("edge")

class ElementKind(Enum):
    r"""The kind of an element: vertex or edge."""
    
    VERTEX = "vertex"
    
    EDGE = "edge"

ELEMENT_KIND__NAME = hydra.core.Name("hydra.pg.model.ElementKind")
ELEMENT_KIND__VERTEX__NAME = hydra.core.Name("vertex")
ELEMENT_KIND__EDGE__NAME = hydra.core.Name("edge")

@dataclass(frozen=True)
class ElementTree(Generic[V]):
    r"""An element together with its dependencies in some context."""
    
    self: Element[V]
    dependencies: frozenlist[ElementTree[V]]

ELEMENT_TREE__NAME = hydra.core.Name("hydra.pg.model.ElementTree")
ELEMENT_TREE__SELF__NAME = hydra.core.Name("self")
ELEMENT_TREE__DEPENDENCIES__NAME = hydra.core.Name("dependencies")

class ElementTypeVertex(Node["VertexType[T]"]): ...

class ElementTypeEdge(Node["EdgeType[T]"]): ...

class _ElementTypeMeta(type):
    def __getitem__(cls, item):
        return object

# The type of a vertex or edge.
class ElementType(metaclass=_ElementTypeMeta):
    r"""ElementTypeVertex[T] | ElementTypeEdge[T]"""
    
    pass

ELEMENT_TYPE__NAME = hydra.core.Name("hydra.pg.model.ElementType")
ELEMENT_TYPE__VERTEX__NAME = hydra.core.Name("vertex")
ELEMENT_TYPE__EDGE__NAME = hydra.core.Name("edge")

@dataclass(frozen=True)
class ElementTypeTree(Generic[T]):
    r"""An element type together with its dependencies in some context."""
    
    self: ElementType[T]
    dependencies: frozenlist[ElementTypeTree[T]]

ELEMENT_TYPE_TREE__NAME = hydra.core.Name("hydra.pg.model.ElementTypeTree")
ELEMENT_TYPE_TREE__SELF__NAME = hydra.core.Name("self")
ELEMENT_TYPE_TREE__DEPENDENCIES__NAME = hydra.core.Name("dependencies")

@dataclass(frozen=True)
class Graph(Generic[V]):
    r"""A graph; a self-contained collection of vertices and edges."""
    
    vertices: FrozenDict[V, Vertex[V]]
    edges: FrozenDict[V, Edge[V]]

GRAPH__NAME = hydra.core.Name("hydra.pg.model.Graph")
GRAPH__VERTICES__NAME = hydra.core.Name("vertices")
GRAPH__EDGES__NAME = hydra.core.Name("edges")

@dataclass(frozen=True)
class GraphSchema(Generic[T]):
    r"""A graph schema; a vertex and edge types for the vertices and edges of a graph conforming to the schema."""
    
    vertices: Annotated[FrozenDict[VertexLabel, VertexType[T]], "A unique vertex type for each vertex label which may occur in a graph"]
    edges: Annotated[FrozenDict[EdgeLabel, EdgeType[T]], "A unique edge type for each edge label which may occur in a graph"]

GRAPH_SCHEMA__NAME = hydra.core.Name("hydra.pg.model.GraphSchema")
GRAPH_SCHEMA__VERTICES__NAME = hydra.core.Name("vertices")
GRAPH_SCHEMA__EDGES__NAME = hydra.core.Name("edges")

class LabelVertex(Node["VertexLabel"]): ...

class LabelEdge(Node["EdgeLabel"]): ...

class _LabelMeta(type):
    def __getitem__(cls, item):
        return object

# Either a vertex or edge label.
class Label(metaclass=_LabelMeta):
    r"""LabelVertex | LabelEdge"""
    
    pass

LABEL__NAME = hydra.core.Name("hydra.pg.model.Label")
LABEL__VERTEX__NAME = hydra.core.Name("vertex")
LABEL__EDGE__NAME = hydra.core.Name("edge")

@dataclass(frozen=True)
class LazyGraph(Generic[V]):
    r"""A graph which does not assume that vertex or edge ids are unique. This is useful in mappings because the id specifications for vertices and/or edges may be non-unique."""
    
    vertices: frozenlist[Vertex[V]]
    edges: frozenlist[Edge[V]]

LAZY_GRAPH__NAME = hydra.core.Name("hydra.pg.model.LazyGraph")
LAZY_GRAPH__VERTICES__NAME = hydra.core.Name("vertices")
LAZY_GRAPH__EDGES__NAME = hydra.core.Name("edges")

@dataclass(frozen=True)
class Property(Generic[V]):
    r"""A key/value property."""
    
    key: Annotated[PropertyKey, "They key of the property"]
    value: Annotated[V, "The value of the property"]

PROPERTY__NAME = hydra.core.Name("hydra.pg.model.Property")
PROPERTY__KEY__NAME = hydra.core.Name("key")
PROPERTY__VALUE__NAME = hydra.core.Name("value")

class PropertyKey(Node[str]):
    r"""A property key."""

PROPERTY_KEY__NAME = hydra.core.Name("hydra.pg.model.PropertyKey")

@dataclass(frozen=True)
class PropertyType(Generic[T]):
    r"""The type of a property."""
    
    key: Annotated[PropertyKey, "A property's key"]
    value: Annotated[T, "The type of a property's value"]
    required: Annotated[bool, "Whether the property is required; values may be omitted from a property map otherwise"]

PROPERTY_TYPE__NAME = hydra.core.Name("hydra.pg.model.PropertyType")
PROPERTY_TYPE__KEY__NAME = hydra.core.Name("key")
PROPERTY_TYPE__VALUE__NAME = hydra.core.Name("value")
PROPERTY_TYPE__REQUIRED__NAME = hydra.core.Name("required")

@dataclass(frozen=True)
class Vertex(Generic[V]):
    r"""A vertex."""
    
    label: Annotated[VertexLabel, "The label of the vertex"]
    id: Annotated[V, "The unique identifier of the vertex"]
    properties: Annotated[FrozenDict[PropertyKey, V], "A key/value map of vertex properties"]

VERTEX__NAME = hydra.core.Name("hydra.pg.model.Vertex")
VERTEX__LABEL__NAME = hydra.core.Name("label")
VERTEX__ID__NAME = hydra.core.Name("id")
VERTEX__PROPERTIES__NAME = hydra.core.Name("properties")

class VertexLabel(Node[str]):
    r"""The label of a vertex. The default (null) vertex is represented by the empty string."""

VERTEX_LABEL__NAME = hydra.core.Name("hydra.pg.model.VertexLabel")

@dataclass(frozen=True)
class VertexType(Generic[T]):
    r"""The type of a vertex."""
    
    label: Annotated[VertexLabel, "The label of any vertex of this vertex type"]
    id: Annotated[T, "The type of the id of any vertex of this vertex type"]
    properties: Annotated[frozenlist[PropertyType[T]], "A list of property types. The types are ordered for the sake of applications in which property order is significant."]

VERTEX_TYPE__NAME = hydra.core.Name("hydra.pg.model.VertexType")
VERTEX_TYPE__LABEL__NAME = hydra.core.Name("label")
VERTEX_TYPE__ID__NAME = hydra.core.Name("id")
VERTEX_TYPE__PROPERTIES__NAME = hydra.core.Name("properties")

@dataclass(frozen=True)
class VertexWithAdjacentEdges(Generic[V]):
    r"""A vertex together with any outgoing and/or incoming edges; a vertex object."""
    
    vertex: Annotated[Vertex[V], "The focus vertex"]
    ins: Annotated[frozenlist[AdjacentEdge[V]], "An adjacency list of edges in which the focus vertex is the head (in-vertex) of the edge"]
    outs: Annotated[frozenlist[AdjacentEdge[V]], "An adjacency list of edges in which the focus vertex is the tail (out-vertex) of the edge"]

VERTEX_WITH_ADJACENT_EDGES__NAME = hydra.core.Name("hydra.pg.model.VertexWithAdjacentEdges")
VERTEX_WITH_ADJACENT_EDGES__VERTEX__NAME = hydra.core.Name("vertex")
VERTEX_WITH_ADJACENT_EDGES__INS__NAME = hydra.core.Name("ins")
VERTEX_WITH_ADJACENT_EDGES__OUTS__NAME = hydra.core.Name("outs")
