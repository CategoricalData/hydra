# Note: this is an automatically generated file. Do not edit.

r"""A typed property graph data model. Property graphs are parameterized a type for property and id values, while property graph schemas are parameterized by a type for property and id types."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Node, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar, cast
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
    
    TYPE_ = hydra.core.Name("hydra.pg.model.AdjacentEdge")
    LABEL = hydra.core.Name("label")
    ID = hydra.core.Name("id")
    VERTEX = hydra.core.Name("vertex")
    PROPERTIES = hydra.core.Name("properties")

class Direction(Enum):
    r"""The direction of an edge or edge pattern."""
    
    OUT = hydra.core.Name("out")
    
    IN = hydra.core.Name("in")
    
    BOTH = hydra.core.Name("both")
    
    UNDIRECTED = hydra.core.Name("undirected")

Direction.TYPE_ = hydra.core.Name("hydra.pg.model.Direction")

@dataclass(frozen=True)
class Edge(Generic[V]):
    r"""An edge."""
    
    label: Annotated[EdgeLabel, "The label of the edge"]
    id: Annotated[V, "The unique identifier of the edge"]
    out: Annotated[V, "The id of the out-vertex (tail) of the edge"]
    in_: Annotated[V, "The id of the in-vertex (head) of the edge"]
    properties: Annotated[FrozenDict[PropertyKey, V], "A key/value map of edge properties"]
    
    TYPE_ = hydra.core.Name("hydra.pg.model.Edge")
    LABEL = hydra.core.Name("label")
    ID = hydra.core.Name("id")
    OUT = hydra.core.Name("out")
    IN = hydra.core.Name("in")
    PROPERTIES = hydra.core.Name("properties")

class EdgeLabel(Node[str]):
    r"""The label of an edge."""

EdgeLabel.TYPE_ = hydra.core.Name("hydra.pg.model.EdgeLabel")

@dataclass(frozen=True)
class EdgeType(Generic[T]):
    r"""The type of an edge."""
    
    label: Annotated[EdgeLabel, "The label of any edge of this edge type"]
    id: Annotated[T, "The type of the id of any edge of this edge type"]
    out: Annotated[VertexLabel, "The label of the out-vertex (tail) of any edge of this edge type"]
    in_: Annotated[VertexLabel, "The label of the in-vertex (head) of any edge of this edge type"]
    properties: Annotated[frozenlist[PropertyType[T]], "A list of property types. The types are ordered for the sake of applications in which property order is significant."]
    
    TYPE_ = hydra.core.Name("hydra.pg.model.EdgeType")
    LABEL = hydra.core.Name("label")
    ID = hydra.core.Name("id")
    OUT = hydra.core.Name("out")
    IN = hydra.core.Name("in")
    PROPERTIES = hydra.core.Name("properties")

class ElementVertex(Node["Vertex[V]"]):
    ...

class ElementEdge(Node["Edge[V]"]):
    ...

class _ElementMeta(type):
    def __getitem__(cls, item):
        return object

# Either a vertex or an edge.
class Element(metaclass=_ElementMeta):
    r"""ElementVertex[V] | ElementEdge[V]"""
    
    TYPE_ = hydra.core.Name("hydra.pg.model.Element")
    VERTEX = hydra.core.Name("vertex")
    EDGE = hydra.core.Name("edge")

class ElementKind(Enum):
    r"""The kind of an element: vertex or edge."""
    
    VERTEX = hydra.core.Name("vertex")
    
    EDGE = hydra.core.Name("edge")

ElementKind.TYPE_ = hydra.core.Name("hydra.pg.model.ElementKind")

@dataclass(frozen=True)
class ElementTree(Generic[V]):
    r"""An element together with its dependencies in some context."""
    
    self: Element[V]
    dependencies: frozenlist[ElementTree[V]]
    
    TYPE_ = hydra.core.Name("hydra.pg.model.ElementTree")
    SELF = hydra.core.Name("self")
    DEPENDENCIES = hydra.core.Name("dependencies")

class ElementTypeVertex(Node["VertexType[T]"]):
    ...

class ElementTypeEdge(Node["EdgeType[T]"]):
    ...

class _ElementTypeMeta(type):
    def __getitem__(cls, item):
        return object

# The type of a vertex or edge.
class ElementType(metaclass=_ElementTypeMeta):
    r"""ElementTypeVertex[T] | ElementTypeEdge[T]"""
    
    TYPE_ = hydra.core.Name("hydra.pg.model.ElementType")
    VERTEX = hydra.core.Name("vertex")
    EDGE = hydra.core.Name("edge")

@dataclass(frozen=True)
class ElementTypeTree(Generic[T]):
    r"""An element type together with its dependencies in some context."""
    
    self: ElementType[T]
    dependencies: frozenlist[ElementTypeTree[T]]
    
    TYPE_ = hydra.core.Name("hydra.pg.model.ElementTypeTree")
    SELF = hydra.core.Name("self")
    DEPENDENCIES = hydra.core.Name("dependencies")

@dataclass(frozen=True)
class Graph(Generic[V]):
    r"""A graph; a self-contained collection of vertices and edges."""
    
    vertices: FrozenDict[V, Vertex[V]]
    edges: FrozenDict[V, Edge[V]]
    
    TYPE_ = hydra.core.Name("hydra.pg.model.Graph")
    VERTICES = hydra.core.Name("vertices")
    EDGES = hydra.core.Name("edges")

@dataclass(frozen=True)
class GraphSchema(Generic[T]):
    r"""A graph schema; a vertex and edge types for the vertices and edges of a graph conforming to the schema."""
    
    vertices: Annotated[FrozenDict[VertexLabel, VertexType[T]], "A unique vertex type for each vertex label which may occur in a graph"]
    edges: Annotated[FrozenDict[EdgeLabel, EdgeType[T]], "A unique edge type for each edge label which may occur in a graph"]
    
    TYPE_ = hydra.core.Name("hydra.pg.model.GraphSchema")
    VERTICES = hydra.core.Name("vertices")
    EDGES = hydra.core.Name("edges")

class LabelVertex(Node["VertexLabel"]):
    ...

class LabelEdge(Node["EdgeLabel"]):
    ...

class _LabelMeta(type):
    def __getitem__(cls, item):
        return object

# Either a vertex or edge label.
class Label(metaclass=_LabelMeta):
    r"""LabelVertex | LabelEdge"""
    
    TYPE_ = hydra.core.Name("hydra.pg.model.Label")
    VERTEX = hydra.core.Name("vertex")
    EDGE = hydra.core.Name("edge")

@dataclass(frozen=True)
class LazyGraph(Generic[V]):
    r"""A graph which does not assume that vertex or edge ids are unique. This is useful in mappings because the id specifications for vertices and/or edges may be non-unique."""
    
    vertices: frozenlist[Vertex[V]]
    edges: frozenlist[Edge[V]]
    
    TYPE_ = hydra.core.Name("hydra.pg.model.LazyGraph")
    VERTICES = hydra.core.Name("vertices")
    EDGES = hydra.core.Name("edges")

@dataclass(frozen=True)
class Property(Generic[V]):
    r"""A key/value property."""
    
    key: Annotated[PropertyKey, "They key of the property"]
    value: Annotated[V, "The value of the property"]
    
    TYPE_ = hydra.core.Name("hydra.pg.model.Property")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

class PropertyKey(Node[str]):
    r"""A property key."""

PropertyKey.TYPE_ = hydra.core.Name("hydra.pg.model.PropertyKey")

@dataclass(frozen=True)
class PropertyType(Generic[T]):
    r"""The type of a property."""
    
    key: Annotated[PropertyKey, "A property's key"]
    value: Annotated[T, "The type of a property's value"]
    required: Annotated[bool, "Whether the property is required; values may be omitted from a property map otherwise"]
    
    TYPE_ = hydra.core.Name("hydra.pg.model.PropertyType")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")
    REQUIRED = hydra.core.Name("required")

@dataclass(frozen=True)
class Vertex(Generic[V]):
    r"""A vertex."""
    
    label: Annotated[VertexLabel, "The label of the vertex"]
    id: Annotated[V, "The unique identifier of the vertex"]
    properties: Annotated[FrozenDict[PropertyKey, V], "A key/value map of vertex properties"]
    
    TYPE_ = hydra.core.Name("hydra.pg.model.Vertex")
    LABEL = hydra.core.Name("label")
    ID = hydra.core.Name("id")
    PROPERTIES = hydra.core.Name("properties")

class VertexLabel(Node[str]):
    r"""The label of a vertex. The default (null) vertex is represented by the empty string."""

VertexLabel.TYPE_ = hydra.core.Name("hydra.pg.model.VertexLabel")

@dataclass(frozen=True)
class VertexType(Generic[T]):
    r"""The type of a vertex."""
    
    label: Annotated[VertexLabel, "The label of any vertex of this vertex type"]
    id: Annotated[T, "The type of the id of any vertex of this vertex type"]
    properties: Annotated[frozenlist[PropertyType[T]], "A list of property types. The types are ordered for the sake of applications in which property order is significant."]
    
    TYPE_ = hydra.core.Name("hydra.pg.model.VertexType")
    LABEL = hydra.core.Name("label")
    ID = hydra.core.Name("id")
    PROPERTIES = hydra.core.Name("properties")

@dataclass(frozen=True)
class VertexWithAdjacentEdges(Generic[V]):
    r"""A vertex together with any outgoing and/or incoming edges; a vertex object."""
    
    vertex: Annotated[Vertex[V], "The focus vertex"]
    ins: Annotated[frozenlist[AdjacentEdge[V]], "An adjacency list of edges in which the focus vertex is the head (in-vertex) of the edge"]
    outs: Annotated[frozenlist[AdjacentEdge[V]], "An adjacency list of edges in which the focus vertex is the tail (out-vertex) of the edge"]
    
    TYPE_ = hydra.core.Name("hydra.pg.model.VertexWithAdjacentEdges")
    VERTEX = hydra.core.Name("vertex")
    INS = hydra.core.Name("ins")
    OUTS = hydra.core.Name("outs")
