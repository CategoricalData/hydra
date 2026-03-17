# Note: this is an automatically generated file. Do not edit.

r"""A model for property graph mapping specifications. See https://github.com/CategoricalData/hydra/wiki/Property-graphs."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Node, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar, cast
import hydra.core
import hydra.pg.model
import hydra.util

S = TypeVar("S")
T = TypeVar("T")
V = TypeVar("V")

@dataclass(frozen=True)
class AnnotationSchema:
    r"""Configurable annotation keys for property graph mapping specifications."""
    
    vertex_label: str
    edge_label: str
    vertex_id: str
    edge_id: str
    property_key: str
    property_value: str
    out_vertex: str
    out_vertex_label: str
    in_vertex: str
    in_vertex_label: str
    out_edge: str
    out_edge_label: str
    in_edge: str
    in_edge_label: str
    ignore: str
    
    TYPE_ = hydra.core.Name("hydra.pg.mapping.AnnotationSchema")
    VERTEX_LABEL = hydra.core.Name("vertexLabel")
    EDGE_LABEL = hydra.core.Name("edgeLabel")
    VERTEX_ID = hydra.core.Name("vertexId")
    EDGE_ID = hydra.core.Name("edgeId")
    PROPERTY_KEY = hydra.core.Name("propertyKey")
    PROPERTY_VALUE = hydra.core.Name("propertyValue")
    OUT_VERTEX = hydra.core.Name("outVertex")
    OUT_VERTEX_LABEL = hydra.core.Name("outVertexLabel")
    IN_VERTEX = hydra.core.Name("inVertex")
    IN_VERTEX_LABEL = hydra.core.Name("inVertexLabel")
    OUT_EDGE = hydra.core.Name("outEdge")
    OUT_EDGE_LABEL = hydra.core.Name("outEdgeLabel")
    IN_EDGE = hydra.core.Name("inEdge")
    IN_EDGE_LABEL = hydra.core.Name("inEdgeLabel")
    IGNORE = hydra.core.Name("ignore")

@dataclass(frozen=True)
class EdgeSpec:
    r"""A mapping specification producing edges of a specified label."""
    
    label: Annotated[hydra.pg.model.EdgeLabel, "The label of the target edges, which must conform to the edge type associated with that label."]
    id: Annotated[ValueSpec, "A specification of the id of each target edge"]
    out: Annotated[ValueSpec, "A specification of the out-vertex reference of each target edge"]
    in_: Annotated[ValueSpec, "A specification of the in-vertex reference of each target edge"]
    properties: Annotated[frozenlist[PropertySpec], "Zero or more property specifications for each target edge"]
    
    TYPE_ = hydra.core.Name("hydra.pg.mapping.EdgeSpec")
    LABEL = hydra.core.Name("label")
    ID = hydra.core.Name("id")
    OUT = hydra.core.Name("out")
    IN = hydra.core.Name("in")
    PROPERTIES = hydra.core.Name("properties")

class ElementSpecVertex(Node["VertexSpec"]):
    ...

class ElementSpecEdge(Node["EdgeSpec"]):
    ...

class _ElementSpecMeta(type):
    def __getitem__(cls, item):
        return object

# Either a vertex specification or an edge specification.
class ElementSpec(metaclass=_ElementSpecMeta):
    r"""ElementSpecVertex | ElementSpecEdge"""
    
    TYPE_ = hydra.core.Name("hydra.pg.mapping.ElementSpec")
    VERTEX = hydra.core.Name("vertex")
    EDGE = hydra.core.Name("edge")

@dataclass(frozen=True)
class PropertySpec:
    r"""A mapping specification producing properties of a specified key, and values of the appropriate type."""
    
    key: Annotated[hydra.pg.model.PropertyKey, "The key of the target properties"]
    value: Annotated[ValueSpec, "A specification of the value of each target property, which must conform to the type associated with the property key"]
    
    TYPE_ = hydra.core.Name("hydra.pg.mapping.PropertySpec")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class Schema(Generic[S, T, V]):
    r"""A set of mappings which translates between Hydra terms and annotations, and application-specific property graph types."""
    
    vertex_id_types: hydra.util.Coder[hydra.core.Type, T]
    vertex_ids: hydra.util.Coder[hydra.core.Term, V]
    edge_id_types: hydra.util.Coder[hydra.core.Type, T]
    edge_ids: hydra.util.Coder[hydra.core.Term, V]
    property_types: hydra.util.Coder[hydra.core.Type, T]
    property_values: hydra.util.Coder[hydra.core.Term, V]
    annotations: AnnotationSchema
    default_vertex_id: V
    default_edge_id: V
    
    TYPE_ = hydra.core.Name("hydra.pg.mapping.Schema")
    VERTEX_ID_TYPES = hydra.core.Name("vertexIdTypes")
    VERTEX_IDS = hydra.core.Name("vertexIds")
    EDGE_ID_TYPES = hydra.core.Name("edgeIdTypes")
    EDGE_IDS = hydra.core.Name("edgeIds")
    PROPERTY_TYPES = hydra.core.Name("propertyTypes")
    PROPERTY_VALUES = hydra.core.Name("propertyValues")
    ANNOTATIONS = hydra.core.Name("annotations")
    DEFAULT_VERTEX_ID = hydra.core.Name("defaultVertexId")
    DEFAULT_EDGE_ID = hydra.core.Name("defaultEdgeId")

class ValueSpecValue:
    r"""A trivial no-op specification which passes the entire value"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ValueSpecValue)
    def __hash__(self):
        return hash("ValueSpecValue")

class ValueSpecPattern(Node[str]):
    r"""A compact path representing the function, e.g. engine-${engineInfo/model/name}"""

class _ValueSpecMeta(type):
    def __getitem__(cls, item):
        return object

# A mapping specification producing values (usually literal values) whose type is understood in context.
class ValueSpec(metaclass=_ValueSpecMeta):
    r"""ValueSpecValue | ValueSpecPattern"""
    
    TYPE_ = hydra.core.Name("hydra.pg.mapping.ValueSpec")
    VALUE = hydra.core.Name("value")
    PATTERN = hydra.core.Name("pattern")

@dataclass(frozen=True)
class VertexSpec:
    r"""A mapping specification producing vertices of a specified label."""
    
    label: Annotated[hydra.pg.model.VertexLabel, "The label of the target vertices, which must conform to the vertex type associated with that label."]
    id: Annotated[ValueSpec, "A specification of the id of each target vertex"]
    properties: Annotated[frozenlist[PropertySpec], "Zero or more property specifications for each target vertex"]
    
    TYPE_ = hydra.core.Name("hydra.pg.mapping.VertexSpec")
    LABEL = hydra.core.Name("label")
    ID = hydra.core.Name("id")
    PROPERTIES = hydra.core.Name("properties")
