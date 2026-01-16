# Note: this is an automatically generated file. Do not edit.

r"""A model for property graph mapping specifications. See https://github.com/CategoricalData/hydra/wiki/Property-graphs."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import Node, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar
import hydra.compute
import hydra.core
import hydra.pg.model

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

ANNOTATION_SCHEMA__NAME = hydra.core.Name("hydra.pg.mapping.AnnotationSchema")
ANNOTATION_SCHEMA__VERTEX_LABEL__NAME = hydra.core.Name("vertexLabel")
ANNOTATION_SCHEMA__EDGE_LABEL__NAME = hydra.core.Name("edgeLabel")
ANNOTATION_SCHEMA__VERTEX_ID__NAME = hydra.core.Name("vertexId")
ANNOTATION_SCHEMA__EDGE_ID__NAME = hydra.core.Name("edgeId")
ANNOTATION_SCHEMA__PROPERTY_KEY__NAME = hydra.core.Name("propertyKey")
ANNOTATION_SCHEMA__PROPERTY_VALUE__NAME = hydra.core.Name("propertyValue")
ANNOTATION_SCHEMA__OUT_VERTEX__NAME = hydra.core.Name("outVertex")
ANNOTATION_SCHEMA__OUT_VERTEX_LABEL__NAME = hydra.core.Name("outVertexLabel")
ANNOTATION_SCHEMA__IN_VERTEX__NAME = hydra.core.Name("inVertex")
ANNOTATION_SCHEMA__IN_VERTEX_LABEL__NAME = hydra.core.Name("inVertexLabel")
ANNOTATION_SCHEMA__OUT_EDGE__NAME = hydra.core.Name("outEdge")
ANNOTATION_SCHEMA__OUT_EDGE_LABEL__NAME = hydra.core.Name("outEdgeLabel")
ANNOTATION_SCHEMA__IN_EDGE__NAME = hydra.core.Name("inEdge")
ANNOTATION_SCHEMA__IN_EDGE_LABEL__NAME = hydra.core.Name("inEdgeLabel")
ANNOTATION_SCHEMA__IGNORE__NAME = hydra.core.Name("ignore")

@dataclass(frozen=True)
class EdgeSpec:
    r"""A mapping specification producing edges of a specified label."""
    
    label: Annotated[hydra.pg.model.EdgeLabel, "The label of the target edges, which must conform to the edge type associated with that label."]
    id: Annotated[ValueSpec, "A specification of the id of each target edge"]
    out: Annotated[ValueSpec, "A specification of the out-vertex reference of each target edge"]
    in_: Annotated[ValueSpec, "A specification of the in-vertex reference of each target edge"]
    properties: Annotated[frozenlist[PropertySpec], "Zero or more property specifications for each target edge"]

EDGE_SPEC__NAME = hydra.core.Name("hydra.pg.mapping.EdgeSpec")
EDGE_SPEC__LABEL__NAME = hydra.core.Name("label")
EDGE_SPEC__ID__NAME = hydra.core.Name("id")
EDGE_SPEC__OUT__NAME = hydra.core.Name("out")
EDGE_SPEC__IN__NAME = hydra.core.Name("in")
EDGE_SPEC__PROPERTIES__NAME = hydra.core.Name("properties")

class ElementSpecVertex(Node["VertexSpec"]): ...

class ElementSpecEdge(Node["EdgeSpec"]): ...

class _ElementSpecMeta(type):
    def __getitem__(cls, item):
        return object

# Either a vertex specification or an edge specification.
class ElementSpec(metaclass=_ElementSpecMeta):
    r"""ElementSpecVertex | ElementSpecEdge"""
    
    pass

ELEMENT_SPEC__NAME = hydra.core.Name("hydra.pg.mapping.ElementSpec")
ELEMENT_SPEC__VERTEX__NAME = hydra.core.Name("vertex")
ELEMENT_SPEC__EDGE__NAME = hydra.core.Name("edge")

@dataclass(frozen=True)
class PropertySpec:
    r"""A mapping specification producing properties of a specified key, and values of the appropriate type."""
    
    key: Annotated[hydra.pg.model.PropertyKey, "The key of the target properties"]
    value: Annotated[ValueSpec, "A specification of the value of each target property, which must conform to the type associated with the property key"]

PROPERTY_SPEC__NAME = hydra.core.Name("hydra.pg.mapping.PropertySpec")
PROPERTY_SPEC__KEY__NAME = hydra.core.Name("key")
PROPERTY_SPEC__VALUE__NAME = hydra.core.Name("value")

@dataclass(frozen=True)
class Schema(Generic[S, T, V]):
    r"""A set of mappings which translates between Hydra terms and annotations, and application-specific property graph types."""
    
    vertex_id_types: hydra.compute.Coder[S, S, hydra.core.Type, T]
    vertex_ids: hydra.compute.Coder[S, S, hydra.core.Term, V]
    edge_id_types: hydra.compute.Coder[S, S, hydra.core.Type, T]
    edge_ids: hydra.compute.Coder[S, S, hydra.core.Term, V]
    property_types: hydra.compute.Coder[S, S, hydra.core.Type, T]
    property_values: hydra.compute.Coder[S, S, hydra.core.Term, V]
    annotations: AnnotationSchema
    default_vertex_id: V
    default_edge_id: V

SCHEMA__NAME = hydra.core.Name("hydra.pg.mapping.Schema")
SCHEMA__VERTEX_ID_TYPES__NAME = hydra.core.Name("vertexIdTypes")
SCHEMA__VERTEX_IDS__NAME = hydra.core.Name("vertexIds")
SCHEMA__EDGE_ID_TYPES__NAME = hydra.core.Name("edgeIdTypes")
SCHEMA__EDGE_IDS__NAME = hydra.core.Name("edgeIds")
SCHEMA__PROPERTY_TYPES__NAME = hydra.core.Name("propertyTypes")
SCHEMA__PROPERTY_VALUES__NAME = hydra.core.Name("propertyValues")
SCHEMA__ANNOTATIONS__NAME = hydra.core.Name("annotations")
SCHEMA__DEFAULT_VERTEX_ID__NAME = hydra.core.Name("defaultVertexId")
SCHEMA__DEFAULT_EDGE_ID__NAME = hydra.core.Name("defaultEdgeId")

class ValueSpecValue:
    r"""A trivial no-op specification which passes the entire value."""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ValueSpecValue)
    def __hash__(self):
        return hash("ValueSpecValue")

class ValueSpecPattern(Node[str]):
    r"""A compact path representing the function, e.g. engine-${engineInfo/model/name}."""

class _ValueSpecMeta(type):
    def __getitem__(cls, item):
        return object

# A mapping specification producing values (usually literal values) whose type is understood in context.
class ValueSpec(metaclass=_ValueSpecMeta):
    r"""ValueSpecValue | ValueSpecPattern"""
    
    pass

VALUE_SPEC__NAME = hydra.core.Name("hydra.pg.mapping.ValueSpec")
VALUE_SPEC__VALUE__NAME = hydra.core.Name("value")
VALUE_SPEC__PATTERN__NAME = hydra.core.Name("pattern")

@dataclass(frozen=True)
class VertexSpec:
    r"""A mapping specification producing vertices of a specified label."""
    
    label: Annotated[hydra.pg.model.VertexLabel, "The label of the target vertices, which must conform to the vertex type associated with that label."]
    id: Annotated[ValueSpec, "A specification of the id of each target vertex"]
    properties: Annotated[frozenlist[PropertySpec], "Zero or more property specifications for each target vertex"]

VERTEX_SPEC__NAME = hydra.core.Name("hydra.pg.mapping.VertexSpec")
VERTEX_SPEC__LABEL__NAME = hydra.core.Name("label")
VERTEX_SPEC__ID__NAME = hydra.core.Name("id")
VERTEX_SPEC__PROPERTIES__NAME = hydra.core.Name("properties")
