# Note: this is an automatically generated file. Do not edit.

r"""Environment types for property graph to RDF mapping."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from functools import lru_cache
from typing import Annotated, Generic, TypeAlias, TypeVar, cast
import hydra.core
import hydra.pg.model
import hydra.rdf.syntax

V = TypeVar("V")

@dataclass(frozen=True)
class PgRdfEnvironment(Generic[V]):
    r"""The environment for property graph to RDF mapping."""

    encode_vertex_id: Annotated[Callable[[V], hydra.rdf.syntax.Iri], "A function which encodes a vertex id as an RDF IRI"]
    encode_vertex_label: Annotated[Callable[[hydra.pg.model.VertexLabel], hydra.rdf.syntax.Iri], "A function which encodes a vertex label as an RDF IRI"]
    encode_edge_id: Annotated[Callable[[V], hydra.rdf.syntax.Iri], "A function which encodes an edge id as an RDF IRI"]
    encode_edge_label: Annotated[Callable[[hydra.pg.model.EdgeLabel], hydra.rdf.syntax.Iri], "A function which encodes an edge label as an RDF IRI"]
    encode_property_key: Annotated[Callable[[hydra.pg.model.PropertyKey], hydra.rdf.syntax.Iri], "A function which encodes a property key as an RDF IRI"]
    encode_property_value: Annotated[Callable[[V], hydra.rdf.syntax.Literal], "A function which encodes a property value as an RDF literal"]

    TYPE_ = hydra.core.Name("hydra.pg.rdf.environment.PgRdfEnvironment")
    ENCODE_VERTEX_ID = hydra.core.Name("encodeVertexId")
    ENCODE_VERTEX_LABEL = hydra.core.Name("encodeVertexLabel")
    ENCODE_EDGE_ID = hydra.core.Name("encodeEdgeId")
    ENCODE_EDGE_LABEL = hydra.core.Name("encodeEdgeLabel")
    ENCODE_PROPERTY_KEY = hydra.core.Name("encodePropertyKey")
    ENCODE_PROPERTY_VALUE = hydra.core.Name("encodePropertyValue")
