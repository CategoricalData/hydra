# Note: this is an automatically generated file. Do not edit.

r"""Error types for property graph validation."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Node
from typing import Annotated, Generic, TypeAlias, TypeVar, cast
import hydra.core
import hydra.pg.model

V = TypeVar("V")

class InvalidEdgeErrorId(Node["InvalidValueError"]):
    r"""The edge id value is invalid"""

class InvalidEdgeErrorInVertexLabel(Node["WrongVertexLabelError"]):
    r"""The in-vertex has the wrong label"""

class InvalidEdgeErrorInVertexNotFound:
    r"""The in-vertex does not exist in the graph"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InvalidEdgeErrorInVertexNotFound)
    def __hash__(self):
        return hash("InvalidEdgeErrorInVertexNotFound")

class InvalidEdgeErrorLabel(Node["NoSuchEdgeLabelError"]):
    r"""The edge label does not exist in the schema"""

class InvalidEdgeErrorOutVertexLabel(Node["WrongVertexLabelError"]):
    r"""The out-vertex has the wrong label"""

class InvalidEdgeErrorOutVertexNotFound:
    r"""The out-vertex does not exist in the graph"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InvalidEdgeErrorOutVertexNotFound)
    def __hash__(self):
        return hash("InvalidEdgeErrorOutVertexNotFound")

class InvalidEdgeErrorProperty(Node["InvalidElementPropertyError"]):
    r"""A property of the edge is invalid"""

class _InvalidEdgeErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error indicating that an edge is invalid.
class InvalidEdgeError(metaclass=_InvalidEdgeErrorMeta):
    r"""InvalidEdgeErrorId | InvalidEdgeErrorInVertexLabel | InvalidEdgeErrorInVertexNotFound | InvalidEdgeErrorLabel | InvalidEdgeErrorOutVertexLabel | InvalidEdgeErrorOutVertexNotFound | InvalidEdgeErrorProperty"""

    TYPE_ = hydra.core.Name("hydra.error.pg.InvalidEdgeError")
    ID = hydra.core.Name("id")
    IN_VERTEX_LABEL = hydra.core.Name("inVertexLabel")
    IN_VERTEX_NOT_FOUND = hydra.core.Name("inVertexNotFound")
    LABEL = hydra.core.Name("label")
    OUT_VERTEX_LABEL = hydra.core.Name("outVertexLabel")
    OUT_VERTEX_NOT_FOUND = hydra.core.Name("outVertexNotFound")
    PROPERTY = hydra.core.Name("property")

@dataclass(frozen=True)
class InvalidElementPropertyError:
    r"""An invalid property on a vertex or edge, identified by its key."""

    key: Annotated[hydra.pg.model.PropertyKey, "The key of the invalid property"]
    error: Annotated[InvalidPropertyError, "The specific error"]

    TYPE_ = hydra.core.Name("hydra.error.pg.InvalidElementPropertyError")
    KEY = hydra.core.Name("key")
    ERROR = hydra.core.Name("error")

@dataclass(frozen=True)
class InvalidGraphEdgeError(Generic[V]):
    r"""An invalid edge within a graph, identified by its id."""

    id: Annotated[V, "The id of the invalid edge"]
    error: Annotated[InvalidEdgeError, "The specific error"]

    TYPE_ = hydra.core.Name("hydra.error.pg.InvalidGraphEdgeError")
    ID = hydra.core.Name("id")
    ERROR = hydra.core.Name("error")

class InvalidGraphErrorEdge(Node["InvalidGraphEdgeError[V]"]):
    r"""An edge in the graph is invalid"""

class InvalidGraphErrorVertex(Node["InvalidGraphVertexError[V]"]):
    r"""A vertex in the graph is invalid"""

class _InvalidGraphErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error indicating that a property graph is invalid.
class InvalidGraphError(metaclass=_InvalidGraphErrorMeta):
    r"""InvalidGraphErrorEdge[V] | InvalidGraphErrorVertex[V]"""

    TYPE_ = hydra.core.Name("hydra.error.pg.InvalidGraphError")
    EDGE = hydra.core.Name("edge")
    VERTEX = hydra.core.Name("vertex")

@dataclass(frozen=True)
class InvalidGraphVertexError(Generic[V]):
    r"""An invalid vertex within a graph, identified by its id."""

    id: Annotated[V, "The id of the invalid vertex"]
    error: Annotated[InvalidVertexError, "The specific error"]

    TYPE_ = hydra.core.Name("hydra.error.pg.InvalidGraphVertexError")
    ID = hydra.core.Name("id")
    ERROR = hydra.core.Name("error")

class InvalidPropertyErrorInvalidValue(Node["InvalidValueError"]):
    r"""The property value failed type validation"""

class InvalidPropertyErrorMissingRequired(Node["hydra.pg.model.PropertyKey"]):
    r"""A required property is missing"""

class InvalidPropertyErrorUnexpectedKey(Node["hydra.pg.model.PropertyKey"]):
    r"""A property has an unexpected key not in the schema"""

class _InvalidPropertyErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error indicating that a property is invalid.
class InvalidPropertyError(metaclass=_InvalidPropertyErrorMeta):
    r"""InvalidPropertyErrorInvalidValue | InvalidPropertyErrorMissingRequired | InvalidPropertyErrorUnexpectedKey"""

    TYPE_ = hydra.core.Name("hydra.error.pg.InvalidPropertyError")
    INVALID_VALUE = hydra.core.Name("invalidValue")
    MISSING_REQUIRED = hydra.core.Name("missingRequired")
    UNEXPECTED_KEY = hydra.core.Name("unexpectedKey")

@dataclass(frozen=True)
class InvalidValueError:
    r"""An error indicating that a value does not match the expected type."""

    expected_type: Annotated[str, "The expected type, as a string"]
    value: Annotated[str, "The actual value, as a string"]

    TYPE_ = hydra.core.Name("hydra.error.pg.InvalidValueError")
    EXPECTED_TYPE = hydra.core.Name("expectedType")
    VALUE = hydra.core.Name("value")

class InvalidVertexErrorId(Node["InvalidValueError"]):
    r"""The vertex id value is invalid"""

class InvalidVertexErrorLabel(Node["NoSuchVertexLabelError"]):
    r"""The vertex label does not exist in the schema"""

class InvalidVertexErrorProperty(Node["InvalidElementPropertyError"]):
    r"""A property of the vertex is invalid"""

class _InvalidVertexErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error indicating that a vertex is invalid.
class InvalidVertexError(metaclass=_InvalidVertexErrorMeta):
    r"""InvalidVertexErrorId | InvalidVertexErrorLabel | InvalidVertexErrorProperty"""

    TYPE_ = hydra.core.Name("hydra.error.pg.InvalidVertexError")
    ID = hydra.core.Name("id")
    LABEL = hydra.core.Name("label")
    PROPERTY = hydra.core.Name("property")

@dataclass(frozen=True)
class NoSuchEdgeLabelError:
    r"""An error indicating that an edge label does not exist in the schema."""

    label: Annotated[hydra.pg.model.EdgeLabel, "The edge label that was not found"]

    TYPE_ = hydra.core.Name("hydra.error.pg.NoSuchEdgeLabelError")
    LABEL = hydra.core.Name("label")

@dataclass(frozen=True)
class NoSuchVertexLabelError:
    r"""An error indicating that a vertex label does not exist in the schema."""

    label: Annotated[hydra.pg.model.VertexLabel, "The vertex label that was not found"]

    TYPE_ = hydra.core.Name("hydra.error.pg.NoSuchVertexLabelError")
    LABEL = hydra.core.Name("label")

@dataclass(frozen=True)
class WrongVertexLabelError:
    r"""An error indicating that a vertex has the wrong label."""

    expected: Annotated[hydra.pg.model.VertexLabel, "The expected vertex label"]
    actual: Annotated[hydra.pg.model.VertexLabel, "The actual vertex label"]

    TYPE_ = hydra.core.Name("hydra.error.pg.WrongVertexLabelError")
    EXPECTED = hydra.core.Name("expected")
    ACTUAL = hydra.core.Name("actual")
