# Note: this is an automatically generated file. Do not edit.

r"""A model derived from TinkerPop's Graph.Features. See
  https://tinkerpop.apache.org/javadocs/current/core/org/apache/tinkerpop/gremlin/structure/Graph.Features.html

An interface that represents the capabilities of a Graph implementation.
By default all methods of features return true and it is up to implementers to disable feature they don't support.
Users should check features prior to using various functions of TinkerPop to help ensure code portability across implementations.
For example, a common usage would be to check if a graph supports transactions prior to calling the commit method on Graph.tx()."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from functools import lru_cache
from typing import Annotated, Generic, TypeAlias, TypeVar, cast
import hydra.core

A = TypeVar("A")

@dataclass(frozen=True)
class DataTypeFeatures:
    r"""Base interface for features that relate to supporting different data types."""

    supports_boolean_array_values: Annotated[bool, "Supports setting of an array of boolean values."]
    supports_boolean_values: Annotated[bool, "Supports setting of a boolean value."]
    supports_byte_array_values: Annotated[bool, "Supports setting of an array of byte values."]
    supports_byte_values: Annotated[bool, "Supports setting of a byte value."]
    supports_double_array_values: Annotated[bool, "Supports setting of an array of double values."]
    supports_double_values: Annotated[bool, "Supports setting of a double value."]
    supports_float_array_values: Annotated[bool, "Supports setting of an array of float values."]
    supports_float_values: Annotated[bool, "Supports setting of a float value."]
    supports_integer_array_values: Annotated[bool, "Supports setting of an array of integer values."]
    supports_integer_values: Annotated[bool, "Supports setting of a integer value."]
    supports_long_array_values: Annotated[bool, "Supports setting of an array of long values."]
    supports_long_values: Annotated[bool, "Supports setting of a long value."]
    supports_map_values: Annotated[bool, "Supports setting of a Map value."]
    supports_mixed_list_values: Annotated[bool, "Supports setting of a List value."]
    supports_serializable_values: Annotated[bool, "Supports setting of a Java serializable value."]
    supports_string_array_values: Annotated[bool, "Supports setting of an array of string values."]
    supports_string_values: Annotated[bool, "Supports setting of a string value."]
    supports_uniform_list_values: Annotated[bool, "Supports setting of a List value."]

    TYPE_ = hydra.core.Name("hydra.tinkerpop.features.DataTypeFeatures")
    SUPPORTS_BOOLEAN_ARRAY_VALUES = hydra.core.Name("supportsBooleanArrayValues")
    SUPPORTS_BOOLEAN_VALUES = hydra.core.Name("supportsBooleanValues")
    SUPPORTS_BYTE_ARRAY_VALUES = hydra.core.Name("supportsByteArrayValues")
    SUPPORTS_BYTE_VALUES = hydra.core.Name("supportsByteValues")
    SUPPORTS_DOUBLE_ARRAY_VALUES = hydra.core.Name("supportsDoubleArrayValues")
    SUPPORTS_DOUBLE_VALUES = hydra.core.Name("supportsDoubleValues")
    SUPPORTS_FLOAT_ARRAY_VALUES = hydra.core.Name("supportsFloatArrayValues")
    SUPPORTS_FLOAT_VALUES = hydra.core.Name("supportsFloatValues")
    SUPPORTS_INTEGER_ARRAY_VALUES = hydra.core.Name("supportsIntegerArrayValues")
    SUPPORTS_INTEGER_VALUES = hydra.core.Name("supportsIntegerValues")
    SUPPORTS_LONG_ARRAY_VALUES = hydra.core.Name("supportsLongArrayValues")
    SUPPORTS_LONG_VALUES = hydra.core.Name("supportsLongValues")
    SUPPORTS_MAP_VALUES = hydra.core.Name("supportsMapValues")
    SUPPORTS_MIXED_LIST_VALUES = hydra.core.Name("supportsMixedListValues")
    SUPPORTS_SERIALIZABLE_VALUES = hydra.core.Name("supportsSerializableValues")
    SUPPORTS_STRING_ARRAY_VALUES = hydra.core.Name("supportsStringArrayValues")
    SUPPORTS_STRING_VALUES = hydra.core.Name("supportsStringValues")
    SUPPORTS_UNIFORM_LIST_VALUES = hydra.core.Name("supportsUniformListValues")

@dataclass(frozen=True)
class EdgeFeatures:
    r"""Features that are related to Edge operations."""

    element_features: ElementFeatures
    properties: EdgePropertyFeatures
    supports_add_edges: Annotated[bool, "Determines if an Edge can be added to a Vertex."]
    supports_remove_edges: Annotated[bool, "Determines if an Edge can be removed from a Vertex."]
    supports_upsert: Annotated[bool, "Determines if the Graph implementation uses upsert functionality as opposed to insert functionality for Vertex.addEdge(String, Vertex, Object...)."]

    TYPE_ = hydra.core.Name("hydra.tinkerpop.features.EdgeFeatures")
    ELEMENT_FEATURES = hydra.core.Name("elementFeatures")
    PROPERTIES = hydra.core.Name("properties")
    SUPPORTS_ADD_EDGES = hydra.core.Name("supportsAddEdges")
    SUPPORTS_REMOVE_EDGES = hydra.core.Name("supportsRemoveEdges")
    SUPPORTS_UPSERT = hydra.core.Name("supportsUpsert")

@dataclass(frozen=True)
class EdgePropertyFeatures:
    r"""Features that are related to Edge Property objects."""

    property_features: PropertyFeatures

    TYPE_ = hydra.core.Name("hydra.tinkerpop.features.EdgePropertyFeatures")
    PROPERTY_FEATURES = hydra.core.Name("propertyFeatures")

@dataclass(frozen=True)
class ElementFeatures:
    r"""Features that are related to Element objects."""

    supports_add_property: Annotated[bool, "Determines if an Element allows properties to be added."]
    supports_any_ids: Annotated[bool, "Determines if an Element any Java object is a suitable identifier."]
    supports_custom_ids: Annotated[bool, "Determines if an Element has a specific custom object as their internal representation."]
    supports_numeric_ids: Annotated[bool, "Determines if an Element has numeric identifiers as their internal representation."]
    supports_remove_property: Annotated[bool, "Determines if an Element allows properties to be removed."]
    supports_string_ids: Annotated[bool, "Determines if an Element has string identifiers as their internal representation."]
    supports_user_supplied_ids: Annotated[bool, "Determines if an Element can have a user defined identifier."]
    supports_uuid_ids: Annotated[bool, "Determines if an Element has UUID identifiers as their internal representation."]

    TYPE_ = hydra.core.Name("hydra.tinkerpop.features.ElementFeatures")
    SUPPORTS_ADD_PROPERTY = hydra.core.Name("supportsAddProperty")
    SUPPORTS_ANY_IDS = hydra.core.Name("supportsAnyIds")
    SUPPORTS_CUSTOM_IDS = hydra.core.Name("supportsCustomIds")
    SUPPORTS_NUMERIC_IDS = hydra.core.Name("supportsNumericIds")
    SUPPORTS_REMOVE_PROPERTY = hydra.core.Name("supportsRemoveProperty")
    SUPPORTS_STRING_IDS = hydra.core.Name("supportsStringIds")
    SUPPORTS_USER_SUPPLIED_IDS = hydra.core.Name("supportsUserSuppliedIds")
    SUPPORTS_UUID_IDS = hydra.core.Name("supportsUuidIds")

@dataclass(frozen=True)
class ExtraFeatures(Generic[A]):
    r"""Additional features which are needed for the complete specification of language constraints in Hydra, above and beyond TinkerPop Graph.Features."""

    supports_map_key: Callable[[hydra.core.Type], bool]

    TYPE_ = hydra.core.Name("hydra.tinkerpop.features.ExtraFeatures")
    SUPPORTS_MAP_KEY = hydra.core.Name("supportsMapKey")

@dataclass(frozen=True)
class Features:
    r"""An interface that represents the capabilities of a Graph implementation. By default all methods of features return true and it is up to implementers to disable feature they don't support. Users should check features prior to using various functions of TinkerPop to help ensure code portability across implementations. For example, a common usage would be to check if a graph supports transactions prior to calling the commit method on Graph.tx().

    As an additional notice to Graph Providers, feature methods will be used by the test suite to determine which tests will be ignored and which will be executed, therefore proper setting of these features is essential to maximizing the amount of testing performed by the suite. Further note, that these methods may be called by the TinkerPop core code to determine what operations may be appropriately executed which will have impact on features utilized by users."""

    edge: Annotated[EdgeFeatures, "Gets the features related to edge operation."]
    graph: Annotated[GraphFeatures, "Gets the features related to graph operation."]
    vertex: Annotated[VertexFeatures, "Gets the features related to vertex operation."]

    TYPE_ = hydra.core.Name("hydra.tinkerpop.features.Features")
    EDGE = hydra.core.Name("edge")
    GRAPH = hydra.core.Name("graph")
    VERTEX = hydra.core.Name("vertex")

@dataclass(frozen=True)
class GraphFeatures:
    r"""Features specific to a operations of a graph."""

    supports_computer: Annotated[bool, "Determines if the Graph implementation supports GraphComputer based processing."]
    supports_concurrent_access: Annotated[bool, "Determines if the Graph implementation supports more than one connection to the same instance at the same time."]
    supports_io_read: Annotated[bool, "Determines if the Graph implementations supports read operations as executed with the GraphTraversalSource.io(String) step."]
    supports_io_write: Annotated[bool, "Determines if the Graph implementations supports write operations as executed with the GraphTraversalSource.io(String) step."]
    supports_persistence: Annotated[bool, "Determines if the Graph implementation supports persisting it's contents natively to disk."]
    supports_threaded_transactions: Annotated[bool, "Determines if the Graph implementation supports threaded transactions which allow a transaction to be executed across multiple threads via Transaction.createThreadedTx()."]
    supports_transactions: Annotated[bool, "Determines if the Graph implementations supports transactions."]
    variables: Annotated[VariableFeatures, "Gets the features related to graph sideEffects operation."]

    TYPE_ = hydra.core.Name("hydra.tinkerpop.features.GraphFeatures")
    SUPPORTS_COMPUTER = hydra.core.Name("supportsComputer")
    SUPPORTS_CONCURRENT_ACCESS = hydra.core.Name("supportsConcurrentAccess")
    SUPPORTS_IO_READ = hydra.core.Name("supportsIoRead")
    SUPPORTS_IO_WRITE = hydra.core.Name("supportsIoWrite")
    SUPPORTS_PERSISTENCE = hydra.core.Name("supportsPersistence")
    SUPPORTS_THREADED_TRANSACTIONS = hydra.core.Name("supportsThreadedTransactions")
    SUPPORTS_TRANSACTIONS = hydra.core.Name("supportsTransactions")
    VARIABLES = hydra.core.Name("variables")

@dataclass(frozen=True)
class PropertyFeatures:
    r"""A base interface for Edge or Vertex Property features."""

    data_type_features: DataTypeFeatures
    supports_properties: Annotated[bool, "Determines if an Element allows for the processing of at least one data type defined by the features."]

    TYPE_ = hydra.core.Name("hydra.tinkerpop.features.PropertyFeatures")
    DATA_TYPE_FEATURES = hydra.core.Name("dataTypeFeatures")
    SUPPORTS_PROPERTIES = hydra.core.Name("supportsProperties")

@dataclass(frozen=True)
class VariableFeatures:
    r"""Features for Graph.Variables."""

    data_type_features: DataTypeFeatures
    supports_variables: Annotated[bool, "If any of the features on Graph.Features.VariableFeatures is true then this value must be true."]

    TYPE_ = hydra.core.Name("hydra.tinkerpop.features.VariableFeatures")
    DATA_TYPE_FEATURES = hydra.core.Name("dataTypeFeatures")
    SUPPORTS_VARIABLES = hydra.core.Name("supportsVariables")

@dataclass(frozen=True)
class VertexFeatures:
    r"""Features that are related to Vertex operations."""

    element_features: ElementFeatures
    properties: VertexPropertyFeatures
    supports_add_vertices: Annotated[bool, "Determines if a Vertex can be added to the Graph."]
    supports_duplicate_multi_properties: Annotated[bool, "Determines if a Vertex can support non-unique values on the same key."]
    supports_meta_properties: Annotated[bool, "Determines if a Vertex can support properties on vertex properties."]
    supports_multi_properties: Annotated[bool, "Determines if a Vertex can support multiple properties with the same key."]
    supports_remove_vertices: Annotated[bool, "Determines if a Vertex can be removed from the Graph."]
    supports_upsert: Annotated[bool, "Determines if the Graph implementation uses upsert functionality as opposed to insert functionality for Graph.addVertex(String)."]

    TYPE_ = hydra.core.Name("hydra.tinkerpop.features.VertexFeatures")
    ELEMENT_FEATURES = hydra.core.Name("elementFeatures")
    PROPERTIES = hydra.core.Name("properties")
    SUPPORTS_ADD_VERTICES = hydra.core.Name("supportsAddVertices")
    SUPPORTS_DUPLICATE_MULTI_PROPERTIES = hydra.core.Name("supportsDuplicateMultiProperties")
    SUPPORTS_META_PROPERTIES = hydra.core.Name("supportsMetaProperties")
    SUPPORTS_MULTI_PROPERTIES = hydra.core.Name("supportsMultiProperties")
    SUPPORTS_REMOVE_VERTICES = hydra.core.Name("supportsRemoveVertices")
    SUPPORTS_UPSERT = hydra.core.Name("supportsUpsert")

@dataclass(frozen=True)
class VertexPropertyFeatures:
    r"""Features that are related to Vertex Property objects."""

    data_type_features: DataTypeFeatures
    property_features: PropertyFeatures
    element_features: ElementFeatures
    supports_remove: Annotated[bool, "Determines if a VertexProperty allows properties to be removed."]

    TYPE_ = hydra.core.Name("hydra.tinkerpop.features.VertexPropertyFeatures")
    DATA_TYPE_FEATURES = hydra.core.Name("dataTypeFeatures")
    PROPERTY_FEATURES = hydra.core.Name("propertyFeatures")
    ELEMENT_FEATURES = hydra.core.Name("elementFeatures")
    SUPPORTS_REMOVE = hydra.core.Name("supportsRemove")
