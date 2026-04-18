# Note: this is an automatically generated file. Do not edit.

r"""An Azure Digital Twin Definition Language (DTLD) model. Based on:
  https://github.com/Azure/opendigitaltwins-dtdl/blob/master/DTDL/v2/dtdlv2.md#digital-twins-definition-language
DTLD features which are not currently included in this model:
  * geospatial schemas (https://github.com/Azure/opendigitaltwins-dtdl/blob/master/DTDL/v2/dtdlv2.md#geospatial-schemas)
  * semantic types and units (https://github.com/Azure/opendigitaltwins-dtdl/blob/master/DTDL/v2/dtdlv2.md#semantic-types)
  * model versioning (https://github.com/Azure/opendigitaltwins-dtdl/blob/master/DTDL/v2/dtdlv2.md#model-versioning)."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class Command:
    r"""A Command describes a function or operation that can be performed on any digital twin."""

    type: Annotated[Iri, "This must be 'Command'"]
    name: Annotated[str, "The 'programming' name of the command. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$."]
    id: Annotated[Maybe[Dtmi], "The ID of the command. If no @id is provided, the digital twin interface processor will assign one."]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]
    command_type: Annotated[Maybe[CommandType], "This property is deprecated. Either value, synchronous or asynchronous, has the same meaning: a command that starts execution within a configurable time and that completes execution within a configurable time."]
    request: Annotated[Maybe[CommandPayload], "A description of the input to the Command"]
    response: Annotated[Maybe[CommandPayload], "A description of the output of the Command"]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Command")
    TYPE = hydra.core.Name("type")
    NAME = hydra.core.Name("name")
    ID = hydra.core.Name("id")
    COMMENT = hydra.core.Name("comment")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")
    COMMAND_TYPE = hydra.core.Name("commandType")
    REQUEST = hydra.core.Name("request")
    RESPONSE = hydra.core.Name("response")

@dataclass(frozen=True)
class CommandPayload:
    r"""A CommandPayload describes the inputs to or the outputs from a Command."""

    name: Annotated[str, "The 'programming' name of the payload. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$."]
    schema: Annotated[Schema, "The data type of the payload"]
    id: Annotated[Maybe[Dtmi], "The ID of the payload. If no @id is provided, the digital twin interface processor will assign one."]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.CommandPayload")
    NAME = hydra.core.Name("name")
    SCHEMA = hydra.core.Name("schema")
    ID = hydra.core.Name("id")
    COMMENT = hydra.core.Name("comment")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")

class CommandType(Enum):
    r"""CommandType is deprecated. Either value, synchronous or asynchronous, has the same meaning: a command that starts execution within a configurable time and that completes execution within a configurable time."""

    SYNCHRONOUS = hydra.core.Name("synchronous")

    ASYNCHRONOUS = hydra.core.Name("asynchronous")

CommandType.TYPE_ = hydra.core.Name("hydra.azure.dtld.CommandType")

@dataclass(frozen=True)
class Component:
    r"""Components enable interfaces to be composed of other interfaces. Components are different from relationships because they describe contents that are directly part of the interface. (A relationship describes a link between two interfaces.)."""

    type: Annotated[Iri, "This must be 'Component'"]
    name: Annotated[str, "The 'programming' name of the component. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$."]
    schema: Annotated[Interface, "The data type of the component"]
    id: Annotated[Maybe[Dtmi], "The ID of the component. If no @id is provided, the digital twin interface processor will assign one."]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Component")
    TYPE = hydra.core.Name("type")
    NAME = hydra.core.Name("name")
    SCHEMA = hydra.core.Name("schema")
    ID = hydra.core.Name("id")
    COMMENT = hydra.core.Name("comment")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")

class Dtmi(Node[str]):
    r"""A digital twin model identifier."""

Dtmi.TYPE_ = hydra.core.Name("hydra.azure.dtld.Dtmi")

@dataclass(frozen=True)
class EnumValue:
    r"""An EnumValue describes an element of an Enum."""

    name: Annotated[str, "The 'programming' name of the enum value. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$."]
    enum_value: Annotated[IntegerOrString, "The on-the-wire value that maps to the EnumValue. EnumValue may be either an integer or a string and must be unique for all enum values in this enum."]
    id: Annotated[Maybe[Dtmi], "The ID of the enum value. If no @id is provided, the digital twin interface processor will assign one."]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.EnumValue")
    NAME = hydra.core.Name("name")
    ENUM_VALUE = hydra.core.Name("enumValue")
    ID = hydra.core.Name("id")
    COMMENT = hydra.core.Name("comment")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")

@dataclass(frozen=True)
class Field:
    r"""A Field describes a field in an Object."""

    name: Annotated[str, "The 'programming' name of the field. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$."]
    schema: Annotated[Schema, "The data type of the field"]
    id: Annotated[Maybe[Dtmi], "The ID of the field. If no @id is provided, the digital twin interface processor will assign one."]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Field")
    NAME = hydra.core.Name("name")
    SCHEMA = hydra.core.Name("schema")
    ID = hydra.core.Name("id")
    COMMENT = hydra.core.Name("comment")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")

class IntegerOrStringInteger(Node[int]):
    ...

class IntegerOrStringString(Node[str]):
    ...

class _IntegerOrStringMeta(type):
    def __getitem__(cls, item):
        return object

class IntegerOrString(metaclass=_IntegerOrStringMeta):
    r"""IntegerOrStringInteger | IntegerOrStringString"""

    TYPE_ = hydra.core.Name("hydra.azure.dtld.IntegerOrString")
    INTEGER = hydra.core.Name("integer")
    STRING = hydra.core.Name("string")

@dataclass(frozen=True)
class Interface:
    id: Annotated[Dtmi, "A digital twin model identifier for the interface"]
    type: Annotated[Iri, "This must be 'Interface'"]
    context: Annotated[Iri, "The context to use when processing this interface. For this version, it must be set to 'dtmi:dtdl:context;2'"]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    contents: Annotated[Maybe[frozenset[Interface_Contents]], "A set of objects that define the contents (Telemetry, Properties, Commands, Relationships, and/or Components) of this interface"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]
    extends: Annotated[Maybe[frozenset[Interface]], "A set of DTMIs that refer to interfaces this interface inherits from. Interfaces can inherit from multiple interfaces."]
    schemas: Annotated[Maybe[frozenset[Schema_Interface]], "A set of IRIs or objects that refer to the reusable schemas within this interface."]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Interface")
    ID = hydra.core.Name("id")
    TYPE = hydra.core.Name("type")
    CONTEXT = hydra.core.Name("context")
    COMMENT = hydra.core.Name("comment")
    CONTENTS = hydra.core.Name("contents")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")
    EXTENDS = hydra.core.Name("extends")
    SCHEMAS = hydra.core.Name("schemas")

class Interface_ContentsCommand(Node["Command"]):
    ...

class Interface_ContentsComponent(Node["Component"]):
    ...

class Interface_ContentsProperty(Node["Property"]):
    ...

class Interface_ContentsRelationship(Node["Relationship"]):
    ...

class Interface_ContentsTelemetry(Node["Telemetry"]):
    ...

class _Interface_ContentsMeta(type):
    def __getitem__(cls, item):
        return object

class Interface_Contents(metaclass=_Interface_ContentsMeta):
    r"""Interface_ContentsCommand | Interface_ContentsComponent | Interface_ContentsProperty | Interface_ContentsRelationship | Interface_ContentsTelemetry"""

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Interface_Contents")
    COMMAND = hydra.core.Name("command")
    COMPONENT = hydra.core.Name("component")
    PROPERTY = hydra.core.Name("property")
    RELATIONSHIP = hydra.core.Name("relationship")
    TELEMETRY = hydra.core.Name("telemetry")

class Iri(Node[str]):
    ...

Iri.TYPE_ = hydra.core.Name("hydra.azure.dtld.Iri")

@dataclass(frozen=True)
class MapKey:
    r"""A MapKey describes the key in a Map. The schema of a MapKey must be string."""

    name: Annotated[str, "The 'programming' name of the map's key. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$."]
    schema: Annotated[Schema, "The data type of the map's key"]
    id: Annotated[Maybe[Dtmi], "The ID of the map key. If no @id is provided, the digital twin interface processor will assign one."]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.MapKey")
    NAME = hydra.core.Name("name")
    SCHEMA = hydra.core.Name("schema")
    ID = hydra.core.Name("id")
    COMMENT = hydra.core.Name("comment")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")

@dataclass(frozen=True)
class MapValue:
    r"""A MapValue describes the values in a Map."""

    name: Annotated[str, "The 'programming' name of the map's value. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$."]
    schema: Annotated[Schema, "The data type of the map's values"]
    id: Annotated[Maybe[Dtmi], "The ID of the map value. If no @id is provided, the digital twin interface processor will assign one."]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.MapValue")
    NAME = hydra.core.Name("name")
    SCHEMA = hydra.core.Name("schema")
    ID = hydra.core.Name("id")
    COMMENT = hydra.core.Name("comment")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")

@dataclass(frozen=True)
class Property:
    r"""A Property describes the read-only and read/write state of any digital twin. For example, a device serial number may be a read-only property, the desired temperature on a thermostat may be a read-write property; and the name of a room may be a read-write property."""

    type: Annotated[Iri, "This must at least be 'Property'. It can also include a semantic type."]
    name: Annotated[str, "The 'programming' name of the property. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$.."]
    schema: Annotated[Schema, "The data type of the Property"]
    id: Annotated[Maybe[Dtmi], "The ID of the property. If no @id is provided, the digital twin interface processor will assign one."]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]
    unit: Annotated[Maybe[Unit], "The unit type of the property. A semantic type is required for the unit property to be available."]
    writable: Annotated[Maybe[bool], "A boolean value that indicates whether the property is writable by an external source, such as an application, or not. The default value is false (read-only)."]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Property")
    TYPE = hydra.core.Name("type")
    NAME = hydra.core.Name("name")
    SCHEMA = hydra.core.Name("schema")
    ID = hydra.core.Name("id")
    COMMENT = hydra.core.Name("comment")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")
    UNIT = hydra.core.Name("unit")
    WRITABLE = hydra.core.Name("writable")

@dataclass(frozen=True)
class Relationship:
    r"""A Relationship describes a link to another digital twin and enables graphs of digital twins to be created. Relationships are different from Components because they describe a link to a separate digital twin."""

    type: Annotated[Iri, "This must be 'Relationship'"]
    name: Annotated[str, "The 'programming' name of the relationship. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$."]
    id: Annotated[Maybe[Dtmi], "The ID of the relationship description. If no @id is provided, the digital twin interface processor will assign one."]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]
    max_multiplicity: Annotated[Maybe[int], "The maximum multiplicity for the target of the relationship. The default value is infinite (there may be an unlimited number of relationship instances for this relationship)."]
    min_multiplicity: Annotated[Maybe[int], "The minimum multiplicity for the target of the relationship. The default value is 0 (this relationship is permitted to have no instances). In DTDL v2, minMultiplicity must always be 0."]
    properties: Annotated[Maybe[frozenset[Property]], "A set of Properties that define relationship-specific state"]
    target: Annotated[Maybe[Interface], "An interface ID. The default value (when target is not specified) is that the target may be any interface."]
    writable: Annotated[Maybe[bool], "A boolean value that indicates whether the relationship is writable by an external source, such as an application, or not. The default value is false (read-only)."]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Relationship")
    TYPE = hydra.core.Name("type")
    NAME = hydra.core.Name("name")
    ID = hydra.core.Name("id")
    COMMENT = hydra.core.Name("comment")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")
    MAX_MULTIPLICITY = hydra.core.Name("maxMultiplicity")
    MIN_MULTIPLICITY = hydra.core.Name("minMultiplicity")
    PROPERTIES = hydra.core.Name("properties")
    TARGET = hydra.core.Name("target")
    WRITABLE = hydra.core.Name("writable")

class SchemaPrimitive(Node["Schema_Primitive"]):
    ...

class SchemaComplex(Node["Schema_Complex"]):
    ...

class _SchemaMeta(type):
    def __getitem__(cls, item):
        return object

# Schemas are used to describe the on-the-wire or serialized format of the data in a digital twin interface. A full set of primitive data types are provided, along with support for a variety of complex schemas in the forms of Arrays, Enums, Maps, and Objects. Schemas described through digital twin's schema definition language are compatible with popular serialization formats, including JSON, Avro, and Protobuf.
class Schema(metaclass=_SchemaMeta):
    r"""SchemaPrimitive | SchemaComplex"""

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Schema")
    PRIMITIVE = hydra.core.Name("primitive")
    COMPLEX = hydra.core.Name("complex")

@dataclass(frozen=True)
class Schema_Array:
    r"""An Array describes an indexable data type where each element is of the same schema. An Array elements' schema can itself be a primitive or complex schema."""

    type: Annotated[Iri, "This must be 'Array'"]
    element_schema: Annotated[Schema, "The data type of the array elements"]
    id: Annotated[Maybe[Dtmi], "The ID of the array. If no @id is provided, the digital twin interface processor will assign one."]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Schema_Array")
    TYPE = hydra.core.Name("type")
    ELEMENT_SCHEMA = hydra.core.Name("elementSchema")
    ID = hydra.core.Name("id")
    COMMENT = hydra.core.Name("comment")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")

class Schema_ComplexArray(Node["Schema_Array"]):
    ...

class Schema_ComplexEnum(Node["Schema_Enum"]):
    ...

class Schema_ComplexMap(Node["Schema_Map"]):
    ...

class Schema_ComplexObject(Node["Schema_Object"]):
    ...

class _Schema_ComplexMeta(type):
    def __getitem__(cls, item):
        return object

# Complex schemas are designed for supporting complex data types made up of primitive data types. Currently the following complex schemas are provided: Array, Enum, Map, and Object. A complex schema can be specified directly as the value in a schema statement or described in the interface schemas set and referenced in the schema statement.
class Schema_Complex(metaclass=_Schema_ComplexMeta):
    r"""Schema_ComplexArray | Schema_ComplexEnum | Schema_ComplexMap | Schema_ComplexObject"""

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Schema_Complex")
    ARRAY = hydra.core.Name("array")
    ENUM = hydra.core.Name("enum")
    MAP = hydra.core.Name("map")
    OBJECT = hydra.core.Name("object")

@dataclass(frozen=True)
class Schema_Enum:
    r"""An Enum describes a data type with a set of named labels that map to values. The values in an Enum can be either integers or strings, but the labels are always strings."""

    type: Annotated[Iri, "Enum"]
    enum_values: Annotated[frozenlist[EnumValue], "A set of enum value and label mappings"]
    value_schema: Annotated[IntegerOrString, "The data type for the enum values. All enum values must be of the same type."]
    id: Annotated[Maybe[Dtmi], "The ID of the enum. If no @id is provided, the digital twin interface processor will assign one."]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Schema_Enum")
    TYPE = hydra.core.Name("type")
    ENUM_VALUES = hydra.core.Name("enumValues")
    VALUE_SCHEMA = hydra.core.Name("valueSchema")
    ID = hydra.core.Name("id")
    COMMENT = hydra.core.Name("comment")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")

@dataclass(frozen=True)
class Schema_Interface:
    r"""Within an interface definition, complex schemas may be defined for reusability across Telemetry, Properties, and Commands. This is designed to promote readability and improved maintenance because schemas that are reused can be defined once (per interface). Interface schemas are defined in the schemas property of an interface."""

    id: Annotated[Dtmi, "The globally unique identifier for the schema"]
    type: Annotated[Schema_Interface_Type, "The type of complex schema. This must refer to one of the complex schema classes (Array, Enum, Map, or Object)."]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Schema_Interface")
    ID = hydra.core.Name("id")
    TYPE = hydra.core.Name("type")
    COMMENT = hydra.core.Name("comment")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")

class Schema_Interface_TypeArray(Node["Schema_Array"]):
    ...

class Schema_Interface_TypeEnum(Node["Schema_Enum"]):
    ...

class Schema_Interface_TypeMap(Node["Schema_Map"]):
    ...

class Schema_Interface_TypeObject(Node["Schema_Object"]):
    ...

class _Schema_Interface_TypeMeta(type):
    def __getitem__(cls, item):
        return object

class Schema_Interface_Type(metaclass=_Schema_Interface_TypeMeta):
    r"""Schema_Interface_TypeArray | Schema_Interface_TypeEnum | Schema_Interface_TypeMap | Schema_Interface_TypeObject"""

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Schema_Interface_Type")
    ARRAY = hydra.core.Name("array")
    ENUM = hydra.core.Name("enum")
    MAP = hydra.core.Name("map")
    OBJECT = hydra.core.Name("object")

@dataclass(frozen=True)
class Schema_Map:
    r"""A Map describes a data type of key-value pairs where the values share the same schema. The key in a Map must be a string. The values in a Map can be any schema."""

    type: Annotated[Iri, "Map"]
    map_key: Annotated[MapKey, "A description of the keys in the map"]
    map_value: Annotated[MapValue, "A description of the values in the map"]
    id: Annotated[Maybe[Dtmi], "The ID of the map. If no @id is provided, the digital twin interface processor will assign one."]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Schema_Map")
    TYPE = hydra.core.Name("type")
    MAP_KEY = hydra.core.Name("mapKey")
    MAP_VALUE = hydra.core.Name("mapValue")
    ID = hydra.core.Name("id")
    COMMENT = hydra.core.Name("comment")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")

@dataclass(frozen=True)
class Schema_Object:
    r"""An Object describes a data type made up of named fields (like a struct in C). The fields in an Object map can be primitive or complex schemas."""

    type: Annotated[Iri, "Object"]
    fields: Annotated[frozenset[Field], "A set of field descriptions, one for each field in the Object"]
    id: Annotated[Maybe[Dtmi], "The ID of the object. If no @id is provided, the digital twin interface processor will assign one."]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Schema_Object")
    TYPE = hydra.core.Name("type")
    FIELDS = hydra.core.Name("fields")
    ID = hydra.core.Name("id")
    COMMENT = hydra.core.Name("comment")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")

class Schema_Primitive(Enum):
    r"""A full set of primitive data types are provided and can be specified directly as the value in a schema statement in a digital twin interface."""

    BOOLEAN = hydra.core.Name("boolean")
    r"""A boolean value"""

    DATE = hydra.core.Name("date")
    r"""A full-date as defined in section 5.6 of RFC 3339"""

    DATE_TIME = hydra.core.Name("dateTime")
    r"""A date-time as defined in RFC 3339"""

    DOUBLE = hydra.core.Name("double")
    r"""An IEEE 8-byte floating point"""

    DURATION = hydra.core.Name("duration")
    r"""A duration in ISO 8601 format"""

    FLOAT = hydra.core.Name("float")
    r"""An IEEE 4-byte floating point"""

    INTEGER = hydra.core.Name("integer")
    r"""A signed 4-byte integer"""

    LONG = hydra.core.Name("long")
    r"""A signed 8-byte integer"""

    STRING = hydra.core.Name("string")
    r"""A UTF8 string"""

    TIME = hydra.core.Name("time")
    r"""A full-time as defined in section 5.6 of RFC 3339"""

Schema_Primitive.TYPE_ = hydra.core.Name("hydra.azure.dtld.Schema_Primitive")

@dataclass(frozen=True)
class Telemetry:
    r"""Telemetry describes the data emitted by any digital twin, whether the data is a regular stream of sensor readings or a computed stream of data, such as occupancy, or an occasional error or information message."""

    type: Annotated[Iri, "This must be at least 'Telemetry'. It can also include a semantic type"]
    name: Annotated[str, "The 'programming' name of the telemetry. The name may only contain the characters a-z, A-Z, 0-9, and underscore, and must match this regular expression ^[a-zA-Z](?:[a-zA-Z0-9_]*[a-zA-Z0-9])?$.."]
    schema: Annotated[Schema, "The data type of the Telemetry"]
    id: Annotated[Maybe[Dtmi], "The ID of the telemetry. If no @id is provided, the digital twin interface processor will assign one."]
    comment: Annotated[Maybe[str], "A comment for model authors"]
    description: Annotated[Maybe[str], "A localizable description for display"]
    display_name: Annotated[Maybe[str], "A localizable name for display"]
    unit: Annotated[Maybe[Unit], "The unit type of the Telemetry. A semantic type is required for the unit property to be available."]

    TYPE_ = hydra.core.Name("hydra.azure.dtld.Telemetry")
    TYPE = hydra.core.Name("type")
    NAME = hydra.core.Name("name")
    SCHEMA = hydra.core.Name("schema")
    ID = hydra.core.Name("id")
    COMMENT = hydra.core.Name("comment")
    DESCRIPTION = hydra.core.Name("description")
    DISPLAY_NAME = hydra.core.Name("displayName")
    UNIT = hydra.core.Name("unit")

class Unit(Node[None]):
    ...

Unit.TYPE_ = hydra.core.Name("hydra.azure.dtld.Unit")
