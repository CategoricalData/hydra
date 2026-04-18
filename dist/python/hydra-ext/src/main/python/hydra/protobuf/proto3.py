# Note: this is an automatically generated file. Do not edit.

r"""A model for Protocol Buffers v3 enum and message types, designed as a target for transformations.This model is loosely based on https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/type.proto, as well as the proto3 reference documentation."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core

class DefinitionEnum(Node["EnumDefinition"]):
    ...

class DefinitionMessage(Node["MessageDefinition"]):
    ...

class _DefinitionMeta(type):
    def __getitem__(cls, item):
        return object

class Definition(metaclass=_DefinitionMeta):
    r"""DefinitionEnum | DefinitionMessage"""

    TYPE_ = hydra.core.Name("hydra.protobuf.proto3.Definition")
    ENUM = hydra.core.Name("enum")
    MESSAGE = hydra.core.Name("message")

@dataclass(frozen=True)
class EnumDefinition:
    r"""Enum type definition."""

    name: Annotated[TypeName, "Enum type name"]
    values: Annotated[frozenlist[EnumValue], "Enum value definitions"]
    options: Annotated[frozenlist[Option], "Protocol buffer options"]

    TYPE_ = hydra.core.Name("hydra.protobuf.proto3.EnumDefinition")
    NAME = hydra.core.Name("name")
    VALUES = hydra.core.Name("values")
    OPTIONS = hydra.core.Name("options")

@dataclass(frozen=True)
class EnumValue:
    r"""Enum value definition."""

    name: Annotated[EnumValueName, "Enum value name"]
    number: Annotated[int, "Enum value number"]
    options: Annotated[frozenlist[Option], "Protocol buffer options"]

    TYPE_ = hydra.core.Name("hydra.protobuf.proto3.EnumValue")
    NAME = hydra.core.Name("name")
    NUMBER = hydra.core.Name("number")
    OPTIONS = hydra.core.Name("options")

class EnumValueName(Node[str]):
    ...

EnumValueName.TYPE_ = hydra.core.Name("hydra.protobuf.proto3.EnumValueName")

@dataclass(frozen=True)
class Field:
    r"""A single field of a message type."""

    name: Annotated[FieldName, "The field name"]
    json_name: Annotated[Maybe[str], "The field JSON name"]
    type: Annotated[FieldType, "The datatype of the field"]
    number: Annotated[int, "The field number"]
    options: Annotated[frozenlist[Option], "The protocol buffer options"]

    TYPE_ = hydra.core.Name("hydra.protobuf.proto3.Field")
    NAME = hydra.core.Name("name")
    JSON_NAME = hydra.core.Name("jsonName")
    TYPE = hydra.core.Name("type")
    NUMBER = hydra.core.Name("number")
    OPTIONS = hydra.core.Name("options")

class FieldName(Node[str]):
    r"""The name of a field."""

FieldName.TYPE_ = hydra.core.Name("hydra.protobuf.proto3.FieldName")

class FieldTypeMap(Node["MapType"]):
    ...

class FieldTypeOneof(Node["frozenlist[Field]"]):
    ...

class FieldTypeRepeated(Node["SimpleType"]):
    ...

class FieldTypeSimple(Node["SimpleType"]):
    ...

class _FieldTypeMeta(type):
    def __getitem__(cls, item):
        return object

class FieldType(metaclass=_FieldTypeMeta):
    r"""FieldTypeMap | FieldTypeOneof | FieldTypeRepeated | FieldTypeSimple"""

    TYPE_ = hydra.core.Name("hydra.protobuf.proto3.FieldType")
    MAP = hydra.core.Name("map")
    ONEOF = hydra.core.Name("oneof")
    REPEATED = hydra.core.Name("repeated")
    SIMPLE = hydra.core.Name("simple")

class FileReference(Node[str]):
    ...

FileReference.TYPE_ = hydra.core.Name("hydra.protobuf.proto3.FileReference")

@dataclass(frozen=True)
class MapType:
    keys: SimpleType
    values: SimpleType

    TYPE_ = hydra.core.Name("hydra.protobuf.proto3.MapType")
    KEYS = hydra.core.Name("keys")
    VALUES = hydra.core.Name("values")

@dataclass(frozen=True)
class MessageDefinition:
    r"""A protocol buffer message type."""

    name: Annotated[TypeName, "The fully qualified message name"]
    fields: Annotated[frozenlist[Field], "The list of fields"]
    options: Annotated[frozenlist[Option], "The protocol buffer options"]

    TYPE_ = hydra.core.Name("hydra.protobuf.proto3.MessageDefinition")
    NAME = hydra.core.Name("name")
    FIELDS = hydra.core.Name("fields")
    OPTIONS = hydra.core.Name("options")

@dataclass(frozen=True)
class Option:
    r"""A protocol buffer option, which can be attached to a message, field, enumeration, etc."""

    name: Annotated[str, "The option's name. For protobuf built-in options (options defined in descriptor.proto), this is the short name. For example, `\"map_entry\"`. For custom options, it should be the fully-qualified name. For example, `\"google.api.http\"`."]
    value: Annotated[Value, "The option's value"]

    TYPE_ = hydra.core.Name("hydra.protobuf.proto3.Option")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")

class PackageName(Node[str]):
    ...

PackageName.TYPE_ = hydra.core.Name("hydra.protobuf.proto3.PackageName")

@dataclass(frozen=True)
class ProtoFile:
    r"""A .proto file, usually containing one or more enum or message type definitions."""

    package: PackageName
    imports: frozenlist[FileReference]
    types: frozenlist[Definition]
    options: frozenlist[Option]

    TYPE_ = hydra.core.Name("hydra.protobuf.proto3.ProtoFile")
    PACKAGE = hydra.core.Name("package")
    IMPORTS = hydra.core.Name("imports")
    TYPES = hydra.core.Name("types")
    OPTIONS = hydra.core.Name("options")

class ScalarType(Enum):
    r"""One of several Proto3 scalar types."""

    BOOL = hydra.core.Name("bool")

    BYTES = hydra.core.Name("bytes")

    DOUBLE = hydra.core.Name("double")

    FIXED32 = hydra.core.Name("fixed32")

    FIXED64 = hydra.core.Name("fixed64")

    FLOAT = hydra.core.Name("float")

    INT32 = hydra.core.Name("int32")

    INT64 = hydra.core.Name("int64")

    SFIXED32 = hydra.core.Name("sfixed32")

    SFIXED64 = hydra.core.Name("sfixed64")

    SINT32 = hydra.core.Name("sint32")

    SINT64 = hydra.core.Name("sint64")

    STRING = hydra.core.Name("string")

    UINT32 = hydra.core.Name("uint32")

    UINT64 = hydra.core.Name("uint64")

ScalarType.TYPE_ = hydra.core.Name("hydra.protobuf.proto3.ScalarType")

class SimpleTypeReference(Node["TypeName"]):
    ...

class SimpleTypeScalar(Node["ScalarType"]):
    ...

class _SimpleTypeMeta(type):
    def __getitem__(cls, item):
        return object

# A scalar type or a reference to an enum type or message type.
class SimpleType(metaclass=_SimpleTypeMeta):
    r"""SimpleTypeReference | SimpleTypeScalar"""

    TYPE_ = hydra.core.Name("hydra.protobuf.proto3.SimpleType")
    REFERENCE = hydra.core.Name("reference")
    SCALAR = hydra.core.Name("scalar")

class TypeName(Node[str]):
    r"""The local name of an enum type or message type."""

TypeName.TYPE_ = hydra.core.Name("hydra.protobuf.proto3.TypeName")

class TypeReference(Node[str]):
    r"""A reference to an enum type or message type."""

TypeReference.TYPE_ = hydra.core.Name("hydra.protobuf.proto3.TypeReference")

class ValueBoolean(Node[bool]):
    ...

class ValueString(Node[str]):
    ...

class _ValueMeta(type):
    def __getitem__(cls, item):
        return object

# A scalar value.
class Value(metaclass=_ValueMeta):
    r"""ValueBoolean | ValueString"""

    TYPE_ = hydra.core.Name("hydra.protobuf.proto3.Value")
    BOOLEAN = hydra.core.Name("boolean")
    STRING = hydra.core.Name("string")
