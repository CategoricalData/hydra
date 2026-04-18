# Note: this is an automatically generated file. Do not edit.

r"""A model for PDL (Pegasus Data Language) schemas. Based on the specification at:
  https://linkedin.github.io/rest.li/pdl_schema."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core
import hydra.json.model

@dataclass(frozen=True)
class Annotations:
    r"""Annotations which can be applied to record fields, aliased union members, enum symbols, or named schemas."""

    doc: Maybe[str]
    deprecated: bool

    TYPE_ = hydra.core.Name("hydra.pegasus.pdl.Annotations")
    DOC = hydra.core.Name("doc")
    DEPRECATED = hydra.core.Name("deprecated")

@dataclass(frozen=True)
class EnumField:
    name: EnumFieldName
    annotations: Annotations

    TYPE_ = hydra.core.Name("hydra.pegasus.pdl.EnumField")
    NAME = hydra.core.Name("name")
    ANNOTATIONS = hydra.core.Name("annotations")

class EnumFieldName(Node[str]):
    ...

EnumFieldName.TYPE_ = hydra.core.Name("hydra.pegasus.pdl.EnumFieldName")

@dataclass(frozen=True)
class EnumSchema:
    fields: frozenlist[EnumField]

    TYPE_ = hydra.core.Name("hydra.pegasus.pdl.EnumSchema")
    FIELDS = hydra.core.Name("fields")

class FieldName(Node[str]):
    ...

FieldName.TYPE_ = hydra.core.Name("hydra.pegasus.pdl.FieldName")

@dataclass(frozen=True)
class NamedSchema:
    qualified_name: QualifiedName
    type: NamedSchemaType
    annotations: Annotations

    TYPE_ = hydra.core.Name("hydra.pegasus.pdl.NamedSchema")
    QUALIFIED_NAME = hydra.core.Name("qualifiedName")
    TYPE = hydra.core.Name("type")
    ANNOTATIONS = hydra.core.Name("annotations")

class NamedSchemaTypeRecord(Node["RecordSchema"]):
    ...

class NamedSchemaTypeEnum(Node["EnumSchema"]):
    ...

class NamedSchemaTypeTyperef(Node["Schema"]):
    ...

class _NamedSchemaTypeMeta(type):
    def __getitem__(cls, item):
        return object

class NamedSchemaType(metaclass=_NamedSchemaTypeMeta):
    r"""NamedSchemaTypeRecord | NamedSchemaTypeEnum | NamedSchemaTypeTyperef"""

    TYPE_ = hydra.core.Name("hydra.pegasus.pdl.NamedSchemaType")
    RECORD = hydra.core.Name("record")
    ENUM = hydra.core.Name("enum")
    TYPEREF = hydra.core.Name("typeref")

class Name(Node[str]):
    ...

Name.TYPE_ = hydra.core.Name("hydra.pegasus.pdl.Name")

class Namespace(Node[str]):
    ...

Namespace.TYPE_ = hydra.core.Name("hydra.pegasus.pdl.Namespace")

class Package(Node[str]):
    ...

Package.TYPE_ = hydra.core.Name("hydra.pegasus.pdl.Package")

class PrimitiveType(Enum):
    BOOLEAN = hydra.core.Name("boolean")

    BYTES = hydra.core.Name("bytes")

    DOUBLE = hydra.core.Name("double")

    FLOAT = hydra.core.Name("float")

    INT = hydra.core.Name("int")

    LONG = hydra.core.Name("long")

    STRING = hydra.core.Name("string")

PrimitiveType.TYPE_ = hydra.core.Name("hydra.pegasus.pdl.PrimitiveType")

class PropertyKey(Node[str]):
    ...

PropertyKey.TYPE_ = hydra.core.Name("hydra.pegasus.pdl.PropertyKey")

@dataclass(frozen=True)
class Property:
    key: PropertyKey
    value: Maybe[hydra.json.model.Value]

    TYPE_ = hydra.core.Name("hydra.pegasus.pdl.Property")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class QualifiedName:
    name: Name
    namespace: Maybe[Namespace]

    TYPE_ = hydra.core.Name("hydra.pegasus.pdl.QualifiedName")
    NAME = hydra.core.Name("name")
    NAMESPACE = hydra.core.Name("namespace")

@dataclass(frozen=True)
class RecordField:
    r"""Note: the default value for an enum-valued must be one of the enumerated string symbols."""

    name: FieldName
    value: Schema
    optional: bool
    default: Maybe[hydra.json.model.Value]
    annotations: Annotations

    TYPE_ = hydra.core.Name("hydra.pegasus.pdl.RecordField")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")
    OPTIONAL = hydra.core.Name("optional")
    DEFAULT = hydra.core.Name("default")
    ANNOTATIONS = hydra.core.Name("annotations")

@dataclass(frozen=True)
class RecordSchema:
    r"""Note: all included schemas must be record schemas."""

    fields: frozenlist[RecordField]
    includes: frozenlist[NamedSchema]

    TYPE_ = hydra.core.Name("hydra.pegasus.pdl.RecordSchema")
    FIELDS = hydra.core.Name("fields")
    INCLUDES = hydra.core.Name("includes")

class SchemaArray(Node["Schema"]):
    ...

class SchemaFixed(Node[int]):
    ...

class SchemaInline(Node["NamedSchema"]):
    ...

class SchemaMap(Node["Schema"]):
    ...

class SchemaNamed(Node["QualifiedName"]):
    ...

class SchemaNull:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SchemaNull)
    def __hash__(self):
        return hash("SchemaNull")

class SchemaPrimitive(Node["PrimitiveType"]):
    ...

class SchemaUnion(Node["UnionSchema"]):
    ...

class _SchemaMeta(type):
    def __getitem__(cls, item):
        return object

class Schema(metaclass=_SchemaMeta):
    r"""SchemaArray | SchemaFixed | SchemaInline | SchemaMap | SchemaNamed | SchemaNull | SchemaPrimitive | SchemaUnion"""

    TYPE_ = hydra.core.Name("hydra.pegasus.pdl.Schema")
    ARRAY = hydra.core.Name("array")
    FIXED = hydra.core.Name("fixed")
    INLINE = hydra.core.Name("inline")
    MAP = hydra.core.Name("map")
    NAMED = hydra.core.Name("named")
    NULL = hydra.core.Name("null")
    PRIMITIVE = hydra.core.Name("primitive")
    UNION = hydra.core.Name("union")

@dataclass(frozen=True)
class SchemaFile:
    namespace: Namespace
    package: Maybe[Package]
    imports: frozenlist[QualifiedName]
    schemas: frozenlist[NamedSchema]

    TYPE_ = hydra.core.Name("hydra.pegasus.pdl.SchemaFile")
    NAMESPACE = hydra.core.Name("namespace")
    PACKAGE = hydra.core.Name("package")
    IMPORTS = hydra.core.Name("imports")
    SCHEMAS = hydra.core.Name("schemas")

@dataclass(frozen=True)
class UnionMember:
    r"""Note: annotations are only available for aliased members."""

    alias: Maybe[FieldName]
    value: Schema
    annotations: Annotations

    TYPE_ = hydra.core.Name("hydra.pegasus.pdl.UnionMember")
    ALIAS = hydra.core.Name("alias")
    VALUE = hydra.core.Name("value")
    ANNOTATIONS = hydra.core.Name("annotations")

class UnionSchema(Node["frozenlist[UnionMember]"]):
    r"""Note: unions are not allowed as member types of other unions."""

UnionSchema.TYPE_ = hydra.core.Name("hydra.pegasus.pdl.UnionSchema")
