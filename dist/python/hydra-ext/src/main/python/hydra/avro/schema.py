# Note: this is an automatically generated file. Do not edit.

r"""A model for Avro schemas. Based on the Avro 1.11.1 specification:
  https://avro.apache.org/docs/1.11.1/specification."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core
import hydra.json.model

@dataclass(frozen=True)
class Array:
    items: Schema

    TYPE_ = hydra.core.Name("hydra.avro.schema.Array")
    ITEMS = hydra.core.Name("items")

@dataclass(frozen=True)
class Enum:
    symbols: Annotated[frozenlist[str], "a JSON array, listing symbols, as JSON strings. All symbols in an enum must be unique; duplicates are prohibited. Every symbol must match the regular expression [A-Za-z_][A-Za-z0-9_]* (the same requirement as for names)"]
    default: Annotated[Maybe[str], "A default value for this enumeration, used during resolution when the reader encounters a symbol from the writer that isn't defined in the reader's schema. The value provided here must be a JSON string that's a member of the symbols array"]

    TYPE_ = hydra.core.Name("hydra.avro.schema.Enum")
    SYMBOLS = hydra.core.Name("symbols")
    DEFAULT = hydra.core.Name("default")

@dataclass(frozen=True)
class Field:
    name: Annotated[str, "a JSON string providing the name of the field"]
    doc: Annotated[Maybe[str], "a JSON string describing this field for users"]
    type: Annotated[Schema, "a schema"]
    default: Annotated[Maybe[hydra.json.model.Value], "default value for this field, only used when reading instances that lack the field for schema evolution purposes"]
    order: Annotated[Maybe[Order], "specifies how this field impacts sort ordering of this record"]
    aliases: Annotated[Maybe[frozenlist[str]], "a JSON array of strings, providing alternate names for this field"]
    annotations: Annotated[FrozenDict[str, hydra.json.model.Value], "Any additional key/value pairs attached to the field"]

    TYPE_ = hydra.core.Name("hydra.avro.schema.Field")
    NAME = hydra.core.Name("name")
    DOC = hydra.core.Name("doc")
    TYPE = hydra.core.Name("type")
    DEFAULT = hydra.core.Name("default")
    ORDER = hydra.core.Name("order")
    ALIASES = hydra.core.Name("aliases")
    ANNOTATIONS = hydra.core.Name("annotations")

@dataclass(frozen=True)
class Fixed:
    size: Annotated[int, "an integer, specifying the number of bytes per value"]

    TYPE_ = hydra.core.Name("hydra.avro.schema.Fixed")
    SIZE = hydra.core.Name("size")

@dataclass(frozen=True)
class Map:
    values: Schema

    TYPE_ = hydra.core.Name("hydra.avro.schema.Map")
    VALUES = hydra.core.Name("values")

@dataclass(frozen=True)
class Named:
    name: Annotated[str, "a string naming this schema"]
    namespace: Annotated[Maybe[str], "a string that qualifies the name"]
    aliases: Annotated[Maybe[frozenlist[str]], "a JSON array of strings, providing alternate names for this schema"]
    doc: Annotated[Maybe[str], "a JSON string providing documentation to the user of this schema"]
    type: NamedType
    annotations: Annotated[FrozenDict[str, hydra.json.model.Value], "Any additional key/value pairs attached to the type"]

    TYPE_ = hydra.core.Name("hydra.avro.schema.Named")
    NAME = hydra.core.Name("name")
    NAMESPACE = hydra.core.Name("namespace")
    ALIASES = hydra.core.Name("aliases")
    DOC = hydra.core.Name("doc")
    TYPE = hydra.core.Name("type")
    ANNOTATIONS = hydra.core.Name("annotations")

class NamedTypeEnum(Node["Enum"]):
    ...

class NamedTypeFixed(Node["Fixed"]):
    ...

class NamedTypeRecord(Node["Record"]):
    ...

class _NamedTypeMeta(type):
    def __getitem__(cls, item):
        return object

class NamedType(metaclass=_NamedTypeMeta):
    r"""NamedTypeEnum | NamedTypeFixed | NamedTypeRecord"""

    TYPE_ = hydra.core.Name("hydra.avro.schema.NamedType")
    ENUM = hydra.core.Name("enum")
    FIXED = hydra.core.Name("fixed")
    RECORD = hydra.core.Name("record")

class Order(Enum):
    ASCENDING = hydra.core.Name("ascending")

    DESCENDING = hydra.core.Name("descending")

    IGNORE = hydra.core.Name("ignore")

Order.TYPE_ = hydra.core.Name("hydra.avro.schema.Order")

class Primitive(Enum):
    NULL = hydra.core.Name("null")
    r"""no value"""

    BOOLEAN = hydra.core.Name("boolean")
    r"""A binary value"""

    INT = hydra.core.Name("int")
    r"""32-bit signed integer"""

    LONG = hydra.core.Name("long")
    r"""64-bit signed integer"""

    FLOAT = hydra.core.Name("float")
    r"""single precision (32-bit) IEEE 754 floating-point number"""

    DOUBLE = hydra.core.Name("double")
    r"""double precision (64-bit) IEEE 754 floating-point number"""

    BYTES = hydra.core.Name("bytes")
    r"""sequence of 8-bit unsigned bytes"""

    STRING = hydra.core.Name("string")
    r"""unicode character sequence"""

Primitive.TYPE_ = hydra.core.Name("hydra.avro.schema.Primitive")

@dataclass(frozen=True)
class Record:
    fields: Annotated[frozenlist[Field], "a JSON array, listing fields"]

    TYPE_ = hydra.core.Name("hydra.avro.schema.Record")
    FIELDS = hydra.core.Name("fields")

class SchemaArray(Node["Array"]):
    ...

class SchemaMap(Node["Map"]):
    ...

class SchemaNamed(Node["Named"]):
    ...

class SchemaPrimitive(Node["Primitive"]):
    ...

class SchemaReference(Node[str]):
    r"""A reference by name to a previously defined type"""

class SchemaUnion(Node["Union"]):
    ...

class _SchemaMeta(type):
    def __getitem__(cls, item):
        return object

class Schema(metaclass=_SchemaMeta):
    r"""SchemaArray | SchemaMap | SchemaNamed | SchemaPrimitive | SchemaReference | SchemaUnion"""

    TYPE_ = hydra.core.Name("hydra.avro.schema.Schema")
    ARRAY = hydra.core.Name("array")
    MAP = hydra.core.Name("map")
    NAMED = hydra.core.Name("named")
    PRIMITIVE = hydra.core.Name("primitive")
    REFERENCE = hydra.core.Name("reference")
    UNION = hydra.core.Name("union")

class Union(Node["frozenlist[Schema]"]):
    ...

Union.TYPE_ = hydra.core.Name("hydra.avro.schema.Union")
