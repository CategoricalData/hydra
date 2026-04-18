# Note: this is an automatically generated file. Do not edit.

r"""A partial Delta Parquet model, based on DataType and its subclasses as specified in the 3.0.0 Java API: https://docs.delta.io/3.0.0/api/java/kernel/io/delta/kernel/types/DataType.html."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class ArrayType:
    r"""Represent array data type."""

    element_type: DataType
    contains_null: bool

    TYPE_ = hydra.core.Name("hydra.delta.parquet.ArrayType")
    ELEMENT_TYPE = hydra.core.Name("elementType")
    CONTAINS_NULL = hydra.core.Name("containsNull")

class BasePrimitiveType(Enum):
    r"""Base class for all primitive types DataType."""

    BINARY = hydra.core.Name("binary")
    r"""The data type representing byte[] values."""

    BOOLEAN = hydra.core.Name("boolean")
    r"""Data type representing boolean type values."""

    BYTE = hydra.core.Name("byte")
    r"""The data type representing byte type values."""

    DATE = hydra.core.Name("date")
    r"""A date type, supporting "0001-01-01" through "9999-12-31". Internally, this is represented as the number of days from 1970-01-01."""

    DOUBLE = hydra.core.Name("double")
    r"""The data type representing double type values."""

    FLOAT = hydra.core.Name("float")
    r"""The data type representing float type values."""

    INTEGER = hydra.core.Name("integer")
    r"""The data type representing integer type values."""

    LONG = hydra.core.Name("long")
    r"""The data type representing long type values."""

    SHORT = hydra.core.Name("short")
    r"""The data type representing short type values."""

    STRING = hydra.core.Name("string")
    r"""The data type representing string type values."""

    TIMESTAMP = hydra.core.Name("timestamp")
    r"""A timestamp type, supporting [0001-01-01T00:00:00.000000Z, 9999-12-31T23:59:59.999999Z] where the left/right-bound is a date and time of the proleptic Gregorian calendar in UTC+00:00. Internally, this is represented as the number of microseconds since the Unix epoch, 1970-01-01 00:00:00 UTC."""

BasePrimitiveType.TYPE_ = hydra.core.Name("hydra.delta.parquet.BasePrimitiveType")

class DataTypeArray(Node["ArrayType"]):
    r"""Represent array data type."""

class DataTypeBase(Node["BasePrimitiveType"]):
    r"""Base class for all primitive types DataType."""

class DataTypeDecimal(Node["DecimalType"]):
    r"""A decimal data type."""

class DataTypeMap(Node["MapType"]):
    r"""Data type representing a map type."""

class DataTypeStruct(Node["StructType"]):
    r"""Struct type which contains one or more columns."""

class _DataTypeMeta(type):
    def __getitem__(cls, item):
        return object

class DataType(metaclass=_DataTypeMeta):
    r"""DataTypeArray | DataTypeBase | DataTypeDecimal | DataTypeMap | DataTypeStruct"""

    TYPE_ = hydra.core.Name("hydra.delta.parquet.DataType")
    ARRAY = hydra.core.Name("array")
    BASE = hydra.core.Name("base")
    DECIMAL = hydra.core.Name("decimal")
    MAP = hydra.core.Name("map")
    STRUCT = hydra.core.Name("struct")

@dataclass(frozen=True)
class DecimalType:
    r"""A decimal data type with fixed precision (the maximum number of digits) and scale (the number of digits on right side of dot). The precision can be up to 38, scale can also be up to 38 (less or equal to precision)."""

    precision: int
    scale: int

    TYPE_ = hydra.core.Name("hydra.delta.parquet.DecimalType")
    PRECISION = hydra.core.Name("precision")
    SCALE = hydra.core.Name("scale")

@dataclass(frozen=True)
class MapType:
    r"""Data type representing a map type."""

    key_type: DataType
    value_type: DataType
    value_contains_null: bool

    TYPE_ = hydra.core.Name("hydra.delta.parquet.MapType")
    KEY_TYPE = hydra.core.Name("keyType")
    VALUE_TYPE = hydra.core.Name("valueType")
    VALUE_CONTAINS_NULL = hydra.core.Name("valueContainsNull")

@dataclass(frozen=True)
class StructField:
    r"""Represents a subfield of StructType with additional properties and metadata."""

    name: str
    data_type: DataType
    nullable: bool

    TYPE_ = hydra.core.Name("hydra.delta.parquet.StructField")
    NAME = hydra.core.Name("name")
    DATA_TYPE = hydra.core.Name("dataType")
    NULLABLE = hydra.core.Name("nullable")

@dataclass(frozen=True)
class StructType:
    r"""Struct type which contains one or more columns."""

    fields: frozenlist[StructField]

    TYPE_ = hydra.core.Name("hydra.delta.parquet.StructType")
    FIELDS = hydra.core.Name("fields")
