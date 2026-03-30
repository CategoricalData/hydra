# Note: this is an automatically generated file. Do not edit.

r"""A syntax model for TinkerPop's GraphSON format. This model is designed to be as inclusive as possible, supporting GraphSON 4.0 as well as earlier versions. See https://github.com/apache/tinkerpop/blob/master/docs/src/dev/io/graphson.asciidoc."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

class BigDecimalValue(Node[str]):
    ...

BigDecimalValue.TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.BigDecimalValue")

@dataclass(frozen=True)
class CompositeTypedValue:
    type: TypeName
    fields: Map

    TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.CompositeTypedValue")
    TYPE = hydra.core.Name("type")
    FIELDS = hydra.core.Name("fields")

class DateTime(Node[str]):
    ...

DateTime.TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.DateTime")

class DoubleValueFinite(Node[float]):
    ...

class DoubleValueInfinity:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, DoubleValueInfinity)
    def __hash__(self):
        return hash("DoubleValueInfinity")

class DoubleValueNegativeInfinity:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, DoubleValueNegativeInfinity)
    def __hash__(self):
        return hash("DoubleValueNegativeInfinity")

class DoubleValueNotANumber:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, DoubleValueNotANumber)
    def __hash__(self):
        return hash("DoubleValueNotANumber")

class _DoubleValueMeta(type):
    def __getitem__(cls, item):
        return object

class DoubleValue(metaclass=_DoubleValueMeta):
    r"""DoubleValueFinite | DoubleValueInfinity | DoubleValueNegativeInfinity | DoubleValueNotANumber"""

    TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.DoubleValue")
    FINITE = hydra.core.Name("finite")
    INFINITY = hydra.core.Name("infinity")
    NEGATIVE_INFINITY = hydra.core.Name("negativeInfinity")
    NOT_A_NUMBER = hydra.core.Name("notANumber")

class Duration(Node[str]):
    ...

Duration.TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.Duration")

class EdgeLabel(Node[str]):
    ...

EdgeLabel.TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.EdgeLabel")

class FloatValueFinite(Node[float]):
    ...

class FloatValueInfinity:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, FloatValueInfinity)
    def __hash__(self):
        return hash("FloatValueInfinity")

class FloatValueNegativeInfinity:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, FloatValueNegativeInfinity)
    def __hash__(self):
        return hash("FloatValueNegativeInfinity")

class FloatValueNotANumber:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, FloatValueNotANumber)
    def __hash__(self):
        return hash("FloatValueNotANumber")

class _FloatValueMeta(type):
    def __getitem__(cls, item):
        return object

class FloatValue(metaclass=_FloatValueMeta):
    r"""FloatValueFinite | FloatValueInfinity | FloatValueNegativeInfinity | FloatValueNotANumber"""

    TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.FloatValue")
    FINITE = hydra.core.Name("finite")
    INFINITY = hydra.core.Name("infinity")
    NEGATIVE_INFINITY = hydra.core.Name("negativeInfinity")
    NOT_A_NUMBER = hydra.core.Name("notANumber")

class Map(Node["frozenlist[ValuePair]"]):
    ...

Map.TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.Map")

@dataclass(frozen=True)
class AdjacentEdge:
    id: Value
    vertex_id: Value
    properties: FrozenDict[PropertyKey, Value]

    TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.AdjacentEdge")
    ID = hydra.core.Name("id")
    VERTEX_ID = hydra.core.Name("vertexId")
    PROPERTIES = hydra.core.Name("properties")

@dataclass(frozen=True)
class PrimitiveTypedValue:
    type: TypeName
    value: str

    TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.PrimitiveTypedValue")
    TYPE = hydra.core.Name("type")
    VALUE = hydra.core.Name("value")

class PropertyKey(Node[str]):
    ...

PropertyKey.TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.PropertyKey")

class TypeName(Node[str]):
    ...

TypeName.TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.TypeName")

class Uuid(Node[str]):
    ...

Uuid.TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.Uuid")

class ValueBigDecimal(Node["BigDecimalValue"]):
    ...

class ValueBigInteger(Node[int]):
    ...

class ValueBinary(Node[str]):
    ...

class ValueBoolean(Node[bool]):
    ...

class ValueByte(Node[int]):
    ...

class ValueChar(Node[int]):
    ...

class ValueComposite(Node["CompositeTypedValue"]):
    ...

class ValueDateTime(Node["DateTime"]):
    ...

class ValueDouble(Node["DoubleValue"]):
    ...

class ValueDuration(Node["Duration"]):
    ...

class ValueFloat(Node["FloatValue"]):
    ...

class ValueInteger(Node[int]):
    ...

class ValueList(Node["frozenlist[Value]"]):
    ...

class ValueLong(Node[int]):
    ...

class ValueMap(Node["Map"]):
    ...

class ValueNull:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ValueNull)
    def __hash__(self):
        return hash("ValueNull")

class ValuePrimitive(Node["PrimitiveTypedValue"]):
    ...

class ValueSet(Node["frozenlist[Value]"]):
    ...

class ValueShort(Node[int]):
    ...

class ValueString(Node[str]):
    ...

class ValueUuid(Node["Uuid"]):
    ...

class _ValueMeta(type):
    def __getitem__(cls, item):
        return object

class Value(metaclass=_ValueMeta):
    r"""ValueBigDecimal | ValueBigInteger | ValueBinary | ValueBoolean | ValueByte | ValueChar | ValueComposite | ValueDateTime | ValueDouble | ValueDuration | ValueFloat | ValueInteger | ValueList | ValueLong | ValueMap | ValueNull | ValuePrimitive | ValueSet | ValueShort | ValueString | ValueUuid"""

    TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.Value")
    BIG_DECIMAL = hydra.core.Name("bigDecimal")
    BIG_INTEGER = hydra.core.Name("bigInteger")
    BINARY = hydra.core.Name("binary")
    BOOLEAN = hydra.core.Name("boolean")
    BYTE = hydra.core.Name("byte")
    CHAR = hydra.core.Name("char")
    COMPOSITE = hydra.core.Name("composite")
    DATE_TIME = hydra.core.Name("dateTime")
    DOUBLE = hydra.core.Name("double")
    DURATION = hydra.core.Name("duration")
    FLOAT = hydra.core.Name("float")
    INTEGER = hydra.core.Name("integer")
    LIST = hydra.core.Name("list")
    LONG = hydra.core.Name("long")
    MAP = hydra.core.Name("map")
    NULL = hydra.core.Name("null")
    PRIMITIVE = hydra.core.Name("primitive")
    SET = hydra.core.Name("set")
    SHORT = hydra.core.Name("short")
    STRING = hydra.core.Name("string")
    UUID = hydra.core.Name("uuid")

@dataclass(frozen=True)
class ValuePair:
    first: Value
    second: Value

    TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.ValuePair")
    FIRST = hydra.core.Name("first")
    SECOND = hydra.core.Name("second")

@dataclass(frozen=True)
class Vertex:
    id: Value
    label: Maybe[VertexLabel]
    in_edges: FrozenDict[EdgeLabel, frozenlist[AdjacentEdge]]
    out_edges: FrozenDict[EdgeLabel, frozenlist[AdjacentEdge]]
    properties: FrozenDict[PropertyKey, frozenlist[VertexPropertyValue]]

    TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.Vertex")
    ID = hydra.core.Name("id")
    LABEL = hydra.core.Name("label")
    IN_EDGES = hydra.core.Name("inEdges")
    OUT_EDGES = hydra.core.Name("outEdges")
    PROPERTIES = hydra.core.Name("properties")

class VertexLabel(Node[str]):
    ...

VertexLabel.TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.VertexLabel")

@dataclass(frozen=True)
class VertexPropertyValue:
    id: Maybe[Value]
    value: Value

    TYPE_ = hydra.core.Name("hydra.pg.graphson.syntax.VertexPropertyValue")
    ID = hydra.core.Name("id")
    VALUE = hydra.core.Name("value")
