# Note: this is an automatically generated file. Do not edit.

r"""A syntax model for TinkerPop's GraphSON format. This model is designed to be as inclusive as possible, supporting GraphSON 4.0 as well as earlier versions. See https://github.com/apache/tinkerpop/blob/master/docs/src/dev/io/graphson.asciidoc."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import FrozenDict, Maybe, Node, frozenlist
from typing import TypeAlias
import hydra.core

class BigDecimalValue(Node[str]): ...

BIG_DECIMAL_VALUE__NAME = hydra.core.Name("hydra.pg.graphson.syntax.BigDecimalValue")

@dataclass(frozen=True)
class CompositeTypedValue:
    type: TypeName
    fields: Map

COMPOSITE_TYPED_VALUE__NAME = hydra.core.Name("hydra.pg.graphson.syntax.CompositeTypedValue")
COMPOSITE_TYPED_VALUE__TYPE__NAME = hydra.core.Name("type")
COMPOSITE_TYPED_VALUE__FIELDS__NAME = hydra.core.Name("fields")

class DateTime(Node[str]): ...

DATE_TIME__NAME = hydra.core.Name("hydra.pg.graphson.syntax.DateTime")

class DoubleValueFinite(Node[float]): ...

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
    
    pass

DOUBLE_VALUE__NAME = hydra.core.Name("hydra.pg.graphson.syntax.DoubleValue")
DOUBLE_VALUE__FINITE__NAME = hydra.core.Name("finite")
DOUBLE_VALUE__INFINITY__NAME = hydra.core.Name("infinity")
DOUBLE_VALUE__NEGATIVE_INFINITY__NAME = hydra.core.Name("negativeInfinity")
DOUBLE_VALUE__NOT_A_NUMBER__NAME = hydra.core.Name("notANumber")

class Duration(Node[str]): ...

DURATION__NAME = hydra.core.Name("hydra.pg.graphson.syntax.Duration")

class EdgeLabel(Node[str]): ...

EDGE_LABEL__NAME = hydra.core.Name("hydra.pg.graphson.syntax.EdgeLabel")

class FloatValueFinite(Node[float]): ...

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
    
    pass

FLOAT_VALUE__NAME = hydra.core.Name("hydra.pg.graphson.syntax.FloatValue")
FLOAT_VALUE__FINITE__NAME = hydra.core.Name("finite")
FLOAT_VALUE__INFINITY__NAME = hydra.core.Name("infinity")
FLOAT_VALUE__NEGATIVE_INFINITY__NAME = hydra.core.Name("negativeInfinity")
FLOAT_VALUE__NOT_A_NUMBER__NAME = hydra.core.Name("notANumber")

class Map(Node["frozenlist[ValuePair]"]): ...

MAP__NAME = hydra.core.Name("hydra.pg.graphson.syntax.Map")

@dataclass(frozen=True)
class AdjacentEdge:
    id: Value
    vertex_id: Value
    properties: FrozenDict[PropertyKey, Value]

ADJACENT_EDGE__NAME = hydra.core.Name("hydra.pg.graphson.syntax.AdjacentEdge")
ADJACENT_EDGE__ID__NAME = hydra.core.Name("id")
ADJACENT_EDGE__VERTEX_ID__NAME = hydra.core.Name("vertexId")
ADJACENT_EDGE__PROPERTIES__NAME = hydra.core.Name("properties")

@dataclass(frozen=True)
class PrimitiveTypedValue:
    type: TypeName
    value: str

PRIMITIVE_TYPED_VALUE__NAME = hydra.core.Name("hydra.pg.graphson.syntax.PrimitiveTypedValue")
PRIMITIVE_TYPED_VALUE__TYPE__NAME = hydra.core.Name("type")
PRIMITIVE_TYPED_VALUE__VALUE__NAME = hydra.core.Name("value")

class PropertyKey(Node[str]): ...

PROPERTY_KEY__NAME = hydra.core.Name("hydra.pg.graphson.syntax.PropertyKey")

class TypeName(Node[str]): ...

TYPE_NAME__NAME = hydra.core.Name("hydra.pg.graphson.syntax.TypeName")

class Uuid(Node[str]): ...

UUID__NAME = hydra.core.Name("hydra.pg.graphson.syntax.Uuid")

class ValueBigDecimal(Node["BigDecimalValue"]): ...

class ValueBigInteger(Node[int]): ...

class ValueBinary(Node[str]): ...

class ValueBoolean(Node[bool]): ...

class ValueByte(Node[int]): ...

class ValueChar(Node[int]): ...

class ValueComposite(Node["CompositeTypedValue"]): ...

class ValueDateTime(Node["DateTime"]): ...

class ValueDouble(Node["DoubleValue"]): ...

class ValueDuration(Node["Duration"]): ...

class ValueFloat(Node["FloatValue"]): ...

class ValueInteger(Node[int]): ...

class ValueList(Node["frozenlist[Value]"]): ...

class ValueLong(Node[int]): ...

class ValueMap(Node["Map"]): ...

class ValueNull:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ValueNull)
    def __hash__(self):
        return hash("ValueNull")

class ValuePrimitive(Node["PrimitiveTypedValue"]): ...

class ValueSet(Node["frozenlist[Value]"]): ...

class ValueShort(Node[int]): ...

class ValueString(Node[str]): ...

class ValueUuid(Node["Uuid"]): ...

class _ValueMeta(type):
    def __getitem__(cls, item):
        return object

class Value(metaclass=_ValueMeta):
    r"""ValueBigDecimal | ValueBigInteger | ValueBinary | ValueBoolean | ValueByte | ValueChar | ValueComposite | ValueDateTime | ValueDouble | ValueDuration | ValueFloat | ValueInteger | ValueList | ValueLong | ValueMap | ValueNull | ValuePrimitive | ValueSet | ValueShort | ValueString | ValueUuid"""
    
    pass

VALUE__NAME = hydra.core.Name("hydra.pg.graphson.syntax.Value")
VALUE__BIG_DECIMAL__NAME = hydra.core.Name("bigDecimal")
VALUE__BIG_INTEGER__NAME = hydra.core.Name("bigInteger")
VALUE__BINARY__NAME = hydra.core.Name("binary")
VALUE__BOOLEAN__NAME = hydra.core.Name("boolean")
VALUE__BYTE__NAME = hydra.core.Name("byte")
VALUE__CHAR__NAME = hydra.core.Name("char")
VALUE__COMPOSITE__NAME = hydra.core.Name("composite")
VALUE__DATE_TIME__NAME = hydra.core.Name("dateTime")
VALUE__DOUBLE__NAME = hydra.core.Name("double")
VALUE__DURATION__NAME = hydra.core.Name("duration")
VALUE__FLOAT__NAME = hydra.core.Name("float")
VALUE__INTEGER__NAME = hydra.core.Name("integer")
VALUE__LIST__NAME = hydra.core.Name("list")
VALUE__LONG__NAME = hydra.core.Name("long")
VALUE__MAP__NAME = hydra.core.Name("map")
VALUE__NULL__NAME = hydra.core.Name("null")
VALUE__PRIMITIVE__NAME = hydra.core.Name("primitive")
VALUE__SET__NAME = hydra.core.Name("set")
VALUE__SHORT__NAME = hydra.core.Name("short")
VALUE__STRING__NAME = hydra.core.Name("string")
VALUE__UUID__NAME = hydra.core.Name("uuid")

@dataclass(frozen=True)
class ValuePair:
    first: Value
    second: Value

VALUE_PAIR__NAME = hydra.core.Name("hydra.pg.graphson.syntax.ValuePair")
VALUE_PAIR__FIRST__NAME = hydra.core.Name("first")
VALUE_PAIR__SECOND__NAME = hydra.core.Name("second")

@dataclass(frozen=True)
class Vertex:
    id: Value
    label: Maybe[VertexLabel]
    in_edges: FrozenDict[EdgeLabel, frozenlist[AdjacentEdge]]
    out_edges: FrozenDict[EdgeLabel, frozenlist[AdjacentEdge]]
    properties: FrozenDict[PropertyKey, frozenlist[VertexPropertyValue]]

VERTEX__NAME = hydra.core.Name("hydra.pg.graphson.syntax.Vertex")
VERTEX__ID__NAME = hydra.core.Name("id")
VERTEX__LABEL__NAME = hydra.core.Name("label")
VERTEX__IN_EDGES__NAME = hydra.core.Name("inEdges")
VERTEX__OUT_EDGES__NAME = hydra.core.Name("outEdges")
VERTEX__PROPERTIES__NAME = hydra.core.Name("properties")

class VertexLabel(Node[str]): ...

VERTEX_LABEL__NAME = hydra.core.Name("hydra.pg.graphson.syntax.VertexLabel")

@dataclass(frozen=True)
class VertexPropertyValue:
    id: Maybe[Value]
    value: Value

VERTEX_PROPERTY_VALUE__NAME = hydra.core.Name("hydra.pg.graphson.syntax.VertexPropertyValue")
VERTEX_PROPERTY_VALUE__ID__NAME = hydra.core.Name("id")
VERTEX_PROPERTY_VALUE__VALUE__NAME = hydra.core.Name("value")
