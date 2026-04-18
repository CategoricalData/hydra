# Note: this is an automatically generated file. Do not edit.

r"""A basic TypeScript model, constructed on the basis of the typescriptlang.org documentation."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class FunctionType:
    parameters: frozenlist[Parameter]
    range_: Type

    TYPE_ = hydra.core.Name("hydra.typeScript.model.FunctionType")
    PARAMETERS = hydra.core.Name("parameters")
    RANGE = hydra.core.Name("range")

@dataclass(frozen=True)
class Parameter:
    name: str
    type: Type

    TYPE_ = hydra.core.Name("hydra.typeScript.model.Parameter")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")

class PrimitiveType(Enum):
    BIGINT = hydra.core.Name("bigint")
    r"""integers in the arbitrary precision format"""

    BOOLEAN = hydra.core.Name("boolean")
    r"""true and false"""

    NULL = hydra.core.Name("null")
    r"""equivalent to the unit type"""

    NUMBER = hydra.core.Name("number")
    r"""a double-precision IEEE 754 floating point"""

    OBJECT = hydra.core.Name("object")
    r"""similar to records"""

    STRING = hydra.core.Name("string")
    r"""an immutable UTF-16 string"""

    SYMBOL = hydra.core.Name("symbol")
    r"""a unique value usually used as a key"""

    UNDEFINED = hydra.core.Name("undefined")
    r"""also equivalent to the unit type"""

PrimitiveType.TYPE_ = hydra.core.Name("hydra.typeScript.model.PrimitiveType")

class TypeArray(Node["Type"]):
    r"""mutable arrays, also written Array<T>"""

class TypeFunction(Node["FunctionType"]):
    r"""functions"""

class TypeNever:
    r"""the bottom type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeNever)
    def __hash__(self):
        return hash("TypeNever")

class TypeObjectLiteral:
    r"""e.g. { property: Type }"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeObjectLiteral)
    def __hash__(self):
        return hash("TypeObjectLiteral")

class TypePrimitive(Node["PrimitiveType"]):
    r"""A primitive type"""

class TypeTuple(Node["frozenlist[Type]"]):
    r"""tuples, which are fixed-length but mutable"""

class TypeUnknown:
    r"""The top type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeUnknown)
    def __hash__(self):
        return hash("TypeUnknown")

class TypeVoid:
    r"""for functions with no documented return value"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeVoid)
    def __hash__(self):
        return hash("TypeVoid")

class _TypeMeta(type):
    def __getitem__(cls, item):
        return object

class Type(metaclass=_TypeMeta):
    r"""TypeArray | TypeFunction | TypeNever | TypeObjectLiteral | TypePrimitive | TypeTuple | TypeUnknown | TypeVoid"""

    TYPE_ = hydra.core.Name("hydra.typeScript.model.Type")
    ARRAY = hydra.core.Name("array")
    FUNCTION = hydra.core.Name("function")
    NEVER = hydra.core.Name("never")
    OBJECT_LITERAL = hydra.core.Name("objectLiteral")
    PRIMITIVE = hydra.core.Name("primitive")
    TUPLE = hydra.core.Name("tuple")
    UNKNOWN = hydra.core.Name("unknown")
    VOID = hydra.core.Name("void")
