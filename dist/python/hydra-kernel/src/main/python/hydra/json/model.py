# Note: this is an automatically generated file. Do not edit.

r"""A JSON syntax model. See the BNF at https://www.json.org."""

from __future__ import annotations
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

class ValueArray(Node["frozenlist[Value]"]):
    r"""A JSON array"""

class ValueBoolean(Node[bool]):
    r"""A boolean value"""

class ValueNull:
    r"""JSON's null value"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ValueNull)
    def __hash__(self):
        return hash("ValueNull")

class ValueNumber(Node[Decimal]):
    r"""A numeric value"""

class ValueObject(Node["FrozenDict[str, Value]"]):
    r"""A JSON object as a set of key/value pairs"""

class ValueString(Node[str]):
    r"""A string value"""

class _ValueMeta(type):
    def __getitem__(cls, item):
        return object

# A JSON value.
class Value(metaclass=_ValueMeta):
    r"""ValueArray | ValueBoolean | ValueNull | ValueNumber | ValueObject | ValueString"""

    TYPE_ = hydra.core.Name("hydra.json.model.Value")
    ARRAY = hydra.core.Name("array")
    BOOLEAN = hydra.core.Name("boolean")
    NULL = hydra.core.Name("null")
    NUMBER = hydra.core.Name("number")
    OBJECT = hydra.core.Name("object")
    STRING = hydra.core.Name("string")
