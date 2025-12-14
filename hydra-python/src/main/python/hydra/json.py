# Note: this is an automatically generated file. Do not edit.

r"""A JSON syntax model. See the BNF at https://www.json.org."""

from __future__ import annotations
from decimal import Decimal
from hydra.dsl.python import FrozenDict, Node, frozenlist
from typing import TypeAlias
import hydra.core

class ValueArray(Node["frozenlist[Value]"]):
    r"""A JSON array."""

class ValueBoolean(Node[bool]):
    r"""A boolean value."""

class ValueNull:
    r"""JSON's null value."""

class ValueNumber(Node[Decimal]):
    r"""A numeric value."""

class ValueObject(Node["FrozenDict[str, Value]"]):
    r"""A JSON object as a set of key/value pairs."""

class ValueString(Node[str]):
    r"""A string value."""

# A JSON value.
Value: TypeAlias = "ValueArray | ValueBoolean | ValueNull | ValueNumber | ValueObject | ValueString"

VALUE__NAME = hydra.core.Name("hydra.json.Value")
VALUE__ARRAY__NAME = hydra.core.Name("array")
VALUE__BOOLEAN__NAME = hydra.core.Name("boolean")
VALUE__NULL__NAME = hydra.core.Name("null")
VALUE__NUMBER__NAME = hydra.core.Name("number")
VALUE__OBJECT__NAME = hydra.core.Name("object")
VALUE__STRING__NAME = hydra.core.Name("string")
