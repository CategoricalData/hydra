"""A JSON syntax model. See the BNF at https://www.json.org."""

from __future__ import annotations
from decimal import Decimal
from hydra.dsl.python import FrozenDict, frozenlist, Node
import hydra.core

class ValueArray(Node["frozenlist[Value]"]):
    """A JSON array."""

class ValueBoolean(Node[bool]):
    """A boolean value."""

class ValueNull(Node[None]):
    """JSON's null value."""

class ValueNumber(Node[Decimal]):
    """A numeric value."""

class ValueObject(Node["FrozenDict[str, Value]"]):
    """A JSON object as a set of key/value pairs."""

class ValueString(Node[str]):
    """A string value."""

# A JSON value.
type Value = ValueArray | ValueBoolean | ValueNull | ValueNumber | ValueObject | ValueString

VALUE__NAME = hydra.core.Name("hydra.json.Value")
VALUE__ARRAY__NAME = hydra.core.Name("array")
VALUE__BOOLEAN__NAME = hydra.core.Name("boolean")
VALUE__NULL__NAME = hydra.core.Name("null")
VALUE__NUMBER__NAME = hydra.core.Name("number")
VALUE__OBJECT__NAME = hydra.core.Name("object")
VALUE__STRING__NAME = hydra.core.Name("string")
