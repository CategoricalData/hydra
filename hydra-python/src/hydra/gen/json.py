"""A JSON syntax model. See the BNF at https://www.json.org."""

from __future__ import annotations

import hydra.gen.core
from hydra.dsl.python import Node


class ValueArray(Node["frozenlist[Value]"]):
    """A JSON array."""

class ValueBoolean(Node[bool]):
    """A boolean value."""

class ValueNull(Node[None]):
    """JSON's null value."""

class ValueNumber(Node[float]):
    """A numeric value."""

class ValueObject(Node["FrozenDict[str, Value]"]):
    """A JSON object as a set of key/value pairs."""

class ValueString(Node[str]):
    """A string value."""

# A JSON value.
type Value = ValueArray | ValueBoolean | ValueNull | ValueNumber | ValueObject | ValueString

VALUE__NAME = hydra.gen.core.Name("hydra.json.Value")
VALUE__ARRAY__NAME = hydra.gen.core.Name("array")
VALUE__BOOLEAN__NAME = hydra.gen.core.Name("boolean")
VALUE__NULL__NAME = hydra.gen.core.Name("null")
VALUE__NUMBER__NAME = hydra.gen.core.Name("number")
VALUE__OBJECT__NAME = hydra.gen.core.Name("object")
VALUE__STRING__NAME = hydra.gen.core.Name("string")
