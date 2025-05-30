"""A JSON syntax model. See the BNF at https://www.json.org."""

from __future__ import annotations
from hydra.dsl.python import FrozenDict, frozenlist, Node

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
