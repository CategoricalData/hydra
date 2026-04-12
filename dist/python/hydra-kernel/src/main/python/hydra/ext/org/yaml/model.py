# Note: this is an automatically generated file. Do not edit.

r"""A basic YAML representation model. Based on:
  https://yaml.org/spec/1.2/spec.html
The Serialization and Presentation properties of YAML,
including directives, comments, anchors, style, formatting, and aliases, are not supported by this model.
In addition, tags are omitted from this model, and non-standard scalars are unsupported."""

from __future__ import annotations
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

class NodeMapping(Node["FrozenDict[Node_, Node_]"]):
    r"""A mapping from nodes to nodes"""

class NodeScalar(Node["Scalar"]):
    r"""A scalar value"""

class NodeSequence(Node["frozenlist[Node_]"]):
    r"""A sequence of nodes"""

class _Node_Meta(type):
    def __getitem__(cls, item):
        return object

# A YAML node (value).
class Node_(metaclass=_Node_Meta):
    r"""NodeMapping | NodeScalar | NodeSequence"""

    TYPE_ = hydra.core.Name("hydra.ext.org.yaml.model.Node")
    MAPPING = hydra.core.Name("mapping")
    SCALAR = hydra.core.Name("scalar")
    SEQUENCE = hydra.core.Name("sequence")

class ScalarBool(Node[bool]):
    r"""Represents a true/false value"""

class ScalarFloat(Node[Decimal]):
    r"""Represents an approximation to real numbers"""

class ScalarInt(Node[int]):
    r"""Represents arbitrary sized finite mathematical integers"""

class ScalarNull:
    r"""Represents the lack of a value"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ScalarNull)
    def __hash__(self):
        return hash("ScalarNull")

class ScalarStr(Node[str]):
    r"""A string value"""

class _ScalarMeta(type):
    def __getitem__(cls, item):
        return object

# A union of scalars supported in the YAML failsafe and JSON schemas. Other scalars are not supported here.
class Scalar(metaclass=_ScalarMeta):
    r"""ScalarBool | ScalarFloat | ScalarInt | ScalarNull | ScalarStr"""

    TYPE_ = hydra.core.Name("hydra.ext.org.yaml.model.Scalar")
    BOOL = hydra.core.Name("bool")
    FLOAT = hydra.core.Name("float")
    INT = hydra.core.Name("int")
    NULL = hydra.core.Name("null")
    STR = hydra.core.Name("str")
