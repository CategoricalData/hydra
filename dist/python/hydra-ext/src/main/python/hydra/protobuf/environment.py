# Note: this is an automatically generated file. Do not edit.

r"""Type definitions for the Protobuf code generation environment."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import Node
from typing import TypeAlias, cast
import hydra.core

class StructuralTypeRefEither(Node["tuple[hydra.core.Type, hydra.core.Type]"]):
    r"""An Either type with left and right component types"""

class StructuralTypeRefPair(Node["tuple[hydra.core.Type, hydra.core.Type]"]):
    r"""A Pair type with first and second component types"""

class _StructuralTypeRefMeta(type):
    def __getitem__(cls, item):
        return object

# A reference to a structural type (Either or Pair) with its component types.
class StructuralTypeRef(metaclass=_StructuralTypeRefMeta):
    r"""StructuralTypeRefEither | StructuralTypeRefPair"""

    TYPE_ = hydra.core.Name("hydra.protobuf.environment.StructuralTypeRef")
    EITHER = hydra.core.Name("either")
    PAIR = hydra.core.Name("pair")
