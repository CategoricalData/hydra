# Note: this is an automatically generated file. Do not edit.

r"""Error types specific to the Hydra kernel."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Node
from typing import Annotated, TypeAlias, cast
import hydra.core

class DecodingError(Node[str]):
    r"""An error that occurred during decoding of a term."""

DecodingError.TYPE_ = hydra.core.Name("hydra.error.DecodingError")

class ErrorDecoding(Node["DecodingError"]):
    r"""An error that occurred during decoding of a term"""

class ErrorOther(Node["OtherError"]):
    r"""Any other error"""

class ErrorUnification(Node["UnificationError"]):
    r"""A type unification error"""

class _ErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error of any kind, with kernel errors particularly differentiated.
class Error(metaclass=_ErrorMeta):
    r"""ErrorDecoding | ErrorOther | ErrorUnification"""
    
    TYPE_ = hydra.core.Name("hydra.error.Error")
    DECODING = hydra.core.Name("decoding")
    OTHER = hydra.core.Name("other")
    UNIFICATION = hydra.core.Name("unification")

class OtherError(Node[str]):
    r"""Any other error."""

OtherError.TYPE_ = hydra.core.Name("hydra.error.OtherError")

@dataclass(frozen=True)
class UnificationError:
    r"""An error that occurred during type unification."""
    
    left_type: Annotated[hydra.core.Type, "The left-hand type in the unification"]
    right_type: Annotated[hydra.core.Type, "The right-hand type in the unification"]
    message: Annotated[str, "A human-readable error message"]
    
    TYPE_ = hydra.core.Name("hydra.error.UnificationError")
    LEFT_TYPE = hydra.core.Name("leftType")
    RIGHT_TYPE = hydra.core.Name("rightType")
    MESSAGE = hydra.core.Name("message")
