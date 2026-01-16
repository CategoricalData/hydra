# Note: this is an automatically generated file. Do not edit.

r"""Type classes."""

from __future__ import annotations
from enum import Enum
from typing import TypeAlias
import hydra.core

class TypeClass(Enum):
    r"""Any of a small number of built-in type classes."""
    
    EQUALITY = "equality"
    
    ORDERING = "ordering"

TYPE_CLASS__NAME = hydra.core.Name("hydra.classes.TypeClass")
TYPE_CLASS__EQUALITY__NAME = hydra.core.Name("equality")
TYPE_CLASS__ORDERING__NAME = hydra.core.Name("ordering")
