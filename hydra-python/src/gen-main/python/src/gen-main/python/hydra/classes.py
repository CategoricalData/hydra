# Note: this is an automatically generated file. Do not edit.

r"""Type classes."""

from __future__ import annotations
from enum import Enum
from typing import TypeAlias
import hydra.core

class TypeClass(Enum):
    r"""Any of a small number of built-in type classes."""
    
    EQUALITY = hydra.core.Name("equality")
    
    ORDERING = hydra.core.Name("ordering")

TypeClass.TYPE_ = hydra.core.Name("hydra.classes.TypeClass")
