"""Phantom types for use with Hydra DSLs."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import Node
from typing import Generic, TypeVar
import hydra.core

A = TypeVar("A")

class TCase(Node["hydra.core.Name"], Generic[A]):
    """An association of a field name (as in a case statement) with a phantom type."""

T_CASE__NAME = hydra.core.Name("hydra.phantoms.TCase")

@dataclass
class TElement(Generic[A]):
    """An association with a named term (element) with a phantom type."""
    
    name: hydra.core.Name
    term: TTerm[A]

T_ELEMENT__NAME = hydra.core.Name("hydra.phantoms.TElement")
T_ELEMENT__NAME__NAME = hydra.core.Name("name")
T_ELEMENT__TERM__NAME = hydra.core.Name("term")

class TField(Node["hydra.core.Field"], Generic[A]):
    """An association with a term-level field with a phantom type."""

T_FIELD__NAME = hydra.core.Name("hydra.phantoms.TField")

class TTerm(Node["hydra.core.Term"], Generic[A]):
    """An association of a term with a phantom type."""

T_TERM__NAME = hydra.core.Name("hydra.phantoms.TTerm")
