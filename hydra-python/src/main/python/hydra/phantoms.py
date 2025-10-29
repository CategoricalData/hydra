# Note: this is an automatically generated file. Do not edit.

r"""Phantom types for use with Hydra DSLs."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import Node
from typing import Generic, TypeVar
import hydra.core

A = TypeVar("A")

@dataclass
class TBinding(Generic[A]):
    r"""An association of a named term (element) with a phantom type."""
    
    name: hydra.core.Name
    term: TTerm[A]

T_BINDING__NAME = hydra.core.Name("hydra.phantoms.TBinding")
T_BINDING__NAME__NAME = hydra.core.Name("name")
T_BINDING__TERM__NAME = hydra.core.Name("term")

class TTerm(Node["hydra.core.Term"], Generic[A]):
    r"""An association of a term with a phantom type."""

T_TERM__NAME = hydra.core.Name("hydra.phantoms.TTerm")
