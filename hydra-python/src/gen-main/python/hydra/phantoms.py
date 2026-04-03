# Note: this is an automatically generated file. Do not edit.

r"""Phantom types for use with Hydra DSLs."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Node
from typing import Annotated, Generic, TypeAlias, TypeVar, cast
import hydra.core

A = TypeVar("A")

@dataclass(frozen=True)
class TBinding(Generic[A]):
    r"""An association of a named term (element) with a phantom type."""

    name: Annotated[hydra.core.Name, "The name of the term"]
    term: Annotated[TTerm[A], "The term with its phantom type"]

    TYPE_ = hydra.core.Name("hydra.phantoms.TBinding")
    NAME = hydra.core.Name("name")
    TERM = hydra.core.Name("term")

class TTerm(Node["hydra.core.Term"], Generic[A]):
    r"""An association of a term with a phantom type."""

TTerm.TYPE_ = hydra.core.Name("hydra.phantoms.TTerm")

@dataclass(frozen=True)
class TTermDefinition(Generic[A]):
    r"""An association of a term definition with a phantom type."""

    name: Annotated[hydra.core.Name, "The name of the term"]
    term: Annotated[TTerm[A], "The term with its phantom type"]

    TYPE_ = hydra.core.Name("hydra.phantoms.TTermDefinition")
    NAME = hydra.core.Name("name")
    TERM = hydra.core.Name("term")
