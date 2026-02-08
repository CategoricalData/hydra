# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.phantoms."""

from __future__ import annotations
from collections.abc import Callable
from typing import TypeVar, cast
import hydra.core
import hydra.encode.core
import hydra.phantoms

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def t_term(a: T0, x: hydra.phantoms.TTerm[T1]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.phantoms.TTerm"), hydra.encode.core.term(x.value))))

def t_binding(a: T0, x: hydra.phantoms.TBinding[T1]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.phantoms.TBinding"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)), hydra.core.Field(hydra.core.Name("term"), t_term(a, x.term))))))
