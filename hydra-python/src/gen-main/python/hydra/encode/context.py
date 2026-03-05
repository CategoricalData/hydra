# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.context."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from typing import TypeVar, cast
import hydra.context
import hydra.core
import hydra.encode.core
import hydra.lib.lists
import hydra.lib.maps

T0 = TypeVar("T0")

def context(x: hydra.context.Context) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.context.Context"), (hydra.core.Field(hydra.core.Name("trace"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2))))), x.trace)))), hydra.core.Field(hydra.core.Name("messages"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2))))), x.messages)))), hydra.core.Field(hydra.core.Name("other"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: hydra.encode.core.name(x1)), (lambda x1: hydra.encode.core.term(x1)), x.other))))))))

def in_context(e: Callable[[T0], hydra.core.Term], x: hydra.context.InContext[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.context.InContext"), (hydra.core.Field(hydra.core.Name("object"), e(x.object)), hydra.core.Field(hydra.core.Name("context"), context(x.context))))))
