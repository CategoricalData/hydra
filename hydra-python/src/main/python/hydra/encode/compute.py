# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.compute."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict, Maybe, frozenlist
from typing import cast
import hydra.compute
import hydra.core
import hydra.encode.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes

def trace(x: hydra.compute.Trace) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.compute.Trace"), (hydra.core.Field(hydra.core.Name("stack"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2))))), xs))))(x.stack)), hydra.core.Field(hydra.core.Name("messages"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2))))), xs))))(x.messages)), hydra.core.Field(hydra.core.Name("other"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(hydra.encode.core.name, hydra.encode.core.term, m))))(x.other))))))

def flow_state(s: Callable[[T0], hydra.core.Term], v: Callable[[T1], hydra.core.Term], x: hydra.compute.FlowState[T0, T1]) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.compute.FlowState"), (hydra.core.Field(hydra.core.Name("value"), (lambda opt: cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(v, opt))))(x.value)), hydra.core.Field(hydra.core.Name("state"), s(x.state)), hydra.core.Field(hydra.core.Name("trace"), trace(x.trace))))))
