# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.compute."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Right, frozenlist
from typing import TypeVar, cast
import hydra.compute
import hydra.core
import hydra.decode.core
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def trace(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.compute.Trace]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.compute.Trace], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def flow_state(s: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.compute.FlowState[T0, T1]]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.compute.FlowState[T0, T1]], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))
