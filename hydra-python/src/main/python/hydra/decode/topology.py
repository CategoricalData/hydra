# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.topology."""

from __future__ import annotations
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import cast
import hydra.core
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.topology
import hydra.util

def vertex(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, int], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def graph(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, FrozenDict[int, frozenlist[int]]]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, FrozenDict[int, frozenlist[int]]], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def tarjan_state(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.topology.TarjanState]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.topology.TarjanState], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))
