# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.tabular."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, Left, Maybe, Right, frozenlist
from typing import cast
import hydra.core
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.tabular
import hydra.util

def data_row(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.DataRow[T0]]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.tabular.DataRow[T0]], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def header_row(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.HeaderRow]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.tabular.HeaderRow], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def table(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.Table[T0]]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.tabular.Table[T0]], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))
