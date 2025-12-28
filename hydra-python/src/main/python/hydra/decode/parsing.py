# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.parsing."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, Left, Right
from typing import cast
import hydra.core
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.parsing
import hydra.util

def parse_error(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.parsing.ParseError]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.parsing.ParseError], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def parse_success(a: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.parsing.ParseSuccess[T0]]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.parsing.ParseSuccess[T0]], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def parse_result(a: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.parsing.ParseResult[T0]]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.parsing.ParseResult[T0]], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))
