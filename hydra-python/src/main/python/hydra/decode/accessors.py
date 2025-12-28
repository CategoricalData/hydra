# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.accessors."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, Left, Right, frozenlist
from typing import cast
import hydra.accessors
import hydra.core
import hydra.decode.core
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.util

def accessor_node(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.accessors.AccessorNode]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.accessors.AccessorNode], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def term_accessor(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.accessors.TermAccessor]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.accessors.TermAccessor], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def accessor_path(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.accessors.AccessorPath]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.accessors.AccessorPath], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def accessor_edge(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.accessors.AccessorEdge]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.accessors.AccessorEdge], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def accessor_graph(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.accessors.AccessorGraph]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.accessors.AccessorGraph], Left(hydra.util.DecodingError(err)))), (lambda stripped: "inline match expressions are unsupported"), hydra.lexical.strip_and_dereference_term_either(cx, raw))
