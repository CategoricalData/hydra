# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.phantoms."""

from __future__ import annotations
from hydra.dsl.python import Either, Left, Right
from typing import TypeVar, cast
import hydra.core
import hydra.decode.core
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.phantoms
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def t_term(a: T0, cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.phantoms.TTerm[T1]]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.phantoms.TTerm[T1]], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def t_binding(a: T0, cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.phantoms.TBinding[T1]]:
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.phantoms.TBinding[T1]], Left(hydra.util.DecodingError(err)))), (lambda stripped: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lexical.strip_and_dereference_term_either(cx, raw))
