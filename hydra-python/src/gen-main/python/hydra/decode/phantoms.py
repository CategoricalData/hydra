# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.phantoms."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Left, Right
from typing import TypeVar, cast
import hydra.core
import hydra.decode.core
import hydra.extract.helpers
import hydra.lexical
import hydra.lib.eithers
import hydra.phantoms
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def t_term(a: T0, cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.phantoms.TTerm[T1]]:
    def _hoist_hydra_decode_phantoms_t_term_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.phantoms.TTerm[T2]]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.phantoms.TTerm(b)), hydra.decode.core.term(cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.phantoms.TTerm"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_phantoms_t_term_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def t_binding(a: T0, cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.phantoms.TBinding[T1]]:
    def _hoist_hydra_decode_phantoms_t_binding_1(a: T2, cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.phantoms.TBinding[T3]]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("term", (lambda v12, v2: t_term(a, v12, v2)), field_map(), cx), (lambda field_term: Right(hydra.phantoms.TBinding(field_name, field_term))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.phantoms.TBinding"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_phantoms_t_binding_1(a, cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
