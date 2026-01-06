# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.tabular."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, Left, Maybe, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.extract.helpers
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.tabular
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def data_row(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.DataRow[T0]]:
    def _hoist_hydra_decode_tabular_data_row_1(cx: hydra.graph.Graph, v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.DataRow[T1]]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: cast(hydra.tabular.DataRow[T1], hydra.tabular.DataRow(b))), hydra.extract.helpers.decode_list((lambda v1, v2: hydra.extract.helpers.decode_maybe(v, v1, v2)), cx, wrapped_term.body))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.tabular.DataRow[T1]], Left(hydra.util.DecodingError("expected wrapped type hydra.tabular.DataRow")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.tabular.DataRow[T0]], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_tabular_data_row_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def header_row(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.HeaderRow]:
    def _hoist_hydra_decode_tabular_header_row_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.LiteralString(value=s):
                return cast(Either[hydra.util.DecodingError, str], Right(s))
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
    def _hoist_hydra_decode_tabular_header_row_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_tabular_header_row_1(v)
            
            case _:
                return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
    def _hoist_hydra_decode_tabular_header_row_3(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.HeaderRow]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.tabular.HeaderRow(b)), hydra.extract.helpers.decode_list((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_hydra_decode_tabular_header_row_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), cx, wrapped_term.body))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.tabular.HeaderRow], Left(hydra.util.DecodingError("expected wrapped type hydra.tabular.HeaderRow")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.tabular.HeaderRow], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_tabular_header_row_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def table(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.Table[T0]]:
    def _hoist_hydra_decode_tabular_table_1(cx: hydra.graph.Graph, v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.Table[T1]]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("header", (lambda v1, v2: hydra.extract.helpers.decode_maybe(header_row, v1, v2)), field_map, cx), (lambda field_header: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("data", (lambda v1, v2: hydra.extract.helpers.decode_list((lambda v1, v2: data_row(v, v1, v2)), v1, v2)), field_map, cx), (lambda field_data: cast(Either[hydra.util.DecodingError, hydra.tabular.Table[T1]], Right(cast(hydra.tabular.Table[T1], hydra.tabular.Table(field_header, field_data))))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.tabular.Table[T1]], Left(hydra.util.DecodingError("expected record of type hydra.tabular.Table")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.tabular.Table[T0]], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_tabular_table_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
