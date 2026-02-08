# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.tabular."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Left, Maybe, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.decode.core
import hydra.decode.relational
import hydra.extract.helpers
import hydra.lexical
import hydra.lib.eithers
import hydra.tabular
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def column_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.ColumnType]:
    def _hoist_hydra_decode_tabular_column_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.ColumnType]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", (lambda x1, x2: hydra.decode.relational.column_name(x1, x2)), field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_type: Right(hydra.tabular.ColumnType(field_name, field_type))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.tabular.ColumnType"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_tabular_column_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def data_row(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.DataRow[T0]]:
    def _hoist_hydra_decode_tabular_data_row_1(cx: hydra.graph.Graph, v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.DataRow[T1]]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.tabular.DataRow(b)), hydra.extract.helpers.decode_list((lambda v12, v2: hydra.extract.helpers.decode_maybe(v, v12, v2)), cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.tabular.DataRow"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_tabular_data_row_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def header_row(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.HeaderRow]:
    def _hoist_hydra_decode_tabular_header_row_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)
            
            case _:
                return Left(hydra.util.DecodingError("expected string literal"))
    def _hoist_hydra_decode_tabular_header_row_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_tabular_header_row_1(v)
            
            case _:
                return Left(hydra.util.DecodingError("expected literal"))
    def _hoist_hydra_decode_tabular_header_row_3(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.HeaderRow]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.tabular.HeaderRow(b)), hydra.extract.helpers.decode_list((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_hydra_decode_tabular_header_row_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.tabular.HeaderRow"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_tabular_header_row_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def table(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.Table[T0]]:
    def _hoist_hydra_decode_tabular_table_1(cx: hydra.graph.Graph, v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.Table[T1]]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("header", (lambda v12, v2: hydra.extract.helpers.decode_maybe((lambda x1, x2: header_row(x1, x2)), v12, v2)), field_map(), cx), (lambda field_header: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("data", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda v13, v22: data_row(v, v13, v22)), v12, v2)), field_map(), cx), (lambda field_data: Right(hydra.tabular.Table(field_header, field_data))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.tabular.Table"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_tabular_table_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def table_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.TableType]:
    def _hoist_hydra_decode_tabular_table_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.tabular.TableType]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", (lambda x1, x2: hydra.decode.relational.relation_name(x1, x2)), field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("columns", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda x1, x2: column_type(x1, x2)), v12, v2)), field_map(), cx), (lambda field_columns: Right(hydra.tabular.TableType(field_name, field_columns))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.tabular.TableType"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_tabular_table_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
