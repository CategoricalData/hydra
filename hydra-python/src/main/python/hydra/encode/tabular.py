# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.tabular."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Maybe, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.encode.core
import hydra.encode.relational
import hydra.lib.lists
import hydra.lib.maybes
import hydra.tabular

T0 = TypeVar("T0")

def column_type(x: hydra.tabular.ColumnType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.tabular.ColumnType"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.relational.column_name(x.name)), hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type(x.type))))))

def data_row(v: Callable[[T0], hydra.core.Term], x: hydra.tabular.DataRow[T0]) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.tabular.DataRow"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda opt: cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(v, opt)))), xs))))(x.value))))

def header_row(x: hydra.tabular.HeaderRow) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.tabular.HeaderRow"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2))))), xs))))(x.value))))

def table(v: Callable[[T0], hydra.core.Term], x: hydra.tabular.Table[T0]) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.tabular.Table"), (hydra.core.Field(hydra.core.Name("header"), (lambda opt: cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(header_row, opt))))(x.header)), hydra.core.Field(hydra.core.Name("data"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda v1: data_row(v, v1)), xs))))(x.data))))))

def table_type(x: hydra.tabular.TableType) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.tabular.TableType"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.relational.relation_name(x.name)), hydra.core.Field(hydra.core.Name("columns"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(column_type, xs))))(x.columns))))))
