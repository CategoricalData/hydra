# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.tabular."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Maybe, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.phantoms

V = TypeVar("V")

def column_type(name: hydra.phantoms.TTerm[hydra.relational.ColumnName], type: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.tabular.ColumnType"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("type"), type.value))))))

def column_type_name(x: hydra.phantoms.TTerm[hydra.tabular.ColumnType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.tabular.ColumnType"), hydra.core.Name("name")))))))), x.value))))

def column_type_type(x: hydra.phantoms.TTerm[hydra.tabular.ColumnType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.tabular.ColumnType"), hydra.core.Name("type")))))))), x.value))))

def column_type_with_name(original: hydra.phantoms.TTerm[hydra.tabular.ColumnType], new_val: hydra.phantoms.TTerm[hydra.relational.ColumnName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.tabular.ColumnType"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.tabular.ColumnType"), hydra.core.Name("type")))))))), original.value)))))))))

def column_type_with_type(original: hydra.phantoms.TTerm[hydra.tabular.ColumnType], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.tabular.ColumnType"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.tabular.ColumnType"), hydra.core.Name("name")))))))), original.value)))), hydra.core.Field(hydra.core.Name("type"), new_val.value))))))

def data_row(x: hydra.phantoms.TTerm[frozenlist[Maybe[V]]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.tabular.DataRow"), x.value))))

def header_row(x: hydra.phantoms.TTerm[frozenlist[str]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.tabular.HeaderRow"), x.value))))

def table(header: hydra.phantoms.TTerm[Maybe[hydra.tabular.HeaderRow]], data: hydra.phantoms.TTerm[frozenlist[hydra.tabular.DataRow[V]]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.tabular.Table"), (hydra.core.Field(hydra.core.Name("header"), header.value), hydra.core.Field(hydra.core.Name("data"), data.value))))))

def table_data(x: hydra.phantoms.TTerm[hydra.tabular.Table[V]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.tabular.Table"), hydra.core.Name("data")))))))), x.value))))

def table_header(x: hydra.phantoms.TTerm[hydra.tabular.Table[V]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.tabular.Table"), hydra.core.Name("header")))))))), x.value))))

def table_type(name: hydra.phantoms.TTerm[hydra.relational.RelationName], columns: hydra.phantoms.TTerm[frozenlist[hydra.tabular.ColumnType]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.tabular.TableType"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("columns"), columns.value))))))

def table_type_columns(x: hydra.phantoms.TTerm[hydra.tabular.TableType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.tabular.TableType"), hydra.core.Name("columns")))))))), x.value))))

def table_type_name(x: hydra.phantoms.TTerm[hydra.tabular.TableType]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.tabular.TableType"), hydra.core.Name("name")))))))), x.value))))

def table_type_with_columns(original: hydra.phantoms.TTerm[hydra.tabular.TableType], new_val: hydra.phantoms.TTerm[frozenlist[hydra.tabular.ColumnType]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.tabular.TableType"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.tabular.TableType"), hydra.core.Name("name")))))))), original.value)))), hydra.core.Field(hydra.core.Name("columns"), new_val.value))))))

def table_type_with_name(original: hydra.phantoms.TTerm[hydra.tabular.TableType], new_val: hydra.phantoms.TTerm[hydra.relational.RelationName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.tabular.TableType"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("columns"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.tabular.TableType"), hydra.core.Name("columns")))))))), original.value)))))))))

def table_with_data(original: hydra.phantoms.TTerm[hydra.tabular.Table[V]], new_val: hydra.phantoms.TTerm[frozenlist[hydra.tabular.DataRow[V]]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.tabular.Table"), (hydra.core.Field(hydra.core.Name("header"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.tabular.Table"), hydra.core.Name("header")))))))), original.value)))), hydra.core.Field(hydra.core.Name("data"), new_val.value))))))

def table_with_header(original: hydra.phantoms.TTerm[hydra.tabular.Table[V]], new_val: hydra.phantoms.TTerm[Maybe[hydra.tabular.HeaderRow]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.tabular.Table"), (hydra.core.Field(hydra.core.Name("header"), new_val.value), hydra.core.Field(hydra.core.Name("data"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.tabular.Table"), hydra.core.Name("data")))))))), original.value)))))))))

def un_data_row(x: hydra.phantoms.TTerm[hydra.tabular.DataRow[V]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.tabular.DataRow"))))))), x.value))))

def un_header_row(x: hydra.phantoms.TTerm[hydra.tabular.HeaderRow]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.tabular.HeaderRow"))))))), x.value))))
