# Note: this is an automatically generated file. Do not edit.

r"""A simple, untyped tabular data model, suitable for CSVs and TSVs."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar
import hydra.core
import hydra.relational

V = TypeVar("V")

@dataclass(frozen=True)
class ColumnType:
    r"""A column type, consisting of a name and a value type."""
    
    name: hydra.relational.ColumnName
    type: hydra.core.Type
    
    TYPE_ = hydra.core.Name("hydra.tabular.ColumnType")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")

class DataRow(Node["frozenlist[Maybe[V]]"], Generic[V]):
    r"""A data row, containing optional-valued cells; one per column."""

DataRow.TYPE_ = hydra.core.Name("hydra.tabular.DataRow")

class HeaderRow(Node[frozenlist[str]]):
    r"""A header row, containing column names (but no types or data)."""

HeaderRow.TYPE_ = hydra.core.Name("hydra.tabular.HeaderRow")

@dataclass(frozen=True)
class Table(Generic[V]):
    r"""A simple table as in a CSV file, having an optional header row and any number of data rows."""
    
    header: Annotated[Maybe[HeaderRow], "The optional header row of the table. If present, the header must have the same number of cells as each data row."]
    data: Annotated[frozenlist[DataRow[V]], "The data rows of the table. Each row must have the same number of cells."]
    
    TYPE_ = hydra.core.Name("hydra.tabular.Table")
    HEADER = hydra.core.Name("header")
    DATA = hydra.core.Name("data")

@dataclass(frozen=True)
class TableType:
    r"""A type definition for a table, including column names and types."""
    
    name: hydra.relational.RelationName
    columns: frozenlist[ColumnType]
    
    TYPE_ = hydra.core.Name("hydra.tabular.TableType")
    NAME = hydra.core.Name("name")
    COLUMNS = hydra.core.Name("columns")
