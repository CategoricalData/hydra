"""A simple, untyped tabular data model, suitable for CSVs and TSVs."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import frozenlist, Node
from typing import Annotated, Generic, TypeVar
import hydra.core

V = TypeVar("V")

class DataRow(Node["frozenlist[V | None]"], Generic[V]):
    """A data row, containing optional-valued cells; one per column."""

DATA_ROW__NAME = hydra.core.Name("hydra.tabular.DataRow")

class HeaderRow(Node[frozenlist[str]]):
    """A header row, containing column names (but no types or data)."""

HEADER_ROW__NAME = hydra.core.Name("hydra.tabular.HeaderRow")

@dataclass
class Table(Generic[V]):
    """A simple table as in a CSV file, having an optional header row and any number of data rows."""
    
    header: Annotated[HeaderRow | None, "The optional header row of the table. If present, the header must have the same number of cells as each data row."]
    data: Annotated[frozenlist[DataRow[V]], "The data rows of the table. Each row must have the same number of cells."]

TABLE__NAME = hydra.core.Name("hydra.tabular.Table")
TABLE__HEADER__NAME = hydra.core.Name("header")
TABLE__DATA__NAME = hydra.core.Name("data")
