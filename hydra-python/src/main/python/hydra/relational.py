# Note: this is an automatically generated file. Do not edit.

r"""An interpretation of Codd's Relational Model, as described in 'A Relational Model of Data for Large Shared Data Banks' (1970). Types ('domains') and values are parameterized so as to allow for application-specific implementations. No special support is provided for 'nonsimple' domains; i.e. relations are assumed to be normalized."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import FrozenDict, Node, frozenlist
from typing import Annotated, Generic, TypeVar
import hydra.core

T = TypeVar("T")
V = TypeVar("V")

class ColumnName(Node[str]):
    r"""A name for a domain which serves to identify the role played by that domain in the given relation; a 'role name' in Codd."""

COLUMN_NAME__NAME = hydra.core.Name("hydra.relational.ColumnName")

@dataclass(frozen=True)
class ColumnSchema(Generic[T]):
    r"""An abstract specification of the domain represented by a column in a relation; a role."""
    
    name: Annotated[ColumnName, "A unique name for the column"]
    domain: Annotated[T, "The domain (type) of the column"]

COLUMN_SCHEMA__NAME = hydra.core.Name("hydra.relational.ColumnSchema")
COLUMN_SCHEMA__NAME__NAME = hydra.core.Name("name")
COLUMN_SCHEMA__DOMAIN__NAME = hydra.core.Name("domain")

@dataclass(frozen=True)
class ForeignKey:
    r"""A mapping from certain columns of a source relation to primary key columns of a target relation."""
    
    foreign_relation: Annotated[RelationName, "The name of the target relation"]
    keys: Annotated[FrozenDict[ColumnName, ColumnName], "The mapping of source column names to target column names. The target column names must together make up the primary key of the target relation."]

FOREIGN_KEY__NAME = hydra.core.Name("hydra.relational.ForeignKey")
FOREIGN_KEY__FOREIGN_RELATION__NAME = hydra.core.Name("foreignRelation")
FOREIGN_KEY__KEYS__NAME = hydra.core.Name("keys")

class PrimaryKey(Node["frozenlist[ColumnName]"]):
    r"""A primary key of a relation, specified either as a single column, or as a list of columns."""

PRIMARY_KEY__NAME = hydra.core.Name("hydra.relational.PrimaryKey")

class Relation(Node["frozenlist[Row[V]]"], Generic[V]):
    r"""A set of distinct n-tuples; a table."""

RELATION__NAME = hydra.core.Name("hydra.relational.Relation")

class RelationName(Node[str]):
    r"""A unique relation (table) name."""

RELATION_NAME__NAME = hydra.core.Name("hydra.relational.RelationName")

@dataclass(frozen=True)
class RelationSchema(Generic[T]):
    r"""An abstract relation; the name and columns of a relation without its actual data."""
    
    name: Annotated[RelationName, "A unique name for the relation"]
    columns: Annotated[frozenlist[ColumnSchema[T]], "A list of column specifications"]
    primary_keys: Annotated[frozenlist[PrimaryKey], "Any number of primary keys for the relation, each of which must be valid for this relation"]
    foreign_keys: Annotated[frozenlist[ForeignKey], "Any number of foreign keys, each of which must be valid for both this relation and the target relation"]

RELATION_SCHEMA__NAME = hydra.core.Name("hydra.relational.RelationSchema")
RELATION_SCHEMA__NAME__NAME = hydra.core.Name("name")
RELATION_SCHEMA__COLUMNS__NAME = hydra.core.Name("columns")
RELATION_SCHEMA__PRIMARY_KEYS__NAME = hydra.core.Name("primaryKeys")
RELATION_SCHEMA__FOREIGN_KEYS__NAME = hydra.core.Name("foreignKeys")

class Relationship(Node["frozenset[FrozenDict[ColumnName, V]]"], Generic[V]):
    r"""A domain-unordered (string-indexed, rather than position-indexed) relation."""

RELATIONSHIP__NAME = hydra.core.Name("hydra.relational.Relationship")

class Row(Node["frozenlist[V]"], Generic[V]):
    r"""An n-tuple which is an element of a given relation."""

ROW__NAME = hydra.core.Name("hydra.relational.Row")
