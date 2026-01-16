# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.relational."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.sets
import hydra.relational

T0 = TypeVar("T0")

def column_name(x: hydra.relational.ColumnName) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.ColumnName"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.value))))

def column_schema(t: Callable[[T0], hydra.core.Term], x: hydra.relational.ColumnSchema[T0]) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.ColumnSchema"), (hydra.core.Field(hydra.core.Name("name"), column_name(x.name)), hydra.core.Field(hydra.core.Name("domain"), t(x.domain))))))

def relation_name(x: hydra.relational.RelationName) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.RelationName"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.value))))

def foreign_key(x: hydra.relational.ForeignKey) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.ForeignKey"), (hydra.core.Field(hydra.core.Name("foreignRelation"), relation_name(x.foreign_relation)), hydra.core.Field(hydra.core.Name("keys"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(column_name, column_name, m))))(x.keys))))))

def primary_key(x: hydra.relational.PrimaryKey) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.PrimaryKey"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(column_name, xs))))(x.value))))

def row(v: Callable[[T0], hydra.core.Term], x: hydra.relational.Row[T0]) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.Row"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(v, xs))))(x.value))))

def relation(v: Callable[[T0], hydra.core.Term], x: hydra.relational.Relation[T0]) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.Relation"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda v1: row(v, v1)), xs))))(x.value))))

def relation_schema(t: Callable[[T0], hydra.core.Term], x: hydra.relational.RelationSchema[T0]) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.RelationSchema"), (hydra.core.Field(hydra.core.Name("name"), relation_name(x.name)), hydra.core.Field(hydra.core.Name("columns"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda v1: column_schema(t, v1)), xs))))(x.columns)), hydra.core.Field(hydra.core.Name("primaryKeys"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(primary_key, xs))))(x.primary_keys)), hydra.core.Field(hydra.core.Name("foreignKeys"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(foreign_key, xs))))(x.foreign_keys))))))

def relationship(v: Callable[[T0], hydra.core.Term], x: hydra.relational.Relationship[T0]) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.Relationship"), (lambda s: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map((lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(column_name, v, m)))), s))))(x.value))))
