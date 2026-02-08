# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.relational."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict
from typing import TypeVar, cast
import hydra.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.sets
import hydra.relational

T0 = TypeVar("T0")

def column_name(x: hydra.relational.ColumnName) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.ColumnName"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def column_schema(t: Callable[[T0], hydra.core.Term], x: hydra.relational.ColumnSchema[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.ColumnSchema"), (hydra.core.Field(hydra.core.Name("name"), column_name(x.name)), hydra.core.Field(hydra.core.Name("domain"), t(x.domain))))))

def relation_name(x: hydra.relational.RelationName) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.RelationName"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def foreign_key(x: hydra.relational.ForeignKey) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.ForeignKey"), (hydra.core.Field(hydra.core.Name("foreignRelation"), relation_name(x.foreign_relation)), hydra.core.Field(hydra.core.Name("keys"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: column_name(x1)), (lambda x1: column_name(x1)), x.keys))))))))

def primary_key(x: hydra.relational.PrimaryKey) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.PrimaryKey"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: column_name(x1)), x.value))))))

def row(v: Callable[[T0], hydra.core.Term], x: hydra.relational.Row[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.Row"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(v, x.value))))))

def relation(v: Callable[[T0], hydra.core.Term], x: hydra.relational.Relation[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.Relation"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda v1: row(v, v1)), x.value))))))

def relation_schema(t: Callable[[T0], hydra.core.Term], x: hydra.relational.RelationSchema[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.RelationSchema"), (hydra.core.Field(hydra.core.Name("name"), relation_name(x.name)), hydra.core.Field(hydra.core.Name("columns"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda v1: column_schema(t, v1)), x.columns)))), hydra.core.Field(hydra.core.Name("primaryKeys"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: primary_key(x1)), x.primary_keys)))), hydra.core.Field(hydra.core.Name("foreignKeys"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: foreign_key(x1)), x.foreign_keys))))))))

def relationship(v: Callable[[T0], hydra.core.Term], x: hydra.relational.Relationship[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.Relationship"), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map((lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: column_name(x1)), v, m)))), x.value))))))
