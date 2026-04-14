# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.relational."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import FrozenDict, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.phantoms

T = TypeVar("T")
V = TypeVar("V")

def column_name(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.ColumnName"), x.value))))

def column_schema(name: hydra.phantoms.TTerm[hydra.relational.ColumnName], domain: hydra.phantoms.TTerm[T]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.ColumnSchema"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("domain"), domain.value))))))

def column_schema_domain(x: hydra.phantoms.TTerm[hydra.relational.ColumnSchema[T]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.ColumnSchema"), hydra.core.Name("domain")))), x.value))))

def column_schema_name(x: hydra.phantoms.TTerm[hydra.relational.ColumnSchema[T]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.ColumnSchema"), hydra.core.Name("name")))), x.value))))

def column_schema_with_domain(original: hydra.phantoms.TTerm[hydra.relational.ColumnSchema[T]], new_val: hydra.phantoms.TTerm[T]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.ColumnSchema"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.ColumnSchema"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("domain"), new_val.value))))))

def column_schema_with_name(original: hydra.phantoms.TTerm[hydra.relational.ColumnSchema[T]], new_val: hydra.phantoms.TTerm[hydra.relational.ColumnName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.ColumnSchema"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("domain"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.ColumnSchema"), hydra.core.Name("domain")))), original.value)))))))))

def foreign_key(foreign_relation: hydra.phantoms.TTerm[hydra.relational.RelationName], keys: hydra.phantoms.TTerm[FrozenDict[hydra.relational.ColumnName, hydra.relational.ColumnName]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.ForeignKey"), (hydra.core.Field(hydra.core.Name("foreignRelation"), foreign_relation.value), hydra.core.Field(hydra.core.Name("keys"), keys.value))))))

def foreign_key_foreign_relation(x: hydra.phantoms.TTerm[hydra.relational.ForeignKey]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.ForeignKey"), hydra.core.Name("foreignRelation")))), x.value))))

def foreign_key_keys(x: hydra.phantoms.TTerm[hydra.relational.ForeignKey]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.ForeignKey"), hydra.core.Name("keys")))), x.value))))

def foreign_key_with_foreign_relation(original: hydra.phantoms.TTerm[hydra.relational.ForeignKey], new_val: hydra.phantoms.TTerm[hydra.relational.RelationName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.ForeignKey"), (hydra.core.Field(hydra.core.Name("foreignRelation"), new_val.value), hydra.core.Field(hydra.core.Name("keys"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.ForeignKey"), hydra.core.Name("keys")))), original.value)))))))))

def foreign_key_with_keys(original: hydra.phantoms.TTerm[hydra.relational.ForeignKey], new_val: hydra.phantoms.TTerm[FrozenDict[hydra.relational.ColumnName, hydra.relational.ColumnName]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.ForeignKey"), (hydra.core.Field(hydra.core.Name("foreignRelation"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.ForeignKey"), hydra.core.Name("foreignRelation")))), original.value)))), hydra.core.Field(hydra.core.Name("keys"), new_val.value))))))

def primary_key(x: hydra.phantoms.TTerm[frozenlist[hydra.relational.ColumnName]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.PrimaryKey"), x.value))))

def relation(x: hydra.phantoms.TTerm[frozenlist[hydra.relational.Row[V]]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.Relation"), x.value))))

def relation_name(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.RelationName"), x.value))))

def relation_schema(name: hydra.phantoms.TTerm[hydra.relational.RelationName], columns: hydra.phantoms.TTerm[frozenlist[hydra.relational.ColumnSchema[T]]], primary_keys: hydra.phantoms.TTerm[frozenlist[hydra.relational.PrimaryKey]], foreign_keys: hydra.phantoms.TTerm[frozenlist[hydra.relational.ForeignKey]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.RelationSchema"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("columns"), columns.value), hydra.core.Field(hydra.core.Name("primaryKeys"), primary_keys.value), hydra.core.Field(hydra.core.Name("foreignKeys"), foreign_keys.value))))))

def relation_schema_columns(x: hydra.phantoms.TTerm[hydra.relational.RelationSchema[T]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("columns")))), x.value))))

def relation_schema_foreign_keys(x: hydra.phantoms.TTerm[hydra.relational.RelationSchema[T]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("foreignKeys")))), x.value))))

def relation_schema_name(x: hydra.phantoms.TTerm[hydra.relational.RelationSchema[T]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("name")))), x.value))))

def relation_schema_primary_keys(x: hydra.phantoms.TTerm[hydra.relational.RelationSchema[T]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("primaryKeys")))), x.value))))

def relation_schema_with_columns(original: hydra.phantoms.TTerm[hydra.relational.RelationSchema[T]], new_val: hydra.phantoms.TTerm[frozenlist[hydra.relational.ColumnSchema[T]]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.RelationSchema"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("columns"), new_val.value), hydra.core.Field(hydra.core.Name("primaryKeys"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("primaryKeys")))), original.value)))), hydra.core.Field(hydra.core.Name("foreignKeys"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("foreignKeys")))), original.value)))))))))

def relation_schema_with_foreign_keys(original: hydra.phantoms.TTerm[hydra.relational.RelationSchema[T]], new_val: hydra.phantoms.TTerm[frozenlist[hydra.relational.ForeignKey]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.RelationSchema"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("columns"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("columns")))), original.value)))), hydra.core.Field(hydra.core.Name("primaryKeys"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("primaryKeys")))), original.value)))), hydra.core.Field(hydra.core.Name("foreignKeys"), new_val.value))))))

def relation_schema_with_name(original: hydra.phantoms.TTerm[hydra.relational.RelationSchema[T]], new_val: hydra.phantoms.TTerm[hydra.relational.RelationName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.RelationSchema"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("columns"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("columns")))), original.value)))), hydra.core.Field(hydra.core.Name("primaryKeys"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("primaryKeys")))), original.value)))), hydra.core.Field(hydra.core.Name("foreignKeys"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("foreignKeys")))), original.value)))))))))

def relation_schema_with_primary_keys(original: hydra.phantoms.TTerm[hydra.relational.RelationSchema[T]], new_val: hydra.phantoms.TTerm[frozenlist[hydra.relational.PrimaryKey]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.relational.RelationSchema"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("columns"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("columns")))), original.value)))), hydra.core.Field(hydra.core.Name("primaryKeys"), new_val.value), hydra.core.Field(hydra.core.Name("foreignKeys"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.relational.RelationSchema"), hydra.core.Name("foreignKeys")))), original.value)))))))))

def relationship(x: hydra.phantoms.TTerm[frozenset[FrozenDict[hydra.relational.ColumnName, V]]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.Relationship"), x.value))))

def row(x: hydra.phantoms.TTerm[frozenlist[V]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.relational.Row"), x.value))))

def un_column_name(x: hydra.phantoms.TTerm[hydra.relational.ColumnName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.relational.ColumnName"))), x.value))))

def un_primary_key(x: hydra.phantoms.TTerm[hydra.relational.PrimaryKey]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.relational.PrimaryKey"))), x.value))))

def un_relation(x: hydra.phantoms.TTerm[hydra.relational.Relation[V]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.relational.Relation"))), x.value))))

def un_relation_name(x: hydra.phantoms.TTerm[hydra.relational.RelationName]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.relational.RelationName"))), x.value))))

def un_relationship(x: hydra.phantoms.TTerm[hydra.relational.Relationship[V]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.relational.Relationship"))), x.value))))

def un_row(x: hydra.phantoms.TTerm[hydra.relational.Row[V]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.relational.Row"))), x.value))))
