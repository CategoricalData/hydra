# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.relational."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import TypeVar
import hydra.core
import hydra.extract.helpers
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.relational
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def column_name(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.ColumnName]:
    def _hoist_hydra_decode_relational_column_name_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)
            
            case _:
                return Left(hydra.util.DecodingError("expected string literal"))
    def _hoist_hydra_decode_relational_column_name_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_relational_column_name_1(v)
            
            case _:
                return Left(hydra.util.DecodingError("expected literal"))
    def _hoist_hydra_decode_relational_column_name_3(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.ColumnName]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.relational.ColumnName(b)), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_hydra_decode_relational_column_name_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.relational.ColumnName"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_relational_column_name_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def column_schema(t: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.ColumnSchema[T0]]:
    def _hoist_hydra_decode_relational_column_schema_1(cx: hydra.graph.Graph, t: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.ColumnSchema[T1]]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", column_name, field_map, cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("domain", t, field_map, cx), (lambda field_domain: Right(hydra.relational.ColumnSchema(field_name, field_domain))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.relational.ColumnSchema"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_relational_column_schema_1(cx, t, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def relation_name(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.RelationName]:
    def _hoist_hydra_decode_relational_relation_name_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)
            
            case _:
                return Left(hydra.util.DecodingError("expected string literal"))
    def _hoist_hydra_decode_relational_relation_name_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_relational_relation_name_1(v)
            
            case _:
                return Left(hydra.util.DecodingError("expected literal"))
    def _hoist_hydra_decode_relational_relation_name_3(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.RelationName]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.relational.RelationName(b)), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_hydra_decode_relational_relation_name_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.relational.RelationName"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_relational_relation_name_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def foreign_key(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.ForeignKey]:
    def _hoist_hydra_decode_relational_foreign_key_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.ForeignKey]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("foreignRelation", relation_name, field_map, cx), (lambda field_foreign_relation: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("keys", (lambda v1, v2: hydra.extract.helpers.decode_map(column_name, column_name, v1, v2)), field_map, cx), (lambda field_keys: Right(hydra.relational.ForeignKey(field_foreign_relation, field_keys))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.relational.ForeignKey"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_relational_foreign_key_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def primary_key(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.PrimaryKey]:
    def _hoist_hydra_decode_relational_primary_key_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.PrimaryKey]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.relational.PrimaryKey(b)), hydra.extract.helpers.decode_list(column_name, cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.relational.PrimaryKey"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_relational_primary_key_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def row(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.Row[T0]]:
    def _hoist_hydra_decode_relational_row_1(cx: hydra.graph.Graph, v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.Row[T1]]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.relational.Row(b)), hydra.extract.helpers.decode_list(v, cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.relational.Row"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_relational_row_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def relation(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.Relation[T0]]:
    def _hoist_hydra_decode_relational_relation_1(cx: hydra.graph.Graph, v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.Relation[T1]]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.relational.Relation(b)), hydra.extract.helpers.decode_list((lambda v1, v2: row(v, v1, v2)), cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.relational.Relation"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_relational_relation_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def relation_schema(t: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.RelationSchema[T0]]:
    def _hoist_hydra_decode_relational_relation_schema_1(cx: hydra.graph.Graph, t: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.RelationSchema[T1]]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", relation_name, field_map, cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("columns", (lambda v1, v2: hydra.extract.helpers.decode_list((lambda v1, v2: column_schema(t, v1, v2)), v1, v2)), field_map, cx), (lambda field_columns: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("primaryKeys", (lambda v1, v2: hydra.extract.helpers.decode_list(primary_key, v1, v2)), field_map, cx), (lambda field_primary_keys: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("foreignKeys", (lambda v1, v2: hydra.extract.helpers.decode_list(foreign_key, v1, v2)), field_map, cx), (lambda field_foreign_keys: Right(hydra.relational.RelationSchema(field_name, field_columns, field_primary_keys, field_foreign_keys))))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.relational.RelationSchema"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_relational_relation_schema_1(cx, t, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def relationship(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.Relationship[T0]]:
    def _hoist_hydra_decode_relational_relationship_1(cx: hydra.graph.Graph, v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.relational.Relationship[T1]]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.relational.Relationship(b)), hydra.extract.helpers.decode_set((lambda v1, v2: hydra.extract.helpers.decode_map(column_name, v, v1, v2)), cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.relational.Relationship"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_relational_relationship_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
