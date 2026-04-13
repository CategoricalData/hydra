# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.relational."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.errors
import hydra.extract.core
import hydra.lib.eithers
import hydra.relational

T0 = TypeVar("T0")

def column_name(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_relational_column_name_1(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)

            case _:
                return Left(hydra.errors.DecodingError("expected string literal"))
    def _hoist_hydra_decode_relational_column_name_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_relational_column_name_1(v)

            case _:
                return Left(hydra.errors.DecodingError("expected literal"))
    def _hoist_hydra_decode_relational_column_name_3(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.relational.ColumnName(b)), hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_hydra_decode_relational_column_name_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx, wrapped_term.body)))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_relational_column_name_3(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def column_schema(t: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_relational_column_schema_1(cx, t, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: column_name(x1, x2)), field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.core.require_field("domain", t, field_map(), cx), (lambda field_domain: Right(hydra.relational.ColumnSchema(field_name, field_domain))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_relational_column_schema_1(cx, t, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def relation_name(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_relational_relation_name_1(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)

            case _:
                return Left(hydra.errors.DecodingError("expected string literal"))
    def _hoist_hydra_decode_relational_relation_name_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_relational_relation_name_1(v)

            case _:
                return Left(hydra.errors.DecodingError("expected literal"))
    def _hoist_hydra_decode_relational_relation_name_3(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.relational.RelationName(b)), hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_hydra_decode_relational_relation_name_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx, wrapped_term.body)))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_relational_relation_name_3(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def foreign_key(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_relational_foreign_key_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("foreignRelation", (lambda x1, x2: relation_name(x1, x2)), field_map(), cx), (lambda field_foreign_relation: hydra.lib.eithers.bind(hydra.extract.core.require_field("keys", (lambda v12, v2: hydra.extract.core.decode_map((lambda x1, x2: column_name(x1, x2)), (lambda x1, x2: column_name(x1, x2)), v12, v2)), field_map(), cx), (lambda field_keys: Right(hydra.relational.ForeignKey(field_foreign_relation, field_keys))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_relational_foreign_key_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def primary_key(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_relational_primary_key_1(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.relational.PrimaryKey(b)), hydra.extract.core.decode_list((lambda x1, x2: column_name(x1, x2)), cx, wrapped_term.body))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_relational_primary_key_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def row(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_relational_row_1(cx, v, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.relational.Row(b)), hydra.extract.core.decode_list(v, cx, wrapped_term.body))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_relational_row_1(cx, v, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def relation(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_relational_relation_1(cx, v, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.relational.Relation(b)), hydra.extract.core.decode_list((lambda v12, v2: row(v, v12, v2)), cx, wrapped_term.body))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_relational_relation_1(cx, v, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def relation_schema(t: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_relational_relation_schema_1(cx, t, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: relation_name(x1, x2)), field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.core.require_field("columns", (lambda v12, v2: hydra.extract.core.decode_list((lambda v13, v22: column_schema(t, v13, v22)), v12, v2)), field_map(), cx), (lambda field_columns: hydra.lib.eithers.bind(hydra.extract.core.require_field("primaryKeys", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: primary_key(x1, x2)), v12, v2)), field_map(), cx), (lambda field_primary_keys: hydra.lib.eithers.bind(hydra.extract.core.require_field("foreignKeys", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: foreign_key(x1, x2)), v12, v2)), field_map(), cx), (lambda field_foreign_keys: Right(hydra.relational.RelationSchema(field_name, field_columns, field_primary_keys, field_foreign_keys))))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_relational_relation_schema_1(cx, t, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def relationship(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_relational_relationship_1(cx, v, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.relational.Relationship(b)), hydra.extract.core.decode_set((lambda v12, v2: hydra.extract.core.decode_map((lambda x1, x2: column_name(x1, x2)), v, v12, v2)), cx, wrapped_term.body))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_relational_relationship_1(cx, v, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))
