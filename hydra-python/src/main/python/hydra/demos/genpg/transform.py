# Note: this is an automatically generated file. Do not edit.

r"""Functions for transforming property graph mappings into property graph elements."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.coders
import hydra.compute
import hydra.core
import hydra.extract.core
import hydra.graph
import hydra.lib.chars
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.pg.model
import hydra.reduction
import hydra.relational
import hydra.rewriting
import hydra.tabular

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def concat_pairs(acc: tuple[frozenlist[T0], frozenlist[T1]], p: tuple[frozenlist[T0], frozenlist[T1]]) -> tuple[frozenlist[T0], frozenlist[T1]]:
    return (hydra.lib.lists.concat2(hydra.lib.pairs.first(acc), hydra.lib.pairs.first(p)), hydra.lib.lists.concat2(hydra.lib.pairs.second(acc), hydra.lib.pairs.second(p)))

def decode_cell(col_type: hydra.tabular.ColumnType, mvalue: Maybe[str]) -> Either[str, Maybe[hydra.core.Term]]:
    r"""Decode a single cell value based on its column type."""
    
    @lru_cache(1)
    def cname() -> str:
        return col_type.name.value
    @lru_cache(1)
    def typ() -> hydra.core.Type:
        return col_type.type
    return hydra.lib.maybes.maybe(Right(Nothing()), (lambda value: (parse_error := hydra.lib.strings.cat(("Invalid value for column ", cname(), ": ", value)), _hoist_body_1 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), _hoist_body_2 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), _hoist_body_3 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.dsl.python.unsupported("inline match expressions are not yet supported"))[4]), mvalue)

def decode_row(col_types: frozenlist[hydra.tabular.ColumnType], row: hydra.tabular.DataRow[str]) -> Either[str, hydra.tabular.DataRow[hydra.core.Term]]:
    r"""Decode a single data row based on column types."""
    
    @lru_cache(1)
    def cells() -> frozenlist[Maybe[str]]:
        return row.value
    return hydra.lib.eithers.map((lambda decoded_cells: hydra.tabular.DataRow(decoded_cells)), hydra.lib.eithers.map_list((lambda pair: (col_type := hydra.lib.pairs.first(pair), mvalue := hydra.lib.pairs.second(pair), decode_cell(col_type, mvalue))[2]), hydra.lib.lists.zip(col_types, cells())))

def decode_table(table_type: hydra.tabular.TableType, table: hydra.tabular.Table[str]) -> Either[str, hydra.tabular.Table[hydra.core.Term]]:
    r"""Decode a table of strings into a table of terms based on column type specifications."""
    
    @lru_cache(1)
    def col_types() -> frozenlist[hydra.tabular.ColumnType]:
        return table_type.columns
    @lru_cache(1)
    def header() -> Maybe[hydra.tabular.HeaderRow]:
        return table.header
    @lru_cache(1)
    def rows() -> frozenlist[hydra.tabular.DataRow[str]]:
        return table.data
    return hydra.lib.eithers.map((lambda decoded_rows: hydra.tabular.Table(header(), decoded_rows)), hydra.lib.eithers.map_list((lambda row: decode_row(col_types(), row)), rows()))

def element_is_edge(el: hydra.pg.model.Element[T0]) -> bool:
    match el:
        case hydra.pg.model.ElementEdge():
            return True
        
        case _:
            return False

def element_is_vertex(el: hydra.pg.model.Element[T0]) -> bool:
    match el:
        case hydra.pg.model.ElementVertex():
            return True
        
        case _:
            return False

def find_tables_in_term(term: hydra.core.Term) -> frozenset[str]:
    def _hoist_hydra_demos_genpg_transform_find_tables_in_term_1(names: frozenset[str], v1: hydra.core.Elimination) -> frozenset[str]:
        match v1:
            case hydra.core.EliminationRecord(value=proj):
                return hydra.lib.sets.insert(proj.type_name.value, names)
            
            case _:
                return names
    def _hoist_hydra_demos_genpg_transform_find_tables_in_term_2(names: frozenset[str], v1: hydra.core.Function) -> frozenset[str]:
        match v1:
            case hydra.core.FunctionElimination(value=e):
                return _hoist_hydra_demos_genpg_transform_find_tables_in_term_1(names, e)
            
            case _:
                return names
    def _hoist_hydra_demos_genpg_transform_find_tables_in_term_3(names: frozenset[str], v1: hydra.core.Term) -> frozenset[str]:
        match v1:
            case hydra.core.TermFunction(value=f):
                return _hoist_hydra_demos_genpg_transform_find_tables_in_term_2(names, f)
            
            case _:
                return names
    return hydra.rewriting.fold_over_term(hydra.coders.TraversalOrder.PRE, (lambda names, t: _hoist_hydra_demos_genpg_transform_find_tables_in_term_3(names, t)), hydra.lib.sets.empty(), term)

def find_tables_in_terms(terms: frozenlist[hydra.core.Term]) -> frozenset[str]:
    r"""Find table names referenced in multiple terms."""
    
    return hydra.lib.sets.unions(hydra.lib.lists.map(find_tables_in_term, terms))

def table_for_edge(edge: hydra.pg.model.Edge[hydra.core.Term]) -> Either[str, str]:
    r"""Get the table name for an edge specification. Returns an error if not exactly one table is referenced."""
    
    @lru_cache(1)
    def label() -> hydra.core.Type:
        return edge.label
    @lru_cache(1)
    def id() -> hydra.core.Type:
        return edge.id
    @lru_cache(1)
    def out_id() -> hydra.core.Type:
        return edge.out
    @lru_cache(1)
    def in_id() -> hydra.core.Type:
        return edge.in_
    @lru_cache(1)
    def props() -> FrozenDict[hydra.pg.model.PropertyKey, hydra.core.Term]:
        return edge.properties
    @lru_cache(1)
    def tables() -> frozenset[str]:
        return find_tables_in_terms(hydra.lib.lists.concat2((id(), out_id(), in_id()), hydra.lib.maps.elems(props())))
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.sets.size(tables()), 1), (lambda : Right(hydra.lib.lists.head(hydra.lib.sets.to_list(tables())))), (lambda : Left(hydra.lib.strings.cat(("Specification for ", label().value, " edges has wrong number of tables")))))

def table_for_vertex(vertex: hydra.pg.model.Vertex[hydra.core.Term]) -> Either[str, str]:
    r"""Get the table name for a vertex specification. Returns an error if not exactly one table is referenced."""
    
    @lru_cache(1)
    def label() -> hydra.core.Type:
        return vertex.label
    @lru_cache(1)
    def id() -> hydra.core.Type:
        return vertex.id
    @lru_cache(1)
    def props() -> FrozenDict[hydra.pg.model.PropertyKey, hydra.core.Term]:
        return vertex.properties
    @lru_cache(1)
    def tables() -> frozenset[str]:
        return find_tables_in_terms(hydra.lib.lists.cons(id(), hydra.lib.maps.elems(props())))
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.sets.size(tables()), 1), (lambda : Right(hydra.lib.lists.head(hydra.lib.sets.to_list(tables())))), (lambda : Left(hydra.lib.strings.cat(("Specification for ", label().value, " vertices has wrong number of tables")))))

def element_specs_by_table(graph: hydra.pg.model.LazyGraph[hydra.core.Term]) -> Either[str, FrozenDict[str, tuple[frozenlist[hydra.pg.model.Vertex[hydra.core.Term]], frozenlist[hydra.pg.model.Edge[hydra.core.Term]]]]]:
    r"""Group element specifications by their source table."""
    
    @lru_cache(1)
    def vertices() -> frozenlist[hydra.pg.model.Vertex[hydra.core.Term]]:
        return graph.vertices
    @lru_cache(1)
    def edges() -> frozenlist[hydra.pg.model.Edge[hydra.core.Term]]:
        return graph.edges
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v: hydra.lib.eithers.map((lambda t: (t, v)), table_for_vertex(v))), vertices()), (lambda vertex_pairs: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda e: hydra.lib.eithers.map((lambda t: (t, e)), table_for_edge(e))), edges()), (lambda edge_pairs: (add_vertex := (lambda m, p: (table := hydra.lib.pairs.first(p), v := hydra.lib.pairs.second(p), existing := hydra.lib.maps.lookup(table, m), current := hydra.lib.maybes.from_maybe(((), ()), existing), hydra.lib.maps.insert(table, (hydra.lib.lists.cons(v, hydra.lib.pairs.first(current)), hydra.lib.pairs.second(current)), m))[4]), add_edge := (lambda m, p: (table := hydra.lib.pairs.first(p), e := hydra.lib.pairs.second(p), existing := hydra.lib.maps.lookup(table, m), current := hydra.lib.maybes.from_maybe(((), ()), existing), hydra.lib.maps.insert(table, (hydra.lib.pairs.first(current), hydra.lib.lists.cons(e, hydra.lib.pairs.second(current))), m))[4]), vertex_map := hydra.lib.lists.foldl((lambda x1, x2: add_vertex(x1, x2)), hydra.lib.maps.empty(), vertex_pairs), Right(hydra.lib.lists.foldl((lambda x1, x2: add_edge(x1, x2)), vertex_map, edge_pairs)))[3]))))

def evaluate_properties(specs: FrozenDict[T0, hydra.core.Term], record: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[T0, hydra.core.Term]]:
    return hydra.lib.flows.map((lambda pairs: hydra.lib.maps.from_list(hydra.lib.maybes.cat(pairs))), hydra.lib.flows.map_list((lambda pair: (k := hydra.lib.pairs.first(pair), spec := hydra.lib.pairs.second(pair), _hoist_body_1 := (lambda v1: hydra.dsl.python.unsupported("inline match expressions are not yet supported")), hydra.lib.flows.bind(hydra.reduction.reduce_term(True, cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(spec, record)))), (lambda value: _hoist_body_1(hydra.rewriting.deannotate_term(value)))))[3]), hydra.lib.maps.to_list(specs)))

def evaluate_edge(edge_spec: hydra.pg.model.Edge[hydra.core.Term], record: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.pg.model.Edge[hydra.core.Term]]]:
    r"""Evaluate an edge specification against a record term to produce an optional edge."""
    
    @lru_cache(1)
    def label() -> hydra.core.Type:
        return edge_spec.label
    @lru_cache(1)
    def id_spec() -> hydra.core.Type:
        return edge_spec.id
    @lru_cache(1)
    def out_spec() -> hydra.core.Type:
        return edge_spec.out
    @lru_cache(1)
    def in_spec() -> hydra.core.Type:
        return edge_spec.in_
    @lru_cache(1)
    def prop_specs() -> FrozenDict[hydra.pg.model.PropertyKey, hydra.core.Term]:
        return edge_spec.properties
    return hydra.lib.flows.bind(hydra.reduction.reduce_term(True, cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(id_spec(), record)))), (lambda id: hydra.lib.flows.bind(hydra.lib.flows.bind(hydra.reduction.reduce_term(True, cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(out_spec(), record)))), (lambda v1: hydra.extract.core.maybe_term((lambda t: hydra.lib.flows.pure(t)), v1))), (lambda m_out_id: hydra.lib.flows.bind(hydra.lib.flows.bind(hydra.reduction.reduce_term(True, cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(in_spec(), record)))), (lambda v1: hydra.extract.core.maybe_term((lambda t: hydra.lib.flows.pure(t)), v1))), (lambda m_in_id: hydra.lib.flows.bind(evaluate_properties(prop_specs(), record), (lambda props: hydra.lib.flows.pure(hydra.lib.maybes.bind(m_out_id, (lambda out_id: hydra.lib.maybes.map((lambda in_id: hydra.pg.model.Edge(label(), id, out_id, in_id, props)), m_in_id))))))))))))

def evaluate_vertex(vertex_spec: hydra.pg.model.Vertex[hydra.core.Term], record: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.pg.model.Vertex[hydra.core.Term]]]:
    r"""Evaluate a vertex specification against a record term to produce an optional vertex."""
    
    @lru_cache(1)
    def label() -> hydra.core.Type:
        return vertex_spec.label
    @lru_cache(1)
    def id_spec() -> hydra.core.Type:
        return vertex_spec.id
    @lru_cache(1)
    def prop_specs() -> FrozenDict[hydra.pg.model.PropertyKey, hydra.core.Term]:
        return vertex_spec.properties
    return hydra.lib.flows.bind(hydra.lib.flows.bind(hydra.reduction.reduce_term(True, cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(id_spec(), record)))), (lambda v1: hydra.extract.core.maybe_term((lambda t: hydra.lib.flows.pure(t)), v1))), (lambda m_id: hydra.lib.flows.bind(evaluate_properties(prop_specs(), record), (lambda props: hydra.lib.flows.pure(hydra.lib.maybes.map((lambda id: hydra.pg.model.Vertex(label(), id, props)), m_id))))))

def list_any(pred: Callable[[T0], bool], xs: frozenlist[T0]) -> bool:
    return hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.lists.filter(pred, xs)))

def make_lazy_graph(vertices: frozenlist[hydra.pg.model.Vertex[T0]], edges: frozenlist[hydra.pg.model.Edge[T0]]) -> hydra.pg.model.LazyGraph[T0]:
    return hydra.pg.model.LazyGraph(vertices, edges)

def normalize_field(s: str) -> Maybe[str]:
    r"""Normalize a CSV field value - empty becomes Nothing."""
    
    return hydra.lib.logic.if_else(hydra.lib.strings.null(s), (lambda : Nothing()), (lambda : Just(s)))

def parse_csv_char(state: tuple[tuple[frozenlist[Maybe[str]], str], bool], c: int) -> tuple[tuple[frozenlist[Maybe[str]], str], bool]:
    r"""Process a single character during CSV parsing."""
    
    @lru_cache(1)
    def acc() -> frozenlist[Maybe[str]]:
        return hydra.lib.pairs.first(hydra.lib.pairs.first(state))
    @lru_cache(1)
    def field() -> str:
        return hydra.lib.pairs.second(hydra.lib.pairs.first(state))
    @lru_cache(1)
    def in_quotes() -> bool:
        return hydra.lib.pairs.second(state)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(c, 34), (lambda : hydra.lib.logic.if_else(in_quotes(), (lambda : ((acc(), field()), False)), (lambda : hydra.lib.logic.if_else(hydra.lib.strings.null(field()), (lambda : ((acc(), field()), True)), (lambda : ((acc(), hydra.lib.strings.cat2(field(), "\"")), in_quotes())))))), (lambda : hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(c, 44), hydra.lib.logic.not_(in_quotes())), (lambda : ((hydra.lib.lists.cons(normalize_field(field()), acc()), ""), False)), (lambda : ((acc(), hydra.lib.strings.cat2(field(), hydra.lib.strings.from_list((c,)))), in_quotes())))))

def parse_csv_line(line: str) -> Either[str, frozenlist[Maybe[str]]]:
    r"""Parse a CSV line into fields. Empty fields become Nothing."""
    
    @lru_cache(1)
    def chars() -> frozenlist[int]:
        return hydra.lib.strings.to_list(line)
    @lru_cache(1)
    def init_state() -> tuple[tuple[frozenlist[T0], str], bool]:
        return (((), ""), False)
    @lru_cache(1)
    def final_state() -> tuple[tuple[frozenlist[Maybe[str]], str], bool]:
        return hydra.lib.lists.foldl(parse_csv_char, init_state(), chars())
    @lru_cache(1)
    def acc() -> frozenlist[Maybe[str]]:
        return hydra.lib.pairs.first(hydra.lib.pairs.first(final_state()))
    @lru_cache(1)
    def field() -> str:
        return hydra.lib.pairs.second(hydra.lib.pairs.first(final_state()))
    @lru_cache(1)
    def in_quotes() -> bool:
        return hydra.lib.pairs.second(final_state())
    return hydra.lib.logic.if_else(in_quotes(), (lambda : Left("Unclosed quoted field")), (lambda : Right(hydra.lib.lists.reverse(hydra.lib.lists.cons(normalize_field(field()), acc())))))

def strip_whitespace(s: str) -> str:
    r"""Strip leading and trailing whitespace from a string."""
    
    @lru_cache(1)
    def chars() -> frozenlist[int]:
        return hydra.lib.strings.to_list(s)
    def is_space_char(c: int) -> bool:
        return hydra.lib.chars.is_space(c)
    @lru_cache(1)
    def trim_left() -> frozenlist[int]:
        return hydra.lib.lists.drop_while(is_space_char, chars())
    @lru_cache(1)
    def trim_right() -> frozenlist[int]:
        return hydra.lib.lists.reverse(hydra.lib.lists.drop_while(is_space_char, hydra.lib.lists.reverse(trim_left())))
    return hydra.lib.strings.from_list(trim_right())

def parse_single_line(line: str) -> Either[str, Maybe[frozenlist[Maybe[str]]]]:
    r"""Parse a single CSV line, returning Nothing for empty lines."""
    
    @lru_cache(1)
    def trimmed() -> str:
        return strip_whitespace(line)
    return hydra.lib.logic.if_else(hydra.lib.strings.null(trimmed()), (lambda : Right(Nothing())), (lambda : hydra.lib.eithers.map((lambda x: Just(x)), parse_csv_line(trimmed()))))

def parse_table_lines(has_header: bool, raw_lines: frozenlist[str]) -> Either[str, hydra.tabular.Table[str]]:
    r"""Parse raw CSV lines into a Table of strings."""
    
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda ln: parse_single_line(ln)), raw_lines), (lambda parsed_rows: (rows := hydra.lib.maybes.cat(parsed_rows), hydra.lib.logic.if_else(has_header, (lambda : (header_row := hydra.lib.lists.head(rows), (data_rows := hydra.lib.lists.tail(rows), hydra.lib.logic.if_else(list_any((lambda m: hydra.lib.maybes.is_nothing(m)), header_row), (lambda : Left("null header column(s)")), (lambda : Right(hydra.tabular.Table(Just(hydra.tabular.HeaderRow(hydra.lib.maybes.cat(header_row))), hydra.lib.lists.map((lambda r: hydra.tabular.DataRow(r)), data_rows))))))[1])[1]), (lambda : Right(hydra.tabular.Table(Nothing(), hydra.lib.lists.map((lambda r: hydra.tabular.DataRow(r)), rows))))))[1]))

def table_types_by_name(table_types: frozenlist[hydra.tabular.TableType]) -> FrozenDict[hydra.relational.RelationName, hydra.tabular.TableType]:
    r"""Build a map from table name to table type."""
    
    return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda t: (t.name, t)), table_types))

def term_row_to_record(table_type: hydra.tabular.TableType, row: hydra.tabular.DataRow[hydra.core.Term]) -> hydra.core.Type:
    r"""Convert a data row to a record term given a table type."""
    
    @lru_cache(1)
    def tname() -> str:
        return table_type.name.value
    @lru_cache(1)
    def col_types() -> frozenlist[hydra.tabular.ColumnType]:
        return table_type.columns
    @lru_cache(1)
    def cells() -> frozenlist[Maybe[hydra.core.Term]]:
        return row.value
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name(tname()), hydra.lib.lists.zip_with((lambda col_type, mvalue: (cname := col_type.name.value, hydra.core.Field(hydra.core.Name(cname), cast(hydra.core.Term, hydra.core.TermMaybe(mvalue))))[1]), col_types(), cells()))))

def transform_record(vspecs: frozenlist[hydra.pg.model.Vertex[hydra.core.Term]], especs: frozenlist[hydra.pg.model.Edge[hydra.core.Term]], record: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, tuple[frozenlist[hydra.pg.model.Vertex[hydra.core.Term]], frozenlist[hydra.pg.model.Edge[hydra.core.Term]]]]:
    r"""Transform a record through vertex and edge specifications to produce vertices and edges."""
    
    return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda spec: evaluate_vertex(spec, record)), vspecs), (lambda m_vertices: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda spec: evaluate_edge(spec, record)), especs), (lambda m_edges: hydra.lib.flows.pure((hydra.lib.maybes.cat(m_vertices), hydra.lib.maybes.cat(m_edges)))))))

def transform_table_rows(vspecs: frozenlist[hydra.pg.model.Vertex[hydra.core.Term]], especs: frozenlist[hydra.pg.model.Edge[hydra.core.Term]], table_type: hydra.tabular.TableType, rows: frozenlist[hydra.tabular.DataRow[hydra.core.Term]]) -> hydra.compute.Flow[hydra.graph.Graph, tuple[frozenlist[hydra.pg.model.Vertex[hydra.core.Term]], frozenlist[hydra.pg.model.Edge[hydra.core.Term]]]]:
    r"""Transform all rows from a table through vertex/edge specifications."""
    
    return hydra.lib.flows.map((lambda pairs: hydra.lib.lists.foldl((lambda x1, x2: concat_pairs(x1, x2)), ((), ()), pairs)), hydra.lib.flows.map_list((lambda row: transform_record(vspecs, especs, term_row_to_record(table_type, row))), rows))
