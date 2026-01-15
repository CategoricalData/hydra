# Note: this is an automatically generated file. Do not edit.

r"""Utility functions for GraphSON encoding and property graph conversion."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import FrozenDict, frozenlist
from typing import TypeVar, cast
import hydra.compute
import hydra.core
import hydra.json.model
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.pg.graphson.construct
import hydra.pg.graphson.syntax
import hydra.pg.model
import hydra.rewriting

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def elements_to_vertices_with_adjacent_edges(els: frozenlist[hydra.pg.model.Element[T0]]) -> frozenlist[hydra.pg.model.VertexWithAdjacentEdges[T0]]:
    @lru_cache(1)
    def partitioned() -> tuple[frozenlist[hydra.pg.model.Vertex[T0]], frozenlist[hydra.pg.model.Edge[T0]]]:
        def _hoist_partitioned_1(acc: tuple[frozenlist[hydra.pg.model.Vertex[T1]], frozenlist[hydra.pg.model.Edge[T1]]], v1: hydra.pg.model.Element[T1]) -> tuple[frozenlist[hydra.pg.model.Vertex[T1]], frozenlist[hydra.pg.model.Edge[T1]]]:
            match v1:
                case hydra.pg.model.ElementVertex(value=v):
                    return (hydra.lib.lists.cons(v, hydra.lib.pairs.first(acc)), hydra.lib.pairs.second(acc))
                
                case hydra.pg.model.ElementEdge(value=e):
                    return (hydra.lib.pairs.first(acc), hydra.lib.lists.cons(e, hydra.lib.pairs.second(acc)))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        return hydra.lib.lists.foldl((lambda acc, el: _hoist_partitioned_1(acc, el)), ((), ()), els)
    @lru_cache(1)
    def vertices() -> frozenlist[hydra.pg.model.Vertex[T0]]:
        return hydra.lib.lists.reverse(hydra.lib.pairs.first(partitioned()))
    @lru_cache(1)
    def edges() -> frozenlist[hydra.pg.model.Edge[T0]]:
        return hydra.lib.lists.reverse(hydra.lib.pairs.second(partitioned()))
    @lru_cache(1)
    def vertex_map0() -> FrozenDict[T0, hydra.pg.model.VertexWithAdjacentEdges[T0]]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda v: (v.id, hydra.pg.model.VertexWithAdjacentEdges(v, (), ()))), vertices()))
    @lru_cache(1)
    def vertex_map1() -> FrozenDict[T0, hydra.pg.model.VertexWithAdjacentEdges[T0]]:
        return hydra.lib.lists.foldl((lambda vmap, edge: (label := edge.label, edge_id := edge.id, out_v := edge.out, in_v := edge.in_, props := edge.properties, adj_edge_out := hydra.pg.model.AdjacentEdge(label, edge_id, in_v, props), adj_edge_in := hydra.pg.model.AdjacentEdge(label, edge_id, out_v, props), vmap1 := hydra.lib.maybes.maybe(vmap, (lambda vae: hydra.lib.maps.insert(out_v, hydra.pg.model.VertexWithAdjacentEdges(vae.vertex, vae.ins, hydra.lib.lists.cons(adj_edge_out, vae.outs)), vmap)), hydra.lib.maps.lookup(out_v, vmap)), hydra.lib.maybes.maybe(vmap1, (lambda vae: hydra.lib.maps.insert(in_v, hydra.pg.model.VertexWithAdjacentEdges(vae.vertex, hydra.lib.lists.cons(adj_edge_in, vae.ins), vae.outs), vmap1)), hydra.lib.maps.lookup(in_v, vmap1)))[8]), vertex_map0(), edges())
    return hydra.lib.maps.elems(vertex_map1())

def encode_string_value(s: str) -> hydra.compute.Flow[T0, hydra.pg.graphson.syntax.Value]:
    return hydra.lib.flows.pure(cast(hydra.pg.graphson.syntax.Value, hydra.pg.graphson.syntax.ValueString(s)))

def encode_term_value(term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.pg.graphson.syntax.Value]:
    def _hoist_hydra_pg_graphson_utils_encode_term_value_1(v1: hydra.core.FloatValue) -> hydra.compute.Flow[T1, hydra.pg.graphson.syntax.Value]:
        match v1:
            case hydra.core.FloatValueBigfloat(value=f):
                return hydra.lib.flows.pure(cast(hydra.pg.graphson.syntax.Value, hydra.pg.graphson.syntax.ValueBigDecimal(hydra.pg.graphson.syntax.BigDecimalValue(hydra.lib.literals.show_bigfloat(f)))))
            
            case hydra.core.FloatValueFloat32(value=f2):
                return hydra.lib.flows.pure(cast(hydra.pg.graphson.syntax.Value, hydra.pg.graphson.syntax.ValueFloat(cast(hydra.pg.graphson.syntax.FloatValue, hydra.pg.graphson.syntax.FloatValueFinite(f2)))))
            
            case hydra.core.FloatValueFloat64(value=f3):
                return hydra.lib.flows.pure(cast(hydra.pg.graphson.syntax.Value, hydra.pg.graphson.syntax.ValueDouble(cast(hydra.pg.graphson.syntax.DoubleValue, hydra.pg.graphson.syntax.DoubleValueFinite(f3)))))
            
            case _:
                return hydra.lib.flows.fail("unsupported float type")
    def _hoist_hydra_pg_graphson_utils_encode_term_value_2(v1: hydra.core.IntegerValue) -> hydra.compute.Flow[T1, hydra.pg.graphson.syntax.Value]:
        match v1:
            case hydra.core.IntegerValueBigint(value=i):
                return hydra.lib.flows.pure(cast(hydra.pg.graphson.syntax.Value, hydra.pg.graphson.syntax.ValueBigInteger(i)))
            
            case hydra.core.IntegerValueInt32(value=i2):
                return hydra.lib.flows.pure(cast(hydra.pg.graphson.syntax.Value, hydra.pg.graphson.syntax.ValueInteger(i2)))
            
            case hydra.core.IntegerValueInt64(value=i3):
                return hydra.lib.flows.pure(cast(hydra.pg.graphson.syntax.Value, hydra.pg.graphson.syntax.ValueLong(i3)))
            
            case _:
                return hydra.lib.flows.fail("unsupported integer type")
    def _hoist_hydra_pg_graphson_utils_encode_term_value_3(v1: hydra.core.Literal) -> hydra.compute.Flow[T1, hydra.pg.graphson.syntax.Value]:
        match v1:
            case hydra.core.LiteralBinary(value=b):
                return hydra.lib.flows.pure(cast(hydra.pg.graphson.syntax.Value, hydra.pg.graphson.syntax.ValueBinary(hydra.lib.literals.binary_to_string(b))))
            
            case hydra.core.LiteralBoolean(value=b2):
                return hydra.lib.flows.pure(cast(hydra.pg.graphson.syntax.Value, hydra.pg.graphson.syntax.ValueBoolean(b2)))
            
            case hydra.core.LiteralFloat(value=fv):
                return _hoist_hydra_pg_graphson_utils_encode_term_value_1(fv)
            
            case hydra.core.LiteralInteger(value=iv):
                return _hoist_hydra_pg_graphson_utils_encode_term_value_2(iv)
            
            case hydra.core.LiteralString(value=s):
                return hydra.lib.flows.pure(cast(hydra.pg.graphson.syntax.Value, hydra.pg.graphson.syntax.ValueString(s)))
            
            case _:
                return hydra.lib.flows.fail("unsupported literal type for GraphSON encoding")
    match hydra.rewriting.deannotate_term(term):
        case hydra.core.TermLiteral(value=lit):
            return _hoist_hydra_pg_graphson_utils_encode_term_value_3(lit)
        
        case hydra.core.TermUnit():
            return hydra.lib.flows.pure(cast(hydra.pg.graphson.syntax.Value, hydra.pg.graphson.syntax.ValueNull()))
        
        case _:
            return hydra.lib.flows.fail("unsupported term variant for GraphSON encoding")

def pg_elements_to_graphson(encode_value: Callable[[T0], hydra.compute.Flow[T1, hydra.pg.graphson.syntax.Value]], els: frozenlist[hydra.pg.model.Element[T0]]) -> hydra.compute.Flow[T1, frozenlist[hydra.json.model.Value]]:
    return hydra.lib.flows.map_list((lambda v1: hydra.pg.graphson.construct.pg_vertex_with_adjacent_edges_to_json(encode_value, v1)), elements_to_vertices_with_adjacent_edges(els))
