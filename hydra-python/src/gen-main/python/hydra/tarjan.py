# Note: this is an automatically generated file. Do not edit.

r"""This implementation of Tarjan's algorithm was originally based on GraphSCC by Iavor S. Diatchki: https://hackage.haskell.org/package/GraphSCC."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import FrozenDict, frozenlist
from typing import TypeVar, cast
import hydra.constants
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.topology

T0 = TypeVar("T0")

def adjacency_lists_to_graph(edges0: frozenlist[tuple[T0, frozenlist[T0]]]) -> tuple[FrozenDict[int, frozenlist[int]], Callable[[int], T0]]:
    r"""Given a list of adjacency lists represented as (key, [key]) pairs, construct a graph along with a function mapping each vertex (an Int) back to its original key."""

    @lru_cache(1)
    def sorted_edges() -> frozenlist[tuple[T0, frozenlist[T0]]]:
        return hydra.lib.lists.sort_on((lambda x1: hydra.lib.pairs.first(x1)), edges0)
    @lru_cache(1)
    def indexed_edges() -> frozenlist[tuple[int, tuple[T0, frozenlist[T0]]]]:
        return hydra.lib.lists.zip(hydra.lib.math.range_(0, hydra.lib.lists.length(sorted_edges())), sorted_edges())
    @lru_cache(1)
    def key_to_vertex() -> FrozenDict[T0, int]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda vk_neighbors: (v := hydra.lib.pairs.first(vk_neighbors), k_neighbors := hydra.lib.pairs.second(vk_neighbors), k := hydra.lib.pairs.first(k_neighbors), (k, v))[3]), indexed_edges()))
    @lru_cache(1)
    def vertex_map() -> FrozenDict[int, T0]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda vk_neighbors: (v := hydra.lib.pairs.first(vk_neighbors), k_neighbors := hydra.lib.pairs.second(vk_neighbors), k := hydra.lib.pairs.first(k_neighbors), (v, k))[3]), indexed_edges()))
    @lru_cache(1)
    def graph() -> FrozenDict[int, frozenlist[int]]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda vk_neighbors: (v := hydra.lib.pairs.first(vk_neighbors), k_neighbors := hydra.lib.pairs.second(vk_neighbors), neighbors := hydra.lib.pairs.second(k_neighbors), (v, hydra.lib.maybes.map_maybe((lambda k: hydra.lib.maps.lookup(k, key_to_vertex())), neighbors)))[3]), indexed_edges()))
    def vertex_to_key(v: int) -> T0:
        return hydra.lib.maybes.from_just(hydra.lib.maps.lookup(v, vertex_map()))
    return (graph(), (lambda x1: vertex_to_key(x1)))

@lru_cache(1)
def initial_state() -> hydra.topology.TarjanState:
    r"""Initial state for Tarjan's algorithm."""

    return hydra.topology.TarjanState(0, hydra.lib.maps.empty(), hydra.lib.maps.empty(), (), hydra.lib.sets.empty(), ())

def pop_stack_until(v: int, st0: hydra.topology.TarjanState) -> tuple[frozenlist[int], hydra.topology.TarjanState]:
    r"""Pop vertices off the stack until the given vertex is reached, collecting the current strongly connected component."""

    def go(acc: frozenlist[int], st: hydra.topology.TarjanState) -> tuple[frozenlist[int], hydra.topology.TarjanState]:
        @lru_cache(1)
        def x() -> int:
            return hydra.lib.lists.head(st.stack)
        @lru_cache(1)
        def xs() -> frozenlist[int]:
            return hydra.lib.lists.tail(st.stack)
        new_st = hydra.topology.TarjanState(st.counter, st.indices, st.low_links, xs(), st.on_stack, st.sccs)
        @lru_cache(1)
        def new_st2() -> hydra.topology.TarjanState:
            return hydra.topology.TarjanState(new_st.counter, new_st.indices, new_st.low_links, new_st.stack, hydra.lib.sets.delete(x(), st.on_stack), new_st.sccs)
        @lru_cache(1)
        def acc_() -> frozenlist[int]:
            return hydra.lib.lists.cons(x(), acc)
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(x(), v), (lambda : (hydra.lib.lists.reverse(acc_()), new_st2())), (lambda : go(acc_(), new_st2())))
    return go((), st0)

def strong_connect(graph: FrozenDict[int, frozenlist[int]], v: int, st: hydra.topology.TarjanState) -> hydra.topology.TarjanState:
    r"""Visit a vertex and recursively explore its successors."""

    i = st.counter
    @lru_cache(1)
    def new_st() -> hydra.topology.TarjanState:
        return hydra.topology.TarjanState(hydra.lib.math.add(i, 1), hydra.lib.maps.insert(v, i, st.indices), hydra.lib.maps.insert(v, i, st.low_links), hydra.lib.lists.cons(v, st.stack), hydra.lib.sets.insert(v, st.on_stack), st.sccs)
    @lru_cache(1)
    def neighbors() -> frozenlist[int]:
        return hydra.lib.maps.find_with_default((), v, graph)
    def process_neighbor(st_: hydra.topology.TarjanState, w: int) -> hydra.topology.TarjanState:
        def low_link(s: hydra.topology.TarjanState) -> hydra.topology.TarjanState:
            @lru_cache(1)
            def low_v1() -> int:
                return hydra.lib.maps.find_with_default(hydra.constants.max_int32, v, s.low_links)
            @lru_cache(1)
            def idx_w() -> int:
                return hydra.lib.maps.find_with_default(hydra.constants.max_int32, w, s.indices)
            return hydra.topology.TarjanState(s.counter, s.indices, hydra.lib.maps.insert(v, hydra.lib.equality.min(low_v1(), idx_w()), s.low_links), s.stack, s.on_stack, s.sccs)
        return hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.maps.member(w, st_.indices)), (lambda : (st_after := strong_connect(graph, w, st_), (low_v2 := hydra.lib.maps.find_with_default(hydra.constants.max_int32, v, st_after.low_links), (low_w := hydra.lib.maps.find_with_default(hydra.constants.max_int32, w, st_after.low_links), hydra.topology.TarjanState(st_after.counter, st_after.indices, hydra.lib.maps.insert(v, hydra.lib.equality.min(low_v2, low_w), st_after.low_links), st_after.stack, st_after.on_stack, st_after.sccs))[1])[1])[1]), (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(w, st_.on_stack), (lambda : low_link(st_)), (lambda : st_))))
    @lru_cache(1)
    def st_after_neighbors() -> hydra.topology.TarjanState:
        return hydra.lib.lists.foldl((lambda x1, x2: process_neighbor(x1, x2)), new_st(), neighbors())
    @lru_cache(1)
    def low_v() -> int:
        return hydra.lib.maps.find_with_default(hydra.constants.max_int32, v, st_after_neighbors().low_links)
    @lru_cache(1)
    def idx_v() -> int:
        return hydra.lib.maps.find_with_default(hydra.constants.max_int32, v, st_after_neighbors().indices)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(low_v(), idx_v()), (lambda : (comp_result := pop_stack_until(v, st_after_neighbors()), (comp := hydra.lib.pairs.first(comp_result), (st_popped := hydra.lib.pairs.second(comp_result), hydra.topology.TarjanState(st_popped.counter, st_popped.indices, st_popped.low_links, st_popped.stack, st_popped.on_stack, hydra.lib.lists.cons(comp, st_popped.sccs)))[1])[1])[1]), (lambda : st_after_neighbors()))

def strongly_connected_components(graph: FrozenDict[int, frozenlist[int]]) -> frozenlist[frozenlist[int]]:
    r"""Compute the strongly connected components of the given graph. The components are returned in reverse topological order."""

    @lru_cache(1)
    def verts() -> frozenlist[int]:
        return hydra.lib.maps.keys(graph)
    @lru_cache(1)
    def final_state() -> hydra.topology.TarjanState:
        return hydra.lib.lists.foldl((lambda st, v: hydra.lib.logic.if_else(hydra.lib.maps.member(v, st.indices), (lambda : st), (lambda : strong_connect(graph, v, st)))), initial_state(), verts())
    return hydra.lib.lists.reverse(hydra.lib.lists.map((lambda x1: hydra.lib.lists.sort(x1)), final_state().sccs))
