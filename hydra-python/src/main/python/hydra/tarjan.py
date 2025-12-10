# Note: this is an automatically generated file. Do not edit.

r"""This implementation of Tarjan's algorithm was originally based on GraphSCC by Iavor S. Diatchki: https://hackage.haskell.org/package/GraphSCC."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict, frozenlist
from typing import cast
import hydra.compute
import hydra.constants
import hydra.core
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.monads
import hydra.topology

def adjacency_lists_to_graph[T0](edges0: frozenlist[tuple[T0, frozenlist[T0]]]) -> tuple[FrozenDict[int, frozenlist[int]], Callable[[int], T0]]:
    sorted_edges = hydra.lib.lists.sort_on(cast(Callable[[tuple[T0, frozenlist[T0]]], T0], hydra.lib.pairs.first), edges0)
    indexed_edges = hydra.lib.lists.zip(hydra.lib.math.range_(0, hydra.lib.lists.length(sorted_edges)), sorted_edges)
    key_to_vertex = cast(FrozenDict[T0, int], hydra.lib.maps.from_list(hydra.lib.lists.map((lambda vk_neighbors: (v := hydra.lib.pairs.first(vk_neighbors), k_neighbors := hydra.lib.pairs.second(vk_neighbors), k := hydra.lib.pairs.first(k_neighbors), cast(tuple[T0, int], (k, v)))[3]), indexed_edges)))
    vertex_map = cast(FrozenDict[int, T0], hydra.lib.maps.from_list(hydra.lib.lists.map((lambda vk_neighbors: (v := hydra.lib.pairs.first(vk_neighbors), k_neighbors := hydra.lib.pairs.second(vk_neighbors), k := hydra.lib.pairs.first(k_neighbors), cast(tuple[int, T0], (v, k)))[3]), indexed_edges)))
    graph = cast(FrozenDict[int, frozenlist[int]], hydra.lib.maps.from_list(hydra.lib.lists.map((lambda vk_neighbors: (v := hydra.lib.pairs.first(vk_neighbors), k_neighbors := hydra.lib.pairs.second(vk_neighbors), neighbors := hydra.lib.pairs.second(k_neighbors), cast(tuple[int, frozenlist[int]], (v, hydra.lib.maybes.map_maybe((lambda k: hydra.lib.maps.lookup(k, key_to_vertex)), neighbors))))[3]), indexed_edges)))
    def vertex_to_key(v: int) -> T0:
        return hydra.lib.maybes.from_just(hydra.lib.maps.lookup(v, vertex_map))
    return cast(tuple[FrozenDict[int, frozenlist[int]], Callable[[int], T0]], (graph, vertex_to_key))

# Initial state for Tarjan's algorithm.
initial_state = hydra.topology.TarjanState(0, cast(FrozenDict[int, int], hydra.lib.maps.empty()), cast(FrozenDict[int, int], hydra.lib.maps.empty()), cast(frozenlist[int], ()), cast(frozenset[int], hydra.lib.sets.empty()), cast(frozenlist[frozenlist[int]], ()))

def pop_stack_until(v: int) -> hydra.compute.Flow[hydra.topology.TarjanState, frozenlist[int]]:
    r"""Pop vertices off the stack until the given vertex is reached, collecting the current strongly connected component."""
    
    def go(acc: frozenlist[int]) -> hydra.compute.Flow[hydra.topology.TarjanState, frozenlist[int]]:
        def succeed(st: hydra.topology.TarjanState) -> hydra.compute.Flow[hydra.topology.TarjanState, frozenlist[int]]:
            x = hydra.lib.lists.head(st.stack)
            xs = hydra.lib.lists.tail(st.stack)
            new_st = hydra.topology.TarjanState(st.counter, st.indices, st.low_links, xs, st.on_stack, st.sccs)
            new_st2 = hydra.topology.TarjanState(new_st.counter, new_st.indices, new_st.low_links, new_st.stack, hydra.lib.sets.delete(x, st.on_stack), new_st.sccs)
            acc_ = hydra.lib.lists.cons(x, acc)
            return hydra.lib.flows.bind(hydra.monads.put_state(new_st2), (lambda _: hydra.lib.logic.if_else(hydra.lib.equality.equal(x, v), (lambda : hydra.lib.flows.pure(hydra.lib.lists.reverse(acc_))), (lambda : go(acc_)))))
        return hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.topology.TarjanState, hydra.topology.TarjanState], hydra.monads.get_state()), (lambda st: hydra.lib.logic.if_else(hydra.lib.lists.null(st.stack), (lambda : hydra.lib.flows.fail("popStackUntil: empty stack")), (lambda : succeed(st)))))
    return go(cast(frozenlist[int], ()))

def strong_connect(graph: FrozenDict[int, frozenlist[int]], v: int) -> hydra.compute.Flow[hydra.topology.TarjanState, None]:
    r"""Visit a vertex and recursively explore its successors."""
    
    return hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.topology.TarjanState, hydra.topology.TarjanState], hydra.monads.get_state()), (lambda st: (i := st.counter, new_st := hydra.topology.TarjanState(hydra.lib.math.add(i, 1), hydra.lib.maps.insert(v, i, st.indices), hydra.lib.maps.insert(v, i, st.low_links), hydra.lib.lists.cons(v, st.stack), hydra.lib.sets.insert(v, st.on_stack), st.sccs), neighbors := hydra.lib.maps.find_with_default(cast(frozenlist[int], ()), v, graph), process_neighbor := (lambda w: (low_link := (lambda st_: (low_v := hydra.lib.maps.find_with_default(hydra.constants.max_int32, v, st_.low_links), idx_w := hydra.lib.maps.find_with_default(hydra.constants.max_int32, w, st_.indices), hydra.lib.flows.bind(hydra.monads.modify((lambda s: hydra.topology.TarjanState(s.counter, s.indices, hydra.lib.maps.insert(v, hydra.lib.equality.min(low_v, idx_w), s.low_links), s.stack, s.on_stack, s.sccs))), (lambda _: hydra.lib.flows.pure(None))))[2]), hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.topology.TarjanState, hydra.topology.TarjanState], hydra.monads.get_state()), (lambda st_: hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.maps.member(w, st_.indices)), (lambda : hydra.lib.flows.bind(strong_connect(graph, w), (lambda _: hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.topology.TarjanState, hydra.topology.TarjanState], hydra.monads.get_state()), (lambda st_after: (low_v := hydra.lib.maps.find_with_default(hydra.constants.max_int32, v, st_after.low_links), low_w := hydra.lib.maps.find_with_default(hydra.constants.max_int32, w, st_after.low_links), hydra.lib.flows.bind(hydra.monads.modify((lambda s: hydra.topology.TarjanState(s.counter, s.indices, hydra.lib.maps.insert(v, hydra.lib.equality.min(low_v, low_w), s.low_links), s.stack, s.on_stack, s.sccs))), (lambda _2: hydra.lib.flows.pure(None))))[2]))))), (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(w, st_.on_stack), (lambda : low_link(st_)), (lambda : hydra.lib.flows.pure(None))))))))[1]), hydra.lib.flows.bind(hydra.monads.put_state(new_st), (lambda _: hydra.lib.flows.bind(hydra.lib.flows.map_list(process_neighbor, neighbors), (lambda _2: hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.topology.TarjanState, hydra.topology.TarjanState], hydra.monads.get_state()), (lambda st_final: (low_v := hydra.lib.maps.find_with_default(hydra.constants.max_int32, v, st_final.low_links), idx_v := hydra.lib.maps.find_with_default(hydra.constants.max_int32, v, st_final.indices), hydra.lib.logic.if_else(hydra.lib.equality.equal(low_v, idx_v), (lambda : hydra.lib.flows.bind(pop_stack_until(v), (lambda comp: hydra.lib.flows.bind(hydra.monads.modify((lambda s: hydra.topology.TarjanState(s.counter, s.indices, s.low_links, s.stack, s.on_stack, hydra.lib.lists.cons(comp, s.sccs)))), (lambda _3: hydra.lib.flows.pure(None)))))), (lambda : hydra.lib.flows.pure(None))))[2])))))))[4]))

def strongly_connected_components(graph: FrozenDict[int, frozenlist[int]]) -> frozenlist[frozenlist[int]]:
    r"""Compute the strongly connected components of the given graph. The components are returned in reverse topological order."""
    
    verts = hydra.lib.maps.keys(graph)
    def process_vertex(v: int) -> hydra.compute.Flow[hydra.topology.TarjanState, None]:
        return hydra.lib.flows.bind(hydra.lib.flows.map((lambda st: hydra.lib.maps.member(v, st.indices)), cast(hydra.compute.Flow[hydra.topology.TarjanState, hydra.topology.TarjanState], hydra.monads.get_state())), (lambda visited: hydra.lib.logic.if_else(hydra.lib.logic.not_(visited), (lambda : strong_connect(graph, v)), (lambda : hydra.lib.flows.pure(None)))))
    final_state = hydra.monads.exec(hydra.lib.flows.map_list(process_vertex, verts), initial_state)
    return hydra.lib.lists.reverse(hydra.lib.lists.map(cast(Callable[[frozenlist[int]], frozenlist[int]], hydra.lib.lists.sort), final_state.sccs))
