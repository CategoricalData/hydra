# Note: this is an automatically generated file. Do not edit.

r"""Utilities for sorting. This module includes an implementation of Tarjan's algorithm, originally based on GraphSCC by Iavor S. Diatchki: https://hackage.haskell.org/package/GraphSCC."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
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
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def adjacency_list_to_map(pairs: frozenlist[tuple[T0, frozenlist[T1]]]) -> FrozenDict[T0, frozenlist[T1]]:
    r"""Convert an adjacency list to a map, concatenating values for duplicate keys."""

    return hydra.lib.lists.foldl((lambda mp, p: (k := hydra.lib.pairs.first(p), vs := hydra.lib.pairs.second(p), existing := hydra.lib.maybes.maybe((lambda : ()), (lambda x1: hydra.lib.equality.identity(x1)), hydra.lib.maps.lookup(k, mp)), hydra.lib.maps.insert(k, hydra.lib.lists.concat2(existing, vs), mp))[3]), hydra.lib.maps.empty(), pairs)

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

def create_ordering_isomorphism(source_ord: frozenlist[T0], target_ord: frozenlist[T0]) -> hydra.topology.OrderingIsomorphism[T1]:
    def source_to_target_mapping(els: frozenlist[T2]) -> frozenlist[T2]:
        @lru_cache(1)
        def mp() -> FrozenDict[T0, T2]:
            return hydra.lib.maps.from_list(hydra.lib.lists.zip(source_ord, els))
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, mp())), target_ord))
    def target_to_source_mapping(els: frozenlist[T2]) -> frozenlist[T2]:
        @lru_cache(1)
        def mp() -> FrozenDict[T0, T2]:
            return hydra.lib.maps.from_list(hydra.lib.lists.zip(target_ord, els))
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, mp())), source_ord))
    return hydra.topology.OrderingIsomorphism((lambda x1: source_to_target_mapping(x1)), (lambda x1: target_to_source_mapping(x1)))

def find_reachable_nodes(adj: Callable[[T0], frozenset[T0]], root: T0) -> frozenset[T0]:
    r"""Given an adjacency function and a distinguished root node, find all reachable nodes (including the root node)."""

    def visit(visited: frozenset[T0], node: T0) -> frozenset[T0]:
        @lru_cache(1)
        def to_visit() -> frozenset[T0]:
            return hydra.lib.sets.difference(adj(node), visited)
        return hydra.lib.logic.if_else(hydra.lib.sets.null(to_visit()), (lambda : visited), (lambda : hydra.lib.lists.foldl((lambda v, n: visit(hydra.lib.sets.insert(n, v), n)), visited, hydra.lib.sets.to_list(to_visit()))))
    return visit(hydra.lib.sets.singleton(root), root)

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

def propagate_tags(edges: frozenlist[tuple[T0, frozenlist[T0]]], node_tags: frozenlist[tuple[T0, frozenlist[T1]]]) -> frozenlist[tuple[T0, frozenset[T1]]]:
    r"""Given a graph as an adjacency list of edges and a list of explicit tags per node, compute the full set of tags for each node by propagating tags through edges. If there is an edge from n1 to n2 and n2 has tag t, then n1 also has tag t. Note: pairs in the output are not ordered."""

    @lru_cache(1)
    def adj_map() -> FrozenDict[T0, frozenlist[T0]]:
        return adjacency_list_to_map(edges)
    @lru_cache(1)
    def tag_map() -> FrozenDict[T0, frozenset[T1]]:
        return hydra.lib.maps.map((lambda x1: hydra.lib.sets.from_list(x1)), adjacency_list_to_map(node_tags))
    @lru_cache(1)
    def all_nodes() -> frozenlist[T0]:
        return hydra.lib.sets.to_list(hydra.lib.sets.from_list(hydra.lib.lists.concat2(hydra.lib.lists.map((lambda x1: hydra.lib.pairs.first(x1)), edges), hydra.lib.lists.map((lambda x1: hydra.lib.pairs.first(x1)), node_tags))))
    def get_tags_for_node(node: T0) -> frozenset[T1]:
        @lru_cache(1)
        def reachable() -> frozenset[T0]:
            return find_reachable_nodes((lambda n: hydra.lib.sets.from_list(hydra.lib.maybes.maybe((lambda : ()), (lambda x1: hydra.lib.equality.identity(x1)), hydra.lib.maps.lookup(n, adj_map())))), node)
        return hydra.lib.sets.unions(hydra.lib.lists.map((lambda n: hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda x1: hydra.lib.equality.identity(x1)), hydra.lib.maps.lookup(n, tag_map()))), hydra.lib.sets.to_list(reachable())))
    return hydra.lib.lists.map((lambda n: (n, get_tags_for_node(n))), all_nodes())

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

def topological_sort_components(pairs: frozenlist[tuple[T0, frozenlist[T0]]]) -> frozenlist[frozenlist[T0]]:
    r"""Find the strongly connected components (including cycles and isolated vertices) of a graph, in (reverse) topological order, i.e. dependencies before dependents."""

    @lru_cache(1)
    def graph_result() -> tuple[FrozenDict[int, frozenlist[int]], Callable[[int], T0]]:
        return adjacency_lists_to_graph(pairs)
    @lru_cache(1)
    def g() -> FrozenDict[int, frozenlist[int]]:
        return hydra.lib.pairs.first(graph_result())
    return hydra.lib.lists.map((lambda comp: hydra.lib.lists.map(hydra.lib.pairs.second(graph_result()), comp)), strongly_connected_components(g()))

def topological_sort(pairs: frozenlist[tuple[T0, frozenlist[T0]]]) -> Either[frozenlist[frozenlist[T0]], frozenlist[T0]]:
    r"""Sort a directed acyclic graph (DAG) based on an adjacency list. Yields a list of nontrivial strongly connected components if the graph has cycles, otherwise a simple list."""

    @lru_cache(1)
    def sccs() -> frozenlist[frozenlist[T0]]:
        return topological_sort_components(pairs)
    def is_cycle(scc: frozenlist[T1]) -> bool:
        return hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.lists.tail(scc)))
    @lru_cache(1)
    def with_cycles() -> frozenlist[frozenlist[T0]]:
        return hydra.lib.lists.filter((lambda x1: is_cycle(x1)), sccs())
    return hydra.lib.logic.if_else(hydra.lib.lists.null(with_cycles()), (lambda : Right(hydra.lib.lists.concat(sccs()))), (lambda : Left(with_cycles())))

def topological_sort_nodes(get_key: Callable[[T0], T1], get_adj: Callable[[T0], frozenlist[T1]], nodes: frozenlist[T0]) -> frozenlist[frozenlist[T0]]:
    r"""Sort a directed acyclic graph (DAG) of nodes using two helper functions: one for node keys, and one for the adjacency list of connected node keys. The result is a list of strongly-connected components (cycles), in which singleton lists represent acyclic nodes."""

    @lru_cache(1)
    def nodes_by_key() -> FrozenDict[T1, T0]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda n: (get_key(n), n)), nodes))
    @lru_cache(1)
    def pairs() -> frozenlist[tuple[T1, frozenlist[T1]]]:
        return hydra.lib.lists.map((lambda n: (get_key(n), get_adj(n))), nodes)
    @lru_cache(1)
    def comps() -> frozenlist[frozenlist[T1]]:
        return topological_sort_components(pairs())
    return hydra.lib.lists.map((lambda c: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda k: hydra.lib.maps.lookup(k, nodes_by_key())), c))), comps())
