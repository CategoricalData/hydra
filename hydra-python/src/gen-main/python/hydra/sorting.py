# Note: this is an automatically generated file. Do not edit.

r"""Utilities for sorting."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import TypeVar
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.tarjan
import hydra.topology

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def adjacency_list_to_map(pairs: frozenlist[tuple[T0, frozenlist[T1]]]) -> FrozenDict[T0, frozenlist[T1]]:
    return hydra.lib.lists.foldl((lambda mp, p: (k := hydra.lib.pairs.first(p), vs := hydra.lib.pairs.second(p), existing := hydra.lib.maybes.maybe((), (lambda x1: hydra.lib.equality.identity(x1)), hydra.lib.maps.lookup(k, mp)), hydra.lib.maps.insert(k, hydra.lib.lists.concat2(existing, vs), mp))[3]), hydra.lib.maps.empty(), pairs)

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
    def visit(visited: frozenset[T0], node: T0) -> frozenset[T0]:
        @lru_cache(1)
        def to_visit() -> frozenset[T0]:
            return hydra.lib.sets.difference(adj(node), visited)
        return hydra.lib.logic.if_else(hydra.lib.sets.null(to_visit()), (lambda : visited), (lambda : hydra.lib.lists.foldl((lambda v, n: visit(hydra.lib.sets.insert(n, v), n)), visited, hydra.lib.sets.to_list(to_visit()))))
    return visit(hydra.lib.sets.singleton(root), root)

def propagate_tags(edges: frozenlist[tuple[T0, frozenlist[T0]]], node_tags: frozenlist[tuple[T0, frozenlist[T1]]]) -> frozenlist[tuple[T0, frozenset[T1]]]:
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
            return find_reachable_nodes((lambda n: hydra.lib.sets.from_list(hydra.lib.maybes.maybe((), (lambda x1: hydra.lib.equality.identity(x1)), hydra.lib.maps.lookup(n, adj_map())))), node)
        return hydra.lib.sets.unions(hydra.lib.lists.map((lambda n: hydra.lib.maybes.maybe(hydra.lib.sets.empty(), (lambda x1: hydra.lib.equality.identity(x1)), hydra.lib.maps.lookup(n, tag_map()))), hydra.lib.sets.to_list(reachable())))
    return hydra.lib.lists.map((lambda n: (n, get_tags_for_node(n))), all_nodes())

def topological_sort_components(pairs: frozenlist[tuple[T0, frozenlist[T0]]]) -> frozenlist[frozenlist[T0]]:
    @lru_cache(1)
    def graph_result() -> tuple[FrozenDict[int, frozenlist[int]], Callable[[int], T0]]:
        return hydra.tarjan.adjacency_lists_to_graph(pairs)
    @lru_cache(1)
    def g() -> FrozenDict[int, frozenlist[int]]:
        return hydra.lib.pairs.first(graph_result())
    return hydra.lib.lists.map((lambda comp: hydra.lib.lists.map(hydra.lib.pairs.second(graph_result()), comp)), hydra.tarjan.strongly_connected_components(g()))

def topological_sort(pairs: frozenlist[tuple[T0, frozenlist[T0]]]) -> Either[frozenlist[frozenlist[T0]], frozenlist[T0]]:
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
