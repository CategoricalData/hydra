# Note: this is an automatically generated file. Do not edit.

r"""Utilities for sorting."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
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

def create_ordering_isomorphism(source_ord: frozenlist[T0], target_ord: frozenlist[T0]) -> hydra.topology.OrderingIsomorphism[T1]:
    def source_to_target_mapping(els: frozenlist[T2]) -> frozenlist[T2]:
        def mp() -> FrozenDict[T0, T2]:
            return cast(FrozenDict[T0, T2], hydra.lib.maps.from_list(hydra.lib.lists.zip(source_ord, els)))
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, mp())), target_ord))
    def target_to_source_mapping(els: frozenlist[T2]) -> frozenlist[T2]:
        def mp() -> FrozenDict[T0, T2]:
            return cast(FrozenDict[T0, T2], hydra.lib.maps.from_list(hydra.lib.lists.zip(target_ord, els)))
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, mp())), source_ord))
    return cast(hydra.topology.OrderingIsomorphism[T1], hydra.topology.OrderingIsomorphism(cast(Callable[[frozenlist[T1]], frozenlist[T1]], (lambda x1: source_to_target_mapping(x1))), cast(Callable[[frozenlist[T1]], frozenlist[T1]], (lambda x1: target_to_source_mapping(x1)))))

def find_reachable_nodes(adj: Callable[[T0], frozenset[T0]], root: T0) -> frozenset[T0]:
    def visit(visited: frozenset[T0], node: T0) -> frozenset[T0]:
        def to_visit() -> frozenset[T0]:
            return hydra.lib.sets.difference(adj(node), visited)
        return hydra.lib.logic.if_else(hydra.lib.sets.null(to_visit()), (lambda : visited), (lambda : hydra.lib.lists.foldl((lambda v, n: visit(hydra.lib.sets.insert(n, v), n)), visited, hydra.lib.sets.to_list(to_visit()))))
    return visit(hydra.lib.sets.singleton(root), root)

def topological_sort_components(pairs: frozenlist[tuple[T0, frozenlist[T0]]]) -> frozenlist[frozenlist[T0]]:
    def graph_result() -> tuple[FrozenDict[int, frozenlist[int]], Callable[[int], T0]]:
        return hydra.tarjan.adjacency_lists_to_graph(pairs)
    def g() -> FrozenDict[int, frozenlist[int]]:
        return hydra.lib.pairs.first(graph_result())
    return hydra.lib.lists.map((lambda comp: hydra.lib.lists.map(hydra.lib.pairs.second(graph_result()), comp)), hydra.tarjan.strongly_connected_components(g()))

def topological_sort(pairs: frozenlist[tuple[T0, frozenlist[T0]]]) -> Either[frozenlist[frozenlist[T0]], frozenlist[T0]]:
    def sccs() -> frozenlist[frozenlist[T0]]:
        return topological_sort_components(pairs)
    def is_cycle(scc: frozenlist[T1]) -> bool:
        return hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.lists.tail(scc)))
    def with_cycles() -> frozenlist[frozenlist[T0]]:
        return hydra.lib.lists.filter(cast(Callable[[frozenlist[T0]], bool], (lambda x1: is_cycle(x1))), sccs())
    return hydra.lib.logic.if_else(hydra.lib.lists.null(with_cycles()), (lambda : cast(Either[frozenlist[frozenlist[T0]], frozenlist[T0]], Right(hydra.lib.lists.concat(sccs())))), (lambda : cast(Either[frozenlist[frozenlist[T0]], frozenlist[T0]], Left(with_cycles()))))

def topological_sort_nodes(get_key: Callable[[T0], T1], get_adj: Callable[[T0], frozenlist[T1]], nodes: frozenlist[T0]) -> frozenlist[frozenlist[T0]]:
    def nodes_by_key() -> FrozenDict[T1, T0]:
        return cast(FrozenDict[T1, T0], hydra.lib.maps.from_list(hydra.lib.lists.map((lambda n: cast(tuple[T1, T0], (get_key(n), n))), nodes)))
    def pairs() -> frozenlist[tuple[T1, frozenlist[T1]]]:
        return hydra.lib.lists.map((lambda n: cast(tuple[T1, frozenlist[T1]], (get_key(n), get_adj(n)))), nodes)
    def comps() -> frozenlist[frozenlist[T1]]:
        return topological_sort_components(pairs())
    return hydra.lib.lists.map((lambda c: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda k: hydra.lib.maps.lookup(k, nodes_by_key())), c))), comps())
