# Note: this is an automatically generated file. Do not edit.

r"""Utilities for sorting."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import cast
import hydra.core
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.tarjan
import hydra.topology

def create_ordering_isomorphism[T0, T1](source_ord: frozenlist[T0], target_ord: frozenlist[T0]) -> hydra.topology.OrderingIsomorphism[T1]:
    def source_to_target_mapping[T2](els: frozenlist[T2]) -> frozenlist[T2]:
        mp = cast(FrozenDict[T0, T2], hydra.lib.maps.from_list(hydra.lib.lists.zip(source_ord, els)))
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, mp)), target_ord))
    def target_to_source_mapping[T2](els: frozenlist[T2]) -> frozenlist[T2]:
        mp = cast(FrozenDict[T0, T2], hydra.lib.maps.from_list(hydra.lib.lists.zip(target_ord, els)))
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, mp)), source_ord))
    return cast(hydra.topology.OrderingIsomorphism[T1], hydra.topology.OrderingIsomorphism(cast(Callable[[frozenlist[T1]], frozenlist[T1]], source_to_target_mapping), cast(Callable[[frozenlist[T1]], frozenlist[T1]], target_to_source_mapping)))

def find_reachable_nodes[T0](adj: Callable[[T0], frozenset[T0]], root: T0) -> frozenset[T0]:
    def visit(visited: frozenset[T0], node: T0) -> frozenset[T0]:
        to_visit = hydra.lib.sets.difference(adj(node), visited)
        return hydra.lib.logic.if_else(hydra.lib.sets.null(to_visit), (lambda : visited), (lambda : hydra.lib.lists.foldl((lambda v, n: visit(hydra.lib.sets.insert(n, v), n)), visited, hydra.lib.sets.to_list(to_visit))))
    return visit(hydra.lib.sets.singleton(root), root)

def topological_sort_components[T0](pairs: frozenlist[Tuple[T0, frozenlist[T0]]]) -> frozenlist[frozenlist[T0]]:
    graph_result = hydra.tarjan.adjacency_lists_to_graph(pairs)
    g = hydra.lib.pairs.first(graph_result)
    get_key = hydra.lib.pairs.second(graph_result)
    return hydra.lib.lists.map((lambda comp: hydra.lib.lists.map(get_key, comp)), hydra.tarjan.strongly_connected_components(g))

def topological_sort[T0](pairs: frozenlist[Tuple[T0, frozenlist[T0]]]) -> Either[frozenlist[frozenlist[T0]], frozenlist[T0]]:
    sccs = topological_sort_components(pairs)
    def is_cycle[T1](scc: frozenlist[T1]) -> bool:
        return hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.lists.tail(scc)))
    with_cycles = hydra.lib.lists.filter(cast(Callable[[frozenlist[T0]], bool], is_cycle), sccs)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(with_cycles), (lambda : cast(Either[frozenlist[frozenlist[T0]], frozenlist[T0]], Right(hydra.lib.lists.concat(sccs)))), (lambda : cast(Either[frozenlist[frozenlist[T0]], frozenlist[T0]], Left(with_cycles))))

def topological_sort_nodes[T0, T1, T2](get_key: T0, get_adj: Callable[[T1], frozenlist[T2]], nodes: frozenlist[T1]) -> frozenlist[frozenlist[T1]]:
    def nodes_by_key[T3]() -> FrozenDict[T3, T1]:
        return cast(FrozenDict[T3, T1], hydra.lib.maps.from_list(hydra.lib.lists.map((lambda n: cast(Tuple[T109083[t109083], T1], (get_key(n), n))), nodes)))
    def pairs[T3]() -> frozenlist[Tuple[T3, frozenlist[T2]]]:
        return hydra.lib.lists.map((lambda n: cast(Tuple[T109097[t109097], frozenlist[T2]], (get_key(n), get_adj(n)))), nodes)
    comps = topological_sort_components(cast(frozenlist[Tuple[T2, frozenlist[T2]]], pairs))
    return hydra.lib.lists.map((lambda c: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda k: hydra.lib.maps.lookup(k, cast(FrozenDict[T2, T1], nodes_by_key))), c))), comps)
