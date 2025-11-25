# Note: this is an automatically generated file. Do not edit.

r"""Utilities for sorting."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import Tuple, cast
import hydra.core
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
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
        return hydra.lib.logic.if_else(hydra.lib.sets.null(to_visit), visited, hydra.lib.lists.foldl((lambda v, n: visit(hydra.lib.sets.insert(n, v), n)), visited, hydra.lib.sets.to_list(to_visit)))
    return visit(hydra.lib.sets.singleton(root), root)

def topological_sort_components[T0](pairs: frozenlist[Tuple[T0, frozenlist[T0]]]) -> frozenlist[frozenlist[T0]]:
    graph_result = hydra.tarjan.adjacency_lists_to_graph(pairs)
    g = graph_result[0]
    get_key = graph_result[1]
    return hydra.lib.lists.map((lambda comp: hydra.lib.lists.map(get_key, comp)), hydra.tarjan.strongly_connected_components(g))

def topological_sort[T0](pairs: frozenlist[Tuple[T0, frozenlist[T0]]]) -> Either[frozenlist[frozenlist[T0]], frozenlist[T0]]:
    sccs = topological_sort_components(pairs)
    def is_cycle[T1](scc: frozenlist[T1]) -> bool:
        return hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.lists.tail(scc)))
    with_cycles = hydra.lib.lists.filter(cast(Callable[[frozenlist[T0]], bool], is_cycle), sccs)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(with_cycles), cast(Either[frozenlist[frozenlist[T0]], frozenlist[T0]], Right(hydra.lib.lists.concat(sccs))), cast(Either[frozenlist[frozenlist[T0]], frozenlist[T0]], Left(with_cycles)))

def topological_sort_nodes[T0, T1](get_key: Callable[[T0], T1], get_adj: Callable[[T0], frozenlist[T1]], nodes: frozenlist[T0]) -> frozenlist[frozenlist[T0]]:
    nodes_by_key = cast(FrozenDict[T1, T0], hydra.lib.maps.from_list(hydra.lib.lists.map((lambda n: (get_key(n), n)), nodes)))
    pairs = hydra.lib.lists.map((lambda n: (get_key(n), get_adj(n))), nodes)
    comps = topological_sort_components(pairs)
    return hydra.lib.lists.map((lambda c: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda k: hydra.lib.maps.lookup(k, nodes_by_key)), c))), comps)
