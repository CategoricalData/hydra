# Note: this is an automatically generated file. Do not edit.

r"""Utilities for working with subterm steps and paths."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.paths
import hydra.rewriting

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def subterm_step(step: hydra.paths.SubtermStep) -> Maybe[str]:
    r"""Convert a subterm step to a string representation."""

    def idx(i: T0) -> Maybe[T1]:
        return Nothing()
    def idx_suff(suffix: str, i: T0) -> Maybe[str]:
        return hydra.lib.maybes.map((lambda s: hydra.lib.strings.cat2(s, suffix)), idx(i))
    match step:
        case hydra.paths.SubtermStepAnnotatedBody():
            return Nothing()

        case hydra.paths.SubtermStepApplicationFunction():
            return Just("fun")

        case hydra.paths.SubtermStepApplicationArgument():
            return Just("arg")

        case hydra.paths.SubtermStepLambdaBody():
            return Just("body")

        case hydra.paths.SubtermStepUnionCasesDefault():
            return Just("default")

        case hydra.paths.SubtermStepUnionCasesBranch(value=name):
            return Just(hydra.lib.strings.cat2(".", name.value))

        case hydra.paths.SubtermStepLetBody():
            return Just("in")

        case hydra.paths.SubtermStepLetBinding(value=name2):
            return Just(hydra.lib.strings.cat2(name2.value, "="))

        case hydra.paths.SubtermStepListElement(value=i):
            return idx(i)

        case hydra.paths.SubtermStepMapKey(value=i2):
            return idx_suff(".key", i2)

        case hydra.paths.SubtermStepMapValue(value=i3):
            return idx_suff(".value", i3)

        case hydra.paths.SubtermStepMaybeTerm():
            return Just("just")

        case hydra.paths.SubtermStepProductTerm(value=i4):
            return idx(i4)

        case hydra.paths.SubtermStepRecordField(value=name3):
            return Just(hydra.lib.strings.cat2(".", name3.value))

        case hydra.paths.SubtermStepSetElement(value=i5):
            return idx(i5)

        case hydra.paths.SubtermStepSumTerm():
            return Nothing()

        case hydra.paths.SubtermStepTypeLambdaBody():
            return Nothing()

        case hydra.paths.SubtermStepTypeApplicationTerm():
            return Nothing()

        case hydra.paths.SubtermStepInjectionTerm():
            return Nothing()

        case hydra.paths.SubtermStepWrappedTerm():
            return Nothing()

        case _:
            raise AssertionError("Unreachable: all variants handled")

def term_to_subterm_graph(namespaces: FrozenDict[hydra.packaging.Namespace, str], term: hydra.core.Term) -> hydra.paths.SubtermGraph:
    r"""Build a subterm graph from a term."""

    dont_care_step = cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepAnnotatedBody())
    def helper(ids: FrozenDict[hydra.core.Name, hydra.paths.SubtermNode], mroot: Maybe[hydra.paths.SubtermNode], path: frozenlist[hydra.paths.SubtermStep], state: tuple[tuple[frozenlist[hydra.paths.SubtermNode], frozenlist[hydra.paths.SubtermEdge]], frozenset[str]], step_term: tuple[hydra.paths.SubtermStep, hydra.core.Term]) -> tuple[tuple[frozenlist[hydra.paths.SubtermNode], frozenlist[hydra.paths.SubtermEdge]], frozenset[str]]:
        @lru_cache(1)
        def step() -> hydra.paths.SubtermStep:
            return hydra.lib.pairs.first(step_term)
        @lru_cache(1)
        def current_term() -> hydra.core.Term:
            return hydra.lib.pairs.second(step_term)
        @lru_cache(1)
        def nodes_edges() -> tuple[frozenlist[hydra.paths.SubtermNode], frozenlist[hydra.paths.SubtermEdge]]:
            return hydra.lib.pairs.first(state)
        @lru_cache(1)
        def visited() -> frozenset[str]:
            return hydra.lib.pairs.second(state)
        @lru_cache(1)
        def nodes() -> frozenlist[hydra.paths.SubtermNode]:
            return hydra.lib.pairs.first(nodes_edges())
        @lru_cache(1)
        def edges() -> frozenlist[hydra.paths.SubtermEdge]:
            return hydra.lib.pairs.second(nodes_edges())
        @lru_cache(1)
        def next_path() -> frozenlist[hydra.paths.SubtermStep]:
            return hydra.lib.lists.cons(step(), path)
        match current_term():
            case hydra.core.TermLet(value=let_expr):
                bindings = let_expr.bindings
                env = let_expr.body
                @lru_cache(1)
                def binding_names() -> frozenlist[hydra.core.Name]:
                    return hydra.lib.lists.map((lambda v1: v1.name), bindings)
                def add_binding_name(nodes_visited_ids: tuple[tuple[frozenlist[hydra.paths.SubtermNode], frozenset[str]], FrozenDict[hydra.core.Name, hydra.paths.SubtermNode]], name: hydra.core.Name) -> tuple[tuple[frozenlist[hydra.paths.SubtermNode], frozenset[str]], FrozenDict[hydra.core.Name, hydra.paths.SubtermNode]]:
                    @lru_cache(1)
                    def current_nodes_visited() -> tuple[frozenlist[hydra.paths.SubtermNode], frozenset[str]]:
                        return hydra.lib.pairs.first(nodes_visited_ids)
                    @lru_cache(1)
                    def current_ids() -> FrozenDict[hydra.core.Name, hydra.paths.SubtermNode]:
                        return hydra.lib.pairs.second(nodes_visited_ids)
                    @lru_cache(1)
                    def current_nodes() -> frozenlist[hydra.paths.SubtermNode]:
                        return hydra.lib.pairs.first(current_nodes_visited())
                    @lru_cache(1)
                    def current_visited() -> frozenset[str]:
                        return hydra.lib.pairs.second(current_nodes_visited())
                    @lru_cache(1)
                    def raw_label() -> str:
                        return hydra.names.compact_name(namespaces, name)
                    @lru_cache(1)
                    def unique_label() -> str:
                        return hydra.names.unique_label(current_visited(), raw_label())
                    node = hydra.paths.SubtermNode(name, raw_label(), unique_label())
                    @lru_cache(1)
                    def new_visited() -> frozenset[str]:
                        return hydra.lib.sets.insert(unique_label(), current_visited())
                    @lru_cache(1)
                    def new_nodes() -> frozenlist[hydra.paths.SubtermNode]:
                        return hydra.lib.lists.cons(node, current_nodes())
                    @lru_cache(1)
                    def new_ids() -> FrozenDict[hydra.core.Name, hydra.paths.SubtermNode]:
                        return hydra.lib.maps.insert(name, node, current_ids())
                    return ((new_nodes(), new_visited()), new_ids())
                @lru_cache(1)
                def nodes_visited_ids1() -> tuple[tuple[frozenlist[hydra.paths.SubtermNode], frozenset[str]], FrozenDict[hydra.core.Name, hydra.paths.SubtermNode]]:
                    return hydra.lib.lists.foldl((lambda x1, x2: add_binding_name(x1, x2)), (((), visited()), ids), binding_names())
                @lru_cache(1)
                def nodes1() -> frozenlist[hydra.paths.SubtermNode]:
                    return hydra.lib.pairs.first(hydra.lib.pairs.first(nodes_visited_ids1()))
                @lru_cache(1)
                def visited1() -> frozenset[str]:
                    return hydra.lib.pairs.second(hydra.lib.pairs.first(nodes_visited_ids1()))
                @lru_cache(1)
                def ids1() -> FrozenDict[hydra.core.Name, hydra.paths.SubtermNode]:
                    return hydra.lib.pairs.second(nodes_visited_ids1())
                def add_binding_term(current_state: tuple[tuple[frozenlist[hydra.paths.SubtermNode], frozenlist[hydra.paths.SubtermEdge]], frozenset[str]], node_binding: tuple[hydra.paths.SubtermNode, hydra.core.Binding]) -> tuple[tuple[frozenlist[hydra.paths.SubtermNode], frozenlist[hydra.paths.SubtermEdge]], frozenset[str]]:
                    @lru_cache(1)
                    def root() -> hydra.paths.SubtermNode:
                        return hydra.lib.pairs.first(node_binding)
                    @lru_cache(1)
                    def binding() -> hydra.core.Binding:
                        return hydra.lib.pairs.second(node_binding)
                    term1 = binding().term
                    return helper(ids1(), Just(root()), (), current_state, (dont_care_step, term1))
                @lru_cache(1)
                def node_binding_pairs() -> frozenlist[tuple[hydra.paths.SubtermNode, hydra.core.Binding]]:
                    return hydra.lib.lists.zip(nodes1(), bindings)
                @lru_cache(1)
                def state_after_bindings() -> tuple[tuple[frozenlist[hydra.paths.SubtermNode], frozenlist[hydra.paths.SubtermEdge]], frozenset[str]]:
                    return hydra.lib.lists.foldl((lambda x1, x2: add_binding_term(x1, x2)), ((hydra.lib.lists.concat2(nodes1(), nodes()), edges()), visited1()), node_binding_pairs())
                return helper(ids1(), mroot, next_path(), state_after_bindings(), (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepLetBody()), env))

            case hydra.core.TermVariable(value=name):
                return hydra.lib.maybes.maybe((lambda : state), (lambda root: hydra.lib.maybes.maybe((lambda : state), (lambda node: (edge := hydra.paths.SubtermEdge(root, hydra.paths.SubtermPath(hydra.lib.lists.reverse(next_path())), node), new_edges := hydra.lib.lists.cons(edge, edges()), ((nodes(), new_edges), visited()))[2]), hydra.lib.maps.lookup(name, ids))), mroot)

            case _:
                return hydra.lib.lists.foldl((lambda v1, v2: helper(ids, mroot, next_path(), v1, v2)), state, hydra.rewriting.subterms_with_steps(current_term()))
    @lru_cache(1)
    def initial_state() -> tuple[tuple[frozenlist[T0], frozenlist[T1]], frozenset[T2]]:
        return (((), ()), hydra.lib.sets.empty())
    @lru_cache(1)
    def result() -> tuple[tuple[frozenlist[hydra.paths.SubtermNode], frozenlist[hydra.paths.SubtermEdge]], frozenset[str]]:
        return helper(hydra.lib.maps.empty(), Nothing(), (), initial_state(), (dont_care_step, term))
    @lru_cache(1)
    def final_nodes_edges() -> tuple[frozenlist[hydra.paths.SubtermNode], frozenlist[hydra.paths.SubtermEdge]]:
        return hydra.lib.pairs.first(result())
    @lru_cache(1)
    def final_nodes() -> frozenlist[hydra.paths.SubtermNode]:
        return hydra.lib.pairs.first(final_nodes_edges())
    @lru_cache(1)
    def final_edges() -> frozenlist[hydra.paths.SubtermEdge]:
        return hydra.lib.pairs.second(final_nodes_edges())
    return hydra.paths.SubtermGraph(final_nodes(), final_edges())
