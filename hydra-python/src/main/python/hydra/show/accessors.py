# Note: this is an automatically generated file. Do not edit.

r"""Utilities for working with term accessors."""

from __future__ import annotations
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.accessors
import hydra.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.names
import hydra.rewriting

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def term_accessor(accessor: hydra.accessors.TermAccessor) -> Maybe[str]:
    r"""Convert a term accessor to a string representation."""
    
    def idx(i: T0) -> Maybe[T1]:
        return cast(Maybe[T1], Nothing())
    def idx_suff(suffix: str, i: T0) -> Maybe[str]:
        return hydra.lib.maybes.map((lambda s: hydra.lib.strings.cat2(s, suffix)), idx(i))
    match accessor:
        case hydra.accessors.TermAccessorAnnotatedBody():
            return cast(Maybe[str], Nothing())
        
        case hydra.accessors.TermAccessorApplicationFunction():
            return cast(Maybe[str], Just("fun"))
        
        case hydra.accessors.TermAccessorApplicationArgument():
            return cast(Maybe[str], Just("arg"))
        
        case hydra.accessors.TermAccessorLambdaBody():
            return cast(Maybe[str], Just("body"))
        
        case hydra.accessors.TermAccessorUnionCasesDefault():
            return cast(Maybe[str], Just("default"))
        
        case hydra.accessors.TermAccessorUnionCasesBranch(value=name):
            return cast(Maybe[str], Just(hydra.lib.strings.cat2(".", name.value)))
        
        case hydra.accessors.TermAccessorLetBody():
            return cast(Maybe[str], Just("in"))
        
        case hydra.accessors.TermAccessorLetBinding(value=name2):
            return cast(Maybe[str], Just(hydra.lib.strings.cat2(name2.value, "=")))
        
        case hydra.accessors.TermAccessorListElement(value=i):
            return idx(i)
        
        case hydra.accessors.TermAccessorMapKey(value=i2):
            return idx_suff(".key", i2)
        
        case hydra.accessors.TermAccessorMapValue(value=i3):
            return idx_suff(".value", i3)
        
        case hydra.accessors.TermAccessorMaybeTerm():
            return cast(Maybe[str], Just("just"))
        
        case hydra.accessors.TermAccessorProductTerm(value=i4):
            return idx(i4)
        
        case hydra.accessors.TermAccessorRecordField(value=name3):
            return cast(Maybe[str], Just(hydra.lib.strings.cat2(".", name3.value)))
        
        case hydra.accessors.TermAccessorSetElement(value=i5):
            return idx(i5)
        
        case hydra.accessors.TermAccessorSumTerm():
            return cast(Maybe[str], Nothing())
        
        case hydra.accessors.TermAccessorTypeLambdaBody():
            return cast(Maybe[str], Nothing())
        
        case hydra.accessors.TermAccessorTypeApplicationTerm():
            return cast(Maybe[str], Nothing())
        
        case hydra.accessors.TermAccessorInjectionTerm():
            return cast(Maybe[str], Nothing())
        
        case hydra.accessors.TermAccessorWrappedTerm():
            return cast(Maybe[str], Nothing())
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def term_to_accessor_graph(namespaces: FrozenDict[hydra.module.Namespace, str], term: hydra.core.Term) -> hydra.core.Type:
    r"""Build an accessor graph from a term."""
    
    dont_care_accessor = cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorAnnotatedBody())
    def helper(ids: FrozenDict[hydra.core.Name, hydra.accessors.AccessorNode], mroot: Maybe[hydra.accessors.AccessorNode], path: frozenlist[hydra.accessors.TermAccessor], state: tuple[tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]], frozenset[str]], accessor_term: tuple[hydra.accessors.TermAccessor, hydra.core.Term]) -> tuple[tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]], frozenset[str]]:
        def accessor() -> hydra.core.Type:
            return hydra.lib.pairs.first(accessor_term)
        def current_term() -> hydra.core.Type:
            return hydra.lib.pairs.second(accessor_term)
        def nodes_edges() -> tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]]:
            return hydra.lib.pairs.first(state)
        def visited() -> frozenset[str]:
            return hydra.lib.pairs.second(state)
        def nodes() -> frozenlist[hydra.accessors.AccessorNode]:
            return hydra.lib.pairs.first(nodes_edges())
        def edges() -> frozenlist[hydra.accessors.AccessorEdge]:
            return hydra.lib.pairs.second(nodes_edges())
        def next_path() -> frozenlist[hydra.accessors.TermAccessor]:
            return hydra.lib.lists.cons(accessor(), path)
        match current_term():
            case hydra.core.TermLet(value=let_expr):
                bindings = let_expr.bindings
                env = let_expr.body
                def binding_names() -> frozenlist[hydra.core.Name]:
                    return hydra.lib.lists.map((lambda v1: v1.name), bindings)
                def add_binding_name(nodes_visited_ids: tuple[tuple[frozenlist[hydra.accessors.AccessorNode], frozenset[str]], FrozenDict[hydra.core.Name, hydra.accessors.AccessorNode]], name: hydra.core.Name) -> tuple[tuple[frozenlist[hydra.accessors.AccessorNode], frozenset[str]], FrozenDict[hydra.core.Name, hydra.accessors.AccessorNode]]:
                    def current_nodes_visited() -> tuple[frozenlist[hydra.accessors.AccessorNode], frozenset[str]]:
                        return hydra.lib.pairs.first(nodes_visited_ids)
                    def current_ids() -> FrozenDict[hydra.core.Name, hydra.accessors.AccessorNode]:
                        return hydra.lib.pairs.second(nodes_visited_ids)
                    def current_nodes() -> frozenlist[hydra.accessors.AccessorNode]:
                        return hydra.lib.pairs.first(current_nodes_visited())
                    def current_visited() -> frozenset[str]:
                        return hydra.lib.pairs.second(current_nodes_visited())
                    raw_label = hydra.names.compact_name(namespaces, name)
                    def unique_label() -> str:
                        return hydra.names.unique_label(current_visited(), raw_label)
                    def node() -> hydra.core.Type:
                        return hydra.accessors.AccessorNode(name, raw_label, unique_label())
                    def new_visited() -> frozenset[str]:
                        return hydra.lib.sets.insert(unique_label(), current_visited())
                    def new_nodes() -> frozenlist[hydra.accessors.AccessorNode]:
                        return hydra.lib.lists.cons(node(), current_nodes())
                    def new_ids() -> FrozenDict[hydra.core.Name, hydra.accessors.AccessorNode]:
                        return hydra.lib.maps.insert(name, node(), current_ids())
                    return cast(tuple[tuple[frozenlist[hydra.accessors.AccessorNode], frozenset[str]], FrozenDict[hydra.core.Name, hydra.accessors.AccessorNode]], (cast(tuple[frozenlist[hydra.accessors.AccessorNode], frozenset[str]], (new_nodes(), new_visited())), new_ids()))
                def nodes_visited_ids1() -> tuple[tuple[frozenlist[hydra.accessors.AccessorNode], frozenset[str]], FrozenDict[hydra.core.Name, hydra.accessors.AccessorNode]]:
                    return hydra.lib.lists.foldl(add_binding_name, cast(tuple[tuple[frozenlist[hydra.accessors.AccessorNode], frozenset[str]], FrozenDict[hydra.core.Name, hydra.accessors.AccessorNode]], (cast(tuple[frozenlist[hydra.accessors.AccessorNode], frozenset[str]], (cast(frozenlist[hydra.accessors.AccessorNode], ()), visited())), ids)), binding_names())
                def nodes1() -> frozenlist[hydra.accessors.AccessorNode]:
                    return hydra.lib.pairs.first(hydra.lib.pairs.first(nodes_visited_ids1()))
                def visited1() -> frozenset[str]:
                    return hydra.lib.pairs.second(hydra.lib.pairs.first(nodes_visited_ids1()))
                def ids1() -> FrozenDict[hydra.core.Name, hydra.accessors.AccessorNode]:
                    return hydra.lib.pairs.second(nodes_visited_ids1())
                def add_binding_term(current_state: tuple[tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]], frozenset[str]], node_binding: tuple[hydra.accessors.AccessorNode, hydra.core.Binding]) -> tuple[tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]], frozenset[str]]:
                    def root() -> hydra.core.Type:
                        return hydra.lib.pairs.first(node_binding)
                    def binding() -> hydra.core.Type:
                        return hydra.lib.pairs.second(node_binding)
                    def term1() -> hydra.core.Type:
                        return binding().term
                    return helper(ids1(), cast(Maybe[hydra.accessors.AccessorNode], Just(root())), cast(frozenlist[hydra.accessors.TermAccessor], ()), current_state, cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (dont_care_accessor, term1())))
                def node_binding_pairs() -> frozenlist[tuple[hydra.accessors.AccessorNode, hydra.core.Binding]]:
                    return hydra.lib.lists.zip(nodes1(), bindings)
                def state_after_bindings() -> tuple[tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]], frozenset[str]]:
                    return hydra.lib.lists.foldl(add_binding_term, cast(tuple[tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]], frozenset[str]], (cast(tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]], (hydra.lib.lists.concat2(nodes1(), nodes()), edges())), visited1())), node_binding_pairs())
                return helper(ids1(), mroot, next_path(), state_after_bindings(), cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLetBody()), env)))
            
            case hydra.core.TermVariable(value=name):
                return hydra.lib.maybes.maybe(state, (lambda root: hydra.lib.maybes.maybe(state, (lambda node: (edge := (lambda : hydra.accessors.AccessorEdge(root, hydra.accessors.AccessorPath(hydra.lib.lists.reverse(next_path())), node)), new_edges := (lambda : hydra.lib.lists.cons(edge(), edges())), cast(tuple[tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]], frozenset[str]], (cast(tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]], (nodes(), new_edges())), visited())))[2]), hydra.lib.maps.lookup(name, ids))), mroot)
            
            case _:
                return hydra.lib.lists.foldl((lambda v1, v2: helper(ids, mroot, next_path(), v1, v2)), state, hydra.rewriting.subterms_with_accessors(current_term()))
    def initial_state() -> tuple[tuple[frozenlist[T0], frozenlist[T1]], frozenset[T2]]:
        return cast(tuple[tuple[frozenlist[T0], frozenlist[T1]], frozenset[T2]], (cast(tuple[frozenlist[T0], frozenlist[T1]], (cast(frozenlist[T0], ()), cast(frozenlist[T1], ()))), cast(frozenset[T2], hydra.lib.sets.empty())))
    def result() -> tuple[tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]], frozenset[str]]:
        return helper(cast(FrozenDict[hydra.core.Name, hydra.accessors.AccessorNode], hydra.lib.maps.empty()), cast(Maybe[hydra.accessors.AccessorNode], Nothing()), cast(frozenlist[hydra.accessors.TermAccessor], ()), cast(tuple[tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]], frozenset[str]], initial_state()), cast(tuple[hydra.accessors.TermAccessor, hydra.core.Term], (dont_care_accessor, term)))
    def final_nodes_edges() -> tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]]:
        return hydra.lib.pairs.first(result())
    def final_nodes() -> frozenlist[hydra.accessors.AccessorNode]:
        return hydra.lib.pairs.first(final_nodes_edges())
    def final_edges() -> frozenlist[hydra.accessors.AccessorEdge]:
        return hydra.lib.pairs.second(final_nodes_edges())
    return hydra.accessors.AccessorGraph(final_nodes(), final_edges())
