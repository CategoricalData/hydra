# Note: this is an automatically generated file. Do not edit.

r"""Utilities for working with term accessors."""

from __future__ import annotations
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import Tuple, cast
import hydra.accessors
import hydra.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.optionals
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.names
import hydra.rewriting

def term_accessor(accessor: hydra.accessors.TermAccessor) -> Maybe[str]:
    r"""Convert a term accessor to a string representation."""
    
    def idx[T0, T1](i: T0) -> Maybe[T1]:
        return Nothing()
    def idx_suff[T0](suffix: str, i: T0) -> Maybe[str]:
        return hydra.lib.optionals.map((lambda s: hydra.lib.strings.cat2(s, suffix)), idx(i))
    match accessor:
        case hydra.accessors.TermAccessorAnnotatedBody():
            return Nothing()
        
        case hydra.accessors.TermAccessorApplicationFunction():
            return Just("fun")
        
        case hydra.accessors.TermAccessorApplicationArgument():
            return Just("arg")
        
        case hydra.accessors.TermAccessorLambdaBody():
            return Just("body")
        
        case hydra.accessors.TermAccessorUnionCasesDefault():
            return Just("default")
        
        case hydra.accessors.TermAccessorUnionCasesBranch(value=name):
            return Just(hydra.lib.strings.cat2(".", name.value))
        
        case hydra.accessors.TermAccessorLetBody():
            return Just("in")
        
        case hydra.accessors.TermAccessorLetBinding(value=name2):
            return Just(hydra.lib.strings.cat2(name2.value, "="))
        
        case hydra.accessors.TermAccessorListElement(value=i):
            return idx(i)
        
        case hydra.accessors.TermAccessorMapKey(value=i2):
            return idx_suff(".key", i2)
        
        case hydra.accessors.TermAccessorMapValue(value=i3):
            return idx_suff(".value", i3)
        
        case hydra.accessors.TermAccessorOptionalTerm():
            return Just("just")
        
        case hydra.accessors.TermAccessorProductTerm(value=i4):
            return idx(i4)
        
        case hydra.accessors.TermAccessorRecordField(value=name3):
            return Just(hydra.lib.strings.cat2(".", name3.value))
        
        case hydra.accessors.TermAccessorSetElement(value=i5):
            return idx(i5)
        
        case hydra.accessors.TermAccessorSumTerm():
            return Nothing()
        
        case hydra.accessors.TermAccessorTypeLambdaBody():
            return Nothing()
        
        case hydra.accessors.TermAccessorTypeApplicationTerm():
            return Nothing()
        
        case hydra.accessors.TermAccessorInjectionTerm():
            return Nothing()
        
        case hydra.accessors.TermAccessorWrappedTerm():
            return Nothing()

def term_to_accessor_graph(namespaces: FrozenDict[hydra.module.Namespace, str], term: hydra.core.Term) -> hydra.accessors.AccessorGraph:
    r"""Build an accessor graph from a term."""
    
    dont_care_accessor = cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorAnnotatedBody(None))
    def helper(ids: FrozenDict[hydra.core.Name, hydra.accessors.AccessorNode], mroot: Maybe[hydra.accessors.AccessorNode], path: frozenlist[hydra.accessors.TermAccessor], state: Tuple[Tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]], frozenset[str]], accessor_term: Tuple[hydra.accessors.TermAccessor, hydra.core.Term]) -> Tuple[Tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]], frozenset[str]]:
        accessor = accessor_term[0]
        current_term = accessor_term[1]
        nodes_edges = state[0]
        visited = state[1]
        nodes = nodes_edges[0]
        edges = nodes_edges[1]
        next_path = hydra.lib.lists.cons(accessor, path)
        match current_term:
            case hydra.core.TermLet(value=let_expr):
                bindings = let_expr.bindings
                env = let_expr.body
                binding_names = hydra.lib.lists.map((lambda v1: v1.name), bindings)
                def add_binding_name(nodes_visited_ids: Tuple[Tuple[frozenlist[hydra.accessors.AccessorNode], frozenset[str]], FrozenDict[hydra.core.Name, hydra.accessors.AccessorNode]], name: hydra.core.Name) -> Tuple[Tuple[frozenlist[hydra.accessors.AccessorNode], frozenset[str]], FrozenDict[hydra.core.Name, hydra.accessors.AccessorNode]]:
                    current_nodes_visited = nodes_visited_ids[0]
                    current_ids = nodes_visited_ids[1]
                    current_nodes = current_nodes_visited[0]
                    current_visited = current_nodes_visited[1]
                    raw_label = hydra.names.compact_name(namespaces, name)
                    unique_label = hydra.names.unique_label(current_visited, raw_label)
                    node = hydra.accessors.AccessorNode(name, raw_label, unique_label)
                    new_visited = hydra.lib.sets.insert(unique_label, current_visited)
                    new_nodes = hydra.lib.lists.cons(node, current_nodes)
                    new_ids = hydra.lib.maps.insert(name, node, current_ids)
                    return ((new_nodes, new_visited), new_ids)
                nodes_visited_ids1 = hydra.lib.lists.foldl(add_binding_name, (((), visited), ids), binding_names)
                nodes1 = nodes_visited_ids1[0][0]
                visited1 = nodes_visited_ids1[0][1]
                ids1 = nodes_visited_ids1[1]
                def add_binding_term(current_state: Tuple[Tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]], frozenset[str]], node_binding: Tuple[hydra.accessors.AccessorNode, hydra.core.Binding]) -> Tuple[Tuple[frozenlist[hydra.accessors.AccessorNode], frozenlist[hydra.accessors.AccessorEdge]], frozenset[str]]:
                    root = node_binding[0]
                    binding = node_binding[1]
                    term1 = binding.term
                    return helper(ids1, Just(root), (), current_state, (dont_care_accessor, term1))
                node_binding_pairs = hydra.lib.lists.zip(nodes1, bindings)
                state_after_bindings = hydra.lib.lists.foldl(add_binding_term, ((hydra.lib.lists.concat2(nodes1, nodes), edges), visited1), node_binding_pairs)
                return helper(ids1, mroot, next_path, state_after_bindings, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLetBody(None)), env))
            
            case hydra.core.TermVariable(value=name):
                return hydra.lib.optionals.maybe(state, (lambda root: hydra.lib.optionals.maybe(state, (lambda node: (edge := hydra.accessors.AccessorEdge(root, hydra.accessors.AccessorPath(hydra.lib.lists.reverse(next_path)), node), new_edges := hydra.lib.lists.cons(edge, edges), ((nodes, new_edges), visited))[2]), hydra.lib.maps.lookup(name, ids))), mroot)
            
            case _:
                return hydra.lib.lists.foldl((lambda v1, v2: helper(ids, mroot, next_path, v1, v2)), state, hydra.rewriting.subterms_with_accessors(current_term))
    initial_state = (((), ()), hydra.lib.sets.empty())
    result = helper(hydra.lib.maps.empty(), Nothing(), (), initial_state, (dont_care_accessor, term))
    final_nodes_edges = result[0]
    final_nodes = final_nodes_edges[0]
    final_edges = final_nodes_edges[1]
    return hydra.accessors.AccessorGraph(final_nodes, final_edges)
