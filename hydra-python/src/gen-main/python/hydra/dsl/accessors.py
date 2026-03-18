# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.accessors."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.core
import hydra.phantoms

def accessor_edge(source: hydra.phantoms.TTerm[hydra.accessors.AccessorNode], path: hydra.phantoms.TTerm[hydra.accessors.AccessorPath], target: hydra.phantoms.TTerm[hydra.accessors.AccessorNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorEdge"), (hydra.core.Field(hydra.core.Name("source"), source.value), hydra.core.Field(hydra.core.Name("path"), path.value), hydra.core.Field(hydra.core.Name("target"), target.value))))))

def accessor_edge_path(x: hydra.phantoms.TTerm[hydra.accessors.AccessorEdge]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorEdge"), hydra.core.Name("path")))))))), x.value))))

def accessor_edge_source(x: hydra.phantoms.TTerm[hydra.accessors.AccessorEdge]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorEdge"), hydra.core.Name("source")))))))), x.value))))

def accessor_edge_target(x: hydra.phantoms.TTerm[hydra.accessors.AccessorEdge]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorEdge"), hydra.core.Name("target")))))))), x.value))))

def accessor_edge_with_path(original: hydra.phantoms.TTerm[hydra.accessors.AccessorEdge], new_val: hydra.phantoms.TTerm[hydra.accessors.AccessorPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorEdge"), (hydra.core.Field(hydra.core.Name("source"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorEdge"), hydra.core.Name("source")))))))), original.value)))), hydra.core.Field(hydra.core.Name("path"), new_val.value), hydra.core.Field(hydra.core.Name("target"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorEdge"), hydra.core.Name("target")))))))), original.value)))))))))

def accessor_edge_with_source(original: hydra.phantoms.TTerm[hydra.accessors.AccessorEdge], new_val: hydra.phantoms.TTerm[hydra.accessors.AccessorNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorEdge"), (hydra.core.Field(hydra.core.Name("source"), new_val.value), hydra.core.Field(hydra.core.Name("path"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorEdge"), hydra.core.Name("path")))))))), original.value)))), hydra.core.Field(hydra.core.Name("target"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorEdge"), hydra.core.Name("target")))))))), original.value)))))))))

def accessor_edge_with_target(original: hydra.phantoms.TTerm[hydra.accessors.AccessorEdge], new_val: hydra.phantoms.TTerm[hydra.accessors.AccessorNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorEdge"), (hydra.core.Field(hydra.core.Name("source"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorEdge"), hydra.core.Name("source")))))))), original.value)))), hydra.core.Field(hydra.core.Name("path"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorEdge"), hydra.core.Name("path")))))))), original.value)))), hydra.core.Field(hydra.core.Name("target"), new_val.value))))))

def accessor_graph(nodes: hydra.phantoms.TTerm[frozenlist[hydra.accessors.AccessorNode]], edges: hydra.phantoms.TTerm[frozenlist[hydra.accessors.AccessorEdge]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorGraph"), (hydra.core.Field(hydra.core.Name("nodes"), nodes.value), hydra.core.Field(hydra.core.Name("edges"), edges.value))))))

def accessor_graph_edges(x: hydra.phantoms.TTerm[hydra.accessors.AccessorGraph]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorGraph"), hydra.core.Name("edges")))))))), x.value))))

def accessor_graph_nodes(x: hydra.phantoms.TTerm[hydra.accessors.AccessorGraph]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorGraph"), hydra.core.Name("nodes")))))))), x.value))))

def accessor_graph_with_edges(original: hydra.phantoms.TTerm[hydra.accessors.AccessorGraph], new_val: hydra.phantoms.TTerm[frozenlist[hydra.accessors.AccessorEdge]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorGraph"), (hydra.core.Field(hydra.core.Name("nodes"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorGraph"), hydra.core.Name("nodes")))))))), original.value)))), hydra.core.Field(hydra.core.Name("edges"), new_val.value))))))

def accessor_graph_with_nodes(original: hydra.phantoms.TTerm[hydra.accessors.AccessorGraph], new_val: hydra.phantoms.TTerm[frozenlist[hydra.accessors.AccessorNode]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorGraph"), (hydra.core.Field(hydra.core.Name("nodes"), new_val.value), hydra.core.Field(hydra.core.Name("edges"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorGraph"), hydra.core.Name("edges")))))))), original.value)))))))))

def accessor_node(name: hydra.phantoms.TTerm[hydra.core.Name], label: hydra.phantoms.TTerm[str], id: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorNode"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("label"), label.value), hydra.core.Field(hydra.core.Name("id"), id.value))))))

def accessor_node_id(x: hydra.phantoms.TTerm[hydra.accessors.AccessorNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorNode"), hydra.core.Name("id")))))))), x.value))))

def accessor_node_label(x: hydra.phantoms.TTerm[hydra.accessors.AccessorNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorNode"), hydra.core.Name("label")))))))), x.value))))

def accessor_node_name(x: hydra.phantoms.TTerm[hydra.accessors.AccessorNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorNode"), hydra.core.Name("name")))))))), x.value))))

def accessor_node_with_id(original: hydra.phantoms.TTerm[hydra.accessors.AccessorNode], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorNode"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorNode"), hydra.core.Name("name")))))))), original.value)))), hydra.core.Field(hydra.core.Name("label"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorNode"), hydra.core.Name("label")))))))), original.value)))), hydra.core.Field(hydra.core.Name("id"), new_val.value))))))

def accessor_node_with_label(original: hydra.phantoms.TTerm[hydra.accessors.AccessorNode], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorNode"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorNode"), hydra.core.Name("name")))))))), original.value)))), hydra.core.Field(hydra.core.Name("label"), new_val.value), hydra.core.Field(hydra.core.Name("id"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorNode"), hydra.core.Name("id")))))))), original.value)))))))))

def accessor_node_with_name(original: hydra.phantoms.TTerm[hydra.accessors.AccessorNode], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.accessors.AccessorNode"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("label"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorNode"), hydra.core.Name("label")))))))), original.value)))), hydra.core.Field(hydra.core.Name("id"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.accessors.AccessorNode"), hydra.core.Name("id")))))))), original.value)))))))))

def accessor_path(x: hydra.phantoms.TTerm[frozenlist[hydra.accessors.TermAccessor]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.accessors.AccessorPath"), x.value))))

term_accessor_annotated_body = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("annotatedBody"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_accessor_application_argument = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("applicationArgument"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_accessor_application_function = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("applicationFunction"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_accessor_injection_term = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("injectionTerm"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_accessor_lambda_body = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("lambdaBody"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def term_accessor_let_binding(x: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("letBinding"), x.value)))))

term_accessor_let_body = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("letBody"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def term_accessor_list_element(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("listElement"), x.value)))))

def term_accessor_map_key(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("mapKey"), x.value)))))

def term_accessor_map_value(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("mapValue"), x.value)))))

term_accessor_maybe_term = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("maybeTerm"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def term_accessor_product_term(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("productTerm"), x.value)))))

def term_accessor_record_field(x: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("recordField"), x.value)))))

def term_accessor_set_element(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("setElement"), x.value)))))

term_accessor_sum_term = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("sumTerm"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_accessor_type_application_term = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("typeApplicationTerm"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_accessor_type_lambda_body = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("typeLambdaBody"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def term_accessor_union_cases_branch(x: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("unionCasesBranch"), x.value)))))

term_accessor_union_cases_default = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("unionCasesDefault"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

term_accessor_wrapped_term = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.accessors.TermAccessor"), hydra.core.Field(hydra.core.Name("wrappedTerm"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def un_accessor_path(x: hydra.phantoms.TTerm[hydra.accessors.AccessorPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.accessors.AccessorPath"))))))), x.value))))
