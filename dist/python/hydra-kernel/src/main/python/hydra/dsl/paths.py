# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.paths."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.core
import hydra.phantoms

def subterm_edge(source: hydra.phantoms.TTerm[hydra.paths.SubtermNode], path: hydra.phantoms.TTerm[hydra.paths.SubtermPath], target: hydra.phantoms.TTerm[hydra.paths.SubtermNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtermEdge"), (hydra.core.Field(hydra.core.Name("source"), source.value), hydra.core.Field(hydra.core.Name("path"), path.value), hydra.core.Field(hydra.core.Name("target"), target.value))))))

def subterm_edge_path(x: hydra.phantoms.TTerm[hydra.paths.SubtermEdge]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermEdge"), hydra.core.Name("path")))), x.value))))

def subterm_edge_source(x: hydra.phantoms.TTerm[hydra.paths.SubtermEdge]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermEdge"), hydra.core.Name("source")))), x.value))))

def subterm_edge_target(x: hydra.phantoms.TTerm[hydra.paths.SubtermEdge]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermEdge"), hydra.core.Name("target")))), x.value))))

def subterm_edge_with_path(original: hydra.phantoms.TTerm[hydra.paths.SubtermEdge], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtermEdge"), (hydra.core.Field(hydra.core.Name("source"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermEdge"), hydra.core.Name("source")))), original.value)))), hydra.core.Field(hydra.core.Name("path"), new_val.value), hydra.core.Field(hydra.core.Name("target"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermEdge"), hydra.core.Name("target")))), original.value)))))))))

def subterm_edge_with_source(original: hydra.phantoms.TTerm[hydra.paths.SubtermEdge], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtermEdge"), (hydra.core.Field(hydra.core.Name("source"), new_val.value), hydra.core.Field(hydra.core.Name("path"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermEdge"), hydra.core.Name("path")))), original.value)))), hydra.core.Field(hydra.core.Name("target"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermEdge"), hydra.core.Name("target")))), original.value)))))))))

def subterm_edge_with_target(original: hydra.phantoms.TTerm[hydra.paths.SubtermEdge], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtermEdge"), (hydra.core.Field(hydra.core.Name("source"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermEdge"), hydra.core.Name("source")))), original.value)))), hydra.core.Field(hydra.core.Name("path"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermEdge"), hydra.core.Name("path")))), original.value)))), hydra.core.Field(hydra.core.Name("target"), new_val.value))))))

def subterm_graph(nodes: hydra.phantoms.TTerm[frozenlist[hydra.paths.SubtermNode]], edges: hydra.phantoms.TTerm[frozenlist[hydra.paths.SubtermEdge]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtermGraph"), (hydra.core.Field(hydra.core.Name("nodes"), nodes.value), hydra.core.Field(hydra.core.Name("edges"), edges.value))))))

def subterm_graph_edges(x: hydra.phantoms.TTerm[hydra.paths.SubtermGraph]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermGraph"), hydra.core.Name("edges")))), x.value))))

def subterm_graph_nodes(x: hydra.phantoms.TTerm[hydra.paths.SubtermGraph]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermGraph"), hydra.core.Name("nodes")))), x.value))))

def subterm_graph_with_edges(original: hydra.phantoms.TTerm[hydra.paths.SubtermGraph], new_val: hydra.phantoms.TTerm[frozenlist[hydra.paths.SubtermEdge]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtermGraph"), (hydra.core.Field(hydra.core.Name("nodes"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermGraph"), hydra.core.Name("nodes")))), original.value)))), hydra.core.Field(hydra.core.Name("edges"), new_val.value))))))

def subterm_graph_with_nodes(original: hydra.phantoms.TTerm[hydra.paths.SubtermGraph], new_val: hydra.phantoms.TTerm[frozenlist[hydra.paths.SubtermNode]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtermGraph"), (hydra.core.Field(hydra.core.Name("nodes"), new_val.value), hydra.core.Field(hydra.core.Name("edges"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermGraph"), hydra.core.Name("edges")))), original.value)))))))))

def subterm_node(name: hydra.phantoms.TTerm[hydra.core.Name], label: hydra.phantoms.TTerm[str], id: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtermNode"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("label"), label.value), hydra.core.Field(hydra.core.Name("id"), id.value))))))

def subterm_node_id(x: hydra.phantoms.TTerm[hydra.paths.SubtermNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermNode"), hydra.core.Name("id")))), x.value))))

def subterm_node_label(x: hydra.phantoms.TTerm[hydra.paths.SubtermNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermNode"), hydra.core.Name("label")))), x.value))))

def subterm_node_name(x: hydra.phantoms.TTerm[hydra.paths.SubtermNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermNode"), hydra.core.Name("name")))), x.value))))

def subterm_node_with_id(original: hydra.phantoms.TTerm[hydra.paths.SubtermNode], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtermNode"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermNode"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("label"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermNode"), hydra.core.Name("label")))), original.value)))), hydra.core.Field(hydra.core.Name("id"), new_val.value))))))

def subterm_node_with_label(original: hydra.phantoms.TTerm[hydra.paths.SubtermNode], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtermNode"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermNode"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("label"), new_val.value), hydra.core.Field(hydra.core.Name("id"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermNode"), hydra.core.Name("id")))), original.value)))))))))

def subterm_node_with_name(original: hydra.phantoms.TTerm[hydra.paths.SubtermNode], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtermNode"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("label"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermNode"), hydra.core.Name("label")))), original.value)))), hydra.core.Field(hydra.core.Name("id"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtermNode"), hydra.core.Name("id")))), original.value)))))))))

def subterm_path(x: hydra.phantoms.TTerm[frozenlist[hydra.paths.SubtermStep]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.paths.SubtermPath"), x.value))))

subterm_step_annotated_body = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("annotatedBody"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subterm_step_application_argument = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("applicationArgument"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subterm_step_application_function = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("applicationFunction"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subterm_step_injection_term = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("injectionTerm"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subterm_step_lambda_body = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("lambdaBody"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def subterm_step_let_binding(x: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("letBinding"), x.value)))))

subterm_step_let_body = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("letBody"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def subterm_step_list_element(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("listElement"), x.value)))))

def subterm_step_map_key(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("mapKey"), x.value)))))

def subterm_step_map_value(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("mapValue"), x.value)))))

subterm_step_maybe_term = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("maybeTerm"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def subterm_step_product_term(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("productTerm"), x.value)))))

def subterm_step_record_field(x: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("recordField"), x.value)))))

def subterm_step_set_element(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("setElement"), x.value)))))

subterm_step_sum_term = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("sumTerm"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subterm_step_type_application_term = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("typeApplicationTerm"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subterm_step_type_lambda_body = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("typeLambdaBody"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def subterm_step_union_cases_branch(x: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("unionCasesBranch"), x.value)))))

subterm_step_union_cases_default = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("unionCasesDefault"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subterm_step_wrapped_term = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtermStep"), hydra.core.Field(hydra.core.Name("wrappedTerm"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def subtype_edge(source: hydra.phantoms.TTerm[hydra.paths.SubtypeNode], path: hydra.phantoms.TTerm[hydra.paths.SubtypePath], target: hydra.phantoms.TTerm[hydra.paths.SubtypeNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtypeEdge"), (hydra.core.Field(hydra.core.Name("source"), source.value), hydra.core.Field(hydra.core.Name("path"), path.value), hydra.core.Field(hydra.core.Name("target"), target.value))))))

def subtype_edge_path(x: hydra.phantoms.TTerm[hydra.paths.SubtypeEdge]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeEdge"), hydra.core.Name("path")))), x.value))))

def subtype_edge_source(x: hydra.phantoms.TTerm[hydra.paths.SubtypeEdge]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeEdge"), hydra.core.Name("source")))), x.value))))

def subtype_edge_target(x: hydra.phantoms.TTerm[hydra.paths.SubtypeEdge]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeEdge"), hydra.core.Name("target")))), x.value))))

def subtype_edge_with_path(original: hydra.phantoms.TTerm[hydra.paths.SubtypeEdge], new_val: hydra.phantoms.TTerm[hydra.paths.SubtypePath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtypeEdge"), (hydra.core.Field(hydra.core.Name("source"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeEdge"), hydra.core.Name("source")))), original.value)))), hydra.core.Field(hydra.core.Name("path"), new_val.value), hydra.core.Field(hydra.core.Name("target"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeEdge"), hydra.core.Name("target")))), original.value)))))))))

def subtype_edge_with_source(original: hydra.phantoms.TTerm[hydra.paths.SubtypeEdge], new_val: hydra.phantoms.TTerm[hydra.paths.SubtypeNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtypeEdge"), (hydra.core.Field(hydra.core.Name("source"), new_val.value), hydra.core.Field(hydra.core.Name("path"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeEdge"), hydra.core.Name("path")))), original.value)))), hydra.core.Field(hydra.core.Name("target"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeEdge"), hydra.core.Name("target")))), original.value)))))))))

def subtype_edge_with_target(original: hydra.phantoms.TTerm[hydra.paths.SubtypeEdge], new_val: hydra.phantoms.TTerm[hydra.paths.SubtypeNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtypeEdge"), (hydra.core.Field(hydra.core.Name("source"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeEdge"), hydra.core.Name("source")))), original.value)))), hydra.core.Field(hydra.core.Name("path"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeEdge"), hydra.core.Name("path")))), original.value)))), hydra.core.Field(hydra.core.Name("target"), new_val.value))))))

def subtype_graph(nodes: hydra.phantoms.TTerm[frozenlist[hydra.paths.SubtypeNode]], edges: hydra.phantoms.TTerm[frozenlist[hydra.paths.SubtypeEdge]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtypeGraph"), (hydra.core.Field(hydra.core.Name("nodes"), nodes.value), hydra.core.Field(hydra.core.Name("edges"), edges.value))))))

def subtype_graph_edges(x: hydra.phantoms.TTerm[hydra.paths.SubtypeGraph]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeGraph"), hydra.core.Name("edges")))), x.value))))

def subtype_graph_nodes(x: hydra.phantoms.TTerm[hydra.paths.SubtypeGraph]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeGraph"), hydra.core.Name("nodes")))), x.value))))

def subtype_graph_with_edges(original: hydra.phantoms.TTerm[hydra.paths.SubtypeGraph], new_val: hydra.phantoms.TTerm[frozenlist[hydra.paths.SubtypeEdge]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtypeGraph"), (hydra.core.Field(hydra.core.Name("nodes"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeGraph"), hydra.core.Name("nodes")))), original.value)))), hydra.core.Field(hydra.core.Name("edges"), new_val.value))))))

def subtype_graph_with_nodes(original: hydra.phantoms.TTerm[hydra.paths.SubtypeGraph], new_val: hydra.phantoms.TTerm[frozenlist[hydra.paths.SubtypeNode]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtypeGraph"), (hydra.core.Field(hydra.core.Name("nodes"), new_val.value), hydra.core.Field(hydra.core.Name("edges"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeGraph"), hydra.core.Name("edges")))), original.value)))))))))

def subtype_node(name: hydra.phantoms.TTerm[hydra.core.Name], label: hydra.phantoms.TTerm[str], id: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtypeNode"), (hydra.core.Field(hydra.core.Name("name"), name.value), hydra.core.Field(hydra.core.Name("label"), label.value), hydra.core.Field(hydra.core.Name("id"), id.value))))))

def subtype_node_id(x: hydra.phantoms.TTerm[hydra.paths.SubtypeNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeNode"), hydra.core.Name("id")))), x.value))))

def subtype_node_label(x: hydra.phantoms.TTerm[hydra.paths.SubtypeNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeNode"), hydra.core.Name("label")))), x.value))))

def subtype_node_name(x: hydra.phantoms.TTerm[hydra.paths.SubtypeNode]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeNode"), hydra.core.Name("name")))), x.value))))

def subtype_node_with_id(original: hydra.phantoms.TTerm[hydra.paths.SubtypeNode], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtypeNode"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeNode"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("label"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeNode"), hydra.core.Name("label")))), original.value)))), hydra.core.Field(hydra.core.Name("id"), new_val.value))))))

def subtype_node_with_label(original: hydra.phantoms.TTerm[hydra.paths.SubtypeNode], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtypeNode"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeNode"), hydra.core.Name("name")))), original.value)))), hydra.core.Field(hydra.core.Name("label"), new_val.value), hydra.core.Field(hydra.core.Name("id"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeNode"), hydra.core.Name("id")))), original.value)))))))))

def subtype_node_with_name(original: hydra.phantoms.TTerm[hydra.paths.SubtypeNode], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.paths.SubtypeNode"), (hydra.core.Field(hydra.core.Name("name"), new_val.value), hydra.core.Field(hydra.core.Name("label"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeNode"), hydra.core.Name("label")))), original.value)))), hydra.core.Field(hydra.core.Name("id"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.paths.SubtypeNode"), hydra.core.Name("id")))), original.value)))))))))

def subtype_path(x: hydra.phantoms.TTerm[frozenlist[hydra.paths.SubtypeStep]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.paths.SubtypePath"), x.value))))

subtype_step_annotated_body = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("annotatedBody"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subtype_step_application_argument = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("applicationArgument"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subtype_step_application_function = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("applicationFunction"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subtype_step_either_left = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("eitherLeft"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subtype_step_either_right = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("eitherRight"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subtype_step_forall_body = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("forallBody"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subtype_step_function_codomain = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("functionCodomain"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subtype_step_function_domain = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("functionDomain"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subtype_step_list_element = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("listElement"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subtype_step_map_keys = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("mapKeys"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subtype_step_map_values = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("mapValues"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subtype_step_maybe_element = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("maybeElement"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subtype_step_pair_first = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("pairFirst"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

subtype_step_pair_second = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("pairSecond"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def subtype_step_record_field(x: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("recordField"), x.value)))))

subtype_step_set_element = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("setElement"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def subtype_step_union_field(x: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("unionField"), x.value)))))

subtype_step_wrapped_type = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.paths.SubtypeStep"), hydra.core.Field(hydra.core.Name("wrappedType"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def un_subterm_path(x: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.paths.SubtermPath"))), x.value))))

def un_subtype_path(x: hydra.phantoms.TTerm[hydra.paths.SubtypePath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.paths.SubtypePath"))), x.value))))
