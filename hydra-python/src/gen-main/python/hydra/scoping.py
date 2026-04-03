# Note: this is an automatically generated file. Do not edit.

r"""Graph context extension and type scheme conversion."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Maybe, Nothing, frozenlist
from typing import cast
import hydra.core
import hydra.graph
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.sets

def f_type_to_type_scheme(typ: hydra.core.Type) -> hydra.core.TypeScheme:
    r"""Convert a forall type to a type scheme."""

    def strip_annotations(t: hydra.core.Type) -> hydra.core.Type:
        while True:
            match t:
                case hydra.core.TypeAnnotated(value=at):
                    t = at.body
                    continue

                case _:
                    return t
    def gather_forall(vars: frozenlist[hydra.core.Name], typ2: hydra.core.Type) -> hydra.core.TypeScheme:
        while True:
            match strip_annotations(typ2):
                case hydra.core.TypeForall(value=ft):
                    vars = hydra.lib.lists.cons(ft.parameter, vars)
                    typ2 = ft.body
                    continue

                case _:
                    return hydra.core.TypeScheme(hydra.lib.lists.reverse(vars), typ2, Nothing())
    return gather_forall((), typ)

def extend_graph_for_lambda(g: hydra.graph.Graph, lam: hydra.core.Lambda) -> hydra.graph.Graph:
    r"""Extend a graph by descending into a lambda body."""

    var = lam.parameter
    return hydra.graph.Graph(g.bound_terms, hydra.lib.maybes.maybe((lambda : g.bound_types), (lambda dom: hydra.lib.maps.insert(var, f_type_to_type_scheme(dom), g.bound_types)), lam.domain), g.class_constraints, hydra.lib.sets.insert(var, g.lambda_variables), hydra.lib.maps.delete(var, g.metadata), g.primitives, g.schema_types, g.type_variables)

def extend_graph_with_bindings(bindings: frozenlist[hydra.core.Binding], g: hydra.graph.Graph) -> hydra.graph.Graph:
    r"""Add bindings to an existing graph."""

    @lru_cache(1)
    def new_terms() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda b: (b.name, b.term)), bindings))
    @lru_cache(1)
    def new_types() -> FrozenDict[hydra.core.Name, hydra.core.TypeScheme]:
        return hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda b: hydra.lib.maybes.map((lambda ts: (b.name, ts)), b.type)), bindings)))
    return hydra.graph.Graph(hydra.lib.maps.union(new_terms(), g.bound_terms), hydra.lib.maps.union(new_types(), g.bound_types), g.class_constraints, g.lambda_variables, g.metadata, g.primitives, g.schema_types, g.type_variables)

def extend_graph_for_let(for_binding: Callable[[hydra.graph.Graph, hydra.core.Binding], Maybe[hydra.core.Term]], g: hydra.graph.Graph, letrec: hydra.core.Let) -> hydra.graph.Graph:
    r"""Extend a graph by descending into a let body."""

    bindings = letrec.bindings
    @lru_cache(1)
    def g2() -> hydra.graph.Graph:
        return extend_graph_with_bindings(bindings, g)
    return hydra.graph.Graph(hydra.lib.maps.union(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda b: (b.name, b.term)), bindings)), g.bound_terms), hydra.lib.maps.union(hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda b: hydra.lib.maybes.map((lambda ts: (b.name, ts)), b.type)), bindings))), g.bound_types), g.class_constraints, hydra.lib.lists.foldl((lambda s, b: hydra.lib.sets.delete(b.name, s)), g.lambda_variables, bindings), hydra.lib.lists.foldl((lambda g_acc, b: (m := g_acc.metadata, new_meta := hydra.lib.maybes.maybe((lambda : hydra.lib.maps.delete(b.name, m)), (lambda t: hydra.lib.maps.insert(b.name, t, m)), for_binding(g_acc, b)), hydra.graph.Graph(g_acc.bound_terms, g_acc.bound_types, g_acc.class_constraints, g_acc.lambda_variables, new_meta, g_acc.primitives, g_acc.schema_types, g_acc.type_variables))[2]), g2(), bindings).metadata, g.primitives, g.schema_types, g.type_variables)

def extend_graph_for_type_lambda(g: hydra.graph.Graph, tlam: hydra.core.TypeLambda) -> hydra.graph.Graph:
    r"""Extend a graph by descending into a type lambda body."""

    name = tlam.parameter
    return hydra.graph.Graph(g.bound_terms, g.bound_types, g.class_constraints, g.lambda_variables, g.metadata, g.primitives, g.schema_types, hydra.lib.sets.insert(name, g.type_variables))

def type_scheme_to_f_type(ts: hydra.core.TypeScheme) -> hydra.core.Type:
    r"""Convert a type scheme to a forall type."""

    vars = ts.variables
    body = ts.type
    return hydra.lib.lists.foldl((lambda t, v: cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(v, t)))), body, hydra.lib.lists.reverse(vars))
