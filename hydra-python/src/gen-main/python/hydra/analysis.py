# Note: this is an automatically generated file. Do not edit.

r"""Module dependency namespace analysis."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Just, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.coders
import hydra.context
import hydra.core
import hydra.decode.core
import hydra.dependencies
import hydra.errors
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.sets
import hydra.module
import hydra.names
import hydra.predicates
import hydra.rewriting
import hydra.strip

T0 = TypeVar("T0")

def add_names_to_namespaces(encode_namespace: Callable[[hydra.module.Namespace], T0], names: frozenset[hydra.core.Name], ns0: hydra.module.Namespaces[T0]) -> hydra.module.Namespaces[T0]:
    r"""Add names to existing namespaces mapping."""

    @lru_cache(1)
    def nss() -> frozenset[hydra.module.Namespace]:
        return hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: hydra.names.namespace_of(x1)), hydra.lib.sets.to_list(names))))
    def to_pair(ns: hydra.module.Namespace) -> tuple[hydra.module.Namespace, T0]:
        return (ns, encode_namespace(ns))
    return hydra.module.Namespaces(ns0.focus, hydra.lib.maps.union(ns0.mapping, hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: to_pair(x1)), hydra.lib.sets.to_list(nss())))))

def definition_dependency_namespaces(defs: frozenlist[hydra.module.Definition]) -> frozenset[hydra.module.Namespace]:
    r"""Get dependency namespaces from definitions."""

    def def_names(def_: hydra.module.Definition) -> frozenset[hydra.core.Name]:
        match def_:
            case hydra.module.DefinitionType(value=type_def):
                return hydra.dependencies.type_dependency_names(True, type_def.type)

            case hydra.module.DefinitionTerm(value=term_def):
                return hydra.dependencies.term_dependency_names(True, True, True, term_def.term)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    @lru_cache(1)
    def all_names() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.unions(hydra.lib.lists.map((lambda x1: def_names(x1)), defs))
    return hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: hydra.names.namespace_of(x1)), hydra.lib.sets.to_list(all_names()))))

def dependency_namespaces(cx: hydra.context.Context, graph: hydra.graph.Graph, binds: bool, with_prims: bool, with_noms: bool, with_schema: bool, els: frozenlist[hydra.core.Binding]) -> Either[hydra.context.InContext[hydra.errors.Error], frozenset[hydra.module.Namespace]]:
    r"""Find dependency namespaces in all of a set of terms (Either version)."""

    def dep_names(el: hydra.core.Binding) -> Either[hydra.context.InContext[hydra.errors.Error], frozenset[hydra.core.Name]]:
        term = el.term
        @lru_cache(1)
        def deannotated_term() -> hydra.core.Term:
            return hydra.strip.deannotate_term(term)
        @lru_cache(1)
        def data_names() -> frozenset[hydra.core.Name]:
            return hydra.dependencies.term_dependency_names(binds, with_prims, with_noms, term)
        @lru_cache(1)
        def schema_names() -> frozenset[hydra.core.Name]:
            return hydra.lib.logic.if_else(with_schema, (lambda : hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda ts: hydra.dependencies.type_dependency_names(True, ts.type)), el.type)), (lambda : hydra.lib.sets.empty()))
        return hydra.lib.logic.if_else(hydra.predicates.is_encoded_type(deannotated_term()), (lambda : hydra.lib.eithers.map((lambda typ: hydra.lib.sets.unions((data_names(), schema_names(), hydra.dependencies.type_dependency_names(True, typ)))), hydra.lib.eithers.bimap((lambda _wc_e: hydra.context.InContext(_wc_e, hydra.context.Context(hydra.lib.lists.cons("dependency namespace (type)", cx.trace), cx.messages, cx.other))), (lambda _wc_a: _wc_a), hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_e.value)))), (lambda _a: _a), hydra.decode.core.type(graph, term))))), (lambda : hydra.lib.logic.if_else(hydra.predicates.is_encoded_term(deannotated_term()), (lambda : hydra.lib.eithers.map((lambda decoded_term: hydra.lib.sets.unions((data_names(), schema_names(), hydra.dependencies.term_dependency_names(binds, with_prims, with_noms, decoded_term)))), hydra.lib.eithers.bimap((lambda _wc_e: hydra.context.InContext(_wc_e, hydra.context.Context(hydra.lib.lists.cons("dependency namespace (term)", cx.trace), cx.messages, cx.other))), (lambda _wc_a: _wc_a), hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_e.value)))), (lambda _a: _a), hydra.decode.core.term(graph, term))))), (lambda : Right(hydra.lib.sets.unions((data_names(), schema_names())))))))
    return hydra.lib.eithers.map((lambda names_list: hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: hydra.names.namespace_of(x1)), hydra.lib.sets.to_list(hydra.lib.sets.unions(names_list)))))), hydra.lib.eithers.map_list((lambda x1: dep_names(x1)), els))

def module_contains_binary_literals(mod: hydra.module.Module) -> bool:
    r"""Check whether a module contains any binary literal values."""

    def check_term(found: bool, term: hydra.core.Term):
        def _hoist_check_term_1(v1):
            match v1:
                case hydra.core.LiteralBinary():
                    return True

                case _:
                    return False
        def _hoist_check_term_2(v1):
            match v1:
                case hydra.core.TermLiteral(value=lit):
                    return _hoist_check_term_1(lit)

                case _:
                    return False
        return hydra.lib.logic.or_(found, _hoist_check_term_2(term))
    def term_contains_binary(term: hydra.core.Term) -> bool:
        return hydra.rewriting.fold_over_term(hydra.coders.TraversalOrder.PRE, (lambda x1, x2: check_term(x1, x2)), False, term)
    @lru_cache(1)
    def def_terms():
        def _hoist_def_terms_1(v1):
            match v1:
                case hydra.module.DefinitionTerm(value=td):
                    return Just(td.term)

                case _:
                    return Nothing()
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_def_terms_1(d)), mod.definitions))
    return hydra.lib.lists.foldl((lambda acc, t: hydra.lib.logic.or_(acc, term_contains_binary(t))), False, def_terms())

def module_dependency_namespaces(cx: hydra.context.Context, graph: hydra.graph.Graph, binds: bool, with_prims: bool, with_noms: bool, with_schema: bool, mod: hydra.module.Module) -> Either[hydra.context.InContext[hydra.errors.Error], frozenset[hydra.module.Namespace]]:
    r"""Find dependency namespaces in all elements of a module, excluding the module's own namespace (Either version)."""

    @lru_cache(1)
    def all_bindings():
        def _hoist_all_bindings_1(v1):
            match v1:
                case hydra.module.DefinitionType(value=td):
                    return Just(hydra.annotations.type_element(td.name, td.type))

                case hydra.module.DefinitionTerm(value=td):
                    return Just(hydra.core.Binding(td.name, td.term, td.type))

                case _:
                    return Nothing()
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_all_bindings_1(d)), mod.definitions))
    return hydra.lib.eithers.map((lambda deps: hydra.lib.sets.delete(mod.namespace, deps)), dependency_namespaces(cx, graph, binds, with_prims, with_noms, with_schema, all_bindings()))

def namespaces_for_definitions(encode_namespace: Callable[[hydra.module.Namespace], T0], focus_ns: hydra.module.Namespace, defs: frozenlist[hydra.module.Definition]) -> hydra.module.Namespaces[T0]:
    r"""Create namespaces mapping for definitions."""

    @lru_cache(1)
    def nss() -> frozenset[hydra.module.Namespace]:
        return hydra.lib.sets.delete(focus_ns, definition_dependency_namespaces(defs))
    def to_pair(ns: hydra.module.Namespace) -> tuple[hydra.module.Namespace, T0]:
        return (ns, encode_namespace(ns))
    return hydra.module.Namespaces(to_pair(focus_ns), hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: to_pair(x1)), hydra.lib.sets.to_list(nss()))))
