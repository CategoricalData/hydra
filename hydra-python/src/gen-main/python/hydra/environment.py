# Note: this is an automatically generated file. Do not edit.

r"""Graph to type environment conversions."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.context
import hydra.core
import hydra.decode.core
import hydra.encode.core
import hydra.errors
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.packaging
import hydra.scoping
import hydra.sorting
import hydra.strip
import hydra.variables

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def definition_as_type_application_term(cx: hydra.context.Context, el: hydra.core.Binding) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.core.TypeApplicationTerm]:
    r"""Convert a definition to a typed term."""

    return hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("missing element type"))), cx))), (lambda ts: Right(hydra.core.TypeApplicationTerm(el.term, ts.type))), el.type)

def graph_as_let(bindings: frozenlist[hydra.core.Binding], body: hydra.core.Term) -> hydra.core.Let:
    r"""Convert bindings and a body to a let expression."""

    return hydra.core.Let(bindings, body)

def graph_as_term(bindings: frozenlist[hydra.core.Binding], body: hydra.core.Term) -> hydra.core.Term:
    r"""Convert bindings and a body to a term, using let-term duality."""

    return cast(hydra.core.Term, hydra.core.TermLet(graph_as_let(bindings, body)))

def graph_as_types(cx: hydra.context.Context, graph: hydra.graph.Graph, els: frozenlist[hydra.core.Binding]) -> Either[hydra.context.InContext[hydra.errors.DecodingError], FrozenDict[hydra.core.Name, hydra.core.Type]]:
    r"""Decode a list of type-encoding bindings into a map of named types."""

    def to_pair(el: hydra.core.Binding) -> Either[hydra.context.InContext[hydra.errors.DecodingError], tuple[hydra.core.Name, hydra.core.Type]]:
        return hydra.lib.eithers.map((lambda typ: (el.name, typ)), hydra.lib.eithers.bimap((lambda _wc_e: hydra.context.InContext(_wc_e, cx)), (lambda _wc_a: _wc_a), hydra.decode.core.type(graph, el.term)))
    return hydra.lib.eithers.map((lambda x1: hydra.lib.maps.from_list(x1)), hydra.lib.eithers.map_list((lambda x1: to_pair(x1)), els))

def partition_definitions(defs: frozenlist[hydra.packaging.Definition]) -> tuple[frozenlist[hydra.packaging.TypeDefinition], frozenlist[hydra.packaging.TermDefinition]]:
    r"""Partition a list of definitions into type definitions and term definitions."""

    def get_type(def_: hydra.packaging.Definition) -> Maybe[hydra.packaging.TypeDefinition]:
        match def_:
            case hydra.packaging.DefinitionType(value=td):
                return Just(td)

            case hydra.packaging.DefinitionTerm():
                return Nothing()

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def get_term(def_: hydra.packaging.Definition) -> Maybe[hydra.packaging.TermDefinition]:
        match def_:
            case hydra.packaging.DefinitionType():
                return Nothing()

            case hydra.packaging.DefinitionTerm(value=td):
                return Just(td)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return (hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: get_type(x1)), defs)), hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: get_term(x1)), defs)))

def reorder_defs(defs: frozenlist[hydra.packaging.Definition]) -> frozenlist[hydra.packaging.Definition]:
    r"""Reorder definitions: types first (with hydra.core.Name first among types), then topologically sorted terms."""

    @lru_cache(1)
    def partitioned() -> tuple[frozenlist[hydra.packaging.TypeDefinition], frozenlist[hydra.packaging.TermDefinition]]:
        return partition_definitions(defs)
    @lru_cache(1)
    def type_defs_raw() -> frozenlist[hydra.packaging.TypeDefinition]:
        return hydra.lib.pairs.first(partitioned())
    @lru_cache(1)
    def name_first() -> frozenlist[hydra.packaging.TypeDefinition]:
        return hydra.lib.lists.filter((lambda td: hydra.lib.equality.equal(td.name, hydra.core.Name("hydra.core.Name"))), type_defs_raw())
    @lru_cache(1)
    def name_rest() -> frozenlist[hydra.packaging.TypeDefinition]:
        return hydra.lib.lists.filter((lambda td: hydra.lib.logic.not_(hydra.lib.equality.equal(td.name, hydra.core.Name("hydra.core.Name")))), type_defs_raw())
    @lru_cache(1)
    def type_defs() -> frozenlist[hydra.packaging.Definition]:
        return hydra.lib.lists.concat((hydra.lib.lists.map((lambda td: cast(hydra.packaging.Definition, hydra.packaging.DefinitionType(td))), name_first()), hydra.lib.lists.map((lambda td: cast(hydra.packaging.Definition, hydra.packaging.DefinitionType(td))), name_rest())))
    @lru_cache(1)
    def term_defs_wrapped() -> frozenlist[hydra.packaging.Definition]:
        return hydra.lib.lists.map((lambda td: cast(hydra.packaging.Definition, hydra.packaging.DefinitionTerm(td))), hydra.lib.pairs.second(partitioned()))
    @lru_cache(1)
    def sorted_term_defs():
        def _hoist_sorted_term_defs_1(v1):
            match v1:
                case hydra.packaging.DefinitionTerm(value=td):
                    return td.name

                case _:
                    raise TypeError("Unsupported Definition")
        def _hoist_sorted_term_defs_2(v1):
            match v1:
                case hydra.packaging.DefinitionTerm(value=td):
                    return hydra.lib.sets.to_list(hydra.variables.free_variables_in_term(td.term))

                case _:
                    return ()
        return hydra.lib.lists.concat(hydra.sorting.topological_sort_nodes((lambda d: _hoist_sorted_term_defs_1(d)), (lambda d: _hoist_sorted_term_defs_2(d)), term_defs_wrapped()))
    return hydra.lib.lists.concat((type_defs(), sorted_term_defs()))

def schema_graph_to_typing_environment(cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.context.InContext[hydra.errors.Error], FrozenDict[hydra.core.Name, hydra.core.TypeScheme]]:
    r"""Convert a schema graph to a typing environment (Either version)."""

    def to_type_scheme(vars: frozenlist[hydra.core.Name], typ: hydra.core.Type) -> hydra.core.TypeScheme:
        while True:
            match hydra.strip.deannotate_type(typ):
                case hydra.core.TypeForall(value=ft):
                    vars = hydra.lib.lists.cons(ft.parameter, vars)
                    typ = ft.body
                    continue

                case _:
                    return hydra.core.TypeScheme(hydra.lib.lists.reverse(vars), typ, Nothing())
    def decode_type(term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type]:
        return hydra.lib.eithers.bimap((lambda _wc_e: hydra.context.InContext(_wc_e, cx)), (lambda _wc_a: _wc_a), hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_e.value)))), (lambda _a: _a), hydra.decode.core.type(g, term)))
    def decode_type_scheme(term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.core.TypeScheme]:
        return hydra.lib.eithers.bimap((lambda _wc_e: hydra.context.InContext(_wc_e, cx)), (lambda _wc_a: _wc_a), hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_e.value)))), (lambda _a: _a), hydra.decode.core.type_scheme(g, term)))
    def to_pair(el: hydra.core.Binding) -> Either[hydra.context.InContext[hydra.errors.Error], Maybe[tuple[hydra.core.Name, hydra.core.TypeScheme]]]:
        def for_term(term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], Maybe[hydra.core.TypeScheme]]:
            match term:
                case hydra.core.TermRecord(value=r):
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(r.type_name, hydra.core.Name("hydra.core.TypeScheme")), (lambda : hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), decode_type_scheme(el.term))), (lambda : Right(Nothing())))

                case hydra.core.TermUnion(value=i):
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(i.type_name, hydra.core.Name("hydra.core.Type")), (lambda : hydra.lib.eithers.map((lambda decoded: Just(to_type_scheme((), decoded))), decode_type(el.term))), (lambda : Right(Nothing())))

                case _:
                    return Right(Nothing())
        return hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : hydra.lib.eithers.map((lambda typ: Just(hydra.scoping.f_type_to_type_scheme(typ))), decode_type(el.term))), (lambda ts: hydra.lib.logic.if_else(hydra.lib.equality.equal(ts, hydra.core.TypeScheme((), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.TypeScheme"))), Nothing())), (lambda : hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), decode_type_scheme(el.term))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(ts, hydra.core.TypeScheme((), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))), Nothing())), (lambda : hydra.lib.eithers.map((lambda decoded: Just(to_type_scheme((), decoded))), decode_type(el.term))), (lambda : for_term(hydra.strip.deannotate_term(el.term))))))), el.type), (lambda mts: Right(hydra.lib.maybes.map((lambda ts: (el.name, ts)), mts))))
    return hydra.lib.eithers.map((lambda mpairs: hydra.lib.maps.from_list(hydra.lib.maybes.cat(mpairs))), hydra.lib.eithers.map_list((lambda x1: to_pair(x1)), hydra.lexical.graph_to_bindings(g)))

def term_as_bindings(term: hydra.core.Term) -> frozenlist[hydra.core.Binding]:
    r"""Extract the bindings from a let term, or return an empty list for other terms."""

    match hydra.strip.deannotate_term(term):
        case hydra.core.TermLet(value=lt):
            return lt.bindings

        case _:
            return ()

def types_to_definitions(type_map: FrozenDict[hydra.core.Name, hydra.core.Type]) -> frozenlist[hydra.core.Binding]:
    r"""Encode a map of named types to a list of bindings."""

    def to_element(pair: tuple[hydra.core.Name, hydra.core.Type]) -> hydra.core.Binding:
        @lru_cache(1)
        def name() -> hydra.core.Name:
            return hydra.lib.pairs.first(pair)
        return hydra.core.Binding(name(), hydra.encode.core.type(hydra.lib.pairs.second(pair)), Nothing())
    return hydra.lib.lists.map((lambda x1: to_element(x1)), hydra.lib.maps.to_list(type_map))

def with_lambda_context(get_context: Callable[[T0], hydra.graph.Graph], set_context: Callable[[hydra.graph.Graph, T0], T1], env: T0, lam: hydra.core.Lambda, body: Callable[[T1], T2]) -> T2:
    r"""Execute a computation in the context of a lambda body, extending the type context with the lambda parameter."""

    @lru_cache(1)
    def new_context() -> hydra.graph.Graph:
        return hydra.scoping.extend_graph_for_lambda(get_context(env), lam)
    return body(set_context(new_context(), env))

def with_let_context(get_context: Callable[[T0], hydra.graph.Graph], set_context: Callable[[hydra.graph.Graph, T0], T1], for_binding: Callable[[hydra.graph.Graph, hydra.core.Binding], Maybe[hydra.core.Term]], env: T0, letrec: hydra.core.Let, body: Callable[[T1], T2]) -> T2:
    r"""Execute a computation in the context of a let body, extending the type context with the let bindings."""

    @lru_cache(1)
    def new_context() -> hydra.graph.Graph:
        return hydra.scoping.extend_graph_for_let(for_binding, get_context(env), letrec)
    return body(set_context(new_context(), env))

def with_type_lambda_context(get_context: Callable[[T0], hydra.graph.Graph], set_context: Callable[[hydra.graph.Graph, T0], T1], env: T0, tlam: hydra.core.TypeLambda, body: Callable[[T1], T2]) -> T2:
    r"""Execute a computation in the context of a type lambda body, extending the type context with the type parameter."""

    @lru_cache(1)
    def new_context() -> hydra.graph.Graph:
        return hydra.scoping.extend_graph_for_type_lambda(get_context(env), tlam)
    return body(set_context(new_context(), env))
