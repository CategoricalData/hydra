# Note: this is an automatically generated file. Do not edit.

r"""A module for lexical operations over graphs."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.context
import hydra.core
import hydra.errors
import hydra.graph
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.show.core
import hydra.strip

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def build_graph(elements: frozenlist[hydra.core.Binding], environment: FrozenDict[hydra.core.Name, Maybe[hydra.core.Term]], primitives: FrozenDict[hydra.core.Name, hydra.graph.Primitive]) -> hydra.graph.Graph:
    r"""Build a Graph from element bindings, environment, and primitives."""

    @lru_cache(1)
    def element_terms() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda b: (b.name, b.term)), elements))
    @lru_cache(1)
    def let_terms() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return hydra.lib.maps.map((lambda mt: hydra.lib.maybes.from_just(mt)), hydra.lib.maps.filter((lambda mt: hydra.lib.maybes.is_just(mt)), environment))
    @lru_cache(1)
    def merged_terms() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return hydra.lib.maps.union(element_terms(), let_terms())
    @lru_cache(1)
    def filtered_terms() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return hydra.lib.maps.filter_with_key((lambda k, _v: hydra.lib.logic.not_(hydra.lib.maps.member(k, primitives))), merged_terms())
    @lru_cache(1)
    def element_types() -> FrozenDict[hydra.core.Name, hydra.core.TypeScheme]:
        return hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda b: hydra.lib.maybes.map((lambda ts: (b.name, ts)), b.type)), elements)))
    @lru_cache(1)
    def filtered_types() -> FrozenDict[hydra.core.Name, hydra.core.TypeScheme]:
        return hydra.lib.maps.filter_with_key((lambda k, _v: hydra.lib.logic.not_(hydra.lib.maps.member(k, primitives))), element_types())
    return hydra.graph.Graph(filtered_terms(), filtered_types(), hydra.lib.maps.empty(), hydra.lib.sets.from_list(hydra.lib.maps.keys(hydra.lib.maps.filter((lambda mt: hydra.lib.maybes.is_nothing(mt)), environment))), hydra.lib.maps.empty(), primitives, hydra.lib.maps.empty(), hydra.lib.sets.empty())

def choose_unique_name(reserved: frozenset[hydra.core.Name], name: hydra.core.Name) -> hydra.core.Name:
    def try_name(index: int) -> hydra.core.Name:
        @lru_cache(1)
        def candidate() -> hydra.core.Name:
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(index, 1), (lambda : name), (lambda : hydra.core.Name(hydra.lib.strings.cat2(name.value, hydra.lib.literals.show_int32(index)))))
        return hydra.lib.logic.if_else(hydra.lib.sets.member(candidate(), reserved), (lambda : try_name(hydra.lib.math.add(index, 1))), (lambda : candidate()))
    return try_name(1)

def dereference_schema_type(name: hydra.core.Name, types: FrozenDict[hydra.core.Name, hydra.core.TypeScheme]) -> Maybe[hydra.core.TypeScheme]:
    r"""Resolve a schema type through a chain of zero or more typedefs."""

    def for_type(t: hydra.core.Type) -> Maybe[hydra.core.TypeScheme]:
        match t:
            case hydra.core.TypeAnnotated(value=at):
                return for_type(at.body)

            case hydra.core.TypeForall(value=ft):
                return hydra.lib.maybes.map((lambda ts: hydra.core.TypeScheme(hydra.lib.lists.cons(ft.parameter, ts.variables), ts.type, ts.constraints)), for_type(ft.body))

            case hydra.core.TypeVariable(value=v):
                return dereference_schema_type(v, types)

            case _:
                return Just(hydra.core.TypeScheme((), t, Nothing()))
    return hydra.lib.maybes.bind(hydra.lib.maps.lookup(name, types), (lambda ts: hydra.lib.maybes.map((lambda ts2: hydra.core.TypeScheme(hydra.lib.lists.concat2(ts.variables, ts2.variables), ts2.type, ts2.constraints)), for_type(ts.type))))

def lookup_binding(graph: hydra.graph.Graph, name: hydra.core.Name) -> Maybe[hydra.core.Binding]:
    r"""Look up a binding in a graph by name."""

    return hydra.lib.maybes.map((lambda term: hydra.core.Binding(name, term, hydra.lib.maps.lookup(name, graph.bound_types))), hydra.lib.maps.lookup(name, graph.bound_terms))

def dereference_variable(graph: hydra.graph.Graph, name: hydra.core.Name) -> Either[hydra.errors.Error, hydra.core.Binding]:
    r"""Look up a binding by name in a graph, returning Either an error or the binding."""

    return hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorResolution(cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorNoSuchBinding(hydra.errors.NoSuchBindingError(name))))))), (lambda right_: Right(right_)), lookup_binding(graph, name))

def elements_to_graph(parent: hydra.graph.Graph, schema_types: FrozenDict[hydra.core.Name, hydra.core.TypeScheme], elements: frozenlist[hydra.core.Binding]) -> hydra.graph.Graph:
    r"""Create a graph from a parent graph, schema types, and list of element bindings."""

    prims = parent.primitives
    @lru_cache(1)
    def g() -> hydra.graph.Graph:
        return build_graph(elements, hydra.lib.maps.empty(), prims)
    return hydra.graph.Graph(g().bound_terms, g().bound_types, g().class_constraints, g().lambda_variables, g().metadata, g().primitives, schema_types, g().type_variables)

@lru_cache(1)
def empty_context() -> hydra.context.Context:
    r"""An empty context; no trace, no messages, no other data."""

    return hydra.context.Context((), (), hydra.lib.maps.empty())

@lru_cache(1)
def empty_graph() -> hydra.graph.Graph:
    r"""An empty graph; no elements, no primitives, no schema."""

    return hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty())

def fields_of(t: hydra.core.Type) -> frozenlist[hydra.core.FieldType]:
    r"""Extract the fields of a record or union type."""

    while True:
        @lru_cache(1)
        def stripped() -> hydra.core.Type:
            return hydra.strip.deannotate_type(t)
        match stripped():
            case hydra.core.TypeForall(value=forall_type):
                t = forall_type.body
                continue

            case hydra.core.TypeRecord(value=rt):
                return rt

            case hydra.core.TypeUnion(value=rt2):
                return rt2

            case _:
                return ()

def get_field(m: FrozenDict[hydra.core.Name, T0], fname: hydra.core.Name, decode: Callable[[T0], Either[hydra.errors.Error, T1]]) -> Either[hydra.errors.Error, T1]:
    return hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorResolution(cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorNoMatchingField(hydra.errors.NoMatchingFieldError(fname))))))), decode, hydra.lib.maps.lookup(fname, m))

def graph_to_bindings(g: hydra.graph.Graph) -> frozenlist[hydra.core.Binding]:
    r"""Reconstruct a list of Bindings from a Graph's boundTerms and boundTypes."""

    return hydra.lib.lists.map((lambda p: (name := hydra.lib.pairs.first(p), term := hydra.lib.pairs.second(p), hydra.core.Binding(name, term, hydra.lib.maps.lookup(name, g.bound_types)))[2]), hydra.lib.maps.to_list(g.bound_terms))

def graph_with_primitives(built_in: frozenlist[hydra.graph.Primitive], user_provided: frozenlist[hydra.graph.Primitive]) -> hydra.graph.Graph:
    r"""Build a graph with primitives assembled from built-in and user-provided lists. User-provided primitives shadow built-in ones."""

    def to_map(ps: frozenlist[hydra.graph.Primitive]) -> FrozenDict[hydra.core.Name, hydra.graph.Primitive]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda p: (p.name, p)), ps))
    @lru_cache(1)
    def prims() -> FrozenDict[hydra.core.Name, hydra.graph.Primitive]:
        return hydra.lib.maps.union(to_map(user_provided), to_map(built_in))
    return build_graph((), hydra.lib.maps.empty(), prims())

def lookup_primitive(graph: hydra.graph.Graph, name: hydra.core.Name) -> Maybe[hydra.graph.Primitive]:
    r"""Look up a primitive function in a graph by name."""

    return hydra.lib.maps.lookup(name, graph.primitives)

def lookup_term(graph: hydra.graph.Graph, name: hydra.core.Name) -> Maybe[hydra.core.Term]:
    r"""Look up a term by name in a graph."""

    return hydra.lib.maps.lookup(name, graph.bound_terms)

def require_binding(graph: hydra.graph.Graph, name: hydra.core.Name) -> Either[hydra.errors.Error, hydra.core.Binding]:
    show_all = False
    def ellipsis(strings: frozenlist[str]) -> frozenlist[str]:
        return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.gt(hydra.lib.lists.length(strings), 3), hydra.lib.logic.not_(show_all)), (lambda : hydra.lib.lists.concat2(hydra.lib.lists.take(3, strings), ("...",))), (lambda : strings))
    @lru_cache(1)
    def err_msg() -> str:
        return hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("no such element: ", name.value), ". Available elements: {"), hydra.lib.strings.intercalate(", ", ellipsis(hydra.lib.lists.map((lambda v1: v1.value), hydra.lib.maps.keys(graph.bound_terms))))), "}")
    return hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorResolution(cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorOther(hydra.errors.OtherResolutionError(err_msg()))))))), (lambda x: Right(x)), lookup_binding(graph, name))

def match_union(graph: hydra.graph.Graph, tname: hydra.core.Name, pairs: frozenlist[tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.Error, T0]]]], term: hydra.core.Term) -> Either[hydra.errors.Error, T0]:
    @lru_cache(1)
    def stripped() -> hydra.core.Term:
        return hydra.strip.deannotate_and_detype_term(term)
    @lru_cache(1)
    def mapping() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.Error, T0]]]:
        return hydra.lib.maps.from_list(pairs)
    match stripped():
        case hydra.core.TermVariable(value=name):
            return hydra.lib.eithers.bind(require_binding(graph, name), (lambda el: match_union(graph, tname, pairs, el.term)))

        case hydra.core.TermInject(value=injection):
            @lru_cache(1)
            def exp() -> Either[hydra.errors.Error, T0]:
                fname = injection.field.name
                val = injection.field.term
                return hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorResolution(cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorNoMatchingField(hydra.errors.NoMatchingFieldError(fname))))))), (lambda f: f(val)), hydra.lib.maps.lookup(fname, mapping()))
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(injection.type_name.value, tname.value), (lambda : exp()), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorResolution(cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat2("injection for type ", tname.value), hydra.show.core.term(term)))))))))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorResolution(cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat2("injection for type ", tname.value), hydra.show.core.term(stripped())))))))

def match_unit_field(fname: T0, x: T1) -> tuple[T0, Callable[[T2], Either[T3, T1]]]:
    return (fname, (lambda ignored: Right(x)))

def match_enum(graph: hydra.graph.Graph, tname: hydra.core.Name, pairs: frozenlist[tuple[hydra.core.Name, T0]], v1: hydra.core.Term) -> Either[hydra.errors.Error, T0]:
    return match_union(graph, tname, hydra.lib.lists.map((lambda pair: match_unit_field(hydra.lib.pairs.first(pair), hydra.lib.pairs.second(pair))), pairs), v1)

def match_record(graph: T0, decode: Callable[[FrozenDict[hydra.core.Name, hydra.core.Term]], Either[hydra.errors.Error, T1]], term: hydra.core.Term) -> Either[hydra.errors.Error, T1]:
    @lru_cache(1)
    def stripped() -> hydra.core.Term:
        return hydra.strip.deannotate_and_detype_term(term)
    match stripped():
        case hydra.core.TermRecord(value=record):
            return decode(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda field: (field.name, field.term)), record.fields)))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorResolution(cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("record", hydra.show.core.term(term)))))))

def require_primitive(graph: hydra.graph.Graph, name: hydra.core.Name) -> Either[hydra.errors.Error, hydra.graph.Primitive]:
    return hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorResolution(cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorNoSuchPrimitive(hydra.errors.NoSuchPrimitiveError(name))))))), (lambda x: Right(x)), lookup_primitive(graph, name))

def require_primitive_type(tx: hydra.graph.Graph, name: hydra.core.Name) -> Either[hydra.errors.Error, hydra.core.TypeScheme]:
    @lru_cache(1)
    def mts() -> Maybe[hydra.core.TypeScheme]:
        return hydra.lib.maybes.map((lambda _p: _p.type), hydra.lib.maps.lookup(name, tx.primitives))
    return hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorResolution(cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorNoSuchPrimitive(hydra.errors.NoSuchPrimitiveError(name))))))), (lambda ts: Right(ts)), mts())

def resolve_term(graph: hydra.graph.Graph, name: hydra.core.Name) -> Maybe[hydra.core.Term]:
    r"""TODO: distinguish between lambda-bound and let-bound variables."""

    def recurse(term: hydra.core.Term) -> Maybe[hydra.core.Term]:
        @lru_cache(1)
        def stripped() -> hydra.core.Term:
            return hydra.strip.deannotate_term(term)
        match stripped():
            case hydra.core.TermVariable(value=name_):
                return resolve_term(graph, name_)

            case _:
                return Just(term)
    return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda x1: recurse(x1)), lookup_term(graph, name))

def require_term(graph: hydra.graph.Graph, name: hydra.core.Name) -> Either[hydra.errors.Error, hydra.core.Term]:
    return hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorResolution(cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorNoSuchBinding(hydra.errors.NoSuchBindingError(name))))))), (lambda x: Right(x)), resolve_term(graph, name))

def strip_and_dereference_term(graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    @lru_cache(1)
    def stripped() -> hydra.core.Term:
        return hydra.strip.deannotate_and_detype_term(term)
    match stripped():
        case hydra.core.TermVariable(value=v):
            return hydra.lib.eithers.bind(require_term(graph, v), (lambda t: strip_and_dereference_term(graph, t)))

        case _:
            return Right(stripped())

def strip_and_dereference_term_either(graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Strip annotations and dereference variables, returning Either an error or the resolved term."""

    @lru_cache(1)
    def stripped() -> hydra.core.Term:
        return hydra.strip.deannotate_and_detype_term(term)
    match stripped():
        case hydra.core.TermVariable(value=v):
            return hydra.lib.eithers.either((lambda left_: Left(left_)), (lambda binding: strip_and_dereference_term_either(graph, binding.term)), dereference_variable(graph, v))

        case _:
            return Right(stripped())
