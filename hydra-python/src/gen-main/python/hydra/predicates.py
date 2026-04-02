# Note: this is an automatically generated file. Do not edit.

r"""Type and term classification predicates."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Right, frozenlist
from typing import cast
import hydra.coders
import hydra.context
import hydra.core
import hydra.decode.core
import hydra.dependencies
import hydra.errors
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.reflect
import hydra.rewriting
import hydra.strip
import hydra.variants

def is_encoded_term(t: hydra.core.Term) -> bool:
    r"""Determines whether a given term is an encoded term (meta-level term)."""

    while True:
        match hydra.strip.deannotate_term(t):
            case hydra.core.TermApplication(value=a):
                t = a.function
                continue

            case hydra.core.TermUnion(value=i):
                return hydra.lib.equality.equal("hydra.core.Term", i.type_name.value)

            case _:
                return False

def is_encoded_type(t: hydra.core.Term) -> bool:
    r"""Determines whether a given term is an encoded type."""

    while True:
        match hydra.strip.deannotate_term(t):
            case hydra.core.TermApplication(value=a):
                t = a.function
                continue

            case hydra.core.TermUnion(value=i):
                return hydra.lib.equality.equal("hydra.core.Type", i.type_name.value)

            case _:
                return False

def is_unit_type(v1: hydra.core.Type) -> bool:
    r"""Check whether a type is the unit type."""

    match v1:
        case hydra.core.TypeUnit():
            return True

        case _:
            return False

def is_enum_row_type(rt: frozenlist[hydra.core.FieldType]) -> bool:
    r"""Check if a row type represents an enum (all fields are unit-typed)."""

    return hydra.lib.lists.foldl(hydra.lib.logic.and_, True, hydra.lib.lists.map((lambda f: is_unit_type(hydra.strip.deannotate_type(f.type))), rt))

def is_enum_type(typ: hydra.core.Type) -> bool:
    r"""Check if a type is an enum type."""

    match hydra.strip.deannotate_type(typ):
        case hydra.core.TypeUnion(value=rt):
            return is_enum_row_type(rt)

        case _:
            return False

def is_nominal_type(typ: hydra.core.Type) -> bool:
    while True:
        match hydra.strip.deannotate_type(typ):
            case hydra.core.TypeRecord():
                return True

            case hydra.core.TypeUnion():
                return True

            case hydra.core.TypeWrap():
                return True

            case hydra.core.TypeForall(value=fa):
                typ = fa.body
                continue

            case _:
                return False

def type_dependencies(cx: hydra.context.Context, graph: hydra.graph.Graph, with_schema: bool, transform: Callable[[hydra.core.Type], hydra.core.Type], name: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], FrozenDict[hydra.core.Name, hydra.core.Type]]:
    r"""Get all type dependencies for a given type name (Either version)."""

    def require_type(name2: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type]:
        @lru_cache(1)
        def cx1() -> hydra.context.Context:
            return hydra.context.Context(hydra.lib.lists.cons(hydra.lib.strings.cat2("type dependencies of ", name2.value), cx.trace), cx.messages, cx.other)
        return hydra.lib.eithers.bind(hydra.lexical.require_element(cx1(), graph, name2), (lambda el: hydra.lib.eithers.bimap((lambda _wc_e: hydra.context.InContext(_wc_e, cx1())), (lambda _wc_a: _wc_a), hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_e.value)))), (lambda _a: _a), hydra.decode.core.type(graph, el.term)))))
    def to_pair(name2: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], tuple[hydra.core.Name, hydra.core.Type]]:
        return hydra.lib.eithers.map((lambda typ: (name2, transform(typ))), require_type(name2))
    def deps(seeds: frozenset[hydra.core.Name], names: FrozenDict[hydra.core.Name, hydra.core.Type]) -> Either[hydra.context.InContext[hydra.errors.Error], FrozenDict[hydra.core.Name, hydra.core.Type]]:
        return hydra.lib.logic.if_else(hydra.lib.sets.null(seeds), (lambda : Right(names)), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: to_pair(x1)), hydra.lib.sets.to_list(seeds)), (lambda pairs: (new_names := hydra.lib.maps.union(names, hydra.lib.maps.from_list(pairs)), refs := hydra.lib.lists.foldl((lambda x1, x2: hydra.lib.sets.union(x1, x2)), hydra.lib.sets.empty(), hydra.lib.lists.map((lambda pair: hydra.dependencies.type_dependency_names(with_schema, hydra.lib.pairs.second(pair))), pairs)), visited := hydra.lib.sets.from_list(hydra.lib.maps.keys(names)), new_seeds := hydra.lib.sets.difference(refs, visited), deps(new_seeds, new_names))[4]))))
    return deps(hydra.lib.sets.singleton(name), hydra.lib.maps.empty())

def is_serializable(cx: hydra.context.Context, graph: hydra.graph.Graph, el: hydra.core.Binding) -> Either[hydra.context.InContext[hydra.errors.Error], bool]:
    r"""Check if an element is serializable (no function types in dependencies) (Either version)."""

    def variants(typ: hydra.core.Type) -> frozenlist[hydra.variants.TypeVariant]:
        return hydra.lib.lists.map((lambda x1: hydra.reflect.type_variant(x1)), hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda m, t: hydra.lib.lists.cons(t, m)), (), typ))
    return hydra.lib.eithers.map((lambda deps: (all_variants := hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda x1: variants(x1)), hydra.lib.maps.elems(deps)))), hydra.lib.logic.not_(hydra.lib.sets.member(hydra.variants.TypeVariant.FUNCTION, all_variants)))[1]), type_dependencies(cx, graph, False, (lambda x1: hydra.lib.equality.identity(x1)), el.name))

def is_serializable_by_name(cx: hydra.context.Context, graph: hydra.graph.Graph, name: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], bool]:
    r"""Check if a type (by name) is serializable, resolving all type dependencies (Either version)."""

    def variants(typ: hydra.core.Type) -> frozenlist[hydra.variants.TypeVariant]:
        return hydra.lib.lists.map((lambda x1: hydra.reflect.type_variant(x1)), hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda m, t: hydra.lib.lists.cons(t, m)), (), typ))
    return hydra.lib.eithers.map((lambda deps: (all_variants := hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda x1: variants(x1)), hydra.lib.maps.elems(deps)))), hydra.lib.logic.not_(hydra.lib.sets.member(hydra.variants.TypeVariant.FUNCTION, all_variants)))[1]), type_dependencies(cx, graph, False, (lambda x1: hydra.lib.equality.identity(x1)), name))

def is_serializable_type(typ: hydra.core.Type) -> bool:
    r"""Check if a type is serializable (no function types in the type itself)."""

    @lru_cache(1)
    def all_variants() -> frozenset[hydra.variants.TypeVariant]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda x1: hydra.reflect.type_variant(x1)), hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda m, t: hydra.lib.lists.cons(t, m)), (), typ)))
    return hydra.lib.logic.not_(hydra.lib.sets.member(hydra.variants.TypeVariant.FUNCTION, all_variants()))

def is_type(t: hydra.core.Type) -> bool:
    r"""Check whether a type is a type (always true for non-encoded types)."""

    while True:
        match hydra.strip.deannotate_type(t):
            case hydra.core.TypeApplication(value=a):
                t = a.function
                continue

            case hydra.core.TypeForall(value=l):
                t = l.body
                continue

            case hydra.core.TypeUnion():
                return False

            case hydra.core.TypeVariable(value=v):
                return hydra.lib.equality.equal(v, hydra.core.Name("hydra.core.Type"))

            case _:
                return False

def is_unit_term(v1: hydra.core.Term) -> bool:
    r"""Check whether a term is the unit term."""

    match v1:
        case hydra.core.TermUnit():
            return True

        case _:
            return False
