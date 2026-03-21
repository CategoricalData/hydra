# Note: this is an automatically generated file. Do not edit.

r"""Various functions for dereferencing and decoding schema types."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.coders
import hydra.constants
import hydra.context
import hydra.core
import hydra.decode.core
import hydra.encode.core
import hydra.errors
import hydra.graph
import hydra.lexical
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
import hydra.module
import hydra.names
import hydra.reflect
import hydra.rewriting
import hydra.show.core
import hydra.sorting
import hydra.substitution
import hydra.typing
import hydra.variants

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

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
                return hydra.rewriting.type_dependency_names(True, type_def.type)

            case hydra.module.DefinitionTerm(value=term_def):
                return hydra.rewriting.term_dependency_names(True, True, True, term_def.term)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    @lru_cache(1)
    def all_names() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.unions(hydra.lib.lists.map((lambda x1: def_names(x1)), defs))
    return hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: hydra.names.namespace_of(x1)), hydra.lib.sets.to_list(all_names()))))

def is_encoded_term(t: hydra.core.Term) -> bool:
    r"""Determines whether a given term is an encoded term (meta-level term)."""

    while True:
        match hydra.rewriting.deannotate_term(t):
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
        match hydra.rewriting.deannotate_term(t):
            case hydra.core.TermApplication(value=a):
                t = a.function
                continue

            case hydra.core.TermUnion(value=i):
                return hydra.lib.equality.equal("hydra.core.Type", i.type_name.value)

            case _:
                return False

def dependency_namespaces(cx: hydra.context.Context, graph: hydra.graph.Graph, binds: bool, with_prims: bool, with_noms: bool, with_schema: bool, els: frozenlist[hydra.core.Binding]) -> Either[hydra.context.InContext[hydra.errors.Error], frozenset[hydra.module.Namespace]]:
    r"""Find dependency namespaces in all of a set of terms (Either version)."""

    def dep_names(el: hydra.core.Binding) -> Either[hydra.context.InContext[hydra.errors.Error], frozenset[hydra.core.Name]]:
        term = el.term
        @lru_cache(1)
        def deannotated_term() -> hydra.core.Term:
            return hydra.rewriting.deannotate_term(term)
        @lru_cache(1)
        def data_names() -> frozenset[hydra.core.Name]:
            return hydra.rewriting.term_dependency_names(binds, with_prims, with_noms, term)
        @lru_cache(1)
        def schema_names() -> frozenset[hydra.core.Name]:
            return hydra.lib.logic.if_else(with_schema, (lambda : hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda ts: hydra.rewriting.type_dependency_names(True, ts.type)), el.type)), (lambda : hydra.lib.sets.empty()))
        return hydra.lib.logic.if_else(is_encoded_type(deannotated_term()), (lambda : hydra.lib.eithers.map((lambda typ: hydra.lib.sets.unions((data_names(), schema_names(), hydra.rewriting.type_dependency_names(True, typ)))), hydra.lib.eithers.bimap((lambda _wc_e: hydra.context.InContext(_wc_e, hydra.context.Context(hydra.lib.lists.cons("dependency namespace (type)", cx.trace), cx.messages, cx.other))), (lambda _wc_a: _wc_a), hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_e.value)))), (lambda _a: _a), hydra.decode.core.type(graph, term))))), (lambda : hydra.lib.logic.if_else(is_encoded_term(deannotated_term()), (lambda : hydra.lib.eithers.map((lambda decoded_term: hydra.lib.sets.unions((data_names(), schema_names(), hydra.rewriting.term_dependency_names(binds, with_prims, with_noms, decoded_term)))), hydra.lib.eithers.bimap((lambda _wc_e: hydra.context.InContext(_wc_e, hydra.context.Context(hydra.lib.lists.cons("dependency namespace (term)", cx.trace), cx.messages, cx.other))), (lambda _wc_a: _wc_a), hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_e.value)))), (lambda _a: _a), hydra.decode.core.term(graph, term))))), (lambda : Right(hydra.lib.sets.unions((data_names(), schema_names())))))))
    return hydra.lib.eithers.map((lambda names_list: hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: hydra.names.namespace_of(x1)), hydra.lib.sets.to_list(hydra.lib.sets.unions(names_list)))))), hydra.lib.eithers.map_list((lambda x1: dep_names(x1)), els))

def dereference_type(cx: hydra.context.Context, graph: hydra.graph.Graph, name: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], Maybe[hydra.core.Type]]:
    r"""Dereference a type name to get the actual type (Either version)."""

    @lru_cache(1)
    def mel() -> Maybe[hydra.core.Binding]:
        return hydra.lexical.dereference_element(graph, name)
    return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda el: hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), hydra.lib.eithers.bimap((lambda _wc_e: hydra.context.InContext(_wc_e, cx)), (lambda _wc_a: _wc_a), hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_e.value)))), (lambda _a: _a), hydra.decode.core.type(graph, el.term))))), mel())

def element_as_type_application_term(cx: hydra.context.Context, el: hydra.core.Binding) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.core.TypeApplicationTerm]:
    r"""Convert an element to a typed term."""

    return hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("missing element type"))), cx))), (lambda ts: Right(hydra.core.TypeApplicationTerm(el.term, ts.type))), el.type)

def elements_with_dependencies(cx: hydra.context.Context, graph: hydra.graph.Graph, original: frozenlist[hydra.core.Binding]) -> Either[hydra.context.InContext[hydra.errors.Error], frozenlist[hydra.core.Binding]]:
    r"""Get elements with their dependencies."""

    def dep_names(el: hydra.core.Binding) -> frozenlist[hydra.core.Name]:
        return hydra.lib.sets.to_list(hydra.rewriting.term_dependency_names(True, False, False, el.term))
    @lru_cache(1)
    def all_dep_names() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.nub(hydra.lib.lists.concat2(hydra.lib.lists.map((lambda v1: v1.name), original), hydra.lib.lists.concat(hydra.lib.lists.map((lambda x1: dep_names(x1)), original))))
    return hydra.lib.eithers.map_list((lambda name: hydra.lexical.require_element(cx, graph, name)), all_dep_names())

def f_type_is_polymorphic(typ: hydra.core.Type) -> bool:
    r"""Test whether a given System F type is polymorphic (i.e., a forall type)."""

    while True:
        match typ:
            case hydra.core.TypeAnnotated(value=at):
                typ = at.body
                continue

            case hydra.core.TypeForall():
                return True

            case _:
                return False

def field_map(fields: frozenlist[hydra.core.Field]) -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    def to_pair(f: hydra.core.Field) -> tuple[hydra.core.Name, hydra.core.Term]:
        return (f.name, f.term)
    return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: to_pair(x1)), fields))

def field_type_map(fields: frozenlist[hydra.core.FieldType]) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
    def to_pair(f: hydra.core.FieldType) -> tuple[hydra.core.Name, hydra.core.Type]:
        return (f.name, f.type)
    return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: to_pair(x1)), fields))

def field_types(cx: hydra.context.Context, graph: hydra.graph.Graph, t: hydra.core.Type) -> Either[hydra.context.InContext[hydra.errors.Error], FrozenDict[hydra.core.Name, hydra.core.Type]]:
    r"""Get field types from a record or union type (Either version)."""

    def to_map(fields: frozenlist[hydra.core.FieldType]) -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda ft: (ft.name, ft.type)), fields))
    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypeForall(value=ft):
            return field_types(cx, graph, ft.body)

        case hydra.core.TypeRecord(value=rt):
            return Right(to_map(rt))

        case hydra.core.TypeUnion(value=rt2):
            return Right(to_map(rt2))

        case hydra.core.TypeVariable(value=name):
            return hydra.lib.eithers.bind(hydra.lexical.require_element(cx, graph, name), (lambda el: hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _wc_e: hydra.context.InContext(_wc_e, cx)), (lambda _wc_a: _wc_a), hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_e.value)))), (lambda _a: _a), hydra.decode.core.type(graph, el.term))), (lambda decoded_type: field_types(cx, graph, decoded_type)))))

        case _:
            return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("expected record or union type but found ", hydra.show.core.type(t)))))), cx))

def find_field_type(cx: hydra.context.Context, fname: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type]:
    r"""Find a field type by name in a list of field types."""

    @lru_cache(1)
    def matching_fields() -> frozenlist[hydra.core.FieldType]:
        return hydra.lib.lists.filter((lambda ft: hydra.lib.equality.equal(ft.name.value, fname.value)), fields)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(matching_fields()), (lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("No such field: ", fname.value)))), cx))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(matching_fields()), 1), (lambda : Right(hydra.lib.lists.head(matching_fields()).type)), (lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("Multiple fields named ", fname.value)))), cx))))))

def normal_type_variable(i: int) -> hydra.core.Name:
    r"""Type variable naming convention follows Haskell: t0, t1, etc."""

    return hydra.core.Name(hydra.lib.strings.cat2("t", hydra.lib.literals.show_int32(i)))

def fresh_name(cx: hydra.context.Context) -> tuple[hydra.core.Name, hydra.context.Context]:
    r"""Generate a fresh type variable name, threading Context."""

    @lru_cache(1)
    def count() -> int:
        return hydra.annotations.get_count(hydra.constants.key_fresh_type_variable_count, cx)
    return (normal_type_variable(count()), hydra.annotations.put_count(hydra.constants.key_fresh_type_variable_count, hydra.lib.math.add(count(), 1), cx))

def fresh_names(n: int, cx: hydra.context.Context) -> tuple[frozenlist[hydra.core.Name], hydra.context.Context]:
    r"""Generate multiple fresh type variable names, threading Context."""

    def go(acc: tuple[frozenlist[hydra.core.Name], hydra.context.Context], _: T0) -> tuple[frozenlist[hydra.core.Name], hydra.context.Context]:
        @lru_cache(1)
        def names() -> frozenlist[hydra.core.Name]:
            return hydra.lib.pairs.first(acc)
        @lru_cache(1)
        def cx0() -> hydra.context.Context:
            return hydra.lib.pairs.second(acc)
        @lru_cache(1)
        def result() -> tuple[hydra.core.Name, hydra.context.Context]:
            return fresh_name(cx0())
        @lru_cache(1)
        def name() -> hydra.core.Name:
            return hydra.lib.pairs.first(result())
        @lru_cache(1)
        def cx1() -> hydra.context.Context:
            return hydra.lib.pairs.second(result())
        return (hydra.lib.lists.concat2(names(), hydra.lib.lists.pure(name())), cx1())
    return hydra.lib.lists.foldl((lambda x1, x2: go(x1, x2)), ((), cx), hydra.lib.lists.replicate(n, None))

def fully_strip_and_normalize_type(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Fully strip a type of forall quantifiers, normalizing bound variable names for alpha-equivalence comparison."""

    def go(depth: int, subst: FrozenDict[hydra.core.Name, hydra.core.Name], t: hydra.core.Type) -> tuple[FrozenDict[hydra.core.Name, hydra.core.Name], hydra.core.Type]:
        while True:
            match hydra.rewriting.deannotate_type(t):
                case hydra.core.TypeForall(value=ft):
                    return (old_var := ft.parameter, (new_var := hydra.core.Name(hydra.lib.strings.cat2("_", hydra.lib.literals.show_int32(depth))), go(hydra.lib.math.add(depth, 1), hydra.lib.maps.insert(old_var, new_var, subst), ft.body))[1])[1]

                case _:
                    return (subst, t)
    @lru_cache(1)
    def result() -> tuple[FrozenDict[hydra.core.Name, hydra.core.Name], hydra.core.Type]:
        return go(0, hydra.lib.maps.empty(), typ)
    @lru_cache(1)
    def subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        return hydra.lib.pairs.first(result())
    @lru_cache(1)
    def body() -> hydra.core.Type:
        return hydra.lib.pairs.second(result())
    return hydra.rewriting.substitute_type_variables(subst(), body())

def fully_strip_type(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Fully strip a type of forall quantifiers."""

    while True:
        match hydra.rewriting.deannotate_type(typ):
            case hydra.core.TypeForall(value=ft):
                typ = ft.body
                continue

            case _:
                return typ

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

def instantiate_type_scheme(cx: hydra.context.Context, scheme: hydra.core.TypeScheme) -> tuple[hydra.core.TypeScheme, hydra.context.Context]:
    r"""Instantiate a type scheme with fresh variables, threading Context."""

    old_vars = scheme.variables
    @lru_cache(1)
    def result() -> tuple[frozenlist[hydra.core.Name], hydra.context.Context]:
        return fresh_names(hydra.lib.lists.length(old_vars), cx)
    @lru_cache(1)
    def new_vars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.pairs.first(result())
    @lru_cache(1)
    def cx2() -> hydra.context.Context:
        return hydra.lib.pairs.second(result())
    @lru_cache(1)
    def subst() -> hydra.typing.TypeSubst:
        return hydra.typing.TypeSubst(hydra.lib.maps.from_list(hydra.lib.lists.zip(old_vars, hydra.lib.lists.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), new_vars()))))
    @lru_cache(1)
    def name_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
        return hydra.lib.maps.from_list(hydra.lib.lists.zip(old_vars, new_vars()))
    @lru_cache(1)
    def renamed_constraints() -> Maybe[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]]:
        return hydra.lib.maybes.map((lambda old_constraints: hydra.lib.maps.from_list(hydra.lib.lists.map((lambda kv: (hydra.lib.maybes.from_maybe((lambda : hydra.lib.pairs.first(kv)), hydra.lib.maps.lookup(hydra.lib.pairs.first(kv), name_subst())), hydra.lib.pairs.second(kv))), hydra.lib.maps.to_list(old_constraints)))), scheme.constraints)
    return (hydra.core.TypeScheme(new_vars(), hydra.substitution.subst_in_type(subst(), scheme.type), renamed_constraints()), cx2())

def type_to_type_scheme(t0: hydra.core.Type) -> hydra.core.TypeScheme:
    r"""Convert a (System F -style) type to a type scheme."""

    def helper(vars: frozenlist[hydra.core.Name], t: hydra.core.Type) -> hydra.core.TypeScheme:
        while True:
            match hydra.rewriting.deannotate_type(t):
                case hydra.core.TypeForall(value=ft):
                    vars = hydra.lib.lists.cons(ft.parameter, vars)
                    t = ft.body
                    continue

                case _:
                    return hydra.core.TypeScheme(hydra.lib.lists.reverse(vars), t, Nothing())
    return helper((), t0)

def instantiate_type(cx: hydra.context.Context, typ: hydra.core.Type) -> tuple[hydra.core.Type, hydra.context.Context]:
    r"""Instantiate a type by replacing all forall-bound type variables with fresh variables, threading Context."""

    @lru_cache(1)
    def result() -> tuple[hydra.core.TypeScheme, hydra.context.Context]:
        return instantiate_type_scheme(cx, type_to_type_scheme(typ))
    return (hydra.rewriting.type_scheme_to_f_type(hydra.lib.pairs.first(result())), hydra.lib.pairs.second(result()))

def is_unit_type(v1: hydra.core.Type) -> bool:
    r"""Check whether a type is the unit type."""

    match v1:
        case hydra.core.TypeUnit():
            return True

        case _:
            return False

def is_enum_row_type(rt: frozenlist[hydra.core.FieldType]) -> bool:
    r"""Check if a row type represents an enum (all fields are unit-typed)."""

    return hydra.lib.lists.foldl(hydra.lib.logic.and_, True, hydra.lib.lists.map((lambda f: is_unit_type(hydra.rewriting.deannotate_type(f.type))), rt))

def is_enum_type(typ: hydra.core.Type) -> bool:
    r"""Check if a type is an enum type."""

    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeUnion(value=rt):
            return is_enum_row_type(rt)

        case _:
            return False

def is_nominal_type(typ: hydra.core.Type) -> bool:
    while True:
        match hydra.rewriting.deannotate_type(typ):
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
        return hydra.lib.logic.if_else(hydra.lib.sets.null(seeds), (lambda : Right(names)), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: to_pair(x1)), hydra.lib.sets.to_list(seeds)), (lambda pairs: (new_names := hydra.lib.maps.union(names, hydra.lib.maps.from_list(pairs)), refs := hydra.lib.lists.foldl((lambda x1, x2: hydra.lib.sets.union(x1, x2)), hydra.lib.sets.empty(), hydra.lib.lists.map((lambda pair: hydra.rewriting.type_dependency_names(with_schema, hydra.lib.pairs.second(pair))), pairs)), visited := hydra.lib.sets.from_list(hydra.lib.maps.keys(names)), new_seeds := hydra.lib.sets.difference(refs, visited), deps(new_seeds, new_names))[4]))))
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
        match hydra.rewriting.deannotate_type(t):
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
    return hydra.lib.lists.foldl((lambda acc, el: hydra.lib.logic.or_(acc, term_contains_binary(el.term))), False, mod.elements)

def module_dependency_namespaces(cx: hydra.context.Context, graph: hydra.graph.Graph, binds: bool, with_prims: bool, with_noms: bool, with_schema: bool, mod: hydra.module.Module) -> Either[hydra.context.InContext[hydra.errors.Error], frozenset[hydra.module.Namespace]]:
    r"""Find dependency namespaces in all elements of a module, excluding the module's own namespace (Either version)."""

    return hydra.lib.eithers.map((lambda deps: hydra.lib.sets.delete(mod.namespace, deps)), dependency_namespaces(cx, graph, binds, with_prims, with_noms, with_schema, mod.elements))

def namespaces_for_definitions(encode_namespace: Callable[[hydra.module.Namespace], T0], focus_ns: hydra.module.Namespace, defs: frozenlist[hydra.module.Definition]) -> hydra.module.Namespaces[T0]:
    r"""Create namespaces mapping for definitions."""

    @lru_cache(1)
    def nss() -> frozenset[hydra.module.Namespace]:
        return hydra.lib.sets.delete(focus_ns, definition_dependency_namespaces(defs))
    def to_pair(ns: hydra.module.Namespace) -> tuple[hydra.module.Namespace, T0]:
        return (ns, encode_namespace(ns))
    return hydra.module.Namespaces(to_pair(focus_ns), hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: to_pair(x1)), hydra.lib.sets.to_list(nss()))))

def nominal_application(tname: hydra.core.Name, args: frozenlist[hydra.core.Type]) -> hydra.core.Type:
    r"""Apply type arguments to a nominal type."""

    return hydra.lib.lists.foldl((lambda t, a: cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(t, a)))), cast(hydra.core.Type, hydra.core.TypeVariable(tname)), args)

def partition_definitions(defs: frozenlist[hydra.module.Definition]) -> tuple[frozenlist[hydra.module.TypeDefinition], frozenlist[hydra.module.TermDefinition]]:
    r"""Partition a list of definitions into type definitions and term definitions."""

    def get_type(def_: hydra.module.Definition) -> Maybe[hydra.module.TypeDefinition]:
        match def_:
            case hydra.module.DefinitionType(value=td):
                return Just(td)

            case hydra.module.DefinitionTerm():
                return Nothing()

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def get_term(def_: hydra.module.Definition) -> Maybe[hydra.module.TermDefinition]:
        match def_:
            case hydra.module.DefinitionType():
                return Nothing()

            case hydra.module.DefinitionTerm(value=td):
                return Just(td)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return (hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: get_type(x1)), defs)), hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: get_term(x1)), defs)))

def require_type(cx: hydra.context.Context, graph: hydra.graph.Graph, name: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type]:
    r"""Require a type by name."""

    return hydra.lib.maybes.maybe((lambda : hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("no such type: ", name.value)))), cx))), (lambda ts: Right(hydra.rewriting.type_scheme_to_f_type(ts))), hydra.lib.maps.lookup(name, graph.bound_types))), (lambda ts: Right(hydra.rewriting.type_scheme_to_f_type(ts))), hydra.lib.maps.lookup(name, graph.schema_types))

def require_row_type(cx: hydra.context.Context, label: str, getter: Callable[[hydra.core.Type], Maybe[T0]], graph: hydra.graph.Graph, name: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], T0]:
    r"""Require a name to resolve to a row type."""

    def raw_type(t: hydra.core.Type) -> hydra.core.Type:
        while True:
            match t:
                case hydra.core.TypeAnnotated(value=at):
                    t = at.body
                    continue

                case hydra.core.TypeForall(value=ft):
                    t = ft.body
                    continue

                case _:
                    return t
    return hydra.lib.eithers.bind(require_type(cx, graph, name), (lambda t: hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat((name.value, " does not resolve to a ", label, " type: ", hydra.show.core.type(t)))))), cx))), (lambda x: Right(x)), getter(raw_type(t)))))

def require_record_type(cx: hydra.context.Context, graph: hydra.graph.Graph, name: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], frozenlist[hydra.core.FieldType]]:
    r"""Require a name to resolve to a record type."""

    def to_record(t: hydra.core.Type) -> Maybe[frozenlist[hydra.core.FieldType]]:
        match t:
            case hydra.core.TypeRecord(value=rt):
                return Just(rt)

            case _:
                return Nothing()
    return require_row_type(cx, "record type", (lambda x1: to_record(x1)), graph, name)

def require_schema_type(cx: hydra.context.Context, types: FrozenDict[hydra.core.Name, hydra.core.TypeScheme], tname: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], tuple[hydra.core.TypeScheme, hydra.context.Context]]:
    r"""Look up a schema type and instantiate it, threading Context."""

    return hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("No such schema type: ", tname.value, ". Available types are: ", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda v1: v1.value), hydra.lib.maps.keys(types)))))))), cx))), (lambda ts: Right(instantiate_type_scheme(cx, hydra.rewriting.deannotate_type_scheme_recursive(ts)))), hydra.lib.maps.lookup(tname, types))

def require_union_type(cx: hydra.context.Context, graph: hydra.graph.Graph, name: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], frozenlist[hydra.core.FieldType]]:
    r"""Require a name to resolve to a union type."""

    def to_union(t: hydra.core.Type) -> Maybe[frozenlist[hydra.core.FieldType]]:
        match t:
            case hydra.core.TypeUnion(value=rt):
                return Just(rt)

            case _:
                return Nothing()
    return require_row_type(cx, "union", (lambda x1: to_union(x1)), graph, name)

def require_union_field(cx: hydra.context.Context, graph: hydra.graph.Graph, tname: hydra.core.Name, fname: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type]:
    r"""Require a field type from a union type."""

    def with_row_type(rt: frozenlist[hydra.core.FieldType]) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type]:
        @lru_cache(1)
        def matches() -> frozenlist[hydra.core.FieldType]:
            return hydra.lib.lists.filter((lambda ft: hydra.lib.equality.equal(ft.name, fname)), rt)
        return hydra.lib.logic.if_else(hydra.lib.lists.null(matches()), (lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("no field \"", fname.value, "\" in union type \"", tname.value))))), cx))), (lambda : Right(hydra.lib.lists.head(matches()).type)))
    return hydra.lib.eithers.bind(require_union_type(cx, graph, tname), (lambda x1: with_row_type(x1)))

def resolve_type(graph: hydra.graph.Graph, typ: hydra.core.Type) -> Maybe[hydra.core.Type]:
    r"""Resolve a type, dereferencing type variables."""

    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeVariable(value=name):
            return hydra.lib.maybes.maybe((lambda : hydra.lib.maybes.map((lambda ts: hydra.rewriting.type_scheme_to_f_type(ts)), hydra.lib.maps.lookup(name, graph.bound_types))), (lambda ts: Just(hydra.rewriting.type_scheme_to_f_type(ts))), hydra.lib.maps.lookup(name, graph.schema_types))

        case _:
            return Just(typ)

def schema_graph_to_typing_environment(cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.context.InContext[hydra.errors.Error], FrozenDict[hydra.core.Name, hydra.core.TypeScheme]]:
    r"""Convert a schema graph to a typing environment (Either version)."""

    def to_type_scheme(vars: frozenlist[hydra.core.Name], typ: hydra.core.Type) -> hydra.core.TypeScheme:
        while True:
            match hydra.rewriting.deannotate_type(typ):
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
        return hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : hydra.lib.eithers.map((lambda typ: Just(hydra.rewriting.f_type_to_type_scheme(typ))), decode_type(el.term))), (lambda ts: hydra.lib.logic.if_else(hydra.lib.equality.equal(ts, hydra.core.TypeScheme((), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.TypeScheme"))), Nothing())), (lambda : hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), decode_type_scheme(el.term))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(ts, hydra.core.TypeScheme((), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))), Nothing())), (lambda : hydra.lib.eithers.map((lambda decoded: Just(to_type_scheme((), decoded))), decode_type(el.term))), (lambda : for_term(hydra.rewriting.deannotate_term(el.term))))))), el.type), (lambda mts: Right(hydra.lib.maybes.map((lambda ts: (el.name, ts)), mts))))
    return hydra.lib.eithers.map((lambda mpairs: hydra.lib.maps.from_list(hydra.lib.maybes.cat(mpairs))), hydra.lib.eithers.map_list((lambda x1: to_pair(x1)), hydra.lexical.graph_to_bindings(g)))

def term_as_bindings(term: hydra.core.Term) -> frozenlist[hydra.core.Binding]:
    r"""Extract the bindings from a let term, or return an empty list for other terms."""

    match hydra.rewriting.deannotate_term(term):
        case hydra.core.TermLet(value=lt):
            return lt.bindings

        case _:
            return ()

def topological_sort_type_definitions(defs: frozenlist[hydra.module.TypeDefinition]) -> frozenlist[frozenlist[hydra.module.TypeDefinition]]:
    r"""Topologically sort type definitions by dependencies."""

    def to_pair(def_: hydra.module.TypeDefinition) -> tuple[hydra.core.Name, frozenlist[hydra.core.Name]]:
        return (def_.name, hydra.lib.sets.to_list(hydra.rewriting.type_dependency_names(False, def_.type)))
    @lru_cache(1)
    def name_to_def() -> FrozenDict[hydra.core.Name, hydra.module.TypeDefinition]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda d: (d.name, d)), defs))
    @lru_cache(1)
    def sorted() -> frozenlist[frozenlist[hydra.core.Name]]:
        return hydra.sorting.topological_sort_components(hydra.lib.lists.map((lambda x1: to_pair(x1)), defs))
    return hydra.lib.lists.map((lambda names: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, name_to_def())), names))), sorted())

def types_to_elements(type_map: FrozenDict[hydra.core.Name, hydra.core.Type]) -> frozenlist[hydra.core.Binding]:
    r"""Encode a map of named types to a list of elements."""

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
        return hydra.rewriting.extend_graph_for_lambda(get_context(env), lam)
    return body(set_context(new_context(), env))

def with_let_context(get_context: Callable[[T0], hydra.graph.Graph], set_context: Callable[[hydra.graph.Graph, T0], T1], for_binding: Callable[[hydra.graph.Graph, hydra.core.Binding], Maybe[hydra.core.Term]], env: T0, letrec: hydra.core.Let, body: Callable[[T1], T2]) -> T2:
    r"""Execute a computation in the context of a let body, extending the type context with the let bindings."""

    @lru_cache(1)
    def new_context() -> hydra.graph.Graph:
        return hydra.rewriting.extend_graph_for_let(for_binding, get_context(env), letrec)
    return body(set_context(new_context(), env))

def with_type_lambda_context(get_context: Callable[[T0], hydra.graph.Graph], set_context: Callable[[hydra.graph.Graph, T0], T1], env: T0, tlam: hydra.core.TypeLambda, body: Callable[[T1], T2]) -> T2:
    r"""Execute a computation in the context of a type lambda body, extending the type context with the type parameter."""

    @lru_cache(1)
    def new_context() -> hydra.graph.Graph:
        return hydra.rewriting.extend_graph_for_type_lambda(get_context(env), tlam)
    return body(set_context(new_context(), env))
