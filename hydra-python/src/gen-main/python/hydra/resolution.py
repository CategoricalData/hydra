# Note: this is an automatically generated file. Do not edit.

r"""Type dereference, lookup, requirements, and instantiation."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.context
import hydra.core
import hydra.decode.core
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
import hydra.lib.strings
import hydra.names
import hydra.scoping
import hydra.show.core
import hydra.strip
import hydra.substitution
import hydra.typing
import hydra.variables

T0 = TypeVar("T0")

def dereference_type(cx: hydra.context.Context, graph: hydra.graph.Graph, name: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], Maybe[hydra.core.Type]]:
    r"""Dereference a type name to get the actual type (Either version)."""

    @lru_cache(1)
    def mel() -> Maybe[hydra.core.Binding]:
        return hydra.lexical.lookup_binding(graph, name)
    return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda el: hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), hydra.lib.eithers.bimap((lambda _wc_e: hydra.context.InContext(_wc_e, cx)), (lambda _wc_a: _wc_a), hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_e.value)))), (lambda _a: _a), hydra.decode.core.type(graph, el.term))))), mel())

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
    match hydra.strip.deannotate_type(t):
        case hydra.core.TypeForall(value=ft):
            return field_types(cx, graph, ft.body)

        case hydra.core.TypeRecord(value=rt):
            return Right(to_map(rt))

        case hydra.core.TypeUnion(value=rt2):
            return Right(to_map(rt2))

        case hydra.core.TypeVariable(value=name):
            return hydra.lib.maybes.maybe((lambda : hydra.lib.eithers.bind(hydra.lexical.require_binding(cx, graph, name), (lambda el: hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _wc_e: hydra.context.InContext(_wc_e, cx)), (lambda _wc_a: _wc_a), hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_e.value)))), (lambda _a: _a), hydra.decode.core.type(graph, el.term))), (lambda decoded_type: field_types(cx, graph, decoded_type)))))), (lambda ts: field_types(cx, graph, ts.type)), hydra.lib.maps.lookup(name, graph.schema_types))

        case _:
            return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("expected record or union type but found ", hydra.show.core.type(t)))))), cx))

def find_field_type(cx: hydra.context.Context, fname: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type]:
    r"""Find a field type by name in a list of field types."""

    @lru_cache(1)
    def matching_fields() -> frozenlist[hydra.core.FieldType]:
        return hydra.lib.lists.filter((lambda ft: hydra.lib.equality.equal(ft.name.value, fname.value)), fields)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(matching_fields()), (lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("No such field: ", fname.value)))), cx))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(matching_fields()), 1), (lambda : Right(hydra.lib.lists.head(matching_fields()).type)), (lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("Multiple fields named ", fname.value)))), cx))))))

def fully_strip_and_normalize_type(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Fully strip a type of forall quantifiers, normalizing bound variable names for alpha-equivalence comparison."""

    def go(depth: int, subst: FrozenDict[hydra.core.Name, hydra.core.Name], t: hydra.core.Type) -> tuple[FrozenDict[hydra.core.Name, hydra.core.Name], hydra.core.Type]:
        while True:
            match hydra.strip.deannotate_type(t):
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
    return hydra.variables.substitute_type_variables(subst(), body())

def fully_strip_type(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Fully strip a type of forall quantifiers."""

    while True:
        match hydra.strip.deannotate_type(typ):
            case hydra.core.TypeForall(value=ft):
                typ = ft.body
                continue

            case _:
                return typ

def instantiate_type_scheme(cx: hydra.context.Context, scheme: hydra.core.TypeScheme) -> tuple[hydra.core.TypeScheme, hydra.context.Context]:
    r"""Instantiate a type scheme with fresh variables, threading Context."""

    old_vars = scheme.variables
    @lru_cache(1)
    def result() -> tuple[frozenlist[hydra.core.Name], hydra.context.Context]:
        return hydra.names.fresh_names(hydra.lib.lists.length(old_vars), cx)
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
            match hydra.strip.deannotate_type(t):
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
    return (hydra.scoping.type_scheme_to_f_type(hydra.lib.pairs.first(result())), hydra.lib.pairs.second(result()))

def nominal_application(tname: hydra.core.Name, args: frozenlist[hydra.core.Type]) -> hydra.core.Type:
    r"""Apply type arguments to a nominal type."""

    return hydra.lib.lists.foldl((lambda t, a: cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(t, a)))), cast(hydra.core.Type, hydra.core.TypeVariable(tname)), args)

def require_type(cx: hydra.context.Context, graph: hydra.graph.Graph, name: hydra.core.Name) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type]:
    r"""Require a type by name."""

    return hydra.lib.maybes.maybe((lambda : hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("no such type: ", name.value)))), cx))), (lambda ts: Right(hydra.scoping.type_scheme_to_f_type(ts))), hydra.lib.maps.lookup(name, graph.bound_types))), (lambda ts: Right(hydra.scoping.type_scheme_to_f_type(ts))), hydra.lib.maps.lookup(name, graph.schema_types))

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

    return hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("No such schema type: ", tname.value, ". Available types are: ", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda v1: v1.value), hydra.lib.maps.keys(types)))))))), cx))), (lambda ts: Right(instantiate_type_scheme(cx, hydra.strip.deannotate_type_scheme_recursive(ts)))), hydra.lib.maps.lookup(tname, types))

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

    match hydra.strip.deannotate_type(typ):
        case hydra.core.TypeVariable(value=name):
            return hydra.lib.maybes.maybe((lambda : hydra.lib.maybes.map((lambda ts: hydra.scoping.type_scheme_to_f_type(ts)), hydra.lib.maps.lookup(name, graph.bound_types))), (lambda ts: Just(hydra.scoping.type_scheme_to_f_type(ts))), hydra.lib.maps.lookup(name, graph.schema_types))

        case _:
            return Just(typ)
