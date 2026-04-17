# Note: this is an automatically generated file. Do not edit.

r"""Functions for generating domain-specific DSL modules from type modules."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Just, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.constants
import hydra.core
import hydra.decode.core
import hydra.encode.core
import hydra.errors
import hydra.formatting
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.names
import hydra.packaging
import hydra.strip

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def collect_forall_vars(typ: hydra.core.Type) -> frozenlist[hydra.core.Name]:
    r"""Collect forall type variable names from a type."""

    match typ:
        case hydra.core.TypeAnnotated(value=at):
            return collect_forall_vars(at.body)

        case hydra.core.TypeForall(value=ft):
            return hydra.lib.lists.cons(ft.parameter, collect_forall_vars(ft.body))

        case _:
            return ()

def find_unique_name(candidate: str, used_names: frozenlist[str]):
    r"""Find a unique name by appending underscores."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(hydra.lib.lists.filter((lambda v1: hydra.lib.equality.equal(candidate, v1)), used_names)), (lambda : candidate), (lambda : find_unique_name(hydra.lib.strings.cat((candidate, "_")), used_names)))

def deduplicate_bindings(bindings: frozenlist[hydra.core.Binding]) -> frozenlist[hydra.core.Binding]:
    r"""Deduplicate bindings by appending underscore suffixes to duplicate names."""

    return hydra.lib.lists.foldl((lambda acc, b: (n := b.name.value, used_names := hydra.lib.lists.map((lambda a: a.name.value), acc), unique_name := find_unique_name(n, used_names), hydra.lib.lists.concat2(acc, (hydra.core.Binding(hydra.core.Name(unique_name), b.term, b.type),)))[3]), (), bindings)

def dsl_binding_name(n: hydra.core.Name) -> hydra.core.Name:
    r"""Generate a binding name for a DSL function from a type name."""

    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", n.value)
    @lru_cache(1)
    def local_part() -> str:
        return hydra.formatting.decapitalize(hydra.names.local_name_of(n))
    local_result = hydra.core.Name(local_part())
    return hydra.lib.maybes.maybe((lambda : local_result), (lambda ns_parts: hydra.lib.maybes.maybe((lambda : local_result), (lambda ns_head_tail: (dsl_ns_parts := hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.pairs.first(ns_head_tail), "hydra"), (lambda : hydra.lib.lists.concat2(("hydra", "dsl"), hydra.lib.pairs.second(ns_head_tail))), (lambda : hydra.lib.lists.concat2(("hydra", "dsl"), ns_parts))), hydra.core.Name(hydra.lib.strings.intercalate(".", hydra.lib.lists.concat2(dsl_ns_parts, (local_part(),)))))[1]), hydra.lib.lists.uncons(ns_parts))), hydra.lib.lists.maybe_init(parts()))

def dsl_definition_name(type_name: hydra.core.Name, local_name: str) -> hydra.core.Name:
    r"""Generate a qualified DSL element name from a type name and local element name."""

    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", type_name.value)
    return hydra.lib.maybes.maybe((lambda : hydra.core.Name(local_name)), (lambda ns_parts: (dsl_ns_parts := hydra.lib.maybes.maybe((lambda : ("hydra", "dsl")), (lambda ns_head_tail: hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.pairs.first(ns_head_tail), "hydra"), (lambda : hydra.lib.lists.concat2(("hydra", "dsl"), hydra.lib.pairs.second(ns_head_tail))), (lambda : hydra.lib.lists.concat2(("hydra", "dsl"), ns_parts)))), hydra.lib.lists.uncons(ns_parts)), hydra.core.Name(hydra.lib.strings.intercalate(".", hydra.lib.lists.concat2(dsl_ns_parts, (local_name,)))))[1]), hydra.lib.lists.maybe_init(parts()))

def dsl_namespace(ns: hydra.packaging.Namespace) -> hydra.packaging.Namespace:
    r"""Generate a DSL module namespace from a source module namespace."""

    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", ns.value)
    @lru_cache(1)
    def prefix_full() -> hydra.packaging.Namespace:
        return hydra.packaging.Namespace(hydra.lib.strings.cat(("hydra.dsl.", ns.value)))
    return hydra.lib.maybes.maybe((lambda : prefix_full()), (lambda ht: hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.pairs.first(ht), "hydra"), (lambda : hydra.packaging.Namespace(hydra.lib.strings.cat(("hydra.dsl.", hydra.lib.strings.intercalate(".", hydra.lib.pairs.second(ht)))))), (lambda : prefix_full()))), hydra.lib.lists.uncons(parts()))

def is_dsl_eligible_binding(cx: T0, graph: T1, b: hydra.core.Binding) -> Either[T2, Maybe[hydra.core.Binding]]:
    r"""Check if a binding is eligible for DSL generation."""

    @lru_cache(1)
    def ns() -> Maybe[hydra.packaging.Namespace]:
        return hydra.names.namespace_of(b.name)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.maybes.maybe((lambda : ""), (lambda v1: v1.value), ns()), "hydra.phantoms"), (lambda : Right(Nothing())), (lambda : Right(Just(b))))

def filter_type_bindings(cx: T0, graph: T1, bindings: frozenlist[hydra.core.Binding]) -> Either[T2, frozenlist[hydra.core.Binding]]:
    r"""Filter bindings to only DSL-eligible type definitions."""

    return hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.cat(x1)), hydra.lib.eithers.map_list((lambda v1: is_dsl_eligible_binding(cx, graph, v1)), hydra.lib.lists.filter((lambda x1: hydra.annotations.is_native_type(x1)), bindings)))

def dsl_type_scheme(orig_type: hydra.core.Type, param_types: frozenlist[hydra.core.Type], result_type: hydra.core.Type) -> hydra.core.TypeScheme:
    r"""Build a TypeScheme with TTerm-wrapped parameter and result types."""

    @lru_cache(1)
    def type_vars() -> frozenlist[hydra.core.Name]:
        return collect_forall_vars(orig_type)
    @lru_cache(1)
    def wrapped_result() -> hydra.core.Type:
        return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.phantoms.TTerm"))), result_type)))
    @lru_cache(1)
    def fun_type() -> hydra.core.Type:
        return hydra.lib.lists.foldr((lambda param_type, acc: cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.phantoms.TTerm"))), param_type))), acc)))), wrapped_result(), param_types)
    return hydra.core.TypeScheme(type_vars(), fun_type(), Nothing())

def nominal_result_type(type_name: hydra.core.Name, orig_type: hydra.core.Type) -> hydra.core.Type:
    r"""Build the nominal result type with type applications for forall variables."""

    @lru_cache(1)
    def vars() -> frozenlist[hydra.core.Name]:
        return collect_forall_vars(orig_type)
    return hydra.lib.lists.foldl((lambda acc, v: cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(acc, cast(hydra.core.Type, hydra.core.TypeVariable(v)))))), cast(hydra.core.Type, hydra.core.TypeVariable(type_name)), vars())

def generate_record_accessor(orig_type: hydra.core.Type, type_name: hydra.core.Name, ft: hydra.core.FieldType) -> hydra.core.Binding:
    r"""Generate a record field accessor function."""

    field_name = ft.name
    @lru_cache(1)
    def accessor_local_name() -> str:
        return hydra.lib.strings.cat((hydra.formatting.decapitalize(hydra.names.local_name_of(type_name)), hydra.lib.strings.intercalate("", hydra.lib.lists.map((lambda s: hydra.formatting.capitalize(s)), hydra.lib.strings.split_on(".", field_name.value)))))
    @lru_cache(1)
    def accessor_name() -> hydra.core.Name:
        return dsl_definition_name(type_name, accessor_local_name())
    @lru_cache(1)
    def param_domain() -> hydra.core.Type:
        return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.phantoms.TTerm"))), nominal_result_type(type_name, orig_type))))
    @lru_cache(1)
    def body() -> hydra.core.Term:
        return cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("x"), Just(param_domain()), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.phantoms.TTerm"), cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("application"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Application"), (hydra.core.Field(hydra.core.Name("function"), cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("project"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Projection"), (hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(type_name.value)))))))), hydra.core.Field(hydra.core.Name("field"), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(field_name.value))))))))))))))))), hydra.core.Field(hydra.core.Name("argument"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.phantoms.TTerm"))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x")))))))))))))))))))))
    @lru_cache(1)
    def ts() -> hydra.core.TypeScheme:
        return dsl_type_scheme(orig_type, (nominal_result_type(type_name, orig_type),), ft.type)
    return hydra.core.Binding(accessor_name(), body(), Just(ts()))

def generate_record_constructor(orig_type: hydra.core.Type, type_name: hydra.core.Name, field_types: frozenlist[hydra.core.FieldType]) -> frozenlist[hydra.core.Binding]:
    r"""Generate a record constructor function."""

    @lru_cache(1)
    def d_fields() -> frozenlist[hydra.core.Term]:
        return hydra.lib.lists.map((lambda ft: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Field"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(ft.name.value)))))))), hydra.core.Field(hydra.core.Name("term"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.phantoms.TTerm"))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name(hydra.formatting.decapitalize(hydra.names.local_name_of(ft.name)))))))))))))), field_types)
    @lru_cache(1)
    def record_term() -> hydra.core.Term:
        return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.phantoms.TTerm"), cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("record"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Record"), (hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(type_name.value)))))))), hydra.core.Field(hydra.core.Name("fields"), cast(hydra.core.Term, hydra.core.TermList(d_fields()))))))))))))))
    @lru_cache(1)
    def param_pairs() -> frozenlist[tuple[str, hydra.core.Type]]:
        return hydra.lib.lists.map((lambda ft: (hydra.formatting.decapitalize(hydra.names.local_name_of(ft.name)), cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.phantoms.TTerm"))), ft.type))))), field_types)
    @lru_cache(1)
    def body() -> hydra.core.Term:
        return hydra.lib.lists.foldl((lambda acc, pp: cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name(hydra.lib.pairs.first(pp)), Just(hydra.lib.pairs.second(pp)), acc)))), record_term(), hydra.lib.lists.reverse(param_pairs()))
    @lru_cache(1)
    def param_types() -> frozenlist[hydra.core.Type]:
        return hydra.lib.lists.map((lambda ft: ft.type), field_types)
    @lru_cache(1)
    def result_type() -> hydra.core.Type:
        return nominal_result_type(type_name, orig_type)
    @lru_cache(1)
    def ts() -> hydra.core.TypeScheme:
        return dsl_type_scheme(orig_type, param_types(), result_type())
    return (hydra.core.Binding(dsl_binding_name(type_name), body(), Just(ts())),)

def generate_record_with_updater(orig_type: hydra.core.Type, type_name: hydra.core.Name, all_fields: frozenlist[hydra.core.FieldType], target_field: hydra.core.FieldType) -> hydra.core.Binding:
    r"""Generate a withXxx record field updater function."""

    target_field_name = target_field.name
    @lru_cache(1)
    def updater_local_name() -> str:
        return hydra.lib.strings.cat((hydra.formatting.decapitalize(hydra.names.local_name_of(type_name)), "With", hydra.lib.strings.intercalate("", hydra.lib.lists.map((lambda s: hydra.formatting.capitalize(s)), hydra.lib.strings.split_on(".", target_field_name.value)))))
    @lru_cache(1)
    def updater_name() -> hydra.core.Name:
        return dsl_definition_name(type_name, updater_local_name())
    @lru_cache(1)
    def d_fields() -> frozenlist[hydra.core.Term]:
        return hydra.lib.lists.map((lambda ft: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Field"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(ft.name.value)))))))), hydra.core.Field(hydra.core.Name("term"), hydra.lib.logic.if_else(hydra.lib.equality.equal(ft.name.value, target_field_name.value), (lambda : cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.phantoms.TTerm"))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("newVal"))))))), (lambda : cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("application"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Application"), (hydra.core.Field(hydra.core.Name("function"), cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("project"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Projection"), (hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(type_name.value)))))))), hydra.core.Field(hydra.core.Name("field"), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(ft.name.value))))))))))))))))), hydra.core.Field(hydra.core.Name("argument"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.phantoms.TTerm"))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("original"))))))))))))))))))))))), all_fields)
    @lru_cache(1)
    def rec_domain() -> hydra.core.Type:
        return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.phantoms.TTerm"))), nominal_result_type(type_name, orig_type))))
    @lru_cache(1)
    def field_domain() -> hydra.core.Type:
        return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.phantoms.TTerm"))), target_field.type)))
    @lru_cache(1)
    def body() -> hydra.core.Term:
        return cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("original"), Just(rec_domain()), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("newVal"), Just(field_domain()), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.phantoms.TTerm"), cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("record"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Record"), (hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(type_name.value)))))))), hydra.core.Field(hydra.core.Name("fields"), cast(hydra.core.Term, hydra.core.TermList(d_fields()))))))))))))))))))))
    @lru_cache(1)
    def rec_type() -> hydra.core.Type:
        return nominal_result_type(type_name, orig_type)
    @lru_cache(1)
    def ts() -> hydra.core.TypeScheme:
        return dsl_type_scheme(orig_type, (rec_type(), target_field.type), rec_type())
    return hydra.core.Binding(updater_name(), body(), Just(ts()))

def generate_union_injector(orig_type: hydra.core.Type, type_name: hydra.core.Name, ft: hydra.core.FieldType) -> hydra.core.Binding:
    r"""Generate a union injection helper."""

    field_name = ft.name
    field_type = ft.type
    @lru_cache(1)
    def injector_local_name() -> str:
        return hydra.lib.strings.cat((hydra.formatting.decapitalize(hydra.names.local_name_of(type_name)), hydra.lib.strings.intercalate("", hydra.lib.lists.map((lambda s: hydra.formatting.capitalize(s)), hydra.lib.strings.split_on(".", field_name.value)))))
    @lru_cache(1)
    def injector_name() -> hydra.core.Name:
        return dsl_definition_name(type_name, injector_local_name())
    @lru_cache(1)
    def is_unit():
        def _hoist_is_unit_1(v1):
            match v1:
                case hydra.core.TypeUnit():
                    return True

                case _:
                    return False
        return _hoist_is_unit_1(hydra.strip.deannotate_type(field_type))
    @lru_cache(1)
    def d_field_value() -> hydra.core.Term:
        return hydra.lib.logic.if_else(is_unit(), (lambda : cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("unit"), cast(hydra.core.Term, hydra.core.TermUnit())))))), (lambda : cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.phantoms.TTerm"))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x"))))))))
    @lru_cache(1)
    def injection_term() -> hydra.core.Term:
        return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.phantoms.TTerm"), cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("inject"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Injection"), (hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(type_name.value)))))))), hydra.core.Field(hydra.core.Name("field"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Field"), (hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(field_name.value)))))))), hydra.core.Field(hydra.core.Name("term"), d_field_value())))))))))))))))))
    @lru_cache(1)
    def variant_domain() -> hydra.core.Type:
        return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.phantoms.TTerm"))), ft.type)))
    @lru_cache(1)
    def body() -> hydra.core.Term:
        return hydra.lib.logic.if_else(is_unit(), (lambda : injection_term()), (lambda : cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("x"), Just(variant_domain()), injection_term())))))
    @lru_cache(1)
    def union_type() -> hydra.core.Type:
        return nominal_result_type(type_name, orig_type)
    @lru_cache(1)
    def ts() -> hydra.core.TypeScheme:
        return hydra.lib.logic.if_else(is_unit(), (lambda : dsl_type_scheme(orig_type, (), union_type())), (lambda : dsl_type_scheme(orig_type, (ft.type,), union_type())))
    return hydra.core.Binding(injector_name(), body(), Just(ts()))

def generate_wrapped_type_accessors(orig_type: hydra.core.Type, type_name: hydra.core.Name, inner_type: hydra.core.Type) -> frozenlist[hydra.core.Binding]:
    r"""Generate wrap/unwrap accessors for a wrapped type."""

    @lru_cache(1)
    def local_name() -> str:
        return hydra.names.local_name_of(type_name)
    @lru_cache(1)
    def wrap_name() -> hydra.core.Name:
        return dsl_definition_name(type_name, hydra.formatting.decapitalize(local_name()))
    @lru_cache(1)
    def unwrap_local_name() -> str:
        return hydra.lib.strings.cat(("un", local_name()))
    @lru_cache(1)
    def unwrap_name() -> hydra.core.Name:
        return dsl_definition_name(type_name, unwrap_local_name())
    @lru_cache(1)
    def wrapper_type() -> hydra.core.Type:
        return nominal_result_type(type_name, orig_type)
    @lru_cache(1)
    def wrap_domain() -> hydra.core.Type:
        return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.phantoms.TTerm"))), inner_type)))
    @lru_cache(1)
    def wrap_body() -> hydra.core.Term:
        return cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("x"), Just(wrap_domain()), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.phantoms.TTerm"), cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("wrap"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedTerm"), (hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(type_name.value)))))))), hydra.core.Field(hydra.core.Name("body"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.phantoms.TTerm"))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x")))))))))))))))))))))
    @lru_cache(1)
    def unwrap_domain() -> hydra.core.Type:
        return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.phantoms.TTerm"))), wrapper_type())))
    @lru_cache(1)
    def unwrap_body() -> hydra.core.Term:
        return cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("x"), Just(unwrap_domain()), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.phantoms.TTerm"), cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("application"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Application"), (hydra.core.Field(hydra.core.Name("function"), cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("unwrap"), cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(type_name.value)))))))))))), hydra.core.Field(hydra.core.Name("argument"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermUnwrap(hydra.core.Name("hydra.phantoms.TTerm"))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x")))))))))))))))))))))
    @lru_cache(1)
    def wrap_ts() -> hydra.core.TypeScheme:
        return dsl_type_scheme(orig_type, (inner_type,), wrapper_type())
    @lru_cache(1)
    def unwrap_ts() -> hydra.core.TypeScheme:
        return dsl_type_scheme(orig_type, (wrapper_type(),), inner_type)
    return (hydra.core.Binding(wrap_name(), wrap_body(), Just(wrap_ts())), hydra.core.Binding(unwrap_name(), unwrap_body(), Just(unwrap_ts())))

def generate_bindings_for_type(cx: T0, graph: hydra.graph.Graph, b: hydra.core.Binding):
    r"""Generate all DSL bindings for a type binding."""

    type_name = b.name
    return hydra.lib.eithers.bind(hydra.decode.core.type(graph, b.term), (lambda raw_type: (typ := hydra.strip.deannotate_type_parameters(hydra.strip.deannotate_type(raw_type)), _hoist_typ_body_1 := (lambda v1: (lambda fts: hydra.lib.lists.concat((generate_record_constructor(raw_type, type_name, fts), hydra.lib.lists.map((lambda v12: generate_record_accessor(raw_type, type_name, v12)), fts), hydra.lib.lists.map((lambda v12: generate_record_with_updater(raw_type, type_name, fts, v12)), fts))))(v1.value) if isinstance(v1, hydra.core.TypeRecord) else (lambda fts: hydra.lib.lists.map((lambda v12: generate_union_injector(raw_type, type_name, v12)), fts))(v1.value) if isinstance(v1, hydra.core.TypeUnion) else (lambda inner_type: generate_wrapped_type_accessors(raw_type, type_name, inner_type))(v1.value) if isinstance(v1, hydra.core.TypeWrap) else ()), Right(_hoist_typ_body_1(typ)))[2]))

def dsl_module(cx: T0, graph: hydra.graph.Graph, mod: hydra.packaging.Module):
    def _hoist_hydra_dsls_dsl_module_1(v1):
        match v1:
            case hydra.packaging.DefinitionType(value=td):
                return Just((schema_term := cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.core.Type"))), (data_term := hydra.annotations.normalize_term_annotations(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(hydra.encode.core.type(td.type.type), hydra.lib.maps.from_list(((hydra.constants.key_type, schema_term),)))))), hydra.core.Binding(td.name, data_term, Just(hydra.core.TypeScheme((), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))), Nothing()))))[1])[1])

            case _:
                return Nothing()
    return hydra.lib.eithers.bind(filter_type_bindings(cx, graph, hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_hydra_dsls_dsl_module_1(d)), mod.definitions))), (lambda type_bindings: hydra.lib.logic.if_else(hydra.lib.lists.null(type_bindings), (lambda : Right(Nothing())), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda b: hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorDecoding(_e))), (lambda x: x), generate_bindings_for_type(cx, graph, b))), type_bindings), (lambda dsl_bindings: Right(Just(hydra.packaging.Module(dsl_namespace(mod.namespace), hydra.lib.lists.map((lambda b: cast(hydra.packaging.Definition, hydra.packaging.DefinitionTerm(hydra.packaging.TermDefinition(b.name, b.term, b.type)))), deduplicate_bindings(hydra.lib.lists.concat(dsl_bindings))), hydra.lib.lists.nub(hydra.lib.lists.map((lambda x1: dsl_namespace(x1)), mod.type_dependencies)), hydra.lib.lists.nub(hydra.lib.lists.concat2((mod.namespace, hydra.packaging.Namespace("hydra.phantoms")), mod.type_dependencies)), Just(hydra.lib.strings.cat(("DSL functions for ", mod.namespace.value))))))))))))
