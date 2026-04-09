# Note: this is an automatically generated file. Do not edit.

r"""Utilities for working with Haskell syntax trees."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Maybe, Right, frozenlist
from typing import TypeVar, cast
import hydra.analysis
import hydra.core
import hydra.ext.haskell.language
import hydra.ext.haskell.syntax
import hydra.formatting
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.packaging
import hydra.strip

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def application_pattern(name: hydra.ext.haskell.syntax.Name, args: frozenlist[hydra.ext.haskell.syntax.Pattern]) -> hydra.ext.haskell.syntax.Pattern:
    r"""Create an application pattern from a name and argument patterns."""

    return cast(hydra.ext.haskell.syntax.Pattern, hydra.ext.haskell.syntax.PatternApplication(hydra.ext.haskell.syntax.ApplicationPattern(name, args)))

def raw_name(n: str) -> hydra.ext.haskell.syntax.Name:
    r"""Create a raw Haskell name from a string without sanitization."""

    return cast(hydra.ext.haskell.syntax.Name, hydra.ext.haskell.syntax.NameNormal(hydra.ext.haskell.syntax.QualifiedName((), hydra.ext.haskell.syntax.NamePart(n))))

def sanitize_haskell_name(v1: str) -> str:
    r"""Sanitize a string to be a valid Haskell identifier, escaping reserved words."""

    return hydra.formatting.sanitize_with_underscores(hydra.ext.haskell.language.reserved_words(), v1)

def simple_name(arg_: str) -> hydra.ext.haskell.syntax.Name:
    r"""Create a sanitized Haskell name from a string."""

    return raw_name(sanitize_haskell_name(arg_))

def element_reference(namespaces: hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName], name: hydra.core.Name) -> hydra.ext.haskell.syntax.Name:
    r"""Generate a Haskell name reference for a Hydra element."""

    @lru_cache(1)
    def namespace_pair() -> tuple[hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName]:
        return namespaces.focus
    @lru_cache(1)
    def gname() -> hydra.packaging.Namespace:
        return hydra.lib.pairs.first(namespace_pair())
    @lru_cache(1)
    def gmod() -> str:
        return hydra.lib.pairs.second(namespace_pair()).value
    @lru_cache(1)
    def namespaces_map() -> FrozenDict[hydra.packaging.Namespace, hydra.ext.haskell.syntax.ModuleName]:
        return namespaces.mapping
    @lru_cache(1)
    def qname() -> hydra.packaging.QualifiedName:
        return hydra.names.qualify_name(name)
    local = qname().local
    @lru_cache(1)
    def esc_local() -> str:
        return sanitize_haskell_name(local)
    mns = qname().namespace
    return hydra.lib.maybes.cases(qname().namespace, (lambda : simple_name(local)), (lambda ns: hydra.lib.maybes.cases(hydra.lib.maps.lookup(ns, namespaces_map()), (lambda : simple_name(local)), (lambda mn: (alias_str := mn.value, hydra.lib.logic.if_else(hydra.lib.equality.equal(ns, gname()), (lambda : simple_name(esc_local())), (lambda : raw_name(hydra.lib.strings.cat((alias_str, ".", sanitize_haskell_name(local)))))))[1]))))

def hsapp(l: hydra.ext.haskell.syntax.Expression, r: hydra.ext.haskell.syntax.Expression) -> hydra.ext.haskell.syntax.Expression:
    r"""Create a Haskell function application expression."""

    return cast(hydra.ext.haskell.syntax.Expression, hydra.ext.haskell.syntax.ExpressionApplication(hydra.ext.haskell.syntax.ApplicationExpression(l, r)))

def hslambda(name: hydra.ext.haskell.syntax.Name, rhs: hydra.ext.haskell.syntax.Expression) -> hydra.ext.haskell.syntax.Expression:
    r"""Create a Haskell lambda expression."""

    return cast(hydra.ext.haskell.syntax.Expression, hydra.ext.haskell.syntax.ExpressionLambda(hydra.ext.haskell.syntax.LambdaExpression((cast(hydra.ext.haskell.syntax.Pattern, hydra.ext.haskell.syntax.PatternName(name)),), rhs)))

def hslit(lit: hydra.ext.haskell.syntax.Literal) -> hydra.ext.haskell.syntax.Expression:
    r"""Create a Haskell literal expression."""

    return cast(hydra.ext.haskell.syntax.Expression, hydra.ext.haskell.syntax.ExpressionLiteral(lit))

def hsvar(s: str) -> hydra.ext.haskell.syntax.Expression:
    r"""Create a Haskell variable expression from a string."""

    return cast(hydra.ext.haskell.syntax.Expression, hydra.ext.haskell.syntax.ExpressionVariable(raw_name(s)))

def namespaces_for_module(mod: hydra.packaging.Module, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName]]:
    r"""Compute the Haskell module namespaces for a Hydra module."""

    return hydra.lib.eithers.bind(hydra.analysis.module_dependency_namespaces(cx, g, True, True, True, True, mod), (lambda nss: (ns := mod.namespace, to_module_name := (lambda namespace: (namespace_str := namespace.value, parts := hydra.lib.strings.split_on(".", namespace_str), last_part := hydra.lib.lists.last(parts), capitalized := hydra.formatting.capitalize(last_part), hydra.ext.haskell.syntax.ModuleName(capitalized))[4]), to_pair := (lambda name: (name, to_module_name(name))), add_pair := (lambda state, name_pair: (current_map := hydra.lib.pairs.first(state), current_set := hydra.lib.pairs.second(state), name := hydra.lib.pairs.first(name_pair), alias := hydra.lib.pairs.second(name_pair), alias_str := alias.value, hydra.lib.logic.if_else(hydra.lib.sets.member(alias, current_set), (lambda : add_pair(state, (name, hydra.ext.haskell.syntax.ModuleName(hydra.lib.strings.cat2(alias_str, "_"))))), (lambda : (hydra.lib.maps.insert(name, alias, current_map), hydra.lib.sets.insert(alias, current_set)))))[5]), focus_pair := to_pair(ns), nss_as_list := hydra.lib.sets.to_list(nss), nss_pairs := hydra.lib.lists.map((lambda x1: to_pair(x1)), nss_as_list), empty_state := (hydra.lib.maps.empty(), hydra.lib.sets.empty()), final_state := hydra.lib.lists.foldl((lambda x1, x2: add_pair(x1, x2)), empty_state, nss_pairs), result_map := hydra.lib.pairs.first(final_state), Right(hydra.packaging.Namespaces(focus_pair, result_map)))[10]))

def newtype_accessor_name(name: hydra.core.Name) -> str:
    r"""Generate an accessor name for a newtype wrapper (e.g., 'unFoo' for Foo)."""

    return hydra.lib.strings.cat2("un", hydra.names.local_name_of(name))

def type_name_for_record(sname: hydra.core.Name) -> str:
    r"""Extract the local type name from a fully qualified record type name."""

    sname_str = sname.value
    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", sname_str)
    return hydra.lib.lists.last(parts())

def record_field_reference(namespaces: hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName], sname: hydra.core.Name, fname: hydra.core.Name) -> hydra.ext.haskell.syntax.Name:
    r"""Generate a Haskell name for a record field accessor."""

    fname_str = fname.value
    @lru_cache(1)
    def qname() -> hydra.packaging.QualifiedName:
        return hydra.names.qualify_name(sname)
    ns = qname().namespace
    @lru_cache(1)
    def type_name_str() -> str:
        return type_name_for_record(sname)
    @lru_cache(1)
    def decapitalized() -> str:
        return hydra.formatting.decapitalize(type_name_str())
    @lru_cache(1)
    def capitalized() -> str:
        return hydra.formatting.capitalize(fname_str)
    @lru_cache(1)
    def nm() -> str:
        return hydra.lib.strings.cat2(decapitalized(), capitalized())
    qual_name = hydra.packaging.QualifiedName(ns, nm())
    @lru_cache(1)
    def unqual_name() -> hydra.core.Name:
        return hydra.names.unqualify_name(qual_name)
    return element_reference(namespaces, unqual_name())

def simple_value_binding(hname: hydra.ext.haskell.syntax.Name, rhs: hydra.ext.haskell.syntax.Expression, bindings: Maybe[hydra.ext.haskell.syntax.LocalBindings]) -> hydra.ext.haskell.syntax.ValueBinding:
    r"""Create a simple value binding (e.g., 'foo = expr' or 'foo = expr where ...')."""

    @lru_cache(1)
    def pat() -> hydra.ext.haskell.syntax.Pattern:
        return cast(hydra.ext.haskell.syntax.Pattern, hydra.ext.haskell.syntax.PatternApplication(hydra.ext.haskell.syntax.ApplicationPattern(hname, ())))
    right_hand_side = hydra.ext.haskell.syntax.RightHandSide(rhs)
    return cast(hydra.ext.haskell.syntax.ValueBinding, hydra.ext.haskell.syntax.ValueBindingSimple(hydra.ext.haskell.syntax.SimpleValueBinding(pat(), right_hand_side, bindings)))

def to_type_application(types: frozenlist[hydra.ext.haskell.syntax.Type]) -> hydra.ext.haskell.syntax.Type:
    r"""Convert a list of types into a nested type application."""

    def app(l: frozenlist[hydra.ext.haskell.syntax.Type]) -> hydra.ext.haskell.syntax.Type:
        return hydra.lib.logic.if_else(hydra.lib.equality.gt(hydra.lib.lists.length(l), 1), (lambda : cast(hydra.ext.haskell.syntax.Type, hydra.ext.haskell.syntax.TypeApplication(hydra.ext.haskell.syntax.ApplicationType(app(hydra.lib.lists.tail(l)), hydra.lib.lists.head(l))))), (lambda : hydra.lib.lists.head(l)))
    return app(hydra.lib.lists.reverse(types))

def union_field_reference(bound_names: frozenset[hydra.core.Name], namespaces: hydra.packaging.Namespaces[hydra.ext.haskell.syntax.ModuleName], sname: hydra.core.Name, fname: hydra.core.Name) -> hydra.ext.haskell.syntax.Name:
    r"""Generate a Haskell name for a union variant constructor, with disambiguation."""

    fname_str = fname.value
    @lru_cache(1)
    def qname() -> hydra.packaging.QualifiedName:
        return hydra.names.qualify_name(sname)
    ns = qname().namespace
    @lru_cache(1)
    def type_name_str() -> str:
        return type_name_for_record(sname)
    @lru_cache(1)
    def capitalized_type_name() -> str:
        return hydra.formatting.capitalize(type_name_str())
    @lru_cache(1)
    def capitalized_field_name() -> str:
        return hydra.formatting.capitalize(fname_str)
    def deconflict(name: str) -> str:
        @lru_cache(1)
        def tname() -> hydra.core.Name:
            return hydra.names.unqualify_name(hydra.packaging.QualifiedName(ns, name))
        return hydra.lib.logic.if_else(hydra.lib.sets.member(tname(), bound_names), (lambda : deconflict(hydra.lib.strings.cat2(name, "_"))), (lambda : name))
    @lru_cache(1)
    def nm() -> str:
        return deconflict(hydra.lib.strings.cat2(capitalized_type_name(), capitalized_field_name()))
    qual_name = hydra.packaging.QualifiedName(ns, nm())
    @lru_cache(1)
    def unqual_name() -> hydra.core.Name:
        return hydra.names.unqualify_name(qual_name)
    return element_reference(namespaces, unqual_name())

def unpack_forall_type(t: hydra.core.Type) -> tuple[frozenlist[hydra.core.Name], hydra.core.Type]:
    r"""Unpack nested forall types into a list of type variables and the inner type."""

    match hydra.strip.deannotate_type(t):
        case hydra.core.TypeForall(value=fat):
            v = fat.parameter
            tbody = fat.body
            @lru_cache(1)
            def recursive_result() -> tuple[frozenlist[hydra.core.Name], hydra.core.Type]:
                return unpack_forall_type(tbody)
            @lru_cache(1)
            def vars() -> frozenlist[hydra.core.Name]:
                return hydra.lib.pairs.first(recursive_result())
            @lru_cache(1)
            def final_type() -> hydra.core.Type:
                return hydra.lib.pairs.second(recursive_result())
            return (hydra.lib.lists.cons(v, vars()), final_type())

        case _:
            return ((), t)
