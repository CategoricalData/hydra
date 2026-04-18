# Note: this is an automatically generated file. Do not edit.

r"""Utilities for working with Haskell syntax trees."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Maybe, Right, frozenlist
from typing import TypeVar, cast
import hydra.analysis
import hydra.core
import hydra.formatting
import hydra.haskell.language
import hydra.haskell.syntax
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.packaging
import hydra.strip

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def application_pattern(name: hydra.haskell.syntax.Name, args: frozenlist[hydra.haskell.syntax.Pattern]) -> hydra.haskell.syntax.Pattern:
    r"""Create an application pattern from a name and argument patterns."""

    return cast(hydra.haskell.syntax.Pattern, hydra.haskell.syntax.PatternApplication(hydra.haskell.syntax.ApplicationPattern(name, args)))

def raw_name(n: str) -> hydra.haskell.syntax.Name:
    r"""Create a raw Haskell name from a string without sanitization."""

    return cast(hydra.haskell.syntax.Name, hydra.haskell.syntax.NameNormal(hydra.haskell.syntax.QualifiedName((), hydra.haskell.syntax.NamePart(n))))

def sanitize_haskell_name(v1: str) -> str:
    r"""Sanitize a string to be a valid Haskell identifier, escaping reserved words."""

    return hydra.formatting.sanitize_with_underscores(hydra.haskell.language.reserved_words(), v1)

def simple_name(arg_: str) -> hydra.haskell.syntax.Name:
    r"""Create a sanitized Haskell name from a string."""

    return raw_name(sanitize_haskell_name(arg_))

def element_reference(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], name: hydra.core.Name) -> hydra.haskell.syntax.Name:
    r"""Generate a Haskell name reference for a Hydra element."""

    @lru_cache(1)
    def namespace_pair() -> tuple[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName]:
        return namespaces.focus
    @lru_cache(1)
    def gname() -> hydra.packaging.Namespace:
        return hydra.lib.pairs.first(namespace_pair())
    @lru_cache(1)
    def gmod() -> str:
        return hydra.lib.pairs.second(namespace_pair()).value
    @lru_cache(1)
    def namespaces_map() -> FrozenDict[hydra.packaging.Namespace, hydra.haskell.syntax.ModuleName]:
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

def hsapp(l: hydra.haskell.syntax.Expression, r: hydra.haskell.syntax.Expression) -> hydra.haskell.syntax.Expression:
    r"""Create a Haskell function application expression."""

    return cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionApplication(hydra.haskell.syntax.ApplicationExpression(l, r)))

def hslambda(name: hydra.haskell.syntax.Name, rhs: hydra.haskell.syntax.Expression) -> hydra.haskell.syntax.Expression:
    r"""Create a Haskell lambda expression."""

    return cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionLambda(hydra.haskell.syntax.LambdaExpression((cast(hydra.haskell.syntax.Pattern, hydra.haskell.syntax.PatternName(name)),), rhs)))

def hslit(lit: hydra.haskell.syntax.Literal) -> hydra.haskell.syntax.Expression:
    r"""Create a Haskell literal expression."""

    return cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionLiteral(lit))

def hsvar(s: str) -> hydra.haskell.syntax.Expression:
    r"""Create a Haskell variable expression from a string."""

    return cast(hydra.haskell.syntax.Expression, hydra.haskell.syntax.ExpressionVariable(raw_name(s)))

def namespaces_for_module(mod: hydra.packaging.Module, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName]]:
    r"""Compute the Haskell module namespaces for a Hydra module."""

    return hydra.lib.eithers.bind(hydra.analysis.module_dependency_namespaces(cx, g, True, True, True, True, mod), (lambda nss: (ns := mod.namespace, segments_of := (lambda namespace: hydra.lib.strings.split_on(".", namespace.value)), alias_from_suffix := (lambda segs, n: (drop_count := hydra.lib.math.sub(hydra.lib.lists.length(segs), n), suffix := hydra.lib.lists.drop(drop_count, segs), capitalized_suffix := hydra.lib.lists.map(hydra.formatting.capitalize, suffix), hydra.haskell.syntax.ModuleName(hydra.lib.strings.cat(capitalized_suffix)))[3]), to_module_name := (lambda namespace: alias_from_suffix(segments_of(namespace), 1)), focus_pair := (ns, to_module_name(ns)), nss_as_list := hydra.lib.sets.to_list(nss), segs_map := hydra.lib.maps.from_list(hydra.lib.lists.map((lambda nm: (nm, segments_of(nm))), nss_as_list)), max_segs := hydra.lib.lists.foldl((lambda a, b: hydra.lib.logic.if_else(hydra.lib.equality.gt(a, b), (lambda : a), (lambda : b))), 1, hydra.lib.lists.map((lambda nm: hydra.lib.lists.length(segments_of(nm))), nss_as_list)), initial_state := hydra.lib.maps.from_list(hydra.lib.lists.map((lambda nm: (nm, 1)), nss_as_list)), segs_for := (lambda nm: hydra.lib.maybes.from_maybe((lambda : ()), hydra.lib.maps.lookup(nm, segs_map))), taken_for := (lambda state, nm: hydra.lib.maybes.from_maybe((lambda : 1), hydra.lib.maps.lookup(nm, state))), grow_step := (lambda state, _ign: (alias_entries := hydra.lib.lists.map((lambda nm: (segs := segs_for(nm), n := taken_for(state, nm), seg_count := hydra.lib.lists.length(segs), alias_str := alias_from_suffix(segs, n).value, (nm, (n, (seg_count, alias_str))))[4]), nss_as_list), alias_counts := hydra.lib.lists.foldl((lambda m, e: (k := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(e))), hydra.lib.maps.insert(k, hydra.lib.math.add(1, hydra.lib.maybes.from_maybe((lambda : 0), hydra.lib.maps.lookup(k, m))), m))[1]), hydra.lib.maps.empty(), alias_entries), alias_min_segs := hydra.lib.lists.foldl((lambda m, e: (seg_count := hydra.lib.pairs.first(hydra.lib.pairs.second(hydra.lib.pairs.second(e))), k := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(e))), existing := hydra.lib.maps.lookup(k, m), hydra.lib.maps.insert(k, hydra.lib.maybes.cases(existing, (lambda : seg_count), (lambda prev: hydra.lib.logic.if_else(hydra.lib.equality.lt(seg_count, prev), (lambda : seg_count), (lambda : prev)))), m))[3]), hydra.lib.maps.empty(), alias_entries), alias_min_segs_count := hydra.lib.lists.foldl((lambda m, e: (seg_count := hydra.lib.pairs.first(hydra.lib.pairs.second(hydra.lib.pairs.second(e))), k := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(e))), min_segs := hydra.lib.maybes.from_maybe((lambda : seg_count), hydra.lib.maps.lookup(k, alias_min_segs)), hydra.lib.logic.if_else(hydra.lib.equality.equal(seg_count, min_segs), (lambda : hydra.lib.maps.insert(k, hydra.lib.math.add(1, hydra.lib.maybes.from_maybe((lambda : 0), hydra.lib.maps.lookup(k, m))), m)), (lambda : m)))[3]), hydra.lib.maps.empty(), alias_entries), hydra.lib.maps.from_list(hydra.lib.lists.map((lambda e: (nm := hydra.lib.pairs.first(e), n := hydra.lib.pairs.first(hydra.lib.pairs.second(e)), seg_count := hydra.lib.pairs.first(hydra.lib.pairs.second(hydra.lib.pairs.second(e))), alias_str := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(e))), count := hydra.lib.maybes.from_maybe((lambda : 0), hydra.lib.maps.lookup(alias_str, alias_counts)), min_segs := hydra.lib.maybes.from_maybe((lambda : seg_count), hydra.lib.maps.lookup(alias_str, alias_min_segs)), min_segs_count := hydra.lib.maybes.from_maybe((lambda : 0), hydra.lib.maps.lookup(alias_str, alias_min_segs_count)), can_grow := hydra.lib.logic.and_(hydra.lib.equality.gt(count, 1), hydra.lib.logic.and_(hydra.lib.equality.gt(seg_count, n), hydra.lib.logic.or_(hydra.lib.equality.gt(seg_count, min_segs), hydra.lib.equality.gt(min_segs_count, 1)))), new_n := hydra.lib.logic.if_else(can_grow, (lambda : hydra.lib.math.add(n, 1)), (lambda : n)), (nm, new_n))[9]), alias_entries)))[4]), final_state := hydra.lib.lists.foldl((lambda x1, x2: grow_step(x1, x2)), initial_state, hydra.lib.lists.replicate(max_segs, None)), result_map := hydra.lib.maps.from_list(hydra.lib.lists.map((lambda nm: (nm, alias_from_suffix(segs_for(nm), taken_for(final_state, nm)))), nss_as_list)), Right(hydra.packaging.Namespaces(focus_pair, result_map)))[14]))

def newtype_accessor_name(name: hydra.core.Name) -> str:
    r"""Generate an accessor name for a newtype wrapper (e.g., 'unFoo' for Foo)."""

    return hydra.lib.strings.cat2("un", hydra.names.local_name_of(name))

def type_name_for_record(sname: hydra.core.Name) -> str:
    r"""Extract the local type name from a fully qualified record type name."""

    sname_str = sname.value
    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", sname_str)
    return hydra.lib.maybes.from_maybe((lambda : sname_str), hydra.lib.lists.maybe_last(parts()))

def record_field_reference(namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], sname: hydra.core.Name, fname: hydra.core.Name) -> hydra.haskell.syntax.Name:
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

def simple_value_binding(hname: hydra.haskell.syntax.Name, rhs: hydra.haskell.syntax.Expression, bindings: Maybe[hydra.haskell.syntax.LocalBindings]) -> hydra.haskell.syntax.ValueBinding:
    r"""Create a simple value binding (e.g., 'foo = expr' or 'foo = expr where ...')."""

    @lru_cache(1)
    def pat() -> hydra.haskell.syntax.Pattern:
        return cast(hydra.haskell.syntax.Pattern, hydra.haskell.syntax.PatternApplication(hydra.haskell.syntax.ApplicationPattern(hname, ())))
    right_hand_side = hydra.haskell.syntax.RightHandSide(rhs)
    return cast(hydra.haskell.syntax.ValueBinding, hydra.haskell.syntax.ValueBindingSimple(hydra.haskell.syntax.SimpleValueBinding(pat(), right_hand_side, bindings)))

def to_type_application(types: frozenlist[hydra.haskell.syntax.Type]) -> hydra.haskell.syntax.Type:
    r"""Convert a list of types into a nested type application."""

    @lru_cache(1)
    def dummy_type() -> hydra.haskell.syntax.Type:
        return cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeVariable(cast(hydra.haskell.syntax.Name, hydra.haskell.syntax.NameNormal(hydra.haskell.syntax.QualifiedName((), hydra.haskell.syntax.NamePart(""))))))
    def app(l: frozenlist[hydra.haskell.syntax.Type]) -> hydra.haskell.syntax.Type:
        return hydra.lib.maybes.from_maybe((lambda : dummy_type()), hydra.lib.maybes.map((lambda p: hydra.lib.logic.if_else(hydra.lib.lists.null(hydra.lib.pairs.second(p)), (lambda : hydra.lib.pairs.first(p)), (lambda : cast(hydra.haskell.syntax.Type, hydra.haskell.syntax.TypeApplication(hydra.haskell.syntax.ApplicationType(app(hydra.lib.pairs.second(p)), hydra.lib.pairs.first(p))))))), hydra.lib.lists.uncons(l)))
    return app(hydra.lib.lists.reverse(types))

def union_field_reference(bound_names: frozenset[hydra.core.Name], namespaces: hydra.packaging.Namespaces[hydra.haskell.syntax.ModuleName], sname: hydra.core.Name, fname: hydra.core.Name) -> hydra.haskell.syntax.Name:
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
