# Note: this is an automatically generated file. Do not edit.

r"""Utilities for working with Haskell syntax trees."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Maybe, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.ext.haskell.ast
import hydra.ext.haskell.language
import hydra.formatting
import hydra.graph
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.names
import hydra.rewriting
import hydra.schemas

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def application_pattern(name: hydra.ext.haskell.ast.Name, args: frozenlist[hydra.ext.haskell.ast.Pattern]) -> hydra.ext.haskell.ast.Pattern:
    r"""Create an application pattern from a name and argument patterns."""
    
    return cast(hydra.ext.haskell.ast.Pattern, hydra.ext.haskell.ast.PatternApplication(hydra.ext.haskell.ast.ApplicationPattern(name, args)))

def raw_name(n: str) -> hydra.ext.haskell.ast.Name:
    r"""Create a raw Haskell name from a string without sanitization."""
    
    return cast(hydra.ext.haskell.ast.Name, hydra.ext.haskell.ast.NameNormal(hydra.ext.haskell.ast.QualifiedName((), hydra.ext.haskell.ast.NamePart(n))))

def sanitize_haskell_name(v1: str) -> str:
    r"""Sanitize a string to be a valid Haskell identifier, escaping reserved words."""
    
    return hydra.formatting.sanitize_with_underscores(hydra.ext.haskell.language.reserved_words(), v1)

def simple_name(arg_: str) -> hydra.ext.haskell.ast.Name:
    r"""Create a sanitized Haskell name from a string."""
    
    return raw_name(sanitize_haskell_name(arg_))

def element_reference(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName], name: hydra.core.Name) -> hydra.ext.haskell.ast.Name:
    r"""Generate a Haskell name reference for a Hydra element."""
    
    @lru_cache(1)
    def namespace_pair() -> tuple[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName]:
        return namespaces.focus
    @lru_cache(1)
    def gname() -> hydra.module.Namespace:
        return hydra.lib.pairs.first(namespace_pair())
    @lru_cache(1)
    def gmod() -> str:
        return hydra.lib.pairs.second(namespace_pair()).value
    @lru_cache(1)
    def namespaces_map() -> FrozenDict[hydra.module.Namespace, hydra.ext.haskell.ast.ModuleName]:
        return namespaces.mapping
    @lru_cache(1)
    def qname() -> hydra.module.QualifiedName:
        return hydra.names.qualify_name(name)
    @lru_cache(1)
    def local() -> str:
        return qname().local
    @lru_cache(1)
    def esc_local() -> str:
        return sanitize_haskell_name(local())
    @lru_cache(1)
    def mns() -> Maybe[hydra.module.Namespace]:
        return qname().namespace
    return hydra.lib.maybes.cases(qname().namespace, simple_name(local()), (lambda ns: hydra.lib.maybes.cases(hydra.lib.maps.lookup(ns, namespaces_map()), simple_name(local()), (lambda mn: (alias_str := mn.value, hydra.lib.logic.if_else(hydra.lib.equality.equal(ns, gname()), (lambda : simple_name(esc_local())), (lambda : raw_name(hydra.lib.strings.cat((alias_str, ".", sanitize_haskell_name(local())))))))[1]))))

def hsapp(l: hydra.ext.haskell.ast.Expression, r: hydra.ext.haskell.ast.Expression) -> hydra.ext.haskell.ast.Expression:
    r"""Create a Haskell function application expression."""
    
    return cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionApplication(hydra.ext.haskell.ast.ApplicationExpression(l, r)))

def hslambda(name: hydra.ext.haskell.ast.Name, rhs: hydra.ext.haskell.ast.Expression) -> hydra.ext.haskell.ast.Expression:
    r"""Create a Haskell lambda expression."""
    
    return cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionLambda(hydra.ext.haskell.ast.LambdaExpression((cast(hydra.ext.haskell.ast.Pattern, hydra.ext.haskell.ast.PatternName(name)),), rhs)))

def hslit(lit: hydra.ext.haskell.ast.Literal) -> hydra.ext.haskell.ast.Expression:
    r"""Create a Haskell literal expression."""
    
    return cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionLiteral(lit))

def hsvar(s: str) -> hydra.ext.haskell.ast.Expression:
    r"""Create a Haskell variable expression from a string."""
    
    return cast(hydra.ext.haskell.ast.Expression, hydra.ext.haskell.ast.ExpressionVariable(raw_name(s)))

def namespaces_for_module(mod: hydra.module.Module) -> hydra.compute.Flow[hydra.graph.Graph, hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName]]:
    r"""Compute the Haskell module namespaces for a Hydra module."""
    
    return hydra.lib.flows.bind(hydra.schemas.module_dependency_namespaces(True, True, True, True, mod), (lambda nss: (ns := mod.namespace, focus_pair := to_pair(ns), nss_as_list := hydra.lib.sets.to_list(nss), nss_pairs := hydra.lib.lists.map((lambda x1: to_pair(x1)), nss_as_list), empty_state := (hydra.lib.maps.empty(), hydra.lib.sets.empty()), final_state := hydra.lib.lists.foldl((lambda x1, x2: add_pair(x1, x2)), empty_state, nss_pairs), result_map := hydra.lib.pairs.first(final_state), to_module_name := (lambda namespace: (namespace_str := namespace.value, parts := hydra.lib.strings.split_on(".", namespace_str), last_part := hydra.lib.lists.last(parts), capitalized := hydra.formatting.capitalize(last_part), hydra.ext.haskell.ast.ModuleName(capitalized))[4]), to_pair := (lambda name: (name, to_module_name(name))), add_pair := (lambda state, name_pair: (current_map := hydra.lib.pairs.first(state), current_set := hydra.lib.pairs.second(state), name := hydra.lib.pairs.first(name_pair), alias := hydra.lib.pairs.second(name_pair), alias_str := alias.value, hydra.lib.logic.if_else(hydra.lib.sets.member(alias, current_set), (lambda : add_pair(state, (name, hydra.ext.haskell.ast.ModuleName(hydra.lib.strings.cat2(alias_str, "_"))))), (lambda : (hydra.lib.maps.insert(name, alias, current_map), hydra.lib.sets.insert(alias, current_set)))))[5]), hydra.lib.flows.pure(hydra.module.Namespaces(focus_pair, result_map)))[10]))

def newtype_accessor_name(name: hydra.core.Name) -> str:
    r"""Generate an accessor name for a newtype wrapper (e.g., 'unFoo' for Foo)."""
    
    return hydra.lib.strings.cat2("un", hydra.names.local_name_of(name))

def type_name_for_record(sname: hydra.core.Name) -> str:
    r"""Extract the local type name from a fully qualified record type name."""
    
    @lru_cache(1)
    def sname_str() -> str:
        return sname.value
    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", sname_str())
    return hydra.lib.lists.last(parts())

def record_field_reference(namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName], sname: hydra.core.Name, fname: hydra.core.Name) -> hydra.ext.haskell.ast.Name:
    r"""Generate a Haskell name for a record field accessor."""
    
    @lru_cache(1)
    def fname_str() -> str:
        return fname.value
    @lru_cache(1)
    def qname() -> hydra.module.QualifiedName:
        return hydra.names.qualify_name(sname)
    @lru_cache(1)
    def ns() -> Maybe[hydra.module.Namespace]:
        return qname().namespace
    @lru_cache(1)
    def type_name_str() -> str:
        return type_name_for_record(sname)
    @lru_cache(1)
    def decapitalized() -> str:
        return hydra.formatting.decapitalize(type_name_str())
    @lru_cache(1)
    def capitalized() -> str:
        return hydra.formatting.capitalize(fname_str())
    @lru_cache(1)
    def nm() -> str:
        return hydra.lib.strings.cat2(decapitalized(), capitalized())
    @lru_cache(1)
    def qual_name() -> hydra.module.QualifiedName:
        return hydra.module.QualifiedName(ns(), nm())
    @lru_cache(1)
    def unqual_name() -> hydra.core.Name:
        return hydra.names.unqualify_name(qual_name())
    return element_reference(namespaces, unqual_name())

def simple_value_binding(hname: hydra.ext.haskell.ast.Name, rhs: hydra.ext.haskell.ast.Expression, bindings: Maybe[hydra.ext.haskell.ast.LocalBindings]) -> hydra.ext.haskell.ast.ValueBinding:
    r"""Create a simple value binding (e.g., 'foo = expr' or 'foo = expr where ...')."""
    
    @lru_cache(1)
    def pat() -> hydra.ext.haskell.ast.Pattern:
        return cast(hydra.ext.haskell.ast.Pattern, hydra.ext.haskell.ast.PatternApplication(hydra.ext.haskell.ast.ApplicationPattern(hname, ())))
    @lru_cache(1)
    def right_hand_side() -> hydra.ext.haskell.ast.RightHandSide:
        return hydra.ext.haskell.ast.RightHandSide(rhs)
    return cast(hydra.ext.haskell.ast.ValueBinding, hydra.ext.haskell.ast.ValueBindingSimple(hydra.ext.haskell.ast.SimpleValueBinding(pat(), right_hand_side(), bindings)))

def to_type_application(types: frozenlist[hydra.ext.haskell.ast.Type]) -> hydra.ext.haskell.ast.Type:
    r"""Convert a list of types into a nested type application."""
    
    def app(l: frozenlist[hydra.ext.haskell.ast.Type]) -> hydra.ext.haskell.ast.Type:
        return hydra.lib.logic.if_else(hydra.lib.equality.gt(hydra.lib.lists.length(l), 1), (lambda : cast(hydra.ext.haskell.ast.Type, hydra.ext.haskell.ast.TypeApplication(hydra.ext.haskell.ast.ApplicationType(app(hydra.lib.lists.tail(l)), hydra.lib.lists.head(l))))), (lambda : hydra.lib.lists.head(l)))
    return app(hydra.lib.lists.reverse(types))

def union_field_reference(g: hydra.graph.Graph, namespaces: hydra.module.Namespaces[hydra.ext.haskell.ast.ModuleName], sname: hydra.core.Name, fname: hydra.core.Name) -> hydra.ext.haskell.ast.Name:
    r"""Generate a Haskell name for a union variant constructor, with disambiguation."""
    
    @lru_cache(1)
    def fname_str() -> str:
        return fname.value
    @lru_cache(1)
    def qname() -> hydra.module.QualifiedName:
        return hydra.names.qualify_name(sname)
    @lru_cache(1)
    def ns() -> Maybe[hydra.module.Namespace]:
        return qname().namespace
    @lru_cache(1)
    def type_name_str() -> str:
        return type_name_for_record(sname)
    @lru_cache(1)
    def capitalized_type_name() -> str:
        return hydra.formatting.capitalize(type_name_str())
    @lru_cache(1)
    def capitalized_field_name() -> str:
        return hydra.formatting.capitalize(fname_str())
    def deconflict(name: str) -> str:
        @lru_cache(1)
        def tname() -> hydra.core.Name:
            return hydra.names.unqualify_name(hydra.module.QualifiedName(ns(), name))
        return hydra.lib.logic.if_else(hydra.lib.maybes.is_just(hydra.lib.lists.find((lambda b: hydra.lib.equality.equal(b.name, tname())), g.elements)), (lambda : deconflict(hydra.lib.strings.cat2(name, "_"))), (lambda : name))
    @lru_cache(1)
    def nm() -> str:
        return deconflict(hydra.lib.strings.cat2(capitalized_type_name(), capitalized_field_name()))
    @lru_cache(1)
    def qual_name() -> hydra.module.QualifiedName:
        return hydra.module.QualifiedName(ns(), nm())
    @lru_cache(1)
    def unqual_name() -> hydra.core.Name:
        return hydra.names.unqualify_name(qual_name())
    return element_reference(namespaces, unqual_name())

def unpack_forall_type(cx: T0, t: hydra.core.Type) -> tuple[frozenlist[hydra.core.Name], hydra.core.Type]:
    match hydra.rewriting.deannotate_type(t):
        case hydra.core.TypeForall(value=fat):
            @lru_cache(1)
            def v() -> hydra.core.Name:
                return fat.parameter
            @lru_cache(1)
            def tbody() -> hydra.core.Type:
                return fat.body
            @lru_cache(1)
            def recursive_result() -> tuple[frozenlist[hydra.core.Name], hydra.core.Type]:
                return unpack_forall_type(cx, tbody())
            @lru_cache(1)
            def vars() -> frozenlist[hydra.core.Name]:
                return hydra.lib.pairs.first(recursive_result())
            @lru_cache(1)
            def final_type() -> hydra.core.Type:
                return hydra.lib.pairs.second(recursive_result())
            return (hydra.lib.lists.cons(v(), vars()), final_type())
        
        case _:
            return ((), t)
