# Note: this is an automatically generated file. Do not edit.

r"""Variable substitution in type and term expressions."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.rewriting
import hydra.typing

T0 = TypeVar("T0")

def subst_in_type(subst: hydra.typing.TypeSubst, typ0: hydra.core.Type) -> hydra.core.Type:
    r"""Apply a type substitution to a type."""
    
    return hydra.lib.logic.if_else(hydra.lib.maps.null(subst.value), (lambda : typ0), (lambda : subst_in_type_non_empty(subst, typ0)))

def subst_in_type_non_empty(subst: hydra.typing.TypeSubst, typ0: hydra.core.Type) -> hydra.core.Type:
    r"""Apply a non-empty type substitution to a type (internal helper)."""
    
    def rewrite(recurse: Callable[[hydra.core.Type], hydra.core.Type], typ: hydra.core.Type) -> hydra.core.Type:
        match typ:
            case hydra.core.TypeForall(value=lt):
                return hydra.lib.maybes.maybe(recurse(typ), (lambda styp: cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(lt.parameter, subst_in_type(remove_var(lt.parameter), lt.body))))), hydra.lib.maps.lookup(lt.parameter, subst.value))
            
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.maybes.maybe(typ, (lambda styp: styp), hydra.lib.maps.lookup(v, subst.value))
            
            case _:
                return recurse(typ)
    def remove_var(v: hydra.core.Name) -> hydra.core.Type:
        return hydra.typing.TypeSubst(hydra.lib.maps.delete(v, subst.value))
    return hydra.rewriting.rewrite_type(rewrite, typ0)

def compose_type_subst_non_empty(s1: hydra.typing.TypeSubst, s2: hydra.typing.TypeSubst) -> hydra.core.Type:
    r"""Compose two non-empty type substitutions (internal helper)."""
    
    def is_extra(k: hydra.core.Name, v: T0) -> bool:
        return hydra.lib.maybes.is_nothing(hydra.lib.maps.lookup(k, s1.value))
    def with_extra() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return hydra.lib.maps.filter_with_key((lambda x1, x2: is_extra(x1, x2)), s2.value)
    return hydra.typing.TypeSubst(hydra.lib.maps.union(with_extra(), hydra.lib.maps.map((lambda v1: subst_in_type(s2, v1)), s1.value)))

def compose_type_subst(s1: hydra.typing.TypeSubst, s2: hydra.typing.TypeSubst) -> hydra.core.Type:
    r"""Compose two type substitutions."""
    
    return hydra.lib.logic.if_else(hydra.lib.maps.null(s1.value), (lambda : s2), (lambda : hydra.lib.logic.if_else(hydra.lib.maps.null(s2.value), (lambda : s1), (lambda : compose_type_subst_non_empty(s1, s2)))))

def id_type_subst() -> hydra.core.Type:
    r"""The identity type substitution."""
    
    return hydra.typing.TypeSubst(hydra.lib.maps.empty())

def compose_type_subst_list(v1: frozenlist[hydra.typing.TypeSubst]) -> hydra.core.Type:
    r"""Compose a list of type substitutions."""
    
    return hydra.lib.lists.foldl(compose_type_subst, id_type_subst(), v1)

def singleton_type_subst(v: hydra.core.Name, t: hydra.core.Type) -> hydra.core.Type:
    r"""Create a type substitution with a single variable mapping."""
    
    return hydra.typing.TypeSubst(hydra.lib.maps.singleton(v, t))

def subst_in_class_constraints(subst: hydra.typing.TypeSubst, constraints: FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]) -> FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]:
    r"""Apply a type substitution to class constraints, propagating to free variables."""
    
    def subst_map() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return subst.value
    def insert_or_merge(var_name: T0, metadata: hydra.core.TypeVariableMetadata, acc: FrozenDict[T0, hydra.core.TypeVariableMetadata]) -> FrozenDict[T0, hydra.core.TypeVariableMetadata]:
        return hydra.lib.maybes.maybe(hydra.lib.maps.insert(var_name, metadata, acc), (lambda existing: (merged := hydra.core.TypeVariableMetadata(hydra.lib.sets.union(existing.classes, metadata.classes)), hydra.lib.maps.insert(var_name, merged, acc))[1]), hydra.lib.maps.lookup(var_name, acc))
    return hydra.lib.lists.foldl((lambda acc, pair: (var_name := hydra.lib.pairs.first(pair), metadata := hydra.lib.pairs.second(pair), hydra.lib.maybes.maybe(insert_or_merge(var_name, metadata, acc), (lambda target_type: (free_vars := hydra.lib.sets.to_list(hydra.rewriting.free_variables_in_type(target_type)), hydra.lib.lists.foldl((lambda acc2, free_var: insert_or_merge(free_var, metadata, acc2)), acc, free_vars))[1]), hydra.lib.maps.lookup(var_name, subst_map())))[2]), hydra.lib.maps.empty(), hydra.lib.maps.to_list(constraints))

def subst_in_type_scheme(subst: hydra.typing.TypeSubst, ts: hydra.core.TypeScheme) -> hydra.core.Type:
    r"""Apply a type substitution to a type scheme."""
    
    return hydra.core.TypeScheme(ts.variables, subst_in_type(subst, ts.type), hydra.lib.maybes.map((lambda v1: subst_in_class_constraints(subst, v1)), ts.constraints))

def subst_in_context(subst: hydra.typing.TypeSubst, cx: hydra.typing.InferenceContext) -> hydra.core.Type:
    r"""Apply a type substitution to an inference context."""
    
    def new_data_types() -> FrozenDict[hydra.core.Name, hydra.core.TypeScheme]:
        return hydra.lib.maps.map((lambda v1: subst_in_type_scheme(subst, v1)), cx.data_types)
    def new_class_constraints() -> FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]:
        return subst_in_class_constraints(subst, cx.class_constraints)
    return hydra.typing.InferenceContext(cx.schema_types, cx.primitive_types, new_data_types(), new_class_constraints(), cx.debug)

def subst_types_in_term(subst: hydra.typing.TypeSubst, term0: hydra.core.Term) -> hydra.core.Type:
    r"""Apply a type substitution to the type annotations within a term."""
    
    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Type:
        def dflt() -> hydra.core.Type:
            return recurse(term)
        def for_function(f: hydra.core.Function) -> hydra.core.Type:
            match f:
                case hydra.core.FunctionElimination():
                    return dflt()
                
                case hydra.core.FunctionLambda(value=l):
                    return for_lambda(l)
                
                case _:
                    return dflt()
        def for_lambda(l: hydra.core.Lambda) -> hydra.core.Type:
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, hydra.lib.maybes.map((lambda v1: subst_in_type(subst, v1)), l.domain), subst_types_in_term(subst, l.body))))))
        def for_let(l: hydra.core.Let) -> hydra.core.Type:
            def rewrite_binding(b: hydra.core.Binding) -> hydra.core.Type:
                return hydra.core.Binding(b.name, subst_types_in_term(subst, b.term), hydra.lib.maybes.map((lambda v1: subst_in_type_scheme(subst, v1)), b.type))
            return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map(rewrite_binding, l.bindings), subst_types_in_term(subst, l.body))))
        def for_type_application(tt: hydra.core.TypeApplicationTerm) -> hydra.core.Type:
            return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(subst_types_in_term(subst, tt.body), subst_in_type(subst, tt.type))))
        def for_type_lambda(ta: hydra.core.TypeLambda) -> hydra.core.Type:
            def param() -> hydra.core.Type:
                return ta.parameter
            def subst2() -> hydra.core.Type:
                return hydra.typing.TypeSubst(hydra.lib.maps.delete(param(), subst.value))
            return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(param(), subst_types_in_term(subst2(), ta.body))))
        match term:
            case hydra.core.TermFunction(value=f):
                return for_function(f)
            
            case hydra.core.TermLet(value=l):
                return for_let(l)
            
            case hydra.core.TermTypeApplication(value=ta):
                return for_type_application(ta)
            
            case hydra.core.TermTypeLambda(value=tl):
                return for_type_lambda(tl)
            
            case _:
                return dflt()
    return hydra.rewriting.rewrite_term(rewrite, term0)

def substitute_in_constraint(subst: hydra.typing.TypeSubst, c: hydra.typing.TypeConstraint) -> hydra.core.Type:
    r"""Apply a type substitution to a type constraint."""
    
    return hydra.typing.TypeConstraint(subst_in_type(subst, c.left), subst_in_type(subst, c.right), c.comment)

def substitute_in_constraints(subst: hydra.typing.TypeSubst, cs: frozenlist[hydra.typing.TypeConstraint]) -> frozenlist[hydra.typing.TypeConstraint]:
    r"""Apply a type substitution to a list of type constraints."""
    
    return hydra.lib.lists.map((lambda v1: substitute_in_constraint(subst, v1)), cs)

def substitute_in_term(subst: hydra.typing.TermSubst, term0: hydra.core.Term) -> hydra.core.Type:
    r"""Apply a term substitution to a term."""
    
    def s() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return subst.value
    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Type:
        def with_lambda(l: hydra.core.Lambda) -> hydra.core.Type:
            def v() -> hydra.core.Type:
                return l.parameter
            def subst2() -> hydra.core.Type:
                return hydra.typing.TermSubst(hydra.lib.maps.delete(v(), s()))
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(v(), l.domain, substitute_in_term(subst2(), l.body))))))
        def with_let(lt: hydra.core.Let) -> hydra.core.Type:
            def bindings() -> frozenlist[hydra.core.Binding]:
                return lt.bindings
            def names() -> frozenset[hydra.core.Name]:
                return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda v1: v1.name), bindings()))
            def subst2() -> hydra.core.Type:
                return hydra.typing.TermSubst(hydra.lib.maps.filter_with_key((lambda k, v: hydra.lib.logic.not_(hydra.lib.sets.member(k, names()))), s()))
            def rewrite_binding(b: hydra.core.Binding) -> hydra.core.Type:
                return hydra.core.Binding(b.name, substitute_in_term(subst2(), b.term), b.type)
            return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map(rewrite_binding, bindings()), substitute_in_term(subst2(), lt.body))))
        def _hoist_body_1(v1: hydra.core.Function) -> hydra.core.Type:
            match v1:
                case hydra.core.FunctionLambda(value=l):
                    return with_lambda(l)
                
                case _:
                    return recurse(term)
        match term:
            case hydra.core.TermFunction(value=fun):
                return _hoist_body_1(fun)
            
            case hydra.core.TermLet(value=l):
                return with_let(l)
            
            case hydra.core.TermVariable(value=name):
                return hydra.lib.maybes.maybe(recurse(term), (lambda sterm: sterm), hydra.lib.maps.lookup(name, s()))
            
            case _:
                return recurse(term)
    return hydra.rewriting.rewrite_term(rewrite, term0)
