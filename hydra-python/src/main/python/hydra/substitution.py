# Note: this is an automatically generated file. Do not edit.

r"""Variable substitution in type and term expressions."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.core
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.optionals
import hydra.lib.sets
import hydra.rewriting
import hydra.typing

def subst_in_type(subst: hydra.typing.TypeSubst, typ0: hydra.core.Type) -> hydra.core.Type:
    def rewrite(recurse: Callable[[hydra.core.Type], hydra.core.Type], typ: hydra.core.Type) -> hydra.core.Type:
        match typ:
            case hydra.core.TypeForall(value=lt):
                return hydra.lib.optionals.maybe(recurse(typ), (lambda styp: cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(lt.parameter, subst_in_type(remove_var(lt.parameter), lt.body))))), hydra.lib.maps.lookup(lt.parameter, subst.value))
            
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.optionals.maybe(typ, (lambda styp: styp), hydra.lib.maps.lookup(v, subst.value))
            
            case _:
                return recurse(typ)
    def remove_var(v: hydra.core.Name) -> hydra.typing.TypeSubst:
        return hydra.typing.TypeSubst(hydra.lib.maps.remove(v, subst.value))
    return hydra.rewriting.rewrite_type(rewrite, typ0)

def compose_type_subst(s1: hydra.typing.TypeSubst, s2: hydra.typing.TypeSubst) -> hydra.typing.TypeSubst:
    def is_extra[T0](k: hydra.core.Name, v: T0) -> bool:
        return hydra.lib.optionals.is_nothing(hydra.lib.maps.lookup(k, s1.value))
    with_extra = hydra.lib.maps.filter_with_key(is_extra, s2.value)
    return hydra.typing.TypeSubst(hydra.lib.maps.union(with_extra, hydra.lib.maps.map((lambda v1: subst_in_type(s2, v1)), s1.value)))

id_type_subst = hydra.typing.TypeSubst(hydra.lib.maps.empty())

def compose_type_subst_list(v1: frozenlist[hydra.typing.TypeSubst]) -> hydra.typing.TypeSubst:
    return hydra.lib.lists.foldl(compose_type_subst, id_type_subst, v1)

def singleton_type_subst(v: hydra.core.Name, t: hydra.core.Type) -> hydra.typing.TypeSubst:
    return hydra.typing.TypeSubst(hydra.lib.maps.singleton(v, t))

def subst_in_type_scheme(subst: hydra.typing.TypeSubst, ts: hydra.core.TypeScheme) -> hydra.core.TypeScheme:
    return hydra.core.TypeScheme(ts.variables, subst_in_type(subst, ts.type))

def subst_in_context(subst: hydra.typing.TypeSubst, cx: hydra.typing.InferenceContext) -> hydra.typing.InferenceContext:
    return hydra.typing.InferenceContext(cx.schema_types, cx.primitive_types, hydra.lib.maps.map((lambda v1: subst_in_type_scheme(subst, v1)), cx.data_types), cx.debug)

def subst_types_in_term(subst: hydra.typing.TypeSubst, term0: hydra.core.Term) -> hydra.core.Term:
    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Term:
        dflt = recurse(term)
        def for_elimination(elm: hydra.core.Elimination) -> hydra.core.Term:
            match elm:
                case hydra.core.EliminationProduct(value=tp):
                    return for_tuple_projection(tp)
                
                case _:
                    return dflt
        def for_function(f: hydra.core.Function) -> hydra.core.Term:
            match f:
                case hydra.core.FunctionElimination(value=e):
                    return for_elimination(e)
                
                case hydra.core.FunctionLambda(value=l):
                    return for_lambda(l)
                
                case _:
                    return dflt
        def for_lambda(l: hydra.core.Lambda) -> hydra.core.Term:
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, hydra.lib.optionals.map((lambda v1: subst_in_type(subst, v1)), l.domain), subst_types_in_term(subst, l.body))))))
        def for_let(l: hydra.core.Let) -> hydra.core.Term:
            def rewrite_binding(b: hydra.core.Binding) -> hydra.core.Binding:
                return hydra.core.Binding(b.name, subst_types_in_term(subst, b.term), hydra.lib.optionals.map((lambda v1: subst_in_type_scheme(subst, v1)), b.type))
            return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map(rewrite_binding, l.bindings), subst_types_in_term(subst, l.body))))
        def for_tuple_projection(tp: hydra.core.TupleProjection) -> hydra.core.Term:
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationProduct(hydra.core.TupleProjection(tp.arity, tp.index, hydra.lib.optionals.map((lambda types: hydra.lib.lists.map((lambda v1: subst_in_type(subst, v1)), types)), tp.domain))))))))
        def for_type_application(tt: hydra.core.TypeApplicationTerm) -> hydra.core.Term:
            return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(subst_types_in_term(subst, tt.body), subst_in_type(subst, tt.type))))
        def for_type_lambda(ta: hydra.core.TypeLambda) -> hydra.core.Term:
            param = ta.parameter
            subst2 = hydra.typing.TypeSubst(hydra.lib.maps.remove(param, subst.value))
            return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(param, subst_types_in_term(subst2, ta.body))))
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
                return dflt
    return hydra.rewriting.rewrite_term(rewrite, term0)

def substitute_in_constraint(subst: hydra.typing.TypeSubst, c: hydra.typing.TypeConstraint) -> hydra.typing.TypeConstraint:
    return hydra.typing.TypeConstraint(subst_in_type(subst, c.left), subst_in_type(subst, c.right), c.comment)

def substitute_in_constraints(subst: hydra.typing.TypeSubst, cs: frozenlist[hydra.typing.TypeConstraint]) -> frozenlist[hydra.typing.TypeConstraint]:
    return hydra.lib.lists.map((lambda v1: substitute_in_constraint(subst, v1)), cs)

def substitute_in_term(subst: hydra.typing.TermSubst, term0: hydra.core.Term) -> hydra.core.Term:
    s = subst.value
    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Term:
        def with_lambda(l: hydra.core.Lambda) -> hydra.core.Term:
            v = l.parameter
            subst2 = hydra.typing.TermSubst(hydra.lib.maps.remove(v, s))
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(v, l.domain, substitute_in_term(subst2, l.body))))))
        def with_let(lt: hydra.core.Let) -> hydra.core.Term:
            bindings = lt.bindings
            names = hydra.lib.sets.from_list(hydra.lib.lists.map((lambda v1: v1.name), bindings))
            subst2 = hydra.typing.TermSubst(hydra.lib.maps.filter_with_key((lambda k, v: hydra.lib.logic.not_(hydra.lib.sets.member(k, names))), s))
            def rewrite_binding(b: hydra.core.Binding) -> hydra.core.Binding:
                return hydra.core.Binding(b.name, substitute_in_term(subst2, b.term), b.type)
            return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map(rewrite_binding, bindings), substitute_in_term(subst2, lt.body))))
        match term:
            case hydra.core.TermFunction(value=fun):
                match fun:
                    case hydra.core.FunctionLambda(value=l):
                        return with_lambda(l)
                    
                    case _:
                        return recurse(term)
            
            case hydra.core.TermLet(value=l):
                return with_let(l)
            
            case hydra.core.TermVariable(value=name):
                return hydra.lib.optionals.maybe(recurse(term), (lambda sterm: sterm), hydra.lib.maps.lookup(name, s))
            
            case _:
                return recurse(term)
    return hydra.rewriting.rewrite_term(rewrite, term0)
