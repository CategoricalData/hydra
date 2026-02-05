# Note: this is an automatically generated file. Do not edit.

r"""Functions for deep term rewriting operations involving hoisting subterms or bindings into enclosing let terms."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.accessors
import hydra.compute
import hydra.core
import hydra.graph
import hydra.lexical
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.rewriting
import hydra.schemas
import hydra.sorting
import hydra.substitution
import hydra.typing

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def augment_bindings_with_new_free_vars(cx: hydra.typing.TypeContext, bound_vars: frozenset[hydra.core.Name], bindings: frozenlist[hydra.core.Binding]) -> tuple[frozenlist[hydra.core.Binding], hydra.typing.TermSubst]:
    r"""Augment bindings with new free variables introduced by substitution, wrapping with lambdas after any type lambdas."""
    
    @lru_cache(1)
    def types() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return cx.types
    def wrap_after_type_lambdas(vars: frozenlist[tuple[hydra.core.Name, Maybe[hydra.core.Type]]], term: hydra.core.Term) -> hydra.core.Type:
        match term:
            case hydra.core.TermTypeLambda(value=tl):
                return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, wrap_after_type_lambdas(vars, tl.body))))
            
            case _:
                return hydra.lib.lists.foldl((lambda t, p: cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.lib.pairs.first(p), hydra.lib.pairs.second(p), t)))))), term, hydra.lib.lists.reverse(vars))
    def augment(b: hydra.core.Binding) -> tuple[hydra.core.Binding, Maybe[tuple[hydra.core.Name, hydra.core.Term]]]:
        @lru_cache(1)
        def free_vars() -> frozenlist[hydra.core.Name]:
            return hydra.lib.sets.to_list(hydra.lib.sets.intersection(bound_vars, hydra.rewriting.free_variables_in_term(b.term)))
        @lru_cache(1)
        def var_type_pairs() -> frozenlist[tuple[hydra.core.Name, Maybe[hydra.core.Type]]]:
            return hydra.lib.lists.map((lambda v: (v, hydra.lib.maps.lookup(v, types()))), free_vars())
        @lru_cache(1)
        def var_types() -> frozenlist[hydra.core.Type]:
            return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: hydra.lib.pairs.second(x1)), var_type_pairs()))
        return hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.lists.null(free_vars()), hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.lists.length(var_types()), hydra.lib.lists.length(var_type_pairs())))), (lambda : (b, Nothing())), (lambda : (hydra.core.Binding(b.name, wrap_after_type_lambdas(var_type_pairs(), b.term), hydra.lib.maybes.map((lambda ts: hydra.core.TypeScheme(ts.variables, hydra.lib.lists.foldl((lambda acc, t: cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(t, acc)))), ts.type, hydra.lib.lists.reverse(var_types())), ts.constraints)), b.type)), Just((b.name, hydra.lib.lists.foldl((lambda t, v: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(t, cast(hydra.core.Term, hydra.core.TermVariable(v)))))), cast(hydra.core.Term, hydra.core.TermVariable(b.name)), free_vars()))))))
    @lru_cache(1)
    def results() -> frozenlist[tuple[hydra.core.Binding, Maybe[tuple[hydra.core.Name, hydra.core.Term]]]]:
        return hydra.lib.lists.map(augment, bindings)
    return (hydra.lib.lists.map((lambda x1: hydra.lib.pairs.first(x1)), results()), hydra.typing.TermSubst(hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: hydra.lib.pairs.second(x1)), results())))))

def binding_is_polymorphic(binding: hydra.core.Binding) -> bool:
    r"""Check if a binding has a polymorphic type (non-empty list of type scheme variables)."""
    
    return hydra.lib.maybes.maybe(False, (lambda ts: hydra.lib.logic.not_(hydra.lib.lists.null(ts.variables))), binding.type)

def binding_uses_context_type_vars(cx: hydra.typing.TypeContext, binding: hydra.core.Binding) -> bool:
    r"""Check if a binding's type uses any type variables from the given TypeContext. Returns True if the free type variables in the binding's type intersect with the type variables in scope (typeContextTypeVariables)."""
    
    return hydra.lib.maybes.maybe(False, (lambda ts: (free_in_type := hydra.rewriting.free_variables_in_type(ts.type), context_type_vars := cx.type_variables, hydra.lib.logic.not_(hydra.lib.sets.null(hydra.lib.sets.intersection(free_in_type, context_type_vars))))[2]), binding.type)

def count_var_occurrences(name: hydra.core.Name, term: hydra.core.Term) -> int:
    r"""Count the number of occurrences of a variable name in a term. Assumes no variable shadowing."""
    
    @lru_cache(1)
    def child_count() -> int:
        return hydra.lib.lists.foldl((lambda acc, t: hydra.lib.math.add(acc, count_var_occurrences(name, t))), 0, hydra.rewriting.subterms(term))
    match term:
        case hydra.core.TermVariable(value=v):
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, name), (lambda : hydra.lib.math.add(1, child_count())), (lambda : child_count()))
        
        case _:
            return child_count()

def rewrite_and_fold_term_with_type_context(f: Callable[[
  Callable[[T0, hydra.core.Term], tuple[T0, hydra.core.Term]],
  hydra.typing.TypeContext,
  T0,
  hydra.core.Term], tuple[T0, hydra.core.Term]], cx0: hydra.typing.TypeContext, val0: T0, term0: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
    def wrapper(low_level_recurse: Callable[[tuple[T0, hydra.typing.TypeContext], hydra.core.Term], tuple[tuple[T0, T1], hydra.core.Term]], val_and_cx: tuple[T0, hydra.typing.TypeContext], term: hydra.core.Term) -> tuple[tuple[T0, hydra.typing.TypeContext], hydra.core.Term]:
        @lru_cache(1)
        def val() -> T0:
            return hydra.lib.pairs.first(val_and_cx)
        @lru_cache(1)
        def cx() -> hydra.core.Type:
            return hydra.lib.pairs.second(val_and_cx)
        @lru_cache(1)
        def cx1() -> hydra.core.Type:
            def _hoist_cx1_1(v1: hydra.core.Function) -> hydra.core.Type:
                match v1:
                    case hydra.core.FunctionLambda(value=l):
                        return hydra.schemas.extend_type_context_for_lambda(cx(), l)
                    
                    case _:
                        return cx()
            match term:
                case hydra.core.TermFunction(value=fun):
                    return _hoist_cx1_1(fun)
                
                case hydra.core.TermLet(value=l):
                    return hydra.schemas.extend_type_context_for_let((lambda _, _2: Nothing()), cx(), l)
                
                case hydra.core.TermTypeLambda(value=tl):
                    return hydra.schemas.extend_type_context_for_type_lambda(cx(), tl)
                
                case _:
                    return cx()
        def recurse_for_user(new_val: T0, subterm: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
            @lru_cache(1)
            def result() -> tuple[tuple[T0, T1], hydra.core.Term]:
                return low_level_recurse((new_val, cx1()), subterm)
            return (hydra.lib.pairs.first(hydra.lib.pairs.first(result())), hydra.lib.pairs.second(result()))
        @lru_cache(1)
        def f_result() -> tuple[T0, hydra.core.Term]:
            return f(recurse_for_user, cx1(), val(), term)
        return ((hydra.lib.pairs.first(f_result()), cx()), hydra.lib.pairs.second(f_result()))
    @lru_cache(1)
    def result() -> tuple[tuple[T0, hydra.typing.TypeContext], hydra.core.Term]:
        return hydra.rewriting.rewrite_and_fold_term((lambda x1, x2, x3: wrapper(x1, x2, x3)), (val0, cx0), term0)
    return (hydra.lib.pairs.first(hydra.lib.pairs.first(result())), hydra.lib.pairs.second(result()))

def hoist_let_bindings_with_predicate(is_parent_binding: Callable[[hydra.core.Binding], bool], should_hoist_binding: Callable[[hydra.typing.TypeContext, hydra.core.Binding], bool], cx0: hydra.typing.TypeContext, let0: hydra.core.Let) -> hydra.core.Type:
    r"""Transform a let-term by pulling let bindings to the top level. The isParentBinding predicate applies to top-level bindings and determines whether their subterm bindings are eligible for hoisting. The shouldHoistBinding predicate takes the TypeContext and a subterm binding, and returns True if the binding should be hoisted. This is useful for targets like Java that cannot have polymorphic definitions in arbitrary positions. The TypeContext provides information about type variables and lambda variables in scope. If a hoisted binding captures let-bound or lambda-bound variables from an enclosing scope, the binding is wrapped in lambdas for those variables, and references are replaced with applications. If a hoisted binding captures type variables from an enclosing type lambda scope, those type variables are added to the binding's type scheme, and references are replaced with type applications. Note: we assume that there is no variable shadowing; use hydra.rewriting.unshadowVariables first."""
    
    def hoist_one(prefix: str, cx: hydra.typing.TypeContext, pair: tuple[frozenlist[tuple[hydra.core.Binding, hydra.core.Term]], frozenset[hydra.core.Name]], binding_with_captured_vars: tuple[hydra.core.Binding, frozenlist[hydra.core.Name]]) -> tuple[frozenlist[tuple[hydra.core.Binding, hydra.core.Term]], frozenset[hydra.core.Name]]:
        @lru_cache(1)
        def binding_and_replacement_pairs() -> frozenlist[tuple[hydra.core.Binding, hydra.core.Term]]:
            return hydra.lib.pairs.first(pair)
        @lru_cache(1)
        def already_used_names() -> frozenset[hydra.core.Name]:
            return hydra.lib.pairs.second(pair)
        @lru_cache(1)
        def b() -> hydra.core.Type:
            return hydra.lib.pairs.first(binding_with_captured_vars)
        @lru_cache(1)
        def captured_term_vars() -> frozenlist[hydra.core.Name]:
            return hydra.lib.pairs.second(binding_with_captured_vars)
        @lru_cache(1)
        def types() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
            return cx.types
        @lru_cache(1)
        def captured_term_var_type_pairs() -> frozenlist[tuple[hydra.core.Name, Maybe[hydra.core.Type]]]:
            return hydra.lib.lists.map((lambda v: (v, hydra.lib.maps.lookup(v, types()))), captured_term_vars())
        @lru_cache(1)
        def captured_type_vars() -> frozenlist[hydra.core.Name]:
            return hydra.lib.sets.to_list(hydra.lib.sets.intersection(cx.type_variables, hydra.lib.maybes.maybe(hydra.lib.sets.empty(), (lambda ts: hydra.rewriting.free_variables_in_type(ts.type)), b().type)))
        @lru_cache(1)
        def global_binding_name() -> hydra.core.Type:
            return hydra.lexical.choose_unique_name(already_used_names(), hydra.core.Name(hydra.lib.strings.cat2(prefix, b().name.value)))
        @lru_cache(1)
        def new_used_names() -> frozenset[hydra.core.Name]:
            return hydra.lib.sets.insert(global_binding_name(), already_used_names())
        @lru_cache(1)
        def captured_term_var_types() -> frozenlist[hydra.core.Type]:
            return hydra.lib.lists.map((lambda typ: hydra.rewriting.deannotate_type_parameters(typ)), hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: hydra.lib.pairs.second(x1)), captured_term_var_type_pairs())))
        @lru_cache(1)
        def new_type_scheme() -> Maybe[hydra.core.TypeScheme]:
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(captured_term_var_types()), hydra.lib.lists.length(captured_term_var_type_pairs())), (lambda : hydra.lib.maybes.map((lambda ts: hydra.core.TypeScheme(hydra.lib.lists.nub(hydra.lib.lists.concat2(captured_type_vars(), ts.variables)), hydra.lib.lists.foldl((lambda t, a: cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(a, t)))), ts.type, hydra.lib.lists.reverse(captured_term_var_types())), ts.constraints)), b().type)), (lambda : Nothing()))
        @lru_cache(1)
        def stripped_term() -> hydra.core.Type:
            return hydra.rewriting.detype_term(b().term)
        @lru_cache(1)
        def term_with_lambdas() -> hydra.core.Type:
            return hydra.lib.lists.foldl((lambda t, p: cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.lib.pairs.first(p), hydra.lib.maybes.map((lambda dom: hydra.rewriting.deannotate_type_parameters(dom)), hydra.lib.pairs.second(p)), t)))))), stripped_term(), hydra.lib.lists.reverse(captured_term_var_type_pairs()))
        @lru_cache(1)
        def term_with_type_lambdas() -> hydra.core.Type:
            return hydra.lib.lists.foldl((lambda t, v: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(v, t)))), term_with_lambdas(), hydra.lib.lists.reverse(hydra.lib.maybes.maybe((), (lambda v1: v1.variables), new_type_scheme())))
        @lru_cache(1)
        def replacement() -> hydra.core.Type:
            return hydra.lib.lists.foldl((lambda t, v: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(t, cast(hydra.core.Term, hydra.core.TermVariable(v)))))), cast(hydra.core.Term, hydra.core.TermVariable(global_binding_name())), captured_term_vars())
        @lru_cache(1)
        def new_binding_and_replacement() -> tuple[hydra.core.Binding, hydra.core.Term]:
            return (hydra.core.Binding(global_binding_name(), term_with_type_lambdas(), new_type_scheme()), replacement())
        @lru_cache(1)
        def new_pairs() -> frozenlist[tuple[hydra.core.Binding, hydra.core.Term]]:
            return hydra.lib.lists.cons(new_binding_and_replacement(), binding_and_replacement_pairs())
        return (new_pairs(), new_used_names())
    def rewrite(prefix: str, recurse: Callable[[tuple[frozenlist[T0], T1], T2], tuple[tuple[frozenlist[hydra.core.Binding], frozenset[hydra.core.Name]], hydra.core.Term]], cx: hydra.typing.TypeContext, bindings_and_names: tuple[frozenlist[hydra.core.Binding], T1], term: T2) -> tuple[tuple[frozenlist[hydra.core.Binding], frozenset[hydra.core.Name]], hydra.core.Term]:
        @lru_cache(1)
        def previously_finished_bindings() -> frozenlist[hydra.core.Binding]:
            return hydra.lib.pairs.first(bindings_and_names)
        @lru_cache(1)
        def empty_bindings_and_names() -> tuple[frozenlist[T3], T1]:
            return ((), hydra.lib.pairs.second(bindings_and_names))
        @lru_cache(1)
        def result() -> tuple[tuple[frozenlist[hydra.core.Binding], frozenset[hydra.core.Name]], hydra.core.Term]:
            return recurse(empty_bindings_and_names(), term)
        @lru_cache(1)
        def new_bindings_and_names() -> tuple[frozenlist[hydra.core.Binding], frozenset[hydra.core.Name]]:
            return hydra.lib.pairs.first(result())
        @lru_cache(1)
        def bindings_so_far() -> frozenlist[hydra.core.Binding]:
            return hydra.lib.pairs.first(new_bindings_and_names())
        @lru_cache(1)
        def already_used_names() -> frozenset[hydra.core.Name]:
            return hydra.lib.pairs.second(new_bindings_and_names())
        @lru_cache(1)
        def new_term() -> hydra.core.Type:
            return hydra.lib.pairs.second(result())
        match new_term():
            case hydra.core.TermLet(value=l):
                @lru_cache(1)
                def body() -> hydra.core.Type:
                    return l.body
                @lru_cache(1)
                def partition_pair() -> tuple[frozenlist[hydra.core.Binding], frozenlist[hydra.core.Binding]]:
                    return hydra.lib.lists.partition((lambda v1: should_hoist_binding(cx, v1)), l.bindings)
                @lru_cache(1)
                def hoist_us() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.pairs.first(partition_pair())
                @lru_cache(1)
                def keep_us() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.pairs.second(partition_pair())
                @lru_cache(1)
                def hoisted_binding_names() -> frozenlist[hydra.core.Name]:
                    return hydra.lib.lists.map((lambda v1: v1.name), hoist_us())
                @lru_cache(1)
                def poly_let_variables() -> frozenset[hydra.core.Name]:
                    return hydra.lib.sets.from_list(hydra.lib.lists.filter((lambda v: hydra.lib.maybes.maybe(False, hydra.schemas.f_type_is_polymorphic, hydra.lib.maps.lookup(v, cx.types))), hydra.lib.sets.to_list(cx.let_variables)))
                @lru_cache(1)
                def bound_term_variables() -> frozenset[hydra.core.Name]:
                    return hydra.lib.sets.union(cx.lambda_variables, cx.let_variables)
                @lru_cache(1)
                def free_variables_in_each_binding() -> frozenlist[frozenlist[hydra.core.Name]]:
                    return hydra.lib.lists.map((lambda b: hydra.lib.sets.to_list(hydra.lib.sets.intersection(bound_term_variables(), hydra.rewriting.free_variables_in_term(b.term)))), hoist_us())
                @lru_cache(1)
                def binding_dependencies() -> frozenlist[tuple[frozenlist[hydra.core.Name], frozenlist[hydra.core.Name]]]:
                    return hydra.lib.lists.map((lambda vars: hydra.lib.lists.partition((lambda v: hydra.lib.sets.member(v, hydra.lib.sets.from_list(hoisted_binding_names()))), vars)), free_variables_in_each_binding())
                @lru_cache(1)
                def binding_edges() -> frozenlist[tuple[hydra.core.Name, frozenlist[hydra.core.Name]]]:
                    return hydra.lib.lists.zip(hoisted_binding_names(), hydra.lib.lists.map((lambda x1: hydra.lib.pairs.first(x1)), binding_dependencies()))
                @lru_cache(1)
                def binding_immediate_captured_vars() -> frozenlist[tuple[hydra.core.Name, frozenlist[hydra.core.Name]]]:
                    return hydra.lib.lists.zip(hoisted_binding_names(), hydra.lib.lists.map((lambda x1: hydra.lib.pairs.second(x1)), binding_dependencies()))
                @lru_cache(1)
                def captured_vars_map() -> FrozenDict[hydra.core.Name, frozenset[hydra.core.Name]]:
                    return hydra.lib.maps.from_list(hydra.sorting.propagate_tags(binding_edges(), binding_immediate_captured_vars()))
                @lru_cache(1)
                def bindings_with_captured_vars() -> frozenlist[tuple[hydra.core.Binding, frozenlist[hydra.core.Name]]]:
                    return hydra.lib.lists.map((lambda b: (b, hydra.lib.maybes.maybe((), (lambda vars: hydra.lib.sets.to_list(hydra.lib.sets.difference(vars, poly_let_variables()))), hydra.lib.maps.lookup(b.name, captured_vars_map())))), hoist_us())
                @lru_cache(1)
                def hoist_pairs_and_names() -> tuple[frozenlist[tuple[hydra.core.Binding, hydra.core.Term]], frozenset[hydra.core.Name]]:
                    return hydra.lib.lists.foldl((lambda v1, v2: hoist_one(prefix, cx, v1, v2)), ((), already_used_names()), bindings_with_captured_vars())
                @lru_cache(1)
                def hoist_pairs() -> frozenlist[tuple[hydra.core.Binding, hydra.core.Term]]:
                    return hydra.lib.lists.reverse(hydra.lib.pairs.first(hoist_pairs_and_names()))
                @lru_cache(1)
                def hoisted_bindings() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.map((lambda x1: hydra.lib.pairs.first(x1)), hoist_pairs())
                @lru_cache(1)
                def replacements() -> frozenlist[hydra.core.Term]:
                    return hydra.lib.lists.map((lambda x1: hydra.lib.pairs.second(x1)), hoist_pairs())
                @lru_cache(1)
                def final_used_names() -> frozenset[hydra.core.Name]:
                    return hydra.lib.pairs.second(hoist_pairs_and_names())
                @lru_cache(1)
                def hoist_name_replacement_pairs() -> frozenlist[tuple[hydra.core.Name, hydra.core.Term]]:
                    return hydra.lib.lists.zip(hydra.lib.lists.map((lambda v1: v1.name), hoist_us()), replacements())
                @lru_cache(1)
                def hoist_binding_map() -> FrozenDict[hydra.core.Name, hydra.core.Binding]:
                    return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda b: (b.name, b)), hoist_us()))
                def is_cacheable(name: hydra.core.Name) -> bool:
                    @lru_cache(1)
                    def multi_ref() -> bool:
                        return hydra.lib.equality.gte(count_var_occurrences(name, body()), 2)
                    @lru_cache(1)
                    def is_poly() -> bool:
                        return hydra.lib.maybes.maybe(False, (lambda b: binding_is_polymorphic(b)), hydra.lib.maps.lookup(name, hoist_binding_map()))
                    return hydra.lib.logic.and_(multi_ref(), hydra.lib.logic.not_(is_poly()))
                @lru_cache(1)
                def single_ref_pairs() -> frozenlist[tuple[hydra.core.Name, hydra.core.Term]]:
                    return hydra.lib.lists.filter((lambda p: hydra.lib.logic.not_(is_cacheable(hydra.lib.pairs.first(p)))), hoist_name_replacement_pairs())
                @lru_cache(1)
                def multi_ref_pairs() -> frozenlist[tuple[hydra.core.Name, hydra.core.Term]]:
                    return hydra.lib.lists.filter((lambda p: is_cacheable(hydra.lib.pairs.first(p))), hoist_name_replacement_pairs())
                @lru_cache(1)
                def full_subst() -> hydra.core.Type:
                    return hydra.typing.TermSubst(hydra.lib.maps.from_list(hoist_name_replacement_pairs()))
                @lru_cache(1)
                def body_only_subst() -> hydra.core.Type:
                    return hydra.typing.TermSubst(hydra.lib.maps.from_list(single_ref_pairs()))
                @lru_cache(1)
                def body_subst() -> hydra.core.Type:
                    return hydra.substitution.substitute_in_term(body_only_subst(), body())
                @lru_cache(1)
                def cache_bindings() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.map((lambda p: hydra.core.Binding(hydra.lib.pairs.first(p), hydra.lib.pairs.second(p), Nothing())), multi_ref_pairs())
                @lru_cache(1)
                def body_with_cache() -> hydra.core.Type:
                    return hydra.lib.logic.if_else(hydra.lib.lists.null(cache_bindings()), (lambda : body_subst()), (lambda : cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(cache_bindings(), body_subst())))))
                @lru_cache(1)
                def keep_us_subst() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.map((lambda v1: hydra.substitution.substitute_in_binding(full_subst(), v1)), keep_us())
                @lru_cache(1)
                def hoisted_bindings_subst() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.map((lambda v1: hydra.substitution.substitute_in_binding(full_subst(), v1)), hoisted_bindings())
                @lru_cache(1)
                def bindings_so_far_subst() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.map((lambda v1: hydra.substitution.substitute_in_binding(full_subst(), v1)), bindings_so_far())
                @lru_cache(1)
                def augment_result() -> tuple[frozenlist[hydra.core.Binding], hydra.typing.TermSubst]:
                    return augment_bindings_with_new_free_vars(cx, hydra.lib.sets.difference(bound_term_variables(), poly_let_variables()), bindings_so_far_subst())
                @lru_cache(1)
                def bindings_so_far_augmented() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.pairs.first(augment_result())
                @lru_cache(1)
                def augment_subst() -> hydra.core.Type:
                    return hydra.lib.pairs.second(augment_result())
                @lru_cache(1)
                def hoisted_bindings_final() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.map((lambda v1: hydra.substitution.substitute_in_binding(augment_subst(), v1)), hoisted_bindings_subst())
                @lru_cache(1)
                def bindings_so_far_final() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.map((lambda v1: hydra.substitution.substitute_in_binding(augment_subst(), v1)), bindings_so_far_augmented())
                @lru_cache(1)
                def body_final() -> hydra.core.Type:
                    return hydra.substitution.substitute_in_term(augment_subst(), body_with_cache())
                @lru_cache(1)
                def keep_us_final() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.map((lambda v1: hydra.substitution.substitute_in_binding(augment_subst(), v1)), keep_us_subst())
                @lru_cache(1)
                def final_term() -> hydra.core.Type:
                    return hydra.lib.logic.if_else(hydra.lib.lists.null(keep_us_final()), (lambda : body_final()), (lambda : cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(keep_us_final(), body_final())))))
                return ((hydra.lib.lists.concat((previously_finished_bindings(), hoisted_bindings_final(), bindings_so_far_final())), final_used_names()), final_term())
            
            case _:
                return ((hydra.lib.lists.concat2(previously_finished_bindings(), bindings_so_far()), already_used_names()), new_term())
    @lru_cache(1)
    def cx1() -> hydra.core.Type:
        return hydra.schemas.extend_type_context_for_let((lambda c, b: Nothing()), cx0, let0)
    def for_active_binding(b: hydra.core.Binding) -> frozenlist[hydra.core.Binding]:
        @lru_cache(1)
        def prefix() -> str:
            return hydra.lib.strings.cat2(b.name.value, "_")
        @lru_cache(1)
        def init() -> tuple[frozenlist[T0], frozenset[hydra.core.Name]]:
            return ((), hydra.lib.sets.singleton(b.name))
        @lru_cache(1)
        def result_pair() -> tuple[tuple[frozenlist[hydra.core.Binding], frozenset[hydra.core.Name]], hydra.core.Term]:
            return rewrite_and_fold_term_with_type_context((lambda v1, v2, v3, v4: rewrite(prefix(), v1, v2, v3, v4)), cx1(), init(), b.term)
        @lru_cache(1)
        def result_bindings() -> frozenlist[hydra.core.Binding]:
            return hydra.lib.pairs.first(hydra.lib.pairs.first(result_pair()))
        @lru_cache(1)
        def result_term() -> hydra.core.Type:
            return hydra.lib.pairs.second(result_pair())
        return hydra.lib.lists.cons(hydra.core.Binding(b.name, result_term(), b.type), result_bindings())
    def for_binding(b: hydra.core.Binding) -> frozenlist[hydra.core.Binding]:
        return hydra.lib.logic.if_else(is_parent_binding(b), (lambda : for_active_binding(b)), (lambda : (b,)))
    return hydra.core.Let(hydra.lib.lists.concat(hydra.lib.lists.map(for_binding, let0.bindings)), let0.body)

def should_hoist_all(_: T0, _2: T1) -> bool:
    return True

def hoist_all_let_bindings(let0: hydra.core.Let) -> hydra.core.Type:
    r"""Transform a let-term by pulling ALL let bindings to the top level. This is useful for targets like Java that don't support nested let expressions at all. If a hoisted binding captures lambda-bound variables from an enclosing scope, the binding is wrapped in lambdas for those variables, and references are replaced with applications. Note: Assumes no variable shadowing; use hydra.rewriting.unshadowVariables first."""
    
    @lru_cache(1)
    def empty_ix() -> hydra.core.Type:
        return hydra.typing.InferenceContext(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), False)
    @lru_cache(1)
    def empty_cx() -> hydra.core.Type:
        return hydra.typing.TypeContext(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), empty_ix())
    return hoist_let_bindings_with_predicate((lambda _: True), (lambda x1, x2: should_hoist_all(x1, x2)), empty_cx(), let0)

def rewrite_and_fold_term_with_type_context_and_path(f: Callable[[
  Callable[[T0, hydra.core.Term], tuple[T0, hydra.core.Term]],
  frozenlist[hydra.accessors.TermAccessor],
  hydra.typing.TypeContext,
  T0,
  hydra.core.Term], tuple[T0, hydra.core.Term]], cx0: hydra.typing.TypeContext, val0: T0, term0: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
    def wrapper(recurse: Callable[[
      frozenlist[hydra.accessors.TermAccessor],
      tuple[hydra.typing.TypeContext, T0],
      hydra.core.Term], tuple[tuple[T1, T0], hydra.core.Term]], path: frozenlist[hydra.accessors.TermAccessor], cx_and_val: tuple[hydra.typing.TypeContext, T0], term: hydra.core.Term) -> tuple[tuple[hydra.typing.TypeContext, T0], hydra.core.Term]:
        @lru_cache(1)
        def cx() -> hydra.core.Type:
            return hydra.lib.pairs.first(cx_and_val)
        @lru_cache(1)
        def val() -> T0:
            return hydra.lib.pairs.second(cx_and_val)
        @lru_cache(1)
        def cx1() -> hydra.core.Type:
            def _hoist_cx1_1(v1: hydra.core.Function) -> hydra.core.Type:
                match v1:
                    case hydra.core.FunctionLambda(value=l):
                        return hydra.schemas.extend_type_context_for_lambda(cx(), l)
                    
                    case _:
                        return cx()
            match term:
                case hydra.core.TermFunction(value=fun):
                    return _hoist_cx1_1(fun)
                
                case hydra.core.TermLet(value=l):
                    return hydra.schemas.extend_type_context_for_let((lambda _, _2: Nothing()), cx(), l)
                
                case hydra.core.TermTypeLambda(value=tl):
                    return hydra.schemas.extend_type_context_for_type_lambda(cx(), tl)
                
                case _:
                    return cx()
        def recurse_for_user(val_in: T0, term_in: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
            @lru_cache(1)
            def result() -> tuple[tuple[T1, T0], hydra.core.Term]:
                return recurse(path, (cx1(), val_in), term_in)
            return (hydra.lib.pairs.second(hydra.lib.pairs.first(result())), hydra.lib.pairs.second(result()))
        @lru_cache(1)
        def f_result() -> tuple[T0, hydra.core.Term]:
            return f(recurse_for_user, path, cx1(), val(), term)
        return ((cx(), hydra.lib.pairs.first(f_result())), hydra.lib.pairs.second(f_result()))
    @lru_cache(1)
    def result() -> tuple[tuple[hydra.typing.TypeContext, T0], hydra.core.Term]:
        return hydra.rewriting.rewrite_and_fold_term_with_path((lambda x1, x2, x3, x4: wrapper(x1, x2, x3, x4)), (cx0, val0), term0)
    return (hydra.lib.pairs.second(hydra.lib.pairs.first(result())), hydra.lib.pairs.second(result()))

def hoist_subterms(should_hoist: Callable[[tuple[frozenlist[hydra.accessors.TermAccessor], hydra.core.Term]], bool], cx0: hydra.typing.TypeContext, term0: hydra.core.Term) -> hydra.core.Type:
    r"""Hoist subterms into local let bindings based on a path-aware predicate. The predicate receives a pair of (path, term) where path is the list of TermAccessors from the root to the current term, and returns True if the term should be hoisted. For each let term found, the immediate subterms (binding values and body) are processed: matching subterms within each immediate subterm are collected and hoisted into a local let that wraps that immediate subterm. If a hoisted term contains free variables that are lambda-bound at an enclosing scope, the hoisted binding is wrapped in lambdas for those variables, and the reference is replaced with an application of those variables."""
    
    def process_immediate_subterm(cx: hydra.typing.TypeContext, counter: int, name_prefix: str, subterm: hydra.core.Term) -> tuple[int, hydra.core.Term]:
        @lru_cache(1)
        def baseline_lambda_vars() -> frozenset[hydra.core.Name]:
            return cx.lambda_variables
        def collect_and_replace(recurse: Callable[[tuple[int, frozenlist[hydra.core.Binding]], hydra.core.Term], tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.core.Term]], path: frozenlist[hydra.accessors.TermAccessor], cx_inner: hydra.typing.TypeContext, acc: tuple[int, frozenlist[hydra.core.Binding]], term: hydra.core.Term) -> tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.core.Term]:
            @lru_cache(1)
            def current_counter() -> int:
                return hydra.lib.pairs.first(acc)
            @lru_cache(1)
            def collected_bindings() -> frozenlist[hydra.core.Binding]:
                return hydra.lib.pairs.second(acc)
            match term:
                case hydra.core.TermLet():
                    return (acc, term)
                
                case hydra.core.TermTypeLambda():
                    return (acc, term)
                
                case _:
                    return (result := recurse(acc, term), (new_acc := hydra.lib.pairs.first(result), (processed_term := hydra.lib.pairs.second(result), (new_counter := hydra.lib.pairs.first(new_acc), (new_bindings := hydra.lib.pairs.second(new_acc), hydra.lib.logic.if_else(should_hoist((path, processed_term)), (lambda : (binding_name := hydra.core.Name(hydra.lib.strings.cat(("_hoist_", name_prefix, "_", hydra.lib.literals.show_int32(new_counter)))), (all_lambda_vars := cx_inner.lambda_variables, (new_lambda_vars := hydra.lib.sets.difference(all_lambda_vars, baseline_lambda_vars()), (free_vars := hydra.rewriting.free_variables_in_term(processed_term), (captured_vars := hydra.lib.sets.to_list(hydra.lib.sets.intersection(new_lambda_vars, free_vars)), (wrapped_term := hydra.lib.lists.foldl((lambda body, var_name: cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(var_name, Nothing(), body)))))), processed_term, hydra.lib.lists.reverse(captured_vars)), (reference := hydra.lib.lists.foldl((lambda fn, var_name: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fn, cast(hydra.core.Term, hydra.core.TermVariable(var_name)))))), cast(hydra.core.Term, hydra.core.TermVariable(binding_name)), captured_vars), (new_binding := hydra.core.Binding(binding_name, wrapped_term, Nothing()), ((hydra.lib.math.add(new_counter, 1), hydra.lib.lists.cons(new_binding, new_bindings)), reference))[1])[1])[1])[1])[1])[1])[1])[1]), (lambda : (new_acc, processed_term))))[1])[1])[1])[1])[1]
        @lru_cache(1)
        def result() -> tuple[tuple[int, frozenlist[hydra.core.Binding]], hydra.core.Term]:
            return rewrite_and_fold_term_with_type_context_and_path(collect_and_replace, cx, (counter, ()), subterm)
        @lru_cache(1)
        def final_acc() -> tuple[int, frozenlist[hydra.core.Binding]]:
            return hydra.lib.pairs.first(result())
        @lru_cache(1)
        def transformed_subterm() -> hydra.core.Type:
            return hydra.lib.pairs.second(result())
        @lru_cache(1)
        def final_counter() -> int:
            return hydra.lib.pairs.first(final_acc())
        @lru_cache(1)
        def bindings() -> frozenlist[hydra.core.Binding]:
            return hydra.lib.pairs.second(final_acc())
        return hydra.lib.logic.if_else(hydra.lib.lists.null(bindings()), (lambda : (final_counter(), transformed_subterm())), (lambda : (local_let := cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.reverse(bindings()), transformed_subterm()))), (final_counter(), local_let))[1]))
    def process_let_term(cx: hydra.typing.TypeContext, counter: T0, lt: hydra.core.Let) -> tuple[T0, hydra.core.Term]:
        @lru_cache(1)
        def bindings() -> frozenlist[hydra.core.Binding]:
            return lt.bindings
        @lru_cache(1)
        def body() -> hydra.core.Type:
            return lt.body
        def process_binding(acc: frozenlist[hydra.core.Binding], binding: hydra.core.Binding) -> frozenlist[hydra.core.Binding]:
            @lru_cache(1)
            def name_prefix() -> str:
                return hydra.lib.strings.intercalate("_", hydra.lib.strings.split_on(".", binding.name.value))
            @lru_cache(1)
            def result() -> tuple[int, hydra.core.Term]:
                return process_immediate_subterm(cx, 1, name_prefix(), binding.term)
            @lru_cache(1)
            def new_value() -> hydra.core.Type:
                return hydra.lib.pairs.second(result())
            @lru_cache(1)
            def new_binding() -> hydra.core.Type:
                return hydra.core.Binding(binding.name, new_value(), binding.type)
            return hydra.lib.lists.cons(new_binding(), acc)
        @lru_cache(1)
        def new_bindings_reversed() -> frozenlist[hydra.core.Binding]:
            return hydra.lib.lists.foldl(process_binding, (), bindings())
        @lru_cache(1)
        def new_bindings() -> frozenlist[hydra.core.Binding]:
            return hydra.lib.lists.reverse(new_bindings_reversed())
        @lru_cache(1)
        def body_result() -> tuple[int, hydra.core.Term]:
            return process_immediate_subterm(cx, 1, "_body", body())
        @lru_cache(1)
        def new_body() -> hydra.core.Type:
            return hydra.lib.pairs.second(body_result())
        return (counter, cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(new_bindings(), new_body()))))
    def rewrite(recurse: Callable[[T0, hydra.core.Term], tuple[T1, hydra.core.Term]], cx: hydra.typing.TypeContext, counter: T0, term: hydra.core.Term) -> tuple[T1, hydra.core.Term]:
        match term:
            case hydra.core.TermLet():
                @lru_cache(1)
                def recursed() -> tuple[T1, hydra.core.Term]:
                    return recurse(counter, term)
                @lru_cache(1)
                def new_counter() -> T1:
                    return hydra.lib.pairs.first(recursed())
                @lru_cache(1)
                def recursed_term() -> hydra.core.Type:
                    return hydra.lib.pairs.second(recursed())
                match recursed_term():
                    case hydra.core.TermLet(value=lt2):
                        return process_let_term(cx, new_counter(), lt2)
                    
                    case _:
                        return (new_counter(), recursed_term())
            
            case _:
                return recurse(counter, term)
    return hydra.lib.pairs.second(rewrite_and_fold_term_with_type_context((lambda x1, x2, x3, x4: rewrite(x1, x2, x3, x4)), cx0, 1, term0))

def is_elimination_union(f: hydra.core.Function) -> bool:
    def _hoist_hydra_hoisting_is_elimination_union_1(v1: hydra.core.Elimination) -> bool:
        match v1:
            case hydra.core.EliminationUnion():
                return True
            
            case _:
                return False
    match f:
        case hydra.core.FunctionElimination(value=e):
            return _hoist_hydra_hoisting_is_elimination_union_1(e)
        
        case _:
            return False

def is_union_elimination(term: hydra.core.Term) -> bool:
    r"""Check if a term is a union elimination (case statement)."""
    
    match term:
        case hydra.core.TermFunction(value=f):
            return is_elimination_union(f)
        
        case _:
            return False

def update_hoist_state(accessor: hydra.accessors.TermAccessor, state: tuple[bool, bool]) -> tuple[bool, bool]:
    r"""Update hoisting state when traversing an accessor. State is (atTopLevel, usedAppLHS). Returns updated state."""
    
    @lru_cache(1)
    def at_top() -> bool:
        return hydra.lib.pairs.first(state)
    @lru_cache(1)
    def used_app() -> bool:
        return hydra.lib.pairs.second(state)
    def _hoist_body_1(v1: hydra.accessors.TermAccessor) -> tuple[bool, bool]:
        match v1:
            case hydra.accessors.TermAccessorAnnotatedBody():
                return (True, used_app())
            
            case hydra.accessors.TermAccessorLetBody():
                return (True, used_app())
            
            case hydra.accessors.TermAccessorLetBinding():
                return (True, used_app())
            
            case hydra.accessors.TermAccessorLambdaBody():
                return hydra.lib.logic.if_else(used_app(), (lambda : (False, True)), (lambda : (True, False)))
            
            case hydra.accessors.TermAccessorUnionCasesBranch():
                return hydra.lib.logic.if_else(used_app(), (lambda : (False, True)), (lambda : (True, False)))
            
            case hydra.accessors.TermAccessorUnionCasesDefault():
                return hydra.lib.logic.if_else(used_app(), (lambda : (False, True)), (lambda : (True, False)))
            
            case hydra.accessors.TermAccessorApplicationFunction():
                return hydra.lib.logic.if_else(used_app(), (lambda : (False, True)), (lambda : (True, True)))
            
            case hydra.accessors.TermAccessorApplicationArgument():
                return (False, used_app())
            
            case _:
                return (False, used_app())
    return hydra.lib.logic.if_else(hydra.lib.logic.not_(at_top()), (lambda : (False, used_app())), (lambda : _hoist_body_1(accessor)))

def should_hoist_case_statement(path_and_term: tuple[frozenlist[hydra.accessors.TermAccessor], hydra.core.Term]) -> bool:
    r"""Predicate for case statement hoisting. Returns True if term is a case statement AND not at top level. Top level = reachable through annotations, let body/binding, lambda bodies, or ONE app LHS. Once through an app LHS, lambda bodies no longer pass through."""
    
    @lru_cache(1)
    def path() -> frozenlist[hydra.accessors.TermAccessor]:
        return hydra.lib.pairs.first(path_and_term)
    @lru_cache(1)
    def term() -> hydra.core.Type:
        return hydra.lib.pairs.second(path_and_term)
    return hydra.lib.logic.if_else(hydra.lib.logic.not_(is_union_elimination(term())), (lambda : False), (lambda : (final_state := hydra.lib.lists.foldl((lambda st, acc: update_hoist_state(acc, st)), (True, False), path()), hydra.lib.logic.not_(hydra.lib.pairs.first(final_state)))[1]))

def hoist_case_statements(v1: hydra.typing.TypeContext, v2: hydra.core.Term) -> hydra.core.Type:
    r"""Hoist case statements into local let bindings. This is useful for targets such as Python which only support case statements (match) at the top level. Case statements are hoisted only when they appear at non-top-level positions. Top level = root, or reachable through annotations, let body/binding, lambda bodies, or ONE application LHS. Once through an application LHS, lambda bodies no longer count as pass-through."""
    
    return hoist_subterms(should_hoist_case_statement, v1, v2)

def hoist_case_statements_in_graph(graph: hydra.graph.Graph) -> hydra.compute.Flow[T0, hydra.graph.Graph]:
    @lru_cache(1)
    def empty_ix() -> hydra.core.Type:
        return hydra.typing.InferenceContext(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), False)
    @lru_cache(1)
    def empty_tx() -> hydra.core.Type:
        return hydra.typing.TypeContext(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), empty_ix())
    @lru_cache(1)
    def gterm0() -> hydra.core.Type:
        return hydra.schemas.graph_as_term(graph)
    @lru_cache(1)
    def gterm1() -> hydra.core.Type:
        return hoist_case_statements(empty_tx(), gterm0())
    @lru_cache(1)
    def new_elements() -> frozenlist[hydra.core.Binding]:
        return hydra.schemas.term_as_graph(gterm1())
    return hydra.lib.flows.pure(hydra.graph.Graph(new_elements(), graph.environment, graph.types, graph.body, graph.primitives, graph.schema))

def should_hoist_polymorphic(cx: hydra.typing.TypeContext, binding: hydra.core.Binding) -> bool:
    r"""Predicate for hoisting polymorphic bindings. Returns True if the binding is polymorphic (has type scheme variables) or if its type uses any type variables from the TypeContext."""
    
    return hydra.lib.logic.or_(binding_is_polymorphic(binding), binding_uses_context_type_vars(cx, binding))

def hoist_let_bindings_with_context(is_parent_binding: Callable[[hydra.core.Binding], bool], cx: hydra.typing.TypeContext, let0: hydra.core.Let) -> hydra.core.Type:
    r"""Transform a let-term by pulling polymorphic let bindings to the top level, using TypeContext. A binding is hoisted if: (1) It is polymorphic (has non-empty typeSchemeVariables), OR (2) Its type uses type variables from the TypeContext (i.e., from enclosing type lambdas). Bindings which are already at the top level are not hoisted. If a hoisted binding captures lambda-bound or let-bound variables from an enclosing scope, the binding is wrapped in lambdas for those variables, and references are replaced with applications. If a hoisted binding uses type variables from the context, those type variables are added to the binding's type scheme. Note: we assume that there is no variable shadowing; use hydra.rewriting.unshadowVariables first."""
    
    return hoist_let_bindings_with_predicate(is_parent_binding, should_hoist_polymorphic, cx, let0)

def hoist_polymorphic_let_bindings(is_parent_binding: Callable[[hydra.core.Binding], bool], let0: hydra.core.Let) -> hydra.core.Type:
    r"""Transform a let-term by pulling all polymorphic let bindings to the top level. This is useful to ensure that polymorphic bindings are not nested within other terms, which is unsupported by certain targets such as Java. Polymorphic bindings are those with a non-empty list of type scheme variables. If a hoisted binding captures lambda-bound variables from an enclosing scope, the binding is wrapped in lambdas for those variables, and references are replaced with applications. Note: Assumes no variable shadowing; use hydra.rewriting.unshadowVariables first."""
    
    @lru_cache(1)
    def empty_ix() -> hydra.core.Type:
        return hydra.typing.InferenceContext(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), False)
    @lru_cache(1)
    def empty_cx() -> hydra.core.Type:
        return hydra.typing.TypeContext(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), empty_ix())
    return hoist_let_bindings_with_predicate(is_parent_binding, should_hoist_polymorphic, empty_cx(), let0)

def is_application_function(acc: hydra.accessors.TermAccessor) -> bool:
    match acc:
        case hydra.accessors.TermAccessorApplicationFunction():
            return True
        
        case _:
            return False

def is_lambda_body(acc: hydra.accessors.TermAccessor) -> bool:
    match acc:
        case hydra.accessors.TermAccessorLambdaBody():
            return True
        
        case _:
            return False

def normalize_path_for_hoisting(path: frozenlist[hydra.accessors.TermAccessor]) -> frozenlist[hydra.accessors.TermAccessor]:
    r"""Normalize a path for hoisting by treating immediately-applied lambdas as let bindings. Replaces [applicationFunction, lambdaBody, ...] with [letBody, ...]."""
    
    def go(remaining: frozenlist[hydra.accessors.TermAccessor]) -> frozenlist[hydra.accessors.TermAccessor]:
        return hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.lists.null(remaining), hydra.lib.lists.null(hydra.lib.lists.tail(remaining))), (lambda : remaining), (lambda : (first := hydra.lib.lists.head(remaining), (second := hydra.lib.lists.head(hydra.lib.lists.tail(remaining)), (rest := hydra.lib.lists.tail(hydra.lib.lists.tail(remaining)), hydra.lib.logic.if_else(hydra.lib.logic.and_(is_application_function(first), is_lambda_body(second)), (lambda : hydra.lib.lists.cons(cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLetBody()), go(rest))), (lambda : hydra.lib.lists.cons(first, go(hydra.lib.lists.tail(remaining))))))[1])[1])[1]))
    return go(path)

def rewrite_term_with_type_context(f: Callable[[
  Callable[[hydra.core.Term], T0],
  hydra.typing.TypeContext,
  hydra.core.Term], T0], cx0: hydra.typing.TypeContext, term0: hydra.core.Term) -> T0:
    def f2(recurse: Callable[[hydra.typing.TypeContext, hydra.core.Term], T0], cx: hydra.typing.TypeContext, term: hydra.core.Term) -> T0:
        def recurse1(term2: hydra.core.Term) -> T0:
            return recurse(cx, term2)
        def _hoist_body_1(v1: hydra.core.Function) -> T0:
            match v1:
                case hydra.core.FunctionLambda(value=l):
                    @lru_cache(1)
                    def cx1() -> hydra.core.Type:
                        return hydra.schemas.extend_type_context_for_lambda(cx, l)
                    def recurse2(term2: hydra.core.Term) -> T0:
                        return recurse(cx1(), term2)
                    return f(recurse2, cx1(), term)
                
                case _:
                    return f(recurse1, cx, term)
        match term:
            case hydra.core.TermFunction(value=fun):
                return _hoist_body_1(fun)
            
            case hydra.core.TermLet(value=l):
                @lru_cache(1)
                def cx1() -> hydra.core.Type:
                    return hydra.schemas.extend_type_context_for_let((lambda _, _2: Nothing()), cx, l)
                def recurse2(term2: hydra.core.Term) -> T0:
                    return recurse(cx1(), term2)
                return f(recurse2, cx1(), term)
            
            case hydra.core.TermTypeLambda(value=tl):
                @lru_cache(1)
                def cx1() -> hydra.core.Type:
                    return hydra.schemas.extend_type_context_for_type_lambda(cx, tl)
                def recurse2(term2: hydra.core.Term) -> T0:
                    return recurse(cx1(), term2)
                return f(recurse2, cx1(), term)
            
            case _:
                return f(recurse1, cx, term)
    def rewrite(cx: hydra.typing.TypeContext, term: hydra.core.Term) -> T0:
        return f2(rewrite, cx, term)
    return rewrite(cx0, term0)
