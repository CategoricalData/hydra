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
import hydra.typing

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")
T4 = TypeVar("T4")

def binding_is_polymorphic(binding: hydra.core.Binding) -> bool:
    r"""Check if a binding has a polymorphic type (non-empty list of type scheme variables)."""
    
    return hydra.lib.maybes.maybe(False, (lambda ts: hydra.lib.logic.not_(hydra.lib.lists.null(ts.variables))), binding.type)

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
        return hydra.typing.InferenceContext(FrozenDict({}), FrozenDict({}), FrozenDict({}), FrozenDict({}), False)
    @lru_cache(1)
    def empty_tx() -> hydra.core.Type:
        return hydra.typing.TypeContext(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), empty_ix())
    @lru_cache(1)
    def gterm0() -> hydra.core.Type:
        return hydra.schemas.graph_as_term(graph)
    @lru_cache(1)
    def gterm1() -> hydra.core.Type:
        return hoist_case_statements(empty_tx(), gterm0())
    @lru_cache(1)
    def new_elements() -> FrozenDict[hydra.core.Name, hydra.core.Binding]:
        return hydra.schemas.term_as_graph(gterm1())
    return hydra.lib.flows.pure(hydra.graph.Graph(new_elements(), graph.environment, graph.types, graph.body, graph.primitives, graph.schema))

def hoist_let_bindings(hoist_all: bool, let0: hydra.core.Let) -> hydra.core.Type:
    r"""Transform a let-term by pulling let bindings to the top level. The hoistAll parameter controls whether to hoist all bindings (True) or only polymorphic ones (False). This is useful for targets like Java that cannot have let-expressions in arbitrary positions. Polymorphic bindings are those with a non-empty list of type scheme variables, OR bindings inside a type lambda scope (which use outer type variables in their types). If a hoisted binding captures lambda-bound variables from an enclosing scope, the binding is wrapped in lambdas for those variables, and references are replaced with applications. If a hoisted binding captures type variables from an enclosing type lambda scope, those type variables are added to the binding's type scheme, and references are replaced with type applications. Note: Assumes no variable shadowing; use hydra.rewriting.unshadowVariables first."""
    
    @lru_cache(1)
    def top_level_names() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda b: b.name), let0.bindings))
    @lru_cache(1)
    def empty_hoisted() -> frozenlist[T0]:
        return ()
    def process_term_for_hoisting(type_vars: frozenlist[hydra.core.Name], lambda_vars: frozenlist[tuple[hydra.core.Name, Maybe[hydra.core.Type]]], reserved: T0, term: hydra.core.Term) -> tuple[frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]], tuple[T0, hydra.core.Term]]:
        def _hoist_process_term_for_hoisting_1(lambda_vars: frozenlist[tuple[hydra.core.Name, Maybe[hydra.core.Type]]], reserved: T0, term: hydra.core.Term, type_vars: frozenlist[hydra.core.Name], v1: hydra.core.Function) -> tuple[frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]], tuple[T0, hydra.core.Term]]:
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    @lru_cache(1)
                    def param_name() -> hydra.core.Type:
                        return lam.parameter
                    @lru_cache(1)
                    def param_domain() -> Maybe[hydra.core.Type]:
                        return lam.domain
                    @lru_cache(1)
                    def new_lambda_vars() -> frozenlist[tuple[hydra.core.Name, Maybe[hydra.core.Type]]]:
                        return hydra.lib.lists.concat2(lambda_vars, hydra.lib.lists.pure((param_name(), param_domain())))
                    @lru_cache(1)
                    def body_result() -> tuple[frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]], tuple[T0, hydra.core.Term]]:
                        return process_term_for_hoisting(type_vars, new_lambda_vars(), reserved, lam.body)
                    @lru_cache(1)
                    def hoisted() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
                        return hydra.lib.pairs.first(body_result())
                    @lru_cache(1)
                    def new_reserved() -> T0:
                        return hydra.lib.pairs.first(hydra.lib.pairs.second(body_result()))
                    @lru_cache(1)
                    def new_body() -> hydra.core.Type:
                        return hydra.lib.pairs.second(hydra.lib.pairs.second(body_result()))
                    return (hoisted(), (new_reserved(), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(param_name(), param_domain(), new_body())))))))
                
                case _:
                    return (empty_hoisted(), (reserved, term))
        match term:
            case hydra.core.TermLet(value=lt):
                @lru_cache(1)
                def bindings_result() -> tuple[frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]], tuple[T0, frozenlist[hydra.core.Binding]]]:
                    return hydra.lib.lists.foldl((lambda v1, v2: process_binding(type_vars, lambda_vars, v1, v2)), (empty_hoisted(), (reserved, ())), lt.bindings)
                @lru_cache(1)
                def hoisted_from_bindings() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
                    return hydra.lib.pairs.first(bindings_result())
                @lru_cache(1)
                def reserved_after_bindings() -> T0:
                    return hydra.lib.pairs.first(hydra.lib.pairs.second(bindings_result()))
                @lru_cache(1)
                def kept_bindings_raw() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.reverse(hydra.lib.pairs.second(hydra.lib.pairs.second(bindings_result())))
                @lru_cache(1)
                def body_result() -> tuple[frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]], tuple[T0, hydra.core.Term]]:
                    return process_term_for_hoisting(type_vars, lambda_vars, reserved_after_bindings(), lt.body)
                @lru_cache(1)
                def hoisted_from_body_raw() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
                    return hydra.lib.pairs.first(body_result())
                @lru_cache(1)
                def reserved_after_body() -> T0:
                    return hydra.lib.pairs.first(hydra.lib.pairs.second(body_result()))
                @lru_cache(1)
                def processed_body_raw() -> hydra.core.Type:
                    return hydra.lib.pairs.second(hydra.lib.pairs.second(body_result()))
                @lru_cache(1)
                def kept_names() -> frozenset[hydra.core.Name]:
                    return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda b: b.name), kept_bindings_raw()))
                @lru_cache(1)
                def hoisted_body_free_vars() -> frozenset[hydra.core.Name]:
                    return hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda info: hydra.lib.sets.to_list(hydra.rewriting.free_variables_in_term(hydra.lib.pairs.first(hydra.lib.pairs.second(info))))), hoisted_from_body_raw())))
                @lru_cache(1)
                def forced_hoist_names() -> frozenset[hydra.core.Name]:
                    return hydra.lib.sets.intersection(kept_names(), hoisted_body_free_vars())
                @lru_cache(1)
                def force_hoisted_bindings() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.filter((lambda b: hydra.lib.sets.member(b.name, forced_hoist_names())), kept_bindings_raw())
                @lru_cache(1)
                def truly_kept_bindings_raw() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.filter((lambda b: hydra.lib.logic.not_(hydra.lib.sets.member(b.name, forced_hoist_names()))), kept_bindings_raw())
                @lru_cache(1)
                def force_hoisted_bindings_with_replaced_refs() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.map((lambda b: (updated_term := replace_references(hoisted_from_bindings(), b.term), hydra.core.Binding(b.name, updated_term, b.type))[1]), force_hoisted_bindings())
                @lru_cache(1)
                def force_hoisted_info() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[T1], Maybe[hydra.core.TypeScheme]]]]]]:
                    return hydra.lib.lists.map((lambda binding: (binding_name := binding.name, free_vars := hydra.rewriting.free_variables_in_term(binding.term), lambda_var_names := hydra.lib.sets.from_list(hydra.lib.lists.map((lambda lv: hydra.lib.pairs.first(lv)), lambda_vars)), captured_var_names := hydra.lib.sets.intersection(lambda_var_names, free_vars), captured_term_vars := hydra.lib.lists.filter((lambda lv: hydra.lib.sets.member(hydra.lib.pairs.first(lv), captured_var_names)), lambda_vars), wrapped_term := hydra.lib.lists.foldl((lambda body, lv: cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.lib.pairs.first(lv), hydra.lib.pairs.second(lv), body)))))), binding.term, hydra.lib.lists.reverse(captured_term_vars)), wrapped_type := hydra.lib.maybes.maybe(Nothing(), (lambda ts: (orig_type := ts.type, new_type := hydra.lib.lists.foldl((lambda inner_type, lv: hydra.lib.maybes.maybe(inner_type, (lambda domain_type: cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(domain_type, inner_type)))), hydra.lib.pairs.second(lv))), orig_type, hydra.lib.lists.reverse(captured_term_vars)), Just(hydra.core.TypeScheme(ts.variables, new_type, ts.constraints)))[2]), binding.type), (binding_name, (wrapped_term, (hydra.lib.lists.map((lambda lv: hydra.lib.pairs.first(lv)), captured_term_vars), ((), wrapped_type)))))[7]), force_hoisted_bindings_with_replaced_refs())
                @lru_cache(1)
                def all_hoisted_from_this_level() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
                    return hydra.lib.lists.concat2(hoisted_from_bindings(), force_hoisted_info())
                @lru_cache(1)
                def hoisted_from_body() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
                    return hydra.lib.lists.map((lambda info: (info_name := get_info_name(info), info_term := hydra.lib.pairs.first(hydra.lib.pairs.second(info)), info_cap_term_vars := get_info_captured_term_vars(info), info_cap_type_vars := get_info_captured_type_vars(info), info_type := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(info)))), updated_term := replace_references(all_hoisted_from_this_level(), info_term), (info_name, (updated_term, (info_cap_term_vars, (info_cap_type_vars, info_type)))))[6]), hoisted_from_body_raw())
                @lru_cache(1)
                def all_hoisted() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
                    return hydra.lib.lists.concat((hoisted_from_body(), hoisted_from_bindings(), force_hoisted_info()))
                @lru_cache(1)
                def kept_bindings() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.lists.map((lambda b: (updated_term := replace_references(all_hoisted_from_this_level(), b.term), hydra.core.Binding(b.name, updated_term, b.type))[1]), truly_kept_bindings_raw())
                @lru_cache(1)
                def processed_body() -> hydra.core.Type:
                    return replace_references(all_hoisted_from_this_level(), processed_body_raw())
                return hydra.lib.logic.if_else(hydra.lib.lists.null(kept_bindings()), (lambda : (all_hoisted(), (reserved_after_body(), processed_body()))), (lambda : (all_hoisted(), (reserved_after_body(), cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(kept_bindings(), processed_body())))))))
            
            case hydra.core.TermFunction(value=f):
                return _hoist_process_term_for_hoisting_1(lambda_vars, reserved, term, type_vars, f)
            
            case hydra.core.TermApplication(value=app):
                @lru_cache(1)
                def fn_result() -> tuple[frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]], tuple[T0, hydra.core.Term]]:
                    return process_term_for_hoisting(type_vars, lambda_vars, reserved, app.function)
                @lru_cache(1)
                def fn_hoisted() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
                    return hydra.lib.pairs.first(fn_result())
                @lru_cache(1)
                def reserved_after_fn() -> T0:
                    return hydra.lib.pairs.first(hydra.lib.pairs.second(fn_result()))
                @lru_cache(1)
                def new_fn() -> hydra.core.Type:
                    return hydra.lib.pairs.second(hydra.lib.pairs.second(fn_result()))
                @lru_cache(1)
                def arg_result() -> tuple[frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]], tuple[T0, hydra.core.Term]]:
                    return process_term_for_hoisting(type_vars, lambda_vars, reserved_after_fn(), app.argument)
                @lru_cache(1)
                def arg_hoisted() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
                    return hydra.lib.pairs.first(arg_result())
                @lru_cache(1)
                def reserved_after_arg() -> T0:
                    return hydra.lib.pairs.first(hydra.lib.pairs.second(arg_result()))
                @lru_cache(1)
                def new_arg() -> hydra.core.Type:
                    return hydra.lib.pairs.second(hydra.lib.pairs.second(arg_result()))
                return (hydra.lib.lists.concat2(arg_hoisted(), fn_hoisted()), (reserved_after_arg(), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(new_fn(), new_arg())))))
            
            case hydra.core.TermAnnotated(value=ann):
                @lru_cache(1)
                def body_result() -> tuple[frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]], tuple[T0, hydra.core.Term]]:
                    return process_term_for_hoisting(type_vars, lambda_vars, reserved, ann.body)
                @lru_cache(1)
                def hoisted() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
                    return hydra.lib.pairs.first(body_result())
                @lru_cache(1)
                def new_reserved() -> T0:
                    return hydra.lib.pairs.first(hydra.lib.pairs.second(body_result()))
                @lru_cache(1)
                def new_body() -> hydra.core.Type:
                    return hydra.lib.pairs.second(hydra.lib.pairs.second(body_result()))
                return (hoisted(), (new_reserved(), cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(new_body(), ann.annotation)))))
            
            case hydra.core.TermTypeLambda(value=tl):
                @lru_cache(1)
                def type_param() -> hydra.core.Type:
                    return tl.parameter
                @lru_cache(1)
                def new_type_vars() -> frozenlist[hydra.core.Name]:
                    return hydra.lib.lists.concat2(type_vars, hydra.lib.lists.pure(type_param()))
                @lru_cache(1)
                def body_result() -> tuple[frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]], tuple[T0, hydra.core.Term]]:
                    return process_term_for_hoisting(new_type_vars(), lambda_vars, reserved, tl.body)
                @lru_cache(1)
                def hoisted() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
                    return hydra.lib.pairs.first(body_result())
                @lru_cache(1)
                def new_reserved() -> T0:
                    return hydra.lib.pairs.first(hydra.lib.pairs.second(body_result()))
                @lru_cache(1)
                def new_body() -> hydra.core.Type:
                    return hydra.lib.pairs.second(hydra.lib.pairs.second(body_result()))
                return (hoisted(), (new_reserved(), cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(type_param(), new_body())))))
            
            case hydra.core.TermTypeApplication(value=ta):
                @lru_cache(1)
                def body_result() -> tuple[frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]], tuple[T0, hydra.core.Term]]:
                    return process_term_for_hoisting(type_vars, lambda_vars, reserved, ta.body)
                @lru_cache(1)
                def hoisted() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
                    return hydra.lib.pairs.first(body_result())
                @lru_cache(1)
                def new_reserved() -> T0:
                    return hydra.lib.pairs.first(hydra.lib.pairs.second(body_result()))
                @lru_cache(1)
                def new_body() -> hydra.core.Type:
                    return hydra.lib.pairs.second(hydra.lib.pairs.second(body_result()))
                return (hoisted(), (new_reserved(), cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(new_body(), ta.type)))))
            
            case _:
                return (empty_hoisted(), (reserved, term))
    def process_binding(type_vars: frozenlist[hydra.core.Name], lambda_vars: frozenlist[tuple[hydra.core.Name, Maybe[hydra.core.Type]]], state: tuple[frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]], tuple[T0, frozenlist[hydra.core.Binding]]], binding: hydra.core.Binding) -> tuple[frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]], tuple[T0, frozenlist[hydra.core.Binding]]]:
        @lru_cache(1)
        def hoisted() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
            return hydra.lib.pairs.first(state)
        @lru_cache(1)
        def reserved() -> T0:
            return hydra.lib.pairs.first(hydra.lib.pairs.second(state))
        @lru_cache(1)
        def kept() -> frozenlist[hydra.core.Binding]:
            return hydra.lib.pairs.second(hydra.lib.pairs.second(state))
        @lru_cache(1)
        def processed_term() -> tuple[frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]], tuple[T0, hydra.core.Term]]:
            return process_term_for_hoisting(type_vars, lambda_vars, reserved(), binding.term)
        @lru_cache(1)
        def inner_hoisted() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
            return hydra.lib.pairs.first(processed_term())
        @lru_cache(1)
        def new_reserved() -> T0:
            return hydra.lib.pairs.first(hydra.lib.pairs.second(processed_term()))
        @lru_cache(1)
        def new_term() -> hydra.core.Type:
            return hydra.lib.pairs.second(hydra.lib.pairs.second(processed_term()))
        @lru_cache(1)
        def is_inside_type_lambda() -> bool:
            return hydra.lib.logic.not_(hydra.lib.lists.null(type_vars))
        @lru_cache(1)
        def used_type_vars() -> frozenlist[hydra.core.Name]:
            return hydra.lib.maybes.maybe((), (lambda ts: (free_in_type := hydra.rewriting.free_variables_in_type(ts.type), hydra.lib.lists.filter((lambda tv: hydra.lib.sets.member(tv, free_in_type)), type_vars))[1]), binding.type)
        @lru_cache(1)
        def uses_outer_type_vars() -> bool:
            return hydra.lib.logic.not_(hydra.lib.lists.null(used_type_vars()))
        @lru_cache(1)
        def should_hoist() -> bool:
            return hydra.lib.logic.if_else(hoist_all, (lambda : hydra.lib.logic.not_(is_inside_type_lambda())), (lambda : hydra.lib.logic.or_(binding_is_polymorphic(binding), uses_outer_type_vars())))
        return hydra.lib.logic.if_else(should_hoist(), (lambda : (binding_name := binding.name, (free_vars := hydra.rewriting.free_variables_in_term(new_term()), (lambda_var_names := hydra.lib.sets.from_list(hydra.lib.lists.map((lambda lv: hydra.lib.pairs.first(lv)), lambda_vars)), (captured_var_names := hydra.lib.sets.intersection(lambda_var_names, free_vars), (captured_term_vars := hydra.lib.lists.filter((lambda lv: hydra.lib.sets.member(hydra.lib.pairs.first(lv), captured_var_names)), lambda_vars), (captured_type_vars := hydra.lib.maybes.maybe((), (lambda ts: (free_in_type := hydra.rewriting.free_variables_in_type(ts.type), already_quantified := hydra.lib.sets.from_list(ts.variables), all_unquantified := hydra.lib.sets.to_list(hydra.lib.sets.difference(free_in_type, already_quantified)), hydra.lib.lists.concat2(used_type_vars(), hydra.lib.lists.filter((lambda tv: hydra.lib.logic.not_(hydra.lib.sets.member(tv, hydra.lib.sets.from_list(used_type_vars())))), all_unquantified)))[3]), binding.type), (wrapped_term := hydra.lib.lists.foldl((lambda body, lv: cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.lib.pairs.first(lv), hydra.lib.pairs.second(lv), body)))))), new_term(), hydra.lib.lists.reverse(captured_term_vars)), (wrapped_type := hydra.lib.maybes.maybe(Nothing(), (lambda ts: (orig_type := ts.type, orig_type_vars := ts.variables, new_type_vars := hydra.lib.lists.concat2(captured_type_vars, orig_type_vars), new_type := hydra.lib.lists.foldl((lambda inner_type, lv: hydra.lib.maybes.maybe(inner_type, (lambda domain_type: cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(domain_type, inner_type)))), hydra.lib.pairs.second(lv))), orig_type, hydra.lib.lists.reverse(captured_term_vars)), Just(hydra.core.TypeScheme(new_type_vars, new_type, ts.constraints)))[4]), binding.type), (hoisted_info := (binding_name, (wrapped_term, (hydra.lib.lists.map((lambda lv: hydra.lib.pairs.first(lv)), captured_term_vars), (captured_type_vars, wrapped_type)))), (hydra.lib.lists.cons(hoisted_info, hydra.lib.lists.concat2(inner_hoisted(), hoisted())), (new_reserved(), kept())))[1])[1])[1])[1])[1])[1])[1])[1])[1]), (lambda : (processed_binding := hydra.core.Binding(binding.name, new_term(), binding.type), (hydra.lib.lists.concat2(inner_hoisted(), hoisted()), (new_reserved(), hydra.lib.lists.cons(processed_binding, kept()))))[1]))
    def replace_references(hoisted_info_list: frozenlist[tuple[hydra.core.Name, tuple[T0, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], T1]]]]], term: hydra.core.Term) -> hydra.core.Type:
        def _hoist_replace_references_1(hoisted_info_list: frozenlist[tuple[hydra.core.Name, tuple[T2, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], T3]]]]], recurse: Callable[[hydra.core.Term], hydra.core.Term], t: hydra.core.Term, v1: hydra.core.Term) -> hydra.core.Type:
            match v1:
                case hydra.core.TermVariable(value=var_name):
                    @lru_cache(1)
                    def matching_info() -> frozenlist[tuple[hydra.core.Name, tuple[T2, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], T3]]]]]:
                        return hydra.lib.lists.filter((lambda info: hydra.lib.equality.equal(var_name, get_info_name(info))), hoisted_info_list)
                    return hydra.lib.logic.if_else(hydra.lib.lists.null(matching_info()), (lambda : t), (lambda : (info := hydra.lib.lists.head(matching_info()), (captured_term_vars := get_info_captured_term_vars(info), (captured_type_vars := get_info_captured_type_vars(info), (with_type_apps := hydra.lib.lists.foldl((lambda fn, type_var_name: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(fn, cast(hydra.core.Type, hydra.core.TypeVariable(type_var_name)))))), cast(hydra.core.Term, hydra.core.TermVariable(var_name)), captured_type_vars), hydra.lib.lists.foldl((lambda fn, captured_name: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fn, cast(hydra.core.Term, hydra.core.TermVariable(captured_name)))))), with_type_apps, captured_term_vars))[1])[1])[1])[1]))
                
                case _:
                    return recurse(t)
        return hydra.rewriting.rewrite_term((lambda recurse, t: _hoist_replace_references_1(hoisted_info_list, recurse, t, t)), term)
    def get_info_name(info: tuple[T0, T1]) -> T0:
        return hydra.lib.pairs.first(info)
    def get_info_captured_term_vars(info: tuple[T0, tuple[T1, tuple[T2, T3]]]) -> T2:
        return hydra.lib.pairs.first(hydra.lib.pairs.second(hydra.lib.pairs.second(info)))
    def get_info_captured_type_vars(info: tuple[T0, tuple[T1, tuple[T2, tuple[T3, T4]]]]) -> T3:
        return hydra.lib.pairs.first(hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(info))))
    @lru_cache(1)
    def empty_type_vars() -> frozenlist[T0]:
        return ()
    @lru_cache(1)
    def empty_lambda_vars() -> frozenlist[T0]:
        return ()
    @lru_cache(1)
    def result() -> tuple[frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]], tuple[frozenset[hydra.core.Name], frozenlist[hydra.core.Binding]]]:
        return hydra.lib.lists.foldl((lambda v1, v2: process_binding(empty_type_vars(), empty_lambda_vars(), v1, v2)), (empty_hoisted(), (top_level_names(), ())), let0.bindings)
    @lru_cache(1)
    def hoisted_info() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
        return hydra.lib.lists.reverse(hydra.lib.pairs.first(result()))
    @lru_cache(1)
    def reserved_names() -> frozenset[hydra.core.Name]:
        return hydra.lib.pairs.first(hydra.lib.pairs.second(result()))
    @lru_cache(1)
    def kept_bindings() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.reverse(hydra.lib.pairs.second(hydra.lib.pairs.second(result())))
    @lru_cache(1)
    def body_result() -> tuple[frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]], tuple[frozenset[hydra.core.Name], hydra.core.Term]]:
        return process_term_for_hoisting(empty_type_vars(), empty_lambda_vars(), reserved_names(), let0.body)
    @lru_cache(1)
    def hoisted_from_body() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
        return hydra.lib.lists.reverse(hydra.lib.pairs.first(body_result()))
    @lru_cache(1)
    def processed_body() -> hydra.core.Type:
        return hydra.lib.pairs.second(hydra.lib.pairs.second(body_result()))
    @lru_cache(1)
    def all_hoisted_info() -> frozenlist[tuple[hydra.core.Name, tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Name], tuple[frozenlist[hydra.core.Name], Maybe[hydra.core.TypeScheme]]]]]]:
        return hydra.lib.lists.concat2(hoisted_info(), hoisted_from_body())
    @lru_cache(1)
    def hoisted_bindings() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.map((lambda info: (name := get_info_name(info), wrapped_term := hydra.lib.pairs.first(hydra.lib.pairs.second(info)), wrapped_type := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(info)))), hydra.core.Binding(name, wrapped_term, wrapped_type))[3]), all_hoisted_info())
    @lru_cache(1)
    def all_bindings() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.concat2(hoisted_bindings(), kept_bindings())
    return hydra.core.Let(all_bindings(), processed_body())

def hoist_polymorphic_let_bindings(v1: hydra.core.Let) -> hydra.core.Type:
    r"""Transform a let-term by pulling all polymorphic let bindings to the top level. This is useful to ensure that polymorphic bindings are not nested within other terms, which is unsupported by certain targets such as Java. Polymorphic bindings are those with a non-empty list of type scheme variables. If a hoisted binding captures lambda-bound variables from an enclosing scope, the binding is wrapped in lambdas for those variables, and references are replaced with applications. Note: Assumes no variable shadowing; use hydra.rewriting.unshadowVariables first."""
    
    return hoist_let_bindings(False, v1)

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
