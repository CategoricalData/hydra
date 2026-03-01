# Note: this is an automatically generated file. Do not edit.

r"""Common utilities for language coders, providing shared patterns for term decomposition and analysis."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.arity
import hydra.checking
import hydra.coders
import hydra.core
import hydra.formatting
import hydra.graph
import hydra.lexical
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.monads
import hydra.rewriting
import hydra.schemas
import hydra.typing

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def try_type_of(msg: str, tc: hydra.graph.Graph, term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Type]:
    r"""Infer the type of a term with tracing."""
    
    return hydra.monads.with_trace(msg, hydra.checking.type_of(tc, (), term))

def analyze_function_term_with_finish(get_t_c: Callable[[T0], hydra.graph.Graph], f_env: T0, tparams: frozenlist[hydra.core.Name], args: frozenlist[hydra.core.Name], bindings: frozenlist[hydra.core.Binding], doms: frozenlist[hydra.core.Type], tapps: frozenlist[hydra.core.Type], body: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.typing.FunctionStructure[T0]]:
    @lru_cache(1)
    def body_with_tapps() -> hydra.core.Term:
        return hydra.lib.lists.foldl((lambda trm, typ: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(trm, typ)))), body, tapps)
    return hydra.lib.flows.bind(hydra.lib.flows.with_default(Nothing(), hydra.lib.flows.map((lambda x1: hydra.lib.maybes.pure(x1)), try_type_of("analyzeFunctionTermWith", get_t_c(f_env), body_with_tapps()))), (lambda mcod: hydra.lib.flows.pure(hydra.typing.FunctionStructure(hydra.lib.lists.reverse(tparams), hydra.lib.lists.reverse(args), bindings, body_with_tapps(), hydra.lib.lists.reverse(doms), mcod, f_env))))

def analyze_function_term_with_gather(for_binding: Callable[[hydra.graph.Graph, hydra.core.Binding], Maybe[hydra.core.Term]], get_t_c: Callable[[T0], hydra.graph.Graph], set_t_c: Callable[[hydra.graph.Graph, T0], T0], arg_mode: bool, g_env: T0, tparams: frozenlist[hydra.core.Name], args: frozenlist[hydra.core.Name], bindings: frozenlist[hydra.core.Binding], doms: frozenlist[hydra.core.Type], tapps: frozenlist[hydra.core.Type], t: hydra.core.Term):
    while True:
        def _hoist_hydra_coder_utils_analyze_function_term_with_gather_1(arg_mode, args, bindings, doms, for_binding, g_env, get_t_c, set_t_c, t, tapps, tparams, v1):
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    return hydra.lib.logic.if_else(arg_mode, (lambda : (v := lam.parameter, (dom := hydra.lib.maybes.maybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("_"))), (lambda x_: x_), lam.domain), (body := lam.body, (new_env := set_t_c(hydra.schemas.extend_graph_for_lambda(get_t_c(g_env), lam), g_env), analyze_function_term_with_gather(for_binding, get_t_c, set_t_c, arg_mode, new_env, tparams, hydra.lib.lists.cons(v, args), bindings, hydra.lib.lists.cons(dom, doms), tapps, body))[1])[1])[1])[1]), (lambda : analyze_function_term_with_finish(get_t_c, g_env, tparams, args, bindings, doms, tapps, t)))
                
                case _:
                    return analyze_function_term_with_finish(get_t_c, g_env, tparams, args, bindings, doms, tapps, t)
        match hydra.rewriting.deannotate_term(t):
            case hydra.core.TermFunction(value=f):
                return _hoist_hydra_coder_utils_analyze_function_term_with_gather_1(arg_mode, args, bindings, doms, for_binding, g_env, get_t_c, set_t_c, t, tapps, tparams, f)
            
            case hydra.core.TermLet(value=lt):
                return (new_bindings := lt.bindings, (body := lt.body, (new_env := set_t_c(hydra.schemas.extend_graph_for_let(for_binding, get_t_c(g_env), lt), g_env), analyze_function_term_with_gather(for_binding, get_t_c, set_t_c, False, new_env, tparams, args, hydra.lib.lists.concat2(bindings, new_bindings), doms, tapps, body))[1])[1])[1]
            
            case hydra.core.TermTypeApplication(value=ta):
                return (ta_body := ta.body, (typ := ta.type, analyze_function_term_with_gather(for_binding, get_t_c, set_t_c, arg_mode, g_env, tparams, args, bindings, doms, hydra.lib.lists.cons(typ, tapps), ta_body))[1])[1]
            
            case hydra.core.TermTypeLambda(value=tl):
                return (tvar := tl.parameter, (tl_body := tl.body, (new_env := set_t_c(hydra.schemas.extend_graph_for_type_lambda(get_t_c(g_env), tl), g_env), analyze_function_term_with_gather(for_binding, get_t_c, set_t_c, arg_mode, new_env, hydra.lib.lists.cons(tvar, tparams), args, bindings, doms, tapps, tl_body))[1])[1])[1]
            
            case _:
                return analyze_function_term_with_finish(get_t_c, g_env, tparams, args, bindings, doms, tapps, t)

def analyze_function_term_with(for_binding: Callable[[hydra.graph.Graph, hydra.core.Binding], Maybe[hydra.core.Term]], get_t_c: Callable[[T0], hydra.graph.Graph], set_t_c: Callable[[hydra.graph.Graph, T0], T0], env: T0, term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.typing.FunctionStructure[T0]]:
    r"""Analyze a function term with configurable binding metadata."""
    
    return analyze_function_term_with_gather(for_binding, get_t_c, set_t_c, True, env, (), (), (), (), (), term)

def is_complex_variable(tc: hydra.graph.Graph, name: hydra.core.Name) -> bool:
    r"""Check if a variable is bound to a complex term."""
    
    @lru_cache(1)
    def meta_lookup() -> Maybe[hydra.core.Term]:
        return hydra.lib.maps.lookup(name, tc.metadata)
    return hydra.lib.logic.if_else(hydra.lib.maybes.is_just(meta_lookup()), (lambda : True), (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(name, tc.lambda_variables), (lambda : True), (lambda : (type_lookup := hydra.lib.maps.lookup(name, tc.bound_types), hydra.lib.logic.not_(hydra.lib.maybes.is_just(type_lookup)))[1]))))

def is_complex_term(tc: hydra.graph.Graph, t: hydra.core.Term) -> bool:
    r"""Check if a term needs to be treated as a function rather than a simple value."""
    
    match t:
        case hydra.core.TermLet():
            return True
        
        case hydra.core.TermTypeApplication():
            return True
        
        case hydra.core.TermTypeLambda():
            return True
        
        case hydra.core.TermVariable(value=name):
            return is_complex_variable(tc, name)
        
        case _:
            return hydra.lib.lists.foldl((lambda b, sub: hydra.lib.logic.or_(b, is_complex_term(tc, sub))), False, hydra.rewriting.subterms(t))

def is_complex_binding(tc: hydra.graph.Graph, b: hydra.core.Binding) -> bool:
    r"""Check if a binding needs to be treated as a function."""
    
    term = b.term
    mts = b.type
    return hydra.lib.maybes.cases(mts, is_complex_term(tc, term), (lambda ts: (is_polymorphic := hydra.lib.logic.not_(hydra.lib.lists.null(ts.variables)), is_non_nullary := hydra.lib.equality.gt(hydra.arity.type_arity(ts.type), 0), is_complex := is_complex_term(tc, term), hydra.lib.logic.or_(hydra.lib.logic.or_(is_polymorphic, is_non_nullary), is_complex))[3]))

def binding_metadata(tc: hydra.graph.Graph, b: hydra.core.Binding) -> Maybe[hydra.core.Term]:
    r"""Produces metadata for a binding if it is complex."""
    
    return hydra.lib.logic.if_else(is_complex_binding(tc, b), (lambda : Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True)))))), (lambda : Nothing()))

def analyze_function_term(get_t_c: Callable[[T0], hydra.graph.Graph], set_t_c: Callable[[hydra.graph.Graph, T0], T0], env: T0, term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.typing.FunctionStructure[T0]]:
    r"""Analyze a function term, collecting lambdas, type lambdas, lets, and type applications."""
    
    return analyze_function_term_with((lambda x1, x2: binding_metadata(x1, x2)), get_t_c, set_t_c, env, term)

def analyze_function_term_inline(get_t_c: Callable[[T0], hydra.graph.Graph], set_t_c: Callable[[hydra.graph.Graph, T0], T0], env: T0, term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.typing.FunctionStructure[T0]]:
    r"""Analyze a function term without recording binding metadata."""
    
    return analyze_function_term_with((lambda _, _2: Nothing()), get_t_c, set_t_c, env, term)

def analyze_function_term_no_infer_with_finish(f_env: T0, tparams: frozenlist[hydra.core.Name], args: frozenlist[hydra.core.Name], bindings: frozenlist[hydra.core.Binding], doms: frozenlist[hydra.core.Type], tapps: frozenlist[hydra.core.Type], body: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.typing.FunctionStructure[T0]]:
    @lru_cache(1)
    def body_with_tapps() -> hydra.core.Term:
        return hydra.lib.lists.foldl((lambda trm, typ: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(trm, typ)))), body, tapps)
    return hydra.lib.flows.pure(hydra.typing.FunctionStructure(hydra.lib.lists.reverse(tparams), hydra.lib.lists.reverse(args), bindings, body_with_tapps(), hydra.lib.lists.reverse(doms), Nothing(), f_env))

def analyze_function_term_no_infer_with_gather(for_binding: Callable[[hydra.graph.Graph, hydra.core.Binding], Maybe[hydra.core.Term]], get_t_c: Callable[[T0], hydra.graph.Graph], set_t_c: Callable[[hydra.graph.Graph, T0], T0], arg_mode: bool, g_env: T0, tparams: frozenlist[hydra.core.Name], args: frozenlist[hydra.core.Name], bindings: frozenlist[hydra.core.Binding], doms: frozenlist[hydra.core.Type], tapps: frozenlist[hydra.core.Type], t: hydra.core.Term):
    while True:
        def _hoist_hydra_coder_utils_analyze_function_term_no_infer_with_gather_1(arg_mode, args, bindings, doms, for_binding, g_env, get_t_c, set_t_c, t, tapps, tparams, v1):
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    return hydra.lib.logic.if_else(arg_mode, (lambda : (v := lam.parameter, (dom := hydra.lib.maybes.maybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("_"))), (lambda x_: x_), lam.domain), (body := lam.body, (new_env := set_t_c(hydra.schemas.extend_graph_for_lambda(get_t_c(g_env), lam), g_env), analyze_function_term_no_infer_with_gather(for_binding, get_t_c, set_t_c, arg_mode, new_env, tparams, hydra.lib.lists.cons(v, args), bindings, hydra.lib.lists.cons(dom, doms), tapps, body))[1])[1])[1])[1]), (lambda : analyze_function_term_no_infer_with_finish(g_env, tparams, args, bindings, doms, tapps, t)))
                
                case _:
                    return analyze_function_term_no_infer_with_finish(g_env, tparams, args, bindings, doms, tapps, t)
        match hydra.rewriting.deannotate_term(t):
            case hydra.core.TermFunction(value=f):
                return _hoist_hydra_coder_utils_analyze_function_term_no_infer_with_gather_1(arg_mode, args, bindings, doms, for_binding, g_env, get_t_c, set_t_c, t, tapps, tparams, f)
            
            case hydra.core.TermLet(value=lt):
                return (new_bindings := lt.bindings, (body := lt.body, (new_env := set_t_c(hydra.schemas.extend_graph_for_let(for_binding, get_t_c(g_env), lt), g_env), analyze_function_term_no_infer_with_gather(for_binding, get_t_c, set_t_c, False, new_env, tparams, args, hydra.lib.lists.concat2(bindings, new_bindings), doms, tapps, body))[1])[1])[1]
            
            case hydra.core.TermTypeApplication(value=ta):
                return (ta_body := ta.body, (typ := ta.type, analyze_function_term_no_infer_with_gather(for_binding, get_t_c, set_t_c, arg_mode, g_env, tparams, args, bindings, doms, hydra.lib.lists.cons(typ, tapps), ta_body))[1])[1]
            
            case hydra.core.TermTypeLambda(value=tl):
                return (tvar := tl.parameter, (tl_body := tl.body, (new_env := set_t_c(hydra.schemas.extend_graph_for_type_lambda(get_t_c(g_env), tl), g_env), analyze_function_term_no_infer_with_gather(for_binding, get_t_c, set_t_c, arg_mode, new_env, hydra.lib.lists.cons(tvar, tparams), args, bindings, doms, tapps, tl_body))[1])[1])[1]
            
            case _:
                return analyze_function_term_no_infer_with_finish(g_env, tparams, args, bindings, doms, tapps, t)

def analyze_function_term_no_infer_with(for_binding: Callable[[hydra.graph.Graph, hydra.core.Binding], Maybe[hydra.core.Term]], get_t_c: Callable[[T0], hydra.graph.Graph], set_t_c: Callable[[hydra.graph.Graph, T0], T0], env: T0, term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.typing.FunctionStructure[T0]]:
    r"""Analyze a function term without type inference, with configurable binding metadata."""
    
    return analyze_function_term_no_infer_with_gather(for_binding, get_t_c, set_t_c, True, env, (), (), (), (), (), term)

def analyze_function_term_no_infer(get_t_c: Callable[[T0], hydra.graph.Graph], set_t_c: Callable[[hydra.graph.Graph, T0], T0], env: T0, term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.typing.FunctionStructure[T0]]:
    r"""Analyze a function term without type inference (performance optimization)."""
    
    return analyze_function_term_no_infer_with((lambda x1, x2: binding_metadata(x1, x2)), get_t_c, set_t_c, env, term)

def comments_from_element(b: hydra.core.Binding) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[str]]:
    r"""Extract comments/description from a Binding."""
    
    return hydra.annotations.get_term_description(b.term)

def comments_from_field_type(ft: hydra.core.FieldType) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[str]]:
    r"""Extract comments/description from a FieldType."""
    
    return hydra.annotations.get_type_description(ft.type)

def gather_applications(term: hydra.core.Term) -> tuple[frozenlist[hydra.core.Term], hydra.core.Term]:
    r"""Gather applications from a term, returning (args, baseTerm)."""
    
    def go(args: frozenlist[hydra.core.Term], t: hydra.core.Term) -> tuple[frozenlist[hydra.core.Term], hydra.core.Term]:
        while True:
            match hydra.rewriting.deannotate_term(t):
                case hydra.core.TermApplication(value=app):
                    return (lhs := app.function, (rhs := app.argument, go(hydra.lib.lists.cons(rhs, args), lhs))[1])[1]
                
                case _:
                    return (args, t)
    return go((), term)

def gather_args(term: hydra.core.Term, args: frozenlist[hydra.core.Term]) -> tuple[hydra.core.Term, frozenlist[hydra.core.Term]]:
    r"""Gather term arguments, stripping type-level constructs."""
    
    while True:
        match hydra.rewriting.deannotate_term(term):
            case hydra.core.TermApplication(value=app):
                return (lhs := app.function, (rhs := app.argument, gather_args(lhs, hydra.lib.lists.cons(rhs, args)))[1])[1]
            
            case hydra.core.TermTypeLambda(value=tl):
                return (body := tl.body, gather_args(body, args))[1]
            
            case hydra.core.TermTypeApplication(value=ta):
                return (body := ta.body, gather_args(body, args))[1]
            
            case _:
                return (term, args)

def gather_args_with_type_apps(term: hydra.core.Term, args: frozenlist[hydra.core.Term], ty_args: frozenlist[hydra.core.Type]) -> tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Term], frozenlist[hydra.core.Type]]]:
    r"""Gather term and type arguments from a term."""
    
    while True:
        match hydra.rewriting.deannotate_term(term):
            case hydra.core.TermApplication(value=app):
                return (lhs := app.function, (rhs := app.argument, gather_args_with_type_apps(lhs, hydra.lib.lists.cons(rhs, args), ty_args))[1])[1]
            
            case hydra.core.TermTypeLambda(value=tl):
                return (body := tl.body, gather_args_with_type_apps(body, args, ty_args))[1]
            
            case hydra.core.TermTypeApplication(value=ta):
                return (body := ta.body, (typ := ta.type, gather_args_with_type_apps(body, args, hydra.lib.lists.cons(typ, ty_args)))[1])[1]
            
            case _:
                return (term, (args, ty_args))

def in_coder_graph_context(get_graph: Callable[[T0], T1], get_meta: Callable[[T0], T2], make_coder: Callable[[T1, T2], T0], graph_flow: hydra.compute.Flow[T1, T3]) -> hydra.compute.Flow[T0, T3]:
    r"""Run a Flow Graph computation within a Flow state computation."""
    
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda st: hydra.lib.flows.bind(hydra.monads.with_state(get_graph(st), hydra.lib.flows.bind(graph_flow, (lambda ret: hydra.lib.flows.bind(hydra.monads.get_state(), (lambda g2: hydra.lib.flows.pure((ret, g2))))))), (lambda result: hydra.lib.flows.bind(hydra.monads.put_state(make_coder(hydra.lib.pairs.second(result), get_meta(st))), (lambda _: hydra.lib.flows.pure(hydra.lib.pairs.first(result))))))))

def is_tail_recursive_in_tail_position(func_name: hydra.core.Name, term: hydra.core.Term):
    r"""Check that all self-references are in tail position."""
    
    @lru_cache(1)
    def stripped() -> hydra.core.Term:
        return hydra.rewriting.deannotate_and_detype_term(term)
    def _hoist_body_1(v1):
        match v1:
            case hydra.core.FunctionLambda(value=lam):
                return is_tail_recursive_in_tail_position(func_name, lam.body)
            
            case _:
                return hydra.rewriting.is_free_variable_in_term(func_name, term)
    match stripped():
        case hydra.core.TermApplication():
            @lru_cache(1)
            def gathered() -> tuple[frozenlist[hydra.core.Term], hydra.core.Term]:
                return gather_applications(stripped())
            @lru_cache(1)
            def gather_args() -> frozenlist[hydra.core.Term]:
                return hydra.lib.pairs.first(gathered())
            @lru_cache(1)
            def gather_fun() -> hydra.core.Term:
                return hydra.lib.pairs.second(gathered())
            @lru_cache(1)
            def stripped_fun() -> hydra.core.Term:
                return hydra.rewriting.deannotate_and_detype_term(gather_fun())
            def _hoist_body_1(v1):
                match v1:
                    case hydra.core.EliminationUnion(value=cs):
                        cases_ = cs.cases
                        dflt = cs.default
                        @lru_cache(1)
                        def branches_ok() -> bool:
                            return hydra.lib.lists.foldl((lambda ok, field: hydra.lib.logic.and_(ok, is_tail_recursive_in_tail_position(func_name, field.term))), True, cases_)
                        @lru_cache(1)
                        def dflt_ok() -> bool:
                            return hydra.lib.maybes.maybe(True, (lambda d: is_tail_recursive_in_tail_position(func_name, d)), dflt)
                        @lru_cache(1)
                        def args_ok() -> bool:
                            return hydra.lib.lists.foldl((lambda ok, arg: hydra.lib.logic.and_(ok, hydra.rewriting.is_free_variable_in_term(func_name, arg))), True, gather_args())
                        return hydra.lib.logic.and_(hydra.lib.logic.and_(branches_ok(), dflt_ok()), args_ok())
                    
                    case _:
                        return hydra.rewriting.is_free_variable_in_term(func_name, term)
            def _hoist_body_2(v1):
                match v1:
                    case hydra.core.FunctionElimination(value=e):
                        return _hoist_body_1(e)
                    
                    case _:
                        return hydra.rewriting.is_free_variable_in_term(func_name, term)
            def _hoist_body_3(v1):
                match v1:
                    case hydra.core.TermVariable(value=vname):
                        return hydra.lib.logic.if_else(hydra.lib.equality.equal(vname, func_name), (lambda : (args_no_func := hydra.lib.lists.foldl((lambda ok, arg: hydra.lib.logic.and_(ok, hydra.rewriting.is_free_variable_in_term(func_name, arg))), True, gather_args()), (args_no_lambda := (_hoist_args_no_lambda_1 := (lambda v12: (lambda lam: (ignore := lam.body, True)[1])(v12.value) if isinstance(v12, hydra.core.FunctionLambda) else False), _hoist_args_no_lambda_2 := (lambda v12: (lambda f2: _hoist_args_no_lambda_1(f2))(v12.value) if isinstance(v12, hydra.core.TermFunction) else False), hydra.lib.lists.foldl((lambda ok, arg: hydra.lib.logic.and_(ok, hydra.lib.logic.not_(hydra.rewriting.fold_over_term(hydra.coders.TraversalOrder.PRE, (lambda found, t: hydra.lib.logic.or_(found, _hoist_args_no_lambda_2(t))), False, arg)))), True, gather_args()))[2], hydra.lib.logic.and_(args_no_func, args_no_lambda))[1])[1]), (lambda : hydra.rewriting.is_free_variable_in_term(func_name, term)))
                    
                    case hydra.core.TermFunction(value=f):
                        return _hoist_body_2(f)
                    
                    case _:
                        return hydra.rewriting.is_free_variable_in_term(func_name, term)
            return _hoist_body_3(stripped_fun())
        
        case hydra.core.TermFunction(value=f):
            return _hoist_body_1(f)
        
        case hydra.core.TermLet(value=lt):
            @lru_cache(1)
            def bindings_ok() -> bool:
                return hydra.lib.lists.foldl((lambda ok, b: hydra.lib.logic.and_(ok, hydra.rewriting.is_free_variable_in_term(func_name, b.term))), True, lt.bindings)
            return hydra.lib.logic.and_(bindings_ok(), is_tail_recursive_in_tail_position(func_name, lt.body))
        
        case _:
            return hydra.rewriting.is_free_variable_in_term(func_name, term)

def is_self_tail_recursive(func_name: hydra.core.Name, body: hydra.core.Term) -> bool:
    r"""Check if a term body is self-tail-recursive with respect to a function name."""
    
    @lru_cache(1)
    def calls_self() -> bool:
        return hydra.lib.logic.not_(hydra.rewriting.is_free_variable_in_term(func_name, body))
    return hydra.lib.logic.if_else(calls_self(), (lambda : is_tail_recursive_in_tail_position(func_name, body)), (lambda : False))

def is_simple_assignment(term: hydra.core.Term):
    while True:
        def _hoist_hydra_coder_utils_is_simple_assignment_1(v1):
            match v1:
                case hydra.core.FunctionLambda():
                    return False
                
                case _:
                    return True
        match term:
            case hydra.core.TermAnnotated(value=at):
                term = at.body
                continue
            
            case hydra.core.TermFunction(value=f):
                return _hoist_hydra_coder_utils_is_simple_assignment_1(f)
            
            case hydra.core.TermLet():
                return False
            
            case hydra.core.TermTypeLambda():
                return False
            
            case hydra.core.TermTypeApplication(value=ta):
                term = ta.body
                continue
            
            case _:
                return (base_term := hydra.lib.pairs.first(gather_args(term, ())), (_hoist_body_1 := (lambda v1: (lambda _: False)(v1.value) if isinstance(v1, hydra.core.EliminationUnion) else True), _hoist_body_2 := (lambda v1: (lambda elim: _hoist_body_1(elim))(v1.value) if isinstance(v1, hydra.core.FunctionElimination) else True), _hoist_body_3 := (lambda v1: (lambda f: _hoist_body_2(f))(v1.value) if isinstance(v1, hydra.core.TermFunction) else True), _hoist_body_3(base_term))[3])[1]

def is_trivial_term(t: hydra.core.Term):
    r"""Check if a term is trivially cheap (no thunking needed)."""
    
    match hydra.rewriting.deannotate_term(t):
        case hydra.core.TermLiteral():
            return True
        
        case hydra.core.TermVariable():
            return True
        
        case hydra.core.TermUnit():
            return True
        
        case hydra.core.TermApplication(value=app):
            fun = app.function
            arg = app.argument
            def _hoist_body_1(v1):
                match v1:
                    case hydra.core.EliminationRecord():
                        return is_trivial_term(arg)
                    
                    case _:
                        return False
            def _hoist_body_2(v1):
                match v1:
                    case hydra.core.FunctionElimination(value=e):
                        return _hoist_body_1(e)
                    
                    case _:
                        return False
            def _hoist_body_3(v1):
                match v1:
                    case hydra.core.TermFunction(value=f):
                        return _hoist_body_2(f)
                    
                    case _:
                        return False
            return _hoist_body_3(fun)
        
        case hydra.core.TermMaybe(value=opt):
            return hydra.lib.maybes.maybe(True, (lambda inner: is_trivial_term(inner)), opt)
        
        case hydra.core.TermTypeApplication(value=ta):
            return is_trivial_term(ta.body)
        
        case hydra.core.TermTypeLambda(value=tl):
            return is_trivial_term(tl.body)
        
        case _:
            return False

def normalize_comment(s: str) -> str:
    r"""Normalize a comment string for consistent output across coders."""
    
    @lru_cache(1)
    def stripped() -> str:
        return hydra.formatting.strip_leading_and_trailing_whitespace(s)
    return hydra.lib.logic.if_else(hydra.lib.strings.null(stripped()), (lambda : ""), (lambda : (last_idx := hydra.lib.math.sub(hydra.lib.strings.length(stripped()), 1), (last_char := hydra.lib.strings.char_at(last_idx, stripped()), hydra.lib.logic.if_else(hydra.lib.equality.equal(last_char, 46), (lambda : stripped()), (lambda : hydra.lib.strings.cat2(stripped(), "."))))[1])[1]))

def update_coder_metadata(get_meta: Callable[[T0], T1], make_coder: Callable[[T2, T3], T0], get_graph: Callable[[T0], T2], f: Callable[[T1], T3]) -> hydra.compute.Flow[T0, None]:
    r"""Update the metadata portion of a coder state."""
    
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda st: hydra.monads.put_state(make_coder(get_graph(st), f(get_meta(st))))))

def with_updated_coder_graph(get_graph: Callable[[T0], T1], get_meta: Callable[[T0], T2], make_coder: Callable[[T1, T2], T0], f: Callable[[T1], T1], flow: hydra.compute.Flow[T0, T3]) -> hydra.compute.Flow[T0, T3]:
    r"""Temporarily update the graph for a computation, then restore it."""
    
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda st: hydra.lib.flows.bind(hydra.monads.put_state(make_coder(f(get_graph(st)), get_meta(st))), (lambda _: hydra.lib.flows.bind(flow, (lambda r: hydra.lib.flows.bind(hydra.monads.get_state(), (lambda st2: hydra.lib.flows.bind(hydra.monads.put_state(make_coder(get_graph(st), get_meta(st2))), (lambda _2: hydra.lib.flows.pure(r)))))))))))

def with_graph_bindings(get_graph: Callable[[T0], hydra.graph.Graph], make_coder: Callable[[hydra.graph.Graph, T1], T0], get_meta: Callable[[T0], T1], bindings: frozenlist[hydra.core.Binding], flow: hydra.compute.Flow[T0, T2]) -> hydra.compute.Flow[T0, T2]:
    r"""Temporarily extend the graph with additional bindings for a computation."""
    
    return with_updated_coder_graph(get_graph, get_meta, make_coder, (lambda v1: hydra.lexical.extend_graph_with_bindings(bindings, v1)), flow)
