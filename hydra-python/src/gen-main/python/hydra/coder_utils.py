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
import hydra.core
import hydra.formatting
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

def try_type_of(msg: str, tc: hydra.typing.TypeContext, term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Type]:
    r"""Infer the type of a term with tracing."""
    
    return hydra.monads.with_trace(msg, hydra.checking.type_of(tc, (), term))

def analyze_function_term_with_finish(get_t_c: Callable[[T0], hydra.typing.TypeContext], f_env: T0, tparams: frozenlist[hydra.core.Name], args: frozenlist[hydra.core.Name], bindings: frozenlist[hydra.core.Binding], doms: frozenlist[hydra.core.Type], tapps: frozenlist[hydra.core.Type], body: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.typing.FunctionStructure[T0]]:
    @lru_cache(1)
    def body_with_tapps() -> hydra.core.Term:
        return hydra.lib.lists.foldl((lambda trm, typ: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(trm, typ)))), body, tapps)
    return hydra.lib.flows.bind(hydra.lib.flows.with_default(Nothing(), hydra.lib.flows.map((lambda x1: hydra.lib.maybes.pure(x1)), try_type_of("analyzeFunctionTermWith", get_t_c(f_env), body_with_tapps()))), (lambda mcod: hydra.lib.flows.pure(hydra.typing.FunctionStructure(hydra.lib.lists.reverse(tparams), hydra.lib.lists.reverse(args), bindings, body_with_tapps(), hydra.lib.lists.reverse(doms), mcod, f_env))))

def analyze_function_term_with_gather(for_binding: Callable[[hydra.typing.TypeContext, hydra.core.Binding], Maybe[hydra.core.Term]], get_t_c: Callable[[T0], hydra.typing.TypeContext], set_t_c: Callable[[hydra.typing.TypeContext, T0], T0], arg_mode: bool, g_env: T0, tparams: frozenlist[hydra.core.Name], args: frozenlist[hydra.core.Name], bindings: frozenlist[hydra.core.Binding], doms: frozenlist[hydra.core.Type], tapps: frozenlist[hydra.core.Type], t: hydra.core.Term):
    def _hoist_hydra_coder_utils_analyze_function_term_with_gather_1(arg_mode, args, bindings, doms, for_binding, g_env, get_t_c, set_t_c, t, tapps, tparams, v1):
        match v1:
            case hydra.core.FunctionLambda(value=lam):
                return hydra.lib.logic.if_else(arg_mode, (lambda : (v := lam.parameter, (dom := hydra.lib.maybes.maybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("_"))), (lambda x_: x_), lam.domain), (body := lam.body, (new_env := set_t_c(hydra.schemas.extend_type_context_for_lambda(get_t_c(g_env), lam), g_env), analyze_function_term_with_gather(for_binding, get_t_c, set_t_c, arg_mode, new_env, tparams, hydra.lib.lists.cons(v, args), bindings, hydra.lib.lists.cons(dom, doms), tapps, body))[1])[1])[1])[1]), (lambda : analyze_function_term_with_finish(get_t_c, g_env, tparams, args, bindings, doms, tapps, t)))
            
            case _:
                return analyze_function_term_with_finish(get_t_c, g_env, tparams, args, bindings, doms, tapps, t)
    match hydra.rewriting.deannotate_term(t):
        case hydra.core.TermFunction(value=f):
            return _hoist_hydra_coder_utils_analyze_function_term_with_gather_1(arg_mode, args, bindings, doms, for_binding, g_env, get_t_c, set_t_c, t, tapps, tparams, f)
        
        case hydra.core.TermLet(value=lt):
            @lru_cache(1)
            def new_bindings() -> frozenlist[hydra.core.Binding]:
                return lt.bindings
            @lru_cache(1)
            def body() -> hydra.core.Term:
                return lt.body
            @lru_cache(1)
            def new_env() -> T0:
                return set_t_c(hydra.schemas.extend_type_context_for_let(for_binding, get_t_c(g_env), lt), g_env)
            return analyze_function_term_with_gather(for_binding, get_t_c, set_t_c, False, new_env(), tparams, args, hydra.lib.lists.concat2(bindings, new_bindings()), doms, tapps, body())
        
        case hydra.core.TermTypeApplication(value=ta):
            @lru_cache(1)
            def ta_body() -> hydra.core.Term:
                return ta.body
            @lru_cache(1)
            def typ() -> hydra.core.Type:
                return ta.type
            return analyze_function_term_with_gather(for_binding, get_t_c, set_t_c, arg_mode, g_env, tparams, args, bindings, doms, hydra.lib.lists.cons(typ(), tapps), ta_body())
        
        case hydra.core.TermTypeLambda(value=tl):
            @lru_cache(1)
            def tvar() -> hydra.core.Name:
                return tl.parameter
            @lru_cache(1)
            def tl_body() -> hydra.core.Term:
                return tl.body
            @lru_cache(1)
            def new_env() -> T0:
                return set_t_c(hydra.schemas.extend_type_context_for_type_lambda(get_t_c(g_env), tl), g_env)
            return analyze_function_term_with_gather(for_binding, get_t_c, set_t_c, arg_mode, new_env(), hydra.lib.lists.cons(tvar(), tparams), args, bindings, doms, tapps, tl_body())
        
        case _:
            return analyze_function_term_with_finish(get_t_c, g_env, tparams, args, bindings, doms, tapps, t)

def analyze_function_term_with(for_binding: Callable[[hydra.typing.TypeContext, hydra.core.Binding], Maybe[hydra.core.Term]], get_t_c: Callable[[T0], hydra.typing.TypeContext], set_t_c: Callable[[hydra.typing.TypeContext, T0], T0], env: T0, term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.typing.FunctionStructure[T0]]:
    r"""Analyze a function term with configurable binding metadata."""
    
    return analyze_function_term_with_gather(for_binding, get_t_c, set_t_c, True, env, (), (), (), (), (), term)

def is_complex_variable(tc: hydra.typing.TypeContext, name: hydra.core.Name) -> bool:
    r"""Check if a variable is bound to a complex term."""
    
    @lru_cache(1)
    def meta_lookup() -> Maybe[hydra.core.Term]:
        return hydra.lib.maps.lookup(name, tc.metadata)
    return hydra.lib.logic.if_else(hydra.lib.maybes.is_just(meta_lookup()), (lambda : True), (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(name, tc.lambda_variables), (lambda : True), (lambda : (type_lookup := hydra.lib.maps.lookup(name, tc.types), hydra.lib.logic.not_(hydra.lib.maybes.is_just(type_lookup)))[1]))))

def is_complex_term(tc: hydra.typing.TypeContext, t: hydra.core.Term) -> bool:
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

def is_complex_binding(tc: hydra.typing.TypeContext, b: hydra.core.Binding) -> bool:
    r"""Check if a binding needs to be treated as a function."""
    
    @lru_cache(1)
    def term() -> hydra.core.Term:
        return b.term
    @lru_cache(1)
    def mts() -> Maybe[hydra.core.TypeScheme]:
        return b.type
    return hydra.lib.maybes.cases(mts(), is_complex_term(tc, term()), (lambda ts: (is_polymorphic := hydra.lib.logic.not_(hydra.lib.lists.null(ts.variables)), is_non_nullary := hydra.lib.equality.gt(hydra.arity.type_arity(ts.type), 0), is_complex := is_complex_term(tc, term()), hydra.lib.logic.or_(hydra.lib.logic.or_(is_polymorphic, is_non_nullary), is_complex))[3]))

def binding_metadata(tc: hydra.typing.TypeContext, b: hydra.core.Binding) -> Maybe[hydra.core.Term]:
    r"""Produces metadata for a binding if it is complex."""
    
    return hydra.lib.logic.if_else(is_complex_binding(tc, b), (lambda : Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True)))))), (lambda : Nothing()))

def analyze_function_term(get_t_c: Callable[[T0], hydra.typing.TypeContext], set_t_c: Callable[[hydra.typing.TypeContext, T0], T0], env: T0, term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.typing.FunctionStructure[T0]]:
    r"""Analyze a function term, collecting lambdas, type lambdas, lets, and type applications."""
    
    return analyze_function_term_with((lambda x1, x2: binding_metadata(x1, x2)), get_t_c, set_t_c, env, term)

def analyze_function_term_inline(get_t_c: Callable[[T0], hydra.typing.TypeContext], set_t_c: Callable[[hydra.typing.TypeContext, T0], T0], env: T0, term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.typing.FunctionStructure[T0]]:
    r"""Analyze a function term without recording binding metadata."""
    
    return analyze_function_term_with((lambda _, _2: Nothing()), get_t_c, set_t_c, env, term)

def analyze_function_term_no_infer_with_finish(f_env: T0, tparams: frozenlist[hydra.core.Name], args: frozenlist[hydra.core.Name], bindings: frozenlist[hydra.core.Binding], doms: frozenlist[hydra.core.Type], tapps: frozenlist[hydra.core.Type], body: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.typing.FunctionStructure[T0]]:
    @lru_cache(1)
    def body_with_tapps() -> hydra.core.Term:
        return hydra.lib.lists.foldl((lambda trm, typ: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(trm, typ)))), body, tapps)
    return hydra.lib.flows.pure(hydra.typing.FunctionStructure(hydra.lib.lists.reverse(tparams), hydra.lib.lists.reverse(args), bindings, body_with_tapps(), hydra.lib.lists.reverse(doms), Nothing(), f_env))

def analyze_function_term_no_infer_with_gather(for_binding: Callable[[hydra.typing.TypeContext, hydra.core.Binding], Maybe[hydra.core.Term]], get_t_c: Callable[[T0], hydra.typing.TypeContext], set_t_c: Callable[[hydra.typing.TypeContext, T0], T0], arg_mode: bool, g_env: T0, tparams: frozenlist[hydra.core.Name], args: frozenlist[hydra.core.Name], bindings: frozenlist[hydra.core.Binding], doms: frozenlist[hydra.core.Type], tapps: frozenlist[hydra.core.Type], t: hydra.core.Term):
    def _hoist_hydra_coder_utils_analyze_function_term_no_infer_with_gather_1(arg_mode, args, bindings, doms, for_binding, g_env, get_t_c, set_t_c, t, tapps, tparams, v1):
        match v1:
            case hydra.core.FunctionLambda(value=lam):
                return hydra.lib.logic.if_else(arg_mode, (lambda : (v := lam.parameter, (dom := hydra.lib.maybes.maybe(cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("_"))), (lambda x_: x_), lam.domain), (body := lam.body, (new_env := set_t_c(hydra.schemas.extend_type_context_for_lambda(get_t_c(g_env), lam), g_env), analyze_function_term_no_infer_with_gather(for_binding, get_t_c, set_t_c, arg_mode, new_env, tparams, hydra.lib.lists.cons(v, args), bindings, hydra.lib.lists.cons(dom, doms), tapps, body))[1])[1])[1])[1]), (lambda : analyze_function_term_no_infer_with_finish(g_env, tparams, args, bindings, doms, tapps, t)))
            
            case _:
                return analyze_function_term_no_infer_with_finish(g_env, tparams, args, bindings, doms, tapps, t)
    match hydra.rewriting.deannotate_term(t):
        case hydra.core.TermFunction(value=f):
            return _hoist_hydra_coder_utils_analyze_function_term_no_infer_with_gather_1(arg_mode, args, bindings, doms, for_binding, g_env, get_t_c, set_t_c, t, tapps, tparams, f)
        
        case hydra.core.TermLet(value=lt):
            @lru_cache(1)
            def new_bindings() -> frozenlist[hydra.core.Binding]:
                return lt.bindings
            @lru_cache(1)
            def body() -> hydra.core.Term:
                return lt.body
            @lru_cache(1)
            def new_env() -> T0:
                return set_t_c(hydra.schemas.extend_type_context_for_let(for_binding, get_t_c(g_env), lt), g_env)
            return analyze_function_term_no_infer_with_gather(for_binding, get_t_c, set_t_c, False, new_env(), tparams, args, hydra.lib.lists.concat2(bindings, new_bindings()), doms, tapps, body())
        
        case hydra.core.TermTypeApplication(value=ta):
            @lru_cache(1)
            def ta_body() -> hydra.core.Term:
                return ta.body
            @lru_cache(1)
            def typ() -> hydra.core.Type:
                return ta.type
            return analyze_function_term_no_infer_with_gather(for_binding, get_t_c, set_t_c, arg_mode, g_env, tparams, args, bindings, doms, hydra.lib.lists.cons(typ(), tapps), ta_body())
        
        case hydra.core.TermTypeLambda(value=tl):
            @lru_cache(1)
            def tvar() -> hydra.core.Name:
                return tl.parameter
            @lru_cache(1)
            def tl_body() -> hydra.core.Term:
                return tl.body
            @lru_cache(1)
            def new_env() -> T0:
                return set_t_c(hydra.schemas.extend_type_context_for_type_lambda(get_t_c(g_env), tl), g_env)
            return analyze_function_term_no_infer_with_gather(for_binding, get_t_c, set_t_c, arg_mode, new_env(), hydra.lib.lists.cons(tvar(), tparams), args, bindings, doms, tapps, tl_body())
        
        case _:
            return analyze_function_term_no_infer_with_finish(g_env, tparams, args, bindings, doms, tapps, t)

def analyze_function_term_no_infer_with(for_binding: Callable[[hydra.typing.TypeContext, hydra.core.Binding], Maybe[hydra.core.Term]], get_t_c: Callable[[T0], hydra.typing.TypeContext], set_t_c: Callable[[hydra.typing.TypeContext, T0], T0], env: T0, term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.typing.FunctionStructure[T0]]:
    r"""Analyze a function term without type inference, with configurable binding metadata."""
    
    return analyze_function_term_no_infer_with_gather(for_binding, get_t_c, set_t_c, True, env, (), (), (), (), (), term)

def analyze_function_term_no_infer(get_t_c: Callable[[T0], hydra.typing.TypeContext], set_t_c: Callable[[hydra.typing.TypeContext, T0], T0], env: T0, term: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.typing.FunctionStructure[T0]]:
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
        match hydra.rewriting.deannotate_term(t):
            case hydra.core.TermApplication(value=app):
                @lru_cache(1)
                def lhs() -> hydra.core.Term:
                    return app.function
                @lru_cache(1)
                def rhs() -> hydra.core.Term:
                    return app.argument
                return go(hydra.lib.lists.cons(rhs(), args), lhs())
            
            case _:
                return (args, t)
    return go((), term)

def gather_args(term: hydra.core.Term, args: frozenlist[hydra.core.Term]) -> tuple[hydra.core.Term, frozenlist[hydra.core.Term]]:
    r"""Gather term arguments, stripping type-level constructs."""
    
    match hydra.rewriting.deannotate_term(term):
        case hydra.core.TermApplication(value=app):
            @lru_cache(1)
            def lhs() -> hydra.core.Term:
                return app.function
            @lru_cache(1)
            def rhs() -> hydra.core.Term:
                return app.argument
            return gather_args(lhs(), hydra.lib.lists.cons(rhs(), args))
        
        case hydra.core.TermTypeLambda(value=tl):
            @lru_cache(1)
            def body() -> hydra.core.Term:
                return tl.body
            return gather_args(body(), args)
        
        case hydra.core.TermTypeApplication(value=ta):
            @lru_cache(1)
            def body() -> hydra.core.Term:
                return ta.body
            return gather_args(body(), args)
        
        case _:
            return (term, args)

def gather_args_with_type_apps(term: hydra.core.Term, args: frozenlist[hydra.core.Term], ty_args: frozenlist[hydra.core.Type]) -> tuple[hydra.core.Term, tuple[frozenlist[hydra.core.Term], frozenlist[hydra.core.Type]]]:
    r"""Gather term and type arguments from a term."""
    
    match hydra.rewriting.deannotate_term(term):
        case hydra.core.TermApplication(value=app):
            @lru_cache(1)
            def lhs() -> hydra.core.Term:
                return app.function
            @lru_cache(1)
            def rhs() -> hydra.core.Term:
                return app.argument
            return gather_args_with_type_apps(lhs(), hydra.lib.lists.cons(rhs(), args), ty_args)
        
        case hydra.core.TermTypeLambda(value=tl):
            @lru_cache(1)
            def body() -> hydra.core.Term:
                return tl.body
            return gather_args_with_type_apps(body(), args, ty_args)
        
        case hydra.core.TermTypeApplication(value=ta):
            @lru_cache(1)
            def body() -> hydra.core.Term:
                return ta.body
            @lru_cache(1)
            def typ() -> hydra.core.Type:
                return ta.type
            return gather_args_with_type_apps(body(), args, hydra.lib.lists.cons(typ(), ty_args))
        
        case _:
            return (term, (args, ty_args))

def in_coder_graph_context(get_graph: Callable[[T0], T1], get_meta: Callable[[T0], T2], make_coder: Callable[[T1, T2], T0], graph_flow: hydra.compute.Flow[T1, T3]) -> hydra.compute.Flow[T0, T3]:
    r"""Run a Flow Graph computation within a Flow state computation."""
    
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda st: hydra.lib.flows.bind(hydra.monads.with_state(get_graph(st), hydra.lib.flows.bind(graph_flow, (lambda ret: hydra.lib.flows.bind(hydra.monads.get_state(), (lambda g2: hydra.lib.flows.pure((ret, g2))))))), (lambda result: hydra.lib.flows.bind(hydra.monads.put_state(make_coder(hydra.lib.pairs.second(result), get_meta(st))), (lambda _: hydra.lib.flows.pure(hydra.lib.pairs.first(result))))))))

def is_simple_assignment(term: hydra.core.Term):
    def _hoist_hydra_coder_utils_is_simple_assignment_1(v1):
        match v1:
            case hydra.core.FunctionLambda():
                return False
            
            case _:
                return True
    match term:
        case hydra.core.TermAnnotated(value=at):
            return is_simple_assignment(at.body)
        
        case hydra.core.TermFunction(value=f):
            return _hoist_hydra_coder_utils_is_simple_assignment_1(f)
        
        case hydra.core.TermLet():
            return False
        
        case hydra.core.TermTypeLambda():
            return False
        
        case hydra.core.TermTypeApplication(value=ta):
            return is_simple_assignment(ta.body)
        
        case _:
            base_term = hydra.lib.pairs.first(gather_args(term, ()))
            match base_term:
                case hydra.core.TermFunction(value=v1):
                    match v1:
                        case hydra.core.FunctionElimination(value=v2):
                            match v2:
                                case hydra.core.EliminationUnion():
                                    return False
                                case _:
                                    return True
                        case _:
                            return True
                case _:
                    return True

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
