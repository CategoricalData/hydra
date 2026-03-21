# Note: this is an automatically generated file. Do not edit.

r"""Common utilities for language coders, providing shared patterns for term decomposition and analysis."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Just, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.arity
import hydra.checking
import hydra.coders
import hydra.core
import hydra.formatting
import hydra.graph
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
import hydra.module
import hydra.names
import hydra.rewriting
import hydra.typing
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def type_of_term(cx: hydra.context.Context, g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type]:
    r"""Check the type of a term."""

    return hydra.lib.eithers.map((lambda x1: hydra.lib.pairs.first(x1)), hydra.checking.type_of(cx, g, (), term))

def analyze_function_term_with_finish(cx: hydra.context.Context, get_t_c: Callable[[T0], hydra.graph.Graph], f_env: T0, tparams: frozenlist[hydra.core.Name], args: frozenlist[hydra.core.Name], bindings: frozenlist[hydra.core.Binding], doms: frozenlist[hydra.core.Type], tapps: frozenlist[hydra.core.Type], body: hydra.core.Term) -> Either[T1, hydra.typing.FunctionStructure[T0]]:
    @lru_cache(1)
    def body_with_tapps() -> hydra.core.Term:
        return hydra.lib.lists.foldl((lambda trm, typ: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(trm, typ)))), body, tapps)
    @lru_cache(1)
    def mcod() -> Maybe[hydra.core.Type]:
        return hydra.lib.eithers.either((lambda _: Nothing()), (lambda c: Just(c)), type_of_term(cx, get_t_c(f_env), body_with_tapps()))
    return Right(hydra.typing.FunctionStructure(hydra.lib.lists.reverse(tparams), hydra.lib.lists.reverse(args), bindings, body_with_tapps(), hydra.lib.lists.reverse(doms), mcod(), f_env))

def analyze_function_term_with_gather(cx: hydra.context.Context, for_binding: Callable[[hydra.graph.Graph, hydra.core.Binding], Maybe[hydra.core.Term]], get_t_c: Callable[[T0], hydra.graph.Graph], set_t_c: Callable[[hydra.graph.Graph, T0], T0], arg_mode: bool, g_env: T0, tparams: frozenlist[hydra.core.Name], args: frozenlist[hydra.core.Name], bindings: frozenlist[hydra.core.Binding], doms: frozenlist[hydra.core.Type], tapps: frozenlist[hydra.core.Type], t: hydra.core.Term):
    while True:
        def _hoist_hydra_coder_utils_analyze_function_term_with_gather_1(arg_mode, args, bindings, cx, doms, for_binding, g_env, get_t_c, set_t_c, t, tapps, tparams, v1):
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    return hydra.lib.logic.if_else(arg_mode, (lambda : (v := lam.parameter, (dom := hydra.lib.maybes.maybe((lambda : cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("_")))), (lambda x_: x_), lam.domain), (body := lam.body, (new_env := set_t_c(hydra.rewriting.extend_graph_for_lambda(get_t_c(g_env), lam), g_env), analyze_function_term_with_gather(cx, for_binding, get_t_c, set_t_c, arg_mode, new_env, tparams, hydra.lib.lists.cons(v, args), bindings, hydra.lib.lists.cons(dom, doms), tapps, body))[1])[1])[1])[1]), (lambda : analyze_function_term_with_finish(cx, get_t_c, g_env, tparams, args, bindings, doms, tapps, t)))

                case _:
                    return analyze_function_term_with_finish(cx, get_t_c, g_env, tparams, args, bindings, doms, tapps, t)
        match hydra.rewriting.deannotate_term(t):
            case hydra.core.TermFunction(value=f):
                return _hoist_hydra_coder_utils_analyze_function_term_with_gather_1(arg_mode, args, bindings, cx, doms, for_binding, g_env, get_t_c, set_t_c, t, tapps, tparams, f)

            case hydra.core.TermLet(value=lt):
                return (new_bindings := lt.bindings, (body := lt.body, (new_env := set_t_c(hydra.rewriting.extend_graph_for_let(for_binding, get_t_c(g_env), lt), g_env), analyze_function_term_with_gather(cx, for_binding, get_t_c, set_t_c, False, new_env, tparams, args, hydra.lib.lists.concat2(bindings, new_bindings), doms, tapps, body))[1])[1])[1]

            case hydra.core.TermTypeApplication(value=ta):
                return (ta_body := ta.body, (typ := ta.type, analyze_function_term_with_gather(cx, for_binding, get_t_c, set_t_c, arg_mode, g_env, tparams, args, bindings, doms, hydra.lib.lists.cons(typ, tapps), ta_body))[1])[1]

            case hydra.core.TermTypeLambda(value=tl):
                return (tvar := tl.parameter, (tl_body := tl.body, (new_env := set_t_c(hydra.rewriting.extend_graph_for_type_lambda(get_t_c(g_env), tl), g_env), analyze_function_term_with_gather(cx, for_binding, get_t_c, set_t_c, arg_mode, new_env, hydra.lib.lists.cons(tvar, tparams), args, bindings, doms, tapps, tl_body))[1])[1])[1]

            case _:
                return analyze_function_term_with_finish(cx, get_t_c, g_env, tparams, args, bindings, doms, tapps, t)

def analyze_function_term_with(cx: hydra.context.Context, for_binding: Callable[[hydra.graph.Graph, hydra.core.Binding], Maybe[hydra.core.Term]], get_t_c: Callable[[T0], hydra.graph.Graph], set_t_c: Callable[[hydra.graph.Graph, T0], T0], env: T0, term: hydra.core.Term) -> Either[T1, hydra.typing.FunctionStructure[T0]]:
    r"""Analyze a function term with configurable binding metadata."""

    return analyze_function_term_with_gather(cx, for_binding, get_t_c, set_t_c, True, env, (), (), (), (), (), term)

def is_complex_variable(tc: hydra.graph.Graph, name: hydra.core.Name) -> bool:
    r"""Check if a variable is bound to a complex term."""

    @lru_cache(1)
    def meta_lookup() -> Maybe[hydra.core.Term]:
        return hydra.lib.maps.lookup(name, tc.metadata)
    return hydra.lib.logic.if_else(hydra.lib.maybes.is_just(meta_lookup()), (lambda : True), (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(name, tc.lambda_variables), (lambda : True), (lambda : (type_lookup := hydra.lib.maps.lookup(name, tc.bound_types), hydra.lib.maybes.maybe((lambda : True), (lambda ts: hydra.lib.equality.gt(hydra.arity.type_scheme_arity(ts), 0)), type_lookup))[1]))))

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
    return hydra.lib.maybes.cases(mts, (lambda : is_complex_term(tc, term)), (lambda ts: (is_polymorphic := hydra.lib.logic.not_(hydra.lib.lists.null(ts.variables)), is_non_nullary := hydra.lib.equality.gt(hydra.arity.type_arity(ts.type), 0), is_complex := is_complex_term(tc, term), hydra.lib.logic.or_(hydra.lib.logic.or_(is_polymorphic, is_non_nullary), is_complex))[3]))

def binding_metadata(tc: hydra.graph.Graph, b: hydra.core.Binding) -> Maybe[hydra.core.Term]:
    r"""Produces metadata for a binding if it is complex."""

    return hydra.lib.logic.if_else(is_complex_binding(tc, b), (lambda : Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True)))))), (lambda : Nothing()))

def analyze_function_term(cx: hydra.context.Context, get_t_c: Callable[[T0], hydra.graph.Graph], set_t_c: Callable[[hydra.graph.Graph, T0], T0], env: T0, term: hydra.core.Term) -> Either[T1, hydra.typing.FunctionStructure[T0]]:
    r"""Analyze a function term, collecting lambdas, type lambdas, lets, and type applications."""

    return analyze_function_term_with(cx, (lambda x1, x2: binding_metadata(x1, x2)), get_t_c, set_t_c, env, term)

def comments_from_element(cx: hydra.context.Context, g: hydra.graph.Graph, b: hydra.core.Binding) -> Either[hydra.context.InContext[hydra.errors.Error], Maybe[str]]:
    r"""Extract comments/description from a Binding."""

    return hydra.annotations.get_term_description(cx, g, b.term)

def comments_from_field_type(cx: hydra.context.Context, g: hydra.graph.Graph, ft: hydra.core.FieldType) -> Either[hydra.context.InContext[hydra.errors.Error], Maybe[str]]:
    r"""Extract comments/description from a FieldType."""

    return hydra.annotations.get_type_description(cx, g, ft.type)

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
                            return hydra.lib.maybes.maybe((lambda : True), (lambda d: is_tail_recursive_in_tail_position(func_name, d)), dflt)
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

                    case hydra.core.EliminationWrap():
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
            return hydra.lib.maybes.maybe((lambda : True), (lambda inner: is_trivial_term(inner)), opt)

        case hydra.core.TermRecord(value=rec):
            return hydra.lib.lists.foldl((lambda acc, fld: hydra.lib.logic.and_(acc, is_trivial_term(fld.term))), True, rec.fields)

        case hydra.core.TermWrap(value=wt):
            return is_trivial_term(wt.body)

        case hydra.core.TermTypeApplication(value=ta):
            return is_trivial_term(ta.body)

        case hydra.core.TermTypeLambda(value=tl):
            return is_trivial_term(tl.body)

        case _:
            return False

def name_to_file_path(ns_conv: hydra.util.CaseConvention, local_conv: hydra.util.CaseConvention, ext: hydra.module.FileExtension, name: hydra.core.Name) -> str:
    r"""Convert a name to file path, given case conventions for namespaces and local names, and assuming '/' as the file path separator."""

    @lru_cache(1)
    def qual_name() -> hydra.module.QualifiedName:
        return hydra.names.qualify_name(name)
    ns = qual_name().namespace
    local = qual_name().local
    def ns_to_file_path(ns2: hydra.module.Namespace) -> str:
        return hydra.lib.strings.intercalate("/", hydra.lib.lists.map((lambda part: hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, ns_conv, part)), hydra.lib.strings.split_on(".", ns2.value)))
    @lru_cache(1)
    def prefix() -> str:
        return hydra.lib.maybes.maybe((lambda : ""), (lambda n: hydra.lib.strings.cat2(ns_to_file_path(n), "/")), ns)
    @lru_cache(1)
    def suffix() -> str:
        return hydra.formatting.convert_case(hydra.util.CaseConvention.PASCAL, local_conv, local)
    return hydra.lib.strings.cat((prefix(), suffix(), ".", ext.value))

def normalize_comment(s: str) -> str:
    r"""Normalize a comment string for consistent output across coders."""

    @lru_cache(1)
    def stripped() -> str:
        return hydra.formatting.strip_leading_and_trailing_whitespace(s)
    return hydra.lib.logic.if_else(hydra.lib.strings.null(stripped()), (lambda : ""), (lambda : (last_idx := hydra.lib.math.sub(hydra.lib.strings.length(stripped()), 1), (last_char := hydra.lib.strings.char_at(last_idx, stripped()), hydra.lib.logic.if_else(hydra.lib.equality.equal(last_char, 46), (lambda : stripped()), (lambda : hydra.lib.strings.cat2(stripped(), "."))))[1])[1]))

def union_type_to_record_type(rt: frozenlist[hydra.core.FieldType]) -> frozenlist[hydra.core.FieldType]:
    r"""Convert a union field type list to a record field type list with optional fields."""

    def make_optional(f: hydra.core.FieldType) -> hydra.core.FieldType:
        fn = f.name
        ft = f.type
        return hydra.core.FieldType(fn, hydra.rewriting.map_beneath_type_annotations((lambda x: cast(hydra.core.Type, hydra.core.TypeMaybe(x))), ft))
    return hydra.lib.lists.map((lambda x1: make_optional(x1)), rt)
