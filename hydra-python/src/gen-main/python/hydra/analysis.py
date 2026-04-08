# Note: this is an automatically generated file. Do not edit.

r"""Module dependency namespace analysis."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Just, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.checking
import hydra.coders
import hydra.constants
import hydra.core
import hydra.decode.core
import hydra.dependencies
import hydra.encode.core
import hydra.errors
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.names
import hydra.packaging
import hydra.predicates
import hydra.rewriting
import hydra.scoping
import hydra.strip
import hydra.typing
import hydra.variables

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def add_names_to_namespaces(encode_namespace: Callable[[hydra.packaging.Namespace], T0], names: frozenset[hydra.core.Name], ns0: hydra.packaging.Namespaces[T0]) -> hydra.packaging.Namespaces[T0]:
    r"""Add names to existing namespaces mapping."""

    @lru_cache(1)
    def nss() -> frozenset[hydra.packaging.Namespace]:
        return hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: hydra.names.namespace_of(x1)), hydra.lib.sets.to_list(names))))
    def to_pair(ns: hydra.packaging.Namespace) -> tuple[hydra.packaging.Namespace, T0]:
        return (ns, encode_namespace(ns))
    return hydra.packaging.Namespaces(ns0.focus, hydra.lib.maps.union(ns0.mapping, hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: to_pair(x1)), hydra.lib.sets.to_list(nss())))))

def analyze_function_term_with_finish(cx: hydra.context.Context, get_t_c: Callable[[T0], hydra.graph.Graph], f_env: T0, tparams: frozenlist[hydra.core.Name], args: frozenlist[hydra.core.Name], bindings: frozenlist[hydra.core.Binding], doms: frozenlist[hydra.core.Type], tapps: frozenlist[hydra.core.Type], body: hydra.core.Term) -> Either[T1, hydra.typing.FunctionStructure[T0]]:
    @lru_cache(1)
    def body_with_tapps() -> hydra.core.Term:
        return hydra.lib.lists.foldl((lambda trm, typ: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(trm, typ)))), body, tapps)
    @lru_cache(1)
    def mcod() -> Maybe[hydra.core.Type]:
        return hydra.lib.eithers.either((lambda _: Nothing()), (lambda c: Just(c)), hydra.checking.type_of_term(cx, get_t_c(f_env), body_with_tapps()))
    return Right(hydra.typing.FunctionStructure(hydra.lib.lists.reverse(tparams), hydra.lib.lists.reverse(args), bindings, body_with_tapps(), hydra.lib.lists.reverse(doms), mcod(), f_env))

def analyze_function_term_with_gather(cx: hydra.context.Context, for_binding: Callable[[hydra.graph.Graph, hydra.core.Binding], Maybe[hydra.core.Term]], get_t_c: Callable[[T0], hydra.graph.Graph], set_t_c: Callable[[hydra.graph.Graph, T0], T0], arg_mode: bool, g_env: T0, tparams: frozenlist[hydra.core.Name], args: frozenlist[hydra.core.Name], bindings: frozenlist[hydra.core.Binding], doms: frozenlist[hydra.core.Type], tapps: frozenlist[hydra.core.Type], t: hydra.core.Term):
    while True:
        def _hoist_hydra_analysis_analyze_function_term_with_gather_1(arg_mode, args, bindings, cx, doms, for_binding, g_env, get_t_c, set_t_c, t, tapps, tparams, v1):
            match v1:
                case hydra.core.FunctionLambda(value=lam):
                    return hydra.lib.logic.if_else(arg_mode, (lambda : (v := lam.parameter, (dom := hydra.lib.maybes.maybe((lambda : cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("_")))), (lambda x_: x_), lam.domain), (body := lam.body, (new_env := set_t_c(hydra.scoping.extend_graph_for_lambda(get_t_c(g_env), lam), g_env), analyze_function_term_with_gather(cx, for_binding, get_t_c, set_t_c, arg_mode, new_env, tparams, hydra.lib.lists.cons(v, args), bindings, hydra.lib.lists.cons(dom, doms), tapps, body))[1])[1])[1])[1]), (lambda : analyze_function_term_with_finish(cx, get_t_c, g_env, tparams, args, bindings, doms, tapps, t)))

                case _:
                    return analyze_function_term_with_finish(cx, get_t_c, g_env, tparams, args, bindings, doms, tapps, t)
        match hydra.strip.deannotate_term(t):
            case hydra.core.TermFunction(value=f):
                return _hoist_hydra_analysis_analyze_function_term_with_gather_1(arg_mode, args, bindings, cx, doms, for_binding, g_env, get_t_c, set_t_c, t, tapps, tparams, f)

            case hydra.core.TermLet(value=lt):
                return (new_bindings := lt.bindings, (body := lt.body, (new_env := set_t_c(hydra.scoping.extend_graph_for_let(for_binding, get_t_c(g_env), lt), g_env), analyze_function_term_with_gather(cx, for_binding, get_t_c, set_t_c, False, new_env, tparams, args, hydra.lib.lists.concat2(bindings, new_bindings), doms, tapps, body))[1])[1])[1]

            case hydra.core.TermTypeApplication(value=ta):
                return (ta_body := ta.body, (typ := ta.type, analyze_function_term_with_gather(cx, for_binding, get_t_c, set_t_c, arg_mode, g_env, tparams, args, bindings, doms, hydra.lib.lists.cons(typ, tapps), ta_body))[1])[1]

            case hydra.core.TermTypeLambda(value=tl):
                return (tvar := tl.parameter, (tl_body := tl.body, (new_env := set_t_c(hydra.scoping.extend_graph_for_type_lambda(get_t_c(g_env), tl), g_env), analyze_function_term_with_gather(cx, for_binding, get_t_c, set_t_c, arg_mode, new_env, hydra.lib.lists.cons(tvar, tparams), args, bindings, doms, tapps, tl_body))[1])[1])[1]

            case _:
                return analyze_function_term_with_finish(cx, get_t_c, g_env, tparams, args, bindings, doms, tapps, t)

def analyze_function_term_with(cx: hydra.context.Context, for_binding: Callable[[hydra.graph.Graph, hydra.core.Binding], Maybe[hydra.core.Term]], get_t_c: Callable[[T0], hydra.graph.Graph], set_t_c: Callable[[hydra.graph.Graph, T0], T0], env: T0, term: hydra.core.Term) -> Either[T1, hydra.typing.FunctionStructure[T0]]:
    r"""Analyze a function term with configurable binding metadata."""

    return analyze_function_term_with_gather(cx, for_binding, get_t_c, set_t_c, True, env, (), (), (), (), (), term)

def analyze_function_term(cx: hydra.context.Context, get_t_c: Callable[[T0], hydra.graph.Graph], set_t_c: Callable[[hydra.graph.Graph, T0], T0], env: T0, term: hydra.core.Term) -> Either[T1, hydra.typing.FunctionStructure[T0]]:
    r"""Analyze a function term, collecting lambdas, type lambdas, lets, and type applications."""

    return analyze_function_term_with(cx, (lambda g, b: hydra.lib.logic.if_else(hydra.predicates.is_complex_binding(g, b), (lambda : Just(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True)))))), (lambda : Nothing()))), get_t_c, set_t_c, env, term)

def definition_dependency_namespaces(defs: frozenlist[hydra.packaging.Definition]) -> frozenset[hydra.packaging.Namespace]:
    r"""Get dependency namespaces from definitions."""

    def def_names(def_: hydra.packaging.Definition) -> frozenset[hydra.core.Name]:
        match def_:
            case hydra.packaging.DefinitionType(value=type_def):
                return hydra.dependencies.type_dependency_names(True, type_def.type.type)

            case hydra.packaging.DefinitionTerm(value=term_def):
                return hydra.dependencies.term_dependency_names(True, True, True, term_def.term)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    @lru_cache(1)
    def all_names() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.unions(hydra.lib.lists.map((lambda x1: def_names(x1)), defs))
    return hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: hydra.names.namespace_of(x1)), hydra.lib.sets.to_list(all_names()))))

def dependency_namespaces(cx: T0, graph: hydra.graph.Graph, binds: bool, with_prims: bool, with_noms: bool, with_schema: bool, els: frozenlist[hydra.core.Binding]) -> Either[hydra.errors.Error, frozenset[hydra.packaging.Namespace]]:
    r"""Find dependency namespaces in all of a set of terms (Either version)."""

    def dep_names(el: hydra.core.Binding) -> Either[hydra.errors.Error, frozenset[hydra.core.Name]]:
        term = el.term
        @lru_cache(1)
        def deannotated_term() -> hydra.core.Term:
            return hydra.strip.deannotate_term(term)
        @lru_cache(1)
        def data_names() -> frozenset[hydra.core.Name]:
            return hydra.dependencies.term_dependency_names(binds, with_prims, with_noms, term)
        @lru_cache(1)
        def schema_names() -> frozenset[hydra.core.Name]:
            return hydra.lib.logic.if_else(with_schema, (lambda : hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda ts: hydra.dependencies.type_dependency_names(True, ts.type)), el.type)), (lambda : hydra.lib.sets.empty()))
        return hydra.lib.logic.if_else(hydra.predicates.is_encoded_type(deannotated_term()), (lambda : hydra.lib.eithers.map((lambda typ: hydra.lib.sets.unions((data_names(), schema_names(), hydra.dependencies.type_dependency_names(True, typ)))), hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorDecoding(_e))), (lambda _a: _a), hydra.decode.core.type(graph, term)))), (lambda : hydra.lib.logic.if_else(hydra.predicates.is_encoded_term(deannotated_term()), (lambda : hydra.lib.eithers.map((lambda decoded_term: hydra.lib.sets.unions((data_names(), schema_names(), hydra.dependencies.term_dependency_names(binds, with_prims, with_noms, decoded_term)))), hydra.lib.eithers.bimap((lambda _e: cast(hydra.errors.Error, hydra.errors.ErrorDecoding(_e))), (lambda _a: _a), hydra.decode.core.term(graph, term)))), (lambda : Right(hydra.lib.sets.unions((data_names(), schema_names())))))))
    return hydra.lib.eithers.map((lambda names_list: hydra.lib.sets.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda x1: hydra.names.namespace_of(x1)), hydra.lib.sets.to_list(hydra.lib.sets.unions(names_list)))))), hydra.lib.eithers.map_list((lambda x1: dep_names(x1)), els))

def gather_applications(term: hydra.core.Term) -> tuple[frozenlist[hydra.core.Term], hydra.core.Term]:
    r"""Gather applications from a term, returning (args, baseTerm)."""

    def go(args: frozenlist[hydra.core.Term], t: hydra.core.Term) -> tuple[frozenlist[hydra.core.Term], hydra.core.Term]:
        while True:
            match hydra.strip.deannotate_term(t):
                case hydra.core.TermApplication(value=app):
                    return (lhs := app.function, (rhs := app.argument, go(hydra.lib.lists.cons(rhs, args), lhs))[1])[1]

                case _:
                    return (args, t)
    return go((), term)

def gather_args(term: hydra.core.Term, args: frozenlist[hydra.core.Term]) -> tuple[hydra.core.Term, frozenlist[hydra.core.Term]]:
    r"""Gather term arguments, stripping type-level constructs."""

    while True:
        match hydra.strip.deannotate_term(term):
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
        match hydra.strip.deannotate_term(term):
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
        return hydra.strip.deannotate_and_detype_term(term)
    def _hoist_stripped_body_1(v1):
        match v1:
            case hydra.core.FunctionLambda(value=lam):
                return is_tail_recursive_in_tail_position(func_name, lam.body)

            case _:
                return hydra.variables.is_free_variable_in_term(func_name, term)
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
                return hydra.strip.deannotate_and_detype_term(gather_fun())
            def _hoist_stripped_fun_body_1(v1):
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
                            return hydra.lib.lists.foldl((lambda ok, arg: hydra.lib.logic.and_(ok, hydra.variables.is_free_variable_in_term(func_name, arg))), True, gather_args())
                        return hydra.lib.logic.and_(hydra.lib.logic.and_(branches_ok(), dflt_ok()), args_ok())

                    case _:
                        return hydra.variables.is_free_variable_in_term(func_name, term)
            def _hoist_stripped_fun_body_2(v1):
                match v1:
                    case hydra.core.FunctionElimination(value=e):
                        return _hoist_stripped_fun_body_1(e)

                    case _:
                        return hydra.variables.is_free_variable_in_term(func_name, term)
            def _hoist_stripped_fun_body_3(v1):
                match v1:
                    case hydra.core.TermVariable(value=vname):
                        return hydra.lib.logic.if_else(hydra.lib.equality.equal(vname, func_name), (lambda : (args_no_func := hydra.lib.lists.foldl((lambda ok, arg: hydra.lib.logic.and_(ok, hydra.variables.is_free_variable_in_term(func_name, arg))), True, gather_args()), (args_no_lambda := (_hoist_args_no_lambda_1 := (lambda v12: (lambda lam: (ignore := lam.body, True)[1])(v12.value) if isinstance(v12, hydra.core.FunctionLambda) else False), _hoist_args_no_lambda_2 := (lambda v12: (lambda f2: _hoist_args_no_lambda_1(f2))(v12.value) if isinstance(v12, hydra.core.TermFunction) else False), hydra.lib.lists.foldl((lambda ok, arg: hydra.lib.logic.and_(ok, hydra.lib.logic.not_(hydra.rewriting.fold_over_term(hydra.coders.TraversalOrder.PRE, (lambda found, t: hydra.lib.logic.or_(found, _hoist_args_no_lambda_2(t))), False, arg)))), True, gather_args()))[2], hydra.lib.logic.and_(args_no_func, args_no_lambda))[1])[1]), (lambda : hydra.variables.is_free_variable_in_term(func_name, term)))

                    case hydra.core.TermFunction(value=f):
                        return _hoist_stripped_fun_body_2(f)

                    case _:
                        return hydra.variables.is_free_variable_in_term(func_name, term)
            return _hoist_stripped_fun_body_3(stripped_fun())

        case hydra.core.TermFunction(value=f):
            return _hoist_stripped_body_1(f)

        case hydra.core.TermLet(value=lt):
            @lru_cache(1)
            def bindings_ok() -> bool:
                return hydra.lib.lists.foldl((lambda ok, b: hydra.lib.logic.and_(ok, hydra.variables.is_free_variable_in_term(func_name, b.term))), True, lt.bindings)
            return hydra.lib.logic.and_(bindings_ok(), is_tail_recursive_in_tail_position(func_name, lt.body))

        case _:
            return hydra.variables.is_free_variable_in_term(func_name, term)

def is_self_tail_recursive(func_name: hydra.core.Name, body: hydra.core.Term) -> bool:
    r"""Check if a term body is self-tail-recursive with respect to a function name."""

    @lru_cache(1)
    def calls_self() -> bool:
        return hydra.lib.logic.not_(hydra.variables.is_free_variable_in_term(func_name, body))
    return hydra.lib.logic.if_else(calls_self(), (lambda : is_tail_recursive_in_tail_position(func_name, body)), (lambda : False))

def is_simple_assignment(term: hydra.core.Term):
    while True:
        def _hoist_hydra_analysis_is_simple_assignment_1(v1):
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
                return _hoist_hydra_analysis_is_simple_assignment_1(f)

            case hydra.core.TermLet():
                return False

            case hydra.core.TermTypeLambda():
                return False

            case hydra.core.TermTypeApplication(value=ta):
                term = ta.body
                continue

            case _:
                return (base_term := hydra.lib.pairs.first(gather_args(term, ())), (_hoist_base_term_body_1 := (lambda v1: (lambda _: False)(v1.value) if isinstance(v1, hydra.core.EliminationUnion) else True), _hoist_base_term_body_2 := (lambda v1: (lambda elim: _hoist_base_term_body_1(elim))(v1.value) if isinstance(v1, hydra.core.FunctionElimination) else True), _hoist_base_term_body_3 := (lambda v1: (lambda f: _hoist_base_term_body_2(f))(v1.value) if isinstance(v1, hydra.core.TermFunction) else True), _hoist_base_term_body_3(base_term))[3])[1]

def module_contains_binary_literals(mod: hydra.packaging.Module) -> bool:
    r"""Check whether a module contains any binary literal values."""

    def check_term(found: bool, term: hydra.core.Term):
        def _hoist_check_term_1(v1):
            match v1:
                case hydra.core.LiteralBinary():
                    return True

                case _:
                    return False
        def _hoist_check_term_2(v1):
            match v1:
                case hydra.core.TermLiteral(value=lit):
                    return _hoist_check_term_1(lit)

                case _:
                    return False
        return hydra.lib.logic.or_(found, _hoist_check_term_2(term))
    def term_contains_binary(term: hydra.core.Term) -> bool:
        return hydra.rewriting.fold_over_term(hydra.coders.TraversalOrder.PRE, (lambda x1, x2: check_term(x1, x2)), False, term)
    @lru_cache(1)
    def def_terms():
        def _hoist_def_terms_1(v1):
            match v1:
                case hydra.packaging.DefinitionTerm(value=td):
                    return Just(td.term)

                case _:
                    return Nothing()
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_def_terms_1(d)), mod.definitions))
    return hydra.lib.lists.foldl((lambda acc, t: hydra.lib.logic.or_(acc, term_contains_binary(t))), False, def_terms())

def module_dependency_namespaces(cx: T0, graph: hydra.graph.Graph, binds: bool, with_prims: bool, with_noms: bool, with_schema: bool, mod: hydra.packaging.Module) -> Either[hydra.errors.Error, frozenset[hydra.packaging.Namespace]]:
    r"""Find dependency namespaces in all elements of a module, excluding the module's own namespace (Either version)."""

    @lru_cache(1)
    def all_bindings():
        def _hoist_all_bindings_1(v1):
            match v1:
                case hydra.packaging.DefinitionType(value=td):
                    return Just((schema_term := cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.core.Type"))), (data_term := hydra.annotations.normalize_term_annotations(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(hydra.encode.core.type(td.type.type), hydra.lib.maps.from_list(((hydra.constants.key_type, schema_term),)))))), hydra.core.Binding(td.name, data_term, Just(hydra.core.TypeScheme((), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))), Nothing()))))[1])[1])

                case hydra.packaging.DefinitionTerm(value=td):
                    return Just(hydra.core.Binding(td.name, td.term, td.type))

                case _:
                    return Nothing()
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_all_bindings_1(d)), mod.definitions))
    return hydra.lib.eithers.map((lambda deps: hydra.lib.sets.delete(mod.namespace, deps)), dependency_namespaces(cx, graph, binds, with_prims, with_noms, with_schema, all_bindings()))

def namespaces_for_definitions(encode_namespace: Callable[[hydra.packaging.Namespace], T0], focus_ns: hydra.packaging.Namespace, defs: frozenlist[hydra.packaging.Definition]) -> hydra.packaging.Namespaces[T0]:
    r"""Create namespaces mapping for definitions."""

    @lru_cache(1)
    def nss() -> frozenset[hydra.packaging.Namespace]:
        return hydra.lib.sets.delete(focus_ns, definition_dependency_namespaces(defs))
    def to_pair(ns: hydra.packaging.Namespace) -> tuple[hydra.packaging.Namespace, T0]:
        return (ns, encode_namespace(ns))
    return hydra.packaging.Namespaces(to_pair(focus_ns), hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: to_pair(x1)), hydra.lib.sets.to_list(nss()))))
