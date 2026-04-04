# Note: this is an automatically generated file. Do not edit.

r"""Dependency extraction, binding sort, and let normalization."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import TypeVar, cast
import hydra.coders
import hydra.core
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.packaging
import hydra.rewriting
import hydra.sorting
import hydra.strip
import hydra.variables

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def term_dependency_names(binds: bool, with_prims: bool, with_noms: bool, term0: hydra.core.Term) -> frozenset[hydra.core.Name]:
    r"""Note: does not distinguish between bound and free variables; use freeVariablesInTerm for that."""

    def add_names(names: frozenset[hydra.core.Name], term: hydra.core.Term):
        def nominal(name: hydra.core.Name) -> frozenset[hydra.core.Name]:
            return hydra.lib.logic.if_else(with_noms, (lambda : hydra.lib.sets.insert(name, names)), (lambda : names))
        def prim(name: hydra.core.Name) -> frozenset[hydra.core.Name]:
            return hydra.lib.logic.if_else(with_prims, (lambda : hydra.lib.sets.insert(name, names)), (lambda : names))
        def var(name: hydra.core.Name) -> frozenset[hydra.core.Name]:
            return hydra.lib.logic.if_else(binds, (lambda : hydra.lib.sets.insert(name, names)), (lambda : names))
        def _hoist_var_body_1(v1):
            match v1:
                case hydra.core.EliminationRecord(value=proj):
                    return nominal(proj.type_name)

                case hydra.core.EliminationUnion(value=case_stmt):
                    return nominal(case_stmt.type_name)

                case hydra.core.EliminationWrap(value=name):
                    return nominal(name)

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def _hoist_var_body_2(v1):
            match v1:
                case hydra.core.FunctionPrimitive(value=name):
                    return prim(name)

                case hydra.core.FunctionElimination(value=e):
                    return _hoist_var_body_1(e)

                case _:
                    return names
        match term:
            case hydra.core.TermFunction(value=f):
                return _hoist_var_body_2(f)

            case hydra.core.TermRecord(value=record):
                return nominal(record.type_name)

            case hydra.core.TermUnion(value=injection):
                return nominal(injection.type_name)

            case hydra.core.TermVariable(value=name):
                return var(name)

            case hydra.core.TermWrap(value=wrapped_term):
                return nominal(wrapped_term.type_name)

            case _:
                return names
    return hydra.rewriting.fold_over_term(hydra.coders.TraversalOrder.PRE, (lambda x1, x2: add_names(x1, x2)), hydra.lib.sets.empty(), term0)

def definitions_with_dependencies(cx: hydra.context.Context, graph: hydra.graph.Graph, original: frozenlist[hydra.core.Binding]) -> Either[hydra.context.InContext[hydra.errors.Error], frozenlist[hydra.core.Binding]]:
    r"""Get definitions with their dependencies."""

    def dep_names(el: hydra.core.Binding) -> frozenlist[hydra.core.Name]:
        return hydra.lib.sets.to_list(term_dependency_names(True, False, False, el.term))
    @lru_cache(1)
    def all_dep_names() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.nub(hydra.lib.lists.concat2(hydra.lib.lists.map((lambda v1: v1.name), original), hydra.lib.lists.concat(hydra.lib.lists.map((lambda x1: dep_names(x1)), original))))
    return hydra.lib.eithers.map_list((lambda name: hydra.lexical.require_binding(cx, graph, name)), all_dep_names())

def flatten_let_terms(term: hydra.core.Term) -> hydra.core.Term:
    r"""Flatten nested let expressions."""

    def rewrite_binding(binding: hydra.core.Binding) -> tuple[hydra.core.Binding, frozenlist[hydra.core.Binding]]:
        key0 = binding.name
        val0 = binding.term
        t = binding.type
        match val0:
            case hydra.core.TermAnnotated(value=at):
                val1 = at.body
                ann = at.annotation
                @lru_cache(1)
                def recursive() -> tuple[hydra.core.Binding, frozenlist[hydra.core.Binding]]:
                    return rewrite_binding(hydra.core.Binding(key0, val1, t))
                @lru_cache(1)
                def inner_binding() -> hydra.core.Binding:
                    return hydra.lib.pairs.first(recursive())
                @lru_cache(1)
                def deps() -> frozenlist[hydra.core.Binding]:
                    return hydra.lib.pairs.second(recursive())
                val2 = inner_binding().term
                return (hydra.core.Binding(key0, cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(val2, ann))), t), deps())

            case hydra.core.TermLet(value=inner_let):
                bindings1 = inner_let.bindings
                body1 = inner_let.body
                prefix = hydra.lib.strings.cat2(key0.value, "_")
                def qualify(n: hydra.core.Name) -> hydra.core.Name:
                    return hydra.core.Name(hydra.lib.strings.cat2(prefix, n.value))
                def to_subst_pair(b: hydra.core.Binding) -> tuple[hydra.core.Name, hydra.core.Name]:
                    return (b.name, qualify(b.name))
                @lru_cache(1)
                def subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                    return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: to_subst_pair(x1)), bindings1))
                def replace_vars(v1: hydra.core.Term) -> hydra.core.Term:
                    return hydra.variables.substitute_variables(subst(), v1)
                @lru_cache(1)
                def new_body() -> hydra.core.Term:
                    return replace_vars(body1)
                def new_binding(b: hydra.core.Binding) -> hydra.core.Binding:
                    return hydra.core.Binding(qualify(b.name), replace_vars(b.term), b.type)
                return (hydra.core.Binding(key0, new_body(), t), hydra.lib.lists.map((lambda x1: new_binding(x1)), bindings1))

            case _:
                return (hydra.core.Binding(key0, val0, t), ())
    def flatten_body_let(bindings: frozenlist[hydra.core.Binding], body: hydra.core.Term) -> tuple[frozenlist[hydra.core.Binding], hydra.core.Term]:
        while True:
            match body:
                case hydra.core.TermLet(value=inner_lt):
                    return (inner_bindings := inner_lt.bindings, (inner_body := inner_lt.body, flatten_body_let(hydra.lib.lists.concat2(bindings, inner_bindings), inner_body))[1])[1]

                case _:
                    return (hydra.lib.lists.concat2((), bindings), body)
    def flatten(recurse: Callable[[T0], hydra.core.Term], term2: T0):
        @lru_cache(1)
        def rewritten() -> hydra.core.Term:
            return recurse(term2)
        def _hoist_rewritten_body_1(v1):
            match v1:
                case hydra.core.TermLet(value=lt):
                    bindings = lt.bindings
                    body = lt.body
                    def for_result(hr: tuple[T1, frozenlist[T1]]) -> frozenlist[T1]:
                        return hydra.lib.lists.concat2(hydra.lib.pairs.second(hr), hydra.lib.lists.pure(hydra.lib.pairs.first(hr)))
                    @lru_cache(1)
                    def flattened_bindings() -> frozenlist[hydra.core.Binding]:
                        return hydra.lib.lists.concat(hydra.lib.lists.map((lambda arg_: for_result(rewrite_binding(arg_))), bindings))
                    @lru_cache(1)
                    def merged() -> tuple[frozenlist[hydra.core.Binding], hydra.core.Term]:
                        return flatten_body_let(flattened_bindings(), body)
                    @lru_cache(1)
                    def new_bindings() -> frozenlist[hydra.core.Binding]:
                        return hydra.lib.pairs.first(merged())
                    @lru_cache(1)
                    def new_body() -> hydra.core.Term:
                        return hydra.lib.pairs.second(merged())
                    return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(new_bindings(), new_body())))

                case _:
                    return rewritten()
        return _hoist_rewritten_body_1(rewritten())
    return hydra.rewriting.rewrite_term((lambda x1, x2: flatten(x1, x2)), term)

def inline_type(schema: FrozenDict[hydra.core.Name, hydra.core.Type], typ: hydra.core.Type) -> Either[str, hydra.core.Type]:
    r"""Inline all type variables in a type using the provided schema (Either version). Note: this function is only appropriate for nonrecursive type definitions."""

    def f(recurse: Callable[[T0], Either[str, hydra.core.Type]], typ2: T0) -> Either[str, hydra.core.Type]:
        def after_recurse(tr: hydra.core.Type):
            def _hoist_after_recurse_1(tr, v1):
                match v1:
                    case hydra.core.TypeVariable(value=v):
                        return hydra.lib.maybes.maybe((lambda : Left(hydra.lib.strings.cat2("No such type in schema: ", v.value))), (lambda v12: inline_type(schema, v12)), hydra.lib.maps.lookup(v, schema))

                    case _:
                        return Right(tr)
            return _hoist_after_recurse_1(tr, tr)
        return hydra.lib.eithers.bind(recurse(typ2), (lambda tr: after_recurse(tr)))
    return hydra.rewriting.rewrite_type_m((lambda x1, x2: f(x1, x2)), typ)

def is_lambda(term: hydra.core.Term):
    while True:
        def _hoist_hydra_dependencies_is_lambda_1(v1):
            match v1:
                case hydra.core.FunctionLambda():
                    return True

                case _:
                    return False
        match hydra.strip.deannotate_term(term):
            case hydra.core.TermFunction(value=_match_value):
                return _hoist_hydra_dependencies_is_lambda_1(_match_value)

            case hydra.core.TermLet(value=lt):
                term = lt.body
                continue

            case _:
                return False

def lift_lambda_above_let(term0: hydra.core.Term) -> hydra.core.Term:
    r"""Rewrite terms like `let foo = bar in λx.baz` to `λx.let foo = bar in baz`, lifting lambda-bound variables above let-bound variables, recursively. This is helpful for targets such as Python."""

    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Term:
        def rewrite_binding(b: hydra.core.Binding) -> hydra.core.Binding:
            return hydra.core.Binding(b.name, rewrite(recurse, b.term), b.type)
        def rewrite_bindings(bs: frozenlist[hydra.core.Binding]) -> frozenlist[hydra.core.Binding]:
            return hydra.lib.lists.map((lambda x1: rewrite_binding(x1)), bs)
        def dig_for_lambdas(original: hydra.core.Term, cons: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term):
            def _hoist_dig_for_lambdas_1(cons, original, v1):
                match v1:
                    case hydra.core.FunctionLambda(value=l):
                        return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, dig_for_lambdas(cons(l.body), (lambda t: cons(t)), l.body))))))

                    case _:
                        return recurse(original)
            match term2:
                case hydra.core.TermAnnotated(value=at):
                    return dig_for_lambdas(original, (lambda t: cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(cons(t), at.annotation)))), at.body)

                case hydra.core.TermFunction(value=f):
                    return _hoist_dig_for_lambdas_1(cons, original, f)

                case hydra.core.TermLet(value=l):
                    return dig_for_lambdas(original, (lambda t: cons(cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(rewrite_bindings(l.bindings), t))))), l.body)

                case _:
                    return recurse(original)
        match term:
            case hydra.core.TermLet(value=l):
                return dig_for_lambdas(term, (lambda t: cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(rewrite_bindings(l.bindings), t)))), l.body)

            case _:
                return recurse(term)
    return hydra.rewriting.rewrite_term((lambda x1, x2: rewrite(x1, x2)), term0)

def prune_let(l: hydra.core.Let) -> hydra.core.Let:
    r"""Given a let expression, remove any unused bindings. The resulting expression is still a let, even if has no remaining bindings."""

    @lru_cache(1)
    def binding_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda b: (b.name, b.term)), l.bindings))
    root_name = hydra.core.Name("[[[root]]]")
    def adj(n: hydra.core.Name) -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.intersection(hydra.lib.sets.from_list(hydra.lib.maps.keys(binding_map())), hydra.variables.free_variables_in_term(hydra.lib.logic.if_else(hydra.lib.equality.equal(n, root_name), (lambda : l.body), (lambda : hydra.lib.maybes.from_just(hydra.lib.maps.lookup(n, binding_map()))))))
    @lru_cache(1)
    def reachable() -> frozenset[hydra.core.Name]:
        return hydra.sorting.find_reachable_nodes((lambda x1: adj(x1)), root_name)
    @lru_cache(1)
    def pruned_bindings() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.filter((lambda b: hydra.lib.sets.member(b.name, reachable())), l.bindings)
    return hydra.core.Let(pruned_bindings(), l.body)

def replace_typedefs(types: FrozenDict[hydra.core.Name, hydra.core.TypeScheme], typ0: hydra.core.Type) -> hydra.core.Type:
    r"""Replace all occurrences of simple typedefs (type aliases) with the aliased types, recursively."""

    def rewrite(recurse: Callable[[hydra.core.Type], hydra.core.Type], typ: hydra.core.Type) -> hydra.core.Type:
        match typ:
            case hydra.core.TypeAnnotated(value=at):
                return cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(rewrite(recurse, at.body), at.annotation)))

            case hydra.core.TypeRecord():
                return typ

            case hydra.core.TypeUnion():
                return typ

            case hydra.core.TypeVariable(value=v):
                def for_mono(t: hydra.core.Type):
                    def _hoist_for_mono_1(t, v1):
                        match v1:
                            case hydra.core.TypeRecord():
                                return typ

                            case hydra.core.TypeUnion():
                                return typ

                            case hydra.core.TypeWrap():
                                return typ

                            case _:
                                return rewrite(recurse, t)
                    return _hoist_for_mono_1(t, t)
                def for_type_scheme(ts: hydra.core.TypeScheme) -> hydra.core.Type:
                    t = ts.type
                    return hydra.lib.logic.if_else(hydra.lib.lists.null(ts.variables), (lambda : for_mono(t)), (lambda : typ))
                return hydra.lib.maybes.maybe((lambda : typ), (lambda ts: for_type_scheme(ts)), hydra.lib.maps.lookup(v, types))

            case hydra.core.TypeWrap():
                return typ

            case _:
                return recurse(typ)
    return hydra.rewriting.rewrite_type((lambda x1, x2: rewrite(x1, x2)), typ0)

def simplify_term(term: hydra.core.Term) -> hydra.core.Term:
    r"""Simplify terms by applying beta reduction where possible."""

    def simplify(recurse: Callable[[hydra.core.Term], T0], term2: hydra.core.Term) -> T0:
        def for_rhs(rhs: hydra.core.Term, var: hydra.core.Name, body: hydra.core.Term):
            def _hoist_for_rhs_1(body, var, v1):
                match v1:
                    case hydra.core.TermVariable(value=v):
                        return simplify_term(hydra.variables.substitute_variable(var, v, body))

                    case _:
                        return term2
            return _hoist_for_rhs_1(body, var, hydra.strip.deannotate_term(rhs))
        def for_lhs(lhs: hydra.core.Term, rhs: hydra.core.Term):
            def for_fun(fun: hydra.core.Function):
                def _hoist_for_fun_1(v1):
                    match v1:
                        case hydra.core.FunctionLambda(value=l):
                            var = l.parameter
                            body = l.body
                            return hydra.lib.logic.if_else(hydra.lib.sets.member(var, hydra.variables.free_variables_in_term(body)), (lambda : for_rhs(rhs, var, body)), (lambda : simplify_term(body)))

                        case _:
                            return term2
                return _hoist_for_fun_1(fun)
            def _hoist_for_fun_body_1(v1):
                match v1:
                    case hydra.core.TermFunction(value=fun):
                        return for_fun(fun)

                    case _:
                        return term2
            return _hoist_for_fun_body_1(hydra.strip.deannotate_term(lhs))
        def for_term(stripped: hydra.core.Term):
            def _hoist_for_term_1(v1):
                match v1:
                    case hydra.core.TermApplication(value=app):
                        lhs = app.function
                        rhs = app.argument
                        return for_lhs(lhs, rhs)

                    case _:
                        return term2
            return _hoist_for_term_1(stripped)
        @lru_cache(1)
        def stripped() -> hydra.core.Term:
            return hydra.strip.deannotate_term(term2)
        return recurse(for_term(stripped()))
    return hydra.rewriting.rewrite_term((lambda x1, x2: simplify(x1, x2)), term)

def to_short_names(original: frozenlist[hydra.core.Name]) -> FrozenDict[hydra.core.Name, hydra.core.Name]:
    r"""Generate short names from a list of fully qualified names."""

    def add_name(acc: FrozenDict[str, frozenset[hydra.core.Name]], name: hydra.core.Name) -> FrozenDict[str, frozenset[hydra.core.Name]]:
        @lru_cache(1)
        def local() -> str:
            return hydra.names.local_name_of(name)
        @lru_cache(1)
        def group() -> frozenset[hydra.core.Name]:
            return hydra.lib.maybes.from_maybe((lambda : hydra.lib.sets.empty()), hydra.lib.maps.lookup(local(), acc))
        return hydra.lib.maps.insert(local(), hydra.lib.sets.insert(name, group()), acc)
    def group_names_by_local(names: frozenlist[hydra.core.Name]) -> FrozenDict[str, frozenset[hydra.core.Name]]:
        return hydra.lib.lists.foldl((lambda x1, x2: add_name(x1, x2)), hydra.lib.maps.empty(), names)
    @lru_cache(1)
    def groups() -> FrozenDict[str, frozenset[hydra.core.Name]]:
        return group_names_by_local(original)
    def rename_group(local_names: tuple[str, frozenset[T0]]) -> frozenlist[tuple[T0, hydra.core.Name]]:
        @lru_cache(1)
        def local() -> str:
            return hydra.lib.pairs.first(local_names)
        @lru_cache(1)
        def names() -> frozenset[T0]:
            return hydra.lib.pairs.second(local_names)
        def range_from(start: int) -> frozenlist[int]:
            return hydra.lib.lists.cons(start, range_from(hydra.lib.math.add(start, 1)))
        def rename(name: T1, i: int) -> tuple[T1, hydra.core.Name]:
            return (name, hydra.core.Name(hydra.lib.logic.if_else(hydra.lib.equality.gt(i, 1), (lambda : hydra.lib.strings.cat2(local(), hydra.lib.literals.show_int32(i))), (lambda : local()))))
        return hydra.lib.lists.zip_with((lambda x1, x2: rename(x1, x2)), hydra.lib.sets.to_list(names()), range_from(1))
    return hydra.lib.maps.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda x1: rename_group(x1)), hydra.lib.maps.to_list(groups()))))

def topological_sort_binding_map(binding_map: FrozenDict[hydra.core.Name, hydra.core.Term]) -> frozenlist[frozenlist[tuple[hydra.core.Name, hydra.core.Term]]]:
    r"""Topological sort of connected components, in terms of dependencies between variable/term binding pairs."""

    @lru_cache(1)
    def bindings() -> frozenlist[tuple[hydra.core.Name, hydra.core.Term]]:
        return hydra.lib.maps.to_list(binding_map)
    @lru_cache(1)
    def keys() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.from_list(hydra.lib.lists.map((lambda x1: hydra.lib.pairs.first(x1)), bindings()))
    def has_type_annotation(term: hydra.core.Term) -> bool:
        while True:
            match term:
                case hydra.core.TermAnnotated(value=at):
                    term = at.body
                    continue

                case _:
                    return False
    def deps_of(name_and_term: tuple[T0, hydra.core.Term]) -> tuple[T0, frozenlist[hydra.core.Name]]:
        @lru_cache(1)
        def name() -> T0:
            return hydra.lib.pairs.first(name_and_term)
        @lru_cache(1)
        def term() -> hydra.core.Term:
            return hydra.lib.pairs.second(name_and_term)
        return (name(), hydra.lib.logic.if_else(has_type_annotation(term()), (lambda : ()), (lambda : hydra.lib.sets.to_list(hydra.lib.sets.intersection(keys(), hydra.variables.free_variables_in_term(term()))))))
    def to_pair(name: hydra.core.Name) -> tuple[hydra.core.Name, hydra.core.Term]:
        return (name, hydra.lib.maybes.from_maybe((lambda : cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("Impossible!"))))), hydra.lib.maps.lookup(name, binding_map)))
    return hydra.lib.lists.map((lambda v1: hydra.lib.lists.map((lambda x1: to_pair(x1)), v1)), hydra.sorting.topological_sort_components(hydra.lib.lists.map((lambda x1: deps_of(x1)), bindings())))

def topological_sort_bindings(els: frozenlist[hydra.core.Binding]) -> Either[frozenlist[frozenlist[hydra.core.Name]], frozenlist[hydra.core.Name]]:
    r"""Topological sort of elements based on their dependencies."""

    def adjlist(e: hydra.core.Binding) -> tuple[hydra.core.Name, frozenlist[hydra.core.Name]]:
        return (e.name, hydra.lib.sets.to_list(term_dependency_names(False, True, True, e.term)))
    return hydra.sorting.topological_sort(hydra.lib.lists.map((lambda x1: adjlist(x1)), els))

def type_names_in_type(typ0: hydra.core.Type) -> frozenset[T0]:
    def add_names(names: T1, typ: T2) -> T1:
        return names
    return hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda x1, x2: add_names(x1, x2)), hydra.lib.sets.empty(), typ0)

def type_dependency_names(with_schema: bool, typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    return hydra.lib.logic.if_else(with_schema, (lambda : hydra.lib.sets.union(hydra.variables.free_variables_in_type(typ), type_names_in_type(typ))), (lambda : hydra.variables.free_variables_in_type(typ)))

def topological_sort_type_definitions(defs: frozenlist[hydra.packaging.TypeDefinition]) -> frozenlist[frozenlist[hydra.packaging.TypeDefinition]]:
    r"""Topologically sort type definitions by dependencies."""

    def to_pair(def_: hydra.packaging.TypeDefinition) -> tuple[hydra.core.Name, frozenlist[hydra.core.Name]]:
        return (def_.name, hydra.lib.sets.to_list(type_dependency_names(False, def_.type.type)))
    @lru_cache(1)
    def name_to_def() -> FrozenDict[hydra.core.Name, hydra.packaging.TypeDefinition]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda d: (d.name, d)), defs))
    @lru_cache(1)
    def sorted() -> frozenlist[frozenlist[hydra.core.Name]]:
        return hydra.sorting.topological_sort_components(hydra.lib.lists.map((lambda x1: to_pair(x1)), defs))
    return hydra.lib.lists.map((lambda names: hydra.lib.maybes.cat(hydra.lib.lists.map((lambda n: hydra.lib.maps.lookup(n, name_to_def())), names))), sorted())
