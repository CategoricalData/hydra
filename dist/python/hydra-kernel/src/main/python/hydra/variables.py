# Note: this is an automatically generated file. Do not edit.

r"""Free variable analysis, term-level substitution, and unshadowing."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.coders
import hydra.core
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
import hydra.rewriting

T0 = TypeVar("T0")

def free_variables_in_type(typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    r"""Find the free variables (i.e. variables not bound by a lambda or let) in a type."""

    @lru_cache(1)
    def dflt_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.lists.foldl((lambda s, t: hydra.lib.sets.union(s, free_variables_in_type(t))), hydra.lib.sets.empty(), hydra.rewriting.subtypes(typ))
    match typ:
        case hydra.core.TypeForall(value=lt):
            return hydra.lib.sets.delete(lt.parameter, free_variables_in_type(lt.body))

        case hydra.core.TypeVariable(value=v):
            return hydra.lib.sets.singleton(v)

        case _:
            return dflt_vars()

def free_type_variables_in_term(term0: hydra.core.Term) -> frozenset[hydra.core.Name]:
    r"""Get the set of free type variables in a term (including schema names, where they appear in type annotations). In this context, only the type schemes of let bindings can bind type variables; type lambdas do not."""

    def all_of(sets: frozenlist[frozenset[T0]]) -> frozenset[T0]:
        return hydra.lib.lists.foldl((lambda x1, x2: hydra.lib.sets.union(x1, x2)), hydra.lib.sets.empty(), sets)
    def try_type(tvars: frozenset[hydra.core.Name], typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.difference(free_variables_in_type(typ), tvars)
    def get_all(vars: frozenset[hydra.core.Name], term: hydra.core.Term) -> frozenset[hydra.core.Name]:
        def recurse(v1: hydra.core.Term) -> frozenset[hydra.core.Name]:
            return get_all(vars, v1)
        @lru_cache(1)
        def dflt() -> frozenset[hydra.core.Name]:
            return all_of(hydra.lib.lists.map((lambda x1: recurse(x1)), hydra.rewriting.subterms(term)))
        match term:
            case hydra.core.TermLambda(value=l):
                @lru_cache(1)
                def domt() -> frozenset[hydra.core.Name]:
                    return hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda v1: try_type(vars, v1)), l.domain)
                return hydra.lib.sets.union(domt(), recurse(l.body))

            case hydra.core.TermLet(value=l2):
                def for_binding(b: hydra.core.Binding) -> frozenset[hydra.core.Name]:
                    @lru_cache(1)
                    def new_vars() -> frozenset[hydra.core.Name]:
                        return hydra.lib.maybes.maybe((lambda : vars), (lambda ts: hydra.lib.sets.union(vars, hydra.lib.sets.from_list(ts.variables))), b.type)
                    return hydra.lib.sets.union(get_all(new_vars(), b.term), hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda ts: try_type(new_vars(), ts.type)), b.type))
                return hydra.lib.sets.union(all_of(hydra.lib.lists.map((lambda x1: for_binding(x1)), l2.bindings)), recurse(l2.body))

            case hydra.core.TermTypeApplication(value=tt):
                return hydra.lib.sets.union(try_type(vars, tt.type), recurse(tt.body))

            case hydra.core.TermTypeLambda(value=tl):
                return hydra.lib.sets.union(try_type(vars, cast(hydra.core.Type, hydra.core.TypeVariable(tl.parameter))), recurse(tl.body))

            case _:
                return dflt()
    return get_all(hydra.lib.sets.empty(), term0)

def free_variables_in_term(term: hydra.core.Term) -> frozenset[hydra.core.Name]:
    r"""Find the free variables (i.e. variables not bound by a lambda or let) in a term."""

    def dflt_vars(_: T0) -> frozenset[hydra.core.Name]:
        return hydra.lib.lists.foldl((lambda s, t: hydra.lib.sets.union(s, free_variables_in_term(t))), hydra.lib.sets.empty(), hydra.rewriting.subterms(term))
    match term:
        case hydra.core.TermLambda(value=l):
            return hydra.lib.sets.delete(l.parameter, free_variables_in_term(l.body))

        case hydra.core.TermLet(value=l2):
            return hydra.lib.sets.difference(dflt_vars(None), hydra.lib.sets.from_list(hydra.lib.lists.map((lambda v1: v1.name), l2.bindings)))

        case hydra.core.TermVariable(value=v):
            return hydra.lib.sets.singleton(v)

        case _:
            return dflt_vars(None)

def free_variables_in_type_ordered(typ: hydra.core.Type) -> frozenlist[hydra.core.Name]:
    r"""Find the free variables in a type in deterministic left-to-right order."""

    def collect_vars(bound_vars: frozenset[hydra.core.Name], t: hydra.core.Type) -> frozenlist[hydra.core.Name]:
        match t:
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.logic.if_else(hydra.lib.sets.member(v, bound_vars), (lambda : ()), (lambda : (v,)))

            case hydra.core.TypeForall(value=ft):
                return collect_vars(hydra.lib.sets.insert(ft.parameter, bound_vars), ft.body)

            case _:
                return hydra.lib.lists.concat(hydra.lib.lists.map((lambda v1: collect_vars(bound_vars, v1)), hydra.rewriting.subtypes(t)))
    return hydra.lib.lists.nub(collect_vars(hydra.lib.sets.empty(), typ))

def free_variables_in_type_scheme(ts: hydra.core.TypeScheme) -> frozenset[hydra.core.Name]:
    r"""Find free variables in a type scheme."""

    vars = ts.variables
    t = ts.type
    return hydra.lib.sets.difference(free_variables_in_type(t), hydra.lib.sets.from_list(vars))

def free_variables_in_type_simple(typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    r"""Same as freeVariablesInType, but ignores the binding action of lambda types."""

    def helper(types: frozenset[hydra.core.Name], typ2: hydra.core.Type) -> frozenset[hydra.core.Name]:
        match typ2:
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.sets.insert(v, types)

            case _:
                return types
    return hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda x1, x2: helper(x1, x2)), hydra.lib.sets.empty(), typ)

def free_variables_in_type_scheme_simple(ts: hydra.core.TypeScheme) -> frozenset[hydra.core.Name]:
    r"""Find free variables in a type scheme (simple version)."""

    vars = ts.variables
    t = ts.type
    return hydra.lib.sets.difference(free_variables_in_type_simple(t), hydra.lib.sets.from_list(vars))

def is_free_variable_in_term(v: hydra.core.Name, term: hydra.core.Term) -> bool:
    r"""Check whether a variable is free (not bound) in a term."""

    return hydra.lib.logic.not_(hydra.lib.sets.member(v, free_variables_in_term(term)))

def normalize_type_variables_in_term(term: hydra.core.Term) -> hydra.core.Term:
    r"""Recursively replace the type variables of let bindings with the systematic type variables t0, t1, t2, ..."""

    def replace_name(subst: FrozenDict[T0, T0], v: T0) -> T0:
        return hydra.lib.maybes.from_maybe((lambda : v), hydra.lib.maps.lookup(v, subst))
    def subst_type(subst: FrozenDict[hydra.core.Name, hydra.core.Name], typ: hydra.core.Type) -> hydra.core.Type:
        def rewrite(recurse: Callable[[hydra.core.Type], hydra.core.Type], typ2: hydra.core.Type) -> hydra.core.Type:
            match typ2:
                case hydra.core.TypeVariable(value=v):
                    return cast(hydra.core.Type, hydra.core.TypeVariable(replace_name(subst, v)))

                case _:
                    return recurse(typ2)
        return hydra.rewriting.rewrite_type((lambda x1, x2: rewrite(x1, x2)), typ)
    def rewrite_with_subst(state: tuple[tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]], int], term0: hydra.core.Term) -> hydra.core.Term:
        @lru_cache(1)
        def sb() -> tuple[FrozenDict[hydra.core.Name, hydra.core.Name], frozenset[hydra.core.Name]]:
            return hydra.lib.pairs.first(state)
        @lru_cache(1)
        def next() -> int:
            return hydra.lib.pairs.second(state)
        @lru_cache(1)
        def subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
            return hydra.lib.pairs.first(sb())
        @lru_cache(1)
        def bound_vars() -> frozenset[hydra.core.Name]:
            return hydra.lib.pairs.second(sb())
        def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term) -> hydra.core.Term:
            match term2:
                case hydra.core.TermLambda(value=l):
                    domain = l.domain
                    return cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(l.parameter, hydra.lib.maybes.map((lambda v1: subst_type(subst(), v1)), domain), rewrite_with_subst(((subst(), bound_vars()), next()), l.body))))

                case hydra.core.TermLet(value=lt):
                    bindings0 = lt.bindings
                    body0 = lt.body
                    def step(acc: frozenlist[hydra.core.Binding], bs: frozenlist[hydra.core.Binding]) -> frozenlist[hydra.core.Binding]:
                        return hydra.lib.maybes.maybe((lambda : hydra.lib.lists.reverse(acc)), (lambda uc: (b := hydra.lib.pairs.first(uc), tl := hydra.lib.pairs.second(uc), no_type := (new_val := rewrite_with_subst(((subst(), bound_vars()), next()), b.term), (b1 := hydra.core.Binding(b.name, new_val, Nothing()), step(hydra.lib.lists.cons(b1, acc), tl))[1])[1], with_type := (lambda ts: (vars := ts.variables, typ := ts.type, k := hydra.lib.lists.length(vars), gen := (lambda i, rem, acc2: (ti := hydra.core.Name(hydra.lib.strings.cat2("t", hydra.lib.literals.show_int32(hydra.lib.math.add(next(), i)))), hydra.lib.logic.if_else(hydra.lib.equality.equal(rem, 0), (lambda : hydra.lib.lists.reverse(acc2)), (lambda : gen(hydra.lib.math.add(i, 1), hydra.lib.math.sub(rem, 1), hydra.lib.lists.cons(ti, acc2)))))[1]), new_vars := gen(0, k, ()), new_subst := hydra.lib.maps.union(hydra.lib.maps.from_list(hydra.lib.lists.zip(vars, new_vars)), subst()), new_bound := hydra.lib.sets.union(bound_vars(), hydra.lib.sets.from_list(new_vars)), new_val := rewrite_with_subst(((new_subst, new_bound), hydra.lib.math.add(next(), k)), b.term), rename_constraint_keys := (lambda constraint_map: hydra.lib.maps.from_list(hydra.lib.lists.map((lambda p: (old_name := hydra.lib.pairs.first(p), meta := hydra.lib.pairs.second(p), new_name := hydra.lib.maybes.from_maybe((lambda : old_name), hydra.lib.maps.lookup(old_name, new_subst)), (new_name, meta))[3]), hydra.lib.maps.to_list(constraint_map)))), old_constraints := ts.constraints, new_constraints := hydra.lib.maybes.map((lambda x1: rename_constraint_keys(x1)), old_constraints), b1 := hydra.core.Binding(b.name, new_val, Just(hydra.core.TypeScheme(new_vars, subst_type(new_subst, typ), new_constraints))), step(hydra.lib.lists.cons(b1, acc), tl))[12]), hydra.lib.maybes.maybe((lambda : no_type), (lambda ts: with_type(ts)), b.type))[4]), hydra.lib.lists.uncons(bs))
                    @lru_cache(1)
                    def bindings1() -> frozenlist[hydra.core.Binding]:
                        return step((), bindings0)
                    return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(bindings1(), rewrite_with_subst(((subst(), bound_vars()), next()), body0))))

                case hydra.core.TermTypeApplication(value=tt):
                    return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(rewrite_with_subst(((subst(), bound_vars()), next()), tt.body), subst_type(subst(), tt.type))))

                case hydra.core.TermTypeLambda(value=ta):
                    return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(replace_name(subst(), ta.parameter), rewrite_with_subst(((subst(), bound_vars()), next()), ta.body))))

                case _:
                    return recurse(term2)
        return hydra.rewriting.rewrite_term((lambda x1, x2: rewrite(x1, x2)), term0)
    return rewrite_with_subst(((hydra.lib.maps.empty(), hydra.lib.sets.empty()), 0), term)

def replace_free_term_variable(vold: hydra.core.Name, tnew: hydra.core.Term, term: hydra.core.Term) -> hydra.core.Term:
    r"""Replace a free variable in a term."""

    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], t: hydra.core.Term) -> hydra.core.Term:
        match t:
            case hydra.core.TermLambda(value=l):
                v = l.parameter
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, vold), (lambda : t), (lambda : recurse(t)))

            case hydra.core.TermVariable(value=v):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, vold), (lambda : tnew), (lambda : cast(hydra.core.Term, hydra.core.TermVariable(v))))

            case _:
                return recurse(t)
    return hydra.rewriting.rewrite_term((lambda x1, x2: rewrite(x1, x2)), term)

def replace_free_type_variable(v: hydra.core.Name, rep: hydra.core.Type, typ: hydra.core.Type) -> hydra.core.Type:
    r"""Replace free occurrences of a name in a type."""

    def map_expr(recurse: Callable[[hydra.core.Type], hydra.core.Type], t: hydra.core.Type) -> hydra.core.Type:
        match t:
            case hydra.core.TypeForall(value=ft):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, ft.parameter), (lambda : t), (lambda : cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(ft.parameter, recurse(ft.body))))))

            case hydra.core.TypeVariable(value=v_):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, v_), (lambda : rep), (lambda : t))

            case _:
                return recurse(t)
    return hydra.rewriting.rewrite_type((lambda x1, x2: map_expr(x1, x2)), typ)

def substitute_type_variables(subst: FrozenDict[hydra.core.Name, hydra.core.Name], typ: hydra.core.Type) -> hydra.core.Type:
    r"""Substitute type variables in a type."""

    def replace(recurse: Callable[[hydra.core.Type], hydra.core.Type], typ2: hydra.core.Type) -> hydra.core.Type:
        match typ2:
            case hydra.core.TypeVariable(value=n):
                return cast(hydra.core.Type, hydra.core.TypeVariable(hydra.lib.maybes.from_maybe((lambda : n), hydra.lib.maps.lookup(n, subst))))

            case _:
                return recurse(typ2)
    return hydra.rewriting.rewrite_type((lambda x1, x2: replace(x1, x2)), typ)

def substitute_type_variables_in_term(subst: FrozenDict[hydra.core.Name, hydra.core.Name], term: hydra.core.Term) -> hydra.core.Term:
    r"""Substitute type variables throughout a term, including in type annotations, type applications, lambda domains, and type schemes."""

    def st(v1: hydra.core.Type) -> hydra.core.Type:
        return substitute_type_variables(subst, v1)
    def st_opt(mt: Maybe[hydra.core.Type]) -> Maybe[hydra.core.Type]:
        return hydra.lib.maybes.map((lambda x1: st(x1)), mt)
    def st_scheme(ts: hydra.core.TypeScheme) -> hydra.core.TypeScheme:
        return hydra.core.TypeScheme(ts.variables, st(ts.type), ts.constraints)
    def st_scheme_opt(mts: Maybe[hydra.core.TypeScheme]) -> Maybe[hydra.core.TypeScheme]:
        return hydra.lib.maybes.map((lambda x1: st_scheme(x1)), mts)
    def replace(recurse: Callable[[hydra.core.Term], hydra.core.Term], t: hydra.core.Term) -> hydra.core.Term:
        match t:
            case hydra.core.TermLambda(value=l):
                return cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(l.parameter, st_opt(l.domain), recurse(l.body))))

            case hydra.core.TermLet(value=lt):
                def map_binding(b: hydra.core.Binding) -> hydra.core.Binding:
                    return hydra.core.Binding(b.name, recurse(b.term), st_scheme_opt(b.type))
                return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map((lambda x1: map_binding(x1)), lt.bindings), recurse(lt.body))))

            case hydra.core.TermTypeApplication(value=tt):
                return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(recurse(tt.body), st(tt.type))))

            case hydra.core.TermTypeLambda(value=tl):
                return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(hydra.lib.maybes.from_maybe((lambda : tl.parameter), hydra.lib.maps.lookup(tl.parameter, subst)), recurse(tl.body))))

            case hydra.core.TermAnnotated(value=at):
                return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(recurse(at.body), at.annotation)))

            case _:
                return recurse(t)
    return hydra.rewriting.rewrite_term((lambda x1, x2: replace(x1, x2)), term)

def substitute_variable(from_: hydra.core.Name, to: hydra.core.Name, term: hydra.core.Term) -> hydra.core.Term:
    r"""Substitute one variable for another in a term."""

    def replace(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term) -> hydra.core.Term:
        match term2:
            case hydra.core.TermVariable(value=x):
                return cast(hydra.core.Term, hydra.core.TermVariable(hydra.lib.logic.if_else(hydra.lib.equality.equal(x, from_), (lambda : to), (lambda : x))))

            case hydra.core.TermLambda(value=l):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(l.parameter, from_), (lambda : term2), (lambda : recurse(term2)))

            case _:
                return recurse(term2)
    return hydra.rewriting.rewrite_term((lambda x1, x2: replace(x1, x2)), term)

def substitute_variables(subst: FrozenDict[hydra.core.Name, hydra.core.Name], term: hydra.core.Term) -> hydra.core.Term:
    r"""Substitute multiple variables in a term."""

    def replace(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term) -> hydra.core.Term:
        match term2:
            case hydra.core.TermVariable(value=n):
                return cast(hydra.core.Term, hydra.core.TermVariable(hydra.lib.maybes.from_maybe((lambda : n), hydra.lib.maps.lookup(n, subst))))

            case hydra.core.TermLambda(value=l):
                return hydra.lib.maybes.maybe((lambda : recurse(term2)), (lambda _: term2), hydra.lib.maps.lookup(l.parameter, subst))

            case _:
                return recurse(term2)
    return hydra.rewriting.rewrite_term((lambda x1, x2: replace(x1, x2)), term)

def unshadow_variables(term0: hydra.core.Term) -> hydra.core.Term:
    r"""Rename all shadowed variables (both lambda parameters and let-bound variables that shadow lambda parameters) in a term."""

    def fresh_name(base: hydra.core.Name, i: int, m: FrozenDict[hydra.core.Name, T0]) -> hydra.core.Name:
        @lru_cache(1)
        def candidate() -> hydra.core.Name:
            return hydra.core.Name(hydra.lib.strings.cat2(base.value, hydra.lib.literals.show_int32(i)))
        return hydra.lib.logic.if_else(hydra.lib.maps.member(candidate(), m), (lambda : fresh_name(base, hydra.lib.math.add(i, 1), m)), (lambda : candidate()))
    def f(recurse: Callable[[FrozenDict[hydra.core.Name, hydra.core.Name], hydra.core.Term], hydra.core.Term], m: FrozenDict[hydra.core.Name, hydra.core.Name], term: hydra.core.Term) -> hydra.core.Term:
        match term:
            case hydra.core.TermLambda(value=l):
                v = l.parameter
                domain = l.domain
                body = l.body
                return hydra.lib.logic.if_else(hydra.lib.maps.member(v, m), (lambda : (v2 := fresh_name(v, 2, m), (m2 := hydra.lib.maps.insert(v, v2, hydra.lib.maps.insert(v2, v2, m)), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(v2, domain, f(recurse, m2, body)))))[1])[1]), (lambda : cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(v, domain, f(recurse, hydra.lib.maps.insert(v, v, m), body))))))

            case hydra.core.TermLet(value=lt):
                @lru_cache(1)
                def m2() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                    return hydra.lib.lists.foldl((lambda acc, b: (bname := b.name, hydra.lib.logic.if_else(hydra.lib.maps.member(bname, acc), (lambda : acc), (lambda : hydra.lib.maps.insert(bname, bname, acc))))[1]), m, lt.bindings)
                return recurse(m2(), term)

            case hydra.core.TermVariable(value=v):
                return cast(hydra.core.Term, hydra.core.TermVariable(hydra.lib.maybes.maybe((lambda : v), (lambda renamed: renamed), hydra.lib.maps.lookup(v, m))))

            case _:
                return recurse(m, term)
    return hydra.rewriting.rewrite_term_with_context((lambda x1, x2, x3: f(x1, x2, x3)), hydra.lib.maps.empty(), term0)
