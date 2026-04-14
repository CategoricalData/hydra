# Note: this is an automatically generated file. Do not edit.

r"""Core rewrite and fold combinators for terms and types."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.coders
import hydra.core
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.paths
import hydra.scoping

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")
T4 = TypeVar("T4")
T5 = TypeVar("T5")
T6 = TypeVar("T6")

def apply_inside_type_lambdas_and_annotations(f: Callable[[hydra.core.Term], hydra.core.Term], term0: hydra.core.Term) -> hydra.core.Term:
    r"""Apply a term-level function inside any leading type lambdas."""

    match term0:
        case hydra.core.TermAnnotated(value=at):
            return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(apply_inside_type_lambdas_and_annotations(f, at.body), at.annotation)))

        case hydra.core.TermTypeLambda(value=tl):
            return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, apply_inside_type_lambdas_and_annotations(f, tl.body))))

        case _:
            return f(term0)

def subterms(v1: hydra.core.Term) -> frozenlist[hydra.core.Term]:
    r"""Find the children of a given term."""

    match v1:
        case hydra.core.TermAnnotated(value=at):
            return (at.body,)

        case hydra.core.TermApplication(value=p):
            return (p.function, p.argument)

        case hydra.core.TermCases(value=cs):
            return hydra.lib.lists.concat2(hydra.lib.maybes.maybe((lambda : ()), (lambda t: (t,)), cs.default), hydra.lib.lists.map((lambda v1: v1.term), cs.cases))

        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: (l,)), (lambda r: (r,)), e)

        case hydra.core.TermLambda(value=l):
            return (l.body,)

        case hydra.core.TermLet(value=lt):
            return hydra.lib.lists.cons(lt.body, hydra.lib.lists.map((lambda v1: v1.term), lt.bindings))

        case hydra.core.TermList(value=l2):
            return l2

        case hydra.core.TermLiteral():
            return ()

        case hydra.core.TermMap(value=m):
            return hydra.lib.lists.concat(hydra.lib.lists.map((lambda p: (hydra.lib.pairs.first(p), hydra.lib.pairs.second(p))), hydra.lib.maps.to_list(m)))

        case hydra.core.TermMaybe(value=m2):
            return hydra.lib.maybes.maybe((lambda : ()), (lambda t: (t,)), m2)

        case hydra.core.TermPair(value=p2):
            return (hydra.lib.pairs.first(p2), hydra.lib.pairs.second(p2))

        case hydra.core.TermProject():
            return ()

        case hydra.core.TermRecord(value=rt):
            return hydra.lib.lists.map((lambda v1: v1.term), rt.fields)

        case hydra.core.TermSet(value=l3):
            return hydra.lib.sets.to_list(l3)

        case hydra.core.TermTypeApplication(value=ta):
            return (ta.body,)

        case hydra.core.TermTypeLambda(value=ta2):
            return (ta2.body,)

        case hydra.core.TermInject(value=ut):
            return (ut.field.term,)

        case hydra.core.TermUnit():
            return ()

        case hydra.core.TermUnwrap():
            return ()

        case hydra.core.TermVariable():
            return ()

        case hydra.core.TermWrap(value=n):
            return (n.body,)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def fold_over_term(order: hydra.coders.TraversalOrder, fld: Callable[[T0, hydra.core.Term], T0], b0: T0, term: hydra.core.Term) -> T0:
    r"""Fold over a term, traversing its subterms in the specified order."""

    match order:
        case hydra.coders.TraversalOrder.PRE:
            return hydra.lib.lists.foldl((lambda v1, v2: fold_over_term(order, fld, v1, v2)), fld(b0, term), subterms(term))

        case hydra.coders.TraversalOrder.POST:
            return fld(hydra.lib.lists.foldl((lambda v1, v2: fold_over_term(order, fld, v1, v2)), b0, subterms(term)), term)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def subtypes(v1: hydra.core.Type) -> frozenlist[hydra.core.Type]:
    r"""Find the children of a given type expression."""

    match v1:
        case hydra.core.TypeAnnotated(value=at):
            return (at.body,)

        case hydra.core.TypeApplication(value=at2):
            return (at2.function, at2.argument)

        case hydra.core.TypeEither(value=et):
            return (et.left, et.right)

        case hydra.core.TypePair(value=pt):
            return (pt.first, pt.second)

        case hydra.core.TypeFunction(value=ft):
            return (ft.domain, ft.codomain)

        case hydra.core.TypeForall(value=lt):
            return (lt.body,)

        case hydra.core.TypeList(value=lt2):
            return (lt2,)

        case hydra.core.TypeLiteral():
            return ()

        case hydra.core.TypeMap(value=mt):
            return (mt.keys, mt.values)

        case hydra.core.TypeMaybe(value=ot):
            return (ot,)

        case hydra.core.TypeRecord(value=rt):
            return hydra.lib.lists.map((lambda v1: v1.type), rt)

        case hydra.core.TypeSet(value=st):
            return (st,)

        case hydra.core.TypeUnion(value=rt2):
            return hydra.lib.lists.map((lambda v1: v1.type), rt2)

        case hydra.core.TypeUnit():
            return ()

        case hydra.core.TypeVariable():
            return ()

        case hydra.core.TypeVoid():
            return ()

        case hydra.core.TypeWrap(value=nt):
            return (nt,)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def fold_over_type(order: hydra.coders.TraversalOrder, fld: Callable[[T0, hydra.core.Type], T0], b0: T0, typ: hydra.core.Type) -> T0:
    r"""Fold over a type, traversing its subtypes in the specified order."""

    match order:
        case hydra.coders.TraversalOrder.PRE:
            return hydra.lib.lists.foldl((lambda v1, v2: fold_over_type(order, fld, v1, v2)), fld(b0, typ), subtypes(typ))

        case hydra.coders.TraversalOrder.POST:
            return fld(hydra.lib.lists.foldl((lambda v1, v2: fold_over_type(order, fld, v1, v2)), b0, subtypes(typ)), typ)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def rewrite_and_fold_term_with_path(f: Callable[[
  Callable[[frozenlist[hydra.paths.SubtermStep], T0, hydra.core.Term], tuple[T0, hydra.core.Term]],
  frozenlist[hydra.paths.SubtermStep],
  T0,
  hydra.core.Term], tuple[T0, hydra.core.Term]], term0: T0, v1: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
    r"""Rewrite a term with path tracking, and fold a function over it, accumulating a value. The path is a list of SubtermSteps from root to current position."""

    def fsub(recurse: Callable[[frozenlist[hydra.paths.SubtermStep], T1, hydra.core.Term], tuple[T1, hydra.core.Term]], path: frozenlist[hydra.paths.SubtermStep], val0: T1, term02: hydra.core.Term):
        def for_single_with_accessor(rec: Callable[[frozenlist[hydra.paths.SubtermStep], T2, T3], tuple[T4, T5]], cons: Callable[[T5], T6], accessor: hydra.paths.SubtermStep, val: T2, term: T3) -> tuple[T4, T6]:
            @lru_cache(1)
            def r() -> tuple[T4, T5]:
                return rec(hydra.lib.lists.concat2(path, (accessor,)), val, term)
            return (hydra.lib.pairs.first(r()), cons(hydra.lib.pairs.second(r())))
        def for_many_with_accessors(rec: Callable[[frozenlist[hydra.paths.SubtermStep], T2, T3], tuple[T2, T4]], cons: Callable[[frozenlist[T4]], T5], val: T2, accessor_term_pairs: frozenlist[tuple[hydra.paths.SubtermStep, T3]]) -> tuple[T2, T5]:
            @lru_cache(1)
            def rr() -> tuple[T2, frozenlist[T4]]:
                return hydra.lib.lists.foldl((lambda r, atp: (r2 := rec(hydra.lib.lists.concat2(path, (hydra.lib.pairs.first(atp),)), hydra.lib.pairs.first(r), hydra.lib.pairs.second(atp)), (hydra.lib.pairs.first(r2), hydra.lib.lists.cons(hydra.lib.pairs.second(r2), hydra.lib.pairs.second(r))))[1]), (val, ()), accessor_term_pairs)
            return (hydra.lib.pairs.first(rr()), cons(hydra.lib.lists.reverse(hydra.lib.pairs.second(rr()))))
        def for_field_with_accessor(mk_accessor: Callable[[hydra.core.Name], hydra.paths.SubtermStep], val: T1, field: hydra.core.Field) -> tuple[T1, hydra.core.Field]:
            @lru_cache(1)
            def r() -> tuple[T1, hydra.core.Term]:
                return recurse(hydra.lib.lists.concat2(path, (mk_accessor(field.name),)), val, field.term)
            return (hydra.lib.pairs.first(r()), hydra.core.Field(field.name, hydra.lib.pairs.second(r())))
        def for_fields_with_accessor(mk_accessor: Callable[[hydra.core.Name], hydra.paths.SubtermStep], v1: T1, v2: frozenlist[tuple[hydra.paths.SubtermStep, hydra.core.Field]]) -> tuple[T1, frozenlist[hydra.core.Field]]:
            return for_many_with_accessors((lambda path1, val1, field1: for_field_with_accessor(mk_accessor, val1, field1)), (lambda x: x), v1, v2)
        def for_pair_with_accessors(key_accessor: hydra.paths.SubtermStep, val_accessor: hydra.paths.SubtermStep, val: T1, kv: tuple[hydra.core.Term, hydra.core.Term]) -> tuple[T1, tuple[hydra.core.Term, hydra.core.Term]]:
            @lru_cache(1)
            def rk() -> tuple[T1, hydra.core.Term]:
                return recurse(hydra.lib.lists.concat2(path, (key_accessor,)), val, hydra.lib.pairs.first(kv))
            @lru_cache(1)
            def rv() -> tuple[T1, hydra.core.Term]:
                return recurse(hydra.lib.lists.concat2(path, (val_accessor,)), hydra.lib.pairs.first(rk()), hydra.lib.pairs.second(kv))
            return (hydra.lib.pairs.first(rv()), (hydra.lib.pairs.second(rk()), hydra.lib.pairs.second(rv())))
        def for_binding_with_accessor(val: T1, binding: hydra.core.Binding) -> tuple[T1, hydra.core.Binding]:
            @lru_cache(1)
            def r() -> tuple[T1, hydra.core.Term]:
                return recurse(hydra.lib.lists.concat2(path, (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepLetBinding(binding.name)),)), val, binding.term)
            return (hydra.lib.pairs.first(r()), hydra.core.Binding(binding.name, hydra.lib.pairs.second(r()), binding.type))
        @lru_cache(1)
        def dflt() -> tuple[T1, hydra.core.Term]:
            return (val0, term02)
        def _hoist_dflt_body_1(v1):
            match v1:
                case hydra.core.TermAnnotated(value=at):
                    return for_single_with_accessor(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(t, at.annotation)))), cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepAnnotatedBody()), val0, at.body)

                case hydra.core.TermApplication(value=a):
                    @lru_cache(1)
                    def rlhs() -> tuple[T1, hydra.core.Term]:
                        return recurse(hydra.lib.lists.concat2(path, (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepApplicationFunction()),)), val0, a.function)
                    @lru_cache(1)
                    def rrhs() -> tuple[T1, hydra.core.Term]:
                        return recurse(hydra.lib.lists.concat2(path, (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepApplicationArgument()),)), hydra.lib.pairs.first(rlhs()), a.argument)
                    return (hydra.lib.pairs.first(rrhs()), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(hydra.lib.pairs.second(rlhs()), hydra.lib.pairs.second(rrhs())))))

                case hydra.core.TermCases(value=cs):
                    @lru_cache(1)
                    def rmd() -> Maybe[tuple[T1, hydra.core.Term]]:
                        return hydra.lib.maybes.map((lambda def_: recurse(hydra.lib.lists.concat2(path, (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepUnionCasesDefault()),)), val0, def_)), cs.default)
                    @lru_cache(1)
                    def val1() -> T1:
                        return hydra.lib.maybes.maybe((lambda : val0), (lambda x1: hydra.lib.pairs.first(x1)), rmd())
                    @lru_cache(1)
                    def rcases() -> tuple[T1, frozenlist[hydra.core.Term]]:
                        return for_many_with_accessors(recurse, (lambda x: x), val1(), hydra.lib.lists.map((lambda f2: (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepUnionCasesBranch(f2.name)), f2.term)), cs.cases))
                    return (hydra.lib.pairs.first(rcases()), cast(hydra.core.Term, hydra.core.TermCases(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map((lambda x1: hydra.lib.pairs.second(x1)), rmd()), hydra.lib.lists.map((lambda ft: hydra.core.Field(hydra.lib.pairs.first(ft), hydra.lib.pairs.second(ft))), hydra.lib.lists.zip(hydra.lib.lists.map((lambda v1: v1.name), cs.cases), hydra.lib.pairs.second(rcases())))))))

                case hydra.core.TermEither(value=e):
                    return hydra.lib.eithers.either((lambda l: (rl := recurse(hydra.lib.lists.concat2(path, (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepSumTerm()),)), val0, l), (hydra.lib.pairs.first(rl), cast(hydra.core.Term, hydra.core.TermEither(Left(hydra.lib.pairs.second(rl))))))[1]), (lambda r: (rr := recurse(hydra.lib.lists.concat2(path, (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepSumTerm()),)), val0, r), (hydra.lib.pairs.first(rr), cast(hydra.core.Term, hydra.core.TermEither(Right(hydra.lib.pairs.second(rr))))))[1]), e)

                case hydra.core.TermLambda(value=l):
                    @lru_cache(1)
                    def rl() -> tuple[T1, hydra.core.Term]:
                        return recurse(hydra.lib.lists.concat2(path, (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepLambdaBody()),)), val0, l.body)
                    return (hydra.lib.pairs.first(rl()), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(l.parameter, l.domain, hydra.lib.pairs.second(rl())))))

                case hydra.core.TermLet(value=l):
                    @lru_cache(1)
                    def renv() -> tuple[T1, hydra.core.Term]:
                        return recurse(hydra.lib.lists.concat2(path, (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepLetBody()),)), val0, l.body)
                    @lru_cache(1)
                    def rbindings() -> tuple[T1, frozenlist[hydra.core.Binding]]:
                        return hydra.lib.lists.foldl((lambda r, binding: (rb := for_binding_with_accessor(hydra.lib.pairs.first(r), binding), (hydra.lib.pairs.first(rb), hydra.lib.lists.cons(hydra.lib.pairs.second(rb), hydra.lib.pairs.second(r))))[1]), (hydra.lib.pairs.first(renv()), ()), l.bindings)
                    return (hydra.lib.pairs.first(rbindings()), cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.reverse(hydra.lib.pairs.second(rbindings())), hydra.lib.pairs.second(renv())))))

                case hydra.core.TermList(value=els):
                    idx = 0
                    @lru_cache(1)
                    def rr() -> tuple[int, tuple[T1, frozenlist[hydra.core.Term]]]:
                        return hydra.lib.lists.foldl((lambda r, el: (r2 := recurse(hydra.lib.lists.concat2(path, (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepListElement(hydra.lib.pairs.first(r))),)), hydra.lib.pairs.first(hydra.lib.pairs.second(r)), el), (hydra.lib.math.add(hydra.lib.pairs.first(r), 1), (hydra.lib.pairs.first(r2), hydra.lib.lists.cons(hydra.lib.pairs.second(r2), hydra.lib.pairs.second(hydra.lib.pairs.second(r))))))[1]), (idx, (val0, ())), els)
                    return (hydra.lib.pairs.first(hydra.lib.pairs.second(rr())), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.reverse(hydra.lib.pairs.second(hydra.lib.pairs.second(rr()))))))

                case hydra.core.TermMap(value=m):
                    idx = 0
                    @lru_cache(1)
                    def rr() -> tuple[int, tuple[T1, frozenlist[tuple[hydra.core.Term, hydra.core.Term]]]]:
                        return hydra.lib.lists.foldl((lambda r, kv: (rk := recurse(hydra.lib.lists.concat2(path, (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepMapKey(hydra.lib.pairs.first(r))),)), hydra.lib.pairs.first(hydra.lib.pairs.second(r)), hydra.lib.pairs.first(kv)), rv := recurse(hydra.lib.lists.concat2(path, (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepMapValue(hydra.lib.pairs.first(r))),)), hydra.lib.pairs.first(rk), hydra.lib.pairs.second(kv)), (hydra.lib.math.add(hydra.lib.pairs.first(r), 1), (hydra.lib.pairs.first(rv), hydra.lib.lists.cons((hydra.lib.pairs.second(rk), hydra.lib.pairs.second(rv)), hydra.lib.pairs.second(hydra.lib.pairs.second(r))))))[2]), (idx, (val0, ())), hydra.lib.maps.to_list(m))
                    return (hydra.lib.pairs.first(hydra.lib.pairs.second(rr())), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(hydra.lib.lists.reverse(hydra.lib.pairs.second(hydra.lib.pairs.second(rr())))))))

                case hydra.core.TermMaybe(value=mt):
                    return hydra.lib.maybes.maybe((lambda : dflt()), (lambda t: for_single_with_accessor(recurse, (lambda t1: cast(hydra.core.Term, hydra.core.TermMaybe(Just(t1)))), cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepMaybeTerm()), val0, t)), mt)

                case hydra.core.TermPair(value=p):
                    @lru_cache(1)
                    def rf() -> tuple[T1, hydra.core.Term]:
                        return recurse(hydra.lib.lists.concat2(path, (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepProductTerm(0)),)), val0, hydra.lib.pairs.first(p))
                    @lru_cache(1)
                    def rs() -> tuple[T1, hydra.core.Term]:
                        return recurse(hydra.lib.lists.concat2(path, (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepProductTerm(1)),)), hydra.lib.pairs.first(rf()), hydra.lib.pairs.second(p))
                    return (hydra.lib.pairs.first(rs()), cast(hydra.core.Term, hydra.core.TermPair((hydra.lib.pairs.second(rf()), hydra.lib.pairs.second(rs())))))

                case hydra.core.TermProject(value=p):
                    return (val0, cast(hydra.core.Term, hydra.core.TermProject(p)))

                case hydra.core.TermRecord(value=r):
                    @lru_cache(1)
                    def rfields() -> tuple[T1, frozenlist[hydra.core.Term]]:
                        return for_many_with_accessors(recurse, (lambda x: x), val0, hydra.lib.lists.map((lambda f2: (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepRecordField(f2.name)), f2.term)), r.fields))
                    return (hydra.lib.pairs.first(rfields()), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, hydra.lib.lists.map((lambda ft: hydra.core.Field(hydra.lib.pairs.first(ft), hydra.lib.pairs.second(ft))), hydra.lib.lists.zip(hydra.lib.lists.map((lambda v1: v1.name), r.fields), hydra.lib.pairs.second(rfields())))))))

                case hydra.core.TermSet(value=els):
                    idx = 0
                    @lru_cache(1)
                    def rr() -> tuple[int, tuple[T1, frozenlist[hydra.core.Term]]]:
                        return hydra.lib.lists.foldl((lambda r, el: (r2 := recurse(hydra.lib.lists.concat2(path, (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepSetElement(hydra.lib.pairs.first(r))),)), hydra.lib.pairs.first(hydra.lib.pairs.second(r)), el), (hydra.lib.math.add(hydra.lib.pairs.first(r), 1), (hydra.lib.pairs.first(r2), hydra.lib.lists.cons(hydra.lib.pairs.second(r2), hydra.lib.pairs.second(hydra.lib.pairs.second(r))))))[1]), (idx, (val0, ())), hydra.lib.sets.to_list(els))
                    return (hydra.lib.pairs.first(hydra.lib.pairs.second(rr())), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(hydra.lib.lists.reverse(hydra.lib.pairs.second(hydra.lib.pairs.second(rr())))))))

                case hydra.core.TermTypeApplication(value=ta):
                    return for_single_with_accessor(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, ta.type)))), cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepTypeApplicationTerm()), val0, ta.body)

                case hydra.core.TermTypeLambda(value=tl):
                    return for_single_with_accessor(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, t)))), cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepTypeLambdaBody()), val0, tl.body)

                case hydra.core.TermInject(value=inj):
                    return for_single_with_accessor(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(inj.type_name, hydra.core.Field(inj.field.name, t))))), cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepInjectionTerm()), val0, inj.field.term)

                case hydra.core.TermUnwrap(value=n):
                    return (val0, cast(hydra.core.Term, hydra.core.TermUnwrap(n)))

                case hydra.core.TermWrap(value=wt):
                    return for_single_with_accessor(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, t)))), cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepWrappedTerm()), val0, wt.body)

                case _:
                    return dflt()
        return _hoist_dflt_body_1(term02)
    def recurse(v1: frozenlist[hydra.paths.SubtermStep], v2: T0, v3: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
        return f((lambda v12, v22, v32: fsub((lambda x1, x2, x3: recurse(x1, x2, x3)), v12, v22, v32)), v1, v2, v3)
    return recurse((), term0, v1)

def rewrite_and_fold_term_with_graph_and_path(f: Callable[[
  Callable[[T0, hydra.core.Term], tuple[T0, hydra.core.Term]],
  frozenlist[hydra.paths.SubtermStep],
  hydra.graph.Graph,
  T0,
  hydra.core.Term], tuple[T0, hydra.core.Term]], cx0: hydra.graph.Graph, val0: T0, term0: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
    r"""Rewrite a term while folding to produce a value, with both Graph and accessor path tracked. The path is a list of SubtermSteps representing the position from the root to the current term. Combines the features of rewriteAndFoldTermWithPath and Graph tracking. The Graph is automatically updated when descending into lambdas, lets, and type lambdas."""

    def wrapper(recurse: Callable[[
      frozenlist[hydra.paths.SubtermStep],
      tuple[hydra.graph.Graph, T0],
      hydra.core.Term], tuple[tuple[T1, T0], hydra.core.Term]], path: frozenlist[hydra.paths.SubtermStep], cx_and_val: tuple[hydra.graph.Graph, T0], term: hydra.core.Term) -> tuple[tuple[hydra.graph.Graph, T0], hydra.core.Term]:
        @lru_cache(1)
        def cx() -> hydra.graph.Graph:
            return hydra.lib.pairs.first(cx_and_val)
        @lru_cache(1)
        def val() -> T0:
            return hydra.lib.pairs.second(cx_and_val)
        @lru_cache(1)
        def cx1():
            def _hoist_cx1_1(v1):
                match v1:
                    case hydra.core.TermLambda(value=l):
                        return hydra.scoping.extend_graph_for_lambda(cx(), l)

                    case hydra.core.TermLet(value=l):
                        return hydra.scoping.extend_graph_for_let((lambda _, _2: Nothing()), cx(), l)

                    case hydra.core.TermTypeLambda(value=tl):
                        return hydra.scoping.extend_graph_for_type_lambda(cx(), tl)

                    case _:
                        return cx()
            return _hoist_cx1_1(term)
        def recurse_for_user(val_in: T0, term_in: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
            @lru_cache(1)
            def result() -> tuple[tuple[T1, T0], hydra.core.Term]:
                return recurse(path, (cx1(), val_in), term_in)
            return (hydra.lib.pairs.second(hydra.lib.pairs.first(result())), hydra.lib.pairs.second(result()))
        @lru_cache(1)
        def f_result() -> tuple[T0, hydra.core.Term]:
            return f((lambda x1, x2: recurse_for_user(x1, x2)), path, cx1(), val(), term)
        return ((cx(), hydra.lib.pairs.first(f_result())), hydra.lib.pairs.second(f_result()))
    @lru_cache(1)
    def result() -> tuple[tuple[hydra.graph.Graph, T0], hydra.core.Term]:
        return rewrite_and_fold_term_with_path((lambda x1, x2, x3, x4: wrapper(x1, x2, x3, x4)), (cx0, val0), term0)
    return (hydra.lib.pairs.second(hydra.lib.pairs.first(result())), hydra.lib.pairs.second(result()))

def fold_term_with_graph_and_path(f: Callable[[
  Callable[[T0, hydra.core.Term], T0],
  frozenlist[hydra.paths.SubtermStep],
  hydra.graph.Graph,
  T0,
  hydra.core.Term], T0], cx0: hydra.graph.Graph, val0: T0, term0: hydra.core.Term) -> T0:
    r"""Fold over a term to produce a value, with both Graph and accessor path tracked. Like rewriteAndFoldTermWithGraphAndPath, but only folds without rewriting. The Graph is automatically updated when descending into lambdas, lets, and type lambdas."""

    def wrapper(recurse: Callable[[T0, hydra.core.Term], tuple[T0, T1]], path: frozenlist[hydra.paths.SubtermStep], cx: hydra.graph.Graph, val: T0, term: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
        def recurse_for_user(val_in: T0, subterm: hydra.core.Term) -> T0:
            @lru_cache(1)
            def r() -> tuple[T0, T1]:
                return recurse(val_in, subterm)
            return hydra.lib.pairs.first(r())
        return (f((lambda x1, x2: recurse_for_user(x1, x2)), path, cx, val, term), term)
    @lru_cache(1)
    def result() -> tuple[T0, hydra.core.Term]:
        return rewrite_and_fold_term_with_graph_and_path((lambda x1, x2, x3, x4, x5: wrapper(x1, x2, x3, x4, x5)), cx0, val0, term0)
    return hydra.lib.pairs.first(result())

def map_beneath_type_annotations(f: Callable[[hydra.core.Type], hydra.core.Type], t: hydra.core.Type) -> hydra.core.Type:
    r"""Apply a transformation to the first type beneath a chain of annotations."""

    match t:
        case hydra.core.TypeAnnotated(value=at):
            return cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(map_beneath_type_annotations(f, at.body), at.annotation)))

        case _:
            return f(t)

def rewrite_and_fold_term(f: Callable[[
  Callable[[T0, hydra.core.Term], tuple[T0, hydra.core.Term]],
  T0,
  hydra.core.Term], tuple[T0, hydra.core.Term]], term0: T0, v1: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
    r"""Rewrite a term, and at the same time, fold a function over it, accumulating a value."""

    def fsub(recurse: Callable[[T1, hydra.core.Term], tuple[T1, hydra.core.Term]], val0: T1, term02: hydra.core.Term):
        def for_single(rec: Callable[[T2, T3], tuple[T4, T5]], cons: Callable[[T5], T6], val: T2, term: T3) -> tuple[T4, T6]:
            @lru_cache(1)
            def r() -> tuple[T4, T5]:
                return rec(val, term)
            return (hydra.lib.pairs.first(r()), cons(hydra.lib.pairs.second(r())))
        def for_many(rec: Callable[[T2, T3], tuple[T2, T4]], cons: Callable[[frozenlist[T4]], T5], val: T2, els: frozenlist[T3]) -> tuple[T2, T5]:
            @lru_cache(1)
            def rr() -> tuple[T2, frozenlist[T4]]:
                return hydra.lib.lists.foldl((lambda r, el: (r2 := rec(hydra.lib.pairs.first(r), el), (hydra.lib.pairs.first(r2), hydra.lib.lists.cons(hydra.lib.pairs.second(r2), hydra.lib.pairs.second(r))))[1]), (val, ()), els)
            return (hydra.lib.pairs.first(rr()), cons(hydra.lib.lists.reverse(hydra.lib.pairs.second(rr()))))
        def for_field(val: T1, field: hydra.core.Field) -> tuple[T1, hydra.core.Field]:
            @lru_cache(1)
            def r() -> tuple[T1, hydra.core.Term]:
                return recurse(val, field.term)
            return (hydra.lib.pairs.first(r()), hydra.core.Field(field.name, hydra.lib.pairs.second(r())))
        def for_fields(v1: T1, v2: frozenlist[hydra.core.Field]) -> tuple[T1, frozenlist[hydra.core.Field]]:
            return for_many((lambda x1, x2: for_field(x1, x2)), (lambda x: x), v1, v2)
        def for_pair(val: T1, kv: tuple[hydra.core.Term, hydra.core.Term]) -> tuple[T1, tuple[hydra.core.Term, hydra.core.Term]]:
            @lru_cache(1)
            def rk() -> tuple[T1, hydra.core.Term]:
                return recurse(val, hydra.lib.pairs.first(kv))
            @lru_cache(1)
            def rv() -> tuple[T1, hydra.core.Term]:
                return recurse(hydra.lib.pairs.first(rk()), hydra.lib.pairs.second(kv))
            return (hydra.lib.pairs.first(rv()), (hydra.lib.pairs.second(rk()), hydra.lib.pairs.second(rv())))
        def for_binding(val: T1, binding: hydra.core.Binding) -> tuple[T1, hydra.core.Binding]:
            @lru_cache(1)
            def r() -> tuple[T1, hydra.core.Term]:
                return recurse(val, binding.term)
            return (hydra.lib.pairs.first(r()), hydra.core.Binding(binding.name, hydra.lib.pairs.second(r()), binding.type))
        @lru_cache(1)
        def dflt() -> tuple[T1, hydra.core.Term]:
            return (val0, term02)
        def _hoist_dflt_body_1(v1):
            match v1:
                case hydra.core.TermAnnotated(value=at):
                    return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(t, at.annotation)))), val0, at.body)

                case hydra.core.TermApplication(value=a):
                    @lru_cache(1)
                    def rlhs() -> tuple[T1, hydra.core.Term]:
                        return recurse(val0, a.function)
                    @lru_cache(1)
                    def rrhs() -> tuple[T1, hydra.core.Term]:
                        return recurse(hydra.lib.pairs.first(rlhs()), a.argument)
                    return (hydra.lib.pairs.first(rrhs()), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(hydra.lib.pairs.second(rlhs()), hydra.lib.pairs.second(rrhs())))))

                case hydra.core.TermCases(value=cs):
                    @lru_cache(1)
                    def rmd() -> Maybe[tuple[T1, hydra.core.Term]]:
                        return hydra.lib.maybes.map((lambda v12: recurse(val0, v12)), cs.default)
                    @lru_cache(1)
                    def val1() -> T1:
                        return hydra.lib.maybes.maybe((lambda : val0), (lambda x1: hydra.lib.pairs.first(x1)), rmd())
                    @lru_cache(1)
                    def rcases() -> tuple[T1, frozenlist[hydra.core.Field]]:
                        return for_fields(val1(), cs.cases)
                    return (hydra.lib.pairs.first(rcases()), cast(hydra.core.Term, hydra.core.TermCases(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map((lambda x1: hydra.lib.pairs.second(x1)), rmd()), hydra.lib.pairs.second(rcases())))))

                case hydra.core.TermEither(value=e):
                    return hydra.lib.eithers.either((lambda l: (rl := recurse(val0, l), (hydra.lib.pairs.first(rl), cast(hydra.core.Term, hydra.core.TermEither(Left(hydra.lib.pairs.second(rl))))))[1]), (lambda r: (rr := recurse(val0, r), (hydra.lib.pairs.first(rr), cast(hydra.core.Term, hydra.core.TermEither(Right(hydra.lib.pairs.second(rr))))))[1]), e)

                case hydra.core.TermLambda(value=l):
                    @lru_cache(1)
                    def rl() -> tuple[T1, hydra.core.Term]:
                        return recurse(val0, l.body)
                    return (hydra.lib.pairs.first(rl()), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(l.parameter, l.domain, hydra.lib.pairs.second(rl())))))

                case hydra.core.TermLet(value=l):
                    @lru_cache(1)
                    def renv() -> tuple[T1, hydra.core.Term]:
                        return recurse(val0, l.body)
                    return for_many((lambda x1, x2: for_binding(x1, x2)), (lambda bins: cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(bins, hydra.lib.pairs.second(renv()))))), hydra.lib.pairs.first(renv()), l.bindings)

                case hydra.core.TermList(value=els):
                    return for_many(recurse, (lambda x: cast(hydra.core.Term, hydra.core.TermList(x))), val0, els)

                case hydra.core.TermMap(value=m):
                    return for_many((lambda x1, x2: for_pair(x1, x2)), (lambda pairs: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(pairs)))), val0, hydra.lib.maps.to_list(m))

                case hydra.core.TermMaybe(value=mt):
                    return hydra.lib.maybes.maybe((lambda : dflt()), (lambda t: for_single(recurse, (lambda t1: cast(hydra.core.Term, hydra.core.TermMaybe(Just(t1)))), val0, t)), mt)

                case hydra.core.TermPair(value=p):
                    @lru_cache(1)
                    def rf() -> tuple[T1, hydra.core.Term]:
                        return recurse(val0, hydra.lib.pairs.first(p))
                    @lru_cache(1)
                    def rs() -> tuple[T1, hydra.core.Term]:
                        return recurse(hydra.lib.pairs.first(rf()), hydra.lib.pairs.second(p))
                    return (hydra.lib.pairs.first(rs()), cast(hydra.core.Term, hydra.core.TermPair((hydra.lib.pairs.second(rf()), hydra.lib.pairs.second(rs())))))

                case hydra.core.TermProject(value=p):
                    return (val0, cast(hydra.core.Term, hydra.core.TermProject(p)))

                case hydra.core.TermRecord(value=r):
                    return for_many((lambda x1, x2: for_field(x1, x2)), (lambda fields: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, fields)))), val0, r.fields)

                case hydra.core.TermSet(value=els):
                    return for_many(recurse, (lambda e: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(e)))), val0, hydra.lib.sets.to_list(els))

                case hydra.core.TermTypeApplication(value=ta):
                    return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, ta.type)))), val0, ta.body)

                case hydra.core.TermTypeLambda(value=tl):
                    return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, t)))), val0, tl.body)

                case hydra.core.TermInject(value=inj):
                    return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(inj.type_name, hydra.core.Field(inj.field.name, t))))), val0, inj.field.term)

                case hydra.core.TermUnwrap(value=n):
                    return (val0, cast(hydra.core.Term, hydra.core.TermUnwrap(n)))

                case hydra.core.TermWrap(value=wt):
                    return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, t)))), val0, wt.body)

                case _:
                    return dflt()
        return _hoist_dflt_body_1(term02)
    def recurse(v1: T0, v2: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
        return f((lambda v12, v22: fsub((lambda x1, x2: recurse(x1, x2)), v12, v22)), v1, v2)
    return recurse(term0, v1)

def rewrite_and_fold_term_with_graph(f: Callable[[
  Callable[[T0, hydra.core.Term], tuple[T0, hydra.core.Term]],
  hydra.graph.Graph,
  T0,
  hydra.core.Term], tuple[T0, hydra.core.Term]], cx0: hydra.graph.Graph, val0: T0, term0: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
    r"""Rewrite a term while folding to produce a value, with Graph updated as we descend into subterms. Combines the features of rewriteAndFoldTerm and rewriteTermWithGraph. The user function f receives a recurse function that handles subterm traversal and Graph management."""

    def wrapper(low_level_recurse: Callable[[tuple[T0, hydra.graph.Graph], hydra.core.Term], tuple[tuple[T0, T1], hydra.core.Term]], val_and_cx: tuple[T0, hydra.graph.Graph], term: hydra.core.Term) -> tuple[tuple[T0, hydra.graph.Graph], hydra.core.Term]:
        @lru_cache(1)
        def val() -> T0:
            return hydra.lib.pairs.first(val_and_cx)
        @lru_cache(1)
        def cx() -> hydra.graph.Graph:
            return hydra.lib.pairs.second(val_and_cx)
        @lru_cache(1)
        def cx1():
            def _hoist_cx1_1(v1):
                match v1:
                    case hydra.core.TermLambda(value=l):
                        return hydra.scoping.extend_graph_for_lambda(cx(), l)

                    case hydra.core.TermLet(value=l):
                        return hydra.scoping.extend_graph_for_let((lambda _, _2: Nothing()), cx(), l)

                    case hydra.core.TermTypeLambda(value=tl):
                        return hydra.scoping.extend_graph_for_type_lambda(cx(), tl)

                    case _:
                        return cx()
            return _hoist_cx1_1(term)
        def recurse_for_user(new_val: T0, subterm: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
            @lru_cache(1)
            def result() -> tuple[tuple[T0, T1], hydra.core.Term]:
                return low_level_recurse((new_val, cx1()), subterm)
            return (hydra.lib.pairs.first(hydra.lib.pairs.first(result())), hydra.lib.pairs.second(result()))
        @lru_cache(1)
        def f_result() -> tuple[T0, hydra.core.Term]:
            return f((lambda x1, x2: recurse_for_user(x1, x2)), cx1(), val(), term)
        return ((hydra.lib.pairs.first(f_result()), cx()), hydra.lib.pairs.second(f_result()))
    @lru_cache(1)
    def result() -> tuple[tuple[T0, hydra.graph.Graph], hydra.core.Term]:
        return rewrite_and_fold_term((lambda x1, x2, x3: wrapper(x1, x2, x3)), (val0, cx0), term0)
    return (hydra.lib.pairs.first(hydra.lib.pairs.first(result())), hydra.lib.pairs.second(result()))

def rewrite_term(f: Callable[[Callable[[hydra.core.Term], hydra.core.Term], hydra.core.Term], hydra.core.Term], term0: hydra.core.Term) -> hydra.core.Term:
    def fsub(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Term:
        def for_field(f2: hydra.core.Field) -> hydra.core.Field:
            return hydra.core.Field(f2.name, recurse(f2.term))
        def for_let(lt: hydra.core.Let) -> hydra.core.Let:
            def map_binding(b: hydra.core.Binding) -> hydra.core.Binding:
                return hydra.core.Binding(b.name, recurse(b.term), b.type)
            return hydra.core.Let(hydra.lib.lists.map((lambda x1: map_binding(x1)), lt.bindings), recurse(lt.body))
        def for_map(m: FrozenDict[hydra.core.Term, hydra.core.Term]) -> FrozenDict[hydra.core.Term, hydra.core.Term]:
            def for_pair(p: tuple[hydra.core.Term, hydra.core.Term]) -> tuple[hydra.core.Term, hydra.core.Term]:
                return (recurse(hydra.lib.pairs.first(p)), recurse(hydra.lib.pairs.second(p)))
            return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: for_pair(x1)), hydra.lib.maps.to_list(m)))
        match term:
            case hydra.core.TermAnnotated(value=at):
                return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(recurse(at.body), at.annotation)))

            case hydra.core.TermApplication(value=a):
                return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(recurse(a.function), recurse(a.argument))))

            case hydra.core.TermCases(value=cs):
                return cast(hydra.core.Term, hydra.core.TermCases(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map(recurse, cs.default), hydra.lib.lists.map((lambda x1: for_field(x1)), cs.cases))))

            case hydra.core.TermEither(value=e):
                return cast(hydra.core.Term, hydra.core.TermEither(hydra.lib.eithers.either((lambda l: Left(recurse(l))), (lambda r: Right(recurse(r))), e)))

            case hydra.core.TermLambda(value=l):
                return cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(l.parameter, l.domain, recurse(l.body))))

            case hydra.core.TermLet(value=lt):
                return cast(hydra.core.Term, hydra.core.TermLet(for_let(lt)))

            case hydra.core.TermList(value=els):
                return cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(recurse, els)))

            case hydra.core.TermLiteral(value=v):
                return cast(hydra.core.Term, hydra.core.TermLiteral(v))

            case hydra.core.TermMap(value=m):
                return cast(hydra.core.Term, hydra.core.TermMap(for_map(m)))

            case hydra.core.TermMaybe(value=m2):
                return cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(recurse, m2)))

            case hydra.core.TermPair(value=p):
                return cast(hydra.core.Term, hydra.core.TermPair((recurse(hydra.lib.pairs.first(p)), recurse(hydra.lib.pairs.second(p)))))

            case hydra.core.TermProject(value=p2):
                return cast(hydra.core.Term, hydra.core.TermProject(p2))

            case hydra.core.TermRecord(value=r):
                return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, hydra.lib.lists.map((lambda x1: for_field(x1)), r.fields))))

            case hydra.core.TermSet(value=s):
                return cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(hydra.lib.lists.map(recurse, hydra.lib.sets.to_list(s)))))

            case hydra.core.TermTypeApplication(value=tt):
                return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(recurse(tt.body), tt.type)))

            case hydra.core.TermTypeLambda(value=ta):
                return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(ta.parameter, recurse(ta.body))))

            case hydra.core.TermInject(value=i):
                return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(i.type_name, for_field(i.field))))

            case hydra.core.TermUnit():
                return cast(hydra.core.Term, hydra.core.TermUnit())

            case hydra.core.TermUnwrap(value=n):
                return cast(hydra.core.Term, hydra.core.TermUnwrap(n))

            case hydra.core.TermVariable(value=v2):
                return cast(hydra.core.Term, hydra.core.TermVariable(v2))

            case hydra.core.TermWrap(value=wt):
                return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, recurse(wt.body))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def recurse(v1: hydra.core.Term) -> hydra.core.Term:
        return f((lambda v12: fsub((lambda x1: recurse(x1)), v12)), v1)
    return recurse(term0)

def rewrite_term_m(f: Callable[[
  Callable[[hydra.core.Term], Either[T0, hydra.core.Term]],
  hydra.core.Term], Either[T0, hydra.core.Term]], term0: hydra.core.Term) -> Either[T0, hydra.core.Term]:
    r"""Either-based term rewriting with custom transformation function."""

    def fsub(recurse: Callable[[hydra.core.Term], Either[T1, hydra.core.Term]], term: hydra.core.Term):
        def for_field(field: hydra.core.Field) -> Either[T1, hydra.core.Field]:
            return hydra.lib.eithers.bind(recurse(field.term), (lambda t: Right(hydra.core.Field(field.name, t))))
        def for_pair(kv: tuple[hydra.core.Term, hydra.core.Term]) -> Either[T1, tuple[hydra.core.Term, hydra.core.Term]]:
            return hydra.lib.eithers.bind(recurse(hydra.lib.pairs.first(kv)), (lambda k: hydra.lib.eithers.bind(recurse(hydra.lib.pairs.second(kv)), (lambda v: Right((k, v))))))
        def map_binding(b: hydra.core.Binding) -> Either[T1, hydra.core.Binding]:
            return hydra.lib.eithers.bind(recurse(b.term), (lambda v: Right(hydra.core.Binding(b.name, v, b.type))))
        def _hoist_map_binding_body_1(v1):
            match v1:
                case hydra.core.TermAnnotated(value=at):
                    return hydra.lib.eithers.bind(recurse(at.body), (lambda ex: Right(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(ex, at.annotation))))))

                case hydra.core.TermApplication(value=app):
                    return hydra.lib.eithers.bind(recurse(app.function), (lambda lhs: hydra.lib.eithers.bind(recurse(app.argument), (lambda rhs: Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs, rhs))))))))

                case hydra.core.TermCases(value=cs):
                    n = cs.type_name
                    def_ = cs.default
                    cs_cases = cs.cases
                    return hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda t: hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), recurse(t))), def_), (lambda rdef: hydra.lib.eithers.map((lambda rcases: cast(hydra.core.Term, hydra.core.TermCases(hydra.core.CaseStatement(n, rdef, rcases)))), hydra.lib.eithers.map_list((lambda x1: for_field(x1)), cs_cases))))

                case hydra.core.TermEither(value=e):
                    return hydra.lib.eithers.bind(hydra.lib.eithers.either((lambda l: hydra.lib.eithers.map((lambda x: Left(x)), recurse(l))), (lambda r: hydra.lib.eithers.map((lambda x: Right(x)), recurse(r))), e), (lambda re: Right(cast(hydra.core.Term, hydra.core.TermEither(re)))))

                case hydra.core.TermLambda(value=l):
                    v = l.parameter
                    d = l.domain
                    body = l.body
                    return hydra.lib.eithers.bind(recurse(body), (lambda rbody: Right(cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(v, d, rbody))))))

                case hydra.core.TermLet(value=lt):
                    bindings = lt.bindings
                    env = lt.body
                    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: map_binding(x1)), bindings), (lambda rbindings: hydra.lib.eithers.bind(recurse(env), (lambda renv: Right(cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(rbindings, renv))))))))

                case hydra.core.TermList(value=els):
                    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list(recurse, els), (lambda rels: Right(cast(hydra.core.Term, hydra.core.TermList(rels)))))

                case hydra.core.TermLiteral(value=v):
                    return Right(cast(hydra.core.Term, hydra.core.TermLiteral(v)))

                case hydra.core.TermMap(value=m):
                    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: for_pair(x1)), hydra.lib.maps.to_list(m)), (lambda pairs: Right(cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(pairs))))))

                case hydra.core.TermMaybe(value=m):
                    return hydra.lib.eithers.bind(hydra.lib.eithers.map_maybe(recurse, m), (lambda rm: Right(cast(hydra.core.Term, hydra.core.TermMaybe(rm)))))

                case hydra.core.TermPair(value=p):
                    return hydra.lib.eithers.bind(recurse(hydra.lib.pairs.first(p)), (lambda rf: hydra.lib.eithers.bind(recurse(hydra.lib.pairs.second(p)), (lambda rs: Right(cast(hydra.core.Term, hydra.core.TermPair((rf, rs))))))))

                case hydra.core.TermProject(value=p):
                    return Right(cast(hydra.core.Term, hydra.core.TermProject(p)))

                case hydra.core.TermRecord(value=r):
                    n = r.type_name
                    fields = r.fields
                    return hydra.lib.eithers.map((lambda rfields: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(n, rfields)))), hydra.lib.eithers.map_list((lambda x1: for_field(x1)), fields))

                case hydra.core.TermSet(value=s):
                    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list(recurse, hydra.lib.sets.to_list(s)), (lambda rlist: Right(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(rlist))))))

                case hydra.core.TermTypeApplication(value=tt):
                    return hydra.lib.eithers.bind(recurse(tt.body), (lambda t: Right(cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, tt.type))))))

                case hydra.core.TermTypeLambda(value=tl):
                    v = tl.parameter
                    body = tl.body
                    return hydra.lib.eithers.bind(recurse(body), (lambda rbody: Right(cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(v, rbody))))))

                case hydra.core.TermInject(value=i):
                    n = i.type_name
                    field = i.field
                    return hydra.lib.eithers.map((lambda rfield: cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(n, rfield)))), for_field(field))

                case hydra.core.TermUnit():
                    return Right(cast(hydra.core.Term, hydra.core.TermUnit()))

                case hydra.core.TermUnwrap(value=n):
                    return Right(cast(hydra.core.Term, hydra.core.TermUnwrap(n)))

                case hydra.core.TermVariable(value=v):
                    return Right(cast(hydra.core.Term, hydra.core.TermVariable(v)))

                case hydra.core.TermWrap(value=wt):
                    name = wt.type_name
                    t = wt.body
                    return hydra.lib.eithers.bind(recurse(t), (lambda rt: Right(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(name, rt))))))

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        return _hoist_map_binding_body_1(term)
    def recurse(v1: hydra.core.Term) -> Either[T0, hydra.core.Term]:
        return f((lambda v12: fsub((lambda x1: recurse(x1)), v12)), v1)
    return recurse(term0)

def rewrite_term_with_context(f: Callable[[
  Callable[[T0, hydra.core.Term], hydra.core.Term],
  T0,
  hydra.core.Term], hydra.core.Term], cx0: T0, term0: hydra.core.Term) -> hydra.core.Term:
    r"""A variant of rewriteTerm which allows a context (e.g. a TypeContext) to be passed down to all subterms during rewriting."""

    def for_subterms(recurse0: Callable[[T1, hydra.core.Term], hydra.core.Term], cx: T1, term: hydra.core.Term):
        def recurse(v1: hydra.core.Term) -> hydra.core.Term:
            return recurse0(cx, v1)
        def for_field(field: hydra.core.Field) -> hydra.core.Field:
            return hydra.core.Field(field.name, recurse(field.term))
        def for_let(lt: hydra.core.Let) -> hydra.core.Let:
            def map_binding(b: hydra.core.Binding) -> hydra.core.Binding:
                return hydra.core.Binding(b.name, recurse(b.term), b.type)
            return hydra.core.Let(hydra.lib.lists.map((lambda x1: map_binding(x1)), lt.bindings), recurse(lt.body))
        def for_map(m: FrozenDict[hydra.core.Term, hydra.core.Term]) -> FrozenDict[hydra.core.Term, hydra.core.Term]:
            def for_pair(p: tuple[hydra.core.Term, hydra.core.Term]) -> tuple[hydra.core.Term, hydra.core.Term]:
                return (recurse(hydra.lib.pairs.first(p)), recurse(hydra.lib.pairs.second(p)))
            return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: for_pair(x1)), hydra.lib.maps.to_list(m)))
        def _hoist_for_map_body_1(v1):
            match v1:
                case hydra.core.TermAnnotated(value=at):
                    return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(recurse(at.body), at.annotation)))

                case hydra.core.TermApplication(value=a):
                    return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(recurse(a.function), recurse(a.argument))))

                case hydra.core.TermCases(value=cs):
                    return cast(hydra.core.Term, hydra.core.TermCases(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map((lambda x1: recurse(x1)), cs.default), hydra.lib.lists.map((lambda x1: for_field(x1)), cs.cases))))

                case hydra.core.TermEither(value=e):
                    return cast(hydra.core.Term, hydra.core.TermEither(hydra.lib.eithers.either((lambda l: Left(recurse(l))), (lambda r: Right(recurse(r))), e)))

                case hydra.core.TermLambda(value=l):
                    return cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(l.parameter, l.domain, recurse(l.body))))

                case hydra.core.TermLet(value=lt):
                    return cast(hydra.core.Term, hydra.core.TermLet(for_let(lt)))

                case hydra.core.TermList(value=els):
                    return cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: recurse(x1)), els)))

                case hydra.core.TermLiteral(value=v):
                    return cast(hydra.core.Term, hydra.core.TermLiteral(v))

                case hydra.core.TermMap(value=m):
                    return cast(hydra.core.Term, hydra.core.TermMap(for_map(m)))

                case hydra.core.TermMaybe(value=m):
                    return cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x1: recurse(x1)), m)))

                case hydra.core.TermPair(value=p):
                    return cast(hydra.core.Term, hydra.core.TermPair((recurse(hydra.lib.pairs.first(p)), recurse(hydra.lib.pairs.second(p)))))

                case hydra.core.TermProject(value=p):
                    return cast(hydra.core.Term, hydra.core.TermProject(p))

                case hydra.core.TermRecord(value=r):
                    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, hydra.lib.lists.map((lambda x1: for_field(x1)), r.fields))))

                case hydra.core.TermSet(value=s):
                    return cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(hydra.lib.lists.map((lambda x1: recurse(x1)), hydra.lib.sets.to_list(s)))))

                case hydra.core.TermTypeApplication(value=tt):
                    return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(recurse(tt.body), tt.type)))

                case hydra.core.TermTypeLambda(value=ta):
                    return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(ta.parameter, recurse(ta.body))))

                case hydra.core.TermInject(value=i):
                    return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(i.type_name, for_field(i.field))))

                case hydra.core.TermUnit():
                    return cast(hydra.core.Term, hydra.core.TermUnit())

                case hydra.core.TermUnwrap(value=n):
                    return cast(hydra.core.Term, hydra.core.TermUnwrap(n))

                case hydra.core.TermVariable(value=v):
                    return cast(hydra.core.Term, hydra.core.TermVariable(v))

                case hydra.core.TermWrap(value=wt):
                    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, recurse(wt.body))))

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        return _hoist_for_map_body_1(term)
    def rewrite(cx: T0, term: hydra.core.Term) -> hydra.core.Term:
        return f((lambda v1, v2: for_subterms((lambda x1, x2: rewrite(x1, x2)), v1, v2)), cx, term)
    return rewrite(cx0, term0)

def rewrite_term_with_context_m(f: Callable[[
  Callable[[T0, hydra.core.Term], Either[T1, hydra.core.Term]],
  T0,
  hydra.core.Term], Either[T1, hydra.core.Term]], cx0: T0, term0: hydra.core.Term) -> Either[T1, hydra.core.Term]:
    r"""Either-based variant of rewriteTermWithContextM which allows a context (e.g. a TypeContext) to be passed down to all subterms during rewriting."""

    def for_subterms(recurse0: Callable[[T2, hydra.core.Term], Either[T3, hydra.core.Term]], cx: T2, term: hydra.core.Term):
        def recurse(v1: hydra.core.Term) -> Either[T3, hydra.core.Term]:
            return recurse0(cx, v1)
        def for_field(field: hydra.core.Field) -> Either[T3, hydra.core.Field]:
            return hydra.lib.eithers.bind(recurse(field.term), (lambda t: Right(hydra.core.Field(field.name, t))))
        def for_pair(kv: tuple[hydra.core.Term, hydra.core.Term]) -> Either[T3, tuple[hydra.core.Term, hydra.core.Term]]:
            return hydra.lib.eithers.bind(recurse(hydra.lib.pairs.first(kv)), (lambda k: hydra.lib.eithers.bind(recurse(hydra.lib.pairs.second(kv)), (lambda v: Right((k, v))))))
        def map_binding(b: hydra.core.Binding) -> Either[T3, hydra.core.Binding]:
            return hydra.lib.eithers.bind(recurse(b.term), (lambda v: Right(hydra.core.Binding(b.name, v, b.type))))
        def _hoist_map_binding_body_1(v1):
            match v1:
                case hydra.core.TermAnnotated(value=at):
                    return hydra.lib.eithers.bind(recurse(at.body), (lambda ex: Right(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(ex, at.annotation))))))

                case hydra.core.TermApplication(value=app):
                    return hydra.lib.eithers.bind(recurse(app.function), (lambda lhs: hydra.lib.eithers.bind(recurse(app.argument), (lambda rhs: Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs, rhs))))))))

                case hydra.core.TermCases(value=cs):
                    n = cs.type_name
                    def_ = cs.default
                    cs_cases = cs.cases
                    return hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda t: hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), recurse(t))), def_), (lambda rdef: hydra.lib.eithers.map((lambda rcases: cast(hydra.core.Term, hydra.core.TermCases(hydra.core.CaseStatement(n, rdef, rcases)))), hydra.lib.eithers.map_list((lambda x1: for_field(x1)), cs_cases))))

                case hydra.core.TermEither(value=e):
                    return hydra.lib.eithers.bind(hydra.lib.eithers.either((lambda l: hydra.lib.eithers.map((lambda x: Left(x)), recurse(l))), (lambda r: hydra.lib.eithers.map((lambda x: Right(x)), recurse(r))), e), (lambda re: Right(cast(hydra.core.Term, hydra.core.TermEither(re)))))

                case hydra.core.TermLambda(value=l):
                    v = l.parameter
                    d = l.domain
                    body = l.body
                    return hydra.lib.eithers.bind(recurse(body), (lambda rbody: Right(cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(v, d, rbody))))))

                case hydra.core.TermLet(value=lt):
                    bindings = lt.bindings
                    body = lt.body
                    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: map_binding(x1)), bindings), (lambda rbindings: hydra.lib.eithers.bind(recurse(body), (lambda rbody: Right(cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(rbindings, rbody))))))))

                case hydra.core.TermList(value=els):
                    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: recurse(x1)), els), (lambda rels: Right(cast(hydra.core.Term, hydra.core.TermList(rels)))))

                case hydra.core.TermLiteral(value=v):
                    return Right(cast(hydra.core.Term, hydra.core.TermLiteral(v)))

                case hydra.core.TermMap(value=m):
                    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: for_pair(x1)), hydra.lib.maps.to_list(m)), (lambda pairs: Right(cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(pairs))))))

                case hydra.core.TermMaybe(value=m):
                    return hydra.lib.eithers.bind(hydra.lib.eithers.map_maybe((lambda x1: recurse(x1)), m), (lambda rm: Right(cast(hydra.core.Term, hydra.core.TermMaybe(rm)))))

                case hydra.core.TermPair(value=p):
                    return hydra.lib.eithers.bind(recurse(hydra.lib.pairs.first(p)), (lambda rfirst: hydra.lib.eithers.bind(recurse(hydra.lib.pairs.second(p)), (lambda rsecond: Right(cast(hydra.core.Term, hydra.core.TermPair((rfirst, rsecond))))))))

                case hydra.core.TermProject(value=p):
                    return Right(cast(hydra.core.Term, hydra.core.TermProject(p)))

                case hydra.core.TermRecord(value=r):
                    n = r.type_name
                    fields = r.fields
                    return hydra.lib.eithers.map((lambda rfields: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(n, rfields)))), hydra.lib.eithers.map_list((lambda x1: for_field(x1)), fields))

                case hydra.core.TermSet(value=s):
                    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: recurse(x1)), hydra.lib.sets.to_list(s)), (lambda rlist: Right(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(rlist))))))

                case hydra.core.TermTypeApplication(value=tt):
                    return hydra.lib.eithers.bind(recurse(tt.body), (lambda t: Right(cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, tt.type))))))

                case hydra.core.TermTypeLambda(value=tl):
                    v = tl.parameter
                    body = tl.body
                    return hydra.lib.eithers.bind(recurse(body), (lambda rbody: Right(cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(v, rbody))))))

                case hydra.core.TermInject(value=i):
                    n = i.type_name
                    field = i.field
                    return hydra.lib.eithers.map((lambda rfield: cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(n, rfield)))), for_field(field))

                case hydra.core.TermUnit():
                    return Right(cast(hydra.core.Term, hydra.core.TermUnit()))

                case hydra.core.TermUnwrap(value=n):
                    return Right(cast(hydra.core.Term, hydra.core.TermUnwrap(n)))

                case hydra.core.TermVariable(value=v):
                    return Right(cast(hydra.core.Term, hydra.core.TermVariable(v)))

                case hydra.core.TermWrap(value=wt):
                    name = wt.type_name
                    t = wt.body
                    return hydra.lib.eithers.bind(recurse(t), (lambda rt: Right(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(name, rt))))))

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        return _hoist_map_binding_body_1(term)
    def rewrite(cx: T0, term: hydra.core.Term) -> Either[T1, hydra.core.Term]:
        return f((lambda v1, v2: for_subterms((lambda x1, x2: rewrite(x1, x2)), v1, v2)), cx, term)
    return rewrite(cx0, term0)

def rewrite_term_with_graph(f: Callable[[Callable[[hydra.core.Term], T0], hydra.graph.Graph, hydra.core.Term], T0], cx0: hydra.graph.Graph, term0: hydra.core.Term) -> T0:
    r"""Rewrite a term with the help of a Graph which is updated as we descend into subterms."""

    def f2(recurse: Callable[[hydra.graph.Graph, hydra.core.Term], T0], cx: hydra.graph.Graph, term: hydra.core.Term) -> T0:
        def recurse1(term2: hydra.core.Term) -> T0:
            return recurse(cx, term2)
        match term:
            case hydra.core.TermLambda(value=l):
                @lru_cache(1)
                def cx1() -> hydra.graph.Graph:
                    return hydra.scoping.extend_graph_for_lambda(cx, l)
                def recurse2(term2: hydra.core.Term) -> T0:
                    return recurse(cx1(), term2)
                return f((lambda x1: recurse2(x1)), cx1(), term)

            case hydra.core.TermLet(value=l2):
                @lru_cache(1)
                def cx1() -> hydra.graph.Graph:
                    return hydra.scoping.extend_graph_for_let((lambda _, _2: Nothing()), cx, l2)
                def recurse2(term2: hydra.core.Term) -> T0:
                    return recurse(cx1(), term2)
                return f((lambda x1: recurse2(x1)), cx1(), term)

            case hydra.core.TermTypeLambda(value=tl):
                @lru_cache(1)
                def cx1() -> hydra.graph.Graph:
                    return hydra.scoping.extend_graph_for_type_lambda(cx, tl)
                def recurse2(term2: hydra.core.Term) -> T0:
                    return recurse(cx1(), term2)
                return f((lambda x1: recurse2(x1)), cx1(), term)

            case _:
                return f((lambda x1: recurse1(x1)), cx, term)
    def rewrite(cx: hydra.graph.Graph, term: hydra.core.Term) -> T0:
        return f2((lambda x1, x2: rewrite(x1, x2)), cx, term)
    return rewrite(cx0, term0)

def rewrite_type(f: Callable[[Callable[[hydra.core.Type], hydra.core.Type], hydra.core.Type], hydra.core.Type], typ0: hydra.core.Type) -> hydra.core.Type:
    def fsub(recurse: Callable[[hydra.core.Type], hydra.core.Type], typ: hydra.core.Type) -> hydra.core.Type:
        def for_field(field: hydra.core.FieldType) -> hydra.core.FieldType:
            return hydra.core.FieldType(field.name, recurse(field.type))
        match typ:
            case hydra.core.TypeAnnotated(value=at):
                return cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(recurse(at.body), at.annotation)))

            case hydra.core.TypeApplication(value=app):
                return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(recurse(app.function), recurse(app.argument))))

            case hydra.core.TypeEither(value=et):
                return cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(recurse(et.left), recurse(et.right))))

            case hydra.core.TypePair(value=pt):
                return cast(hydra.core.Type, hydra.core.TypePair(hydra.core.PairType(recurse(pt.first), recurse(pt.second))))

            case hydra.core.TypeFunction(value=fun):
                return cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(recurse(fun.domain), recurse(fun.codomain))))

            case hydra.core.TypeForall(value=lt):
                return cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(lt.parameter, recurse(lt.body))))

            case hydra.core.TypeList(value=t):
                return cast(hydra.core.Type, hydra.core.TypeList(recurse(t)))

            case hydra.core.TypeLiteral(value=lt2):
                return cast(hydra.core.Type, hydra.core.TypeLiteral(lt2))

            case hydra.core.TypeMap(value=mt):
                return cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(recurse(mt.keys), recurse(mt.values))))

            case hydra.core.TypeMaybe(value=t2):
                return cast(hydra.core.Type, hydra.core.TypeMaybe(recurse(t2)))

            case hydra.core.TypeRecord(value=rt):
                return cast(hydra.core.Type, hydra.core.TypeRecord(hydra.lib.lists.map((lambda x1: for_field(x1)), rt)))

            case hydra.core.TypeSet(value=t3):
                return cast(hydra.core.Type, hydra.core.TypeSet(recurse(t3)))

            case hydra.core.TypeUnion(value=rt2):
                return cast(hydra.core.Type, hydra.core.TypeUnion(hydra.lib.lists.map((lambda x1: for_field(x1)), rt2)))

            case hydra.core.TypeUnit():
                return cast(hydra.core.Type, hydra.core.TypeUnit())

            case hydra.core.TypeVariable(value=v):
                return cast(hydra.core.Type, hydra.core.TypeVariable(v))

            case hydra.core.TypeVoid():
                return cast(hydra.core.Type, hydra.core.TypeVoid())

            case hydra.core.TypeWrap(value=wt):
                return cast(hydra.core.Type, hydra.core.TypeWrap(recurse(wt)))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def recurse(v1: hydra.core.Type) -> hydra.core.Type:
        return f((lambda v12: fsub((lambda x1: recurse(x1)), v12)), v1)
    return recurse(typ0)

def rewrite_type_m(f: Callable[[
  Callable[[hydra.core.Type], Either[T0, hydra.core.Type]],
  hydra.core.Type], Either[T0, hydra.core.Type]], typ0: hydra.core.Type) -> Either[T0, hydra.core.Type]:
    r"""Either-based type rewriting."""

    def fsub(recurse: Callable[[hydra.core.Type], Either[T1, hydra.core.Type]], typ: hydra.core.Type) -> Either[T1, hydra.core.Type]:
        match typ:
            case hydra.core.TypeAnnotated(value=at):
                return hydra.lib.eithers.bind(recurse(at.body), (lambda t: Right(cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(t, at.annotation))))))

            case hydra.core.TypeApplication(value=at2):
                return hydra.lib.eithers.bind(recurse(at2.function), (lambda lhs: hydra.lib.eithers.bind(recurse(at2.argument), (lambda rhs: Right(cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(lhs, rhs))))))))

            case hydra.core.TypeEither(value=et):
                return hydra.lib.eithers.bind(recurse(et.left), (lambda left: hydra.lib.eithers.bind(recurse(et.right), (lambda right: Right(cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(left, right))))))))

            case hydra.core.TypePair(value=pt):
                return hydra.lib.eithers.bind(recurse(pt.first), (lambda pair_first: hydra.lib.eithers.bind(recurse(pt.second), (lambda pair_second: Right(cast(hydra.core.Type, hydra.core.TypePair(hydra.core.PairType(pair_first, pair_second))))))))

            case hydra.core.TypeFunction(value=ft):
                return hydra.lib.eithers.bind(recurse(ft.domain), (lambda dom: hydra.lib.eithers.bind(recurse(ft.codomain), (lambda cod: Right(cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(dom, cod))))))))

            case hydra.core.TypeForall(value=ft2):
                return hydra.lib.eithers.bind(recurse(ft2.body), (lambda b: Right(cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(ft2.parameter, b))))))

            case hydra.core.TypeList(value=t):
                return hydra.lib.eithers.bind(recurse(t), (lambda rt: Right(cast(hydra.core.Type, hydra.core.TypeList(rt)))))

            case hydra.core.TypeLiteral(value=lt):
                return Right(cast(hydra.core.Type, hydra.core.TypeLiteral(lt)))

            case hydra.core.TypeMap(value=mt):
                return hydra.lib.eithers.bind(recurse(mt.keys), (lambda kt: hydra.lib.eithers.bind(recurse(mt.values), (lambda vt: Right(cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(kt, vt))))))))

            case hydra.core.TypeMaybe(value=t2):
                return hydra.lib.eithers.bind(recurse(t2), (lambda rt: Right(cast(hydra.core.Type, hydra.core.TypeMaybe(rt)))))

            case hydra.core.TypeRecord(value=rt):
                def for_field(f2: hydra.core.FieldType) -> Either[T1, hydra.core.FieldType]:
                    return hydra.lib.eithers.bind(recurse(f2.type), (lambda t: Right(hydra.core.FieldType(f2.name, t))))
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: for_field(x1)), rt), (lambda rfields: Right(cast(hydra.core.Type, hydra.core.TypeRecord(rfields)))))

            case hydra.core.TypeSet(value=t3):
                return hydra.lib.eithers.bind(recurse(t3), (lambda rt: Right(cast(hydra.core.Type, hydra.core.TypeSet(rt)))))

            case hydra.core.TypeUnion(value=rt2):
                def for_field(f2: hydra.core.FieldType) -> Either[T1, hydra.core.FieldType]:
                    return hydra.lib.eithers.bind(recurse(f2.type), (lambda t: Right(hydra.core.FieldType(f2.name, t))))
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: for_field(x1)), rt2), (lambda rfields: Right(cast(hydra.core.Type, hydra.core.TypeUnion(rfields)))))

            case hydra.core.TypeUnit():
                return Right(cast(hydra.core.Type, hydra.core.TypeUnit()))

            case hydra.core.TypeVariable(value=v):
                return Right(cast(hydra.core.Type, hydra.core.TypeVariable(v)))

            case hydra.core.TypeVoid():
                return Right(cast(hydra.core.Type, hydra.core.TypeVoid()))

            case hydra.core.TypeWrap(value=wt):
                return hydra.lib.eithers.bind(recurse(wt), (lambda t: Right(cast(hydra.core.Type, hydra.core.TypeWrap(t)))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def recurse(v1: hydra.core.Type) -> Either[T0, hydra.core.Type]:
        return f((lambda v12: fsub((lambda x1: recurse(x1)), v12)), v1)
    return recurse(typ0)

def subterms_with_steps(v1: hydra.core.Term) -> frozenlist[tuple[hydra.paths.SubtermStep, hydra.core.Term]]:
    r"""Find the children of a given term."""

    match v1:
        case hydra.core.TermAnnotated(value=at):
            return ((cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepAnnotatedBody()), at.body),)

        case hydra.core.TermApplication(value=p):
            return ((cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepApplicationFunction()), p.function), (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepApplicationArgument()), p.argument))

        case hydra.core.TermCases(value=cs):
            return hydra.lib.lists.concat2(hydra.lib.maybes.maybe((lambda : ()), (lambda t: ((cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepUnionCasesDefault()), t),)), cs.default), hydra.lib.lists.map((lambda f: (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepUnionCasesBranch(f.name)), f.term)), cs.cases))

        case hydra.core.TermEither():
            return ()

        case hydra.core.TermLambda(value=l):
            return ((cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepLambdaBody()), l.body),)

        case hydra.core.TermLet(value=lt):
            return hydra.lib.lists.cons((cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepLetBody()), lt.body), hydra.lib.lists.map((lambda b: (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepLetBinding(b.name)), b.term)), lt.bindings))

        case hydra.core.TermList(value=l2):
            return hydra.lib.lists.map((lambda e: (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepListElement(0)), e)), l2)

        case hydra.core.TermLiteral():
            return ()

        case hydra.core.TermMap(value=m):
            return hydra.lib.lists.concat(hydra.lib.lists.map((lambda p: ((cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepMapKey(0)), hydra.lib.pairs.first(p)), (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepMapValue(0)), hydra.lib.pairs.second(p)))), hydra.lib.maps.to_list(m)))

        case hydra.core.TermMaybe(value=m2):
            return hydra.lib.maybes.maybe((lambda : ()), (lambda t: ((cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepMaybeTerm()), t),)), m2)

        case hydra.core.TermPair():
            return ()

        case hydra.core.TermProject():
            return ()

        case hydra.core.TermRecord(value=rt):
            return hydra.lib.lists.map((lambda f: (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepRecordField(f.name)), f.term)), rt.fields)

        case hydra.core.TermSet(value=s):
            return hydra.lib.lists.map((lambda e: (cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepListElement(0)), e)), hydra.lib.sets.to_list(s))

        case hydra.core.TermTypeApplication(value=ta):
            return ((cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepTypeApplicationTerm()), ta.body),)

        case hydra.core.TermTypeLambda(value=ta2):
            return ((cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepTypeLambdaBody()), ta2.body),)

        case hydra.core.TermInject(value=ut):
            return ((cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepInjectionTerm()), ut.field.term),)

        case hydra.core.TermUnit():
            return ()

        case hydra.core.TermUnwrap():
            return ()

        case hydra.core.TermVariable():
            return ()

        case hydra.core.TermWrap(value=n):
            return ((cast(hydra.paths.SubtermStep, hydra.paths.SubtermStepWrappedTerm()), n.body),)

        case _:
            raise AssertionError("Unreachable: all variants handled")
