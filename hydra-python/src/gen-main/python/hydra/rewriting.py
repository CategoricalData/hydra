# Note: this is an automatically generated file. Do not edit.

r"""Utilities for type and term rewriting and analysis."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.accessors
import hydra.coders
import hydra.core
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
import hydra.sorting

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")
T4 = TypeVar("T4")
T5 = TypeVar("T5")
T6 = TypeVar("T6")
X = TypeVar("X")

def apply_inside_type_lambdas_and_annotations(f: Callable[[hydra.core.Term], hydra.core.Term], term0: hydra.core.Term) -> hydra.core.Term:
    r"""Apply a term-level function inside any leading type lambdas."""

    match term0:
        case hydra.core.TermAnnotated(value=at):
            return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(apply_inside_type_lambdas_and_annotations(f, at.body), at.annotation)))

        case hydra.core.TermTypeLambda(value=tl):
            return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, apply_inside_type_lambdas_and_annotations(f, tl.body))))

        case _:
            return f(term0)

def deannotate_and_detype_term(t: hydra.core.Term) -> hydra.core.Term:
    r"""Strip type annotations from the top levels of a term."""

    while True:
        match t:
            case hydra.core.TermAnnotated(value=at):
                t = at.body
                continue

            case hydra.core.TermTypeApplication(value=tt):
                t = tt.body
                continue

            case hydra.core.TermTypeLambda(value=ta):
                t = ta.body
                continue

            case _:
                return t

def deannotate_term(t: hydra.core.Term) -> hydra.core.Term:
    r"""Strip all annotations (including System F type annotations) from the top levels of a term."""

    while True:
        match t:
            case hydra.core.TermAnnotated(value=at):
                t = at.body
                continue

            case _:
                return t

def deannotate_type(t: hydra.core.Type) -> hydra.core.Type:
    r"""Strip all annotations from a term."""

    while True:
        match t:
            case hydra.core.TypeAnnotated(value=arg_):
                t = arg_.body
                continue

            case _:
                return t

def deannotate_type_parameters(t: hydra.core.Type) -> hydra.core.Type:
    r"""Strip any top-level type lambdas from a type, extracting the (possibly nested) type body."""

    while True:
        match deannotate_type(t):
            case hydra.core.TypeForall(value=lt):
                t = lt.body
                continue

            case _:
                return t

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

            case hydra.core.TypeWrap(value=wt):
                return cast(hydra.core.Type, hydra.core.TypeWrap(recurse(wt)))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def recurse(v1: hydra.core.Type) -> hydra.core.Type:
        return f((lambda v12: fsub((lambda x1: recurse(x1)), v12)), v1)
    return recurse(typ0)

def deannotate_type_recursive(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Recursively strip all annotations from a type."""

    def strip(recurse: Callable[[T0], hydra.core.Type], typ2: T0):
        @lru_cache(1)
        def rewritten() -> hydra.core.Type:
            return recurse(typ2)
        def _hoist_body_1(v1):
            match v1:
                case hydra.core.TypeAnnotated(value=at):
                    return at.body

                case _:
                    return rewritten()
        return _hoist_body_1(rewritten())
    return rewrite_type((lambda x1, x2: strip(x1, x2)), typ)

def deannotate_type_scheme_recursive(ts: hydra.core.TypeScheme) -> hydra.core.TypeScheme:
    r"""Recursively strip all annotations from a type scheme."""

    vars = ts.variables
    typ = ts.type
    constraints = ts.constraints
    return hydra.core.TypeScheme(vars, deannotate_type_recursive(typ), constraints)

def detype_term(t: hydra.core.Term) -> hydra.core.Term:
    r"""Strip System F type annotations from the top levels of a term, but leave application-specific annotations intact."""

    match t:
        case hydra.core.TermAnnotated(value=at):
            subj = at.body
            ann = at.annotation
            return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(detype_term(subj), ann)))

        case hydra.core.TermTypeApplication(value=tt):
            return deannotate_and_detype_term(tt.body)

        case hydra.core.TermTypeLambda(value=ta):
            return deannotate_and_detype_term(ta.body)

        case _:
            return t

def f_type_to_type_scheme(typ: hydra.core.Type) -> hydra.core.TypeScheme:
    r"""Convert a forall type to a type scheme."""

    def gather_forall(vars: frozenlist[hydra.core.Name], typ2: hydra.core.Type) -> hydra.core.TypeScheme:
        while True:
            match deannotate_type(typ2):
                case hydra.core.TypeForall(value=ft):
                    vars = hydra.lib.lists.cons(ft.parameter, vars)
                    typ2 = ft.body
                    continue

                case _:
                    return hydra.core.TypeScheme(hydra.lib.lists.reverse(vars), typ2, Nothing())
    return gather_forall((), typ)

def rewrite_term(f: Callable[[Callable[[hydra.core.Term], hydra.core.Term], hydra.core.Term], hydra.core.Term], term0: hydra.core.Term) -> hydra.core.Term:
    def fsub(recurse: Callable[[hydra.core.Term], hydra.core.Term], term: hydra.core.Term) -> hydra.core.Term:
        def for_field(f2: hydra.core.Field) -> hydra.core.Field:
            return hydra.core.Field(f2.name, recurse(f2.term))
        def for_elimination(elm: hydra.core.Elimination) -> hydra.core.Elimination:
            match elm:
                case hydra.core.EliminationRecord(value=p):
                    return cast(hydra.core.Elimination, hydra.core.EliminationRecord(p))

                case hydra.core.EliminationUnion(value=cs):
                    return cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map(recurse, cs.default), hydra.lib.lists.map((lambda x1: for_field(x1)), cs.cases))))

                case hydra.core.EliminationWrap(value=name):
                    return cast(hydra.core.Elimination, hydra.core.EliminationWrap(name))

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def for_function(fun: hydra.core.Function) -> hydra.core.Function:
            match fun:
                case hydra.core.FunctionElimination(value=elm):
                    return cast(hydra.core.Function, hydra.core.FunctionElimination(for_elimination(elm)))

                case hydra.core.FunctionLambda(value=l):
                    return cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, recurse(l.body))))

                case hydra.core.FunctionPrimitive(value=name):
                    return cast(hydra.core.Function, hydra.core.FunctionPrimitive(name))

                case _:
                    raise AssertionError("Unreachable: all variants handled")
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

            case hydra.core.TermEither(value=e):
                return cast(hydra.core.Term, hydra.core.TermEither(hydra.lib.eithers.either((lambda l: Left(recurse(l))), (lambda r: Right(recurse(r))), e)))

            case hydra.core.TermFunction(value=fun):
                return cast(hydra.core.Term, hydra.core.TermFunction(for_function(fun)))

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

            case hydra.core.TermRecord(value=r):
                return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, hydra.lib.lists.map((lambda x1: for_field(x1)), r.fields))))

            case hydra.core.TermSet(value=s):
                return cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(hydra.lib.lists.map(recurse, hydra.lib.sets.to_list(s)))))

            case hydra.core.TermTypeApplication(value=tt):
                return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(recurse(tt.body), tt.type)))

            case hydra.core.TermTypeLambda(value=ta):
                return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(ta.parameter, recurse(ta.body))))

            case hydra.core.TermUnion(value=i):
                return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(i.type_name, for_field(i.field))))

            case hydra.core.TermUnit():
                return cast(hydra.core.Term, hydra.core.TermUnit())

            case hydra.core.TermVariable(value=v2):
                return cast(hydra.core.Term, hydra.core.TermVariable(v2))

            case hydra.core.TermWrap(value=wt):
                return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, recurse(wt.body))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def recurse(v1: hydra.core.Term) -> hydra.core.Term:
        return f((lambda v12: fsub((lambda x1: recurse(x1)), v12)), v1)
    return recurse(term0)

def substitute_variables(subst: FrozenDict[hydra.core.Name, hydra.core.Name], term: hydra.core.Term) -> hydra.core.Term:
    r"""Substitute multiple variables in a term."""

    def replace(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term):
        def _hoist_replace_1(recurse, term2, v1):
            match v1:
                case hydra.core.FunctionLambda(value=l):
                    return hydra.lib.maybes.maybe((lambda : recurse(term2)), (lambda _: term2), hydra.lib.maps.lookup(l.parameter, subst))

                case _:
                    return recurse(term2)
        match term2:
            case hydra.core.TermVariable(value=n):
                return cast(hydra.core.Term, hydra.core.TermVariable(hydra.lib.maybes.from_maybe((lambda : n), hydra.lib.maps.lookup(n, subst))))

            case hydra.core.TermFunction(value=_match_value):
                return _hoist_replace_1(recurse, term2, _match_value)

            case _:
                return recurse(term2)
    return rewrite_term((lambda x1, x2: replace(x1, x2)), term)

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
                    return substitute_variables(subst(), v1)
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
        def _hoist_body_1(v1):
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
        return _hoist_body_1(rewritten())
    return rewrite_term((lambda x1, x2: flatten(x1, x2)), term)

def subterms(v1: hydra.core.Term) -> frozenlist[hydra.core.Term]:
    r"""Find the children of a given term."""

    match v1:
        case hydra.core.TermAnnotated(value=at):
            return (at.body,)

        case hydra.core.TermApplication(value=p):
            return (p.function, p.argument)

        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: (l,)), (lambda r: (r,)), e)

        case hydra.core.TermFunction(value=v12):
            match v12:
                case hydra.core.FunctionElimination(value=v13):
                    match v13:
                        case hydra.core.EliminationUnion(value=cs):
                            return hydra.lib.lists.concat2(hydra.lib.maybes.maybe((lambda : ()), (lambda t: (t,)), cs.default), hydra.lib.lists.map((lambda v1: v1.term), cs.cases))

                        case _:
                            return ()

                case hydra.core.FunctionLambda(value=l):
                    return (l.body,)

                case _:
                    return ()

        case hydra.core.TermLet(value=lt):
            return hydra.lib.lists.cons(lt.body, hydra.lib.lists.map((lambda v1: v1.term), lt.bindings))

        case hydra.core.TermList(value=l):
            return l

        case hydra.core.TermLiteral():
            return ()

        case hydra.core.TermMap(value=m):
            return hydra.lib.lists.concat(hydra.lib.lists.map((lambda p: (hydra.lib.pairs.first(p), hydra.lib.pairs.second(p))), hydra.lib.maps.to_list(m)))

        case hydra.core.TermMaybe(value=m2):
            return hydra.lib.maybes.maybe((lambda : ()), (lambda t: (t,)), m2)

        case hydra.core.TermPair(value=p2):
            return (hydra.lib.pairs.first(p2), hydra.lib.pairs.second(p2))

        case hydra.core.TermRecord(value=rt):
            return hydra.lib.lists.map((lambda v1: v1.term), rt.fields)

        case hydra.core.TermSet(value=l2):
            return hydra.lib.sets.to_list(l2)

        case hydra.core.TermTypeApplication(value=ta):
            return (ta.body,)

        case hydra.core.TermTypeLambda(value=ta2):
            return (ta2.body,)

        case hydra.core.TermUnion(value=ut):
            return (ut.field.term,)

        case hydra.core.TermUnit():
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

def free_variables_in_type(typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    r"""Find the free variables (i.e. variables not bound by a lambda or let) in a type."""

    @lru_cache(1)
    def dflt_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.lists.foldl((lambda s, t: hydra.lib.sets.union(s, free_variables_in_type(t))), hydra.lib.sets.empty(), subtypes(typ))
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
    def get_all(vars: frozenset[hydra.core.Name], term: hydra.core.Term):
        def recurse(v1: hydra.core.Term) -> frozenset[hydra.core.Name]:
            return get_all(vars, v1)
        @lru_cache(1)
        def dflt() -> frozenset[hydra.core.Name]:
            return all_of(hydra.lib.lists.map((lambda x1: recurse(x1)), subterms(term)))
        def _hoist_body_1(v1):
            match v1:
                case hydra.core.FunctionElimination():
                    return dflt()

                case hydra.core.FunctionLambda(value=l):
                    @lru_cache(1)
                    def domt() -> frozenset[hydra.core.Name]:
                        return hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda v12: try_type(vars, v12)), l.domain)
                    return hydra.lib.sets.union(domt(), recurse(l.body))

                case _:
                    return dflt()
        match term:
            case hydra.core.TermFunction(value=f):
                return _hoist_body_1(f)

            case hydra.core.TermLet(value=l):
                def for_binding(b: hydra.core.Binding) -> frozenset[hydra.core.Name]:
                    @lru_cache(1)
                    def new_vars() -> frozenset[hydra.core.Name]:
                        return hydra.lib.maybes.maybe((lambda : vars), (lambda ts: hydra.lib.sets.union(vars, hydra.lib.sets.from_list(ts.variables))), b.type)
                    return hydra.lib.sets.union(get_all(new_vars(), b.term), hydra.lib.maybes.maybe((lambda : hydra.lib.sets.empty()), (lambda ts: try_type(new_vars(), ts.type)), b.type))
                return hydra.lib.sets.union(all_of(hydra.lib.lists.map((lambda x1: for_binding(x1)), l.bindings)), recurse(l.body))

            case hydra.core.TermTypeApplication(value=tt):
                return hydra.lib.sets.union(try_type(vars, tt.type), recurse(tt.body))

            case hydra.core.TermTypeLambda(value=tl):
                return hydra.lib.sets.union(try_type(vars, cast(hydra.core.Type, hydra.core.TypeVariable(tl.parameter))), recurse(tl.body))

            case _:
                return dflt()
    return get_all(hydra.lib.sets.empty(), term0)

def free_variables_in_term(term: hydra.core.Term):
    r"""Find the free variables (i.e. variables not bound by a lambda or let) in a term."""

    @lru_cache(1)
    def dflt_vars() -> frozenset[hydra.core.Name]:
        return hydra.lib.lists.foldl((lambda s, t: hydra.lib.sets.union(s, free_variables_in_term(t))), hydra.lib.sets.empty(), subterms(term))
    def _hoist_body_1(v1):
        match v1:
            case hydra.core.FunctionLambda(value=l):
                return hydra.lib.sets.delete(l.parameter, free_variables_in_term(l.body))

            case _:
                return dflt_vars()
    match term:
        case hydra.core.TermFunction(value=_match_value):
            return _hoist_body_1(_match_value)

        case hydra.core.TermLet(value=l):
            return hydra.lib.sets.difference(dflt_vars(), hydra.lib.sets.from_list(hydra.lib.lists.map((lambda v1: v1.name), l.bindings)))

        case hydra.core.TermVariable(value=v):
            return hydra.lib.sets.singleton(v)

        case _:
            return dflt_vars()

def free_variables_in_type_ordered(typ: hydra.core.Type) -> frozenlist[hydra.core.Name]:
    r"""Find the free variables in a type in deterministic left-to-right order."""

    def collect_vars(bound_vars: frozenset[hydra.core.Name], t: hydra.core.Type) -> frozenlist[hydra.core.Name]:
        match t:
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.logic.if_else(hydra.lib.sets.member(v, bound_vars), (lambda : ()), (lambda : (v,)))

            case hydra.core.TypeForall(value=ft):
                return collect_vars(hydra.lib.sets.insert(ft.parameter, bound_vars), ft.body)

            case _:
                return hydra.lib.lists.concat(hydra.lib.lists.map((lambda v1: collect_vars(bound_vars, v1)), subtypes(t)))
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
    return fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda x1, x2: helper(x1, x2)), hydra.lib.sets.empty(), typ)

def free_variables_in_type_scheme_simple(ts: hydra.core.TypeScheme) -> frozenset[hydra.core.Name]:
    r"""Find free variables in a type scheme (simple version)."""

    vars = ts.variables
    t = ts.type
    return hydra.lib.sets.difference(free_variables_in_type_simple(t), hydra.lib.sets.from_list(vars))

def rewrite_type_m(f: Callable[[
  Callable[[hydra.core.Type], Either[T0, hydra.core.Type]],
  hydra.core.Type], Either[T0, hydra.core.Type]], typ0: hydra.core.Type) -> Either[T0, hydra.core.Type]:
    r"""Either-based type rewriting."""

    def fsub(recurse: Callable[[hydra.core.Type], Either[T1, hydra.core.Type]], typ: hydra.core.Type) -> Either[hydra.core.Type, hydra.core.Type]:
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
                def for_field(f2: hydra.core.FieldType) -> Either[hydra.core.Type, hydra.core.FieldType]:
                    return hydra.lib.eithers.bind(recurse(f2.type), (lambda t: Right(hydra.core.FieldType(f2.name, t))))
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: for_field(x1)), rt), (lambda rfields: Right(cast(hydra.core.Type, hydra.core.TypeRecord(rfields)))))

            case hydra.core.TypeSet(value=t3):
                return hydra.lib.eithers.bind(recurse(t3), (lambda rt: Right(cast(hydra.core.Type, hydra.core.TypeSet(rt)))))

            case hydra.core.TypeUnion(value=rt2):
                def for_field(f2: hydra.core.FieldType) -> Either[hydra.core.Type, hydra.core.FieldType]:
                    return hydra.lib.eithers.bind(recurse(f2.type), (lambda t: Right(hydra.core.FieldType(f2.name, t))))
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: for_field(x1)), rt2), (lambda rfields: Right(cast(hydra.core.Type, hydra.core.TypeUnion(rfields)))))

            case hydra.core.TypeUnit():
                return Right(cast(hydra.core.Type, hydra.core.TypeUnit()))

            case hydra.core.TypeVariable(value=v):
                return Right(cast(hydra.core.Type, hydra.core.TypeVariable(v)))

            case hydra.core.TypeWrap(value=wt):
                return hydra.lib.eithers.bind(recurse(wt), (lambda t: Right(cast(hydra.core.Type, hydra.core.TypeWrap(t)))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def recurse(v1: hydra.core.Type) -> Either[T0, hydra.core.Type]:
        return f((lambda v12: fsub((lambda x1: recurse(x1)), v12)), v1)
    return recurse(typ0)

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
    return rewrite_type_m((lambda x1, x2: f(x1, x2)), typ)

def is_free_variable_in_term(v: hydra.core.Name, term: hydra.core.Term) -> bool:
    r"""Check whether a variable is free (not bound) in a term."""

    return hydra.lib.logic.not_(hydra.lib.sets.member(v, free_variables_in_term(term)))

def is_lambda(term: hydra.core.Term):
    while True:
        def _hoist_hydra_rewriting_is_lambda_1(v1):
            match v1:
                case hydra.core.FunctionLambda():
                    return True

                case _:
                    return False
        match deannotate_term(term):
            case hydra.core.TermFunction(value=_match_value):
                return _hoist_hydra_rewriting_is_lambda_1(_match_value)

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
    return rewrite_term((lambda x1, x2: rewrite(x1, x2)), term0)

def map_beneath_type_annotations(f: Callable[[hydra.core.Type], hydra.core.Type], t: hydra.core.Type) -> hydra.core.Type:
    r"""Apply a transformation to the first type beneath a chain of annotations."""

    match t:
        case hydra.core.TypeAnnotated(value=at):
            return cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(map_beneath_type_annotations(f, at.body), at.annotation)))

        case _:
            return f(t)

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
        return rewrite_type((lambda x1, x2: rewrite(x1, x2)), typ)
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
        def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term):
            def _hoist_rewrite_1(recurse, term2, v1):
                match v1:
                    case hydra.core.FunctionElimination():
                        return recurse(term2)

                    case hydra.core.FunctionLambda(value=l):
                        domain = l.domain
                        return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, hydra.lib.maybes.map((lambda v12: subst_type(subst(), v12)), domain), rewrite_with_subst(((subst(), bound_vars()), next()), l.body))))))

                    case _:
                        return recurse(term2)
            match term2:
                case hydra.core.TermFunction(value=_match_value):
                    return _hoist_rewrite_1(recurse, term2, _match_value)

                case hydra.core.TermLet(value=lt):
                    bindings0 = lt.bindings
                    body0 = lt.body
                    def step(acc: frozenlist[hydra.core.Binding], bs: frozenlist[hydra.core.Binding]) -> frozenlist[hydra.core.Binding]:
                        @lru_cache(1)
                        def b() -> hydra.core.Binding:
                            return hydra.lib.lists.head(bs)
                        @lru_cache(1)
                        def tl() -> frozenlist[hydra.core.Binding]:
                            return hydra.lib.lists.tail(bs)
                        @lru_cache(1)
                        def no_type() -> frozenlist[hydra.core.Binding]:
                            @lru_cache(1)
                            def new_val() -> hydra.core.Term:
                                return rewrite_with_subst(((subst(), bound_vars()), next()), b().term)
                            b1 = hydra.core.Binding(b().name, new_val(), Nothing())
                            return step(hydra.lib.lists.cons(b1, acc), tl())
                        def with_type(ts: hydra.core.TypeScheme) -> frozenlist[hydra.core.Binding]:
                            vars = ts.variables
                            typ = ts.type
                            @lru_cache(1)
                            def k() -> int:
                                return hydra.lib.lists.length(vars)
                            def gen(i: int, rem: int, acc2: frozenlist[hydra.core.Name]) -> frozenlist[hydra.core.Name]:
                                @lru_cache(1)
                                def ti() -> hydra.core.Name:
                                    return hydra.core.Name(hydra.lib.strings.cat2("t", hydra.lib.literals.show_int32(hydra.lib.math.add(next(), i))))
                                return hydra.lib.logic.if_else(hydra.lib.equality.equal(rem, 0), (lambda : hydra.lib.lists.reverse(acc2)), (lambda : gen(hydra.lib.math.add(i, 1), hydra.lib.math.sub(rem, 1), hydra.lib.lists.cons(ti(), acc2))))
                            @lru_cache(1)
                            def new_vars() -> frozenlist[hydra.core.Name]:
                                return gen(0, k(), ())
                            @lru_cache(1)
                            def new_subst() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                                return hydra.lib.maps.union(hydra.lib.maps.from_list(hydra.lib.lists.zip(vars, new_vars())), subst())
                            @lru_cache(1)
                            def new_bound() -> frozenset[hydra.core.Name]:
                                return hydra.lib.sets.union(bound_vars(), hydra.lib.sets.from_list(new_vars()))
                            @lru_cache(1)
                            def new_val() -> hydra.core.Term:
                                return rewrite_with_subst(((new_subst(), new_bound()), hydra.lib.math.add(next(), k())), b().term)
                            def rename_constraint_keys(constraint_map: FrozenDict[hydra.core.Name, T0]) -> FrozenDict[hydra.core.Name, T0]:
                                return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda p: (old_name := hydra.lib.pairs.first(p), meta := hydra.lib.pairs.second(p), new_name := hydra.lib.maybes.from_maybe((lambda : old_name), hydra.lib.maps.lookup(old_name, new_subst())), (new_name, meta))[3]), hydra.lib.maps.to_list(constraint_map)))
                            old_constraints = ts.constraints
                            @lru_cache(1)
                            def new_constraints() -> Maybe[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]]:
                                return hydra.lib.maybes.map((lambda x1: rename_constraint_keys(x1)), old_constraints)
                            @lru_cache(1)
                            def b1() -> hydra.core.Binding:
                                return hydra.core.Binding(b().name, new_val(), Just(hydra.core.TypeScheme(new_vars(), subst_type(new_subst(), typ), new_constraints())))
                            return step(hydra.lib.lists.cons(b1(), acc), tl())
                        return hydra.lib.logic.if_else(hydra.lib.lists.null(bs), (lambda : hydra.lib.lists.reverse(acc)), (lambda : hydra.lib.maybes.maybe((lambda : no_type()), (lambda ts: with_type(ts)), b().type)))
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
        return rewrite_term((lambda x1, x2: rewrite(x1, x2)), term0)
    return rewrite_with_subst(((hydra.lib.maps.empty(), hydra.lib.sets.empty()), 0), term)

def prune_let(l: hydra.core.Let) -> hydra.core.Let:
    r"""Given a let expression, remove any unused bindings. The resulting expression is still a let, even if has no remaining bindings."""

    @lru_cache(1)
    def binding_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda b: (b.name, b.term)), l.bindings))
    root_name = hydra.core.Name("[[[root]]]")
    def adj(n: hydra.core.Name) -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.intersection(hydra.lib.sets.from_list(hydra.lib.maps.keys(binding_map())), free_variables_in_term(hydra.lib.logic.if_else(hydra.lib.equality.equal(n, root_name), (lambda : l.body), (lambda : hydra.lib.maybes.from_just(hydra.lib.maps.lookup(n, binding_map()))))))
    @lru_cache(1)
    def reachable() -> frozenset[hydra.core.Name]:
        return hydra.sorting.find_reachable_nodes((lambda x1: adj(x1)), root_name)
    @lru_cache(1)
    def pruned_bindings() -> frozenlist[hydra.core.Binding]:
        return hydra.lib.lists.filter((lambda b: hydra.lib.sets.member(b.name, reachable())), l.bindings)
    return hydra.core.Let(pruned_bindings(), l.body)

def remove_term_annotations(term: hydra.core.Term) -> hydra.core.Term:
    r"""Recursively remove term annotations, including within subterms."""

    def remove(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term) -> hydra.core.Term:
        @lru_cache(1)
        def rewritten() -> hydra.core.Term:
            return recurse(term2)
        match term2:
            case hydra.core.TermAnnotated(value=at):
                return at.body

            case _:
                return rewritten()
    return rewrite_term((lambda x1, x2: remove(x1, x2)), term)

def remove_type_annotations(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Recursively remove type annotations, including within subtypes."""

    def remove(recurse: Callable[[T0], hydra.core.Type], typ2: T0):
        @lru_cache(1)
        def rewritten() -> hydra.core.Type:
            return recurse(typ2)
        def _hoist_body_1(v1):
            match v1:
                case hydra.core.TypeAnnotated(value=at):
                    return at.body

                case _:
                    return rewritten()
        return _hoist_body_1(rewritten())
    return rewrite_type((lambda x1, x2: remove(x1, x2)), typ)

def remove_type_annotations_from_term(term: hydra.core.Term) -> hydra.core.Term:
    r"""Strip type annotations (TypeLambda, TypeApplication, binding type schemes) from terms while preserving lambda domain types and other annotations."""

    def strip(recurse: Callable[[T0], hydra.core.Term], term2: T0):
        @lru_cache(1)
        def rewritten() -> hydra.core.Term:
            return recurse(term2)
        def strip_binding(b: hydra.core.Binding) -> hydra.core.Binding:
            return hydra.core.Binding(b.name, b.term, Nothing())
        def _hoist_body_1(v1):
            match v1:
                case hydra.core.TermLet(value=lt):
                    return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map((lambda x1: strip_binding(x1)), lt.bindings), lt.body)))

                case hydra.core.TermTypeApplication(value=tt):
                    return tt.body

                case hydra.core.TermTypeLambda(value=ta):
                    return ta.body

                case _:
                    return rewritten()
        return _hoist_body_1(rewritten())
    return rewrite_term((lambda x1, x2: strip(x1, x2)), term)

def remove_types_from_term(term: hydra.core.Term) -> hydra.core.Term:
    r"""Strip type annotations from terms while preserving other annotations."""

    def strip(recurse: Callable[[T0], hydra.core.Term], term2: T0):
        @lru_cache(1)
        def rewritten() -> hydra.core.Term:
            return recurse(term2)
        def strip_binding(b: hydra.core.Binding) -> hydra.core.Binding:
            return hydra.core.Binding(b.name, b.term, Nothing())
        def _hoist_body_1(f, v1):
            match v1:
                case hydra.core.FunctionElimination(value=e):
                    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(e))))

                case hydra.core.FunctionLambda(value=l):
                    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, Nothing(), l.body)))))

                case _:
                    return cast(hydra.core.Term, hydra.core.TermFunction(f))
        def _hoist_body_2(v1):
            match v1:
                case hydra.core.TermFunction(value=f):
                    return _hoist_body_1(f, f)

                case hydra.core.TermLet(value=lt):
                    return cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.map((lambda x1: strip_binding(x1)), lt.bindings), lt.body)))

                case hydra.core.TermTypeApplication(value=tt):
                    return tt.body

                case hydra.core.TermTypeLambda(value=ta):
                    return ta.body

                case _:
                    return rewritten()
        return _hoist_body_2(rewritten())
    return rewrite_term((lambda x1, x2: strip(x1, x2)), term)

def replace_free_term_variable(vold: hydra.core.Name, tnew: hydra.core.Term, term: hydra.core.Term) -> hydra.core.Term:
    r"""Replace a free variable in a term."""

    def rewrite(recurse: Callable[[hydra.core.Term], hydra.core.Term], t: hydra.core.Term):
        def _hoist_rewrite_1(recurse, t, v1):
            match v1:
                case hydra.core.FunctionLambda(value=l):
                    v = l.parameter
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, vold), (lambda : t), (lambda : recurse(t)))

                case _:
                    return recurse(t)
        match t:
            case hydra.core.TermFunction(value=f):
                return _hoist_rewrite_1(recurse, t, f)

            case hydra.core.TermVariable(value=v):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(v, vold), (lambda : tnew), (lambda : cast(hydra.core.Term, hydra.core.TermVariable(v))))

            case _:
                return recurse(t)
    return rewrite_term((lambda x1, x2: rewrite(x1, x2)), term)

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
    return rewrite_type((lambda x1, x2: map_expr(x1, x2)), typ)

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
    return rewrite_type((lambda x1, x2: rewrite(x1, x2)), typ0)

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
        def for_field(val: T1, field: hydra.core.Field) -> tuple[hydra.core.Term, hydra.core.Field]:
            @lru_cache(1)
            def r() -> tuple[T1, hydra.core.Term]:
                return recurse(val, field.term)
            return (hydra.lib.pairs.first(r()), hydra.core.Field(field.name, hydra.lib.pairs.second(r())))
        def for_fields(v1: T1, v2: frozenlist[hydra.core.Field]) -> tuple[T1, frozenlist[hydra.core.Field]]:
            return for_many((lambda x1, x2: for_field(x1, x2)), (lambda x: x), v1, v2)
        def for_pair(val: T1, kv: tuple[hydra.core.Term, hydra.core.Term]) -> tuple[hydra.core.Term, tuple[hydra.core.Term, hydra.core.Term]]:
            @lru_cache(1)
            def rk() -> tuple[T1, hydra.core.Term]:
                return recurse(val, hydra.lib.pairs.first(kv))
            @lru_cache(1)
            def rv() -> tuple[T1, hydra.core.Term]:
                return recurse(hydra.lib.pairs.first(rk()), hydra.lib.pairs.second(kv))
            return (hydra.lib.pairs.first(rv()), (hydra.lib.pairs.second(rk()), hydra.lib.pairs.second(rv())))
        def for_binding(val: T1, binding: hydra.core.Binding) -> tuple[hydra.core.Term, hydra.core.Binding]:
            @lru_cache(1)
            def r() -> tuple[T1, hydra.core.Term]:
                return recurse(val, binding.term)
            return (hydra.lib.pairs.first(r()), hydra.core.Binding(binding.name, hydra.lib.pairs.second(r()), binding.type))
        def for_elimination(val: T1, elm: hydra.core.Elimination) -> tuple[hydra.core.Elimination, hydra.core.Elimination]:
            @lru_cache(1)
            def r():
                def _hoist_r_1(v1):
                    match v1:
                        case hydra.core.EliminationUnion(value=cs):
                            @lru_cache(1)
                            def rmd() -> Maybe[tuple[T1, hydra.core.Term]]:
                                return hydra.lib.maybes.map((lambda v12: recurse(val, v12)), cs.default)
                            @lru_cache(1)
                            def val1() -> tuple[T1, hydra.core.Term]:
                                return hydra.lib.maybes.maybe((lambda : val), (lambda x1: hydra.lib.pairs.first(x1)), rmd())
                            @lru_cache(1)
                            def rcases() -> tuple[T1, frozenlist[hydra.core.Field]]:
                                return for_fields(val1(), cs.cases)
                            return (hydra.lib.pairs.first(rcases()), cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map((lambda x1: hydra.lib.pairs.second(x1)), rmd()), hydra.lib.pairs.second(rcases())))))

                        case _:
                            return (val, elm)
                return _hoist_r_1(elm)
            return (hydra.lib.pairs.first(r()), hydra.lib.pairs.second(r()))
        def for_function(val: T1, fun: hydra.core.Function):
            def _hoist_for_function_1(fun, val, v1):
                match v1:
                    case hydra.core.FunctionElimination(value=elm):
                        @lru_cache(1)
                        def re() -> tuple[T1, hydra.core.Elimination]:
                            return for_elimination(val, elm)
                        return (hydra.lib.pairs.first(re()), cast(hydra.core.Function, hydra.core.FunctionElimination(hydra.lib.pairs.second(re()))))

                    case hydra.core.FunctionLambda(value=l):
                        @lru_cache(1)
                        def rl() -> tuple[T1, hydra.core.Term]:
                            return recurse(val, l.body)
                        return (hydra.lib.pairs.first(rl()), cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, hydra.lib.pairs.second(rl())))))

                    case _:
                        return (val, fun)
            return _hoist_for_function_1(fun, val, fun)
        @lru_cache(1)
        def dflt() -> tuple[T1, hydra.core.Term]:
            return (val0, term02)
        def _hoist_body_1(v1):
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

                case hydra.core.TermEither(value=e):
                    return hydra.lib.eithers.either((lambda l: (rl := recurse(val0, l), (hydra.lib.pairs.first(rl), cast(hydra.core.Term, hydra.core.TermEither(Left(hydra.lib.pairs.second(rl))))))[1]), (lambda r: (rr := recurse(val0, r), (hydra.lib.pairs.first(rr), cast(hydra.core.Term, hydra.core.TermEither(Right(hydra.lib.pairs.second(rr))))))[1]), e)

                case hydra.core.TermFunction(value=f2):
                    return for_single((lambda x1, x2: for_function(x1, x2)), (lambda f3: cast(hydra.core.Term, hydra.core.TermFunction(f3))), val0, f2)

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

                case hydra.core.TermRecord(value=r):
                    return for_many((lambda x1, x2: for_field(x1, x2)), (lambda fields: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, fields)))), val0, r.fields)

                case hydra.core.TermSet(value=els):
                    return for_many(recurse, (lambda e: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(e)))), val0, hydra.lib.sets.to_list(els))

                case hydra.core.TermTypeApplication(value=ta):
                    return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, ta.type)))), val0, ta.body)

                case hydra.core.TermTypeLambda(value=tl):
                    return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, t)))), val0, tl.body)

                case hydra.core.TermUnion(value=inj):
                    return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(inj.type_name, hydra.core.Field(inj.field.name, t))))), val0, inj.field.term)

                case hydra.core.TermWrap(value=wt):
                    return for_single(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, t)))), val0, wt.body)

                case _:
                    return dflt()
        return _hoist_body_1(term02)
    def recurse(v1: T0, v2: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
        return f((lambda v12, v22: fsub((lambda x1, x2: recurse(x1, x2)), v12, v22)), v1, v2)
    return recurse(term0, v1)

def rewrite_and_fold_term_with_path(f: Callable[[
  Callable[[frozenlist[hydra.accessors.TermAccessor], T0, hydra.core.Term], tuple[T0, hydra.core.Term]],
  frozenlist[hydra.accessors.TermAccessor],
  T0,
  hydra.core.Term], tuple[T0, hydra.core.Term]], term0: T0, v1: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
    r"""Rewrite a term with path tracking, and fold a function over it, accumulating a value. The path is a list of TermAccessors from root to current position."""

    def fsub(recurse: Callable[[frozenlist[hydra.accessors.TermAccessor], T1, hydra.core.Term], tuple[T1, hydra.core.Term]], path: frozenlist[hydra.accessors.TermAccessor], val0: T1, term02: hydra.core.Term):
        def for_single_with_accessor(rec: Callable[[frozenlist[hydra.accessors.TermAccessor], T2, T3], tuple[T4, T5]], cons: Callable[[T5], T6], accessor: hydra.accessors.TermAccessor, val: T2, term: T3) -> tuple[T4, T6]:
            @lru_cache(1)
            def r() -> tuple[T4, T5]:
                return rec(hydra.lib.lists.concat2(path, (accessor,)), val, term)
            return (hydra.lib.pairs.first(r()), cons(hydra.lib.pairs.second(r())))
        def for_many_with_accessors(rec: Callable[[frozenlist[hydra.accessors.TermAccessor], T2, T3], tuple[T2, T4]], cons: Callable[[frozenlist[T4]], T5], val: T2, accessor_term_pairs: frozenlist[tuple[hydra.accessors.TermAccessor, T3]]) -> tuple[T2, T5]:
            @lru_cache(1)
            def rr() -> tuple[T2, frozenlist[T4]]:
                return hydra.lib.lists.foldl((lambda r, atp: (r2 := rec(hydra.lib.lists.concat2(path, (hydra.lib.pairs.first(atp),)), hydra.lib.pairs.first(r), hydra.lib.pairs.second(atp)), (hydra.lib.pairs.first(r2), hydra.lib.lists.cons(hydra.lib.pairs.second(r2), hydra.lib.pairs.second(r))))[1]), (val, ()), accessor_term_pairs)
            return (hydra.lib.pairs.first(rr()), cons(hydra.lib.lists.reverse(hydra.lib.pairs.second(rr()))))
        def for_field_with_accessor(mk_accessor: Callable[[hydra.core.Name], hydra.accessors.TermAccessor], val: T1, field: hydra.core.Field) -> tuple[hydra.core.Term, hydra.core.Field]:
            @lru_cache(1)
            def r() -> tuple[T1, hydra.core.Term]:
                return recurse(hydra.lib.lists.concat2(path, (mk_accessor(field.name),)), val, field.term)
            return (hydra.lib.pairs.first(r()), hydra.core.Field(field.name, hydra.lib.pairs.second(r())))
        def for_fields_with_accessor(mk_accessor: Callable[[hydra.core.Name], hydra.accessors.TermAccessor], v1: T1, v2: frozenlist[tuple[hydra.accessors.TermAccessor, hydra.core.Field]]) -> tuple[T1, frozenlist[hydra.core.Field]]:
            return for_many_with_accessors((lambda path1, val1, field1: for_field_with_accessor(mk_accessor, val1, field1)), (lambda x: x), v1, v2)
        def for_pair_with_accessors(key_accessor: hydra.accessors.TermAccessor, val_accessor: hydra.accessors.TermAccessor, val: T1, kv: tuple[hydra.core.Term, hydra.core.Term]) -> tuple[hydra.core.Term, tuple[hydra.core.Term, hydra.core.Term]]:
            @lru_cache(1)
            def rk() -> tuple[T1, hydra.core.Term]:
                return recurse(hydra.lib.lists.concat2(path, (key_accessor,)), val, hydra.lib.pairs.first(kv))
            @lru_cache(1)
            def rv() -> tuple[T1, hydra.core.Term]:
                return recurse(hydra.lib.lists.concat2(path, (val_accessor,)), hydra.lib.pairs.first(rk()), hydra.lib.pairs.second(kv))
            return (hydra.lib.pairs.first(rv()), (hydra.lib.pairs.second(rk()), hydra.lib.pairs.second(rv())))
        def for_binding_with_accessor(val: T1, binding: hydra.core.Binding) -> tuple[hydra.core.Term, hydra.core.Binding]:
            @lru_cache(1)
            def r() -> tuple[T1, hydra.core.Term]:
                return recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLetBinding(binding.name)),)), val, binding.term)
            return (hydra.lib.pairs.first(r()), hydra.core.Binding(binding.name, hydra.lib.pairs.second(r()), binding.type))
        def for_elimination(val: T1, elm: hydra.core.Elimination) -> tuple[hydra.core.Elimination, hydra.core.Elimination]:
            @lru_cache(1)
            def r():
                def _hoist_r_1(v1):
                    match v1:
                        case hydra.core.EliminationUnion(value=cs):
                            @lru_cache(1)
                            def rmd() -> Maybe[tuple[T1, hydra.core.Term]]:
                                return hydra.lib.maybes.map((lambda def_: recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorUnionCasesDefault()),)), val, def_)), cs.default)
                            @lru_cache(1)
                            def val1() -> tuple[T1, hydra.core.Term]:
                                return hydra.lib.maybes.maybe((lambda : val), (lambda x1: hydra.lib.pairs.first(x1)), rmd())
                            @lru_cache(1)
                            def rcases() -> tuple[T1, frozenlist[hydra.core.Term]]:
                                return for_many_with_accessors(recurse, (lambda x: x), val1(), hydra.lib.lists.map((lambda f2: (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorUnionCasesBranch(f2.name)), f2.term)), cs.cases))
                            return (hydra.lib.pairs.first(rcases()), cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map((lambda x1: hydra.lib.pairs.second(x1)), rmd()), hydra.lib.lists.map((lambda ft: hydra.core.Field(hydra.lib.pairs.first(ft), hydra.lib.pairs.second(ft))), hydra.lib.lists.zip(hydra.lib.lists.map((lambda v1: v1.name), cs.cases), hydra.lib.pairs.second(rcases())))))))

                        case _:
                            return (val, elm)
                return _hoist_r_1(elm)
            return (hydra.lib.pairs.first(r()), hydra.lib.pairs.second(r()))
        def for_function(val: T1, fun: hydra.core.Function):
            def _hoist_for_function_1(fun, val, v1):
                match v1:
                    case hydra.core.FunctionElimination(value=elm):
                        @lru_cache(1)
                        def re() -> tuple[T1, hydra.core.Elimination]:
                            return for_elimination(val, elm)
                        return (hydra.lib.pairs.first(re()), cast(hydra.core.Function, hydra.core.FunctionElimination(hydra.lib.pairs.second(re()))))

                    case hydra.core.FunctionLambda(value=l):
                        @lru_cache(1)
                        def rl() -> tuple[T1, hydra.core.Term]:
                            return recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLambdaBody()),)), val, l.body)
                        return (hydra.lib.pairs.first(rl()), cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, hydra.lib.pairs.second(rl())))))

                    case _:
                        return (val, fun)
            return _hoist_for_function_1(fun, val, fun)
        @lru_cache(1)
        def dflt() -> tuple[T1, hydra.core.Term]:
            return (val0, term02)
        def _hoist_body_1(v1):
            match v1:
                case hydra.core.TermAnnotated(value=at):
                    return for_single_with_accessor(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(t, at.annotation)))), cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorAnnotatedBody()), val0, at.body)

                case hydra.core.TermApplication(value=a):
                    @lru_cache(1)
                    def rlhs() -> tuple[T1, hydra.core.Term]:
                        return recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorApplicationFunction()),)), val0, a.function)
                    @lru_cache(1)
                    def rrhs() -> tuple[T1, hydra.core.Term]:
                        return recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorApplicationArgument()),)), hydra.lib.pairs.first(rlhs()), a.argument)
                    return (hydra.lib.pairs.first(rrhs()), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(hydra.lib.pairs.second(rlhs()), hydra.lib.pairs.second(rrhs())))))

                case hydra.core.TermEither(value=e):
                    return hydra.lib.eithers.either((lambda l: (rl := recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorSumTerm()),)), val0, l), (hydra.lib.pairs.first(rl), cast(hydra.core.Term, hydra.core.TermEither(Left(hydra.lib.pairs.second(rl))))))[1]), (lambda r: (rr := recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorSumTerm()),)), val0, r), (hydra.lib.pairs.first(rr), cast(hydra.core.Term, hydra.core.TermEither(Right(hydra.lib.pairs.second(rr))))))[1]), e)

                case hydra.core.TermFunction(value=f2):
                    @lru_cache(1)
                    def rf() -> tuple[T1, hydra.core.Function]:
                        return for_function(val0, f2)
                    return (hydra.lib.pairs.first(rf()), cast(hydra.core.Term, hydra.core.TermFunction(hydra.lib.pairs.second(rf()))))

                case hydra.core.TermLet(value=l):
                    @lru_cache(1)
                    def renv() -> tuple[T1, hydra.core.Term]:
                        return recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLetBody()),)), val0, l.body)
                    @lru_cache(1)
                    def rbindings() -> tuple[hydra.core.Binding, frozenlist[hydra.core.Binding]]:
                        return hydra.lib.lists.foldl((lambda r, binding: (rb := for_binding_with_accessor(hydra.lib.pairs.first(r), binding), (hydra.lib.pairs.first(rb), hydra.lib.lists.cons(hydra.lib.pairs.second(rb), hydra.lib.pairs.second(r))))[1]), (hydra.lib.pairs.first(renv()), ()), l.bindings)
                    return (hydra.lib.pairs.first(rbindings()), cast(hydra.core.Term, hydra.core.TermLet(hydra.core.Let(hydra.lib.lists.reverse(hydra.lib.pairs.second(rbindings())), hydra.lib.pairs.second(renv())))))

                case hydra.core.TermList(value=els):
                    idx = 0
                    @lru_cache(1)
                    def rr() -> tuple[int, tuple[hydra.core.Term, frozenlist[hydra.core.Term]]]:
                        return hydra.lib.lists.foldl((lambda r, el: (r2 := recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorListElement(hydra.lib.pairs.first(r))),)), hydra.lib.pairs.first(hydra.lib.pairs.second(r)), el), (hydra.lib.math.add(hydra.lib.pairs.first(r), 1), (hydra.lib.pairs.first(r2), hydra.lib.lists.cons(hydra.lib.pairs.second(r2), hydra.lib.pairs.second(hydra.lib.pairs.second(r))))))[1]), (idx, (val0, ())), els)
                    return (hydra.lib.pairs.first(hydra.lib.pairs.second(rr())), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.reverse(hydra.lib.pairs.second(hydra.lib.pairs.second(rr()))))))

                case hydra.core.TermMap(value=m):
                    idx = 0
                    @lru_cache(1)
                    def rr() -> tuple[int, tuple[tuple[hydra.core.Term, hydra.core.Term], frozenlist[tuple[hydra.core.Term, hydra.core.Term]]]]:
                        return hydra.lib.lists.foldl((lambda r, kv: (rk := recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMapKey(hydra.lib.pairs.first(r))),)), hydra.lib.pairs.first(hydra.lib.pairs.second(r)), hydra.lib.pairs.first(kv)), rv := recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMapValue(hydra.lib.pairs.first(r))),)), hydra.lib.pairs.first(rk), hydra.lib.pairs.second(kv)), (hydra.lib.math.add(hydra.lib.pairs.first(r), 1), (hydra.lib.pairs.first(rv), hydra.lib.lists.cons((hydra.lib.pairs.second(rk), hydra.lib.pairs.second(rv)), hydra.lib.pairs.second(hydra.lib.pairs.second(r))))))[2]), (idx, (val0, ())), hydra.lib.maps.to_list(m))
                    return (hydra.lib.pairs.first(hydra.lib.pairs.second(rr())), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(hydra.lib.lists.reverse(hydra.lib.pairs.second(hydra.lib.pairs.second(rr())))))))

                case hydra.core.TermMaybe(value=mt):
                    return hydra.lib.maybes.maybe((lambda : dflt()), (lambda t: for_single_with_accessor(recurse, (lambda t1: cast(hydra.core.Term, hydra.core.TermMaybe(Just(t1)))), cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMaybeTerm()), val0, t)), mt)

                case hydra.core.TermPair(value=p):
                    @lru_cache(1)
                    def rf() -> tuple[T1, hydra.core.Term]:
                        return recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorProductTerm(0)),)), val0, hydra.lib.pairs.first(p))
                    @lru_cache(1)
                    def rs() -> tuple[T1, hydra.core.Term]:
                        return recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorProductTerm(1)),)), hydra.lib.pairs.first(rf()), hydra.lib.pairs.second(p))
                    return (hydra.lib.pairs.first(rs()), cast(hydra.core.Term, hydra.core.TermPair((hydra.lib.pairs.second(rf()), hydra.lib.pairs.second(rs())))))

                case hydra.core.TermRecord(value=r):
                    @lru_cache(1)
                    def rfields() -> tuple[T1, frozenlist[hydra.core.Term]]:
                        return for_many_with_accessors(recurse, (lambda x: x), val0, hydra.lib.lists.map((lambda f2: (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorRecordField(f2.name)), f2.term)), r.fields))
                    return (hydra.lib.pairs.first(rfields()), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, hydra.lib.lists.map((lambda ft: hydra.core.Field(hydra.lib.pairs.first(ft), hydra.lib.pairs.second(ft))), hydra.lib.lists.zip(hydra.lib.lists.map((lambda v1: v1.name), r.fields), hydra.lib.pairs.second(rfields())))))))

                case hydra.core.TermSet(value=els):
                    idx = 0
                    @lru_cache(1)
                    def rr() -> tuple[int, tuple[hydra.core.Term, frozenlist[hydra.core.Term]]]:
                        return hydra.lib.lists.foldl((lambda r, el: (r2 := recurse(hydra.lib.lists.concat2(path, (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorSetElement(hydra.lib.pairs.first(r))),)), hydra.lib.pairs.first(hydra.lib.pairs.second(r)), el), (hydra.lib.math.add(hydra.lib.pairs.first(r), 1), (hydra.lib.pairs.first(r2), hydra.lib.lists.cons(hydra.lib.pairs.second(r2), hydra.lib.pairs.second(hydra.lib.pairs.second(r))))))[1]), (idx, (val0, ())), hydra.lib.sets.to_list(els))
                    return (hydra.lib.pairs.first(hydra.lib.pairs.second(rr())), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(hydra.lib.lists.reverse(hydra.lib.pairs.second(hydra.lib.pairs.second(rr())))))))

                case hydra.core.TermTypeApplication(value=ta):
                    return for_single_with_accessor(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(t, ta.type)))), cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorTypeApplicationTerm()), val0, ta.body)

                case hydra.core.TermTypeLambda(value=tl):
                    return for_single_with_accessor(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(tl.parameter, t)))), cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorTypeLambdaBody()), val0, tl.body)

                case hydra.core.TermUnion(value=inj):
                    return for_single_with_accessor(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(inj.type_name, hydra.core.Field(inj.field.name, t))))), cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorInjectionTerm()), val0, inj.field.term)

                case hydra.core.TermWrap(value=wt):
                    return for_single_with_accessor(recurse, (lambda t: cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, t)))), cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorWrappedTerm()), val0, wt.body)

                case _:
                    return dflt()
        return _hoist_body_1(term02)
    def recurse(v1: frozenlist[hydra.accessors.TermAccessor], v2: T0, v3: hydra.core.Term) -> tuple[T0, hydra.core.Term]:
        return f((lambda v12, v22, v32: fsub((lambda x1, x2, x3: recurse(x1, x2, x3)), v12, v22, v32)), v1, v2, v3)
    return recurse((), term0, v1)

def rewrite_term_m(f: Callable[[
  Callable[[hydra.core.Term], Either[T0, hydra.core.Term]],
  hydra.core.Term], Either[T0, hydra.core.Term]], term0: hydra.core.Term) -> Either[T0, hydra.core.Term]:
    r"""Either-based term rewriting with custom transformation function."""

    def fsub(recurse: Callable[[hydra.core.Term], Either[T1, hydra.core.Term]], term: hydra.core.Term):
        def for_field(field: hydra.core.Field) -> Either[hydra.core.Term, hydra.core.Field]:
            return hydra.lib.eithers.bind(recurse(field.term), (lambda t: Right(hydra.core.Field(field.name, t))))
        def for_pair(kv: tuple[hydra.core.Term, hydra.core.Term]) -> Either[hydra.core.Term, tuple[hydra.core.Term, hydra.core.Term]]:
            return hydra.lib.eithers.bind(recurse(hydra.lib.pairs.first(kv)), (lambda k: hydra.lib.eithers.bind(recurse(hydra.lib.pairs.second(kv)), (lambda v: Right((k, v))))))
        def map_binding(b: hydra.core.Binding) -> Either[hydra.core.Term, hydra.core.Binding]:
            return hydra.lib.eithers.bind(recurse(b.term), (lambda v: Right(hydra.core.Binding(b.name, v, b.type))))
        def _hoist_body_1(v1):
            match v1:
                case hydra.core.TermAnnotated(value=at):
                    return hydra.lib.eithers.bind(recurse(at.body), (lambda ex: Right(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(ex, at.annotation))))))

                case hydra.core.TermApplication(value=app):
                    return hydra.lib.eithers.bind(recurse(app.function), (lambda lhs: hydra.lib.eithers.bind(recurse(app.argument), (lambda rhs: Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs, rhs))))))))

                case hydra.core.TermEither(value=e):
                    return hydra.lib.eithers.bind(hydra.lib.eithers.either((lambda l: hydra.lib.eithers.map((lambda x: Left(x)), recurse(l))), (lambda r: hydra.lib.eithers.map((lambda x: Right(x)), recurse(r))), e), (lambda re: Right(cast(hydra.core.Term, hydra.core.TermEither(re)))))

                case hydra.core.TermFunction(value=fun):
                    def for_elm(e: hydra.core.Elimination):
                        def _hoist_for_elm_1(v12):
                            match v12:
                                case hydra.core.EliminationRecord(value=p):
                                    return Right(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(p)))))

                                case hydra.core.EliminationUnion(value=cs):
                                    n = cs.type_name
                                    def_ = cs.default
                                    cases = cs.cases
                                    return hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda t: hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), recurse(t))), def_), (lambda rdef: hydra.lib.eithers.map((lambda rcases: cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(n, rdef, rcases)))))), hydra.lib.eithers.map_list((lambda x1: for_field(x1)), cases))))

                                case hydra.core.EliminationWrap(value=name):
                                    return Right(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(name)))))

                                case _:
                                    raise AssertionError("Unreachable: all variants handled")
                        return _hoist_for_elm_1(e)
                    def for_fun(fun2: hydra.core.Function):
                        def _hoist_for_fun_1(v12):
                            match v12:
                                case hydra.core.FunctionElimination(value=e):
                                    return for_elm(e)

                                case hydra.core.FunctionLambda(value=l):
                                    v = l.parameter
                                    d = l.domain
                                    body = l.body
                                    return hydra.lib.eithers.bind(recurse(body), (lambda rbody: Right(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(v, d, rbody))))))

                                case hydra.core.FunctionPrimitive(value=name):
                                    return Right(cast(hydra.core.Function, hydra.core.FunctionPrimitive(name)))

                                case _:
                                    raise AssertionError("Unreachable: all variants handled")
                        return _hoist_for_fun_1(fun2)
                    return hydra.lib.eithers.bind(for_fun(fun), (lambda rfun: Right(cast(hydra.core.Term, hydra.core.TermFunction(rfun)))))

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

                case hydra.core.TermUnion(value=i):
                    n = i.type_name
                    field = i.field
                    return hydra.lib.eithers.map((lambda rfield: cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(n, rfield)))), for_field(field))

                case hydra.core.TermUnit():
                    return Right(cast(hydra.core.Term, hydra.core.TermUnit()))

                case hydra.core.TermVariable(value=v):
                    return Right(cast(hydra.core.Term, hydra.core.TermVariable(v)))

                case hydra.core.TermWrap(value=wt):
                    name = wt.type_name
                    t = wt.body
                    return hydra.lib.eithers.bind(recurse(t), (lambda rt: Right(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(name, rt))))))

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        return _hoist_body_1(term)
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
        def for_elimination(elm: hydra.core.Elimination):
            def _hoist_for_elimination_1(v1):
                match v1:
                    case hydra.core.EliminationRecord(value=p):
                        return cast(hydra.core.Elimination, hydra.core.EliminationRecord(p))

                    case hydra.core.EliminationUnion(value=cs):
                        return cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(cs.type_name, hydra.lib.maybes.map((lambda x1: recurse(x1)), cs.default), hydra.lib.lists.map((lambda x1: for_field(x1)), cs.cases))))

                    case hydra.core.EliminationWrap(value=name):
                        return cast(hydra.core.Elimination, hydra.core.EliminationWrap(name))

                    case _:
                        raise AssertionError("Unreachable: all variants handled")
            return _hoist_for_elimination_1(elm)
        def for_function(fun: hydra.core.Function):
            def _hoist_for_function_1(v1):
                match v1:
                    case hydra.core.FunctionElimination(value=elm):
                        return cast(hydra.core.Function, hydra.core.FunctionElimination(for_elimination(elm)))

                    case hydra.core.FunctionLambda(value=l):
                        return cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(l.parameter, l.domain, recurse(l.body))))

                    case hydra.core.FunctionPrimitive(value=name):
                        return cast(hydra.core.Function, hydra.core.FunctionPrimitive(name))

                    case _:
                        raise AssertionError("Unreachable: all variants handled")
            return _hoist_for_function_1(fun)
        def for_let(lt: hydra.core.Let) -> hydra.core.Let:
            def map_binding(b: hydra.core.Binding) -> hydra.core.Binding:
                return hydra.core.Binding(b.name, recurse(b.term), b.type)
            return hydra.core.Let(hydra.lib.lists.map((lambda x1: map_binding(x1)), lt.bindings), recurse(lt.body))
        def for_map(m: FrozenDict[hydra.core.Term, hydra.core.Term]) -> FrozenDict[hydra.core.Term, hydra.core.Term]:
            def for_pair(p: tuple[hydra.core.Term, hydra.core.Term]) -> tuple[hydra.core.Term, hydra.core.Term]:
                return (recurse(hydra.lib.pairs.first(p)), recurse(hydra.lib.pairs.second(p)))
            return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: for_pair(x1)), hydra.lib.maps.to_list(m)))
        def _hoist_body_1(v1):
            match v1:
                case hydra.core.TermAnnotated(value=at):
                    return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(recurse(at.body), at.annotation)))

                case hydra.core.TermApplication(value=a):
                    return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(recurse(a.function), recurse(a.argument))))

                case hydra.core.TermEither(value=e):
                    return cast(hydra.core.Term, hydra.core.TermEither(hydra.lib.eithers.either((lambda l: Left(recurse(l))), (lambda r: Right(recurse(r))), e)))

                case hydra.core.TermFunction(value=fun):
                    return cast(hydra.core.Term, hydra.core.TermFunction(for_function(fun)))

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

                case hydra.core.TermRecord(value=r):
                    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(r.type_name, hydra.lib.lists.map((lambda x1: for_field(x1)), r.fields))))

                case hydra.core.TermSet(value=s):
                    return cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(hydra.lib.lists.map((lambda x1: recurse(x1)), hydra.lib.sets.to_list(s)))))

                case hydra.core.TermTypeApplication(value=tt):
                    return cast(hydra.core.Term, hydra.core.TermTypeApplication(hydra.core.TypeApplicationTerm(recurse(tt.body), tt.type)))

                case hydra.core.TermTypeLambda(value=ta):
                    return cast(hydra.core.Term, hydra.core.TermTypeLambda(hydra.core.TypeLambda(ta.parameter, recurse(ta.body))))

                case hydra.core.TermUnion(value=i):
                    return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(i.type_name, for_field(i.field))))

                case hydra.core.TermUnit():
                    return cast(hydra.core.Term, hydra.core.TermUnit())

                case hydra.core.TermVariable(value=v):
                    return cast(hydra.core.Term, hydra.core.TermVariable(v))

                case hydra.core.TermWrap(value=wt):
                    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wt.type_name, recurse(wt.body))))

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        return _hoist_body_1(term)
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
        def for_elimination(e: hydra.core.Elimination):
            def _hoist_for_elimination_1(v1):
                match v1:
                    case hydra.core.EliminationRecord(value=p):
                        return Right(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(p)))))

                    case hydra.core.EliminationUnion(value=cs):
                        n = cs.type_name
                        def_ = cs.default
                        cases = cs.cases
                        return hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda t: hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), recurse(t))), def_), (lambda rdef: hydra.lib.eithers.map((lambda rcases: cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(n, rdef, rcases)))))), hydra.lib.eithers.map_list((lambda x1: for_field(x1)), cases))))

                    case hydra.core.EliminationWrap(value=name):
                        return Right(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(name)))))

                    case _:
                        raise AssertionError("Unreachable: all variants handled")
            return _hoist_for_elimination_1(e)
        def for_function(fun: hydra.core.Function):
            def _hoist_for_function_1(v1):
                match v1:
                    case hydra.core.FunctionElimination(value=e):
                        return for_elimination(e)

                    case hydra.core.FunctionLambda(value=l):
                        v = l.parameter
                        d = l.domain
                        body = l.body
                        return hydra.lib.eithers.bind(recurse(body), (lambda rbody: Right(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(v, d, rbody))))))

                    case hydra.core.FunctionPrimitive(value=name):
                        return Right(cast(hydra.core.Function, hydra.core.FunctionPrimitive(name)))

                    case _:
                        raise AssertionError("Unreachable: all variants handled")
            return _hoist_for_function_1(fun)
        def map_binding(b: hydra.core.Binding) -> Either[T3, hydra.core.Binding]:
            return hydra.lib.eithers.bind(recurse(b.term), (lambda v: Right(hydra.core.Binding(b.name, v, b.type))))
        def _hoist_body_1(v1):
            match v1:
                case hydra.core.TermAnnotated(value=at):
                    return hydra.lib.eithers.bind(recurse(at.body), (lambda ex: Right(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(ex, at.annotation))))))

                case hydra.core.TermApplication(value=app):
                    return hydra.lib.eithers.bind(recurse(app.function), (lambda lhs: hydra.lib.eithers.bind(recurse(app.argument), (lambda rhs: Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(lhs, rhs))))))))

                case hydra.core.TermEither(value=e):
                    return hydra.lib.eithers.bind(hydra.lib.eithers.either((lambda l: hydra.lib.eithers.map((lambda x: Left(x)), recurse(l))), (lambda r: hydra.lib.eithers.map((lambda x: Right(x)), recurse(r))), e), (lambda re: Right(cast(hydra.core.Term, hydra.core.TermEither(re)))))

                case hydra.core.TermFunction(value=fun):
                    return hydra.lib.eithers.bind(for_function(fun), (lambda rfun: Right(cast(hydra.core.Term, hydra.core.TermFunction(rfun)))))

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

                case hydra.core.TermUnion(value=i):
                    n = i.type_name
                    field = i.field
                    return hydra.lib.eithers.map((lambda rfield: cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(n, rfield)))), for_field(field))

                case hydra.core.TermUnit():
                    return Right(cast(hydra.core.Term, hydra.core.TermUnit()))

                case hydra.core.TermVariable(value=v):
                    return Right(cast(hydra.core.Term, hydra.core.TermVariable(v)))

                case hydra.core.TermWrap(value=wt):
                    name = wt.type_name
                    t = wt.body
                    return hydra.lib.eithers.bind(recurse(t), (lambda rt: Right(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(name, rt))))))

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        return _hoist_body_1(term)
    def rewrite(cx: T0, term: hydra.core.Term) -> Either[T1, hydra.core.Term]:
        return f((lambda v1, v2: for_subterms((lambda x1, x2: rewrite(x1, x2)), v1, v2)), cx, term)
    return rewrite(cx0, term0)

def substitute_variable(from_: hydra.core.Name, to: hydra.core.Name, term: hydra.core.Term) -> hydra.core.Term:
    r"""Substitute one variable for another in a term."""

    def replace(recurse: Callable[[hydra.core.Term], hydra.core.Term], term2: hydra.core.Term):
        def _hoist_replace_1(recurse, term2, v1):
            match v1:
                case hydra.core.FunctionLambda(value=l):
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(l.parameter, from_), (lambda : term2), (lambda : recurse(term2)))

                case _:
                    return recurse(term2)
        match term2:
            case hydra.core.TermVariable(value=x):
                return cast(hydra.core.Term, hydra.core.TermVariable(hydra.lib.logic.if_else(hydra.lib.equality.equal(x, from_), (lambda : to), (lambda : x))))

            case hydra.core.TermFunction(value=_match_value):
                return _hoist_replace_1(recurse, term2, _match_value)

            case _:
                return recurse(term2)
    return rewrite_term((lambda x1, x2: replace(x1, x2)), term)

def simplify_term(term: hydra.core.Term) -> hydra.core.Term:
    r"""Simplify terms by applying beta reduction where possible."""

    def simplify(recurse: Callable[[hydra.core.Term], T0], term2: hydra.core.Term) -> T0:
        def for_rhs(rhs: hydra.core.Term, var: hydra.core.Name, body: hydra.core.Term):
            def _hoist_for_rhs_1(body, var, v1):
                match v1:
                    case hydra.core.TermVariable(value=v):
                        return simplify_term(substitute_variable(var, v, body))

                    case _:
                        return term2
            return _hoist_for_rhs_1(body, var, deannotate_term(rhs))
        def for_lhs(lhs: hydra.core.Term, rhs: hydra.core.Term):
            def for_fun(fun: hydra.core.Function):
                def _hoist_for_fun_1(v1):
                    match v1:
                        case hydra.core.FunctionLambda(value=l):
                            var = l.parameter
                            body = l.body
                            return hydra.lib.logic.if_else(hydra.lib.sets.member(var, free_variables_in_term(body)), (lambda : for_rhs(rhs, var, body)), (lambda : simplify_term(body)))

                        case _:
                            return term2
                return _hoist_for_fun_1(fun)
            def _hoist_body_1(v1):
                match v1:
                    case hydra.core.TermFunction(value=fun):
                        return for_fun(fun)

                    case _:
                        return term2
            return _hoist_body_1(deannotate_term(lhs))
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
            return deannotate_term(term2)
        return recurse(for_term(stripped()))
    return rewrite_term((lambda x1, x2: simplify(x1, x2)), term)

def strip_type_lambdas(t: hydra.core.Term) -> hydra.core.Term:
    r"""Strip outer type lambda wrappers from a term, preserving type application wrappers and annotations."""

    match t:
        case hydra.core.TermAnnotated(value=at):
            subj = at.body
            ann = at.annotation
            return cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(strip_type_lambdas(subj), ann)))

        case hydra.core.TermTypeLambda(value=ta):
            return strip_type_lambdas(ta.body)

        case _:
            return t

def substitute_type_variables(subst: FrozenDict[hydra.core.Name, hydra.core.Name], typ: hydra.core.Type) -> hydra.core.Type:
    r"""Substitute type variables in a type."""

    def replace(recurse: Callable[[hydra.core.Type], hydra.core.Type], typ2: hydra.core.Type) -> hydra.core.Type:
        match typ2:
            case hydra.core.TypeVariable(value=n):
                return cast(hydra.core.Type, hydra.core.TypeVariable(hydra.lib.maybes.from_maybe((lambda : n), hydra.lib.maps.lookup(n, subst))))

            case _:
                return recurse(typ2)
    return rewrite_type((lambda x1, x2: replace(x1, x2)), typ)

def subterms_with_accessors(v1: hydra.core.Term) -> frozenlist[tuple[hydra.accessors.TermAccessor, hydra.core.Term]]:
    r"""Find the children of a given term."""

    match v1:
        case hydra.core.TermAnnotated(value=at):
            return ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorAnnotatedBody()), at.body),)

        case hydra.core.TermApplication(value=p):
            return ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorApplicationFunction()), p.function), (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorApplicationArgument()), p.argument))

        case hydra.core.TermEither():
            return ()

        case hydra.core.TermFunction(value=v12):
            match v12:
                case hydra.core.FunctionElimination(value=v13):
                    match v13:
                        case hydra.core.EliminationUnion(value=cs):
                            return hydra.lib.lists.concat2(hydra.lib.maybes.maybe((lambda : ()), (lambda t: ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorUnionCasesDefault()), t),)), cs.default), hydra.lib.lists.map((lambda f: (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorUnionCasesBranch(f.name)), f.term)), cs.cases))

                        case _:
                            return ()

                case hydra.core.FunctionLambda(value=l):
                    return ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLambdaBody()), l.body),)

                case _:
                    return ()

        case hydra.core.TermLet(value=lt):
            return hydra.lib.lists.cons((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLetBody()), lt.body), hydra.lib.lists.map((lambda b: (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorLetBinding(b.name)), b.term)), lt.bindings))

        case hydra.core.TermList(value=l):
            return hydra.lib.lists.map((lambda e: (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorListElement(0)), e)), l)

        case hydra.core.TermLiteral():
            return ()

        case hydra.core.TermMap(value=m):
            return hydra.lib.lists.concat(hydra.lib.lists.map((lambda p: ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMapKey(0)), hydra.lib.pairs.first(p)), (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMapValue(0)), hydra.lib.pairs.second(p)))), hydra.lib.maps.to_list(m)))

        case hydra.core.TermMaybe(value=m2):
            return hydra.lib.maybes.maybe((lambda : ()), (lambda t: ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorMaybeTerm()), t),)), m2)

        case hydra.core.TermPair():
            return ()

        case hydra.core.TermRecord(value=rt):
            return hydra.lib.lists.map((lambda f: (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorRecordField(f.name)), f.term)), rt.fields)

        case hydra.core.TermSet(value=s):
            return hydra.lib.lists.map((lambda e: (cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorListElement(0)), e)), hydra.lib.sets.to_list(s))

        case hydra.core.TermTypeApplication(value=ta):
            return ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorTypeApplicationTerm()), ta.body),)

        case hydra.core.TermTypeLambda(value=ta2):
            return ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorTypeLambdaBody()), ta2.body),)

        case hydra.core.TermUnion(value=ut):
            return ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorInjectionTerm()), ut.field.term),)

        case hydra.core.TermUnit():
            return ()

        case hydra.core.TermVariable():
            return ()

        case hydra.core.TermWrap(value=n):
            return ((cast(hydra.accessors.TermAccessor, hydra.accessors.TermAccessorWrappedTerm()), n.body),)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def term_dependency_names(binds: bool, with_prims: bool, with_noms: bool, term0: hydra.core.Term) -> frozenset[hydra.core.Name]:
    r"""Note: does not distinguish between bound and free variables; use freeVariablesInTerm for that."""

    def add_names(names: frozenset[hydra.core.Name], term: hydra.core.Term):
        def nominal(name: hydra.core.Name) -> frozenset[hydra.core.Name]:
            return hydra.lib.logic.if_else(with_noms, (lambda : hydra.lib.sets.insert(name, names)), (lambda : names))
        def prim(name: hydra.core.Name) -> frozenset[hydra.core.Name]:
            return hydra.lib.logic.if_else(with_prims, (lambda : hydra.lib.sets.insert(name, names)), (lambda : names))
        def var(name: hydra.core.Name) -> frozenset[hydra.core.Name]:
            return hydra.lib.logic.if_else(binds, (lambda : hydra.lib.sets.insert(name, names)), (lambda : names))
        def _hoist_body_1(v1):
            match v1:
                case hydra.core.EliminationRecord(value=proj):
                    return nominal(proj.type_name)

                case hydra.core.EliminationUnion(value=case_stmt):
                    return nominal(case_stmt.type_name)

                case hydra.core.EliminationWrap(value=name):
                    return nominal(name)

                case _:
                    raise AssertionError("Unreachable: all variants handled")
        def _hoist_body_2(v1):
            match v1:
                case hydra.core.FunctionPrimitive(value=name):
                    return prim(name)

                case hydra.core.FunctionElimination(value=e):
                    return _hoist_body_1(e)

                case _:
                    return names
        match term:
            case hydra.core.TermFunction(value=f):
                return _hoist_body_2(f)

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
    return fold_over_term(hydra.coders.TraversalOrder.PRE, (lambda x1, x2: add_names(x1, x2)), hydra.lib.sets.empty(), term0)

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
        return (name(), hydra.lib.logic.if_else(has_type_annotation(term()), (lambda : ()), (lambda : hydra.lib.sets.to_list(hydra.lib.sets.intersection(keys(), free_variables_in_term(term()))))))
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
    return fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda x1, x2: add_names(x1, x2)), hydra.lib.sets.empty(), typ0)

def type_dependency_names(with_schema: bool, typ: hydra.core.Type) -> frozenset[hydra.core.Name]:
    return hydra.lib.logic.if_else(with_schema, (lambda : hydra.lib.sets.union(free_variables_in_type(typ), type_names_in_type(typ))), (lambda : free_variables_in_type(typ)))

def type_scheme_to_f_type(ts: hydra.core.TypeScheme) -> hydra.core.Type:
    r"""Convert a type scheme to a forall type."""

    vars = ts.variables
    body = ts.type
    return hydra.lib.lists.foldl((lambda t, v: cast(hydra.core.Type, hydra.core.TypeForall(hydra.core.ForallType(v, t)))), body, hydra.lib.lists.reverse(vars))

def unshadow_variables(term0: hydra.core.Term) -> hydra.core.Term:
    r"""Rename all shadowed variables (both lambda parameters and let-bound variables that shadow lambda parameters) in a term."""

    def fresh_name(base: hydra.core.Name, i: int, m: FrozenDict[hydra.core.Name, T0]) -> hydra.core.Name:
        @lru_cache(1)
        def candidate() -> hydra.core.Name:
            return hydra.core.Name(hydra.lib.strings.cat2(base.value, hydra.lib.literals.show_int32(i)))
        return hydra.lib.logic.if_else(hydra.lib.maps.member(candidate(), m), (lambda : fresh_name(base, hydra.lib.math.add(i, 1), m)), (lambda : candidate()))
    def f(recurse: Callable[[FrozenDict[hydra.core.Name, hydra.core.Name], hydra.core.Term], hydra.core.Term], m: FrozenDict[hydra.core.Name, hydra.core.Name], term: hydra.core.Term):
        def _hoist_f_1(m, recurse, term, v1):
            match v1:
                case hydra.core.FunctionLambda(value=l):
                    v = l.parameter
                    domain = l.domain
                    body = l.body
                    return hydra.lib.logic.if_else(hydra.lib.maps.member(v, m), (lambda : (v2 := fresh_name(v, 2, m), (m2 := hydra.lib.maps.insert(v, v2, hydra.lib.maps.insert(v2, v2, m)), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(v2, domain, f(recurse, m2, body)))))))[1])[1]), (lambda : cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(v, domain, f(recurse, hydra.lib.maps.insert(v, v, m), body))))))))

                case _:
                    return recurse(m, term)
        match term:
            case hydra.core.TermFunction(value=fn):
                return _hoist_f_1(m, recurse, term, fn)

            case hydra.core.TermLet(value=lt):
                @lru_cache(1)
                def m2() -> FrozenDict[hydra.core.Name, hydra.core.Name]:
                    return hydra.lib.lists.foldl((lambda acc, b: (bname := b.name, hydra.lib.logic.if_else(hydra.lib.maps.member(bname, acc), (lambda : acc), (lambda : hydra.lib.maps.insert(bname, bname, acc))))[1]), m, lt.bindings)
                return recurse(m2(), term)

            case hydra.core.TermVariable(value=v):
                return cast(hydra.core.Term, hydra.core.TermVariable(hydra.lib.maybes.maybe((lambda : v), (lambda renamed: renamed), hydra.lib.maps.lookup(v, m))))

            case _:
                return recurse(m, term)
    return rewrite_term_with_context((lambda x1, x2, x3: f(x1, x2, x3)), hydra.lib.maps.empty(), term0)
