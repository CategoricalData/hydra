# Note: this is an automatically generated file. Do not edit.

r"""Utilities for type unification."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import TypeVar, cast
import hydra.coders
import hydra.context
import hydra.core
import hydra.errors
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.rewriting
import hydra.show.core
import hydra.substitution
import hydra.typing

T0 = TypeVar("T0")

def join_types(cx: hydra.context.Context, left: hydra.core.Type, right: hydra.core.Type, comment: str):
    r"""Join two types, producing a list of type constraints.The comment is used to provide context for the constraints."""

    @lru_cache(1)
    def sleft() -> hydra.core.Type:
        return hydra.rewriting.deannotate_type(left)
    @lru_cache(1)
    def sright() -> hydra.core.Type:
        return hydra.rewriting.deannotate_type(right)
    def join_one(l: hydra.core.Type, r: hydra.core.Type) -> hydra.typing.TypeConstraint:
        return hydra.typing.TypeConstraint(l, r, hydra.lib.strings.cat2("join types; ", comment))
    @lru_cache(1)
    def cannot_unify() -> Either[hydra.context.InContext[hydra.errors.UnificationError], T0]:
        return Left(hydra.context.InContext(hydra.errors.UnificationError(sleft(), sright(), hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("cannot unify ", hydra.show.core.type(sleft())), " with "), hydra.show.core.type(sright()))), cx))
    @lru_cache(1)
    def assert_equal() -> Either[hydra.context.InContext[hydra.errors.UnificationError], frozenlist[T0]]:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(sleft(), sright()), (lambda : Right(())), (lambda : cannot_unify()))
    def join_list(lefts: frozenlist[hydra.core.Type], rights: frozenlist[hydra.core.Type]) -> Either[hydra.context.InContext[hydra.errors.UnificationError], frozenlist[hydra.typing.TypeConstraint]]:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(lefts), hydra.lib.lists.length(rights)), (lambda : Right(hydra.lib.lists.zip_with((lambda x1, x2: join_one(x1, x2)), lefts, rights))), (lambda : cannot_unify()))
    def join_row_types(left2: frozenlist[hydra.core.FieldType], right2: frozenlist[hydra.core.FieldType]) -> Either[hydra.context.InContext[hydra.errors.UnificationError], frozenlist[hydra.typing.TypeConstraint]]:
        return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(hydra.lib.lists.length(hydra.lib.lists.map((lambda v1: v1.name), left2)), hydra.lib.lists.length(hydra.lib.lists.map((lambda v1: v1.name), right2))), hydra.lib.lists.foldl(hydra.lib.logic.and_, True, hydra.lib.lists.zip_with((lambda left3, right3: hydra.lib.equality.equal(left3.value, right3.value)), hydra.lib.lists.map((lambda v1: v1.name), left2), hydra.lib.lists.map((lambda v1: v1.name), right2)))), (lambda : join_list(hydra.lib.lists.map((lambda v1: v1.type), left2), hydra.lib.lists.map((lambda v1: v1.type), right2))), (lambda : cannot_unify()))
    def _hoist_join_row_types_body_1(l, v1):
        match v1:
            case hydra.core.TypeApplication(value=r):
                return Right((join_one(l.function, r.function), join_one(l.argument, r.argument)))

            case _:
                return cannot_unify()
    def _hoist_join_row_types_body_2(l, v1):
        match v1:
            case hydra.core.TypeEither(value=r):
                return Right((join_one(l.left, r.left), join_one(l.right, r.right)))

            case _:
                return cannot_unify()
    def _hoist_join_row_types_body_3(l, v1):
        match v1:
            case hydra.core.TypeFunction(value=r):
                return Right((join_one(l.domain, r.domain), join_one(l.codomain, r.codomain)))

            case _:
                return cannot_unify()
    def _hoist_join_row_types_body_4(l, v1):
        match v1:
            case hydra.core.TypeList(value=r):
                return Right((join_one(l, r),))

            case _:
                return cannot_unify()
    def _hoist_join_row_types_body_5(l, v1):
        match v1:
            case hydra.core.TypeMap(value=r):
                return Right((join_one(l.keys, r.keys), join_one(l.values, r.values)))

            case _:
                return cannot_unify()
    def _hoist_join_row_types_body_6(l, v1):
        match v1:
            case hydra.core.TypeMaybe(value=r):
                return Right((join_one(l, r),))

            case _:
                return cannot_unify()
    def _hoist_join_row_types_body_7(l, v1):
        match v1:
            case hydra.core.TypePair(value=r):
                return Right((join_one(l.first, r.first), join_one(l.second, r.second)))

            case _:
                return cannot_unify()
    def _hoist_join_row_types_body_8(l, v1):
        match v1:
            case hydra.core.TypeRecord(value=r):
                return join_row_types(l, r)

            case _:
                return cannot_unify()
    def _hoist_join_row_types_body_9(l, v1):
        match v1:
            case hydra.core.TypeSet(value=r):
                return Right((join_one(l, r),))

            case _:
                return cannot_unify()
    def _hoist_join_row_types_body_10(l, v1):
        match v1:
            case hydra.core.TypeUnion(value=r):
                return join_row_types(l, r)

            case _:
                return cannot_unify()
    def _hoist_join_row_types_body_11(v1):
        match v1:
            case hydra.core.TypeUnit():
                return Right(())

            case _:
                return cannot_unify()
    def _hoist_join_row_types_body_12(v1):
        match v1:
            case hydra.core.TypeVoid():
                return Right(())

            case _:
                return cannot_unify()
    def _hoist_join_row_types_body_13(l, v1):
        match v1:
            case hydra.core.TypeWrap(value=r):
                return Right((join_one(l, r),))

            case _:
                return cannot_unify()
    match sleft():
        case hydra.core.TypeApplication(value=l):
            return _hoist_join_row_types_body_1(l, sright())

        case hydra.core.TypeEither(value=l2):
            return _hoist_join_row_types_body_2(l2, sright())

        case hydra.core.TypeFunction(value=l3):
            return _hoist_join_row_types_body_3(l3, sright())

        case hydra.core.TypeList(value=l4):
            return _hoist_join_row_types_body_4(l4, sright())

        case hydra.core.TypeLiteral():
            return assert_equal()

        case hydra.core.TypeMap(value=l5):
            return _hoist_join_row_types_body_5(l5, sright())

        case hydra.core.TypeMaybe(value=l6):
            return _hoist_join_row_types_body_6(l6, sright())

        case hydra.core.TypePair(value=l7):
            return _hoist_join_row_types_body_7(l7, sright())

        case hydra.core.TypeRecord(value=l8):
            return _hoist_join_row_types_body_8(l8, sright())

        case hydra.core.TypeSet(value=l9):
            return _hoist_join_row_types_body_9(l9, sright())

        case hydra.core.TypeUnion(value=l10):
            return _hoist_join_row_types_body_10(l10, sright())

        case hydra.core.TypeUnit():
            return _hoist_join_row_types_body_11(sright())

        case hydra.core.TypeVoid():
            return _hoist_join_row_types_body_12(sright())

        case hydra.core.TypeWrap(value=l11):
            return _hoist_join_row_types_body_13(l11, sright())

        case _:
            return cannot_unify()

def variable_occurs_in_type(var: hydra.core.Name, typ0: hydra.core.Type) -> bool:
    r"""Determine whether a type variable appears within a type expression.No distinction is made between free and bound type variables."""

    def try_type(b: bool, typ: hydra.core.Type) -> bool:
        match typ:
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.logic.or_(b, hydra.lib.equality.equal(v.value, var.value))

            case _:
                return b
    return hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda x1, x2: try_type(x1, x2)), False, typ0)

def unify_type_constraints(cx: hydra.context.Context, schema_types: FrozenDict[hydra.core.Name, T0], constraints: frozenlist[hydra.typing.TypeConstraint]) -> Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst]:
    r"""Robinson's algorithm, following https://www.cs.cornell.edu/courses/cs6110/2017sp/lectures/lec23.pdf
    Specifically this is an implementation of the following rules:
      * Unify({(x, t)} ∪ E) = {t/x} Unify(E{t/x}) if x ∉ FV(t)
      * Unify(∅) = I (the identity substitution x ↦ x)
      * Unify({(x, x)} ∪ E) = Unify(E)
      * Unify({(f(s1, ..., sn), f(t1, ..., tn))} ∪ E) = Unify({(s1, t1), ..., (sn, tn)} ∪ E))."""

    def with_constraint(c: hydra.typing.TypeConstraint, rest: frozenlist[hydra.typing.TypeConstraint]):
        @lru_cache(1)
        def sleft() -> hydra.core.Type:
            return hydra.rewriting.deannotate_type(c.left)
        @lru_cache(1)
        def sright() -> hydra.core.Type:
            return hydra.rewriting.deannotate_type(c.right)
        comment = c.comment
        def bind(v: hydra.core.Name, t: hydra.core.Type) -> Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst]:
            @lru_cache(1)
            def subst() -> hydra.typing.TypeSubst:
                return hydra.substitution.singleton_type_subst(v, t)
            def with_result(s: hydra.typing.TypeSubst) -> hydra.typing.TypeSubst:
                return hydra.substitution.compose_type_subst(subst(), s)
            return hydra.lib.eithers.map((lambda x1: with_result(x1)), unify_type_constraints(cx, schema_types, hydra.substitution.substitute_in_constraints(subst(), rest)))
        def try_binding(v: hydra.core.Name, t: hydra.core.Type) -> Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst]:
            return hydra.lib.logic.if_else(variable_occurs_in_type(v, t), (lambda : Left(hydra.context.InContext(hydra.errors.UnificationError(sleft(), sright(), hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("Variable ", v.value), " appears free in type "), hydra.show.core.type(t)), " ("), comment), ")")), cx))), (lambda : bind(v, t)))
        @lru_cache(1)
        def no_vars() -> Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst]:
            def with_constraints(constraints2: frozenlist[hydra.typing.TypeConstraint]) -> Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst]:
                return unify_type_constraints(cx, schema_types, hydra.lib.lists.concat2(constraints2, rest))
            return hydra.lib.eithers.bind(join_types(cx, sleft(), sright(), comment), (lambda x1: with_constraints(x1)))
        @lru_cache(1)
        def dflt() -> Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst]:
            match sright():
                case hydra.core.TypeVariable(value=name):
                    return try_binding(name, sleft())

                case _:
                    return no_vars()
        def _hoist_dflt_body_1(name, v1):
            match v1:
                case hydra.core.TypeVariable(value=name2):
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(name.value, name2.value), (lambda : unify_type_constraints(cx, schema_types, rest)), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(hydra.lib.maps.lookup(name, schema_types)), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(hydra.lib.maps.lookup(name2, schema_types)), (lambda : Left(hydra.context.InContext(hydra.errors.UnificationError(sleft(), sright(), hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("Attempted to unify schema names ", name.value), " and "), name2.value), " ("), comment), ")")), cx))), (lambda : bind(name2, sleft())))), (lambda : bind(name, sright())))))

                case _:
                    return try_binding(name, sright())
        match sleft():
            case hydra.core.TypeVariable(value=name):
                return _hoist_dflt_body_1(name, sright())

            case _:
                return dflt()
    return hydra.lib.logic.if_else(hydra.lib.lists.null(constraints), (lambda : Right(hydra.substitution.id_type_subst())), (lambda : with_constraint(hydra.lib.lists.head(constraints), hydra.lib.lists.tail(constraints))))

def unify_type_lists(cx: hydra.context.Context, schema_types: FrozenDict[hydra.core.Name, T0], l: frozenlist[hydra.core.Type], r: frozenlist[hydra.core.Type], comment: str) -> Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst]:
    def to_constraint(l2: hydra.core.Type, r2: hydra.core.Type) -> hydra.typing.TypeConstraint:
        return hydra.typing.TypeConstraint(l2, r2, comment)
    return unify_type_constraints(cx, schema_types, hydra.lib.lists.zip_with((lambda x1, x2: to_constraint(x1, x2)), l, r))

def unify_types(cx: hydra.context.Context, schema_types: FrozenDict[hydra.core.Name, T0], l: hydra.core.Type, r: hydra.core.Type, comment: str) -> Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst]:
    return unify_type_constraints(cx, schema_types, (hydra.typing.TypeConstraint(l, r, comment),))
