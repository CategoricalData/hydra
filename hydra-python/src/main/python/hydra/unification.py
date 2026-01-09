# Note: this is an automatically generated file. Do not edit.

r"""Utilities for type unification."""

from __future__ import annotations
from hydra.dsl.python import FrozenDict, frozenlist
from typing import TypeVar
import hydra.coders
import hydra.compute
import hydra.core
import hydra.lib.equality
import hydra.lib.flows
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
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def join_types(left: hydra.core.Type, right: hydra.core.Type, comment: str) -> hydra.compute.Flow[T0, frozenlist[hydra.typing.TypeConstraint]]:
    sleft = hydra.rewriting.deannotate_type(left)
    sright = hydra.rewriting.deannotate_type(right)
    def join_one(l: hydra.core.Type, r: hydra.core.Type) -> hydra.core.Type:
        return hydra.typing.TypeConstraint(l, r, hydra.lib.strings.cat2("join types; ", comment))
    def cannot_unify() -> hydra.compute.Flow[T1, T2]:
        return hydra.lib.flows.fail(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("cannot unify ", hydra.show.core.type(sleft)), " with "), hydra.show.core.type(sright)))
    def assert_equal() -> hydra.compute.Flow[T1, frozenlist[T2]]:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(sleft, sright), (lambda : hydra.lib.flows.pure(())), (lambda : cannot_unify()))
    def join_list(lefts: frozenlist[hydra.core.Type], rights: frozenlist[hydra.core.Type]) -> hydra.compute.Flow[T1, frozenlist[hydra.typing.TypeConstraint]]:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(lefts), hydra.lib.lists.length(rights)), (lambda : hydra.lib.flows.pure(hydra.lib.lists.zip_with(join_one, lefts, rights))), (lambda : cannot_unify()))
    def join_row_types(left2: hydra.core.RowType, right2: hydra.core.RowType) -> hydra.compute.Flow[T1, frozenlist[hydra.typing.TypeConstraint]]:
        return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(left2.type_name.value, right2.type_name.value), hydra.lib.logic.and_(hydra.lib.equality.equal(hydra.lib.lists.length(hydra.lib.lists.map((lambda v1: v1.name), left2.fields)), hydra.lib.lists.length(hydra.lib.lists.map((lambda v1: v1.name), right2.fields))), hydra.lib.lists.foldl(hydra.lib.logic.and_, True, hydra.lib.lists.zip_with((lambda left3, right3: hydra.lib.equality.equal(left3.value, right3.value)), hydra.lib.lists.map((lambda v1: v1.name), left2.fields), hydra.lib.lists.map((lambda v1: v1.name), right2.fields))))), (lambda : join_list(hydra.lib.lists.map((lambda v1: v1.type), left2.fields), hydra.lib.lists.map((lambda v1: v1.type), right2.fields))), (lambda : cannot_unify()))
    def _hoist_body_1(l: hydra.core.ApplicationType, v1: hydra.core.Type) -> hydra.compute.Flow[T1, frozenlist[hydra.typing.TypeConstraint]]:
        match v1:
            case hydra.core.TypeApplication(value=r):
                return hydra.lib.flows.pure((join_one(l.function, r.function), join_one(l.argument, r.argument)))
            
            case _:
                return cannot_unify()
    def _hoist_body_2(l: hydra.core.EitherType, v1: hydra.core.Type) -> hydra.compute.Flow[T1, frozenlist[hydra.typing.TypeConstraint]]:
        match v1:
            case hydra.core.TypeEither(value=r):
                return hydra.lib.flows.pure((join_one(l.left, r.left), join_one(l.right, r.right)))
            
            case _:
                return cannot_unify()
    def _hoist_body_3(l: hydra.core.FunctionType, v1: hydra.core.Type) -> hydra.compute.Flow[T1, frozenlist[hydra.typing.TypeConstraint]]:
        match v1:
            case hydra.core.TypeFunction(value=r):
                return hydra.lib.flows.pure((join_one(l.domain, r.domain), join_one(l.codomain, r.codomain)))
            
            case _:
                return cannot_unify()
    def _hoist_body_4(l: hydra.core.Type, v1: hydra.core.Type) -> hydra.compute.Flow[T1, frozenlist[hydra.typing.TypeConstraint]]:
        match v1:
            case hydra.core.TypeList(value=r):
                return hydra.lib.flows.pure((join_one(l, r),))
            
            case _:
                return cannot_unify()
    def _hoist_body_5(l: hydra.core.MapType, v1: hydra.core.Type) -> hydra.compute.Flow[T1, frozenlist[hydra.typing.TypeConstraint]]:
        match v1:
            case hydra.core.TypeMap(value=r):
                return hydra.lib.flows.pure((join_one(l.keys, r.keys), join_one(l.values, r.values)))
            
            case _:
                return cannot_unify()
    def _hoist_body_6(l: hydra.core.Type, v1: hydra.core.Type) -> hydra.compute.Flow[T1, frozenlist[hydra.typing.TypeConstraint]]:
        match v1:
            case hydra.core.TypeMaybe(value=r):
                return hydra.lib.flows.pure((join_one(l, r),))
            
            case _:
                return cannot_unify()
    def _hoist_body_7(l: hydra.core.PairType, v1: hydra.core.Type) -> hydra.compute.Flow[T1, frozenlist[hydra.typing.TypeConstraint]]:
        match v1:
            case hydra.core.TypePair(value=r):
                return hydra.lib.flows.pure((join_one(l.first, r.first), join_one(l.second, r.second)))
            
            case _:
                return cannot_unify()
    def _hoist_body_8(l: hydra.core.RowType, v1: hydra.core.Type) -> hydra.compute.Flow[T1, frozenlist[hydra.typing.TypeConstraint]]:
        match v1:
            case hydra.core.TypeRecord(value=r):
                return join_row_types(l, r)
            
            case _:
                return cannot_unify()
    def _hoist_body_9(l: hydra.core.Type, v1: hydra.core.Type) -> hydra.compute.Flow[T1, frozenlist[hydra.typing.TypeConstraint]]:
        match v1:
            case hydra.core.TypeSet(value=r):
                return hydra.lib.flows.pure((join_one(l, r),))
            
            case _:
                return cannot_unify()
    def _hoist_body_10(l: hydra.core.RowType, v1: hydra.core.Type) -> hydra.compute.Flow[T1, frozenlist[hydra.typing.TypeConstraint]]:
        match v1:
            case hydra.core.TypeUnion(value=r):
                return join_row_types(l, r)
            
            case _:
                return cannot_unify()
    def _hoist_body_11(v1: hydra.core.Type) -> hydra.compute.Flow[T1, frozenlist[T2]]:
        match v1:
            case hydra.core.TypeUnit():
                return hydra.lib.flows.pure(())
            
            case _:
                return cannot_unify()
    def _hoist_body_12(l: hydra.core.WrappedType, v1: hydra.core.Type) -> hydra.compute.Flow[T1, frozenlist[hydra.typing.TypeConstraint]]:
        match v1:
            case hydra.core.TypeWrap(value=r):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(l.type_name.value, r.type_name.value), (lambda : hydra.lib.flows.pure((join_one(l.body, r.body),))), (lambda : cannot_unify()))
            
            case _:
                return cannot_unify()
    match sleft:
        case hydra.core.TypeApplication(value=l):
            return _hoist_body_1(l, sright)
        
        case hydra.core.TypeEither(value=l2):
            return _hoist_body_2(l2, sright)
        
        case hydra.core.TypeFunction(value=l3):
            return _hoist_body_3(l3, sright)
        
        case hydra.core.TypeList(value=l4):
            return _hoist_body_4(l4, sright)
        
        case hydra.core.TypeLiteral():
            return assert_equal()
        
        case hydra.core.TypeMap(value=l5):
            return _hoist_body_5(l5, sright)
        
        case hydra.core.TypeMaybe(value=l6):
            return _hoist_body_6(l6, sright)
        
        case hydra.core.TypePair(value=l7):
            return _hoist_body_7(l7, sright)
        
        case hydra.core.TypeRecord(value=l8):
            return _hoist_body_8(l8, sright)
        
        case hydra.core.TypeSet(value=l9):
            return _hoist_body_9(l9, sright)
        
        case hydra.core.TypeUnion(value=l10):
            return _hoist_body_10(l10, sright)
        
        case hydra.core.TypeUnit():
            return _hoist_body_11(sright)
        
        case hydra.core.TypeWrap(value=l11):
            return _hoist_body_12(l11, sright)
        
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
    return hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, try_type, False, typ0)

def unify_type_constraints(schema_types: FrozenDict[hydra.core.Name, T0], constraints: frozenlist[hydra.typing.TypeConstraint]) -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
    def with_constraint(c: hydra.typing.TypeConstraint, rest: frozenlist[hydra.typing.TypeConstraint]) -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
        sleft = hydra.rewriting.deannotate_type(c.left)
        sright = hydra.rewriting.deannotate_type(c.right)
        comment = c.comment
        def bind(v: hydra.core.Name, t: hydra.core.Type) -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
            subst = hydra.substitution.singleton_type_subst(v, t)
            def with_result(s: hydra.typing.TypeSubst) -> hydra.core.Type:
                return hydra.substitution.compose_type_subst(subst, s)
            return hydra.lib.flows.map(with_result, unify_type_constraints(schema_types, hydra.substitution.substitute_in_constraints(subst, rest)))
        def try_binding(v: hydra.core.Name, t: hydra.core.Type) -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
            return hydra.lib.logic.if_else(variable_occurs_in_type(v, t), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("Variable ", v.value), " appears free in type "), hydra.show.core.type(t)), " ("), comment), ")"))), (lambda : bind(v, t)))
        def no_vars() -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
            def with_constraints(constraints2: frozenlist[hydra.typing.TypeConstraint]) -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
                return unify_type_constraints(schema_types, hydra.lib.lists.concat2(constraints2, rest))
            return hydra.lib.flows.bind(join_types(sleft, sright, comment), with_constraints)
        def dflt() -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
            match sright:
                case hydra.core.TypeVariable(value=name):
                    return try_binding(name, sleft)
                
                case _:
                    return no_vars()
        def _hoist_body_1(name: hydra.core.Name, v1: hydra.core.Type) -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
            match v1:
                case hydra.core.TypeVariable(value=name2):
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(name.value, name2.value), (lambda : unify_type_constraints(schema_types, rest)), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(hydra.lib.maps.lookup(name, schema_types)), (lambda : hydra.lib.logic.if_else(hydra.lib.maybes.is_just(hydra.lib.maps.lookup(name2, schema_types)), (lambda : hydra.lib.flows.fail(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("Attempted to unify schema names ", name.value), " and "), name2.value), " ("), comment), ")"))), (lambda : bind(name2, sleft)))), (lambda : bind(name, sright)))))
                
                case _:
                    return try_binding(name, sright)
        match sleft:
            case hydra.core.TypeVariable(value=name):
                return _hoist_body_1(name, sright)
            
            case _:
                return dflt()
    return hydra.lib.logic.if_else(hydra.lib.lists.null(constraints), (lambda : hydra.lib.flows.pure(hydra.substitution.id_type_subst())), (lambda : with_constraint(hydra.lib.lists.head(constraints), hydra.lib.lists.tail(constraints))))

def unify_type_lists(schema_types: FrozenDict[hydra.core.Name, T0], l: frozenlist[hydra.core.Type], r: frozenlist[hydra.core.Type], comment: str) -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
    def to_constraint(l2: hydra.core.Type, r2: hydra.core.Type) -> hydra.core.Type:
        return hydra.typing.TypeConstraint(l2, r2, comment)
    return unify_type_constraints(schema_types, hydra.lib.lists.zip_with(to_constraint, l, r))

def unify_types(schema_types: FrozenDict[hydra.core.Name, T0], l: hydra.core.Type, r: hydra.core.Type, comment: str) -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
    return unify_type_constraints(schema_types, (hydra.typing.TypeConstraint(l, r, comment),))
