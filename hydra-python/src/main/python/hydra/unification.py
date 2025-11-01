# Note: this is an automatically generated file. Do not edit.

r"""Utilities for type unification."""

from __future__ import annotations
from hydra.dsl.python import FrozenDict, frozenlist
from typing import cast
import hydra.coders
import hydra.compute
import hydra.core
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.optionals
import hydra.lib.strings
import hydra.rewriting
import hydra.show.core
import hydra.substitution
import hydra.typing

def join_types[T0](left: hydra.core.Type, right: hydra.core.Type, comment: str) -> hydra.compute.Flow[T0, frozenlist[hydra.typing.TypeConstraint]]:
    sleft = hydra.rewriting.deannotate_type(left)
    sright = hydra.rewriting.deannotate_type(right)
    def join_one(l: hydra.core.Type, r: hydra.core.Type) -> hydra.typing.TypeConstraint:
        return hydra.typing.TypeConstraint(l, r, hydra.lib.strings.cat(("join types; ", comment)))
    cannot_unify = hydra.lib.flows.fail(hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat(("cannot unify ", hydra.show.core.type(sleft))), " with ")), hydra.show.core.type(sright))))
    assert_equal = hydra.lib.logic.if_else(hydra.lib.equality.equal(sleft, sright), hydra.lib.flows.pure(()), cannot_unify)
    def join_list[T1](lefts: frozenlist[hydra.core.Type], rights: frozenlist[hydra.core.Type]) -> hydra.compute.Flow[T1, frozenlist[hydra.typing.TypeConstraint]]:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(lefts), hydra.lib.lists.length(rights)), hydra.lib.flows.pure(hydra.lib.lists.zip_with(join_one, lefts, rights)), cannot_unify)
    def join_row_types[T1](left2: hydra.core.RowType, right2: hydra.core.RowType) -> hydra.compute.Flow[T1, frozenlist[hydra.typing.TypeConstraint]]:
        return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.equal(left2.type_name.value, right2.type_name.value), hydra.lib.logic.and_(hydra.lib.equality.equal(hydra.lib.lists.length(hydra.lib.lists.map((lambda v1: v1.name), left2.fields)), hydra.lib.lists.length(hydra.lib.lists.map((lambda v1: v1.name), right2.fields))), hydra.lib.lists.foldl(hydra.lib.logic.and_, True, hydra.lib.lists.zip_with((lambda left3, right3: hydra.lib.equality.equal(left3.value, right3.value)), hydra.lib.lists.map((lambda v1: v1.name), left2.fields), hydra.lib.lists.map((lambda v1: v1.name), right2.fields))))), join_list(hydra.lib.lists.map((lambda v1: v1.type), left2.fields), hydra.lib.lists.map((lambda v1: v1.type), right2.fields)), cannot_unify)
    match sleft:
        case hydra.core.TypeApplication(value=l):
            match sright:
                case hydra.core.TypeApplication(value=r):
                    return hydra.lib.flows.pure((join_one(l.function, r.function), join_one(l.argument, r.argument)))
                
                case _:
                    return cannot_unify
        
        case hydra.core.TypeFunction(value=l2):
            match sright:
                case hydra.core.TypeFunction(value=r):
                    return hydra.lib.flows.pure((join_one(l2.domain, r.domain), join_one(l2.codomain, r.codomain)))
                
                case _:
                    return cannot_unify
        
        case hydra.core.TypeList(value=l3):
            match sright:
                case hydra.core.TypeList(value=r):
                    return hydra.lib.flows.pure((join_one(l3, r),))
                
                case _:
                    return cannot_unify
        
        case hydra.core.TypeLiteral():
            return assert_equal
        
        case hydra.core.TypeMap(value=l4):
            match sright:
                case hydra.core.TypeMap(value=r):
                    return hydra.lib.flows.pure((join_one(l4.keys, r.keys), join_one(l4.values, r.values)))
                
                case _:
                    return cannot_unify
        
        case hydra.core.TypeOptional(value=l5):
            match sright:
                case hydra.core.TypeOptional(value=r):
                    return hydra.lib.flows.pure((join_one(l5, r),))
                
                case _:
                    return cannot_unify
        
        case hydra.core.TypeProduct(value=l6):
            match sright:
                case hydra.core.TypeProduct(value=r):
                    return join_list(l6, r)
                
                case _:
                    return cannot_unify
        
        case hydra.core.TypeRecord(value=l7):
            match sright:
                case hydra.core.TypeRecord(value=r):
                    return join_row_types(l7, r)
                
                case _:
                    return cannot_unify
        
        case hydra.core.TypeSet(value=l8):
            match sright:
                case hydra.core.TypeSet(value=r):
                    return hydra.lib.flows.pure((join_one(l8, r),))
                
                case _:
                    return cannot_unify
        
        case hydra.core.TypeSum(value=l9):
            match sright:
                case hydra.core.TypeSum(value=r):
                    return join_list(l9, r)
                
                case _:
                    return cannot_unify
        
        case hydra.core.TypeUnion(value=l10):
            match sright:
                case hydra.core.TypeUnion(value=r):
                    return join_row_types(l10, r)
                
                case _:
                    return cannot_unify
        
        case hydra.core.TypeUnit():
            match sright:
                case hydra.core.TypeUnit():
                    return hydra.lib.flows.pure(())
                
                case _:
                    return cannot_unify
        
        case hydra.core.TypeWrap(value=l11):
            match sright:
                case hydra.core.TypeWrap(value=r):
                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(l11.type_name.value, r.type_name.value), hydra.lib.flows.pure((join_one(l11.body, r.body),)), cannot_unify)
                
                case _:
                    return cannot_unify
        
        case _:
            return cannot_unify

def variable_occurs_in_type(var: hydra.core.Name, typ0: hydra.core.Type) -> bool:
    r"""Determine whether a type variable appears within a type expression.No distinction is made between free and bound type variables."""
    
    def try_type(b: bool, typ: hydra.core.Type) -> bool:
        match typ:
            case hydra.core.TypeVariable(value=v):
                return hydra.lib.logic.or_(b, hydra.lib.equality.equal(v.value, var.value))
            
            case _:
                return b
    return hydra.rewriting.fold_over_type(cast(hydra.coders.TraversalOrder, hydra.coders.TraversalOrderPre(None)), try_type, False, typ0)

def unify_type_constraints[T0, T1](schema_types: FrozenDict[hydra.core.Name, T0], constraints: frozenlist[hydra.typing.TypeConstraint]) -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
    def with_constraint(c: hydra.typing.TypeConstraint, rest: frozenlist[hydra.typing.TypeConstraint]) -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
        sleft = hydra.rewriting.deannotate_type(c.left)
        sright = hydra.rewriting.deannotate_type(c.right)
        comment = c.comment
        def bind(v: hydra.core.Name, t: hydra.core.Type) -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
            subst = hydra.substitution.singleton_type_subst(v, t)
            def with_result(s: hydra.typing.TypeSubst) -> hydra.typing.TypeSubst:
                return hydra.substitution.compose_type_subst(subst, s)
            return hydra.lib.flows.map(with_result, unify_type_constraints(schema_types, hydra.substitution.substitute_in_constraints(subst, rest)))
        def try_binding(v: hydra.core.Name, t: hydra.core.Type) -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
            return hydra.lib.logic.if_else(variable_occurs_in_type(v, t), hydra.lib.flows.fail(hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat(("Variable ", v.value)), " appears free in type ")), hydra.show.core.type(t))), " (")), comment)), ")"))), bind(v, t))
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
        match sleft:
            case hydra.core.TypeVariable(value=name):
                match sright:
                    case hydra.core.TypeVariable(value=name2):
                        return hydra.lib.logic.if_else(hydra.lib.equality.equal(name.value, name2.value), unify_type_constraints(schema_types, rest), hydra.lib.logic.if_else(hydra.lib.optionals.is_just(hydra.lib.maps.lookup(name, schema_types)), hydra.lib.logic.if_else(hydra.lib.optionals.is_just(hydra.lib.maps.lookup(name2, schema_types)), hydra.lib.flows.fail(hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat(("Attempted to unify schema names ", name.value)), " and ")), name2.value)), " (")), comment)), ")"))), bind(name2, sleft)), bind(name, sright)))
                    
                    case _:
                        return try_binding(name, sright)
            
            case _:
                return dflt()
    return hydra.lib.logic.if_else(hydra.lib.lists.null(constraints), hydra.lib.flows.pure(hydra.substitution.id_type_subst), with_constraint(hydra.lib.lists.head(constraints), hydra.lib.lists.tail(constraints)))

def unify_type_lists[T0, T1](schema_types: FrozenDict[hydra.core.Name, T0], l: frozenlist[hydra.core.Type], r: frozenlist[hydra.core.Type], comment: str) -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
    def to_constraint(l2: hydra.core.Type, r2: hydra.core.Type) -> hydra.typing.TypeConstraint:
        return hydra.typing.TypeConstraint(l2, r2, comment)
    return unify_type_constraints(schema_types, hydra.lib.lists.zip_with(to_constraint, l, r))

def unify_types[T0, T1](schema_types: FrozenDict[hydra.core.Name, T0], l: hydra.core.Type, r: hydra.core.Type, comment: str) -> hydra.compute.Flow[T1, hydra.typing.TypeSubst]:
    return unify_type_constraints(schema_types, (hydra.typing.TypeConstraint(l, r, comment),))
