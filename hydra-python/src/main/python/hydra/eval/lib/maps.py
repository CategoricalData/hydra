# Note: this is an automatically generated file. Do not edit.

r"""Evaluation-level implementations of Map functions for the Hydra interpreter."""

from __future__ import annotations
from hydra.dsl.python import FrozenDict, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.compute
import hydra.core
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.pairs
import hydra.monads
import hydra.show.core

T0 = TypeVar("T0")

def alter(fun_term: hydra.core.Term, key_term: hydra.core.Term, map_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    match map_term:
        case hydra.core.TermMap(value=m):
            def current_val() -> Maybe[hydra.core.Term]:
                return hydra.lib.maps.lookup(key_term, m)
            def new_val() -> hydra.core.Term:
                return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, cast(hydra.core.Term, hydra.core.TermMaybe(current_val())))))
            return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.maybes.maybe"))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.maps.delete"))))), key_term))), map_term)))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("newV"), Nothing(), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.maps.insert"))))), key_term))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("newV")))))), map_term))))))))))), new_val()))))
        
        case _:
            return hydra.monads.unexpected("map value", hydra.show.core.term(map_term))

def bimap(key_fun: hydra.core.Term, val_fun: hydra.core.Term, map_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    match map_term:
        case hydra.core.TermMap(value=m):
            def pairs() -> frozenlist[tuple[hydra.core.Term, hydra.core.Term]]:
                return hydra.lib.maps.to_list(m)
            return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda p: (k := hydra.lib.pairs.first(p), v := hydra.lib.pairs.second(p), (cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(key_fun, k))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(val_fun, v)))))[2]), pairs())))))
        
        case _:
            return hydra.monads.unexpected("map value", hydra.show.core.term(map_term))

def filter(val_pred: hydra.core.Term, map_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    match map_term:
        case hydra.core.TermMap(value=m):
            def pairs() -> frozenlist[tuple[hydra.core.Term, hydra.core.Term]]:
                return hydra.lib.maps.to_list(m)
            return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.maps.fromList"))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.concat"))))), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda p: (v := hydra.lib.pairs.second(p), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.logic.ifElse"))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(val_pred, v)))))), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.pure(cast(hydra.core.Term, hydra.core.TermPair((hydra.lib.pairs.first(p), v))))))))), cast(hydra.core.Term, hydra.core.TermList(()))))))[1]), pairs()))))))))))
        
        case _:
            return hydra.monads.unexpected("map value", hydra.show.core.term(map_term))

def filter_with_key(pred: hydra.core.Term, map_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    match map_term:
        case hydra.core.TermMap(value=m):
            def pairs() -> frozenlist[tuple[hydra.core.Term, hydra.core.Term]]:
                return hydra.lib.maps.to_list(m)
            return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.maps.fromList"))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.concat"))))), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda p: (k := hydra.lib.pairs.first(p), v := hydra.lib.pairs.second(p), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.logic.ifElse"))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(pred, k))), v)))))), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.pure(cast(hydra.core.Term, hydra.core.TermPair((k, v))))))))), cast(hydra.core.Term, hydra.core.TermList(()))))))[2]), pairs()))))))))))
        
        case _:
            return hydra.monads.unexpected("map value", hydra.show.core.term(map_term))

def map(val_fun: hydra.core.Term, map_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    match map_term:
        case hydra.core.TermMap(value=m):
            def pairs() -> frozenlist[tuple[hydra.core.Term, hydra.core.Term]]:
                return hydra.lib.maps.to_list(m)
            return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda p: (k := hydra.lib.pairs.first(p), v := hydra.lib.pairs.second(p), (k, cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(val_fun, v)))))[2]), pairs())))))
        
        case _:
            return hydra.monads.unexpected("map value", hydra.show.core.term(map_term))

def map_keys(key_fun: hydra.core.Term, map_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    match map_term:
        case hydra.core.TermMap(value=m):
            def pairs() -> frozenlist[tuple[hydra.core.Term, hydra.core.Term]]:
                return hydra.lib.maps.to_list(m)
            return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda p: (k := hydra.lib.pairs.first(p), v := hydra.lib.pairs.second(p), (cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(key_fun, k))), v))[2]), pairs())))))
        
        case _:
            return hydra.monads.unexpected("map value", hydra.show.core.term(map_term))
