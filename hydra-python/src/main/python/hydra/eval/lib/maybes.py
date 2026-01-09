# Note: this is an automatically generated file. Do not edit.

r"""Evaluation-level implementations of Maybe functions for the Hydra interpreter."""

from __future__ import annotations
from hydra.dsl.python import Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.compute
import hydra.core
import hydra.extract.core
import hydra.graph
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.maybes
import hydra.monads
import hydra.show.core

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def apply(fun_opt_term: hydra.core.Term, arg_opt_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    def _hoist_hydra_eval_lib_maybes_apply_1(arg_opt_term: hydra.core.Term, mf: Maybe[hydra.core.Term], v1: hydra.core.Term) -> hydra.compute.Flow[T1, hydra.core.Term]:
        match v1:
            case hydra.core.TermMaybe(value=mx):
                return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.bind(mf, (lambda f: hydra.lib.maybes.map((lambda x: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(f, x)))), mx))))))
            
            case _:
                return hydra.monads.unexpected("optional value", hydra.show.core.term(arg_opt_term))
    match fun_opt_term:
        case hydra.core.TermMaybe(value=mf):
            return _hoist_hydra_eval_lib_maybes_apply_1(arg_opt_term, mf, arg_opt_term)
        
        case _:
            return hydra.monads.unexpected("optional function", hydra.show.core.term(fun_opt_term))

def bind(opt_term: hydra.core.Term, fun_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    match opt_term:
        case hydra.core.TermMaybe(value=m):
            return hydra.lib.flows.pure(hydra.lib.maybes.maybe(cast(hydra.core.Term, hydra.core.TermMaybe(Nothing())), (lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, val)))), m))
        
        case _:
            return hydra.monads.unexpected("optional value", hydra.show.core.term(opt_term))

def cases(opt_term: hydra.core.Term, default_term: hydra.core.Term, fun_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    match opt_term:
        case hydra.core.TermMaybe(value=m):
            return hydra.lib.flows.pure(hydra.lib.maybes.maybe(default_term, (lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, val)))), m))
        
        case _:
            return hydra.monads.unexpected("optional value", hydra.show.core.term(opt_term))

def compose(fun_f: hydra.core.Term, fun_g: hydra.core.Term, x_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.maybes.bind"))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_f, x_term)))))), fun_g))))

def map(fun_term: hydra.core.Term, opt_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    match opt_term:
        case hydra.core.TermMaybe(value=m):
            return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, val)))), m))))
        
        case _:
            return hydra.monads.unexpected("optional value", hydra.show.core.term(opt_term))

def map_maybe(fun_term: hydra.core.Term, list_term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
    r"""Interpreter-friendly mapMaybe for List terms."""
    
    return hydra.lib.flows.bind(hydra.extract.core.list(list_term), (lambda elements: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.maybes.cat"))))), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda el: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, el)))), elements)))))))))

def maybe(default_term: hydra.core.Term, fun_term: hydra.core.Term, opt_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    match opt_term:
        case hydra.core.TermMaybe(value=m):
            return hydra.lib.flows.pure(hydra.lib.maybes.maybe(default_term, (lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, val)))), m))
        
        case _:
            return hydra.monads.unexpected("optional value", hydra.show.core.term(opt_term))
