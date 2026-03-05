# Note: this is an automatically generated file. Do not edit.

r"""Evaluation-level implementations of Maybe functions for the Hydra interpreter."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.context
import hydra.core
import hydra.error
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maybes
import hydra.lib.strings
import hydra.show.core

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def apply(cx: hydra.context.Context, g: T0, fun_opt_term: hydra.core.Term, arg_opt_term: hydra.core.Term):
    def _hoist_hydra_eval_lib_maybes_apply_1(arg_opt_term, cx, mf, v1):
        match v1:
            case hydra.core.TermMaybe(value=mx):
                return Right(cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.bind(mf, (lambda f: hydra.lib.maybes.map((lambda x: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(f, x)))), mx))))))
            
            case _:
                return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "optional value"), " but found "), hydra.show.core.term(arg_opt_term))), cx))
    match fun_opt_term:
        case hydra.core.TermMaybe(value=mf):
            return _hoist_hydra_eval_lib_maybes_apply_1(arg_opt_term, cx, mf, arg_opt_term)
        
        case _:
            return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "optional function"), " but found "), hydra.show.core.term(fun_opt_term))), cx))

def bind(cx: hydra.context.Context, g: T0, opt_term: hydra.core.Term, fun_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
    r"""Interpreter-friendly monadic bind for Maybe terms."""
    
    match opt_term:
        case hydra.core.TermMaybe(value=m):
            return Right(hydra.lib.maybes.maybe(cast(hydra.core.Term, hydra.core.TermMaybe(Nothing())), (lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, val)))), m))
        
        case _:
            return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "optional value"), " but found "), hydra.show.core.term(opt_term))), cx))

def cases(cx: hydra.context.Context, g: T0, opt_term: hydra.core.Term, default_term: hydra.core.Term, fun_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
    r"""Interpreter-friendly case analysis for Maybe terms (cases argument order)."""
    
    match opt_term:
        case hydra.core.TermMaybe(value=m):
            return Right(hydra.lib.maybes.maybe(default_term, (lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, val)))), m))
        
        case _:
            return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "optional value"), " but found "), hydra.show.core.term(opt_term))), cx))

def compose(cx: T0, g: T1, fun_f: hydra.core.Term, fun_g: hydra.core.Term, x_term: hydra.core.Term) -> Either[T2, hydra.core.Term]:
    r"""Interpreter-friendly Kleisli composition for Maybe."""
    
    return Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.maybes.bind"))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_f, x_term)))))), fun_g))))

def map(cx: hydra.context.Context, g: T0, fun_term: hydra.core.Term, opt_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
    r"""Interpreter-friendly map for Maybe terms."""
    
    match opt_term:
        case hydra.core.TermMaybe(value=m):
            return Right(cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, val)))), m))))
        
        case _:
            return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "optional value"), " but found "), hydra.show.core.term(opt_term))), cx))

def map_maybe(cx: hydra.context.Context, g: hydra.graph.Graph, fun_term: hydra.core.Term, list_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
    r"""Interpreter-friendly mapMaybe for List terms."""
    
    return hydra.lib.eithers.bind(hydra.extract.core.list(cx, g, list_term), (lambda elements: Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.maybes.cat"))))), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda el: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, el)))), elements)))))))))

def maybe(cx: hydra.context.Context, g: T0, default_term: hydra.core.Term, fun_term: hydra.core.Term, opt_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
    r"""Interpreter-friendly case analysis for Maybe terms."""
    
    match opt_term:
        case hydra.core.TermMaybe(value=m):
            return Right(hydra.lib.maybes.maybe(default_term, (lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, val)))), m))
        
        case _:
            return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "optional value"), " but found "), hydra.show.core.term(opt_term))), cx))
