# Note: this is an automatically generated file. Do not edit.

r"""Evaluation-level implementations of Either functions for the Hydra interpreter."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.context
import hydra.core
import hydra.error
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maybes
import hydra.lib.sets
import hydra.lib.strings
import hydra.show.core

T0 = TypeVar("T0")

def bimap(cx: hydra.context.Context, g: T0, left_fun: hydra.core.Term, right_fun: hydra.core.Term, either_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
    r"""Interpreter-friendly bimap for Either terms."""
    
    match either_term:
        case hydra.core.TermEither(value=e):
            return Right(hydra.lib.eithers.either((lambda val: cast(hydra.core.Term, hydra.core.TermEither(Left(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(left_fun, val))))))), (lambda val: cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(right_fun, val))))))), e))
        
        case _:
            return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "either value"), " but found "), hydra.show.core.term(either_term))), cx))

def bind(cx: hydra.context.Context, g: T0, either_term: hydra.core.Term, fun_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
    r"""Interpreter-friendly bind for Either terms."""
    
    match either_term:
        case hydra.core.TermEither(value=e):
            return Right(hydra.lib.eithers.either((lambda val: cast(hydra.core.Term, hydra.core.TermEither(Left(val)))), (lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, val)))), e))
        
        case _:
            return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "either value"), " but found "), hydra.show.core.term(either_term))), cx))

def either(cx: hydra.context.Context, g: T0, left_fun: hydra.core.Term, right_fun: hydra.core.Term, either_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
    r"""Interpreter-friendly case analysis for Either terms."""
    
    match either_term:
        case hydra.core.TermEither(value=e):
            return Right(hydra.lib.eithers.either((lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(left_fun, val)))), (lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(right_fun, val)))), e))
        
        case _:
            return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "either value"), " but found "), hydra.show.core.term(either_term))), cx))

def foldl(cx: hydra.context.Context, g: hydra.graph.Graph, fun_term: hydra.core.Term, init_term: hydra.core.Term, list_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
    r"""Interpreter-friendly foldl for Either."""
    
    return hydra.lib.eithers.bind(hydra.extract.core.list(cx, g, list_term), (lambda elements: Right(hydra.lib.lists.foldl((lambda acc, el: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.eithers.either"))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("err"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Left(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("err")))))))))))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("a"), Nothing(), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("a")))))), el))))))))))), acc)))), cast(hydra.core.Term, hydra.core.TermEither(Right(init_term))), elements))))

def map(cx: hydra.context.Context, g: T0, right_fun: hydra.core.Term, either_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
    r"""Interpreter-friendly map for Either terms."""
    
    match either_term:
        case hydra.core.TermEither(value=e):
            return Right(hydra.lib.eithers.either((lambda val: cast(hydra.core.Term, hydra.core.TermEither(Left(val)))), (lambda val: cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(right_fun, val))))))), e))
        
        case _:
            return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "either value"), " but found "), hydra.show.core.term(either_term))), cx))

def map_list(cx: hydra.context.Context, g: hydra.graph.Graph, fun_term: hydra.core.Term, list_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
    r"""Interpreter-friendly mapList for Either (traverse)."""
    
    return hydra.lib.eithers.bind(hydra.extract.core.list(cx, g, list_term), (lambda elements: Right(hydra.lib.lists.foldl((lambda acc, el: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.eithers.either"))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("err"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Left(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("err")))))))))))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), Nothing(), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.eithers.either"))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("accErr"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Left(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("accErr")))))))))))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("ys"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.cons"))))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("y")))))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("ys"))))))))))))))))), acc))))))))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, el))))))), cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermList(()))))), hydra.lib.lists.reverse(elements)))))

def map_maybe(cx: hydra.context.Context, g: T0, fun_term: hydra.core.Term, maybe_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
    r"""Interpreter-friendly mapMaybe for Either (traverse over Maybe)."""
    
    match maybe_term:
        case hydra.core.TermMaybe(value=opt):
            return Right(hydra.lib.maybes.maybe(cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermMaybe(Nothing()))))), (lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.eithers.either"))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("err"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Left(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("err")))))))))))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermMaybe(Just(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("y"))))))))))))))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, val))))))), opt))
        
        case _:
            return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "maybe value"), " but found "), hydra.show.core.term(maybe_term))), cx))

def map_set(cx: hydra.context.Context, g: hydra.graph.Graph, fun_term: hydra.core.Term, set_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
    r"""Interpreter-friendly mapSet for Either (traverse over Set)."""
    
    return hydra.lib.eithers.bind(hydra.extract.core.set(cx, g, set_term), (lambda elements: Right(hydra.lib.lists.foldl((lambda acc, el: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.eithers.either"))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("err"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Left(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("err")))))))))))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), Nothing(), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.eithers.either"))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("accErr"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Left(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("accErr")))))))))))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("ys"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.sets.insert"))))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("y")))))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("ys"))))))))))))))))), acc))))))))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, el))))))), cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(())))))), hydra.lib.sets.to_list(elements)))))
