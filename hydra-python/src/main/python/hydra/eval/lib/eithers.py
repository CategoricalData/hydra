# Note: this is an automatically generated file. Do not edit.

r"""Evaluation-level implementations of Either functions for the Hydra interpreter."""

from __future__ import annotations
from hydra.dsl.python import Either, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.compute
import hydra.core
import hydra.extract.core
import hydra.graph
import hydra.lib.eithers
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.maybes
import hydra.monads
import hydra.show.core

T0 = TypeVar("T0")

def bimap(left_fun: hydra.core.Term, right_fun: hydra.core.Term, either_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    match either_term:
        case hydra.core.TermEither(value=e):
            return hydra.lib.flows.pure(hydra.lib.eithers.either((lambda val: cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Left(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(left_fun, val)))))))), (lambda val: cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(right_fun, val)))))))), e))
        
        case _:
            return hydra.monads.unexpected("either value", hydra.show.core.term(either_term))

def either(left_fun: hydra.core.Term, right_fun: hydra.core.Term, either_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    match either_term:
        case hydra.core.TermEither(value=e):
            return hydra.lib.flows.pure(hydra.lib.eithers.either((lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(left_fun, val)))), (lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(right_fun, val)))), e))
        
        case _:
            return hydra.monads.unexpected("either value", hydra.show.core.term(either_term))

def map(right_fun: hydra.core.Term, either_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    match either_term:
        case hydra.core.TermEither(value=e):
            return hydra.lib.flows.pure(hydra.lib.eithers.either((lambda val: cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Left(cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Left(val))))))))), (lambda val: cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(right_fun, val)))))))), e))
        
        case _:
            return hydra.monads.unexpected("either value", hydra.show.core.term(either_term))

def map_list(fun_term: hydra.core.Term, list_term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
    r"""Interpreter-friendly mapList for Either (traverse)."""
    
    return hydra.lib.flows.bind(hydra.extract.core.list(list_term), (lambda elements: hydra.lib.flows.pure(hydra.lib.lists.foldl((lambda acc, el: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.eithers.either"))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("err"), cast(Maybe[hydra.core.Type], Nothing()), cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Left(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("err"))))))))))))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), cast(Maybe[hydra.core.Type], Nothing()), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.eithers.either"))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("accErr"), cast(Maybe[hydra.core.Type], Nothing()), cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Left(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("accErr"))))))))))))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("ys"), cast(Maybe[hydra.core.Type], Nothing()), cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.cons"))))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("y")))))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("ys")))))))))))))))))), acc))))))))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, el))))))), cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Right(cast(hydra.core.Term, hydra.core.TermList(cast(frozenlist[hydra.core.Term], ()))))))), elements))))

def map_maybe(fun_term: hydra.core.Term, maybe_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    match maybe_term:
        case hydra.core.TermMaybe(value=opt):
            return hydra.lib.flows.pure(hydra.lib.maybes.maybe(cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Right(cast(hydra.core.Term, hydra.core.TermMaybe(cast(Maybe[hydra.core.Term], Nothing()))))))), (lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.eithers.either"))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("err"), cast(Maybe[hydra.core.Type], Nothing()), cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Left(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("err"))))))))))))))), cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), cast(Maybe[hydra.core.Type], Nothing()), cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Right(cast(hydra.core.Term, hydra.core.TermMaybe(cast(Maybe[hydra.core.Term], Just(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("y"))))))))))))))))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, val))))))), opt))
        
        case _:
            return hydra.monads.unexpected("maybe value", hydra.show.core.term(maybe_term))
