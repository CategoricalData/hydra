# Note: this is an automatically generated file. Do not edit.

r"""Evaluation-level implementations of Either functions for the Hydra interpreter."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.errors
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.show.core

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def bimap(cx: T0, g: T1, left_fun: hydra.core.Term, right_fun: hydra.core.Term, either_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly bimap for Either terms."""

    match either_term:
        case hydra.core.TermEither(value=e):
            return Right(hydra.lib.eithers.either((lambda val: cast(hydra.core.Term, hydra.core.TermEither(Left(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(left_fun, val))))))), (lambda val: cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(right_fun, val))))))), e))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("either value", hydra.show.core.term(either_term)))))))

def bind(cx: T0, g: T1, either_term: hydra.core.Term, fun_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly bind for Either terms."""

    match either_term:
        case hydra.core.TermEither(value=e):
            return Right(hydra.lib.eithers.either((lambda val: cast(hydra.core.Term, hydra.core.TermEither(Left(val)))), (lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, val)))), e))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("either value", hydra.show.core.term(either_term)))))))

def either(cx: T0, g: T1, left_fun: hydra.core.Term, right_fun: hydra.core.Term, either_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly case analysis for Either terms."""

    match either_term:
        case hydra.core.TermEither(value=e):
            return Right(hydra.lib.eithers.either((lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(left_fun, val)))), (lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(right_fun, val)))), e))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("either value", hydra.show.core.term(either_term)))))))

def foldl(cx: T0, g: hydra.graph.Graph, fun_term: hydra.core.Term, init_term: hydra.core.Term, list_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly foldl for Either."""

    return hydra.lib.eithers.bind(hydra.extract.core.list(g, list_term), (lambda elements: Right(hydra.lib.lists.foldl((lambda acc, el: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.eithers.either"))), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("err"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Left(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("err")))))))))))), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("a"), Nothing(), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("a")))))), el))))))))), acc)))), cast(hydra.core.Term, hydra.core.TermEither(Right(init_term))), elements))))

def from_left(cx: T0, g: T1, default_term: hydra.core.Term, either_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly fromLeft for Either terms."""

    match either_term:
        case hydra.core.TermEither(value=e):
            return Right(hydra.lib.eithers.either((lambda val: val), (lambda _: default_term), e))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("either value", hydra.show.core.term(either_term)))))))

def from_right(cx: T0, g: T1, default_term: hydra.core.Term, either_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly fromRight for Either terms."""

    match either_term:
        case hydra.core.TermEither(value=e):
            return Right(hydra.lib.eithers.either((lambda _: default_term), (lambda val: val), e))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("either value", hydra.show.core.term(either_term)))))))

def is_left(cx: T0, g: T1, either_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly isLeft for Either terms."""

    match either_term:
        case hydra.core.TermEither(value=e):
            return Right(hydra.lib.eithers.either((lambda _: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True))))), (lambda _: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(False))))), e))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("either value", hydra.show.core.term(either_term)))))))

def is_right(cx: T0, g: T1, either_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly isRight for Either terms."""

    match either_term:
        case hydra.core.TermEither(value=e):
            return Right(hydra.lib.eithers.either((lambda _: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(False))))), (lambda _: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True))))), e))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("either value", hydra.show.core.term(either_term)))))))

def lefts(cx: T0, g: hydra.graph.Graph, list_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    def _hoist_hydra_eval_lib_eithers_lefts_1(acc, v1):
        match v1:
            case hydra.core.TermEither(value=e):
                return hydra.lib.eithers.either((lambda val: hydra.lib.lists.concat2(acc, hydra.lib.lists.pure(val))), (lambda _: acc), e)

            case _:
                return acc
    return hydra.lib.eithers.bind(hydra.extract.core.list(g, list_term), (lambda elements: Right(cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.foldl((lambda acc, el: _hoist_hydra_eval_lib_eithers_lefts_1(acc, el)), (), elements))))))

def map(cx: T0, g: T1, right_fun: hydra.core.Term, either_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly map for Either terms."""

    match either_term:
        case hydra.core.TermEither(value=e):
            return Right(hydra.lib.eithers.either((lambda val: cast(hydra.core.Term, hydra.core.TermEither(Left(val)))), (lambda val: cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(right_fun, val))))))), e))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("either value", hydra.show.core.term(either_term)))))))

def map_list(cx: T0, g: hydra.graph.Graph, fun_term: hydra.core.Term, list_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly mapList for Either (traverse)."""

    return hydra.lib.eithers.bind(hydra.extract.core.list(g, list_term), (lambda elements: Right(hydra.lib.lists.foldl((lambda acc, el: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.eithers.either"))), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("err"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Left(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("err")))))))))))), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("y"), Nothing(), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.eithers.either"))), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("accErr"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Left(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("accErr")))))))))))), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("ys"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.lists.cons"))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("y")))))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("ys"))))))))))))))), acc))))))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, el))))))), cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermList(()))))), hydra.lib.lists.reverse(elements)))))

def map_maybe(cx: T0, g: T1, fun_term: hydra.core.Term, maybe_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly mapMaybe for Either (traverse over Maybe)."""

    match maybe_term:
        case hydra.core.TermMaybe(value=opt):
            return Right(hydra.lib.maybes.maybe((lambda : cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermMaybe(Nothing())))))), (lambda val: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.eithers.either"))), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("err"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Left(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("err")))))))))))), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("y"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermMaybe(Just(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("y"))))))))))))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, val))))))), opt))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("maybe value", hydra.show.core.term(maybe_term)))))))

def map_set(cx: T0, g: hydra.graph.Graph, fun_term: hydra.core.Term, set_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly mapSet for Either (traverse over Set)."""

    return hydra.lib.eithers.bind(hydra.extract.core.set(g, set_term), (lambda elements: Right(hydra.lib.lists.foldl((lambda acc, el: cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.eithers.either"))), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("err"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Left(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("err")))))))))))), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("y"), Nothing(), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.eithers.either"))), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("accErr"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Left(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("accErr")))))))))))), cast(hydra.core.Term, hydra.core.TermLambda(hydra.core.Lambda(hydra.core.Name("ys"), Nothing(), cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.sets.insert"))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("y")))))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("ys"))))))))))))))), acc))))))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(fun_term, el))))))), cast(hydra.core.Term, hydra.core.TermEither(Right(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(())))))), hydra.lib.sets.to_list(elements)))))

def partition_eithers(cx: T0, g: hydra.graph.Graph, list_term: hydra.core.Term):
    r"""Interpreter-friendly partitionEithers for list of Either terms."""

    return hydra.lib.eithers.bind(hydra.extract.core.list(g, list_term), (lambda elements: Right(hydra.lib.lists.foldl((lambda acc, el: (ls := hydra.lib.pairs.first(acc), rs := hydra.lib.pairs.second(acc), _hoist_rs_body_1 := (lambda v1: (lambda e: hydra.lib.eithers.either((lambda val: (hydra.lib.lists.concat2(ls, hydra.lib.lists.pure(val)), rs)), (lambda val: (ls, hydra.lib.lists.concat2(rs, hydra.lib.lists.pure(val)))), e))(v1.value) if isinstance(v1, hydra.core.TermEither) else acc), _hoist_rs_body_1(el))[3]), ((), ()), elements))))

def rights(cx: T0, g: hydra.graph.Graph, list_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    def _hoist_hydra_eval_lib_eithers_rights_1(acc, v1):
        match v1:
            case hydra.core.TermEither(value=e):
                return hydra.lib.eithers.either((lambda _: acc), (lambda val: hydra.lib.lists.concat2(acc, hydra.lib.lists.pure(val))), e)

            case _:
                return acc
    return hydra.lib.eithers.bind(hydra.extract.core.list(g, list_term), (lambda elements: Right(cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.foldl((lambda acc, el: _hoist_hydra_eval_lib_eithers_rights_1(acc, el)), (), elements))))))
