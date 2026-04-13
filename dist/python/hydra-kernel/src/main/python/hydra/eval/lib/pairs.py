# Note: this is an automatically generated file. Do not edit.

r"""Evaluation-level implementations of Pair functions for the Hydra interpreter."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Left, Right
from typing import TypeVar, cast
import hydra.core
import hydra.errors
import hydra.lib.pairs
import hydra.show.core

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def bimap(cx: T0, g: T1, first_fun: hydra.core.Term, second_fun: hydra.core.Term, pair_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly bimap for Pair terms."""

    match pair_term:
        case hydra.core.TermPair(value=p):
            @lru_cache(1)
            def fst() -> hydra.core.Term:
                return hydra.lib.pairs.first(p)
            @lru_cache(1)
            def snd() -> hydra.core.Term:
                return hydra.lib.pairs.second(p)
            return Right(cast(hydra.core.Term, hydra.core.TermPair((cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(first_fun, fst()))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(second_fun, snd())))))))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("pair value", hydra.show.core.term(pair_term)))))))

def first(cx: T0, g: T1, pair_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly first for Pair terms."""

    match pair_term:
        case hydra.core.TermPair(value=p):
            return Right(hydra.lib.pairs.first(p))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("pair value", hydra.show.core.term(pair_term)))))))

def second(cx: T0, g: T1, pair_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Interpreter-friendly second for Pair terms."""

    match pair_term:
        case hydra.core.TermPair(value=p):
            return Right(hydra.lib.pairs.second(p))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("pair value", hydra.show.core.term(pair_term)))))))
