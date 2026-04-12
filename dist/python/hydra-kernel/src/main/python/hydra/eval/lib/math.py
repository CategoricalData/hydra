# Note: this is an automatically generated file. Do not edit.

r"""Evaluation-level implementations of Math functions for the Hydra interpreter."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Right
from typing import TypeVar, cast
import hydra.core

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def even(cx: T0, g: T1, x: hydra.core.Term) -> Either[T2, hydra.core.Term]:
    r"""Interpreter-friendly even."""

    return Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.equality.equal"))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.math.mod"))), x))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(2)))))))))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(0))))))))))

def odd(cx: T0, g: T1, x: hydra.core.Term) -> Either[T2, hydra.core.Term]:
    r"""Interpreter-friendly odd."""

    return Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.logic.not"))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.math.even"))), x)))))))

def pred(cx: T0, g: T1, x: hydra.core.Term) -> Either[T2, hydra.core.Term]:
    r"""Interpreter-friendly predecessor."""

    return Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.math.sub"))), x))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(1))))))))))

def succ(cx: T0, g: T1, x: hydra.core.Term) -> Either[T2, hydra.core.Term]:
    r"""Interpreter-friendly successor."""

    return Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.lib.math.add"))), x))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(1))))))))))
