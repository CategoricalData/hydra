# Note: this is an automatically generated file. Do not edit.

r"""Evaluation-level implementations of Logic functions for the Hydra interpreter."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Right
from typing import TypeVar, cast
import hydra.core

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def and_(cx: T0, g: T1, a: hydra.core.Term, b: hydra.core.Term) -> Either[T2, hydra.core.Term]:
    r"""Interpreter-friendly logical AND."""

    return Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.logic.ifElse"))))), a))), b))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(False))))))))

def not_(cx: T0, g: T1, a: hydra.core.Term) -> Either[T2, hydra.core.Term]:
    r"""Interpreter-friendly logical NOT."""

    return Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.logic.ifElse"))))), a))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(False))))))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True))))))))

def or_(cx: T0, g: T1, a: hydra.core.Term, b: hydra.core.Term) -> Either[T2, hydra.core.Term]:
    r"""Interpreter-friendly logical OR."""

    return Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.logic.ifElse"))))), a))), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True))))))), b))))
