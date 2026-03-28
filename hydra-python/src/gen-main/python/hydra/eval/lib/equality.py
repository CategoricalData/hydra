# Note: this is an automatically generated file. Do not edit.

r"""Evaluation-level implementations of Equality functions for the Hydra interpreter."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Right
from typing import TypeVar, cast
import hydra.core

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def identity(cx: T0, g: T1, x: T2) -> Either[T3, T2]:
    r"""Interpreter-friendly identity function."""

    return Right(x)

def max(cx: T0, g: T1, x: hydra.core.Term, y: hydra.core.Term) -> Either[T2, hydra.core.Term]:
    r"""Interpreter-friendly max."""

    return Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.logic.ifElse"))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.equality.gte"))))), x))), y)))))), x))), y))))

def min(cx: T0, g: T1, x: hydra.core.Term, y: hydra.core.Term) -> Either[T2, hydra.core.Term]:
    r"""Interpreter-friendly min."""

    return Right(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.logic.ifElse"))))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.equality.lte"))))), x))), y)))))), x))), y))))
