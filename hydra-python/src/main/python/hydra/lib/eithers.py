"""Python implementations of hydra.lib.eithers primitives."""

from collections.abc import Callable
from typing import TypeVar
from hydra.dsl.python import Either, Left, Right

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")


def either[A, B, C](f: Callable[[A], C], g: Callable[[B], C], e: Either[A, B]) -> C:
    """Eliminate an Either value by applying one of two functions."""
    match e:
        case Left(val):
            return f(val)
        case Right(val):
            return g(val)


def is_left(e: Either[A, B]) -> bool:
    """Check if an Either is a Left value."""
    return isinstance(e, Left)


def is_right(e: Either[A, B]) -> bool:
    """Check if an Either is a Right value."""
    return isinstance(e, Right)


def map_left[A, B, C](f: Callable[[A], C], e: Either[A, B]) -> Either[C, B]:
    """Map a function over the Left side of an Either."""
    match e:
        case Left(val):
            return Left(f(val))
        case Right(val):
            return Right(val)


def map_right[A, B, C](f: Callable[[B], C], e: Either[A, B]) -> Either[A, C]:
    """Map a function over the Right side of an Either."""
    match e:
        case Left(val):
            return Left(val)
        case Right(val):
            return Right(f(val))


def from_left[A, B](default: A, e: Either[A, B]) -> A:
    """Extract the Left value, or return a default."""
    match e:
        case Left(val):
            return val
        case Right(_):
            return default


def from_right[A, B](default: B, e: Either[A, B]) -> B:
    """Extract the Right value, or return a default."""
    match e:
        case Left(_):
            return default
        case Right(val):
            return val


def lefts[A, B](eithers: list[Either[A, B]]) -> list[A]:
    """Extract all Left values from a list of Eithers."""
    result: list[A] = []
    for e in eithers:
        match e:
            case Left(val):
                result.append(val)
            case Right(_):
                pass
    return result


def rights[A, B](eithers: list[Either[A, B]]) -> list[B]:
    """Extract all Right values from a list of Eithers."""
    result: list[B] = []
    for e in eithers:
        match e:
            case Left(_):
                pass
            case Right(val):
                result.append(val)
    return result


def partition_eithers[A, B](eithers: list[Either[A, B]]) -> tuple[list[A], list[B]]:
    """Partition a list of Eithers into lefts and rights."""
    left_vals: list[A] = []
    right_vals: list[B] = []
    for e in eithers:
        match e:
            case Left(val):
                left_vals.append(val)
            case Right(val):
                right_vals.append(val)
    return (left_vals, right_vals)
