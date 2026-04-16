# Note: this is an automatically generated file. Do not edit.

r"""Utilities for extracting values from JSON objects."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.json.model
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def show_value(value: T0) -> str:
    r"""Show a JSON value as a string (placeholder implementation)."""

    return "TODO: implement showValue"

def expect_array(value: hydra.json.model.Value) -> Either[str, frozenlist[hydra.json.model.Value]]:
    r"""Extract an array from a JSON value, failing if the value is not an array."""

    match value:
        case hydra.json.model.ValueArray(value=els):
            return Right(els)

        case _:
            return Left(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "JSON array"), hydra.lib.strings.cat2(" but found ", show_value(value))))

def expect_number(value: hydra.json.model.Value) -> Either[str, Decimal]:
    r"""Extract a number from a JSON value, failing if the value is not a number."""

    match value:
        case hydra.json.model.ValueNumber(value=d):
            return Right(d)

        case _:
            return Left(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "JSON number"), hydra.lib.strings.cat2(" but found ", show_value(value))))

def expect_object(value: hydra.json.model.Value) -> Either[str, FrozenDict[str, hydra.json.model.Value]]:
    r"""Extract an object from a JSON value, failing if the value is not an object."""

    match value:
        case hydra.json.model.ValueObject(value=m):
            return Right(m)

        case _:
            return Left(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "JSON object"), hydra.lib.strings.cat2(" but found ", show_value(value))))

def expect_string(value: hydra.json.model.Value) -> Either[str, str]:
    r"""Extract a string from a JSON value, failing if the value is not a string."""

    match value:
        case hydra.json.model.ValueString(value=s):
            return Right(s)

        case _:
            return Left(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", "JSON string"), hydra.lib.strings.cat2(" but found ", show_value(value))))

def opt(fname: T0, m: FrozenDict[T0, T1]) -> Maybe[T1]:
    r"""Look up an optional field in a JSON object."""

    return hydra.lib.maps.lookup(fname, m)

def opt_array(fname: T0, m: FrozenDict[T0, hydra.json.model.Value]) -> Either[str, Maybe[frozenlist[hydra.json.model.Value]]]:
    r"""Look up an optional array field in a JSON object."""

    return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda a: hydra.lib.eithers.map((lambda x: Just(x)), expect_array(a))), opt(fname, m))

def opt_string(fname: T0, m: FrozenDict[T0, hydra.json.model.Value]) -> Either[str, Maybe[str]]:
    r"""Look up an optional string field in a JSON object."""

    return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda s: hydra.lib.eithers.map((lambda x: Just(x)), expect_string(s))), opt(fname, m))

def require(fname: T0, m: FrozenDict[T0, T1]) -> Either[str, T1]:
    r"""Look up a required field in a JSON object, failing if not found."""

    return hydra.lib.maybes.maybe((lambda : Left(hydra.lib.strings.cat(("required attribute ", show_value(fname), " not found")))), (lambda value: Right(value)), hydra.lib.maps.lookup(fname, m))

def require_array(fname: T0, m: FrozenDict[T0, hydra.json.model.Value]) -> Either[str, frozenlist[hydra.json.model.Value]]:
    r"""Look up a required array field in a JSON object."""

    return hydra.lib.eithers.bind(require(fname, m), (lambda x1: expect_array(x1)))

def require_number(fname: T0, m: FrozenDict[T0, hydra.json.model.Value]) -> Either[str, Decimal]:
    r"""Look up a required number field in a JSON object."""

    return hydra.lib.eithers.bind(require(fname, m), (lambda x1: expect_number(x1)))

def require_string(fname: T0, m: FrozenDict[T0, hydra.json.model.Value]) -> Either[str, str]:
    r"""Look up a required string field in a JSON object."""

    return hydra.lib.eithers.bind(require(fname, m), (lambda x1: expect_string(x1)))
