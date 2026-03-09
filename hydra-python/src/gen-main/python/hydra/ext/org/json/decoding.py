# Note: this is an automatically generated file. Do not edit.

r"""Decoding functions for JSON data."""

from __future__ import annotations
from collections.abc import Callable
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
T2 = TypeVar("T2")
T3 = TypeVar("T3")
X = TypeVar("X")

def decode_array(decode_elem: Callable[[hydra.json.model.Value], Either[str, T0]], v1: hydra.json.model.Value) -> Either[str, frozenlist[T0]]:
    r"""Decode a JSON array using a decoder for elements."""
    
    match v1:
        case hydra.json.model.ValueArray(value=a):
            return hydra.lib.eithers.map_list(decode_elem, a)
        
        case _:
            return Left("expected an array")

def decode_boolean(v1: hydra.json.model.Value) -> Either[str, bool]:
    r"""Decode a JSON boolean value."""
    
    match v1:
        case hydra.json.model.ValueBoolean(value=b):
            return Right(b)
        
        case _:
            return Left("expected a boolean")

def decode_optional_field(decode_value: Callable[[T0], Either[T1, T2]], name: T3, m: FrozenDict[T3, T0]) -> Either[T0, Maybe[T2]]:
    r"""Decode an optional field from a JSON object."""
    
    return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda v: hydra.lib.eithers.map((lambda x: Just(x)), decode_value(v))), hydra.lib.maps.lookup(name, m))

def decode_field(decode_value: Callable[[T0], Either[str, T1]], name: str, m: FrozenDict[str, T0]) -> Either[str, T1]:
    r"""Decode a required field from a JSON object."""
    
    return hydra.lib.eithers.bind(decode_optional_field(decode_value, name, m), (lambda v1: hydra.lib.maybes.maybe((lambda : Left(hydra.lib.strings.cat2("missing field: ", name))), (lambda f: Right(f)), v1)))

def decode_object(v1: hydra.json.model.Value) -> Either[str, FrozenDict[str, hydra.json.model.Value]]:
    r"""Decode a JSON object value."""
    
    match v1:
        case hydra.json.model.ValueObject(value=o):
            return Right(o)
        
        case _:
            return Left("expected an object")

def decode_string(v1: hydra.json.model.Value) -> Either[str, str]:
    r"""Decode a JSON string value."""
    
    match v1:
        case hydra.json.model.ValueString(value=s):
            return Right(s)
        
        case _:
            return Left("expected a string")
