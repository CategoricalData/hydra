# Note: this is an automatically generated file. Do not edit.

r"""Decoding functions for JSON data."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.compute
import hydra.core
import hydra.json.model
import hydra.lib.flows
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def decode_array(decode_elem: Callable[[hydra.json.model.Value], hydra.compute.Flow[T0, T1]], v1: hydra.json.model.Value) -> hydra.compute.Flow[T0, frozenlist[T1]]:
    match v1:
        case hydra.json.model.ValueArray(value=a):
            return hydra.lib.flows.map_list(decode_elem, a)
        
        case _:
            return hydra.lib.flows.fail("expected an array")

def decode_boolean(v1: hydra.json.model.Value) -> hydra.compute.Flow[T0, bool]:
    match v1:
        case hydra.json.model.ValueBoolean(value=b):
            return hydra.lib.flows.pure(b)
        
        case _:
            return hydra.lib.flows.fail("expected a boolean")

def decode_optional_field(decode_value: Callable[[T0], hydra.compute.Flow[T1, T2]], name: T3, m: FrozenDict[T3, T0]) -> hydra.compute.Flow[T1, Maybe[T2]]:
    return hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[T2], Nothing())), (lambda v: hydra.lib.flows.map((lambda x: cast(Maybe[T2], Just(x))), decode_value(v))), hydra.lib.maps.lookup(name, m))

def decode_field(decode_value: Callable[[T0], hydra.compute.Flow[T1, T2]], name: str, m: FrozenDict[str, T0]) -> hydra.compute.Flow[T1, T2]:
    return hydra.lib.flows.bind(decode_optional_field(decode_value, name, m), (lambda v1: hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("missing field: ", name)), (lambda f: hydra.lib.flows.pure(f)), v1)))

def decode_object(v1: hydra.json.model.Value) -> hydra.compute.Flow[T0, FrozenDict[str, hydra.json.model.Value]]:
    match v1:
        case hydra.json.model.ValueObject(value=o):
            return hydra.lib.flows.pure(o)
        
        case _:
            return hydra.lib.flows.fail("expected an object")

def decode_string(v1: hydra.json.model.Value) -> hydra.compute.Flow[T0, str]:
    match v1:
        case hydra.json.model.ValueString(value=s):
            return hydra.lib.flows.pure(s)
        
        case _:
            return hydra.lib.flows.fail("expected a string")
