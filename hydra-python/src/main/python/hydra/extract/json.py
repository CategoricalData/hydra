# Note: this is an automatically generated file. Do not edit.

r"""Utilities for extracting values from JSON objects."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from hydra.dsl.python import FrozenDict, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.compute
import hydra.core
import hydra.json
import hydra.lib.flows
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.monads

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def show_value(value: T0) -> str:
    return "TODO: implement showValue"

def expect_array(value: hydra.json.Value) -> hydra.compute.Flow[T0, frozenlist[hydra.json.Value]]:
    match value:
        case hydra.json.ValueArray(value=els):
            return hydra.lib.flows.pure(els)
        
        case _:
            return hydra.monads.unexpected("JSON array", show_value(value))

def expect_number(value: hydra.json.Value) -> hydra.compute.Flow[T0, Decimal]:
    match value:
        case hydra.json.ValueNumber(value=d):
            return hydra.lib.flows.pure(d)
        
        case _:
            return hydra.monads.unexpected("JSON number", show_value(value))

def expect_object(value: hydra.json.Value) -> hydra.compute.Flow[T0, FrozenDict[str, hydra.json.Value]]:
    match value:
        case hydra.json.ValueObject(value=m):
            return hydra.lib.flows.pure(m)
        
        case _:
            return hydra.monads.unexpected("JSON object", show_value(value))

def expect_string(value: hydra.json.Value) -> hydra.compute.Flow[T0, str]:
    match value:
        case hydra.json.ValueString(value=s):
            return hydra.lib.flows.pure(s)
        
        case _:
            return hydra.monads.unexpected("JSON string", show_value(value))

def opt(fname: T0, m: FrozenDict[T0, T1]) -> Maybe[T1]:
    return hydra.lib.maps.lookup(fname, m)

def opt_array(fname: T0, m: FrozenDict[T0, hydra.json.Value]) -> hydra.compute.Flow[T1, Maybe[frozenlist[hydra.json.Value]]]:
    return hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[frozenlist[hydra.json.Value]], Nothing())), (lambda a: hydra.lib.flows.map(cast(Callable[[frozenlist[hydra.json.Value]], Maybe[frozenlist[hydra.json.Value]]], (lambda x1: hydra.lib.maybes.pure(x1))), expect_array(a))), opt(fname, m))

def opt_string(fname: T0, m: FrozenDict[T0, hydra.json.Value]) -> hydra.compute.Flow[T1, Maybe[str]]:
    return hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[str], Nothing())), (lambda s: hydra.lib.flows.map(cast(Callable[[str], Maybe[str]], (lambda x1: hydra.lib.maybes.pure(x1))), expect_string(s))), opt(fname, m))

def require(fname: T0, m: FrozenDict[T0, T1]) -> hydra.compute.Flow[T2, T1]:
    return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("required attribute ", show_value(fname), " not found"))), (lambda value: hydra.lib.flows.pure(value)), hydra.lib.maps.lookup(fname, m))

def require_array(fname: T0, m: FrozenDict[T0, hydra.json.Value]) -> hydra.compute.Flow[T1, frozenlist[hydra.json.Value]]:
    return hydra.lib.flows.bind(require(fname, m), cast(Callable[[hydra.json.Value], hydra.compute.Flow[T1, frozenlist[hydra.json.Value]]], (lambda x1: expect_array(x1))))

def require_number(fname: T0, m: FrozenDict[T0, hydra.json.Value]) -> hydra.compute.Flow[T1, Decimal]:
    return hydra.lib.flows.bind(require(fname, m), cast(Callable[[hydra.json.Value], hydra.compute.Flow[T1, Decimal]], (lambda x1: expect_number(x1))))

def require_string(fname: T0, m: FrozenDict[T0, hydra.json.Value]) -> hydra.compute.Flow[T1, str]:
    return hydra.lib.flows.bind(require(fname, m), cast(Callable[[hydra.json.Value], hydra.compute.Flow[T1, str]], (lambda x1: expect_string(x1))))
