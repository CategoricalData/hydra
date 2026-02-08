# Note: this is an automatically generated file. Do not edit.
# hydra.lib.flows primitives

from __future__ import annotations
from typing import cast
from decimal import Decimal
from hydra.dsl.python import FrozenDict, frozenlist, Either, Left, Right, Maybe, Just, Nothing
import hydra.accessors
import hydra.annotations
import hydra.ast
import hydra.classes
import hydra.coders
import hydra.compute
import hydra.constants
import hydra.core
import hydra.decode.core
import hydra.encode.core
import hydra.ext.haskell.operators
import hydra.extract.core
import hydra.extract.helpers
import hydra.formatting
import hydra.graph
import hydra.json.model
import hydra.lexical
import hydra.lib.chars
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.monads
import hydra.names
import hydra.parsing
import hydra.rewriting
import hydra.serialization
import hydra.show.core
import hydra.sorting
import hydra.tarjan
import hydra.testing
import hydra.topology
import hydra.typing
import hydra.util

# apply

def test_apply__apply_add():

    assert (hydra.monads.bind(hydra.monads.pure((lambda x1: hydra.lib.math.add(3, x1))), (lambda f: hydra.monads.bind(hydra.monads.pure(5), (lambda x: hydra.monads.pure(f(x)))))).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(8), None, hydra.compute.Trace((), (), FrozenDict({}))))

# bind

def test_bind__bind_add():

    assert (hydra.monads.bind(hydra.monads.pure(5), (lambda n: hydra.monads.pure(hydra.lib.math.add(n, 5)))).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(10), None, hydra.compute.Trace((), (), FrozenDict({}))))

def test_bind__bind_multiply():

    assert (hydra.monads.bind(hydra.monads.pure(3), (lambda n: hydra.monads.pure(hydra.lib.math.mul(n, 4)))).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(12), None, hydra.compute.Trace((), (), FrozenDict({}))))

# fail

def test_fail__fail_with_message():

    assert (hydra.monads.fail("test error message").value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Nothing(), None, hydra.compute.Trace((), ("Error: test error message ()",), FrozenDict({}))))

# foldl

def test_foldl__foldl_sum():

    assert (hydra.monads.bind(hydra.monads.pure(0), (lambda a0: hydra.monads.bind(hydra.monads.pure(hydra.lib.math.add(a0, 1)), (lambda a1: hydra.monads.bind(hydra.monads.pure(hydra.lib.math.add(a1, 2)), (lambda a2: hydra.monads.pure(hydra.lib.math.add(a2, 3)))))))).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(6), None, hydra.compute.Trace((), (), FrozenDict({}))))

# map

def test_map__map_negate():

    assert (hydra.lib.flows.map(hydra.lib.math.negate, hydra.monads.pure(5)).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(-5), None, hydra.compute.Trace((), (), FrozenDict({}))))

def test_map__map_abs():

    assert (hydra.lib.flows.map(hydra.lib.math.abs, hydra.monads.pure(-3)).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(3), None, hydra.compute.Trace((), (), FrozenDict({}))))

# mapElems

def test_mapelems__mapelems_add_one():

    assert (hydra.monads.bind(hydra.monads.pure(hydra.lib.math.add(1, 1)), (lambda v1: hydra.monads.bind(hydra.monads.pure(hydra.lib.math.add(2, 1)), (lambda v2: hydra.monads.pure(hydra.lib.maps.from_list((("a", v1), ("b", v2)))))))).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(FrozenDict({
  "a": 2,
  "b": 3})), None, hydra.compute.Trace((), (), FrozenDict({}))))

# mapKeys

def test_mapkeys__mapkeys_add_one():

    assert (hydra.monads.bind(hydra.monads.pure(hydra.lib.math.add(1, 1)), (lambda k1: hydra.monads.bind(hydra.monads.pure(hydra.lib.math.add(2, 1)), (lambda k2: hydra.monads.pure(hydra.lib.maps.from_list(((k1, "a"), (k2, "b")))))))).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(FrozenDict({
  2: "a",
  3: "b"})), None, hydra.compute.Trace((), (), FrozenDict({}))))

# mapList

def test_maplist__maplist_add_one():

    assert (hydra.monads.bind(hydra.monads.pure(hydra.lib.math.add(1, 1)), (lambda y1: hydra.monads.bind(hydra.monads.pure(hydra.lib.math.add(2, 1)), (lambda y2: hydra.monads.bind(hydra.monads.pure(hydra.lib.math.add(3, 1)), (lambda y3: hydra.monads.pure((y1, y2, y3)))))))).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just((2, 3, 4)), None, hydra.compute.Trace((), (), FrozenDict({}))))

# mapMaybe

def test_mapmaybe__mapmaybe_just():

    assert (hydra.monads.bind(hydra.monads.pure(hydra.lib.math.add(5, 1)), (lambda y: hydra.monads.pure(Just(y)))).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(Just(6)), None, hydra.compute.Trace((), (), FrozenDict({}))))

def test_mapmaybe__mapmaybe_nothing():

    assert (hydra.monads.pure(Nothing()).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(Nothing()), None, hydra.compute.Trace((), (), FrozenDict({}))))

# mapSet

def test_mapset__mapset_add_one():

    assert (hydra.monads.bind(hydra.monads.pure(hydra.lib.math.add(1, 1)), (lambda y1: hydra.monads.bind(hydra.monads.pure(hydra.lib.math.add(2, 1)), (lambda y2: hydra.monads.bind(hydra.monads.pure(hydra.lib.math.add(3, 1)), (lambda y3: hydra.monads.pure(hydra.lib.sets.from_list((y1, y2, y3))))))))).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(frozenset({2, 3, 4})), None, hydra.compute.Trace((), (), FrozenDict({}))))

# pure

def test_pure__pure_integer():

    assert (hydra.monads.pure(42).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(42), None, hydra.compute.Trace((), (), FrozenDict({}))))

def test_pure__pure_zero():

    assert (hydra.monads.pure(0).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(0), None, hydra.compute.Trace((), (), FrozenDict({}))))

def test_pure__pure_negative():

    assert (hydra.monads.pure(-5).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(-5), None, hydra.compute.Trace((), (), FrozenDict({}))))

def test_pure__pure_string():

    assert (hydra.monads.pure("hello").value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just("hello"), None, hydra.compute.Trace((), (), FrozenDict({}))))

# sequence

def test_sequence__sequence_pure_list():

    assert (hydra.monads.bind(hydra.monads.pure(1), (lambda x1: hydra.monads.bind(hydra.monads.pure(2), (lambda x2: hydra.monads.bind(hydra.monads.pure(3), (lambda x3: hydra.monads.pure((x1, x2, x3)))))))).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just((1, 2, 3)), None, hydra.compute.Trace((), (), FrozenDict({}))))

# withDefault

def test_withdefault__withdefault_on_success_returns_original():

    assert (hydra.lib.flows.with_default(0, hydra.monads.pure(42)).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(42), None, hydra.compute.Trace((), (), FrozenDict({}))))

def test_withdefault__withdefault_on_failure_returns_fallback():

    assert (hydra.lib.flows.with_default(99, hydra.monads.fail("error")).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(99), None, hydra.compute.Trace((), (), FrozenDict({}))))
