# Note: this is an automatically generated file. Do not edit.
# hydra.lib.flows primitives

from __future__ import annotations
from typing import cast
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
import hydra.formatting
import hydra.graph
import hydra.json
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

# bind

def test_bind__bind_add():

    assert (hydra.monads.bind(hydra.monads.pure(5), (lambda n: hydra.monads.pure(hydra.lib.math.add(n, 5)))).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(10), None, hydra.compute.Trace((), (), FrozenDict({}))))

def test_bind__bind_multiply():

    assert (hydra.monads.bind(hydra.monads.pure(3), (lambda n: hydra.monads.pure(hydra.lib.math.mul(n, 4)))).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(12), None, hydra.compute.Trace((), (), FrozenDict({}))))

# fail

def test_fail__fail_with_message():

    assert (hydra.monads.fail("test error message").value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Nothing(), None, hydra.compute.Trace((), ("Error: test error message ()",), FrozenDict({}))))

# map

def test_map__map_negate():

    assert (hydra.monads.map(hydra.lib.math.negate, hydra.monads.pure(5)).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(-5), None, hydra.compute.Trace((), (), FrozenDict({}))))

def test_map__map_abs():

    assert (hydra.monads.map(hydra.lib.math.abs, hydra.monads.pure(-3)).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(3), None, hydra.compute.Trace((), (), FrozenDict({}))))

# pure

def test_pure__pure_integer():

    assert (hydra.monads.pure(42).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(42), None, hydra.compute.Trace((), (), FrozenDict({}))))

def test_pure__pure_zero():

    assert (hydra.monads.pure(0).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(0), None, hydra.compute.Trace((), (), FrozenDict({}))))

def test_pure__pure_negative():

    assert (hydra.monads.pure(-5).value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just(-5), None, hydra.compute.Trace((), (), FrozenDict({}))))

def test_pure__pure_string():

    assert (hydra.monads.pure("hello").value(None, hydra.compute.Trace((), (), FrozenDict({})))) == (hydra.compute.FlowState(Just("hello"), None, hydra.compute.Trace((), (), FrozenDict({}))))
