# Note: this is an automatically generated file. Do not edit.
# hydra.lib.maybes primitives

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

# isJust

def test_isjust__just_value():

    assert (hydra.lib.maybes.is_just(Just(42))) == (True)

def test_isjust__nothing():

    assert (hydra.lib.maybes.is_just(Nothing())) == (False)

# isNothing

def test_isnothing__just_value():

    assert (hydra.lib.maybes.is_nothing(Just(42))) == (False)

def test_isnothing__nothing():

    assert (hydra.lib.maybes.is_nothing(Nothing())) == (True)

# fromMaybe

def test_frommaybe__just_value():

    assert (hydra.lib.maybes.from_maybe(0, Just(42))) == (42)

def test_frommaybe__nothing_with_default():

    assert (hydra.lib.maybes.from_maybe(99, Nothing())) == (99)

# maybe

def test_maybe__just_value_applies_function():

    assert (hydra.lib.maybes.maybe(0, (lambda x: hydra.lib.math.mul(x, 2)), Just(5))) == (10)

def test_maybe__nothing_returns_default():

    assert (hydra.lib.maybes.maybe(99, (lambda x: hydra.lib.math.mul(x, 2)), Nothing())) == (99)

# pure

def test_pure__wraps_integer():

    assert (hydra.lib.maybes.pure(42)) == (Just(42))

def test_pure__wraps_string():

    assert (hydra.lib.maybes.pure("hello")) == (Just("hello"))

# cat

def test_cat__filters_nothings():

    assert (hydra.lib.maybes.cat((Just(1), Nothing(), Just(2)))) == ((1, 2))

def test_cat__all_justs():

    assert (hydra.lib.maybes.cat((Just(1), Just(2)))) == ((1, 2))

def test_cat__all_nothings():

    assert (hydra.lib.maybes.cat((Nothing(), Nothing()))) == (())

def test_cat__empty_list():

    assert (hydra.lib.maybes.cat(())) == (())

# map

def test_map__maps_just_value():

    assert (hydra.lib.maybes.map((lambda x: hydra.lib.math.mul(x, 2)), Just(5))) == (Just(10))

def test_map__nothing_unchanged():

    assert (hydra.lib.maybes.map((lambda x: hydra.lib.math.mul(x, 2)), Nothing())) == (Nothing())
