# Note: this is an automatically generated file. Do not edit.
# hydra.lib.maybes primitives

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

# apply

def test_apply__both_just():

    assert (hydra.lib.maybes.apply(Just((lambda x1: hydra.lib.math.add(3, x1))), Just(5))) == (Just(8))

def test_apply__nothing_function():

    assert (hydra.lib.maybes.apply(Nothing(), Just(5))) == (Nothing())

def test_apply__nothing_value():

    assert (hydra.lib.maybes.apply(Just((lambda x1: hydra.lib.math.add(3, x1))), Nothing())) == (Nothing())

# bind

def test_bind__just_to_just():

    assert (hydra.lib.maybes.bind(Just(5), (lambda x: Just(hydra.lib.math.mul(x, 2))))) == (Just(10))

def test_bind__nothing_to_nothing():

    assert (hydra.lib.maybes.bind(Nothing(), (lambda x: Just(hydra.lib.math.mul(x, 2))))) == (Nothing())

# cases

def test_cases__just_applies_function():

    assert (hydra.lib.maybes.cases(Just(5), 0, (lambda x: hydra.lib.math.mul(x, 2)))) == (10)

def test_cases__nothing_returns_default():

    assert (hydra.lib.maybes.cases(Nothing(), 99, (lambda x: hydra.lib.math.mul(x, 2)))) == (99)

# cat

def test_cat__filters_nothings():

    assert (hydra.lib.maybes.cat((Just(1), Nothing(), Just(2)))) == ((1, 2))

def test_cat__all_justs():

    assert (hydra.lib.maybes.cat((Just(1), Just(2)))) == ((1, 2))

def test_cat__all_nothings():

    assert (hydra.lib.maybes.cat((Nothing(), Nothing()))) == (())

def test_cat__empty_list():

    assert (hydra.lib.maybes.cat(())) == (())

# compose

def test_compose__both_succeed():

    assert (hydra.lib.maybes.compose((lambda x: hydra.lib.logic.if_else(hydra.lib.equality.lte(x, 5), (lambda : Just(hydra.lib.math.add(x, 1))), (lambda : Nothing()))), (lambda y: hydra.lib.logic.if_else(hydra.lib.equality.gte(y, 5), (lambda : Just(hydra.lib.math.mul(y, 2))), (lambda : Nothing()))), 5)) == (Just(12))

def test_compose__first_fails():

    assert (hydra.lib.maybes.compose((lambda x: hydra.lib.logic.if_else(hydra.lib.equality.lte(x, 5), (lambda : Just(hydra.lib.math.add(x, 1))), (lambda : Nothing()))), (lambda y: hydra.lib.logic.if_else(hydra.lib.equality.gte(y, 5), (lambda : Just(hydra.lib.math.mul(y, 2))), (lambda : Nothing()))), 10)) == (Nothing())

def test_compose__second_fails():

    assert (hydra.lib.maybes.compose((lambda x: hydra.lib.logic.if_else(hydra.lib.equality.lte(x, 5), (lambda : Just(hydra.lib.math.add(x, 1))), (lambda : Nothing()))), (lambda y: hydra.lib.logic.if_else(hydra.lib.equality.gte(y, 5), (lambda : Just(hydra.lib.math.mul(y, 2))), (lambda : Nothing()))), 3)) == (Nothing())

# fromJust

def test_fromjust__extract_from_just():

    assert (hydra.lib.maybes.from_just(Just(42))) == (42)

# fromMaybe

def test_frommaybe__just_value():

    assert (hydra.lib.maybes.from_maybe(0, Just(42))) == (42)

def test_frommaybe__nothing_with_default():

    assert (hydra.lib.maybes.from_maybe(99, Nothing())) == (99)

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

# map

def test_map__maps_just_value():

    assert (hydra.lib.maybes.map((lambda x: hydra.lib.math.mul(x, 2)), Just(5))) == (Just(10))

def test_map__nothing_unchanged():

    assert (hydra.lib.maybes.map((lambda x: hydra.lib.math.mul(x, 2)), Nothing())) == (Nothing())

# mapMaybe

def test_mapmaybe__filter_and_transform():

    assert (hydra.lib.maybes.map_maybe((lambda x: hydra.lib.logic.if_else(hydra.lib.equality.gt(x, 2), (lambda : Just(hydra.lib.math.mul(x, 2))), (lambda : Nothing()))), (1, 2, 3, 4, 5))) == ((6, 8, 10))

def test_mapmaybe__empty_result():

    assert (hydra.lib.maybes.map_maybe((lambda x: hydra.lib.logic.if_else(hydra.lib.equality.gt(x, 2), (lambda : Just(hydra.lib.math.mul(x, 2))), (lambda : Nothing()))), (1, 2))) == (())

def test_mapmaybe__empty_input():

    assert (hydra.lib.maybes.map_maybe((lambda x: hydra.lib.logic.if_else(hydra.lib.equality.gt(x, 2), (lambda : Just(hydra.lib.math.mul(x, 2))), (lambda : Nothing()))), ())) == (())

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
