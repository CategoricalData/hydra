# Note: this is an automatically generated file. Do not edit.
# hydra.lib.pairs primitives

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

# bimap

def test_bimap__transform_both_elements():

    assert (hydra.lib.pairs.bimap((lambda x: hydra.lib.math.mul(x, 2)), (lambda s: hydra.lib.strings.length(s)), (5, "ab"))) == ((10, 2))

def test_bimap__with_zero():

    assert (hydra.lib.pairs.bimap((lambda x: hydra.lib.math.mul(x, 2)), (lambda s: hydra.lib.strings.length(s)), (0, "hello"))) == ((0, 5))

# first

def test_first__extract_first_element():

    assert (hydra.lib.pairs.first((42, "hello"))) == (42)

def test_first__with_zero():

    assert (hydra.lib.pairs.first((0, "world"))) == (0)

def test_first__negative_number():

    assert (hydra.lib.pairs.first((-5, "test"))) == (-5)

# second

def test_second__extract_second_element():

    assert (hydra.lib.pairs.second((42, "hello"))) == ("hello")

def test_second__empty_string():

    assert (hydra.lib.pairs.second((0, ""))) == ("")

def test_second__long_string():

    assert (hydra.lib.pairs.second((123, "testing"))) == ("testing")
