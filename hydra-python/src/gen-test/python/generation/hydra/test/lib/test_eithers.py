# Note: this is an automatically generated file. Do not edit.
# hydra.lib.eithers primitives

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

# bimap

def test_bimap__map_left_value():

    assert (hydra.lib.eithers.bimap((lambda x: hydra.lib.math.mul(x, 2)), (lambda s: hydra.lib.strings.length(s)), Left(5))) == (Left(10))

def test_bimap__map_right_value():

    assert (hydra.lib.eithers.bimap((lambda x: hydra.lib.math.mul(x, 2)), (lambda s: hydra.lib.strings.length(s)), Right("ab"))) == (Right(2))

# isLeft

def test_isleft__left_value():

    assert (hydra.lib.eithers.is_left(Left(42))) == (True)

def test_isleft__right_value():

    assert (hydra.lib.eithers.is_left(Right("test"))) == (False)

# isRight

def test_isright__right_value():

    assert (hydra.lib.eithers.is_right(Right("test"))) == (True)

def test_isright__left_value():

    assert (hydra.lib.eithers.is_right(Left(42))) == (False)

# fromLeft

def test_fromleft__extract_left():

    assert (hydra.lib.eithers.from_left(99, Left(42))) == (42)

def test_fromleft__use_default_for_right():

    assert (hydra.lib.eithers.from_left(99, Right("test"))) == (99)

# fromRight

def test_fromright__extract_right():

    assert (hydra.lib.eithers.from_right("default", Right("test"))) == ("test")

def test_fromright__use_default_for_left():

    assert (hydra.lib.eithers.from_right("default", Left(42))) == ("default")

# either

def test_either__apply_left_function():

    assert (hydra.lib.eithers.either((lambda x: hydra.lib.math.mul(x, 2)), (lambda s: hydra.lib.strings.length(s)), Left(5))) == (10)

def test_either__apply_right_function():

    assert (hydra.lib.eithers.either((lambda x: hydra.lib.math.mul(x, 2)), (lambda s: hydra.lib.strings.length(s)), Right("ab"))) == (2)

# lefts

def test_lefts__filter_left_values():

    assert (hydra.lib.eithers.lefts((Left(1), Right("a"), Left(2), Right("b")))) == ((1, 2))

def test_lefts__all_lefts():

    assert (hydra.lib.eithers.lefts((Left(1), Left(2)))) == ((1, 2))

def test_lefts__all_rights():

    assert (hydra.lib.eithers.lefts((Right("a"), Right("b")))) == (())

def test_lefts__empty_list():

    assert (hydra.lib.eithers.lefts(())) == (())

# rights

def test_rights__filter_right_values():

    assert (hydra.lib.eithers.rights((Left(1), Right("a"), Left(2), Right("b")))) == (("a", "b"))

def test_rights__all_rights():

    assert (hydra.lib.eithers.rights((Right("a"), Right("b")))) == (("a", "b"))

def test_rights__all_lefts():

    assert (hydra.lib.eithers.rights((Left(1), Left(2)))) == (())

def test_rights__empty_list():

    assert (hydra.lib.eithers.rights(())) == (())

# partitionEithers

def test_partitioneithers__partition_mixed():

    assert (hydra.lib.eithers.partition_eithers((Left(1), Right("a"), Left(2), Right("b")))) == (((1, 2), ("a", "b")))

def test_partitioneithers__all_lefts():

    assert (hydra.lib.eithers.partition_eithers((Left(1), Left(2)))) == (((1, 2), ()))

def test_partitioneithers__all_rights():

    assert (hydra.lib.eithers.partition_eithers((Right("a"), Right("b")))) == (((), ("a", "b")))

def test_partitioneithers__empty_list():

    assert (hydra.lib.eithers.partition_eithers(())) == (((), ()))
