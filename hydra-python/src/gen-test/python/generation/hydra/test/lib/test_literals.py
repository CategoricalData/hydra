# Note: this is an automatically generated file. Do not edit.
# hydra.lib.literals primitives

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

# bigintToInt32

def test_biginttoint32__positive():

    assert (hydra.lib.literals.bigint_to_int32(42)) == (42)

def test_biginttoint32__negative():

    assert (hydra.lib.literals.bigint_to_int32(-42)) == (-42)

def test_biginttoint32__zero():

    assert (hydra.lib.literals.bigint_to_int32(0)) == (0)

# int32ToBigint

def test_int32tobigint__positive():

    assert (hydra.lib.literals.int32_to_bigint(42)) == (42)

def test_int32tobigint__negative():

    assert (hydra.lib.literals.int32_to_bigint(-42)) == (-42)

def test_int32tobigint__zero():

    assert (hydra.lib.literals.int32_to_bigint(0)) == (0)
