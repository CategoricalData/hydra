# Note: this is an automatically generated file. Do not edit.
# hydra.lib.logic primitives

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

# and

def test_and__true_and_true():

    assert (hydra.lib.logic.and_(True, True)) == (True)

def test_and__true_and_false():

    assert (hydra.lib.logic.and_(True, False)) == (False)

def test_and__false_and_true():

    assert (hydra.lib.logic.and_(False, True)) == (False)

def test_and__false_and_false():

    assert (hydra.lib.logic.and_(False, False)) == (False)

# ifElse

# boolean values

def test_ifelse__boolean_values__true_condition_returns_then():

    assert (hydra.lib.logic.if_else(True, (lambda : True), (lambda : False))) == (True)

def test_ifelse__boolean_values__false_condition_returns_else():

    assert (hydra.lib.logic.if_else(False, (lambda : True), (lambda : False))) == (False)

# integer values

def test_ifelse__integer_values__true_selects_first_int():

    assert (hydra.lib.logic.if_else(True, (lambda : 42), (lambda : 0))) == (42)

def test_ifelse__integer_values__false_selects_second_int():

    assert (hydra.lib.logic.if_else(False, (lambda : 42), (lambda : 0))) == (0)

# string values

def test_ifelse__string_values__true_selects_first_string():

    assert (hydra.lib.logic.if_else(True, (lambda : "yes"), (lambda : "no"))) == ("yes")

def test_ifelse__string_values__false_selects_second_string():

    assert (hydra.lib.logic.if_else(False, (lambda : "yes"), (lambda : "no"))) == ("no")

# not

def test_not__not_true():

    assert (hydra.lib.logic.not_(True)) == (False)

def test_not__not_false():

    assert (hydra.lib.logic.not_(False)) == (True)

# or

def test_or__true_or_true():

    assert (hydra.lib.logic.or_(True, True)) == (True)

def test_or__true_or_false():

    assert (hydra.lib.logic.or_(True, False)) == (True)

def test_or__false_or_true():

    assert (hydra.lib.logic.or_(False, True)) == (True)

def test_or__false_or_false():

    assert (hydra.lib.logic.or_(False, False)) == (False)
