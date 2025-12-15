# Note: this is an automatically generated file. Do not edit.
# hydra.lib.math primitives

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

# abs

def test_abs__positive():

    assert (hydra.lib.math.abs(5)) == (5)

def test_abs__negative():

    assert (hydra.lib.math.abs(-5)) == (5)

def test_abs__zero():

    assert (hydra.lib.math.abs(0)) == (0)

# add

def test_add__positive_numbers():

    assert (hydra.lib.math.add(3, 5)) == (8)

def test_add__negative_numbers():

    assert (hydra.lib.math.add(-3, -5)) == (-8)

def test_add__mixed_sign():

    assert (hydra.lib.math.add(10, -3)) == (7)

def test_add__with_zero():

    assert (hydra.lib.math.add(42, 0)) == (42)

# div

def test_div__exact_division():

    assert (hydra.lib.math.div(10, 2)) == (5)

def test_div__truncates_toward_negative_infinity():

    assert (hydra.lib.math.div(10, 3)) == (3)

def test_div__negative_dividend():

    assert (hydra.lib.math.div(-10, 3)) == (-4)

def test_div__negative_divisor():

    assert (hydra.lib.math.div(10, -3)) == (-4)

# even

def test_even__even_positive():

    assert (hydra.lib.math.even(4)) == (True)

def test_even__odd_positive():

    assert (hydra.lib.math.even(5)) == (False)

def test_even__even_negative():

    assert (hydra.lib.math.even(-4)) == (True)

def test_even__odd_negative():

    assert (hydra.lib.math.even(-5)) == (False)

def test_even__zero():

    assert (hydra.lib.math.even(0)) == (True)

# mod

def test_mod__basic_modulo():

    assert (hydra.lib.math.mod(10, 3)) == (1)

def test_mod__exact_division():

    assert (hydra.lib.math.mod(10, 2)) == (0)

def test_mod__negative_dividend():

    assert (hydra.lib.math.mod(-10, 3)) == (2)

def test_mod__negative_divisor():

    assert (hydra.lib.math.mod(10, -3)) == (-2)

# mul

def test_mul__positive_numbers():

    assert (hydra.lib.math.mul(3, 5)) == (15)

def test_mul__negative_numbers():

    assert (hydra.lib.math.mul(-3, -5)) == (15)

def test_mul__mixed_sign():

    assert (hydra.lib.math.mul(3, -5)) == (-15)

def test_mul__with_zero():

    assert (hydra.lib.math.mul(42, 0)) == (0)

def test_mul__with_one():

    assert (hydra.lib.math.mul(42, 1)) == (42)

# negate

def test_negate__positive():

    assert (hydra.lib.math.negate(5)) == (-5)

def test_negate__negative():

    assert (hydra.lib.math.negate(-5)) == (5)

def test_negate__zero():

    assert (hydra.lib.math.negate(0)) == (0)

# odd

def test_odd__odd_positive():

    assert (hydra.lib.math.odd(5)) == (True)

def test_odd__even_positive():

    assert (hydra.lib.math.odd(4)) == (False)

def test_odd__odd_negative():

    assert (hydra.lib.math.odd(-5)) == (True)

def test_odd__even_negative():

    assert (hydra.lib.math.odd(-4)) == (False)

def test_odd__zero():

    assert (hydra.lib.math.odd(0)) == (False)

# pred

def test_pred__positive():

    assert (hydra.lib.math.pred(5)) == (4)

def test_pred__zero():

    assert (hydra.lib.math.pred(0)) == (-1)

def test_pred__negative():

    assert (hydra.lib.math.pred(-5)) == (-6)

# range

def test_range__ascending_range():

    assert (hydra.lib.math.range_(1, 5)) == ((1, 2, 3, 4, 5))

def test_range__single_element():

    assert (hydra.lib.math.range_(5, 5)) == ((5,))

def test_range__two_elements():

    assert (hydra.lib.math.range_(3, 4)) == ((3, 4))

def test_range__negative_start():

    assert (hydra.lib.math.range_(-2, 2)) == ((-2, -1, 0, 1, 2))

# rem

def test_rem__basic_remainder():

    assert (hydra.lib.math.rem(10, 3)) == (1)

def test_rem__exact_division():

    assert (hydra.lib.math.rem(10, 2)) == (0)

def test_rem__negative_dividend():

    assert (hydra.lib.math.rem(-10, 3)) == (-1)

def test_rem__negative_divisor():

    assert (hydra.lib.math.rem(10, -3)) == (1)

# signum

def test_signum__positive():

    assert (hydra.lib.math.signum(5)) == (1)

def test_signum__negative():

    assert (hydra.lib.math.signum(-5)) == (-1)

def test_signum__zero():

    assert (hydra.lib.math.signum(0)) == (0)

# sub

def test_sub__positive_numbers():

    assert (hydra.lib.math.sub(10, 3)) == (7)

def test_sub__negative_numbers():

    assert (hydra.lib.math.sub(-10, -3)) == (-7)

def test_sub__mixed_sign():

    assert (hydra.lib.math.sub(10, -3)) == (13)

def test_sub__with_zero():

    assert (hydra.lib.math.sub(42, 0)) == (42)

# succ

def test_succ__positive():

    assert (hydra.lib.math.succ(5)) == (6)

def test_succ__zero():

    assert (hydra.lib.math.succ(0)) == (1)

def test_succ__negative():

    assert (hydra.lib.math.succ(-5)) == (-4)
