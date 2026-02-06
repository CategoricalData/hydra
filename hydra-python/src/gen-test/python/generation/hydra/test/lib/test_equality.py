# Note: this is an automatically generated file. Do not edit.
# hydra.lib.equality primitives

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

# compare

def test_compare__less_than():

    assert (hydra.lib.equality.compare(3, 5)) == (hydra.util.Comparison.LESS_THAN)

def test_compare__equal():

    assert (hydra.lib.equality.compare(5, 5)) == (hydra.util.Comparison.EQUAL_TO)

def test_compare__greater_than():

    assert (hydra.lib.equality.compare(5, 3)) == (hydra.util.Comparison.GREATER_THAN)

# equal

def test_equal__equal_integers():

    assert (hydra.lib.equality.equal(5, 5)) == (True)

def test_equal__unequal_integers():

    assert (hydra.lib.equality.equal(5, 3)) == (False)

# gt

def test_gt__greater():

    assert (hydra.lib.equality.gt(5, 3)) == (True)

def test_gt__equal():

    assert (hydra.lib.equality.gt(5, 5)) == (False)

def test_gt__less():

    assert (hydra.lib.equality.gt(3, 5)) == (False)

# gte

def test_gte__greater():

    assert (hydra.lib.equality.gte(5, 3)) == (True)

def test_gte__equal():

    assert (hydra.lib.equality.gte(5, 5)) == (True)

def test_gte__less():

    assert (hydra.lib.equality.gte(3, 5)) == (False)

# identity

def test_identity__integer():

    assert (hydra.lib.equality.identity(42)) == (42)

# lt

def test_lt__less():

    assert (hydra.lib.equality.lt(3, 5)) == (True)

def test_lt__equal():

    assert (hydra.lib.equality.lt(5, 5)) == (False)

def test_lt__greater():

    assert (hydra.lib.equality.lt(5, 3)) == (False)

# lte

def test_lte__less():

    assert (hydra.lib.equality.lte(3, 5)) == (True)

def test_lte__equal():

    assert (hydra.lib.equality.lte(5, 5)) == (True)

def test_lte__greater():

    assert (hydra.lib.equality.lte(5, 3)) == (False)

# max

def test_max__first_greater():

    assert (hydra.lib.equality.max(5, 3)) == (5)

def test_max__second_greater():

    assert (hydra.lib.equality.max(3, 5)) == (5)

def test_max__equal():

    assert (hydra.lib.equality.max(5, 5)) == (5)

# min

def test_min__first_less():

    assert (hydra.lib.equality.min(3, 5)) == (3)

def test_min__second_less():

    assert (hydra.lib.equality.min(5, 3)) == (3)

def test_min__equal():

    assert (hydra.lib.equality.min(5, 5)) == (5)

# compare strings

def test_compare_strings__less_than__lexicographic_():

    assert (hydra.lib.equality.compare("apple", "banana")) == (hydra.util.Comparison.LESS_THAN)

def test_compare_strings__equal():

    assert (hydra.lib.equality.compare("hello", "hello")) == (hydra.util.Comparison.EQUAL_TO)

def test_compare_strings__greater_than__lexicographic_():

    assert (hydra.lib.equality.compare("zebra", "apple")) == (hydra.util.Comparison.GREATER_THAN)

def test_compare_strings__empty_vs_non_empty():

    assert (hydra.lib.equality.compare("", "a")) == (hydra.util.Comparison.LESS_THAN)

def test_compare_strings__prefix_vs_longer():

    assert (hydra.lib.equality.compare("ab", "abc")) == (hydra.util.Comparison.LESS_THAN)

# lt strings

def test_lt_strings__less__lexicographic_():

    assert (hydra.lib.equality.lt("apple", "banana")) == (True)

def test_lt_strings__equal():

    assert (hydra.lib.equality.lt("hello", "hello")) == (False)

def test_lt_strings__greater():

    assert (hydra.lib.equality.lt("zebra", "apple")) == (False)

# gt strings

def test_gt_strings__greater__lexicographic_():

    assert (hydra.lib.equality.gt("zebra", "apple")) == (True)

def test_gt_strings__equal():

    assert (hydra.lib.equality.gt("hello", "hello")) == (False)

def test_gt_strings__less():

    assert (hydra.lib.equality.gt("apple", "banana")) == (False)

# max strings

def test_max_strings__first_greater():

    assert (hydra.lib.equality.max("zebra", "apple")) == ("zebra")

def test_max_strings__second_greater():

    assert (hydra.lib.equality.max("apple", "zebra")) == ("zebra")

def test_max_strings__equal():

    assert (hydra.lib.equality.max("hello", "hello")) == ("hello")

# min strings

def test_min_strings__first_less():

    assert (hydra.lib.equality.min("apple", "zebra")) == ("apple")

def test_min_strings__second_less():

    assert (hydra.lib.equality.min("zebra", "apple")) == ("apple")

def test_min_strings__equal():

    assert (hydra.lib.equality.min("hello", "hello")) == ("hello")

# compare floats

def test_compare_floats__less_than():

    assert (hydra.lib.equality.compare(1.5, 2.5)) == (hydra.util.Comparison.LESS_THAN)

def test_compare_floats__equal():

    assert (hydra.lib.equality.compare(3.14, 3.14)) == (hydra.util.Comparison.EQUAL_TO)

def test_compare_floats__greater_than():

    assert (hydra.lib.equality.compare(5.0, 3.0)) == (hydra.util.Comparison.GREATER_THAN)

def test_compare_floats__negative_vs_positive():

    assert (hydra.lib.equality.compare(-1.0, 1.0)) == (hydra.util.Comparison.LESS_THAN)

# lt floats

def test_lt_floats__less():

    assert (hydra.lib.equality.lt(1.5, 2.5)) == (True)

def test_lt_floats__equal():

    assert (hydra.lib.equality.lt(3.14, 3.14)) == (False)

def test_lt_floats__greater():

    assert (hydra.lib.equality.lt(5.0, 3.0)) == (False)

# gt floats

def test_gt_floats__greater():

    assert (hydra.lib.equality.gt(5.0, 3.0)) == (True)

def test_gt_floats__equal():

    assert (hydra.lib.equality.gt(3.14, 3.14)) == (False)

def test_gt_floats__less():

    assert (hydra.lib.equality.gt(1.5, 2.5)) == (False)
