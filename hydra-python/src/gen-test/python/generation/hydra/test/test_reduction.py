# Note: this is an automatically generated file. Do not edit.
# reduction

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

# beta reduction

def test_beta_reduction__identity_function_applied_to_literal():

    assert ((lambda x: x)(42)) == (42)

def test_beta_reduction__constant_function():

    assert ((lambda x: 1)(42)) == (1)

def test_beta_reduction__nested_application():

    assert ((lambda x, y: x)(1, 2)) == (1)

# monomorphic primitives

def test_monomorphic_primitives__toupper_on_lowercase():

    assert (hydra.lib.strings.to_upper("hello")) == ("HELLO")

def test_monomorphic_primitives__toupper_on_mixed_case():

    assert (hydra.lib.strings.to_upper("Hello World")) == ("HELLO WORLD")

def test_monomorphic_primitives__toupper_on_empty_string():

    assert (hydra.lib.strings.to_upper("")) == ("")

def test_monomorphic_primitives__tolower_on_uppercase():

    assert (hydra.lib.strings.to_lower("HELLO")) == ("hello")

def test_monomorphic_primitives__string_length():

    assert (hydra.lib.strings.length("hello")) == (5)

def test_monomorphic_primitives__string_length_of_empty():

    assert (hydra.lib.strings.length("")) == (0)

def test_monomorphic_primitives__add_two_positive_integers():

    assert (hydra.lib.math.add(3, 5)) == (8)

def test_monomorphic_primitives__add_negative_and_positive():

    assert (hydra.lib.math.add(-10, 3)) == (-7)

def test_monomorphic_primitives__add_with_zero():

    assert (hydra.lib.math.add(0, 42)) == (42)

def test_monomorphic_primitives__subtract_integers():

    assert (hydra.lib.math.sub(10, 3)) == (7)

def test_monomorphic_primitives__multiply_integers():

    assert (hydra.lib.math.mul(6, 7)) == (42)

def test_monomorphic_primitives__multiply_by_zero():

    assert (hydra.lib.math.mul(100, 0)) == (0)

def test_monomorphic_primitives__divide_integers():

    assert (hydra.lib.math.div(20, 4)) == (5)

def test_monomorphic_primitives__modulo():

    assert (hydra.lib.math.mod(17, 5)) == (2)

def test_monomorphic_primitives__spliton_basic():

    assert (hydra.lib.strings.split_on(",", "a,b,c")) == (("a", "b", "c"))

def test_monomorphic_primitives__cat2_strings():

    assert (hydra.lib.strings.cat2("hello", "world")) == ("helloworld")

# polymorphic primitives

def test_polymorphic_primitives__length_of_integer_list():

    assert (hydra.lib.lists.length((1, 2, 3))) == (3)

def test_polymorphic_primitives__length_of_string_list():

    assert (hydra.lib.lists.length(("a", "b"))) == (2)

def test_polymorphic_primitives__length_of_empty_list():

    assert (hydra.lib.lists.length(())) == (0)

def test_polymorphic_primitives__length_of_single_element_list():

    assert (hydra.lib.lists.length((True,))) == (1)

def test_polymorphic_primitives__head_of_integer_list():

    assert (hydra.lib.lists.head((10, 20, 30))) == (10)

def test_polymorphic_primitives__head_of_string_list():

    assert (hydra.lib.lists.head(("first", "second"))) == ("first")

def test_polymorphic_primitives__last_of_integer_list():

    assert (hydra.lib.lists.last((10, 20, 30))) == (30)

def test_polymorphic_primitives__concat_two_integer_lists():

    assert (hydra.lib.lists.concat2((1, 2), (3, 4))) == ((1, 2, 3, 4))

def test_polymorphic_primitives__concat_with_empty_list():

    assert (hydra.lib.lists.concat2((), (1, 2))) == ((1, 2))

def test_polymorphic_primitives__reverse_integer_list():

    assert (hydra.lib.lists.reverse((1, 2, 3))) == ((3, 2, 1))

def test_polymorphic_primitives__reverse_empty_list():

    assert (hydra.lib.lists.reverse(())) == (())

# nullary primitives

def test_nullary_primitives__empty_set_has_size_zero():

    assert (hydra.lib.sets.size(hydra.lib.sets.empty())) == (0)

# literals as values

def test_literals_as_values__integer_literal_is_a_value():

    assert (42) == (42)

def test_literals_as_values__negative_integer_literal():

    assert (-17) == (-17)

def test_literals_as_values__zero_integer_literal():

    assert (0) == (0)

def test_literals_as_values__string_literal_is_a_value():

    assert ("hello") == ("hello")

def test_literals_as_values__empty_string_literal():

    assert ("") == ("")

def test_literals_as_values__string_with_special_characters():

    assert ("hello\nworld\ttab") == ("hello\nworld\ttab")

def test_literals_as_values__boolean_true_is_a_value():

    assert (True) == (True)

def test_literals_as_values__boolean_false_is_a_value():

    assert (False) == (False)

def test_literals_as_values__float_literal_is_a_value():

    assert (3.14) == (3.14)

def test_literals_as_values__negative_float_literal():

    assert (-2.718) == (-2.718)

def test_literals_as_values__zero_float_literal():

    assert (0.0) == (0.0)

# list reduction

def test_list_reduction__empty_list_is_a_value():

    assert (()) == (())

def test_list_reduction__list_of_literals_is_a_value():

    assert ((1, 2, 3)) == ((1, 2, 3))

def test_list_reduction__list_with_reducible_element():

    assert (((lambda x: x)(42),)) == ((42,))

# optional reduction

def test_optional_reduction__nothing_is_a_value():

    assert (Nothing()) == (Nothing())

def test_optional_reduction__just_literal_is_a_value():

    assert (Just(42)) == (Just(42))

def test_optional_reduction__just_with_reducible_content():

    assert (Just((lambda x: x)(42))) == (Just(42))
