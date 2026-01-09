# Note: this is an automatically generated file. Do not edit.
# hydra.lib.sets primitives

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

# empty

def test_empty__empty_set():

    assert (hydra.lib.sets.empty()) == (frozenset({}))

# singleton

def test_singleton__single_element():

    assert (hydra.lib.sets.singleton(42)) == (frozenset({42}))

# fromList

def test_fromlist__create_from_list():

    assert (hydra.lib.sets.from_list((1, 2, 3))) == (frozenset({1, 2, 3}))

def test_fromlist__duplicates_removed():

    assert (hydra.lib.sets.from_list((1, 2, 1, 3))) == (frozenset({1, 2, 3}))

def test_fromlist__empty_list():

    assert (hydra.lib.sets.from_list(())) == (frozenset({}))

# toList

def test_tolist__convert_to_list():

    assert (hydra.lib.sets.to_list(frozenset({1, 2, 3}))) == ((1, 2, 3))

def test_tolist__empty_set():

    assert (hydra.lib.sets.to_list(frozenset({}))) == (())

# insert

def test_insert__insert_new_element():

    assert (hydra.lib.sets.insert(4, frozenset({1, 2, 3}))) == (frozenset({1, 2, 3, 4}))

def test_insert__insert_existing_element():

    assert (hydra.lib.sets.insert(2, frozenset({1, 2, 3}))) == (frozenset({1, 2, 3}))

def test_insert__insert_into_empty():

    assert (hydra.lib.sets.insert(1, frozenset({}))) == (frozenset({1}))

# delete

def test_delete__delete_existing():

    assert (hydra.lib.sets.delete(2, frozenset({1, 2, 3}))) == (frozenset({1, 3}))

def test_delete__delete_non_existing():

    assert (hydra.lib.sets.delete(4, frozenset({1, 2, 3}))) == (frozenset({1, 2, 3}))

def test_delete__delete_from_empty():

    assert (hydra.lib.sets.delete(1, frozenset({}))) == (frozenset({}))

# member

def test_member__element_exists():

    assert (hydra.lib.sets.member(2, frozenset({1, 2, 3}))) == (True)

def test_member__element_missing():

    assert (hydra.lib.sets.member(4, frozenset({1, 2, 3}))) == (False)

def test_member__empty_set():

    assert (hydra.lib.sets.member(1, frozenset({}))) == (False)

# size

def test_size__three_elements():

    assert (hydra.lib.sets.size(frozenset({1, 2, 3}))) == (3)

def test_size__single_element():

    assert (hydra.lib.sets.size(frozenset({42}))) == (1)

def test_size__empty_set():

    assert (hydra.lib.sets.size(frozenset({}))) == (0)

# null

def test_null__empty_set():

    assert (hydra.lib.sets.null(frozenset({}))) == (True)

def test_null__non_empty_set():

    assert (hydra.lib.sets.null(frozenset({1, 2}))) == (False)

# union

def test_union__union_two_sets():

    assert (hydra.lib.sets.union(frozenset({1, 2}), frozenset({2, 3}))) == (frozenset({1, 2, 3}))

def test_union__union_with_empty():

    assert (hydra.lib.sets.union(frozenset({1, 2}), frozenset({}))) == (frozenset({1, 2}))

def test_union__empty_with_non_empty():

    assert (hydra.lib.sets.union(frozenset({}), frozenset({1, 2}))) == (frozenset({1, 2}))

# unions

def test_unions__union_of_multiple_sets():

    assert (hydra.lib.sets.unions((frozenset({1, 2}), frozenset({2, 3}), frozenset({3, 4})))) == (frozenset({1, 2, 3, 4}))

def test_unions__union_with_empty_sets():

    assert (hydra.lib.sets.unions((frozenset({1, 2}), frozenset({}), frozenset({3})))) == (frozenset({1, 2, 3}))

def test_unions__empty_list_of_sets():

    assert (hydra.lib.sets.unions(())) == (frozenset({}))

def test_unions__single_set():

    assert (hydra.lib.sets.unions((frozenset({1, 2, 3}),))) == (frozenset({1, 2, 3}))

# intersection

def test_intersection__common_elements():

    assert (hydra.lib.sets.intersection(frozenset({1, 2, 3}), frozenset({2, 3, 4}))) == (frozenset({2, 3}))

def test_intersection__no_common_elements():

    assert (hydra.lib.sets.intersection(frozenset({1, 2}), frozenset({3, 4}))) == (frozenset({}))

def test_intersection__intersection_with_empty():

    assert (hydra.lib.sets.intersection(frozenset({1, 2}), frozenset({}))) == (frozenset({}))

# difference

def test_difference__remove_elements():

    assert (hydra.lib.sets.difference(frozenset({1, 2, 3}), frozenset({2, 4}))) == (frozenset({1, 3}))

def test_difference__no_overlap():

    assert (hydra.lib.sets.difference(frozenset({1, 2}), frozenset({3, 4}))) == (frozenset({1, 2}))

def test_difference__difference_with_empty():

    assert (hydra.lib.sets.difference(frozenset({1, 2}), frozenset({}))) == (frozenset({1, 2}))

# map

def test_map__map_function():

    assert (hydra.lib.sets.map((lambda x: hydra.lib.math.mul(x, 2)), frozenset({1, 2, 3}))) == (frozenset({2, 4, 6}))

def test_map__map_on_empty():

    assert (hydra.lib.sets.map((lambda x: hydra.lib.math.mul(x, 2)), frozenset({}))) == (frozenset({}))
