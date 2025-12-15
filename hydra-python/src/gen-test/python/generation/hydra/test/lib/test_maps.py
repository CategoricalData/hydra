# Note: this is an automatically generated file. Do not edit.
# hydra.lib.maps primitives

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

# empty

def test_empty__empty_map():

    assert (hydra.lib.maps.empty()) == (FrozenDict({}))

# singleton

def test_singleton__single_entry():

    assert (hydra.lib.maps.singleton(42, "hello")) == (FrozenDict({
  42: "hello"}))

# fromList

def test_fromlist__create_from_pairs():

    assert (hydra.lib.maps.from_list(((1, "a"), (2, "b")))) == (FrozenDict({
  1: "a",
  2: "b"}))

def test_fromlist__duplicate_keys():

    assert (hydra.lib.maps.from_list(((1, "a"), (1, "b")))) == (FrozenDict({
  1: "b"}))

def test_fromlist__empty_list():

    assert (hydra.lib.maps.from_list(())) == (FrozenDict({}))

# toList

def test_tolist__convert_to_pairs():

    assert (hydra.lib.maps.to_list(FrozenDict({
  1: "a",
  2: "b"}))) == (((1, "a"), (2, "b")))

def test_tolist__empty_map():

    assert (hydra.lib.maps.to_list(FrozenDict({}))) == (())

# insert

def test_insert__insert_new_key():

    assert (hydra.lib.maps.insert(3, "c", FrozenDict({
  1: "a",
  2: "b"}))) == (FrozenDict({
  1: "a",
  2: "b",
  3: "c"}))

def test_insert__update_existing():

    assert (hydra.lib.maps.insert(2, "updated", FrozenDict({
  1: "a",
  2: "b"}))) == (FrozenDict({
  1: "a",
  2: "updated"}))

def test_insert__insert_into_empty():

    assert (hydra.lib.maps.insert(1, "x", FrozenDict({}))) == (FrozenDict({
  1: "x"}))

# remove

def test_remove__remove_existing():

    assert (hydra.lib.maps.delete(2, FrozenDict({
  1: "a",
  2: "b",
  3: "c"}))) == (FrozenDict({
  1: "a",
  3: "c"}))

def test_remove__remove_non_existing():

    assert (hydra.lib.maps.delete(4, FrozenDict({
  1: "a",
  2: "b"}))) == (FrozenDict({
  1: "a",
  2: "b"}))

def test_remove__remove_from_empty():

    assert (hydra.lib.maps.delete(1, FrozenDict({}))) == (FrozenDict({}))

# lookup

def test_lookup__find_existing_key():

    assert (hydra.lib.maps.lookup(2, FrozenDict({
  1: "a",
  2: "b"}))) == (Just("b"))

def test_lookup__key_not_found():

    assert (hydra.lib.maps.lookup(3, FrozenDict({
  1: "a",
  2: "b"}))) == (Nothing())

def test_lookup__lookup_in_empty():

    assert (hydra.lib.maps.lookup(1, FrozenDict({}))) == (Nothing())

# member

def test_member__key_exists():

    assert (hydra.lib.maps.member(2, FrozenDict({
  1: "a",
  2: "b"}))) == (True)

def test_member__key_missing():

    assert (hydra.lib.maps.member(3, FrozenDict({
  1: "a",
  2: "b"}))) == (False)

def test_member__empty_map():

    assert (hydra.lib.maps.member(1, FrozenDict({}))) == (False)

# size

def test_size__three_entries():

    assert (hydra.lib.maps.size(FrozenDict({
  1: "a",
  2: "b",
  3: "c"}))) == (3)

def test_size__single_entry():

    assert (hydra.lib.maps.size(FrozenDict({
  42: "test"}))) == (1)

def test_size__empty_map():

    assert (hydra.lib.maps.size(FrozenDict({}))) == (0)

# null

def test_null__empty_map():

    assert (hydra.lib.maps.null(FrozenDict({}))) == (True)

def test_null__non_empty_map():

    assert (hydra.lib.maps.null(FrozenDict({
  1: "a"}))) == (False)

# keys

def test_keys__get_all_keys():

    assert (hydra.lib.maps.keys(FrozenDict({
  1: "a",
  2: "b",
  3: "c"}))) == ((1, 2, 3))

def test_keys__empty_map():

    assert (hydra.lib.maps.keys(FrozenDict({}))) == (())

# elems

def test_elems__get_all_elements():

    assert (hydra.lib.maps.elems(FrozenDict({
  1: "a",
  2: "b"}))) == (("a", "b"))

def test_elems__empty_map():

    assert (hydra.lib.maps.elems(FrozenDict({}))) == (())

# map

def test_map__map_over_values():

    assert (hydra.lib.maps.map((lambda s: hydra.lib.strings.to_upper(s)), FrozenDict({
  1: "a",
  2: "b"}))) == (FrozenDict({
  1: "A",
  2: "B"}))

def test_map__map_empty():

    assert (hydra.lib.maps.map((lambda s: hydra.lib.strings.to_upper(s)), FrozenDict({}))) == (FrozenDict({}))

# findWithDefault

def test_findwithdefault__find_existing():

    assert (hydra.lib.maps.find_with_default("default", 2, FrozenDict({
  1: "a",
  2: "b"}))) == ("b")

def test_findwithdefault__use_default():

    assert (hydra.lib.maps.find_with_default("default", 3, FrozenDict({
  1: "a",
  2: "b"}))) == ("default")
