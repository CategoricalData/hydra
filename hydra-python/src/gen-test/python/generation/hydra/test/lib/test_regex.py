# Note: this is an automatically generated file. Do not edit.
# hydra.lib.regex primitives

from __future__ import annotations
from typing import cast
from decimal import Decimal
from hydra.dsl.python import FrozenDict, frozenlist, Either, Left, Right, Maybe, Just, Nothing
import hydra.annotations
import hydra.ast
import hydra.classes
import hydra.coders
import hydra.constants
import hydra.context
import hydra.core
import hydra.decode.core
import hydra.encode.core
import hydra.error.checking
import hydra.error.core
import hydra.errors
import hydra.extract.core
import hydra.extract.core
import hydra.formatting
import hydra.graph
import hydra.json.model
import hydra.lexical
import hydra.lib.chars
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.parsing
import hydra.paths
import hydra.rewriting
import hydra.serialization
import hydra.show.core
import hydra.show.error.core
import hydra.show.errors
import hydra.show.variants
import hydra.show.typing
import hydra.sorting
import hydra.testing
import hydra.topology
import hydra.typing
import hydra.util
import hydra.variants

# matches

def test_matches__exact_match():

    assert (hydra.lib.regex.matches("hello", "hello")) == (True)

def test_matches__pattern_match():

    assert (hydra.lib.regex.matches("[a-z]+", "hello")) == (True)

def test_matches__no_match():

    assert (hydra.lib.regex.matches("[0-9]+", "hello")) == (False)

def test_matches__partial_content_does_not_match():

    assert (hydra.lib.regex.matches("[a-z]+", "hello123")) == (False)

def test_matches__digit_pattern():

    assert (hydra.lib.regex.matches("[0-9]+", "12345")) == (True)

def test_matches__mixed_pattern():

    assert (hydra.lib.regex.matches("[a-z]+[0-9]+", "hello123")) == (True)

def test_matches__empty_pattern_matches_empty():

    assert (hydra.lib.regex.matches("", "")) == (True)

def test_matches__empty_pattern_does_not_match_non_empty():

    assert (hydra.lib.regex.matches("", "hello")) == (False)

def test_matches__star_matches_empty():

    assert (hydra.lib.regex.matches("a*", "")) == (True)

def test_matches__alternation():

    assert (hydra.lib.regex.matches("cat|dog", "cat")) == (True)

def test_matches__alternation_second():

    assert (hydra.lib.regex.matches("cat|dog", "dog")) == (True)

def test_matches__alternation_no_match():

    assert (hydra.lib.regex.matches("cat|dog", "bird")) == (False)

def test_matches__quantifier():

    assert (hydra.lib.regex.matches("ab?c", "ac")) == (True)

def test_matches__quantifier_with_optional():

    assert (hydra.lib.regex.matches("ab?c", "abc")) == (True)

# find

def test_find__simple_find():

    assert (hydra.lib.regex.find("[0-9]+", "abc123def")) == (Just("123"))

def test_find__no_match():

    assert (hydra.lib.regex.find("[0-9]+", "abcdef")) == (Nothing())

def test_find__find_first():

    assert (hydra.lib.regex.find("[a-z]+", "123abc456def")) == (Just("abc"))

def test_find__empty_input():

    assert (hydra.lib.regex.find("[0-9]+", "")) == (Nothing())

def test_find__full_match():

    assert (hydra.lib.regex.find(".*", "hello")) == (Just("hello"))

# findAll

def test_findall__multiple_matches():

    assert (hydra.lib.regex.find_all("[0-9]+", "a1b2c3")) == (("1", "2", "3"))

def test_findall__no_matches():

    assert (hydra.lib.regex.find_all("[0-9]+", "abc")) == (())

def test_findall__overlapping_words():

    assert (hydra.lib.regex.find_all("[a-z]+", "abc def ghi")) == (("abc", "def", "ghi"))

def test_findall__single_match():

    assert (hydra.lib.regex.find_all("hello", "say hello world")) == (("hello",))

# replace

def test_replace__basic_replace():

    assert (hydra.lib.regex.replace("[0-9]+", "X", "abc123def456")) == ("abcXdef456")

def test_replace__no_match():

    assert (hydra.lib.regex.replace("[0-9]+", "X", "abcdef")) == ("abcdef")

def test_replace__replace_at_start():

    assert (hydra.lib.regex.replace("^[a-z]+", "X", "abc123")) == ("X123")

def test_replace__empty_replacement():

    assert (hydra.lib.regex.replace("[0-9]+", "", "abc123def")) == ("abcdef")

# replaceAll

def test_replaceall__replace_all_digits():

    assert (hydra.lib.regex.replace_all("[0-9]+", "X", "a1b2c3")) == ("aXbXcX")

def test_replaceall__no_match():

    assert (hydra.lib.regex.replace_all("[0-9]+", "X", "abc")) == ("abc")

def test_replaceall__replace_all_words():

    assert (hydra.lib.regex.replace_all("[a-z]+", "X", "abc 123 def")) == ("X 123 X")

def test_replaceall__empty_replacement():

    assert (hydra.lib.regex.replace_all("[0-9]+", "", "a1b2c3")) == ("abc")

# split

def test_split__split_on_comma():

    assert (hydra.lib.regex.split(",", "a,b,c")) == (("a", "b", "c"))

def test_split__split_on_spaces():

    assert (hydra.lib.regex.split(" +", "a b  c")) == (("a", "b", "c"))

def test_split__no_match():

    assert (hydra.lib.regex.split(",", "abc")) == (("abc",))

def test_split__split_on_digits():

    assert (hydra.lib.regex.split("[0-9]+", "a1b2c")) == (("a", "b", "c"))

def test_split__trailing_delimiter():

    assert (hydra.lib.regex.split(",", "a,b,")) == (("a", "b", ""))
