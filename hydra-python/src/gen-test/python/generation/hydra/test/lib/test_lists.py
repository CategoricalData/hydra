# Note: this is an automatically generated file. Do not edit.
# hydra.lib.lists primitives

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

# apply

# string transformations

def test_apply__string_transformations__string_transformations():

    assert (hydra.lib.lists.apply((hydra.lib.strings.to_upper, hydra.lib.strings.to_lower), ("One", "Two", "Three"))) == (("ONE", "TWO", "THREE", "one", "two", "three"))

# edge cases

def test_apply__edge_cases__empty_function_list():

    assert (hydra.lib.lists.apply((), ("a", "b"))) == (())

def test_apply__edge_cases__empty_input_list():

    assert (hydra.lib.lists.apply((hydra.lib.strings.to_upper,), ())) == (())

def test_apply__edge_cases__single_function():

    assert (hydra.lib.lists.apply((hydra.lib.strings.to_upper,), ("hello",))) == (("HELLO",))

def test_apply__edge_cases__single_input():

    assert (hydra.lib.lists.apply((hydra.lib.strings.to_upper, hydra.lib.strings.to_lower), ("Test",))) == (("TEST", "test"))

# at

def test_at__first_element():

    assert (hydra.lib.lists.at(0, (1, 2, 3))) == (1)

def test_at__middle_element():

    assert (hydra.lib.lists.at(1, (1, 2, 3))) == (2)

def test_at__last_element():

    assert (hydra.lib.lists.at(2, (1, 2, 3))) == (3)

def test_at__single_element_list():

    assert (hydra.lib.lists.at(0, (42,))) == (42)

def test_at__string_list_access():

    assert (hydra.lib.lists.at(1, ("hello", "world"))) == ("world")

# bind

def test_bind__negation_function():

    assert (hydra.lib.lists.bind((1, 2, 3, 4), (lambda x: hydra.lib.lists.pure(hydra.lib.math.negate(x))))) == ((-1, -2, -3, -4))

def test_bind__empty_list():

    assert (hydra.lib.lists.bind((), (lambda x: hydra.lib.lists.pure(hydra.lib.math.negate(x))))) == (())

def test_bind__single_element():

    assert (hydra.lib.lists.bind((5,), (lambda x: hydra.lib.lists.pure(hydra.lib.math.negate(x))))) == ((-5,))

def test_bind__duplicate_elements():

    assert (hydra.lib.lists.bind((1, 1, 2), (lambda x: hydra.lib.lists.pure(hydra.lib.math.negate(x))))) == ((-1, -1, -2))

# concat

def test_concat__multiple_non_empty_lists():

    assert (hydra.lib.lists.concat(((1, 2, 3), (4, 5), (6, 7, 8)))) == ((1, 2, 3, 4, 5, 6, 7, 8))

def test_concat__empty_lists_included():

    assert (hydra.lib.lists.concat(((), (1, 2), (), (3,)))) == ((1, 2, 3))

def test_concat__single_list():

    assert (hydra.lib.lists.concat(((1, 2, 3),))) == ((1, 2, 3))

def test_concat__all_empty_lists():

    assert (hydra.lib.lists.concat(((), (), ()))) == (())

def test_concat__empty_list_of_lists():

    assert (hydra.lib.lists.concat(())) == (())

# concat2

def test_concat2__two_non_empty_lists():

    assert (hydra.lib.lists.concat2((1, 2), (3, 4))) == ((1, 2, 3, 4))

def test_concat2__first_list_empty():

    assert (hydra.lib.lists.concat2((), (1, 2))) == ((1, 2))

def test_concat2__second_list_empty():

    assert (hydra.lib.lists.concat2((1, 2), ())) == ((1, 2))

def test_concat2__both_lists_empty():

    assert (hydra.lib.lists.concat2((), ())) == (())

def test_concat2__single_elements():

    assert (hydra.lib.lists.concat2((1,), (2,))) == ((1, 2))

def test_concat2__string_lists():

    assert (hydra.lib.lists.concat2(("a", "b"), ("c", "d"))) == (("a", "b", "c", "d"))

# cons

def test_cons__cons_to_non_empty_list():

    assert (hydra.lib.lists.cons(1, (2, 3))) == ((1, 2, 3))

def test_cons__cons_to_empty_list():

    assert (hydra.lib.lists.cons(1, ())) == ((1,))

def test_cons__cons_negative_number():

    assert (hydra.lib.lists.cons(-1, (2, 3))) == ((-1, 2, 3))

def test_cons__cons_string():

    assert (hydra.lib.lists.cons("hello", ("world",))) == (("hello", "world"))

# drop

def test_drop__drop_from_beginning():

    assert (hydra.lib.lists.drop(2, (1, 2, 3, 4, 5))) == ((3, 4, 5))

def test_drop__drop_zero_elements():

    assert (hydra.lib.lists.drop(0, (1, 2, 3))) == ((1, 2, 3))

def test_drop__drop_all_elements():

    assert (hydra.lib.lists.drop(3, (1, 2, 3))) == (())

def test_drop__drop_more_than_length():

    assert (hydra.lib.lists.drop(5, (1, 2))) == (())

def test_drop__drop_from_empty_list():

    assert (hydra.lib.lists.drop(3, ())) == (())

def test_drop__drop_negative_amount():

    assert (hydra.lib.lists.drop(-1, (1, 2, 3))) == ((1, 2, 3))

# dropWhile

def test_dropwhile__drop_while_less_than_3():

    assert (hydra.lib.lists.drop_while((lambda x: hydra.lib.equality.lt(x, 3)), (1, 2, 3, 2, 1))) == ((3, 2, 1))

def test_dropwhile__drop_all_elements():

    assert (hydra.lib.lists.drop_while((lambda x: hydra.lib.equality.lt(x, 10)), (1, 2, 3))) == (())

def test_dropwhile__drop_no_elements():

    assert (hydra.lib.lists.drop_while((lambda x: hydra.lib.equality.lt(x, 0)), (1, 2, 3))) == ((1, 2, 3))

def test_dropwhile__empty_list():

    assert (hydra.lib.lists.drop_while((lambda x: hydra.lib.equality.lt(x, 5)), ())) == (())

# elem

def test_elem__element_present():

    assert (hydra.lib.lists.elem(2, (1, 2, 3))) == (True)

def test_elem__element_not_present():

    assert (hydra.lib.lists.elem(4, (1, 2, 3))) == (False)

def test_elem__empty_list():

    assert (hydra.lib.lists.elem(1, ())) == (False)

def test_elem__single_element_present():

    assert (hydra.lib.lists.elem(1, (1,))) == (True)

def test_elem__single_element_not_present():

    assert (hydra.lib.lists.elem(2, (1,))) == (False)

def test_elem__duplicate_elements():

    assert (hydra.lib.lists.elem(2, (1, 2, 2, 3))) == (True)

def test_elem__string_element_present():

    assert (hydra.lib.lists.elem("hello", ("world", "hello", "test"))) == (True)

def test_elem__string_element_not_present():

    assert (hydra.lib.lists.elem("missing", ("world", "hello"))) == (False)

# filter

def test_filter__filter_positive_numbers():

    assert (hydra.lib.lists.filter((lambda x: hydra.lib.equality.gt(x, 0)), (-1, 2, -3, 4, 5))) == ((2, 4, 5))

def test_filter__filter_all_elements():

    assert (hydra.lib.lists.filter((lambda x: hydra.lib.equality.lt(x, 10)), (1, 2, 3))) == ((1, 2, 3))

def test_filter__filter_no_elements():

    assert (hydra.lib.lists.filter((lambda x: hydra.lib.equality.gt(x, 10)), (1, 2, 3))) == (())

def test_filter__empty_list():

    assert (hydra.lib.lists.filter((lambda x: hydra.lib.equality.gt(x, 0)), ())) == (())

# foldl

def test_foldl__sum_with_addition():

    assert (hydra.lib.lists.foldl(hydra.lib.math.add, 0, (1, 2, 3, 4))) == (10)

def test_foldl__product_with_multiplication():

    assert (hydra.lib.lists.foldl(hydra.lib.math.mul, 1, (2, 3, 4))) == (24)

def test_foldl__empty_list():

    assert (hydra.lib.lists.foldl(hydra.lib.math.add, 5, ())) == (5)

def test_foldl__single_element():

    assert (hydra.lib.lists.foldl(hydra.lib.math.add, 10, (5,))) == (15)

def test_foldl__subtraction_fold():

    assert (hydra.lib.lists.foldl(hydra.lib.math.sub, 10, (1, 2, 3))) == (4)

# group

def test_group__consecutive_duplicates():

    assert (hydra.lib.lists.group((1, 1, 2, 2, 2, 3, 1))) == (((1, 1), (2, 2, 2), (3,), (1,)))

def test_group__no_duplicates():

    assert (hydra.lib.lists.group((1, 2, 3))) == (((1,), (2,), (3,)))

def test_group__all_same():

    assert (hydra.lib.lists.group((1, 1, 1))) == (((1, 1, 1),))

def test_group__empty_list():

    assert (hydra.lib.lists.group(())) == (())

def test_group__single_element():

    assert (hydra.lib.lists.group((1,))) == (((1,),))

# head

def test_head__three_element_list():

    assert (hydra.lib.lists.head((1, 2, 3))) == (1)

def test_head__single_element_list():

    assert (hydra.lib.lists.head((42,))) == (42)

def test_head__negative_numbers():

    assert (hydra.lib.lists.head((-1, -2, -3))) == (-1)

def test_head__string_list():

    assert (hydra.lib.lists.head(("hello", "world"))) == ("hello")

# init

def test_init__multiple_elements():

    assert (hydra.lib.lists.init((1, 2, 3, 4))) == ((1, 2, 3))

def test_init__two_elements():

    assert (hydra.lib.lists.init((1, 2))) == ((1,))

def test_init__single_element():

    assert (hydra.lib.lists.init((1,))) == (())

def test_init__string_list():

    assert (hydra.lib.lists.init(("a", "b", "c"))) == (("a", "b"))

# intercalate

def test_intercalate__double_zero_separator():

    assert (hydra.lib.lists.intercalate((0, 0), ((1, 2, 3), (4, 5), (6, 7, 8)))) == ((1, 2, 3, 0, 0, 4, 5, 0, 0, 6, 7, 8))

def test_intercalate__empty_separator():

    assert (hydra.lib.lists.intercalate((), ((1, 2), (3, 4)))) == ((1, 2, 3, 4))

def test_intercalate__single_element_separator():

    assert (hydra.lib.lists.intercalate((99,), ((1,), (2,), (3,)))) == ((1, 99, 2, 99, 3))

def test_intercalate__empty_list_of_lists():

    assert (hydra.lib.lists.intercalate((0,), ())) == (())

def test_intercalate__single_list():

    assert (hydra.lib.lists.intercalate((0,), ((1, 2, 3),))) == ((1, 2, 3))

def test_intercalate__lists_with_empty_lists():

    assert (hydra.lib.lists.intercalate((0,), ((), (1,), ()))) == ((0, 1, 0))

# intersperse

def test_intersperse__string_interspersion():

    assert (hydra.lib.lists.intersperse("and", ("one", "two", "three"))) == (("one", "and", "two", "and", "three"))

def test_intersperse__single_element():

    assert (hydra.lib.lists.intersperse("x", ("only",))) == (("only",))

def test_intersperse__empty_list():

    assert (hydra.lib.lists.intersperse("x", ())) == (())

def test_intersperse__two_elements():

    assert (hydra.lib.lists.intersperse("+", ("a", "b"))) == (("a", "+", "b"))

def test_intersperse__number_interspersion():

    assert (hydra.lib.lists.intersperse(0, (1, 2, 3))) == ((1, 0, 2, 0, 3))

# last

def test_last__three_element_list():

    assert (hydra.lib.lists.last((1, 2, 3))) == (3)

def test_last__single_element_list():

    assert (hydra.lib.lists.last((42,))) == (42)

def test_last__negative_numbers():

    assert (hydra.lib.lists.last((-1, -2, -3))) == (-3)

def test_last__string_list():

    assert (hydra.lib.lists.last(("hello", "world"))) == ("world")

# length

def test_length__three_elements():

    assert (hydra.lib.lists.length((1, 2, 3))) == (3)

def test_length__empty_list():

    assert (hydra.lib.lists.length(())) == (0)

def test_length__single_element():

    assert (hydra.lib.lists.length((42,))) == (1)

def test_length__many_elements():

    assert (hydra.lib.lists.length((1, 2, 3, 4, 5, 6, 7, 8, 9, 10))) == (10)

def test_length__string_list():

    assert (hydra.lib.lists.length(("a", "b", "c"))) == (3)

# map

def test_map__string_to_uppercase():

    assert (hydra.lib.lists.map(hydra.lib.strings.to_upper, ("one", "two"))) == (("ONE", "TWO"))

def test_map__empty_list():

    assert (hydra.lib.lists.map(hydra.lib.strings.to_upper, ())) == (())

def test_map__single_element():

    assert (hydra.lib.lists.map(hydra.lib.strings.to_upper, ("hello",))) == (("HELLO",))

def test_map__number_negation():

    assert (hydra.lib.lists.map(hydra.lib.math.negate, (1, 2, 3))) == ((-1, -2, -3))

def test_map__identity_function():

    assert (hydra.lib.lists.map((lambda x1: hydra.lib.equality.identity(x1)), (1, 2, 3))) == ((1, 2, 3))

# nub

def test_nub__remove_duplicates():

    assert (hydra.lib.lists.nub((1, 2, 1, 3, 2, 4))) == ((1, 2, 3, 4))

def test_nub__no_duplicates():

    assert (hydra.lib.lists.nub((1, 2, 3))) == ((1, 2, 3))

def test_nub__all_duplicates():

    assert (hydra.lib.lists.nub((1, 1, 1))) == ((1,))

def test_nub__empty_list():

    assert (hydra.lib.lists.nub(())) == (())

def test_nub__single_element():

    assert (hydra.lib.lists.nub((1,))) == ((1,))

def test_nub__string_duplicates():

    assert (hydra.lib.lists.nub(("a", "b", "a", "c"))) == (("a", "b", "c"))

# null

def test_null__empty_int_list():

    assert (hydra.lib.lists.null(())) == (True)

def test_null__single_element():

    assert (hydra.lib.lists.null((1,))) == (False)

def test_null__multiple_elements():

    assert (hydra.lib.lists.null((1, 2, 3))) == (False)

def test_null__empty_string_list():

    assert (hydra.lib.lists.null(())) == (True)

def test_null__non_empty_string_list():

    assert (hydra.lib.lists.null(("a",))) == (False)

# pure

def test_pure__string_element():

    assert (hydra.lib.lists.pure("one")) == (("one",))

def test_pure__empty_string():

    assert (hydra.lib.lists.pure("")) == (("",))

def test_pure__number_element():

    assert (hydra.lib.lists.pure(42)) == ((42,))

def test_pure__negative_number():

    assert (hydra.lib.lists.pure(-5)) == ((-5,))

# replicate

def test_replicate__replicate_three_times():

    assert (hydra.lib.lists.replicate(3, 42)) == ((42, 42, 42))

def test_replicate__replicate_zero_times():

    assert (hydra.lib.lists.replicate(0, 1)) == (())

def test_replicate__replicate_once():

    assert (hydra.lib.lists.replicate(1, 99)) == ((99,))

def test_replicate__replicate_string():

    assert (hydra.lib.lists.replicate(2, "hello")) == (("hello", "hello"))

# reverse

def test_reverse__multiple_elements():

    assert (hydra.lib.lists.reverse((1, 2, 3, 4))) == ((4, 3, 2, 1))

def test_reverse__single_element():

    assert (hydra.lib.lists.reverse((1,))) == ((1,))

def test_reverse__empty_list():

    assert (hydra.lib.lists.reverse(())) == (())

def test_reverse__two_elements():

    assert (hydra.lib.lists.reverse((1, 2))) == ((2, 1))

def test_reverse__string_list():

    assert (hydra.lib.lists.reverse(("a", "b", "c"))) == (("c", "b", "a"))

# safeHead

def test_safehead__non_empty_int_list():

    assert (hydra.lib.lists.safe_head((1, 2, 3))) == (Just(1))

def test_safehead__empty_int_list():

    assert (hydra.lib.lists.safe_head(())) == (Nothing())

def test_safehead__single_element():

    assert (hydra.lib.lists.safe_head((42,))) == (Just(42))

def test_safehead__non_empty_string_list():

    assert (hydra.lib.lists.safe_head(("hello", "world"))) == (Just("hello"))

def test_safehead__empty_string_list():

    assert (hydra.lib.lists.safe_head(())) == (Nothing())

# singleton

def test_singleton__number_element():

    assert (hydra.lib.lists.singleton(42)) == ((42,))

def test_singleton__negative_number():

    assert (hydra.lib.lists.singleton(-1)) == ((-1,))

def test_singleton__zero():

    assert (hydra.lib.lists.singleton(0)) == ((0,))

def test_singleton__string_element():

    assert (hydra.lib.lists.singleton("hello")) == (("hello",))

# sort

def test_sort__unsorted_numbers():

    assert (hydra.lib.lists.sort((3, 1, 4, 1, 5))) == ((1, 1, 3, 4, 5))

def test_sort__already_sorted():

    assert (hydra.lib.lists.sort((1, 2, 3))) == ((1, 2, 3))

def test_sort__reverse_sorted():

    assert (hydra.lib.lists.sort((3, 2, 1))) == ((1, 2, 3))

def test_sort__single_element():

    assert (hydra.lib.lists.sort((1,))) == ((1,))

def test_sort__empty_list():

    assert (hydra.lib.lists.sort(())) == (())

def test_sort__duplicates():

    assert (hydra.lib.lists.sort((2, 1, 2, 3, 1))) == ((1, 1, 2, 2, 3))

def test_sort__string_sort():

    assert (hydra.lib.lists.sort(("zebra", "apple", "banana"))) == (("apple", "banana", "zebra"))

# sortOn

def test_sorton__sort_by_string_length():

    assert (hydra.lib.lists.sort_on(hydra.lib.strings.length, ("hello", "hi", "world"))) == (("hi", "hello", "world"))

def test_sorton__empty_string_list():

    assert (hydra.lib.lists.sort_on(hydra.lib.strings.length, ())) == (())

def test_sorton__single_string_element():

    assert (hydra.lib.lists.sort_on(hydra.lib.strings.length, ("test",))) == (("test",))

def test_sorton__sort_by_negation():

    assert (hydra.lib.lists.sort_on(hydra.lib.math.negate, (1, 3, 2))) == ((3, 2, 1))

def test_sorton__sort_by_absolute_value():

    assert (hydra.lib.lists.sort_on(hydra.lib.math.abs, (-1, -3, 2))) == ((-1, 2, -3))

# span

def test_span__span_less_than_3():

    assert (hydra.lib.lists.span((lambda x: hydra.lib.equality.lt(x, 3)), (1, 2, 3, 1, 2))) == (((1, 2), (3, 1, 2)))

def test_span__span_all_elements():

    assert (hydra.lib.lists.span((lambda x: hydra.lib.equality.lt(x, 10)), (1, 2, 3))) == (((1, 2, 3), ()))

def test_span__span_no_elements():

    assert (hydra.lib.lists.span((lambda x: hydra.lib.equality.gt(x, 10)), (1, 2, 3))) == (((), (1, 2, 3)))

def test_span__empty_list():

    assert (hydra.lib.lists.span((lambda x: hydra.lib.equality.lt(x, 5)), ())) == (((), ()))

# tail

def test_tail__multiple_elements():

    assert (hydra.lib.lists.tail((1, 2, 3, 4))) == ((2, 3, 4))

def test_tail__two_elements():

    assert (hydra.lib.lists.tail((1, 2))) == ((2,))

def test_tail__single_element():

    assert (hydra.lib.lists.tail((1,))) == (())

def test_tail__string_list():

    assert (hydra.lib.lists.tail(("a", "b", "c"))) == (("b", "c"))

# take

def test_take__take_from_beginning():

    assert (hydra.lib.lists.take(2, (1, 2, 3, 4, 5))) == ((1, 2))

def test_take__take_zero_elements():

    assert (hydra.lib.lists.take(0, (1, 2, 3))) == (())

def test_take__take_all_elements():

    assert (hydra.lib.lists.take(3, (1, 2, 3))) == ((1, 2, 3))

def test_take__take_more_than_length():

    assert (hydra.lib.lists.take(5, (1, 2))) == ((1, 2))

def test_take__take_from_empty_list():

    assert (hydra.lib.lists.take(3, ())) == (())

def test_take__take_negative_amount():

    assert (hydra.lib.lists.take(-1, (1, 2, 3))) == (())

# transpose

def test_transpose__square_matrix():

    assert (hydra.lib.lists.transpose(((1, 2, 3), (4, 5, 6)))) == (((1, 4), (2, 5), (3, 6)))

def test_transpose__empty_lists():

    assert (hydra.lib.lists.transpose(())) == (())

def test_transpose__single_row():

    assert (hydra.lib.lists.transpose(((1, 2, 3),))) == (((1,), (2,), (3,)))

def test_transpose__single_column():

    assert (hydra.lib.lists.transpose(((1,), (2,), (3,)))) == (((1, 2, 3),))

def test_transpose__ragged_matrix():

    assert (hydra.lib.lists.transpose(((1, 2), (3,), (4, 5, 6)))) == (((1, 3, 4), (2, 5), (6,)))

# zip

def test_zip__equal_length_lists():

    assert (hydra.lib.lists.zip((1, 2, 3), ("a", "b", "c"))) == (((1, "a"), (2, "b"), (3, "c")))

def test_zip__first_list_shorter():

    assert (hydra.lib.lists.zip((1, 2), ("a", "b", "c"))) == (((1, "a"), (2, "b")))

def test_zip__second_list_shorter():

    assert (hydra.lib.lists.zip((1, 2, 3), ("a", "b"))) == (((1, "a"), (2, "b")))

def test_zip__empty_first_list():

    assert (hydra.lib.lists.zip((), ("a", "b"))) == (())

def test_zip__empty_second_list():

    assert (hydra.lib.lists.zip((1, 2), ())) == (())

def test_zip__both_empty_lists():

    assert (hydra.lib.lists.zip((), ())) == (())

# zipWith

def test_zipwith__addition():

    assert (hydra.lib.lists.zip_with(hydra.lib.math.add, (1, 2, 3), (4, 5, 6))) == ((5, 7, 9))

def test_zipwith__first_list_shorter():

    assert (hydra.lib.lists.zip_with(hydra.lib.math.add, (1, 2), (4, 5, 6))) == ((5, 7))

def test_zipwith__second_list_shorter():

    assert (hydra.lib.lists.zip_with(hydra.lib.math.add, (1, 2, 3), (4, 5))) == ((5, 7))

def test_zipwith__empty_first_list():

    assert (hydra.lib.lists.zip_with(hydra.lib.math.add, (), (1, 2, 3))) == (())

def test_zipwith__empty_second_list():

    assert (hydra.lib.lists.zip_with(hydra.lib.math.add, (1, 2, 3), ())) == (())

def test_zipwith__string_concatenation():

    assert (hydra.lib.lists.zip_with(hydra.lib.strings.cat2, ("a", "b"), ("1", "2"))) == (("a1", "b2"))
