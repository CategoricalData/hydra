# Note: this is an automatically generated file. Do not edit.
# hydra.lib.strings primitives

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

# cat

def test_cat__basic_concatenation():

    assert (hydra.lib.strings.cat(("one", "two", "three"))) == ("onetwothree")

def test_cat__single_string():

    assert (hydra.lib.strings.cat(("hello",))) == ("hello")

def test_cat__empty_list():

    assert (hydra.lib.strings.cat(())) == ("")

def test_cat__with_empty_strings():

    assert (hydra.lib.strings.cat(("", "one", "", ""))) == ("one")

def test_cat__all_empty_strings():

    assert (hydra.lib.strings.cat(("", "", "", ""))) == ("")

def test_cat__unicode_strings():

    assert (hydra.lib.strings.cat(("ñ", "世", "🌍"))) == ("ñ世🌍")

def test_cat__combining_characters():

    assert (hydra.lib.strings.cat(("e", "́"))) == ("é")

def test_cat__control_characters():

    assert (hydra.lib.strings.cat(("\n", "\t", "\r"))) == ("\n\t\r")

def test_cat__null_character():

    assert (hydra.lib.strings.cat(("hello", "\x00", "world"))) == ("hello\x00world")

# cat2

def test_cat2__basic_concatenation():

    assert (hydra.lib.strings.cat2("hello", "world")) == ("helloworld")

def test_cat2__empty_first_string():

    assert (hydra.lib.strings.cat2("", "world")) == ("world")

def test_cat2__empty_second_string():

    assert (hydra.lib.strings.cat2("hello", "")) == ("hello")

def test_cat2__both_empty_strings():

    assert (hydra.lib.strings.cat2("", "")) == ("")

def test_cat2__unicode_characters():

    assert (hydra.lib.strings.cat2("ñ", "世")) == ("ñ世")

def test_cat2__special_characters():

    assert (hydra.lib.strings.cat2("\n", "\t")) == ("\n\t")

def test_cat2__null_characters():

    assert (hydra.lib.strings.cat2("hello\x00", "world")) == ("hello\x00world")

# charAt

def test_charat__first_character():

    assert (hydra.lib.strings.char_at(0, "hello")) == (104)

def test_charat__middle_character():

    assert (hydra.lib.strings.char_at(2, "hello")) == (108)

def test_charat__last_character():

    assert (hydra.lib.strings.char_at(4, "hello")) == (111)

def test_charat__single_character_string():

    assert (hydra.lib.strings.char_at(0, "a")) == (97)

def test_charat__unicode_character():

    assert (hydra.lib.strings.char_at(0, "ñ")) == (241)

def test_charat__multi_byte_unicode():

    assert (hydra.lib.strings.char_at(0, "世")) == (19990)

def test_charat__second_of_combining_pair():

    assert (hydra.lib.strings.char_at(1, "é")) == (769)

# fromList

def test_fromlist__basic_ascii_string():

    assert (hydra.lib.strings.from_list((104, 101, 108, 108, 111))) == ("hello")

def test_fromlist__empty_code_point_list():

    assert (hydra.lib.strings.from_list(())) == ("")

def test_fromlist__single_character():

    assert (hydra.lib.strings.from_list((97,))) == ("a")

def test_fromlist__unicode_characters():

    assert (hydra.lib.strings.from_list((241, 19990, 127757))) == ("ñ世🌍")

def test_fromlist__combining_character_sequence():

    assert (hydra.lib.strings.from_list((101, 769))) == ("é")

def test_fromlist__special_characters():

    assert (hydra.lib.strings.from_list((10, 9, 13))) == ("\n\t\r")

def test_fromlist__null_character():

    assert (hydra.lib.strings.from_list((104, 0, 105))) == ("h\x00i")

# intercalate

def test_intercalate__comma_separator():

    assert (hydra.lib.strings.intercalate(",", ("one", "two", "three"))) == ("one,two,three")

def test_intercalate__empty_separator():

    assert (hydra.lib.strings.intercalate("", ("a", "b", "c"))) == ("abc")

def test_intercalate__multi_character_separator():

    assert (hydra.lib.strings.intercalate(" | ", ("A", "B", "C"))) == ("A | B | C")

def test_intercalate__empty_string_list():

    assert (hydra.lib.strings.intercalate(",", ())) == ("")

def test_intercalate__single_item_list():

    assert (hydra.lib.strings.intercalate(",", ("only",))) == ("only")

def test_intercalate__empty_strings_in_list():

    assert (hydra.lib.strings.intercalate(",", ("", "a", ""))) == (",a,")

def test_intercalate__unicode_separator():

    assert (hydra.lib.strings.intercalate("🌍", ("link1", "link2"))) == ("link1🌍link2")

def test_intercalate__newline_separator():

    assert (hydra.lib.strings.intercalate("\n", ("line1", "line2"))) == ("line1\nline2")

# length

def test_length__empty_string():

    assert (hydra.lib.strings.length("")) == (0)

def test_length__single_character():

    assert (hydra.lib.strings.length("a")) == (1)

def test_length__basic_word():

    assert (hydra.lib.strings.length("hello")) == (5)

def test_length__unicode_characters():

    assert (hydra.lib.strings.length("ñ世🌍")) == (3)

def test_length__combining_character_sequence():

    assert (hydra.lib.strings.length("é")) == (2)

def test_length__special_characters():

    assert (hydra.lib.strings.length("\n\t\r")) == (3)

# lines

def test_lines__single_line():

    assert (hydra.lib.strings.lines("hello world")) == (("hello world",))

def test_lines__two_lines():

    assert (hydra.lib.strings.lines("hello\nworld")) == (("hello", "world"))

def test_lines__three_lines():

    assert (hydra.lib.strings.lines("one\ntwo\nthree")) == (("one", "two", "three"))

def test_lines__empty_string():

    assert (hydra.lib.strings.lines("")) == (())

def test_lines__just_newline():

    assert (hydra.lib.strings.lines("\n")) == (("",))

def test_lines__trailing_newline():

    assert (hydra.lib.strings.lines("hello\n")) == (("hello",))

def test_lines__leading_newline():

    assert (hydra.lib.strings.lines("\nhello")) == (("", "hello"))

def test_lines__multiple_consecutive_newlines():

    assert (hydra.lib.strings.lines("a\n\nb")) == (("a", "", "b"))

def test_lines__unicode_content():

    assert (hydra.lib.strings.lines("ñ\n世")) == (("ñ", "世"))

def test_lines__tabs_not_split():

    assert (hydra.lib.strings.lines("a\tb\nc")) == (("a\tb", "c"))

# null

def test_null__empty_string():

    assert (hydra.lib.strings.null("")) == (True)

def test_null__single_character():

    assert (hydra.lib.strings.null("a")) == (False)

def test_null__space():

    assert (hydra.lib.strings.null(" ")) == (False)

def test_null__unicode_space():

    assert (hydra.lib.strings.null(" ")) == (False)

def test_null__newline():

    assert (hydra.lib.strings.null("\n")) == (False)

def test_null__null_character():

    assert (hydra.lib.strings.null("\x00")) == (False)

def test_null__multi_character():

    assert (hydra.lib.strings.null("hello")) == (False)

# splitOn

def test_spliton__basic_separator():

    assert (hydra.lib.strings.split_on("ss", "Mississippi")) == (("Mi", "i", "ippi"))

def test_spliton__single_char_separator():

    assert (hydra.lib.strings.split_on(" ", "one two three")) == (("one", "two", "three"))

def test_spliton__multi_char_separator():

    assert (hydra.lib.strings.split_on("  ", "a  b  c")) == (("a", "b", "c"))

def test_spliton__separator_not_found():

    assert (hydra.lib.strings.split_on("x", "hello")) == (("hello",))

def test_spliton__separator_at_start():

    assert (hydra.lib.strings.split_on("h", "hello")) == (("", "ello"))

def test_spliton__separator_at_end():

    assert (hydra.lib.strings.split_on("o", "hello")) == (("hell", ""))

def test_spliton__leading_and_trailing_separator():

    assert (hydra.lib.strings.split_on(" ", " one two ")) == (("", "one", "two", ""))

def test_spliton__whole_string_as_separator():

    assert (hydra.lib.strings.split_on("Mississippi", "Mississippi")) == (("", ""))

def test_spliton__consecutive_separators():

    assert (hydra.lib.strings.split_on(" ", "a  b")) == (("a", "", "b"))

def test_spliton__multiple_occurrences():

    assert (hydra.lib.strings.split_on("l", "hello")) == (("he", "", "o"))

def test_spliton__overlapping_pattern():

    assert (hydra.lib.strings.split_on("aa", "aaa")) == (("", "a"))

def test_spliton__empty_separator():

    assert (hydra.lib.strings.split_on("", "abc")) == (("", "a", "b", "c"))

def test_spliton__separator_on_empty_string():

    assert (hydra.lib.strings.split_on("x", "")) == (("",))

def test_spliton__both_empty():

    assert (hydra.lib.strings.split_on("", "")) == (("",))

def test_spliton__single_char_both():

    assert (hydra.lib.strings.split_on("a", "a")) == (("", ""))

def test_spliton__unicode_separator():

    assert (hydra.lib.strings.split_on("世", "hello世world")) == (("hello", "world"))

def test_spliton__unicode_content():

    assert (hydra.lib.strings.split_on(",", "ñ,世,🌍")) == (("ñ", "世", "🌍"))

def test_spliton__newline_separator():

    assert (hydra.lib.strings.split_on("\n", "line1\nline2\nline3")) == (("line1", "line2", "line3"))

# toList

def test_tolist__empty_string():

    assert (hydra.lib.strings.to_list("")) == (())

def test_tolist__single_character():

    assert (hydra.lib.strings.to_list("a")) == ((97,))

def test_tolist__basic_word():

    assert (hydra.lib.strings.to_list("hello")) == ((104, 101, 108, 108, 111))

def test_tolist__unicode_characters():

    assert (hydra.lib.strings.to_list("ñ世🌍")) == ((241, 19990, 127757))

def test_tolist__combining_character_sequence():

    assert (hydra.lib.strings.to_list("é")) == ((101, 769))

def test_tolist__control_characters():

    assert (hydra.lib.strings.to_list("\n\t\r")) == ((10, 9, 13))

def test_tolist__null_character():

    assert (hydra.lib.strings.to_list("h\x00i")) == ((104, 0, 105))

# toLower

def test_tolower__mixed_case():

    assert (hydra.lib.strings.to_lower("Hello World")) == ("hello world")

def test_tolower__all_uppercase():

    assert (hydra.lib.strings.to_lower("HELLO")) == ("hello")

def test_tolower__all_lowercase():

    assert (hydra.lib.strings.to_lower("hello")) == ("hello")

def test_tolower__empty_string():

    assert (hydra.lib.strings.to_lower("")) == ("")

def test_tolower__with_numbers_and_punctuation():

    assert (hydra.lib.strings.to_lower("Abc123, XYZ!")) == ("abc123, xyz!")

def test_tolower__control_characters():

    assert (hydra.lib.strings.to_lower("\n\t\r")) == ("\n\t\r")

def test_tolower__unicode_accented_chars():

    assert (hydra.lib.strings.to_lower("ÑÁÉÍÓÚ")) == ("ñáéíóú")

# toUpper

def test_toupper__mixed_case():

    assert (hydra.lib.strings.to_upper("hello World")) == ("HELLO WORLD")

def test_toupper__all_lowercase():

    assert (hydra.lib.strings.to_upper("hello")) == ("HELLO")

def test_toupper__all_uppercase():

    assert (hydra.lib.strings.to_upper("HELLO")) == ("HELLO")

def test_toupper__empty_string():

    assert (hydra.lib.strings.to_upper("")) == ("")

def test_toupper__with_numbers_and_punctuation():

    assert (hydra.lib.strings.to_upper("abc123, xyz!")) == ("ABC123, XYZ!")

def test_toupper__control_characters():

    assert (hydra.lib.strings.to_upper("\n\t\r")) == ("\n\t\r")

def test_toupper__unicode_accented_chars():

    assert (hydra.lib.strings.to_upper("ñáéíóú")) == ("ÑÁÉÍÓÚ")

# unlines

def test_unlines__multiple_lines():

    assert (hydra.lib.strings.unlines(("one", "two", "three"))) == ("one\ntwo\nthree\n")

def test_unlines__single_line():

    assert (hydra.lib.strings.unlines(("hello",))) == ("hello\n")

def test_unlines__empty_list():

    assert (hydra.lib.strings.unlines(())) == ("")

def test_unlines__with_empty_lines():

    assert (hydra.lib.strings.unlines(("hello", "", "world"))) == ("hello\n\nworld\n")

def test_unlines__all_empty_lines():

    assert (hydra.lib.strings.unlines(("", "", ""))) == ("\n\n\n")

def test_unlines__unicode_content():

    assert (hydra.lib.strings.unlines(("ñoño", "世界"))) == ("ñoño\n世界\n")
