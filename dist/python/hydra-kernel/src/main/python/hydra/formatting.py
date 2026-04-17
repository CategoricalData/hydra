# Note: this is an automatically generated file. Do not edit.

r"""String formatting types and functions."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.lib.chars
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.util

T0 = TypeVar("T0")

def map_first_letter(mapping: Callable[[str], str], s: str) -> str:
    r"""A helper which maps the first letter of a string to another string."""

    return hydra.lib.logic.if_else(hydra.lib.strings.null(s), (lambda : s), (lambda : (list := hydra.lib.strings.to_list(s), hydra.lib.maybes.from_maybe((lambda : s), hydra.lib.maybes.map((lambda uc: (first_letter := mapping(hydra.lib.strings.from_list(hydra.lib.lists.pure(hydra.lib.pairs.first(uc)))), hydra.lib.strings.cat2(first_letter, hydra.lib.strings.from_list(hydra.lib.pairs.second(uc))))[1]), hydra.lib.lists.uncons(list))))[1]))

def capitalize(v1: str) -> str:
    r"""Capitalize the first letter of a string."""

    return map_first_letter(hydra.lib.strings.to_upper, v1)

def decapitalize(v1: str) -> str:
    r"""Decapitalize the first letter of a string."""

    return map_first_letter(hydra.lib.strings.to_lower, v1)

def convert_case(from_: hydra.util.CaseConvention, to: hydra.util.CaseConvention, original: str) -> str:
    r"""Convert a string from one case convention to another."""

    @lru_cache(1)
    def parts() -> frozenlist[str]:
        @lru_cache(1)
        def by_caps() -> frozenlist[str]:
            def split_on_uppercase(acc: frozenlist[frozenlist[int]], c: int) -> frozenlist[frozenlist[int]]:
                return hydra.lib.lists.concat2(hydra.lib.logic.if_else(hydra.lib.chars.is_upper(c), (lambda : ((),)), (lambda : ())), hydra.lib.maybes.from_maybe((lambda : acc), hydra.lib.maybes.map((lambda uc: hydra.lib.lists.cons(hydra.lib.lists.cons(c, hydra.lib.pairs.first(uc)), hydra.lib.pairs.second(uc))), hydra.lib.lists.uncons(acc))))
            return hydra.lib.lists.map(hydra.lib.strings.from_list, hydra.lib.lists.foldl(split_on_uppercase, ((),), hydra.lib.lists.reverse(hydra.lib.strings.to_list(decapitalize(original)))))
        @lru_cache(1)
        def by_underscores() -> frozenlist[str]:
            return hydra.lib.strings.split_on("_", original)
        match from_:
            case hydra.util.CaseConvention.CAMEL:
                return by_caps()

            case hydra.util.CaseConvention.PASCAL:
                return by_caps()

            case hydra.util.CaseConvention.LOWER_SNAKE:
                return by_underscores()

            case hydra.util.CaseConvention.UPPER_SNAKE:
                return by_underscores()

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match to:
        case hydra.util.CaseConvention.CAMEL:
            return decapitalize(hydra.lib.strings.cat(hydra.lib.lists.map((lambda arg_: capitalize(hydra.lib.strings.to_lower(arg_))), parts())))

        case hydra.util.CaseConvention.PASCAL:
            return hydra.lib.strings.cat(hydra.lib.lists.map((lambda arg_: capitalize(hydra.lib.strings.to_lower(arg_))), parts()))

        case hydra.util.CaseConvention.LOWER_SNAKE:
            return hydra.lib.strings.intercalate("_", hydra.lib.lists.map(hydra.lib.strings.to_lower, parts()))

        case hydra.util.CaseConvention.UPPER_SNAKE:
            return hydra.lib.strings.intercalate("_", hydra.lib.lists.map(hydra.lib.strings.to_upper, parts()))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def convert_case_camel_to_lower_snake(v1: str) -> str:
    r"""Convert a string from camel case to lower snake case."""

    return convert_case(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.LOWER_SNAKE, v1)

def convert_case_camel_or_underscore_to_lower_snake(s: str) -> str:
    r"""Convert a string from camel case (possibly with underscores) to lower snake case. Splits on underscores first, then converts each part from camel case."""

    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on("_", s)
    @lru_cache(1)
    def snake_parts() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda p: convert_case_camel_to_lower_snake(p)), parts())
    return hydra.lib.strings.intercalate("_", snake_parts())

def convert_case_camel_to_upper_snake(v1: str) -> str:
    r"""Convert a string from camel case to upper snake case."""

    return convert_case(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.UPPER_SNAKE, v1)

def convert_case_pascal_to_upper_snake(v1: str) -> str:
    r"""Convert a string from pascal case to upper snake case."""

    return convert_case(hydra.util.CaseConvention.PASCAL, hydra.util.CaseConvention.UPPER_SNAKE, v1)

def escape_with_underscore(reserved: frozenset[str], s: str) -> str:
    r"""Escape reserved words by appending an underscore."""

    return hydra.lib.logic.if_else(hydra.lib.sets.member(s, reserved), (lambda : hydra.lib.strings.cat2(s, "_")), (lambda : s))

def indent_lines(s: str) -> str:
    r"""Indent each line of a string with four spaces."""

    def indent(l: str) -> str:
        return hydra.lib.strings.cat2("    ", l)
    return hydra.lib.strings.unlines(hydra.lib.lists.map(indent, hydra.lib.strings.lines(s)))

def java_style_comment(s: str) -> str:
    r"""Format a string as a Java-style block comment."""

    return hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("/**\n", " * "), s), "\n */")

def non_alnum_to_underscores(input: str) -> str:
    r"""Replace sequences of non-alphanumeric characters with single underscores."""

    def is_alnum(c: int) -> bool:
        return hydra.lib.logic.or_(hydra.lib.logic.and_(hydra.lib.equality.gte(c, 65), hydra.lib.equality.lte(c, 90)), hydra.lib.logic.or_(hydra.lib.logic.and_(hydra.lib.equality.gte(c, 97), hydra.lib.equality.lte(c, 122)), hydra.lib.logic.and_(hydra.lib.equality.gte(c, 48), hydra.lib.equality.lte(c, 57))))
    def replace(p: tuple[frozenlist[int], bool], c: int) -> tuple[frozenlist[int], bool]:
        @lru_cache(1)
        def s() -> frozenlist[int]:
            return hydra.lib.pairs.first(p)
        @lru_cache(1)
        def b() -> bool:
            return hydra.lib.pairs.second(p)
        return hydra.lib.logic.if_else(is_alnum(c), (lambda : (hydra.lib.lists.cons(c, s()), False)), (lambda : hydra.lib.logic.if_else(b(), (lambda : (s(), True)), (lambda : (hydra.lib.lists.cons(95, s()), True)))))
    @lru_cache(1)
    def result() -> tuple[frozenlist[int], bool]:
        return hydra.lib.lists.foldl(replace, ((), False), hydra.lib.strings.to_list(input))
    return hydra.lib.strings.from_list(hydra.lib.lists.reverse(hydra.lib.pairs.first(result())))

def strip_leading_and_trailing_whitespace(s: str) -> str:
    r"""Remove leading and trailing whitespace from a string."""

    return hydra.lib.strings.from_list(hydra.lib.lists.drop_while(hydra.lib.chars.is_space, hydra.lib.lists.reverse(hydra.lib.lists.drop_while(hydra.lib.chars.is_space, hydra.lib.lists.reverse(hydra.lib.strings.to_list(s))))))

def normalize_comment(s: str) -> str:
    r"""Normalize a comment string for consistent output across coders."""

    @lru_cache(1)
    def stripped() -> str:
        return strip_leading_and_trailing_whitespace(s)
    return hydra.lib.logic.if_else(hydra.lib.strings.null(stripped()), (lambda : ""), (lambda : (last_idx := hydra.lib.math.sub(hydra.lib.strings.length(stripped()), 1), (appended := hydra.lib.strings.cat2(stripped(), "."), hydra.lib.maybes.maybe((lambda : appended), (lambda last_char: hydra.lib.logic.if_else(hydra.lib.equality.equal(last_char, 46), (lambda : stripped()), (lambda : appended))), hydra.lib.strings.maybe_char_at(last_idx, stripped())))[1])[1]))

def sanitize_with_underscores(reserved: frozenset[str], s: str) -> str:
    r"""Sanitize a string by replacing non-alphanumeric characters and escaping reserved words."""

    return escape_with_underscore(reserved, non_alnum_to_underscores(s))

def show_list(f: Callable[[T0], str], els: frozenlist[T0]) -> str:
    r"""Format a list of elements as a bracketed, comma-separated string."""

    return hydra.lib.strings.cat(("[", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map(f, els)), "]"))

def with_character_aliases(original: str) -> str:
    r"""Replace special characters with their alphanumeric aliases."""

    @lru_cache(1)
    def aliases() -> FrozenDict[int, str]:
        return hydra.lib.maps.from_list(((32, "sp"), (33, "excl"), (34, "quot"), (35, "num"), (36, "dollar"), (37, "percnt"), (38, "amp"), (39, "apos"), (40, "lpar"), (41, "rpar"), (42, "ast"), (43, "plus"), (44, "comma"), (45, "minus"), (46, "period"), (47, "sol"), (58, "colon"), (59, "semi"), (60, "lt"), (61, "equals"), (62, "gt"), (63, "quest"), (64, "commat"), (91, "lsqb"), (92, "bsol"), (93, "rsqb"), (94, "circ"), (95, "lowbar"), (96, "grave"), (123, "lcub"), (124, "verbar"), (125, "rcub"), (126, "tilde")))
    def alias(c: int) -> frozenlist[int]:
        return hydra.lib.maybes.from_maybe((lambda : hydra.lib.lists.pure(c)), hydra.lib.maybes.map(hydra.lib.strings.to_list, hydra.lib.maps.lookup(c, aliases())))
    return hydra.lib.strings.from_list(hydra.lib.lists.filter(hydra.lib.chars.is_alpha_num, hydra.lib.lists.concat(hydra.lib.lists.map(alias, hydra.lib.strings.to_list(original)))))

def wrap_line(maxlen: int, input: str) -> str:
    r"""A simple soft line wrap which is suitable for code comments."""

    def helper(prev: frozenlist[frozenlist[int]], rem: frozenlist[int]) -> frozenlist[frozenlist[int]]:
        @lru_cache(1)
        def trunc() -> frozenlist[int]:
            return hydra.lib.lists.take(maxlen, rem)
        @lru_cache(1)
        def span_result() -> tuple[frozenlist[int], frozenlist[int]]:
            return hydra.lib.lists.span((lambda c: hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.equality.equal(c, 32)), hydra.lib.logic.not_(hydra.lib.equality.equal(c, 9)))), hydra.lib.lists.reverse(trunc()))
        @lru_cache(1)
        def prefix() -> frozenlist[int]:
            return hydra.lib.lists.reverse(hydra.lib.pairs.second(span_result()))
        @lru_cache(1)
        def suffix() -> frozenlist[int]:
            return hydra.lib.lists.reverse(hydra.lib.pairs.first(span_result()))
        return hydra.lib.logic.if_else(hydra.lib.equality.lte(hydra.lib.lists.length(rem), maxlen), (lambda : hydra.lib.lists.reverse(hydra.lib.lists.cons(rem, prev))), (lambda : hydra.lib.logic.if_else(hydra.lib.lists.null(prefix()), (lambda : helper(hydra.lib.lists.cons(trunc(), prev), hydra.lib.lists.drop(maxlen, rem))), (lambda : hydra.lib.maybes.from_maybe((lambda : helper(hydra.lib.lists.cons(trunc(), prev), hydra.lib.lists.drop(maxlen, rem))), hydra.lib.maybes.map((lambda pfx_init: helper(hydra.lib.lists.cons(pfx_init, prev), hydra.lib.lists.concat2(suffix(), hydra.lib.lists.drop(maxlen, rem)))), hydra.lib.lists.maybe_init(prefix())))))))
    return hydra.lib.strings.from_list(hydra.lib.lists.intercalate((10,), helper((), hydra.lib.strings.to_list(input))))
