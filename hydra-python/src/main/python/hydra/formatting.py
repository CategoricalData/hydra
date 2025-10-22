# Note: this is an automatically generated file. Do not edit.

"""String formatting types and functions."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import frozenlist, FrozenDict
from typing import Tuple
import hydra.core
import hydra.lib.chars
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.optionals
import hydra.lib.sets
import hydra.lib.strings
import hydra.mantle

def map_first_letter(mapping: Callable[[str], str], s: str) -> str:
    """A helper which maps the first letter of a string to another string."""
    
    list = hydra.lib.strings.to_list(s)
    first_letter = mapping(hydra.lib.strings.from_list(hydra.lib.lists.pure(hydra.lib.lists.head(list))))
    return hydra.lib.logic.if_else(hydra.lib.strings.null(s), s, hydra.lib.strings.cat2(first_letter, hydra.lib.strings.from_list(hydra.lib.lists.tail(list))))

def capitalize(v1: str) -> str:
    """Capitalize the first letter of a string."""
    
    return map_first_letter(lambda v12: hydra.lib.strings.to_upper(v12), v1)

def decapitalize(v1: str) -> str:
    """Decapitalize the first letter of a string."""
    
    return map_first_letter(lambda v12: hydra.lib.strings.to_lower(v12), v1)

def convert_case(from_: hydra.mantle.CaseConvention, to: hydra.mantle.CaseConvention, original: str) -> str:
    """Convert a string from one case convention to another."""
    
    def parts() -> frozenlist[str]:
        def by_caps() -> frozenlist[str]:
            def split_on_uppercase(acc: frozenlist[frozenlist[int]], c: int) -> frozenlist[frozenlist[int]]:
                return hydra.lib.lists.concat2(hydra.lib.logic.if_else(hydra.lib.chars.is_upper(c), ((),), ()), hydra.lib.lists.cons(hydra.lib.lists.cons(c, hydra.lib.lists.head(acc)), hydra.lib.lists.tail(acc)))
            return hydra.lib.lists.map(lambda v1: hydra.lib.strings.from_list(v1), hydra.lib.lists.foldl(split_on_uppercase, ((),), hydra.lib.lists.reverse(hydra.lib.strings.to_list(decapitalize(original)))))
        by_underscores = hydra.lib.strings.split_on("_", original)
        match from_:
            case hydra.mantle.CaseConvention.CAMEL:
                return by_caps()
            
            case hydra.mantle.CaseConvention.PASCAL:
                return by_caps()
            
            case hydra.mantle.CaseConvention.LOWER_SNAKE:
                return by_underscores
            
            case hydra.mantle.CaseConvention.UPPER_SNAKE:
                return by_underscores
    match to:
        case hydra.mantle.CaseConvention.CAMEL:
            return decapitalize(hydra.lib.strings.cat(hydra.lib.lists.map(lambda arg_: capitalize(hydra.lib.strings.to_lower(arg_)), parts())))
        
        case hydra.mantle.CaseConvention.PASCAL:
            return hydra.lib.strings.cat(hydra.lib.lists.map(lambda arg_: capitalize(hydra.lib.strings.to_lower(arg_)), parts()))
        
        case hydra.mantle.CaseConvention.LOWER_SNAKE:
            return hydra.lib.strings.intercalate("_", hydra.lib.lists.map(lambda v1: hydra.lib.strings.to_lower(v1), parts()))
        
        case hydra.mantle.CaseConvention.UPPER_SNAKE:
            return hydra.lib.strings.intercalate("_", hydra.lib.lists.map(lambda v1: hydra.lib.strings.to_upper(v1), parts()))

def convert_case_camel_to_lower_snake(v1: str) -> str:
    """Convert a string from camel case to lower snake case."""
    
    return convert_case(hydra.mantle.CaseConvention.CAMEL, hydra.mantle.CaseConvention.LOWER_SNAKE, v1)

def convert_case_camel_to_upper_snake(v1: str) -> str:
    """Convert a string from camel case to upper snake case."""
    
    return convert_case(hydra.mantle.CaseConvention.CAMEL, hydra.mantle.CaseConvention.UPPER_SNAKE, v1)

def convert_case_pascal_to_upper_snake(v1: str) -> str:
    """Convert a string from pascal case to upper snake case."""
    
    return convert_case(hydra.mantle.CaseConvention.PASCAL, hydra.mantle.CaseConvention.UPPER_SNAKE, v1)

def escape_with_underscore(reserved: frozenset[str], s: str) -> str:
    return hydra.lib.logic.if_else(hydra.lib.sets.member(s, reserved), hydra.lib.strings.cat((s, "_")), s)

def indent_lines(s: str) -> str:
    def indent(l: str) -> str:
        return hydra.lib.strings.cat(("    ", l))
    return hydra.lib.strings.unlines(hydra.lib.lists.map(indent, hydra.lib.strings.lines(s)))

def java_style_comment(s: str) -> str:
    return hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat(("/**\n", " * ")), s)), "\n */"))

def non_alnum_to_underscores(input: str) -> str:
    def is_alnum(c: int) -> bool:
        return hydra.lib.logic.or_(hydra.lib.logic.and_(hydra.lib.equality.gte(c, 65), hydra.lib.equality.lte(c, 90)), hydra.lib.logic.or_(hydra.lib.logic.and_(hydra.lib.equality.gte(c, 97), hydra.lib.equality.lte(c, 122)), hydra.lib.logic.and_(hydra.lib.equality.gte(c, 48), hydra.lib.equality.lte(c, 57))))
    def replace(p: Tuple[frozenlist[int], bool], c: int) -> Tuple[frozenlist[int], bool]:
        s = p[0]
        b = p[1]
        return hydra.lib.logic.if_else(is_alnum(c), (hydra.lib.lists.cons(c, s), False), hydra.lib.logic.if_else(b, (s, True), (hydra.lib.lists.cons(95, s), True)))
    result = hydra.lib.lists.foldl(replace, ((), False), hydra.lib.strings.to_list(input))
    return hydra.lib.strings.from_list(hydra.lib.lists.reverse(result[0]))

def sanitize_with_underscores(reserved: frozenset[str], s: str) -> str:
    return escape_with_underscore(reserved, non_alnum_to_underscores(s))

def show_list[T0](f: Callable[[T0], str], els: frozenlist[T0]) -> str:
    return hydra.lib.strings.cat(("[", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map(f, els)), "]"))

def strip_leading_and_trailing_whitespace(s: str) -> str:
    return hydra.lib.strings.from_list(hydra.lib.lists.drop_while(lambda v1: hydra.lib.chars.is_space(v1), hydra.lib.lists.reverse(hydra.lib.lists.drop_while(lambda v1: hydra.lib.chars.is_space(v1), hydra.lib.lists.reverse(hydra.lib.strings.to_list(s))))))

def with_character_aliases(original: str) -> str:
    aliases: FrozenDict[int, str] = hydra.lib.maps.from_list(((32, "sp"), (33, "excl"), (34, "quot"), (35, "num"), (36, "dollar"), (37, "percnt"), (38, "amp"), (39, "apos"), (40, "lpar"), (41, "rpar"), (42, "ast"), (43, "plus"), (44, "comma"), (45, "minus"), (46, "period"), (47, "sol"), (58, "colon"), (59, "semi"), (60, "lt"), (61, "equals"), (62, "gt"), (63, "quest"), (64, "commat"), (91, "lsqb"), (92, "bsol"), (93, "rsqb"), (94, "circ"), (95, "lowbar"), (96, "grave"), (123, "lcub"), (124, "verbar"), (125, "rcub"), (126, "tilde")))
    def alias(c: int) -> frozenlist[int]:
        return hydra.lib.optionals.from_maybe(hydra.lib.lists.pure(c), hydra.lib.optionals.map(lambda v1: hydra.lib.strings.to_list(v1), hydra.lib.maps.lookup(c, aliases)))
    return hydra.lib.strings.from_list(hydra.lib.lists.filter(lambda v1: hydra.lib.chars.is_alpha_num(v1), hydra.lib.lists.concat(hydra.lib.lists.map(alias, hydra.lib.strings.to_list(original)))))

def wrap_line(maxlen: int, input: str) -> str:
    """A simple soft line wrap which is suitable for code comments."""
    
    def helper(prev: frozenlist[frozenlist[int]], rem: frozenlist[int]) -> frozenlist[frozenlist[int]]:
        trunc = hydra.lib.lists.take(maxlen, rem)
        span_result = hydra.lib.lists.span(lambda c: hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.lib.equality.equal(c, 32)), hydra.lib.logic.not_(hydra.lib.equality.equal(c, 9))), hydra.lib.lists.reverse(trunc))
        prefix = hydra.lib.lists.reverse(span_result[1])
        suffix = hydra.lib.lists.reverse(span_result[0])
        return hydra.lib.logic.if_else(hydra.lib.equality.lte(hydra.lib.lists.length(rem), maxlen), hydra.lib.lists.reverse(hydra.lib.lists.cons(rem, prev)), hydra.lib.logic.if_else(hydra.lib.lists.null(prefix), helper(hydra.lib.lists.cons(trunc, prev), hydra.lib.lists.drop(maxlen, rem)), helper(hydra.lib.lists.cons(hydra.lib.lists.init(prefix), prev), hydra.lib.lists.concat2(suffix, hydra.lib.lists.drop(maxlen, rem)))))
    return hydra.lib.strings.from_list(hydra.lib.lists.intercalate((10,), helper((), hydra.lib.strings.to_list(input))))
