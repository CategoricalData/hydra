"""String formatting types and functions."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import frozenlist
import hydra.core
import hydra.lib.chars
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.strings
import hydra.mantle

def capitalize(v1: str) -> str:
    """Capitalize the first letter of a string."""
    
    return map_first_letter(lambda v1: hydra.lib.strings.to_upper(v1), v1)

def convert_case(from_: hydra.mantle.CaseConvention, to: hydra.mantle.CaseConvention, original: str) -> str:
    """Convert a string from one case convention to another."""
    
    def parts() -> frozenlist[str]:
        by_underscores = hydra.lib.strings.split_on("_", original)
        def by_caps() -> frozenlist[str]:
            def split_on_uppercase(acc: frozenlist[frozenlist[int]], c: int) -> frozenlist[frozenlist[int]]:
                return hydra.lib.lists.concat2(hydra.lib.logic.if_else(hydra.lib.chars.is_upper(c), tuple([tuple([])]), tuple([])), hydra.lib.lists.cons(hydra.lib.lists.cons(c, hydra.lib.lists.head(acc)), hydra.lib.lists.tail(acc)))
            return hydra.lib.lists.map(lambda v1: hydra.lib.strings.from_list(v1), hydra.lib.lists.foldl(split_on_uppercase, tuple([tuple([])]), hydra.lib.lists.reverse(hydra.lib.strings.to_list(decapitalize(original)))))
        match from_:
            case hydra.mantle.CaseConvention.CAMEL:
                return by_caps
            
            case hydra.mantle.CaseConvention.PASCAL:
                return by_caps
            
            case hydra.mantle.CaseConvention.LOWER_SNAKE:
                return by_underscores
            
            case hydra.mantle.CaseConvention.UPPER_SNAKE:
                return by_underscores
    match to:
        case hydra.mantle.CaseConvention.CAMEL:
            return decapitalize(hydra.lib.strings.cat(hydra.lib.lists.map(lambda arg_: capitalize(hydra.lib.strings.to_lower(arg_)), parts)))
        
        case hydra.mantle.CaseConvention.PASCAL:
            return hydra.lib.strings.cat(hydra.lib.lists.map(lambda arg_: capitalize(hydra.lib.strings.to_lower(arg_)), parts))
        
        case hydra.mantle.CaseConvention.LOWER_SNAKE:
            return hydra.lib.strings.intercalate("_", hydra.lib.lists.map(lambda v1: hydra.lib.strings.to_lower(v1), parts))
        
        case hydra.mantle.CaseConvention.UPPER_SNAKE:
            return hydra.lib.strings.intercalate("_", hydra.lib.lists.map(lambda v1: hydra.lib.strings.to_upper(v1), parts))

def convert_case_camel_to_lower_snake(v1: str) -> str:
    """Convert a string from camel case to lower snake case."""
    
    return convert_case(hydra.mantle.CaseConvention.CAMEL, hydra.mantle.CaseConvention.LOWER_SNAKE, v1)

def convert_case_camel_to_upper_snake(v1: str) -> str:
    """Convert a string from camel case to upper snake case."""
    
    return convert_case(hydra.mantle.CaseConvention.CAMEL, hydra.mantle.CaseConvention.UPPER_SNAKE, v1)

def convert_case_pascal_to_upper_snake(v1: str) -> str:
    """Convert a string from pascal case to upper snake case."""
    
    return convert_case(hydra.mantle.CaseConvention.PASCAL, hydra.mantle.CaseConvention.UPPER_SNAKE, v1)

def decapitalize(v1: str) -> str:
    """Decapitalize the first letter of a string."""
    
    return map_first_letter(lambda v1: hydra.lib.strings.to_lower(v1), v1)

def map_first_letter(mapping: Callable[[str], str], s: str) -> str:
    """A helper which maps the first letter of a string to another string."""
    
    list = hydra.lib.strings.to_list(s)
    first_letter = mapping()(hydra.lib.strings.from_list(hydra.lib.lists.pure(hydra.lib.lists.head(list))))
    return hydra.lib.logic.if_else(hydra.lib.strings.is_empty(s), s, hydra.lib.strings.cat2(first_letter, hydra.lib.strings.from_list(hydra.lib.lists.tail(list))))