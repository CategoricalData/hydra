"""String formatting types and functions."""

from __future__ import annotations
import hydra.core
import hydra.lib.chars
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.strings
import hydra.mantle

def capitalize(v2):
    """Capitalize the first letter of a string."""
    
    return map_first_letter(lambda v1: hydra.lib.strings.to_upper(v1))(v2)

def convert_case(from_, to, original):
    """Convert a string from one case convention to another."""
    
    def parts():
        by_underscores = hydra.lib.strings.split_on("_")(original)
        def by_caps():
            def split_on_uppercase(acc, c):
                return hydra.lib.lists.concat2(hydra.lib.logic.if_else(hydra.lib.chars.is_upper(c))([[]])([]))(hydra.lib.lists.cons(hydra.lib.lists.cons(c)(hydra.lib.lists.head(acc)))(hydra.lib.lists.tail(acc)))
            return hydra.lib.lists.map(lambda v1: hydra.lib.strings.from_list(v1))(hydra.lib.lists.foldl(split_on_uppercase)([[]])(hydra.lib.lists.reverse(hydra.lib.strings.to_list(decapitalize(original)))))
        match from_:
            case hydra.mantle.CaseConventionCamel(_):
                return by_caps
            
            case hydra.mantle.CaseConventionPascal(_):
                return by_caps
            
            case hydra.mantle.CaseConventionLowerSnake(_):
                return by_underscores
            
            case hydra.mantle.CaseConventionUpperSnake(_):
                return by_underscores
            
            case _:
                raise TypeError("Unsupported CaseConvention")
    match to:
        case hydra.mantle.CaseConventionCamel(_):
            return decapitalize(hydra.lib.strings.cat(hydra.lib.lists.map(lambda x: capitalize(hydra.lib.strings.to_lower(x)))(parts)))
        
        case hydra.mantle.CaseConventionPascal(_):
            return hydra.lib.strings.cat(hydra.lib.lists.map(lambda x: capitalize(hydra.lib.strings.to_lower(x)))(parts))
        
        case hydra.mantle.CaseConventionLowerSnake(_):
            return hydra.lib.strings.intercalate("_")(hydra.lib.lists.map(lambda v1: hydra.lib.strings.to_lower(v1))(parts))
        
        case hydra.mantle.CaseConventionUpperSnake(_):
            return hydra.lib.strings.intercalate("_")(hydra.lib.lists.map(lambda v1: hydra.lib.strings.to_upper(v1))(parts))
        
        case _:
            raise TypeError("Unsupported CaseConvention")

def convert_case_camel_to_lower_snake(v3):
    """Convert a string from camel case to lower snake case."""
    
    return convert_case(hydra.mantle.CaseConvention.CAMEL)(hydra.mantle.CaseConvention.LOWER_SNAKE)(v3)

def convert_case_camel_to_upper_snake(v3):
    """Convert a string from camel case to upper snake case."""
    
    return convert_case(hydra.mantle.CaseConvention.CAMEL)(hydra.mantle.CaseConvention.UPPER_SNAKE)(v3)

def convert_case_pascal_to_upper_snake(v3):
    """Convert a string from pascal case to upper snake case."""
    
    return convert_case(hydra.mantle.CaseConvention.PASCAL)(hydra.mantle.CaseConvention.UPPER_SNAKE)(v3)

def decapitalize(v2):
    """Decapitalize the first letter of a string."""
    
    return map_first_letter(lambda v1: hydra.lib.strings.to_lower(v1))(v2)

def map_first_letter(mapping, s):
    """A helper which maps the first letter of a string to another string."""
    
    list = hydra.lib.strings.to_list(s)
    first_letter = mapping(hydra.lib.strings.from_list(hydra.lib.lists.pure(hydra.lib.lists.head(list))))
    return hydra.lib.logic.if_else(hydra.lib.strings.is_empty(s))(s)(hydra.lib.strings.cat2(first_letter)(hydra.lib.strings.from_list(hydra.lib.lists.tail(list))))