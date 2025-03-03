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
    
    return map_first_letter(lambda v1: hydra.lib.strings.to_upper(V1))(V2)

def convert_case(from_, to, original):
    """Convert a string from one case convention to another."""
    
    def parts():
        by_underscores = hydra.lib.strings.split_on("_")(Original)
        def by_caps():
            def split_on_uppercase(acc, c):
                return hydra.lib.lists.concat2(hydra.lib.logic.if_else(hydra.lib.chars.is_upper(C))(tuple([tuple([])]))(tuple([])))(hydra.lib.lists.cons(hydra.lib.lists.cons(C)(hydra.lib.lists.head(Acc)))(hydra.lib.lists.tail(Acc)))
            return hydra.lib.lists.map(lambda v1: hydra.lib.strings.from_list(V1))(hydra.lib.lists.foldl(SplitOnUppercase)(tuple([tuple([])]))(hydra.lib.lists.reverse(hydra.lib.strings.to_list(decapitalize(Original)))))
        match From:
            case hydra.mantle.CaseConventionCamel(_):
                return ByCaps
            
            case hydra.mantle.CaseConventionPascal(_):
                return ByCaps
            
            case hydra.mantle.CaseConventionLowerSnake(_):
                return ByUnderscores
            
            case hydra.mantle.CaseConventionUpperSnake(_):
                return ByUnderscores
            
            case _:
                raise TypeError("Unsupported CaseConvention")
    match To:
        case hydra.mantle.CaseConventionCamel(_):
            return decapitalize(hydra.lib.strings.cat(hydra.lib.lists.map(lambda x: capitalize(hydra.lib.strings.to_lower(X)))(Parts)))
        
        case hydra.mantle.CaseConventionPascal(_):
            return hydra.lib.strings.cat(hydra.lib.lists.map(lambda x: capitalize(hydra.lib.strings.to_lower(X)))(Parts))
        
        case hydra.mantle.CaseConventionLowerSnake(_):
            return hydra.lib.strings.intercalate("_")(hydra.lib.lists.map(lambda v1: hydra.lib.strings.to_lower(V1))(Parts))
        
        case hydra.mantle.CaseConventionUpperSnake(_):
            return hydra.lib.strings.intercalate("_")(hydra.lib.lists.map(lambda v1: hydra.lib.strings.to_upper(V1))(Parts))
        
        case _:
            raise TypeError("Unsupported CaseConvention")

def convert_case_camel_to_lower_snake(v3):
    """Convert a string from camel case to lower snake case."""
    
    return convert_case(hydra.mantle.CaseConvention.CAMEL)(hydra.mantle.CaseConvention.LOWER_SNAKE)(V3)

def convert_case_camel_to_upper_snake(v3):
    """Convert a string from camel case to upper snake case."""
    
    return convert_case(hydra.mantle.CaseConvention.CAMEL)(hydra.mantle.CaseConvention.UPPER_SNAKE)(V3)

def convert_case_pascal_to_upper_snake(v3):
    """Convert a string from pascal case to upper snake case."""
    
    return convert_case(hydra.mantle.CaseConvention.PASCAL)(hydra.mantle.CaseConvention.UPPER_SNAKE)(V3)

def decapitalize(v2):
    """Decapitalize the first letter of a string."""
    
    return map_first_letter(lambda v1: hydra.lib.strings.to_lower(V1))(V2)

def map_first_letter(mapping, s):
    """A helper which maps the first letter of a string to another string."""
    
    list = hydra.lib.strings.to_list(S)
    first_letter = mapping(hydra.lib.strings.from_list(hydra.lib.lists.pure(hydra.lib.lists.head(List))))
    return hydra.lib.logic.if_else(hydra.lib.strings.is_empty(S))(S)(hydra.lib.strings.cat2(FirstLetter)(hydra.lib.strings.from_list(hydra.lib.lists.tail(List))))