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
    
    mapFirstLetter(lambda v1: hydra.lib.strings.toUpper(v1))(v2)

def convertCase(from_, to, original):
    """Convert a string from one case convention to another."""
    
    def parts():
        byUnderscores = hydra.lib.strings.splitOn("_")(original)
        def byCaps():
            def splitOnUppercase(acc, c):
                hydra.lib.lists.concat2(hydra.lib.logic.ifElse(hydra.lib.chars.isUpper(c))([[]])([]))(hydra.lib.lists.cons(hydra.lib.lists.cons(c)(hydra.lib.lists.head(acc)))(hydra.lib.lists.tail(acc)))
            hydra.lib.lists.map(lambda v1: hydra.lib.strings.fromList(v1))(hydra.lib.lists.foldl(splitOnUppercase)([[]])(hydra.lib.lists.reverse(hydra.lib.strings.toList(decapitalize(original)))))
        match from_:
            case hydra.mantle.CaseConventionCamel(_):
                return byCaps
            
            case hydra.mantle.CaseConventionPascal(_):
                return byCaps
            
            case hydra.mantle.CaseConventionLowerSnake(_):
                return byUnderscores
            
            case hydra.mantle.CaseConventionUpperSnake(_):
                return byUnderscores
            
            case _:
                raise TypeError("Unsupported CaseConvention")
    match to:
        case hydra.mantle.CaseConventionCamel(_):
            return decapitalize(hydra.lib.strings.cat(hydra.lib.lists.map(lambda x: capitalize(hydra.lib.strings.toLower(x)))(parts)))
        
        case hydra.mantle.CaseConventionPascal(_):
            return hydra.lib.strings.cat(hydra.lib.lists.map(lambda x: capitalize(hydra.lib.strings.toLower(x)))(parts))
        
        case hydra.mantle.CaseConventionLowerSnake(_):
            return hydra.lib.strings.intercalate("_")(hydra.lib.lists.map(lambda v1: hydra.lib.strings.toLower(v1))(parts))
        
        case hydra.mantle.CaseConventionUpperSnake(_):
            return hydra.lib.strings.intercalate("_")(hydra.lib.lists.map(lambda v1: hydra.lib.strings.toUpper(v1))(parts))
        
        case _:
            raise TypeError("Unsupported CaseConvention")

def convertCaseCamelToLowerSnake(v3):
    """Convert a string from camel case to lower snake case."""
    
    convertCase(hydra.mantle.CaseConvention.CAMEL)(hydra.mantle.CaseConvention.LOWER_SNAKE)(v3)

def convertCaseCamelToUpperSnake(v3):
    """Convert a string from camel case to upper snake case."""
    
    convertCase(hydra.mantle.CaseConvention.CAMEL)(hydra.mantle.CaseConvention.UPPER_SNAKE)(v3)

def convertCasePascalToUpperSnake(v3):
    """Convert a string from pascal case to upper snake case."""
    
    convertCase(hydra.mantle.CaseConvention.PASCAL)(hydra.mantle.CaseConvention.UPPER_SNAKE)(v3)

def decapitalize(v2):
    """Decapitalize the first letter of a string."""
    
    mapFirstLetter(lambda v1: hydra.lib.strings.toLower(v1))(v2)

def mapFirstLetter(mapping, s):
    """A helper which maps the first letter of a string to another string."""
    
    list = hydra.lib.strings.toList(s)
    firstLetter = mapping(hydra.lib.strings.fromList(hydra.lib.lists.pure(hydra.lib.lists.head(list))))
    hydra.lib.logic.ifElse(hydra.lib.strings.isEmpty(s))(s)(hydra.lib.strings.cat2(firstLetter)(hydra.lib.strings.fromList(hydra.lib.lists.tail(list))))