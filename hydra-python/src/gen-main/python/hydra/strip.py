"""Several functions for stripping annotations from types and terms."""

from __future__ import annotations
import hydra.core

def fully_strip_term(t):
    """Strip all annotations from a term, including first-class type annotations."""
    
    match t:
        case hydra.core.TermAnnotated(x):
            return fully_strip_term(x.subject)
        
        case hydra.core.TermTyped(x):
            return fully_strip_term(x.term)
        
        case _:
            return t

def strip_term(t):
    """Strip all annotations from a term."""
    
    match t:
        case hydra.core.TermAnnotated(x):
            return strip_term(x.subject)
        
        case _:
            return t

def strip_type(t):
    """Strip all annotations from a term."""
    
    match t:
        case hydra.core.TypeAnnotated(x):
            return strip_type(x.subject)
        
        case _:
            return t

def strip_type_parameters(t):
    """Strip any top-level type lambdas from a type, extracting the (possibly nested) type body."""
    
    match strip_type(t):
        case hydra.core.TypeLambda(lt):
            return strip_type_parameters(lt.body)
        
        case _:
            return t