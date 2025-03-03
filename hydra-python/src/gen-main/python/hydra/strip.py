"""Several functions for stripping annotations from types and terms."""

from __future__ import annotations
import hydra.core

def fully_strip_term(t):
    """Strip all annotations from a term, including first-class type annotations."""
    
    match T:
        case hydra.core.TermAnnotated(x):
            return fully_strip_term(X.subject)
        
        case hydra.core.TermTyped(x):
            return fully_strip_term(X.term)
        
        case _:
            return T

def strip_term(t):
    """Strip all annotations from a term."""
    
    match T:
        case hydra.core.TermAnnotated(x):
            return strip_term(X.subject)
        
        case _:
            return T

def strip_type(t):
    """Strip all annotations from a term."""
    
    match T:
        case hydra.core.TypeAnnotated(x):
            return strip_type(X.subject)
        
        case _:
            return T

def strip_type_parameters(t):
    """Strip any top-level type lambdas from a type, extracting the (possibly nested) type body."""
    
    match strip_type(T):
        case hydra.core.TypeLambda(lt):
            return strip_type_parameters(Lt.body)
        
        case _:
            return T