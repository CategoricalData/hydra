"""Several functions for stripping annotations from types and terms."""

from __future__ import annotations
import hydra.core

def fully_strip_term(t: hydra.core.Term) -> hydra.core.Term:
    """Strip all annotations from a term, including first-class type annotations."""
    
    match t:
        case hydra.core.TermAnnotated(arg_):
            return fully_strip_term(arg_.subject)
        
        case hydra.core.TermTyped(arg_):
            return fully_strip_term(arg_.term)
        
        case _:
            return t

def strip_term(t: hydra.core.Term) -> hydra.core.Term:
    """Strip all annotations from a term."""
    
    match t:
        case hydra.core.TermAnnotated(arg_):
            return strip_term(arg_.subject)
        
        case _:
            return t

def strip_type(t: hydra.core.Type) -> hydra.core.Type:
    """Strip all annotations from a term."""
    
    match t:
        case hydra.core.TypeAnnotated(arg_):
            return strip_type(arg_.subject)
        
        case _:
            return t

def strip_type_parameters(t: hydra.core.Type) -> hydra.core.Type:
    """Strip any top-level type lambdas from a type, extracting the (possibly nested) type body."""
    
    match strip_type(t):
        case hydra.core.TypeForall(lt):
            return strip_type_parameters(lt.body)
        
        case _:
            return t
