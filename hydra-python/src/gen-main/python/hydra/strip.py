"""Several functions for stripping annotations from types and terms."""

from __future__ import annotations
import hydra.core

def fullyStripTerm(t) :
    """Strip all annotations from a term, including first-class type annotations."""
    
    match t:
        case hydra.core.TermAnnotated(x):
            return fullyStripTerm(x.subject)
        
        case hydra.core.TermTyped(x):
            return fullyStripTerm(x.term)
        
        case _:
            return t

def stripTerm(t) :
    """Strip all annotations from a term."""
    
    match t:
        case hydra.core.TermAnnotated(x):
            return stripTerm(x.subject)
        
        case _:
            return t

def stripType(t) :
    """Strip all annotations from a term."""
    
    match t:
        case hydra.core.TypeAnnotated(x):
            return stripType(x.subject)
        
        case _:
            return t

def stripTypeParameters(t) :
    """Strip any top-level type lambdas from a type, extracting the (possibly nested) type body."""
    
    match stripType(t):
        case hydra.core.TypeLambda(lt):
            return stripTypeParameters(lt.body)
        
        case _:
            return t