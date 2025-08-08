"""Several functions for stripping annotations from types and terms."""

from __future__ import annotations

import hydra.gen.core


def fully_strip_term(t: hydra.gen.core.Term) -> hydra.gen.core.Term:
    """Strip all annotations from a term, including first-class type annotations."""
    match t:
        case hydra.gen.core.TermAnnotated(arg_):
            return fully_strip_term(arg_.subject)
        
        case hydra.gen.core.TermTyped(arg_):
            return fully_strip_term(arg_.term)
        
        case _:
            return t

def strip_term(t: hydra.gen.core.Term) -> hydra.gen.core.Term:
    """Strip all annotations from a term."""
    match t:
        case hydra.gen.core.TermAnnotated(arg_):
            return strip_term(arg_.subject)
        
        case _:
            return t

def strip_type(t: hydra.gen.core.Type) -> hydra.gen.core.Type:
    """Strip all annotations from a term."""
    match t:
        case hydra.gen.core.TypeAnnotated(arg_):
            return strip_type(arg_.subject)
        
        case _:
            return t

def strip_type_parameters(t: hydra.gen.core.Type) -> hydra.gen.core.Type:
    """Strip any top-level type lambdas from a type, extracting the (possibly nested) type body."""
    match strip_type(t):
        case hydra.gen.core.TypeForall(lt):
            return strip_type_parameters(lt.body)
        
        case _:
            return t
