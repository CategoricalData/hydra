# Note: this is an automatically generated file. Do not edit.

r"""String representations of hydra.meta types."""

from __future__ import annotations
import hydra.core
import hydra.meta

def term_variant(v1: hydra.meta.TermVariant) -> str:
    r"""Show a term variant as a string."""
    
    match v1:
        case hydra.meta.TermVariant.ANNOTATED:
            return "annotated"
        
        case hydra.meta.TermVariant.APPLICATION:
            return "application"
        
        case hydra.meta.TermVariant.FUNCTION:
            return "function"
        
        case hydra.meta.TermVariant.LET:
            return "let"
        
        case hydra.meta.TermVariant.LIST:
            return "list"
        
        case hydra.meta.TermVariant.LITERAL:
            return "literal"
        
        case hydra.meta.TermVariant.MAP:
            return "map"
        
        case hydra.meta.TermVariant.MAYBE:
            return "maybe"
        
        case hydra.meta.TermVariant.PRODUCT:
            return "product"
        
        case hydra.meta.TermVariant.RECORD:
            return "record"
        
        case hydra.meta.TermVariant.SET:
            return "set"
        
        case hydra.meta.TermVariant.SUM:
            return "sum"
        
        case hydra.meta.TermVariant.TYPE_LAMBDA:
            return "typeLambda"
        
        case hydra.meta.TermVariant.TYPE_APPLICATION:
            return "typeApplication"
        
        case hydra.meta.TermVariant.UNION:
            return "union"
        
        case hydra.meta.TermVariant.UNIT:
            return "unit"
        
        case hydra.meta.TermVariant.VARIABLE:
            return "variable"
        
        case hydra.meta.TermVariant.WRAP:
            return "wrap"
        
        case _:
            raise TypeError("Unsupported TermVariant")

def type_variant(v1: hydra.meta.TypeVariant) -> str:
    r"""Show a type variant as a string."""
    
    match v1:
        case hydra.meta.TypeVariant.ANNOTATED:
            return "annotated"
        
        case hydra.meta.TypeVariant.APPLICATION:
            return "application"
        
        case hydra.meta.TypeVariant.FORALL:
            return "forall"
        
        case hydra.meta.TypeVariant.FUNCTION:
            return "function"
        
        case hydra.meta.TypeVariant.LIST:
            return "list"
        
        case hydra.meta.TypeVariant.LITERAL:
            return "literal"
        
        case hydra.meta.TypeVariant.MAP:
            return "map"
        
        case hydra.meta.TypeVariant.MAYBE:
            return "maybe"
        
        case hydra.meta.TypeVariant.PRODUCT:
            return "product"
        
        case hydra.meta.TypeVariant.RECORD:
            return "record"
        
        case hydra.meta.TypeVariant.SET:
            return "set"
        
        case hydra.meta.TypeVariant.SUM:
            return "sum"
        
        case hydra.meta.TypeVariant.UNION:
            return "union"
        
        case hydra.meta.TypeVariant.UNIT:
            return "unit"
        
        case hydra.meta.TypeVariant.VARIABLE:
            return "variable"
        
        case hydra.meta.TypeVariant.WRAP:
            return "wrap"
        
        case _:
            raise TypeError("Unsupported TypeVariant")
