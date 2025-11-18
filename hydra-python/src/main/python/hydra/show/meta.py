# Note: this is an automatically generated file. Do not edit.

r"""String representations of hydra.meta types."""

from __future__ import annotations
import hydra.core
import hydra.variants

def term_variant(v1: hydra.variants.TermVariant) -> str:
    r"""Show a term variant as a string."""
    
    match v1:
        case hydra.variants.TermVariant.ANNOTATED:
            return "annotated"
        
        case hydra.variants.TermVariant.APPLICATION:
            return "application"
        
        case hydra.variants.TermVariant.EITHER:
            return "either"
        
        case hydra.variants.TermVariant.FUNCTION:
            return "function"
        
        case hydra.variants.TermVariant.LET:
            return "let"
        
        case hydra.variants.TermVariant.LIST:
            return "list"
        
        case hydra.variants.TermVariant.LITERAL:
            return "literal"
        
        case hydra.variants.TermVariant.MAP:
            return "map"
        
        case hydra.variants.TermVariant.MAYBE:
            return "maybe"
        
        case hydra.variants.TermVariant.PAIR:
            return "pair"
        
        case hydra.variants.TermVariant.PRODUCT:
            return "product"
        
        case hydra.variants.TermVariant.RECORD:
            return "record"
        
        case hydra.variants.TermVariant.SET:
            return "set"
        
        case hydra.variants.TermVariant.SUM:
            return "sum"
        
        case hydra.variants.TermVariant.TYPE_LAMBDA:
            return "typeLambda"
        
        case hydra.variants.TermVariant.TYPE_APPLICATION:
            return "typeApplication"
        
        case hydra.variants.TermVariant.UNION:
            return "union"
        
        case hydra.variants.TermVariant.UNIT:
            return "unit"
        
        case hydra.variants.TermVariant.VARIABLE:
            return "variable"
        
        case hydra.variants.TermVariant.WRAP:
            return "wrap"
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def type_variant(v1: hydra.variants.TypeVariant) -> str:
    r"""Show a type variant as a string."""
    
    match v1:
        case hydra.variants.TypeVariant.ANNOTATED:
            return "annotated"
        
        case hydra.variants.TypeVariant.APPLICATION:
            return "application"
        
        case hydra.variants.TypeVariant.EITHER:
            return "either"
        
        case hydra.variants.TypeVariant.FORALL:
            return "forall"
        
        case hydra.variants.TypeVariant.FUNCTION:
            return "function"
        
        case hydra.variants.TypeVariant.LIST:
            return "list"
        
        case hydra.variants.TypeVariant.LITERAL:
            return "literal"
        
        case hydra.variants.TypeVariant.MAP:
            return "map"
        
        case hydra.variants.TypeVariant.MAYBE:
            return "maybe"
        
        case hydra.variants.TypeVariant.PAIR:
            return "pair"
        
        case hydra.variants.TypeVariant.PRODUCT:
            return "product"
        
        case hydra.variants.TypeVariant.RECORD:
            return "record"
        
        case hydra.variants.TypeVariant.SET:
            return "set"
        
        case hydra.variants.TypeVariant.SUM:
            return "sum"
        
        case hydra.variants.TypeVariant.UNION:
            return "union"
        
        case hydra.variants.TypeVariant.UNIT:
            return "unit"
        
        case hydra.variants.TypeVariant.VARIABLE:
            return "variable"
        
        case hydra.variants.TypeVariant.WRAP:
            return "wrap"
        
        case _:
            raise AssertionError("Unreachable: all variants handled")
