# Note: this is an automatically generated file. Do not edit.

r"""String representations of hydra.mantle types."""

from __future__ import annotations
import hydra.core
import hydra.mantle

def term_variant(v1: hydra.mantle.TermVariant) -> str:
    r"""Show a term variant as a string."""
    
    match v1:
        case hydra.mantle.TermVariant.ANNOTATED:
            return "annotated"
        
        case hydra.mantle.TermVariant.APPLICATION:
            return "application"
        
        case hydra.mantle.TermVariant.FUNCTION:
            return "function"
        
        case hydra.mantle.TermVariant.LET:
            return "let"
        
        case hydra.mantle.TermVariant.LIST:
            return "list"
        
        case hydra.mantle.TermVariant.LITERAL:
            return "literal"
        
        case hydra.mantle.TermVariant.MAP:
            return "map"
        
        case hydra.mantle.TermVariant.OPTIONAL:
            return "optional"
        
        case hydra.mantle.TermVariant.PRODUCT:
            return "product"
        
        case hydra.mantle.TermVariant.RECORD:
            return "record"
        
        case hydra.mantle.TermVariant.SET:
            return "set"
        
        case hydra.mantle.TermVariant.SUM:
            return "sum"
        
        case hydra.mantle.TermVariant.TYPE_LAMBDA:
            return "typeLambda"
        
        case hydra.mantle.TermVariant.TYPE_APPLICATION:
            return "typeApplication"
        
        case hydra.mantle.TermVariant.UNION:
            return "union"
        
        case hydra.mantle.TermVariant.UNIT:
            return "unit"
        
        case hydra.mantle.TermVariant.VARIABLE:
            return "variable"
        
        case hydra.mantle.TermVariant.WRAP:
            return "wrap"

def type_variant(v1: hydra.mantle.TypeVariant) -> str:
    r"""Show a type variant as a string."""
    
    match v1:
        case hydra.mantle.TypeVariant.ANNOTATED:
            return "annotated"
        
        case hydra.mantle.TypeVariant.APPLICATION:
            return "application"
        
        case hydra.mantle.TypeVariant.FORALL:
            return "forall"
        
        case hydra.mantle.TypeVariant.FUNCTION:
            return "function"
        
        case hydra.mantle.TypeVariant.LIST:
            return "list"
        
        case hydra.mantle.TypeVariant.LITERAL:
            return "literal"
        
        case hydra.mantle.TypeVariant.MAP:
            return "map"
        
        case hydra.mantle.TypeVariant.OPTIONAL:
            return "optional"
        
        case hydra.mantle.TypeVariant.PRODUCT:
            return "product"
        
        case hydra.mantle.TypeVariant.RECORD:
            return "record"
        
        case hydra.mantle.TypeVariant.SET:
            return "set"
        
        case hydra.mantle.TypeVariant.SUM:
            return "sum"
        
        case hydra.mantle.TypeVariant.UNION:
            return "union"
        
        case hydra.mantle.TypeVariant.UNIT:
            return "unit"
        
        case hydra.mantle.TypeVariant.VARIABLE:
            return "variable"
        
        case hydra.mantle.TypeVariant.WRAP:
            return "wrap"
