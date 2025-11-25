# Note: this is an automatically generated file. Do not edit.

r"""String representations of hydra.util types."""

from __future__ import annotations
import hydra.core
import hydra.util

def case_convention(c: hydra.util.CaseConvention) -> str:
    r"""Show a case convention as a string."""
    
    match c:
        case hydra.util.CaseConvention.LOWER_SNAKE:
            return "lower_snake_case"
        
        case hydra.util.CaseConvention.UPPER_SNAKE:
            return "UPPER_SNAKE_CASE"
        
        case hydra.util.CaseConvention.CAMEL:
            return "camelCase"
        
        case hydra.util.CaseConvention.PASCAL:
            return "PascalCase"
        
        case _:
            raise AssertionError("Unreachable: all variants handled")
