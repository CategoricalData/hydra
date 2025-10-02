# Note: this is an automatically generated file. Do not edit.

"""Natural-language descriptions for hydra.mantle types."""

from __future__ import annotations
import hydra.core
import hydra.lib.literals
import hydra.lib.strings
import hydra.mantle

def precision(v1: hydra.mantle.Precision) -> str:
    """Display numeric precision as a string."""
    
    match v1:
        case hydra.mantle.PrecisionArbitrary():
            return "arbitrary-precision"
        
        case hydra.mantle.PrecisionBits(value=bits):
            return hydra.lib.strings.cat((hydra.lib.literals.show_int32(bits), "-bit"))
