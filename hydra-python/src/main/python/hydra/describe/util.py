# Note: this is an automatically generated file. Do not edit.

r"""Natural-language descriptions for hydra.util types."""

from __future__ import annotations
import hydra.core
import hydra.lib.literals
import hydra.lib.strings
import hydra.util

def precision(v1: hydra.util.Precision) -> str:
    r"""Display numeric precision as a string."""
    
    match v1:
        case hydra.util.PrecisionArbitrary():
            return "arbitrary-precision"
        
        case hydra.util.PrecisionBits(value=bits):
            return hydra.lib.strings.cat((hydra.lib.literals.show_int32(bits), "-bit"))
