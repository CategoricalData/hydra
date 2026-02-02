# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.classes."""

from __future__ import annotations
from typing import cast
import hydra.classes
import hydra.core

def type_class(v1: hydra.classes.TypeClass) -> hydra.core.Type:
    match v1:
        case hydra.classes.TypeClass.EQUALITY:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.classes.TypeClass"), hydra.core.Field(hydra.core.Name("equality"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.classes.TypeClass.ORDERING:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.classes.TypeClass"), hydra.core.Field(hydra.core.Name("ordering"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")
