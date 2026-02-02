# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.coders."""

from __future__ import annotations
from typing import cast
import hydra.coders
import hydra.core

def coder_direction(v1: hydra.coders.CoderDirection) -> hydra.core.Type:
    match v1:
        case hydra.coders.CoderDirection.ENCODE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.coders.CoderDirection"), hydra.core.Field(hydra.core.Name("encode"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.coders.CoderDirection.DECODE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.coders.CoderDirection"), hydra.core.Field(hydra.core.Name("decode"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def language_name(x: hydra.coders.LanguageName) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.coders.LanguageName"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def traversal_order(v1: hydra.coders.TraversalOrder) -> hydra.core.Type:
    match v1:
        case hydra.coders.TraversalOrder.PRE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.coders.TraversalOrder"), hydra.core.Field(hydra.core.Name("pre"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.coders.TraversalOrder.POST:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.coders.TraversalOrder"), hydra.core.Field(hydra.core.Name("post"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")
