# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.json.model."""

from __future__ import annotations
from decimal import Decimal
from hydra.dsl.python import FrozenDict, frozenlist
from typing import cast
import hydra.core
import hydra.json.model
import hydra.lib.lists
import hydra.lib.maps

def value(v1: hydra.json.model.Value) -> hydra.core.Term:
    match v1:
        case hydra.json.model.ValueArray(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.model.Value"), hydra.core.Field(hydra.core.Name("array"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: value(x1)), y)))))))
        
        case hydra.json.model.ValueBoolean(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.model.Value"), hydra.core.Field(hydra.core.Name("boolean"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(y2))))))))
        
        case hydra.json.model.ValueNull():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.model.Value"), hydra.core.Field(hydra.core.Name("null"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.json.model.ValueNumber(value=y4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.model.Value"), hydra.core.Field(hydra.core.Name("number"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueBigfloat(y4))))))))))
        
        case hydra.json.model.ValueObject(value=y5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.model.Value"), hydra.core.Field(hydra.core.Name("object"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x))))), (lambda x1: value(x1)), y5)))))))
        
        case hydra.json.model.ValueString(value=y6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.model.Value"), hydra.core.Field(hydra.core.Name("string"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(y6))))))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")
