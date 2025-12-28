# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.json."""

from __future__ import annotations
from decimal import Decimal
from hydra.dsl.python import FrozenDict, frozenlist
from typing import cast
import hydra.core
import hydra.json
import hydra.lib.lists
import hydra.lib.maps

def value(v1: hydra.json.Value) -> hydra.core.Type:
    match v1:
        case hydra.json.ValueArray(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.Value"), hydra.core.Field(hydra.core.Name("array"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(value, xs))))(v)))))
        
        case hydra.json.ValueBoolean(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.Value"), hydra.core.Field(hydra.core.Name("boolean"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(x)))))(v2)))))
        
        case hydra.json.ValueNull(value=v3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.Value"), hydra.core.Field(hydra.core.Name("null"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(v3)))))
        
        case hydra.json.ValueNumber(value=v4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.Value"), hydra.core.Field(hydra.core.Name("number"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueBigfloat(x)))))))(v4)))))
        
        case hydra.json.ValueObject(value=v5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.Value"), hydra.core.Field(hydra.core.Name("object"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x))))), value, m))))(v5)))))
        
        case hydra.json.ValueString(value=v6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.Value"), hydra.core.Field(hydra.core.Name("string"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x)))))(v6)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")
