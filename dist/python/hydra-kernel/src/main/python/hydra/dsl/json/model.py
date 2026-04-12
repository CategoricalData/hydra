# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.json.model."""

from __future__ import annotations
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import FrozenDict, frozenlist
from typing import cast
import hydra.core
import hydra.phantoms

def value_array(x: hydra.phantoms.TTerm[frozenlist[hydra.json.model.Value]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.model.Value"), hydra.core.Field(hydra.core.Name("array"), x.value)))))

def value_boolean(x: hydra.phantoms.TTerm[bool]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.model.Value"), hydra.core.Field(hydra.core.Name("boolean"), x.value)))))

value_null = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.model.Value"), hydra.core.Field(hydra.core.Name("null"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def value_number(x: hydra.phantoms.TTerm[Decimal]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.model.Value"), hydra.core.Field(hydra.core.Name("number"), x.value)))))

def value_object(x: hydra.phantoms.TTerm[FrozenDict[str, hydra.json.model.Value]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.model.Value"), hydra.core.Field(hydra.core.Name("object"), x.value)))))

def value_string(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.json.model.Value"), hydra.core.Field(hydra.core.Name("string"), x.value)))))
