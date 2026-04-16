# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.yaml.model."""

from __future__ import annotations
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import FrozenDict, frozenlist
from typing import cast
import hydra.core
import hydra.phantoms

def node_mapping(x: hydra.phantoms.TTerm[FrozenDict[hydra.yaml.model.Node_, hydra.yaml.model.Node_]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.yaml.model.Node"), hydra.core.Field(hydra.core.Name("mapping"), x.value)))))

def node_scalar(x: hydra.phantoms.TTerm[hydra.yaml.model.Scalar]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.yaml.model.Node"), hydra.core.Field(hydra.core.Name("scalar"), x.value)))))

def node_sequence(x: hydra.phantoms.TTerm[frozenlist[hydra.yaml.model.Node_]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.yaml.model.Node"), hydra.core.Field(hydra.core.Name("sequence"), x.value)))))

def scalar_bool(x: hydra.phantoms.TTerm[bool]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.yaml.model.Scalar"), hydra.core.Field(hydra.core.Name("bool"), x.value)))))

def scalar_decimal(x: hydra.phantoms.TTerm[Decimal]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.yaml.model.Scalar"), hydra.core.Field(hydra.core.Name("decimal"), x.value)))))

def scalar_float(x: hydra.phantoms.TTerm[Decimal]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.yaml.model.Scalar"), hydra.core.Field(hydra.core.Name("float"), x.value)))))

def scalar_int(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.yaml.model.Scalar"), hydra.core.Field(hydra.core.Name("int"), x.value)))))

scalar_null = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.yaml.model.Scalar"), hydra.core.Field(hydra.core.Name("null"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def scalar_str(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.yaml.model.Scalar"), hydra.core.Field(hydra.core.Name("str"), x.value)))))
