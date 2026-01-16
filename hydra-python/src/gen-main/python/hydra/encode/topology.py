# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.topology."""

from __future__ import annotations
from hydra.dsl.python import FrozenDict, frozenlist
from typing import cast
import hydra.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.sets
import hydra.topology

def vertex(x: int) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x))))))

def graph(m: FrozenDict[int, frozenlist[int]]) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(vertex, (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(vertex, xs)))), m)))

def tarjan_state(x: hydra.topology.TarjanState) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.topology.TarjanState"), (hydra.core.Field(hydra.core.Name("counter"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x2)))))))(x.counter)), hydra.core.Field(hydra.core.Name("indices"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(vertex, (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x2))))))), m))))(x.indices)), hydra.core.Field(hydra.core.Name("lowLinks"), (lambda m: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(vertex, (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x2))))))), m))))(x.low_links)), hydra.core.Field(hydra.core.Name("stack"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(vertex, xs))))(x.stack)), hydra.core.Field(hydra.core.Name("onStack"), (lambda s: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map(vertex, s))))(x.on_stack)), hydra.core.Field(hydra.core.Name("sccs"), (lambda xs: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda xs2: cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map(vertex, xs2)))), xs))))(x.sccs))))))
