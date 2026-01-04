# Note: this is an automatically generated file. Do not edit.

r"""Evaluation-level implementations of Pair functions for the Hydra interpreter."""

from __future__ import annotations
from typing import TypeVar, cast
import hydra.compute
import hydra.core
import hydra.lib.flows
import hydra.lib.pairs
import hydra.monads
import hydra.show.core

T0 = TypeVar("T0")

def bimap(first_fun: hydra.core.Term, second_fun: hydra.core.Term, pair_term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    match pair_term:
        case hydra.core.TermPair(value=p):
            def fst() -> hydra.core.Term:
                return hydra.lib.pairs.first(p)
            def snd() -> hydra.core.Term:
                return hydra.lib.pairs.second(p)
            return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermPair(cast(tuple[hydra.core.Term, hydra.core.Term], (cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(first_fun, fst()))), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(second_fun, snd()))))))))
        
        case _:
            return hydra.monads.unexpected("pair value", hydra.show.core.term(pair_term))
