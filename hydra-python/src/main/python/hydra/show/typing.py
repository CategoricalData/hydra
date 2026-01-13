# Note: this is an automatically generated file. Do not edit.

r"""String representations of hydra.typing types."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import FrozenDict, frozenlist
import hydra.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.pairs
import hydra.lib.strings
import hydra.show.core
import hydra.typing

def type_constraint(tc: hydra.typing.TypeConstraint) -> str:
    r"""Show a type constraint as a string."""
    
    @lru_cache(1)
    def ltyp() -> hydra.core.Type:
        return tc.left
    @lru_cache(1)
    def rtyp() -> hydra.core.Type:
        return tc.right
    return hydra.lib.strings.cat((hydra.show.core.type(ltyp()), "≡", hydra.show.core.type(rtyp())))

def type_subst(ts: hydra.typing.TypeSubst) -> str:
    r"""Show a type substitution as a string."""
    
    @lru_cache(1)
    def subst() -> FrozenDict[hydra.core.Name, hydra.core.Type]:
        return ts.value
    @lru_cache(1)
    def pairs() -> frozenlist[tuple[hydra.core.Name, hydra.core.Type]]:
        return hydra.lib.maps.to_list(subst())
    def show_pair(pair: tuple[hydra.core.Name, hydra.core.Type]) -> str:
        @lru_cache(1)
        def name() -> str:
            return hydra.lib.pairs.first(pair).value
        @lru_cache(1)
        def typ() -> hydra.core.Type:
            return hydra.lib.pairs.second(pair)
        return hydra.lib.strings.cat((name(), "↦", hydra.show.core.type(typ())))
    @lru_cache(1)
    def pair_strs() -> frozenlist[str]:
        return hydra.lib.lists.map(show_pair, pairs())
    return hydra.lib.strings.cat(("{", hydra.lib.strings.intercalate(",", pair_strs()), "}"))
