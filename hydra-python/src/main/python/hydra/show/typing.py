# Note: this is an automatically generated file. Do not edit.

r"""String representations of hydra.typing types."""

from __future__ import annotations
import hydra.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.pairs
import hydra.lib.strings
import hydra.show.core
import hydra.typing

def type_constraint(tc: hydra.typing.TypeConstraint) -> str:
    r"""Show a type constraint as a string."""
    
    ltyp = tc.left
    rtyp = tc.right
    return hydra.lib.strings.cat((hydra.show.core.type(ltyp), "≡", hydra.show.core.type(rtyp)))

def type_subst(ts: hydra.typing.TypeSubst) -> str:
    r"""Show a type substitution as a string."""
    
    subst = ts.value
    pairs = hydra.lib.maps.to_list(subst)
    def show_pair(pair: Tuple[hydra.core.Name, hydra.core.Type]) -> str:
        name = hydra.lib.pairs.first(pair).value
        typ = hydra.lib.pairs.second(pair)
        return hydra.lib.strings.cat((name, "↦", hydra.show.core.type(typ)))
    pair_strs = hydra.lib.lists.map(show_pair, pairs)
    return hydra.lib.strings.cat(("{", hydra.lib.strings.intercalate(",", pair_strs), "}"))
