"""Meta-DSL for constructing graph-related terms (Graph, Primitive, Comparison).

Mirrors the Haskell module Hydra.Dsl.Meta.Graph, providing phantom-typed constructors,
accessors, and modifiers for graph-related Hydra types.

Most functions are re-exported from the generated module hydra.dsl.graph. This module
adds custom helpers for Comparison union constructors.
"""

import hydra.dsl.meta.phantoms as Phantoms
import hydra.dsl.meta.lib.lists as Lists
import hydra.dsl.meta.lib.maps as Maps
import hydra.dsl.meta.lib.sets as Sets
from hydra.core import Name
from hydra.phantoms import TTerm

# Re-export everything from the generated DSL module.
from hydra.dsl.graph import *  # noqa: F401, F403
import hydra.dsl.graph as _Gen


# ============================================================
# Comparison (custom -- not in hydra.graph type module)
# ============================================================

def comparison_less_than() -> TTerm:
    """The lessThan Comparison variant."""
    _COMPARISON_NAME = Name("hydra.util.Comparison")
    _COMPARISON_LESS_THAN = Name("lessThan")
    return Phantoms.inject_unit(_COMPARISON_NAME, _COMPARISON_LESS_THAN)


def comparison_equal_to() -> TTerm:
    """The equalTo Comparison variant."""
    _COMPARISON_NAME = Name("hydra.util.Comparison")
    _COMPARISON_EQUAL_TO = Name("equalTo")
    return Phantoms.inject_unit(_COMPARISON_NAME, _COMPARISON_EQUAL_TO)


def comparison_greater_than() -> TTerm:
    """The greaterThan Comparison variant."""
    _COMPARISON_NAME = Name("hydra.util.Comparison")
    _COMPARISON_GREATER_THAN = Name("greaterThan")
    return Phantoms.inject_unit(_COMPARISON_NAME, _COMPARISON_GREATER_THAN)


# ============================================================
# Graph helpers (custom)
# ============================================================

def empty_graph() -> TTerm:
    """An empty graph: no terms, types, primitives, or schema."""
    return _Gen.graph(
        Maps.empty(),  # boundTerms
        Maps.empty(),  # boundTypes
        Maps.empty(),  # classConstraints
        Sets.empty(),  # lambdaVariables
        Maps.empty(),  # metadata
        Maps.empty(),  # primitives
        Maps.empty(),  # schemaTypes
        Sets.empty(),  # typeVariables
    )


def graph_primitive_types(g: TTerm) -> TTerm:
    """Extract a Map from primitive name to TypeScheme from a graph."""
    return Maps.from_list(Lists.map(
        Phantoms.lam(
            "_gpt_p",
            Phantoms.pair(
                _Gen.primitive_name(Phantoms.var("_gpt_p")),
                _Gen.primitive_type_scheme(Phantoms.var("_gpt_p")),
            ),
        ),
        Maps.elems(_Gen.graph_primitives(g)),
    ))
