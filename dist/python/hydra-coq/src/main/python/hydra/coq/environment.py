# Note: this is an automatically generated file. Do not edit.

r"""Environment types for Coq code generation."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import FrozenDict
from typing import Annotated, TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class CoqEnvironment:
    r"""Cross-module state threaded through the Coq encoder."""

    current_namespace: Annotated[str, "The Hydra namespace of the module currently being encoded (e.g. \"hydra.core\"). Used by the name resolver to decide whether a cross-namespace reference needs to stay qualified."]
    constructor_counts: Annotated[FrozenDict[str, int], "Number of constructors in each union type, keyed by local type name (e.g. \"Term\" -> 14). Used to decide whether a match is exhaustive."]
    ambiguous_names: Annotated[frozenset[str], "Local names (without namespace prefix) that are defined in more than one module. References to these must be kept fully qualified."]
    sanitized_accessors: Annotated[frozenset[str], "Accessor names for record fields that were sanitized to unit due to Coq's strict positivity requirement. Applications of these accessors are replaced with hydra_unreachable at emission time."]

    TYPE_ = hydra.core.Name("hydra.coq.environment.CoqEnvironment")
    CURRENT_NAMESPACE = hydra.core.Name("currentNamespace")
    CONSTRUCTOR_COUNTS = hydra.core.Name("constructorCounts")
    AMBIGUOUS_NAMES = hydra.core.Name("ambiguousNames")
    SANITIZED_ACCESSORS = hydra.core.Name("sanitizedAccessors")
