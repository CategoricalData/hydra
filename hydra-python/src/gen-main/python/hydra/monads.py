# Note: this is an automatically generated file. Do not edit.

r"""Miscellaneous helper functions."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import Maybe, frozenlist
from typing import TypeVar, cast
import hydra.context
import hydra.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes

T0 = TypeVar("T0")

@lru_cache(1)
def empty_context() -> hydra.context.Context:
    r"""An empty context with no trace, messages, or other data."""
    
    return hydra.context.Context((), (), hydra.lib.maps.empty())

def maybe_to_list(mx: Maybe[T0]) -> frozenlist[T0]:
    r"""Converts an optional value either to an empty list (if nothing) or a singleton list (if just)."""
    
    return hydra.lib.maybes.maybe((), (lambda x1: hydra.lib.lists.pure(x1)), mx)
