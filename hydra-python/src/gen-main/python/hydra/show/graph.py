# Note: this is an automatically generated file. Do not edit.

r"""String representations of hydra.graph types."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.core
import hydra.lib.lists
import hydra.lib.strings
import hydra.show.core

def graph(elements: frozenlist[hydra.core.Binding]) -> str:
    r"""Show a list of bindings as a string."""

    @lru_cache(1)
    def element_strs() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda x1: hydra.show.core.binding(x1)), elements)
    return hydra.lib.strings.cat(("{", hydra.lib.strings.intercalate(", ", element_strs()), "}"))
