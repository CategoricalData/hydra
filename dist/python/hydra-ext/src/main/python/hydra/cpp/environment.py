# Note: this is an automatically generated file. Do not edit.

r"""Type definitions for C++ code generation environment."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import FrozenDict, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core
import hydra.packaging

@dataclass(frozen=True)
class CppEnvironment:
    r"""Environment for C++ code generation."""

    namespaces: Annotated[hydra.packaging.Namespaces[str], "Namespace mapping for code generation"]
    bound_type_variables: Annotated[tuple[frozenlist[hydra.core.Name], FrozenDict[hydra.core.Name, str]], "Type variables in scope, with their C++ names"]

    TYPE_ = hydra.core.Name("hydra.cpp.environment.CppEnvironment")
    NAMESPACES = hydra.core.Name("namespaces")
    BOUND_TYPE_VARIABLES = hydra.core.Name("boundTypeVariables")
