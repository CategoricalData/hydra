# Note: this is an automatically generated file. Do not edit.

r"""A model for Hydra packages."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core
import hydra.module

@dataclass(frozen=True)
class Package:
    r"""A package, which is a named collection of modules with metadata and dependencies."""

    name: Annotated[PackageName, "The name of the package"]
    modules: Annotated[frozenlist[hydra.module.Module], "The modules in this package"]
    dependencies: Annotated[frozenlist[PackageName], "The packages which this package depends on"]
    description: Annotated[Maybe[str], "An optional human-readable description of the package"]

    TYPE_ = hydra.core.Name("hydra.packaging.Package")
    NAME = hydra.core.Name("name")
    MODULES = hydra.core.Name("modules")
    DEPENDENCIES = hydra.core.Name("dependencies")
    DESCRIPTION = hydra.core.Name("description")

class PackageName(Node[str]):
    r"""The unique name of a package, e.g. "hydra-kernel" or "hydra-python"."""

PackageName.TYPE_ = hydra.core.Name("hydra.packaging.PackageName")
