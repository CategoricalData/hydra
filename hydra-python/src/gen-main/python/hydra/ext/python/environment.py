# Note: this is an automatically generated file. Do not edit.

r"""Environment types for Python code generation."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import FrozenDict, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core
import hydra.ext.python.syntax
import hydra.graph
import hydra.module

class PythonVersion(Enum):
    r"""Target Python version for code generation."""

    PYTHON310 = hydra.core.Name("python310")

    PYTHON312 = hydra.core.Name("python312")

PythonVersion.TYPE_ = hydra.core.Name("hydra.ext.python.environment.PythonVersion")

@dataclass(frozen=True)
class PythonEnvironment:
    r"""Environment for Python code generation."""

    namespaces: Annotated[hydra.module.Namespaces[hydra.ext.python.syntax.DottedName], "Namespace mapping for imports"]
    bound_type_variables: Annotated[tuple[frozenlist[hydra.core.Name], FrozenDict[hydra.core.Name, hydra.ext.python.syntax.Name]], "Type variables in scope, with their Python names"]
    graph: Annotated[hydra.graph.Graph, "Graph context for type inference"]
    nullary_bindings: Annotated[frozenset[hydra.core.Name], "Set of nullary bindings (need call syntax)"]
    version: Annotated[PythonVersion, "Target Python version"]
    skip_casts: Annotated[bool, "When True, skip generating cast() calls for reduced memory usage"]
    inline_variables: Annotated[frozenset[hydra.core.Name], "Variables that are inline let bindings (walrus operators)"]

    TYPE_ = hydra.core.Name("hydra.ext.python.environment.PythonEnvironment")
    NAMESPACES = hydra.core.Name("namespaces")
    BOUND_TYPE_VARIABLES = hydra.core.Name("boundTypeVariables")
    GRAPH = hydra.core.Name("graph")
    NULLARY_BINDINGS = hydra.core.Name("nullaryBindings")
    VERSION = hydra.core.Name("version")
    SKIP_CASTS = hydra.core.Name("skipCasts")
    INLINE_VARIABLES = hydra.core.Name("inlineVariables")

@dataclass(frozen=True)
class PythonModuleMetadata:
    r"""Temporary metadata used to create the header section of a Python file."""

    namespaces: Annotated[hydra.module.Namespaces[hydra.ext.python.syntax.DottedName], "Namespace mapping for imports"]
    type_variables: Annotated[frozenset[hydra.core.Name], "Type variables used in the module"]
    uses_annotated: bool
    uses_callable: bool
    uses_cast: bool
    uses_lru_cache: bool
    uses_type_alias: bool
    uses_dataclass: bool
    uses_decimal: bool
    uses_either: bool
    uses_enum: bool
    uses_frozen_dict: bool
    uses_frozen_list: bool
    uses_generic: bool
    uses_just: bool
    uses_left: bool
    uses_maybe: bool
    uses_name: bool
    uses_node: bool
    uses_nothing: bool
    uses_right: bool
    uses_type_var: bool

    TYPE_ = hydra.core.Name("hydra.ext.python.environment.PythonModuleMetadata")
    NAMESPACES = hydra.core.Name("namespaces")
    TYPE_VARIABLES = hydra.core.Name("typeVariables")
    USES_ANNOTATED = hydra.core.Name("usesAnnotated")
    USES_CALLABLE = hydra.core.Name("usesCallable")
    USES_CAST = hydra.core.Name("usesCast")
    USES_LRU_CACHE = hydra.core.Name("usesLruCache")
    USES_TYPE_ALIAS = hydra.core.Name("usesTypeAlias")
    USES_DATACLASS = hydra.core.Name("usesDataclass")
    USES_DECIMAL = hydra.core.Name("usesDecimal")
    USES_EITHER = hydra.core.Name("usesEither")
    USES_ENUM = hydra.core.Name("usesEnum")
    USES_FROZEN_DICT = hydra.core.Name("usesFrozenDict")
    USES_FROZEN_LIST = hydra.core.Name("usesFrozenList")
    USES_GENERIC = hydra.core.Name("usesGeneric")
    USES_JUST = hydra.core.Name("usesJust")
    USES_LEFT = hydra.core.Name("usesLeft")
    USES_MAYBE = hydra.core.Name("usesMaybe")
    USES_NAME = hydra.core.Name("usesName")
    USES_NODE = hydra.core.Name("usesNode")
    USES_NOTHING = hydra.core.Name("usesNothing")
    USES_RIGHT = hydra.core.Name("usesRight")
    USES_TYPE_VAR = hydra.core.Name("usesTypeVar")

@dataclass(frozen=True)
class PyGraph:
    r"""Combined graph and metadata state for Python code generation."""

    graph: Annotated[hydra.graph.Graph, "The Hydra graph being processed"]
    metadata: Annotated[PythonModuleMetadata, "Accumulated module metadata"]

    TYPE_ = hydra.core.Name("hydra.ext.python.environment.PyGraph")
    GRAPH = hydra.core.Name("graph")
    METADATA = hydra.core.Name("metadata")
