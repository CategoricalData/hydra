# Note: this is an automatically generated file. Do not edit.

r"""Helper types for Python code generation."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from hydra.dsl.python import FrozenDict, frozenlist
from typing import Annotated, TypeAlias
import hydra.core
import hydra.ext.python.syntax
import hydra.graph
import hydra.module
import hydra.typing

class PythonVersion(Enum):
    r"""Target Python version for code generation."""
    
    PYTHON310 = "python310"
    
    PYTHON312 = "python312"

PYTHON_VERSION__NAME = hydra.core.Name("hydra.ext.python.helpers.PythonVersion")
PYTHON_VERSION__PYTHON310__NAME = hydra.core.Name("python310")
PYTHON_VERSION__PYTHON312__NAME = hydra.core.Name("python312")

@dataclass(frozen=True)
class PythonEnvironment:
    r"""Environment for Python code generation."""
    
    namespaces: Annotated[hydra.module.Namespaces[hydra.ext.python.syntax.DottedName], "Namespace mapping for imports"]
    bound_type_variables: Annotated[tuple[frozenlist[hydra.core.Name], FrozenDict[hydra.core.Name, hydra.ext.python.syntax.Name]], "Type variables in scope, with their Python names"]
    type_context: Annotated[hydra.typing.TypeContext, "Type context for type inference"]
    nullary_bindings: Annotated[frozenset[hydra.core.Name], "Set of nullary bindings (need call syntax)"]
    version: Annotated[PythonVersion, "Target Python version"]
    skip_casts: Annotated[bool, "When True, skip generating cast() calls for reduced memory usage"]
    inline_variables: Annotated[frozenset[hydra.core.Name], "Variables that are inline let bindings (walrus operators)"]

PYTHON_ENVIRONMENT__NAME = hydra.core.Name("hydra.ext.python.helpers.PythonEnvironment")
PYTHON_ENVIRONMENT__NAMESPACES__NAME = hydra.core.Name("namespaces")
PYTHON_ENVIRONMENT__BOUND_TYPE_VARIABLES__NAME = hydra.core.Name("boundTypeVariables")
PYTHON_ENVIRONMENT__TYPE_CONTEXT__NAME = hydra.core.Name("typeContext")
PYTHON_ENVIRONMENT__NULLARY_BINDINGS__NAME = hydra.core.Name("nullaryBindings")
PYTHON_ENVIRONMENT__VERSION__NAME = hydra.core.Name("version")
PYTHON_ENVIRONMENT__SKIP_CASTS__NAME = hydra.core.Name("skipCasts")
PYTHON_ENVIRONMENT__INLINE_VARIABLES__NAME = hydra.core.Name("inlineVariables")

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

PYTHON_MODULE_METADATA__NAME = hydra.core.Name("hydra.ext.python.helpers.PythonModuleMetadata")
PYTHON_MODULE_METADATA__NAMESPACES__NAME = hydra.core.Name("namespaces")
PYTHON_MODULE_METADATA__TYPE_VARIABLES__NAME = hydra.core.Name("typeVariables")
PYTHON_MODULE_METADATA__USES_ANNOTATED__NAME = hydra.core.Name("usesAnnotated")
PYTHON_MODULE_METADATA__USES_CALLABLE__NAME = hydra.core.Name("usesCallable")
PYTHON_MODULE_METADATA__USES_CAST__NAME = hydra.core.Name("usesCast")
PYTHON_MODULE_METADATA__USES_LRU_CACHE__NAME = hydra.core.Name("usesLruCache")
PYTHON_MODULE_METADATA__USES_TYPE_ALIAS__NAME = hydra.core.Name("usesTypeAlias")
PYTHON_MODULE_METADATA__USES_DATACLASS__NAME = hydra.core.Name("usesDataclass")
PYTHON_MODULE_METADATA__USES_DECIMAL__NAME = hydra.core.Name("usesDecimal")
PYTHON_MODULE_METADATA__USES_EITHER__NAME = hydra.core.Name("usesEither")
PYTHON_MODULE_METADATA__USES_ENUM__NAME = hydra.core.Name("usesEnum")
PYTHON_MODULE_METADATA__USES_FROZEN_DICT__NAME = hydra.core.Name("usesFrozenDict")
PYTHON_MODULE_METADATA__USES_FROZEN_LIST__NAME = hydra.core.Name("usesFrozenList")
PYTHON_MODULE_METADATA__USES_GENERIC__NAME = hydra.core.Name("usesGeneric")
PYTHON_MODULE_METADATA__USES_JUST__NAME = hydra.core.Name("usesJust")
PYTHON_MODULE_METADATA__USES_LEFT__NAME = hydra.core.Name("usesLeft")
PYTHON_MODULE_METADATA__USES_MAYBE__NAME = hydra.core.Name("usesMaybe")
PYTHON_MODULE_METADATA__USES_NAME__NAME = hydra.core.Name("usesName")
PYTHON_MODULE_METADATA__USES_NODE__NAME = hydra.core.Name("usesNode")
PYTHON_MODULE_METADATA__USES_NOTHING__NAME = hydra.core.Name("usesNothing")
PYTHON_MODULE_METADATA__USES_RIGHT__NAME = hydra.core.Name("usesRight")
PYTHON_MODULE_METADATA__USES_TYPE_VAR__NAME = hydra.core.Name("usesTypeVar")

@dataclass(frozen=True)
class PyGraph:
    r"""Combined graph and metadata state for Python code generation."""
    
    graph: Annotated[hydra.graph.Graph, "The Hydra graph being processed"]
    metadata: Annotated[PythonModuleMetadata, "Accumulated module metadata"]

PY_GRAPH__NAME = hydra.core.Name("hydra.ext.python.helpers.PyGraph")
PY_GRAPH__GRAPH__NAME = hydra.core.Name("graph")
PY_GRAPH__METADATA__NAME = hydra.core.Name("metadata")
