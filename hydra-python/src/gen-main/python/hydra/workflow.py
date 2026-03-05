# Note: this is an automatically generated file. Do not edit.

r"""A model for Hydra transformation workflows."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core
import hydra.module

@dataclass(frozen=True)
class HydraSchemaSpec:
    r"""The specification of a Hydra schema, provided as a set of modules and a distinguished type."""
    
    modules: Annotated[frozenlist[hydra.module.Module], "The modules to include in the schema graph"]
    type_name: Annotated[hydra.core.Name, "The name of the top-level type; all data which passes through the workflow will be instances of this type"]
    
    TYPE_ = hydra.core.Name("hydra.workflow.HydraSchemaSpec")
    MODULES = hydra.core.Name("modules")
    TYPE_NAME = hydra.core.Name("typeName")

class SchemaSpecHydra(Node["HydraSchemaSpec"]):
    r"""A native Hydra schema"""

class SchemaSpecFile(Node[str]):
    r"""A schema provided as a file, available at the given file path"""

class SchemaSpecProvided:
    r"""A schema which will be provided within the workflow"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SchemaSpecProvided)
    def __hash__(self):
        return hash("SchemaSpecProvided")

class _SchemaSpecMeta(type):
    def __getitem__(cls, item):
        return object

# The specification of a schema at the source end of a workflow.
class SchemaSpec(metaclass=_SchemaSpecMeta):
    r"""SchemaSpecHydra | SchemaSpecFile | SchemaSpecProvided"""
    
    TYPE_ = hydra.core.Name("hydra.workflow.SchemaSpec")
    HYDRA = hydra.core.Name("hydra")
    FILE = hydra.core.Name("file")
    PROVIDED = hydra.core.Name("provided")

@dataclass(frozen=True)
class TransformWorkflow:
    r"""The specification of a workflow which takes a schema specification, reads data from a directory, and writes data to another directory."""
    
    name: Annotated[str, "A descriptive name for the workflow"]
    schema_spec: Annotated[SchemaSpec, "The schema specification"]
    src_dir: Annotated[str, "The source directory"]
    dest_dir: Annotated[str, "The destination directory"]
    
    TYPE_ = hydra.core.Name("hydra.workflow.TransformWorkflow")
    NAME = hydra.core.Name("name")
    SCHEMA_SPEC = hydra.core.Name("schemaSpec")
    SRC_DIR = hydra.core.Name("srcDir")
    DEST_DIR = hydra.core.Name("destDir")
