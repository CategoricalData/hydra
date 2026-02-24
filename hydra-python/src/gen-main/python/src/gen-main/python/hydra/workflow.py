# Note: this is an automatically generated file. Do not edit.

r"""A model for Hydra transformation workflows."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from hydra.dsl.python import Node, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar
import hydra.compute
import hydra.core
import hydra.graph
import hydra.module

A = TypeVar("A")
S = TypeVar("S")

@dataclass(frozen=True)
class HydraSchemaSpec:
    r"""The specification of a Hydra schema, provided as a set of modules and a distinguished type."""
    
    modules: Annotated[frozenlist[hydra.module.Module], "The modules to include in the schema graph"]
    type_name: Annotated[hydra.core.Name, "The name of the top-level type; all data which passes through the workflow will be instances of this type"]
    
    TYPE_ = hydra.core.Name("hydra.workflow.HydraSchemaSpec")
    MODULES = hydra.core.Name("modules")
    TYPE_NAME = hydra.core.Name("typeName")

@dataclass(frozen=True)
class LastMile(Generic[S, A]):
    r"""The last mile of a transformation, which encodes and serializes terms to a file."""
    
    encoder: Annotated[Callable[[hydra.core.Type], hydra.compute.Flow[S, Callable[[hydra.core.Term, hydra.graph.Graph], hydra.compute.Flow[S, frozenlist[A]]]]], "An encoder for terms to a list of output objects"]
    serializer: Annotated[Callable[[frozenlist[A]], hydra.compute.Flow[S, str]], "A function which serializes a list of output objects to a string representation"]
    file_extension: Annotated[str, "A file extension for the generated file(s)"]
    
    TYPE_ = hydra.core.Name("hydra.workflow.LastMile")
    ENCODER = hydra.core.Name("encoder")
    SERIALIZER = hydra.core.Name("serializer")
    FILE_EXTENSION = hydra.core.Name("fileExtension")

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
