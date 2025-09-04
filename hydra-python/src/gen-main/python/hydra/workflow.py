# Note: this is an automatically generated file. Do not edit.

"""A model for Hydra transformation workflows."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from hydra.dsl.python import frozenlist, Node
from typing import Annotated, Generic, TypeVar
import hydra.compute
import hydra.core
import hydra.graph
import hydra.module

A = TypeVar("A")
S = TypeVar("S")

@dataclass
class HydraSchemaSpec:
    """The specification of a Hydra schema, provided as a set of modules and a distinguished type."""
    
    modules: Annotated[frozenlist[hydra.module.Module], "The modules to include in the schema graph"]
    type_name: Annotated[hydra.core.Name, "The name of the top-level type; all data which passes through the workflow will be instances of this type"]

HYDRA_SCHEMA_SPEC__NAME = hydra.core.Name("hydra.workflow.HydraSchemaSpec")
HYDRA_SCHEMA_SPEC__MODULES__NAME = hydra.core.Name("modules")
HYDRA_SCHEMA_SPEC__TYPE_NAME__NAME = hydra.core.Name("typeName")

@dataclass
class LastMile(Generic[S, A]):
    """The last mile of a transformation, which encodes and serializes terms to a file."""
    
    encoder: Annotated[Callable[[hydra.core.Type], hydra.compute.Flow[S, Callable[[hydra.core.Term, hydra.graph.Graph], hydra.compute.Flow[S, frozenlist[A]]]]], "An encoder for terms to a list of output objects"]
    serializer: Annotated[Callable[[frozenlist[A]], hydra.compute.Flow[S, str]], "A function which serializes a list of output objects to a string representation"]
    file_extension: Annotated[str, "A file extension for the generated file(s)"]

LAST_MILE__NAME = hydra.core.Name("hydra.workflow.LastMile")
LAST_MILE__ENCODER__NAME = hydra.core.Name("encoder")
LAST_MILE__SERIALIZER__NAME = hydra.core.Name("serializer")
LAST_MILE__FILE_EXTENSION__NAME = hydra.core.Name("fileExtension")

class SchemaSpecHydra(Node["HydraSchemaSpec"]):
    """A native Hydra schema."""

class SchemaSpecFile(Node[str]):
    """A schema provided as a file, available at the given file path."""

class SchemaSpecProvided(Node[None]):
    """A schema which will be provided within the workflow."""

# The specification of a schema at the source end of a workflow.
type SchemaSpec = SchemaSpecHydra | SchemaSpecFile | SchemaSpecProvided

SCHEMA_SPEC__NAME = hydra.core.Name("hydra.workflow.SchemaSpec")
SCHEMA_SPEC__HYDRA__NAME = hydra.core.Name("hydra")
SCHEMA_SPEC__FILE__NAME = hydra.core.Name("file")
SCHEMA_SPEC__PROVIDED__NAME = hydra.core.Name("provided")

@dataclass
class TransformWorkflow:
    """The specification of a workflow which takes a schema specification, reads data from a directory, and writes data to another directory."""
    
    name: Annotated[str, "A descriptive name for the workflow"]
    schema_spec: Annotated[SchemaSpec, "The schema specification"]
    src_dir: Annotated[str, "The source directory"]
    dest_dir: Annotated[str, "The destination directory"]

TRANSFORM_WORKFLOW__NAME = hydra.core.Name("hydra.workflow.TransformWorkflow")
TRANSFORM_WORKFLOW__NAME__NAME = hydra.core.Name("name")
TRANSFORM_WORKFLOW__SCHEMA_SPEC__NAME = hydra.core.Name("schemaSpec")
TRANSFORM_WORKFLOW__SRC_DIR__NAME = hydra.core.Name("srcDir")
TRANSFORM_WORKFLOW__DEST_DIR__NAME = hydra.core.Name("destDir")
