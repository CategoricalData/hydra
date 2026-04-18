# Note: this is an automatically generated file. Do not edit.

r"""Type definitions for the Avro code generation environment."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Maybe
from typing import Annotated, TypeAlias, cast
import hydra.avro.schema
import hydra.coders
import hydra.core
import hydra.json.model

@dataclass(frozen=True)
class AvroQualifiedName:
    r"""An Avro qualified name with optional namespace."""

    namespace: Annotated[Maybe[str], "The optional namespace"]
    name: Annotated[str, "The local name"]

    TYPE_ = hydra.core.Name("hydra.avro.environment.AvroQualifiedName")
    NAMESPACE = hydra.core.Name("namespace")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class AvroForeignKey:
    r"""An Avro foreign key annotation linking a field to another type."""

    type_name: Annotated[hydra.core.Name, "The Hydra type name referenced by this foreign key"]
    constructor: Annotated[Callable[[str], hydra.core.Name], "A function which constructs element names from string values"]

    TYPE_ = hydra.core.Name("hydra.avro.environment.AvroForeignKey")
    TYPE_NAME = hydra.core.Name("typeName")
    CONSTRUCTOR = hydra.core.Name("constructor")

@dataclass(frozen=True)
class AvroPrimaryKey:
    r"""An Avro primary key annotation identifying the element name field."""

    field_name: Annotated[hydra.core.Name, "The name of the primary key field"]
    constructor: Annotated[Callable[[str], hydra.core.Name], "A function which constructs element names from string values"]

    TYPE_ = hydra.core.Name("hydra.avro.environment.AvroPrimaryKey")
    FIELD_NAME = hydra.core.Name("fieldName")
    CONSTRUCTOR = hydra.core.Name("constructor")

@dataclass(frozen=True)
class AvroEnvironment:
    r"""Environment for Avro-to-Hydra code generation."""

    named_adapters: Annotated[FrozenDict[AvroQualifiedName, hydra.coders.Adapter[hydra.avro.schema.Schema, hydra.core.Type, hydra.json.model.Value, hydra.core.Term]], "Named adapters for previously processed schemas"]
    namespace: Annotated[Maybe[str], "The current Avro namespace"]
    elements: Annotated[FrozenDict[hydra.core.Name, hydra.core.Binding], "Generated Hydra elements"]

    TYPE_ = hydra.core.Name("hydra.avro.environment.AvroEnvironment")
    NAMED_ADAPTERS = hydra.core.Name("namedAdapters")
    NAMESPACE = hydra.core.Name("namespace")
    ELEMENTS = hydra.core.Name("elements")

@dataclass(frozen=True)
class EncodeEnvironment:
    r"""Environment for Hydra-to-Avro encoding, tracking which named types have been emitted."""

    type_map: Annotated[FrozenDict[hydra.core.Name, hydra.core.Type], "All named types available for reference"]
    emitted: Annotated[FrozenDict[hydra.core.Name, hydra.coders.Adapter[hydra.core.Type, hydra.avro.schema.Schema, hydra.core.Term, hydra.json.model.Value]], "Adapters for types that have already been fully emitted (emit references for these)"]

    TYPE_ = hydra.core.Name("hydra.avro.environment.EncodeEnvironment")
    TYPE_MAP = hydra.core.Name("typeMap")
    EMITTED = hydra.core.Name("emitted")
