# Note: this is an automatically generated file. Do not edit.

r"""Based on https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/source_context.proto."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from typing import Annotated, TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class SourceContext:
    r"""`SourceContext` represents information about the source of a protobuf element, like the file in which it is defined."""

    file_name: Annotated[str, "The path-qualified name of the .proto file that contained the associated protobuf element.  For example: `\"google/protobuf/source_context.proto\"`."]

    TYPE_ = hydra.core.Name("hydra.protobuf.sourceContext.SourceContext")
    FILE_NAME = hydra.core.Name("fileName")
