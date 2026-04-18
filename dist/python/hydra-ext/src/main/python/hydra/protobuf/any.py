# Note: this is an automatically generated file. Do not edit.

r"""Based on https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/any.proto."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from typing import Annotated, TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class Any:
    r"""`Any` contains an arbitrary serialized protocol buffer message along with a URL that describes the type of the serialized message."""

    type_url: Annotated[str, "A URL/resource name that uniquely identifies the type of the serialized protocol buffer message."]
    value: Annotated[bytes, "Must be a valid serialized protocol buffer of the above specified type."]

    TYPE_ = hydra.core.Name("hydra.protobuf.any.Any")
    TYPE_URL = hydra.core.Name("typeUrl")
    VALUE = hydra.core.Name("value")
