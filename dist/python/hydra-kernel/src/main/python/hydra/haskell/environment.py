# Note: this is an automatically generated file. Do not edit.

r"""Environment types for Haskell code generation."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from typing import Annotated, TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class HaskellModuleMetadata:
    r"""Metadata used to determine which standard imports are needed in a generated Haskell module."""

    uses_byte_string: Annotated[bool, "Whether the module uses Data.ByteString (B.ByteString)"]
    uses_int: Annotated[bool, "Whether the module uses Data.Int (I.Int8, I.Int16, I.Int64)"]
    uses_map: Annotated[bool, "Whether the module uses Data.Map (M.Map, M.fromList, M.empty)"]
    uses_set: Annotated[bool, "Whether the module uses Data.Set (S.Set, S.fromList, S.empty)"]

    TYPE_ = hydra.core.Name("hydra.haskell.environment.HaskellModuleMetadata")
    USES_BYTE_STRING = hydra.core.Name("usesByteString")
    USES_INT = hydra.core.Name("usesInt")
    USES_MAP = hydra.core.Name("usesMap")
    USES_SET = hydra.core.Name("usesSet")
