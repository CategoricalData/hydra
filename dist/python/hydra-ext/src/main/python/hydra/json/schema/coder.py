# Note: this is an automatically generated file. Do not edit.

r"""JSON Schema code generator: converts Hydra modules to JSON Schema documents."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.formatting
import hydra.json.schema
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.strings
import hydra.strip

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")
T4 = TypeVar("T4")
T5 = TypeVar("T5")

def construct_module(cx: T0, g: T1, opts: T2, mod: T3, type_defs: T4) -> T5:
    r"""Construct JSON Schema documents from type definitions."""

    while True:
        cx = cx
        g = g
        opts = opts
        mod = mod
        type_defs = type_defs
        continue

def encode_type(cx: T0, g: T1, optional: T2, typ: T3) -> T4:
    r"""Encode a Hydra type as a list of JSON Schema restrictions."""

    while True:
        cx = cx
        g = g
        optional = optional
        typ = typ
        continue

def encode_field(cx: T0, g: T1, ft: hydra.core.FieldType) -> Either[T2, tuple[hydra.json.schema.Keyword, hydra.json.schema.Schema]]:
    r"""Encode a field type as a JSON Schema keyword-schema pair."""

    name = ft.name
    typ = ft.type
    return hydra.lib.eithers.map((lambda res: (hydra.json.schema.Keyword(name.value), hydra.json.schema.Schema(res))), encode_type(cx, g, False, typ))

def encode_name(name: hydra.core.Name) -> str:
    r"""Encode a Hydra name as a safe identifier string, replacing non-alphanumeric characters with underscores."""

    return hydra.formatting.non_alnum_to_underscores(name.value)

def encode_named_type(cx: T0, g: T1, name: hydra.core.Name, typ: hydra.core.Type) -> Either[T2, frozenlist[hydra.json.schema.Restriction]]:
    r"""Encode a named type as a list of JSON Schema restrictions with a title."""

    return hydra.lib.eithers.map((lambda res: hydra.lib.lists.concat(((cast(hydra.json.schema.Restriction, hydra.json.schema.RestrictionTitle(name.value)),), res))), encode_type(cx, g, False, hydra.strip.deannotate_type(typ)))

def is_required_field(ft: hydra.core.FieldType) -> bool:
    r"""Determine whether a field is required (i.e., not optional/Maybe)."""

    typ = ft.type
    match hydra.strip.deannotate_type(typ):
        case hydra.core.TypeMaybe():
            return False

        case _:
            return True

def module_to_json_schema(opts: T0, mod: T1, defs: T2, cx: T3, g: T4) -> T5:
    r"""Convert a Hydra module to a map of JSON Schema documents."""

    while True:
        opts = opts
        mod = mod
        defs = defs
        cx = cx
        g = g
        continue

def reference_restriction(name: hydra.core.Name) -> hydra.json.schema.Restriction:
    r"""Create a JSON Schema reference restriction for a named type."""

    return cast(hydra.json.schema.Restriction, hydra.json.schema.RestrictionReference(hydra.json.schema.SchemaReference(hydra.lib.strings.cat(("#/$defs/", encode_name(name))))))
