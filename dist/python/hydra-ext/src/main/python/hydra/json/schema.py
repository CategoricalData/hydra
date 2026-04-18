# Note: this is an automatically generated file. Do not edit.

r"""A model for JSON Schema. Based on https://cswr.github.io/JsonSchema/spec/grammar."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core
import hydra.json.model

@dataclass(frozen=True)
class Document:
    id: Maybe[str]
    definitions: Maybe[FrozenDict[Keyword, Schema]]
    root: Schema

    TYPE_ = hydra.core.Name("hydra.json.schema.Document")
    ID = hydra.core.Name("id")
    DEFINITIONS = hydra.core.Name("definitions")
    ROOT = hydra.core.Name("root")

class Keyword(Node[str]):
    ...

Keyword.TYPE_ = hydra.core.Name("hydra.json.schema.Keyword")

class Schema(Node["frozenlist[Restriction]"]):
    ...

Schema.TYPE_ = hydra.core.Name("hydra.json.schema.Schema")

class RestrictionType(Node["Type"]):
    ...

class RestrictionString(Node["StringRestriction"]):
    ...

class RestrictionNumber(Node["NumericRestriction"]):
    ...

class RestrictionArray(Node["ArrayRestriction"]):
    ...

class RestrictionObject(Node["ObjectRestriction"]):
    ...

class RestrictionMultiple(Node["MultipleRestriction"]):
    ...

class RestrictionReference(Node["SchemaReference"]):
    ...

class RestrictionTitle(Node[str]):
    ...

class RestrictionDescription(Node[str]):
    ...

class _RestrictionMeta(type):
    def __getitem__(cls, item):
        return object

class Restriction(metaclass=_RestrictionMeta):
    r"""RestrictionType | RestrictionString | RestrictionNumber | RestrictionArray | RestrictionObject | RestrictionMultiple | RestrictionReference | RestrictionTitle | RestrictionDescription"""

    TYPE_ = hydra.core.Name("hydra.json.schema.Restriction")
    TYPE = hydra.core.Name("type")
    STRING = hydra.core.Name("string")
    NUMBER = hydra.core.Name("number")
    ARRAY = hydra.core.Name("array")
    OBJECT = hydra.core.Name("object")
    MULTIPLE = hydra.core.Name("multiple")
    REFERENCE = hydra.core.Name("reference")
    TITLE = hydra.core.Name("title")
    DESCRIPTION = hydra.core.Name("description")

class TypeSingle(Node["TypeName"]):
    ...

class TypeMultiple(Node["frozenlist[TypeName]"]):
    ...

class _TypeMeta(type):
    def __getitem__(cls, item):
        return object

class Type(metaclass=_TypeMeta):
    r"""TypeSingle | TypeMultiple"""

    TYPE_ = hydra.core.Name("hydra.json.schema.Type")
    SINGLE = hydra.core.Name("single")
    MULTIPLE = hydra.core.Name("multiple")

class TypeName(Enum):
    STRING = hydra.core.Name("string")

    INTEGER = hydra.core.Name("integer")

    NUMBER = hydra.core.Name("number")

    BOOLEAN = hydra.core.Name("boolean")

    NULL = hydra.core.Name("null")

    ARRAY = hydra.core.Name("array")

    OBJECT = hydra.core.Name("object")

TypeName.TYPE_ = hydra.core.Name("hydra.json.schema.TypeName")

class StringRestrictionMinLength(Node[int]):
    ...

class StringRestrictionMaxLength(Node[int]):
    ...

class StringRestrictionPattern(Node["RegularExpression"]):
    ...

class _StringRestrictionMeta(type):
    def __getitem__(cls, item):
        return object

class StringRestriction(metaclass=_StringRestrictionMeta):
    r"""StringRestrictionMinLength | StringRestrictionMaxLength | StringRestrictionPattern"""

    TYPE_ = hydra.core.Name("hydra.json.schema.StringRestriction")
    MIN_LENGTH = hydra.core.Name("minLength")
    MAX_LENGTH = hydra.core.Name("maxLength")
    PATTERN = hydra.core.Name("pattern")

class RegularExpression(Node[str]):
    ...

RegularExpression.TYPE_ = hydra.core.Name("hydra.json.schema.RegularExpression")

class NumericRestrictionMinimum(Node["Limit"]):
    ...

class NumericRestrictionMaximum(Node["Limit"]):
    ...

class NumericRestrictionMultipleOf(Node[int]):
    ...

class _NumericRestrictionMeta(type):
    def __getitem__(cls, item):
        return object

class NumericRestriction(metaclass=_NumericRestrictionMeta):
    r"""NumericRestrictionMinimum | NumericRestrictionMaximum | NumericRestrictionMultipleOf"""

    TYPE_ = hydra.core.Name("hydra.json.schema.NumericRestriction")
    MINIMUM = hydra.core.Name("minimum")
    MAXIMUM = hydra.core.Name("maximum")
    MULTIPLE_OF = hydra.core.Name("multipleOf")

@dataclass(frozen=True)
class Limit:
    value: int
    exclusive: bool

    TYPE_ = hydra.core.Name("hydra.json.schema.Limit")
    VALUE = hydra.core.Name("value")
    EXCLUSIVE = hydra.core.Name("exclusive")

class ArrayRestrictionItems(Node["Items"]):
    ...

class ArrayRestrictionAdditionalItems(Node["AdditionalItems"]):
    ...

class ArrayRestrictionMinItems(Node[int]):
    ...

class ArrayRestrictionMaxItems(Node[int]):
    ...

class ArrayRestrictionUniqueItems(Node[bool]):
    ...

class _ArrayRestrictionMeta(type):
    def __getitem__(cls, item):
        return object

class ArrayRestriction(metaclass=_ArrayRestrictionMeta):
    r"""ArrayRestrictionItems | ArrayRestrictionAdditionalItems | ArrayRestrictionMinItems | ArrayRestrictionMaxItems | ArrayRestrictionUniqueItems"""

    TYPE_ = hydra.core.Name("hydra.json.schema.ArrayRestriction")
    ITEMS = hydra.core.Name("items")
    ADDITIONAL_ITEMS = hydra.core.Name("additionalItems")
    MIN_ITEMS = hydra.core.Name("minItems")
    MAX_ITEMS = hydra.core.Name("maxItems")
    UNIQUE_ITEMS = hydra.core.Name("uniqueItems")

class ItemsSameItems(Node["Schema"]):
    ...

class ItemsVarItems(Node["frozenlist[Schema]"]):
    ...

class _ItemsMeta(type):
    def __getitem__(cls, item):
        return object

class Items(metaclass=_ItemsMeta):
    r"""ItemsSameItems | ItemsVarItems"""

    TYPE_ = hydra.core.Name("hydra.json.schema.Items")
    SAME_ITEMS = hydra.core.Name("sameItems")
    VAR_ITEMS = hydra.core.Name("varItems")

class AdditionalItemsAny(Node[bool]):
    ...

class AdditionalItemsSchema(Node["Schema"]):
    ...

class _AdditionalItemsMeta(type):
    def __getitem__(cls, item):
        return object

class AdditionalItems(metaclass=_AdditionalItemsMeta):
    r"""AdditionalItemsAny | AdditionalItemsSchema"""

    TYPE_ = hydra.core.Name("hydra.json.schema.AdditionalItems")
    ANY = hydra.core.Name("any")
    SCHEMA = hydra.core.Name("schema")

class ObjectRestrictionProperties(Node["FrozenDict[Keyword, Schema]"]):
    ...

class ObjectRestrictionAdditionalProperties(Node["AdditionalItems"]):
    ...

class ObjectRestrictionRequired(Node["frozenlist[Keyword]"]):
    ...

class ObjectRestrictionMinProperties(Node[int]):
    ...

class ObjectRestrictionMaxProperties(Node[int]):
    ...

class ObjectRestrictionDependencies(Node["FrozenDict[Keyword, SchemaOrArray]"]):
    ...

class ObjectRestrictionPatternProperties(Node["FrozenDict[RegularExpression, Schema]"]):
    ...

class _ObjectRestrictionMeta(type):
    def __getitem__(cls, item):
        return object

class ObjectRestriction(metaclass=_ObjectRestrictionMeta):
    r"""ObjectRestrictionProperties | ObjectRestrictionAdditionalProperties | ObjectRestrictionRequired | ObjectRestrictionMinProperties | ObjectRestrictionMaxProperties | ObjectRestrictionDependencies | ObjectRestrictionPatternProperties"""

    TYPE_ = hydra.core.Name("hydra.json.schema.ObjectRestriction")
    PROPERTIES = hydra.core.Name("properties")
    ADDITIONAL_PROPERTIES = hydra.core.Name("additionalProperties")
    REQUIRED = hydra.core.Name("required")
    MIN_PROPERTIES = hydra.core.Name("minProperties")
    MAX_PROPERTIES = hydra.core.Name("maxProperties")
    DEPENDENCIES = hydra.core.Name("dependencies")
    PATTERN_PROPERTIES = hydra.core.Name("patternProperties")

class SchemaOrArraySchema(Node["Schema"]):
    ...

class SchemaOrArrayArray(Node["frozenlist[Keyword]"]):
    ...

class _SchemaOrArrayMeta(type):
    def __getitem__(cls, item):
        return object

class SchemaOrArray(metaclass=_SchemaOrArrayMeta):
    r"""SchemaOrArraySchema | SchemaOrArrayArray"""

    TYPE_ = hydra.core.Name("hydra.json.schema.SchemaOrArray")
    SCHEMA = hydra.core.Name("schema")
    ARRAY = hydra.core.Name("array")

class MultipleRestrictionAllOf(Node["frozenlist[Schema]"]):
    ...

class MultipleRestrictionAnyOf(Node["frozenlist[Schema]"]):
    ...

class MultipleRestrictionOneOf(Node["frozenlist[Schema]"]):
    ...

class MultipleRestrictionNot(Node["Schema"]):
    ...

class MultipleRestrictionEnum(Node["frozenlist[hydra.json.model.Value]"]):
    ...

class _MultipleRestrictionMeta(type):
    def __getitem__(cls, item):
        return object

class MultipleRestriction(metaclass=_MultipleRestrictionMeta):
    r"""MultipleRestrictionAllOf | MultipleRestrictionAnyOf | MultipleRestrictionOneOf | MultipleRestrictionNot | MultipleRestrictionEnum"""

    TYPE_ = hydra.core.Name("hydra.json.schema.MultipleRestriction")
    ALL_OF = hydra.core.Name("allOf")
    ANY_OF = hydra.core.Name("anyOf")
    ONE_OF = hydra.core.Name("oneOf")
    NOT = hydra.core.Name("not")
    ENUM = hydra.core.Name("enum")

class SchemaReference(Node[str]):
    ...

SchemaReference.TYPE_ = hydra.core.Name("hydra.json.schema.SchemaReference")
