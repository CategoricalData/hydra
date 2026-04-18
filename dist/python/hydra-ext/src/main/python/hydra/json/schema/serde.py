# Note: this is an automatically generated file. Do not edit.

r"""Serialization functions for converting JSON Schema documents to JSON values."""

from __future__ import annotations
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Maybe, frozenlist
from typing import cast
import hydra.core
import hydra.json.model
import hydra.json.schema
import hydra.json.writer
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs

def encode_integer(n: int) -> hydra.json.model.Value:
    r"""Encode an integer as a JSON number value."""

    return cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.int32_to_bigint(n))))

key_items = "items"

key_additional_items = "additionalItems"

key_max_items = "maxItems"

key_min_items = "minItems"

key_unique_items = "uniqueItems"

key_all_of = "allOf"

key_any_of = "anyOf"

key_enum = "enum"

key_not = "not"

key_one_of = "oneOf"

key_exclusive_maximum = "exclusiveMaximum"

key_exclusive_minimum = "exclusiveMinimum"

key_maximum = "maximum"

key_minimum = "minimum"

key_multiple_of = "multipleOf"

def encode_numeric_restriction(r: hydra.json.schema.NumericRestriction) -> frozenlist[tuple[str, hydra.json.model.Value]]:
    r"""Encode a numeric restriction as a list of key-value pairs."""

    match r:
        case hydra.json.schema.NumericRestrictionMinimum(value=lim):
            value = lim.value
            excl = lim.exclusive
            return hydra.lib.lists.concat((((key_minimum, encode_integer(value)),), hydra.lib.logic.if_else(excl, (lambda : ((key_exclusive_minimum, cast(hydra.json.model.Value, hydra.json.model.ValueBoolean(True))),)), (lambda : ()))))

        case hydra.json.schema.NumericRestrictionMaximum(value=lim2):
            value = lim2.value
            excl = lim2.exclusive
            return hydra.lib.lists.concat((((key_maximum, encode_integer(value)),), hydra.lib.logic.if_else(excl, (lambda : ((key_exclusive_maximum, cast(hydra.json.model.Value, hydra.json.model.ValueBoolean(True))),)), (lambda : ()))))

        case hydra.json.schema.NumericRestrictionMultipleOf(value=n):
            return ((key_multiple_of, encode_integer(n)),)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_keyword(k: hydra.json.schema.Keyword) -> hydra.json.model.Value:
    r"""Encode a keyword as a JSON string value."""

    return cast(hydra.json.model.Value, hydra.json.model.ValueString(k.value))

key_additional_properties = "additionalProperties"

key_dependencies = "dependencies"

key_max_properties = "maxProperties"

key_min_properties = "minProperties"

key_pattern_properties = "patternProperties"

key_properties = "properties"

key_required = "required"

def encode_schema_reference(sr: hydra.json.schema.SchemaReference) -> hydra.json.model.Value:
    r"""Encode a schema reference as a JSON string value."""

    return cast(hydra.json.model.Value, hydra.json.model.ValueString(sr.value))

key_max_length = "maxLength"

key_min_length = "minLength"

key_pattern = "pattern"

def encode_string_restriction(r: hydra.json.schema.StringRestriction) -> tuple[str, hydra.json.model.Value]:
    r"""Encode a string restriction as a key-value pair."""

    match r:
        case hydra.json.schema.StringRestrictionMaxLength(value=n):
            return (key_max_length, cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.int32_to_bigint(n)))))

        case hydra.json.schema.StringRestrictionMinLength(value=n2):
            return (key_min_length, cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.int32_to_bigint(n2)))))

        case hydra.json.schema.StringRestrictionPattern(value=re):
            return (key_pattern, cast(hydra.json.model.Value, hydra.json.model.ValueString(re.value)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_type_name(t: hydra.json.schema.TypeName) -> hydra.json.model.Value:
    r"""Encode a type name as a JSON string value."""

    match t:
        case hydra.json.schema.TypeName.STRING:
            return cast(hydra.json.model.Value, hydra.json.model.ValueString("string"))

        case hydra.json.schema.TypeName.INTEGER:
            return cast(hydra.json.model.Value, hydra.json.model.ValueString("integer"))

        case hydra.json.schema.TypeName.NUMBER:
            return cast(hydra.json.model.Value, hydra.json.model.ValueString("number"))

        case hydra.json.schema.TypeName.BOOLEAN:
            return cast(hydra.json.model.Value, hydra.json.model.ValueString("boolean"))

        case hydra.json.schema.TypeName.NULL:
            return cast(hydra.json.model.Value, hydra.json.model.ValueString("null"))

        case hydra.json.schema.TypeName.ARRAY:
            return cast(hydra.json.model.Value, hydra.json.model.ValueString("array"))

        case hydra.json.schema.TypeName.OBJECT:
            return cast(hydra.json.model.Value, hydra.json.model.ValueString("object"))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_type(t: hydra.json.schema.Type) -> hydra.json.model.Value:
    r"""Encode a type as a JSON value."""

    match t:
        case hydra.json.schema.TypeSingle(value=name):
            return encode_type_name(name)

        case hydra.json.schema.TypeMultiple(value=names):
            return cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map((lambda x1: encode_type_name(x1)), names)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

key_description = "description"

key_ref = "$ref"

key_title = "title"

key_type = "type"

def encode_additional_items(ai: hydra.json.schema.AdditionalItems) -> hydra.json.model.Value:
    r"""Encode additional items as a JSON value."""

    match ai:
        case hydra.json.schema.AdditionalItemsAny(value=b):
            return cast(hydra.json.model.Value, hydra.json.model.ValueBoolean(b))

        case hydra.json.schema.AdditionalItemsSchema(value=schema):
            return encode_schema(schema)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_array_restriction(r: hydra.json.schema.ArrayRestriction) -> tuple[str, hydra.json.model.Value]:
    r"""Encode an array restriction as a key-value pair."""

    match r:
        case hydra.json.schema.ArrayRestrictionItems(value=items):
            return encode_items(items)

        case hydra.json.schema.ArrayRestrictionAdditionalItems(value=ai):
            return (key_additional_items, encode_additional_items(ai))

        case hydra.json.schema.ArrayRestrictionMinItems(value=n):
            return (key_min_items, encode_integer(n))

        case hydra.json.schema.ArrayRestrictionMaxItems(value=n2):
            return (key_max_items, encode_integer(n2))

        case hydra.json.schema.ArrayRestrictionUniqueItems(value=b):
            return (key_unique_items, cast(hydra.json.model.Value, hydra.json.model.ValueBoolean(b)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_items(items: hydra.json.schema.Items):
    def _hoist_hydra_json_schema_serde_encode_items_1(v1):
        match v1:
            case hydra.json.schema.ItemsSameItems(value=schema):
                return encode_schema(schema)

            case hydra.json.schema.ItemsVarItems(value=schemas):
                return cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map((lambda x1: encode_schema(x1)), schemas)))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return (key_items, _hoist_hydra_json_schema_serde_encode_items_1(items))

def encode_keyword_schema_or_array(p: tuple[hydra.json.schema.Keyword, hydra.json.schema.SchemaOrArray]) -> tuple[str, hydra.json.model.Value]:
    r"""Encode a keyword-schema-or-array pair as a key-value pair."""

    @lru_cache(1)
    def k() -> hydra.json.schema.Keyword:
        return hydra.lib.pairs.first(p)
    @lru_cache(1)
    def s() -> hydra.json.schema.SchemaOrArray:
        return hydra.lib.pairs.second(p)
    return (k().value, encode_schema_or_array(s()))

def encode_multiple_restriction(r: hydra.json.schema.MultipleRestriction) -> tuple[str, hydra.json.model.Value]:
    r"""Encode a multiple restriction as a key-value pair."""

    match r:
        case hydra.json.schema.MultipleRestrictionAllOf(value=schemas):
            return (key_all_of, cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map((lambda x1: encode_schema(x1)), schemas))))

        case hydra.json.schema.MultipleRestrictionAnyOf(value=schemas2):
            return (key_any_of, cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map((lambda x1: encode_schema(x1)), schemas2))))

        case hydra.json.schema.MultipleRestrictionOneOf(value=schemas3):
            return (key_one_of, cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map((lambda x1: encode_schema(x1)), schemas3))))

        case hydra.json.schema.MultipleRestrictionNot(value=schema):
            return (key_not, encode_schema(schema))

        case hydra.json.schema.MultipleRestrictionEnum(value=values):
            return (key_enum, cast(hydra.json.model.Value, hydra.json.model.ValueArray(values)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_object_restriction(r: hydra.json.schema.ObjectRestriction) -> tuple[str, hydra.json.model.Value]:
    r"""Encode an object restriction as a key-value pair."""

    match r:
        case hydra.json.schema.ObjectRestrictionProperties(value=props):
            return (key_properties, cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: encode_property(x1)), hydra.lib.maps.to_list(props))))))

        case hydra.json.schema.ObjectRestrictionAdditionalProperties(value=ai):
            return (key_additional_properties, encode_additional_items(ai))

        case hydra.json.schema.ObjectRestrictionRequired(value=keys):
            return (key_required, cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map((lambda x1: encode_keyword(x1)), keys))))

        case hydra.json.schema.ObjectRestrictionMinProperties(value=n):
            return (key_min_properties, encode_integer(n))

        case hydra.json.schema.ObjectRestrictionMaxProperties(value=n2):
            return (key_max_properties, encode_integer(n2))

        case hydra.json.schema.ObjectRestrictionDependencies(value=deps):
            return (key_dependencies, cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: encode_keyword_schema_or_array(x1)), hydra.lib.maps.to_list(deps))))))

        case hydra.json.schema.ObjectRestrictionPatternProperties(value=props2):
            return (key_pattern_properties, cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: encode_pattern_property(x1)), hydra.lib.maps.to_list(props2))))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_pattern_property(p: tuple[hydra.json.schema.RegularExpression, hydra.json.schema.Schema]) -> tuple[str, hydra.json.model.Value]:
    r"""Encode a pattern property pair as a key-value pair."""

    @lru_cache(1)
    def pat() -> hydra.json.schema.RegularExpression:
        return hydra.lib.pairs.first(p)
    @lru_cache(1)
    def s() -> hydra.json.schema.Schema:
        return hydra.lib.pairs.second(p)
    return (pat().value, encode_schema(s()))

def encode_property(p: tuple[hydra.json.schema.Keyword, hydra.json.schema.Schema]) -> tuple[str, hydra.json.model.Value]:
    r"""Encode a property pair as a key-value pair."""

    @lru_cache(1)
    def k() -> hydra.json.schema.Keyword:
        return hydra.lib.pairs.first(p)
    @lru_cache(1)
    def s() -> hydra.json.schema.Schema:
        return hydra.lib.pairs.second(p)
    return (k().value, encode_schema(s()))

def encode_restriction(r: hydra.json.schema.Restriction) -> frozenlist[tuple[str, hydra.json.model.Value]]:
    r"""Encode a restriction as a list of key-value pairs."""

    match r:
        case hydra.json.schema.RestrictionType(value=t):
            return ((key_type, encode_type(t)),)

        case hydra.json.schema.RestrictionString(value=sr):
            return (encode_string_restriction(sr),)

        case hydra.json.schema.RestrictionNumber(value=nr):
            return encode_numeric_restriction(nr)

        case hydra.json.schema.RestrictionArray(value=ar):
            return (encode_array_restriction(ar),)

        case hydra.json.schema.RestrictionObject(value=or_):
            return (encode_object_restriction(or_),)

        case hydra.json.schema.RestrictionMultiple(value=mr):
            return (encode_multiple_restriction(mr),)

        case hydra.json.schema.RestrictionReference(value=sr2):
            return ((key_ref, encode_schema_reference(sr2)),)

        case hydra.json.schema.RestrictionTitle(value=s):
            return ((key_title, cast(hydra.json.model.Value, hydra.json.model.ValueString(s))),)

        case hydra.json.schema.RestrictionDescription(value=s2):
            return ((key_description, cast(hydra.json.model.Value, hydra.json.model.ValueString(s2))),)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_schema(s: hydra.json.schema.Schema) -> hydra.json.model.Value:
    r"""Encode a schema as a JSON object value."""

    return cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.lists.concat(hydra.lib.lists.map((lambda x1: encode_restriction(x1)), s.value)))))

def encode_schema_or_array(soa: hydra.json.schema.SchemaOrArray) -> hydra.json.model.Value:
    r"""Encode a schema or array as a JSON value."""

    match soa:
        case hydra.json.schema.SchemaOrArraySchema(value=s):
            return encode_schema(s)

        case hydra.json.schema.SchemaOrArrayArray(value=keys):
            return cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map((lambda x1: encode_keyword(x1)), keys)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def from_object(v: hydra.json.model.Value) -> FrozenDict[str, hydra.json.model.Value]:
    r"""Extract the map from a JSON object value."""

    match v:
        case hydra.json.model.ValueObject(value=mp):
            return mp

        case _:
            raise TypeError("Unsupported Value")

key_definitions = "$defs"

key_id = "$id"

key_schema = "$schema"

def to_object(pairs: frozenlist[tuple[str, Maybe[hydra.json.model.Value]]]) -> hydra.json.model.Value:
    r"""Construct a JSON object from a list of optional key-value pairs, filtering out Nothing values."""

    return cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda p: (k := hydra.lib.pairs.first(p), mv := hydra.lib.pairs.second(p), hydra.lib.maybes.map((lambda v: (k, v)), mv))[2]), pairs)))))

def json_schema_document_to_json_value(doc: hydra.json.schema.Document) -> hydra.json.model.Value:
    r"""Convert a JSON Schema document to a JSON value."""

    mid = doc.id
    mdefs = doc.definitions
    root = doc.root
    @lru_cache(1)
    def schema_map() -> FrozenDict[str, hydra.json.model.Value]:
        return from_object(encode_schema(root))
    @lru_cache(1)
    def rest_map() -> FrozenDict[str, hydra.json.model.Value]:
        return from_object(to_object(((key_id, hydra.lib.maybes.map((lambda i: cast(hydra.json.model.Value, hydra.json.model.ValueString(i))), mid)), (key_schema, hydra.lib.maybes.pure(cast(hydra.json.model.Value, hydra.json.model.ValueString("http://json-schema.org/2020-12/schema")))), (key_definitions, hydra.lib.maybes.map((lambda mp: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda p: (k := hydra.lib.pairs.first(p), schema := hydra.lib.pairs.second(p), (k.value, encode_schema(schema)))[2]), hydra.lib.maps.to_list(mp)))))), mdefs)))))
    return cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.union(schema_map(), rest_map())))

def json_schema_document_to_string(doc: hydra.json.schema.Document) -> str:
    r"""Convert a JSON Schema document to a JSON string."""

    return hydra.json.writer.print_json(json_schema_document_to_json_value(doc))

key_label = "label"
