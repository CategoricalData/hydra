# Note: this is an automatically generated file. Do not edit.

r"""JSON serialization and deserialization for Avro schemas."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.avro.schema
import hydra.coders
import hydra.core
import hydra.errors
import hydra.json.model
import hydra.json.parser
import hydra.json.writer
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.parsing

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

avro_type = "type"

avro_items = "items"

def err(cx: T0, msg: str) -> Either[hydra.errors.Error, T1]:
    r"""Construct an error result with a message in context."""

    return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(msg))))

def require_e(cx: T0, fname: str, m: FrozenDict[str, T1]) -> Either[hydra.errors.Error, T1]:
    r"""Look up a required attribute in a JSON object map."""

    return hydra.lib.maybes.maybe((lambda : err(cx, hydra.lib.strings.cat(("required attribute ", hydra.lib.literals.show_string(fname), " not found")))), (lambda v: Right(v)), hydra.lib.maps.lookup(fname, m))

avro_default = "default"

avro_symbols = "symbols"

def expect_string_e(cx: T0, value: hydra.json.model.Value) -> Either[T1, str]:
    r"""Extract a JSON string or return an error."""

    match value:
        case hydra.json.model.ValueString(value=v):
            return Right(v)

        case _:
            raise TypeError("Unsupported Value")

def opt_string_e(cx: T0, fname: T1, m: FrozenDict[T1, hydra.json.model.Value]) -> Either[T2, Maybe[str]]:
    r"""Look up an optional string attribute in a JSON object map."""

    return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda v: hydra.lib.eithers.map((lambda s: hydra.lib.maybes.pure(s)), expect_string_e(cx, v))), hydra.lib.maps.lookup(fname, m))

def expect_array_e(cx: T0, value: hydra.json.model.Value) -> Either[T1, frozenlist[hydra.json.model.Value]]:
    r"""Extract a JSON array or return an error."""

    match value:
        case hydra.json.model.ValueArray(value=v):
            return Right(v)

        case _:
            raise TypeError("Unsupported Value")

def require_array_e(cx: T0, fname: str, m: FrozenDict[str, hydra.json.model.Value]) -> Either[hydra.errors.Error, frozenlist[hydra.json.model.Value]]:
    r"""Look up a required array attribute in a JSON object map."""

    return hydra.lib.eithers.bind(require_e(cx, fname, m), (lambda v: expect_array_e(cx, v)))

def decode_enum(cx: T0, m: FrozenDict[str, hydra.json.model.Value]) -> Either[hydra.errors.Error, hydra.avro.schema.NamedType]:
    r"""Decode an Avro enum type from a JSON object map."""

    return hydra.lib.eithers.bind(require_array_e(cx, avro_symbols, m), (lambda syms: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: expect_string_e(cx, v1)), syms), (lambda symbols: hydra.lib.eithers.bind(opt_string_e(cx, avro_default, m), (lambda def_val: Right(cast(hydra.avro.schema.NamedType, hydra.avro.schema.NamedTypeEnum(hydra.avro.schema.Enum(symbols, def_val))))))))))

avro_size = "size"

def expect_number_e(cx: T0, value: hydra.json.model.Value) -> Either[T1, Decimal]:
    r"""Extract a JSON number or return an error."""

    match value:
        case hydra.json.model.ValueNumber(value=v):
            return Right(v)

        case _:
            raise TypeError("Unsupported Value")

def require_number_e(cx: T0, fname: str, m: FrozenDict[str, hydra.json.model.Value]) -> Either[hydra.errors.Error, Decimal]:
    r"""Look up a required number attribute in a JSON object map."""

    return hydra.lib.eithers.bind(require_e(cx, fname, m), (lambda v: expect_number_e(cx, v)))

def decode_fixed(cx: T0, m: FrozenDict[str, hydra.json.model.Value]) -> Either[hydra.errors.Error, hydra.avro.schema.NamedType]:
    r"""Decode an Avro fixed type from a JSON object map."""

    return hydra.lib.eithers.bind(require_number_e(cx, avro_size, m), (lambda n: (size := hydra.lib.literals.bigint_to_int32(hydra.lib.literals.decimal_to_bigint(n)), Right(cast(hydra.avro.schema.NamedType, hydra.avro.schema.NamedTypeFixed(hydra.avro.schema.Fixed(size)))))[1]))

avro_values = "values"

avro_doc = "doc"

avro_name = "name"

avro_namespace = "namespace"

avro_aliases = "aliases"

def opt_array_e(cx: T0, fname: T1, m: FrozenDict[T1, hydra.json.model.Value]) -> Either[T2, Maybe[frozenlist[hydra.json.model.Value]]]:
    r"""Look up an optional array attribute in a JSON object map."""

    return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda v: hydra.lib.eithers.map((lambda a: hydra.lib.maybes.pure(a)), expect_array_e(cx, v))), hydra.lib.maps.lookup(fname, m))

def decode_aliases(cx: T0, m: FrozenDict[str, hydra.json.model.Value]) -> Either[T1, Maybe[frozenlist[str]]]:
    r"""Decode aliases from a JSON object map."""

    return hydra.lib.eithers.bind(opt_array_e(cx, avro_aliases, m), (lambda m_arr: hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda arr: hydra.lib.eithers.map((lambda strs: hydra.lib.maybes.pure(strs)), hydra.lib.eithers.map_list((lambda v1: expect_string_e(cx, v1)), arr))), m_arr)))

def get_annotations(m: FrozenDict[str, T0]) -> FrozenDict[str, T0]:
    r"""Extract annotation entries (keys starting with @) from a JSON object map."""

    return hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda entry: (k := hydra.lib.pairs.first(entry), v := hydra.lib.pairs.second(entry), hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.maybes.from_maybe((lambda : 0), hydra.lib.strings.maybe_char_at(0, k)), 64), (lambda : hydra.lib.maybes.pure((hydra.lib.strings.from_list(hydra.lib.lists.drop(1, hydra.lib.strings.to_list(k))), v))), (lambda : Nothing())))[2]), hydra.lib.maps.to_list(m))))

def require_string_e(cx: T0, fname: str, m: FrozenDict[str, hydra.json.model.Value]) -> Either[hydra.errors.Error, str]:
    r"""Look up a required string attribute in a JSON object map."""

    return hydra.lib.eithers.bind(require_e(cx, fname, m), (lambda v: expect_string_e(cx, v)))

def decode_named_schema(cx: T0, m: FrozenDict[str, hydra.json.model.Value], named_type_result: Either[hydra.errors.Error, hydra.avro.schema.NamedType]) -> Either[hydra.errors.Error, hydra.avro.schema.Schema]:
    r"""Decode a named Avro schema from a JSON object map and a decoded named type result."""

    return hydra.lib.eithers.bind(require_string_e(cx, avro_name, m), (lambda name: hydra.lib.eithers.bind(opt_string_e(cx, avro_namespace, m), (lambda ns: hydra.lib.eithers.bind(opt_string_e(cx, avro_doc, m), (lambda sdoc: hydra.lib.eithers.bind(decode_aliases(cx, m), (lambda aliases: hydra.lib.eithers.bind(named_type_result, (lambda named_type: Right(cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaNamed(hydra.avro.schema.Named(name, ns, aliases, sdoc, named_type, get_annotations(m)))))))))))))))

def decode_primitive_name(s: str) -> Maybe[hydra.avro.schema.Primitive]:
    r"""Decode a primitive type name string to a Primitive, or Nothing if not a primitive."""

    return hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "null"), (lambda : Just(hydra.avro.schema.Primitive.NULL)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "boolean"), (lambda : Just(hydra.avro.schema.Primitive.BOOLEAN)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "int"), (lambda : Just(hydra.avro.schema.Primitive.INT)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "long"), (lambda : Just(hydra.avro.schema.Primitive.LONG)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "float"), (lambda : Just(hydra.avro.schema.Primitive.FLOAT)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "double"), (lambda : Just(hydra.avro.schema.Primitive.DOUBLE)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "bytes"), (lambda : Just(hydra.avro.schema.Primitive.BYTES)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "string"), (lambda : Just(hydra.avro.schema.Primitive.STRING)), (lambda : Nothing()))))))))))))))))

avro_fields = "fields"

avro_order = "order"

def decode_order(cx: T0, o: str) -> Either[hydra.errors.Error, hydra.avro.schema.Order]:
    r"""Decode an Avro field ordering from a string."""

    return hydra.lib.logic.if_else(hydra.lib.equality.equal(o, "ascending"), (lambda : Right(hydra.avro.schema.Order.ASCENDING)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(o, "descending"), (lambda : Right(hydra.avro.schema.Order.DESCENDING)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(o, "ignore"), (lambda : Right(hydra.avro.schema.Order.IGNORE)), (lambda : err(cx, hydra.lib.strings.cat(("unknown order: ", o)))))))))

def opt_e(k: T0, m: FrozenDict[T0, T1]) -> Maybe[T1]:
    r"""Look up an optional attribute in a JSON object map."""

    return hydra.lib.maps.lookup(k, m)

def expect_object_e(cx: T0, value: hydra.json.model.Value) -> Either[T1, FrozenDict[str, hydra.json.model.Value]]:
    r"""Extract a JSON object or return an error."""

    match value:
        case hydra.json.model.ValueObject(value=v):
            return Right(v)

        case _:
            raise TypeError("Unsupported Value")

def show_json_value(v: hydra.json.model.Value) -> str:
    r"""Convert a JSON value to its string representation."""

    return hydra.json.writer.print_json(v)

def decode_array_schema(cx: T0, m: FrozenDict[str, hydra.json.model.Value]) -> Either[hydra.errors.Error, hydra.avro.schema.Schema]:
    r"""Decode an Avro array schema from a JSON object map."""

    return hydra.lib.eithers.bind(require_e(cx, avro_items, m), (lambda items: hydra.lib.eithers.map((lambda s: cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaArray(hydra.avro.schema.Array(s)))), decode_schema(cx, items))))

def decode_field(cx: T0, m: FrozenDict[str, hydra.json.model.Value]) -> Either[hydra.errors.Error, hydra.avro.schema.Field]:
    r"""Decode an Avro field from a JSON object map."""

    return hydra.lib.eithers.bind(require_string_e(cx, avro_name, m), (lambda name: hydra.lib.eithers.bind(opt_string_e(cx, avro_doc, m), (lambda fdoc: hydra.lib.eithers.bind(require_e(cx, avro_type, m), (lambda type_json: hydra.lib.eithers.bind(decode_schema(cx, type_json), (lambda field_type: hydra.lib.eithers.bind(hydra.lib.eithers.bind(opt_string_e(cx, avro_order, m), (lambda m_ord: hydra.lib.eithers.map_maybe((lambda v1: decode_order(cx, v1)), m_ord))), (lambda order: hydra.lib.eithers.bind(decode_aliases(cx, m), (lambda aliases: Right(hydra.avro.schema.Field(name, fdoc, field_type, opt_e(avro_default, m), order, aliases, get_annotations(m)))))))))))))))

def decode_map_schema(cx: T0, m: FrozenDict[str, hydra.json.model.Value]) -> Either[hydra.errors.Error, hydra.avro.schema.Schema]:
    r"""Decode an Avro map schema from a JSON object map."""

    return hydra.lib.eithers.bind(require_e(cx, avro_values, m), (lambda values: hydra.lib.eithers.map((lambda s: cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaMap(hydra.avro.schema.Map(s)))), decode_schema(cx, values))))

def decode_object_schema(cx: T0, m: FrozenDict[str, hydra.json.model.Value], type_name: str) -> Either[hydra.errors.Error, hydra.avro.schema.Schema]:
    r"""Decode an Avro schema from a JSON object given the type name."""

    return hydra.lib.logic.if_else(hydra.lib.equality.equal(type_name, "array"), (lambda : decode_array_schema(cx, m)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(type_name, "map"), (lambda : decode_map_schema(cx, m)), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(type_name, "record"), (lambda : decode_named_schema(cx, m, decode_record(cx, m))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(type_name, "enum"), (lambda : decode_named_schema(cx, m, decode_enum(cx, m))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(type_name, "fixed"), (lambda : decode_named_schema(cx, m, decode_fixed(cx, m))), (lambda : hydra.lib.maybes.maybe((lambda : err(cx, hydra.lib.strings.cat(("unknown type: ", type_name)))), (lambda p: Right(cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaPrimitive(p)))), decode_primitive_name(type_name))))))))))))

def decode_record(cx: T0, m: FrozenDict[str, hydra.json.model.Value]) -> Either[hydra.errors.Error, hydra.avro.schema.NamedType]:
    r"""Decode an Avro record type from a JSON object map."""

    return hydra.lib.eithers.bind(require_array_e(cx, avro_fields, m), (lambda field_jsons: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda fj: hydra.lib.eithers.bind(expect_object_e(cx, fj), (lambda fm: decode_field(cx, fm)))), field_jsons), (lambda fields: Right(cast(hydra.avro.schema.NamedType, hydra.avro.schema.NamedTypeRecord(hydra.avro.schema.Record(fields))))))))

def decode_schema(cx: T0, v: hydra.json.model.Value) -> Either[hydra.errors.Error, hydra.avro.schema.Schema]:
    r"""Decode an Avro schema from a JSON value."""

    match v:
        case hydra.json.model.ValueString(value=s):
            return hydra.lib.maybes.maybe((lambda : Right(cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaReference(s)))), (lambda p: Right(cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaPrimitive(p)))), decode_primitive_name(s))

        case hydra.json.model.ValueArray(value=schemas):
            return hydra.lib.eithers.map((lambda decoded: cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaUnion(hydra.avro.schema.Union(decoded)))), hydra.lib.eithers.map_list((lambda v1: decode_schema(cx, v1)), schemas))

        case hydra.json.model.ValueObject(value=m):
            return hydra.lib.eithers.bind(require_string_e(cx, avro_type, m), (lambda type_name: decode_object_schema(cx, m, type_name)))

        case _:
            return err(cx, hydra.lib.strings.cat(("unexpected JSON value for schema: ", show_json_value(v))))

def encode_annotations(m: FrozenDict[str, T0]) -> frozenlist[tuple[str, T0]]:
    r"""Encode annotations as key-value pairs with @ prefix on keys."""

    return hydra.lib.lists.map((lambda entry: (hydra.lib.strings.cat2("@", hydra.lib.pairs.first(entry)), hydra.lib.pairs.second(entry))), hydra.lib.maps.to_list(m))

def encode_enum(e: hydra.avro.schema.Enum) -> frozenlist[tuple[str, hydra.json.model.Value]]:
    r"""Encode an Avro enum type as key-value pairs."""

    return hydra.lib.lists.concat(((("type", cast(hydra.json.model.Value, hydra.json.model.ValueString("enum"))),), (("symbols", cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map((lambda s: cast(hydra.json.model.Value, hydra.json.model.ValueString(s))), e.symbols)))),), hydra.lib.maybes.maybe((lambda : ()), (lambda d: (("default", cast(hydra.json.model.Value, hydra.json.model.ValueString(d))),)), e.default)))

def encode_fixed(f: hydra.avro.schema.Fixed) -> frozenlist[tuple[str, hydra.json.model.Value]]:
    r"""Encode an Avro fixed type as key-value pairs."""

    return (("type", cast(hydra.json.model.Value, hydra.json.model.ValueString("fixed"))), ("size", cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.int32_to_bigint(f.size))))))

def encode_order(o: hydra.avro.schema.Order) -> tuple[str, hydra.json.model.Value]:
    def _hoist_hydra_avro_schema_json_encode_order_1(v1):
        match v1:
            case hydra.avro.schema.Order.ASCENDING:
                return "ascending"

            case hydra.avro.schema.Order.DESCENDING:
                return "descending"

            case hydra.avro.schema.Order.IGNORE:
                return "ignore"

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return ("order", cast(hydra.json.model.Value, hydra.json.model.ValueString(_hoist_hydra_avro_schema_json_encode_order_1(o))))

def encode_primitive(p: hydra.avro.schema.Primitive) -> hydra.json.model.Value:
    def _hoist_hydra_avro_schema_json_encode_primitive_1(v1):
        match v1:
            case hydra.avro.schema.Primitive.NULL:
                return "null"

            case hydra.avro.schema.Primitive.BOOLEAN:
                return "boolean"

            case hydra.avro.schema.Primitive.INT:
                return "int"

            case hydra.avro.schema.Primitive.LONG:
                return "long"

            case hydra.avro.schema.Primitive.FLOAT:
                return "float"

            case hydra.avro.schema.Primitive.DOUBLE:
                return "double"

            case hydra.avro.schema.Primitive.BYTES:
                return "bytes"

            case hydra.avro.schema.Primitive.STRING:
                return "string"

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return cast(hydra.json.model.Value, hydra.json.model.ValueString(_hoist_hydra_avro_schema_json_encode_primitive_1(p)))

def encode_array(arr: hydra.avro.schema.Array) -> hydra.json.model.Value:
    r"""Encode an Avro array schema to a JSON object."""

    return cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list((("type", cast(hydra.json.model.Value, hydra.json.model.ValueString("array"))), ("items", encode_schema(arr.items))))))

def encode_field(f: hydra.avro.schema.Field) -> hydra.json.model.Value:
    r"""Encode an Avro field to a JSON object."""

    return cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.lists.concat(((("name", cast(hydra.json.model.Value, hydra.json.model.ValueString(f.name))),), (("type", encode_schema(f.type)),), hydra.lib.maybes.maybe((lambda : ()), (lambda d: (("doc", cast(hydra.json.model.Value, hydra.json.model.ValueString(d))),)), f.doc), hydra.lib.maybes.maybe((lambda : ()), (lambda d: (("default", d),)), f.default), hydra.lib.maybes.maybe((lambda : ()), (lambda o: (encode_order(o),)), f.order), hydra.lib.maybes.maybe((lambda : ()), (lambda als: (("aliases", cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map((lambda a: cast(hydra.json.model.Value, hydra.json.model.ValueString(a))), als)))),)), f.aliases), encode_annotations(f.annotations))))))

def encode_map(mp: hydra.avro.schema.Map) -> hydra.json.model.Value:
    r"""Encode an Avro map schema to a JSON object."""

    return cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list((("type", cast(hydra.json.model.Value, hydra.json.model.ValueString("map"))), ("values", encode_schema(mp.values))))))

def encode_named(n: hydra.avro.schema.Named) -> hydra.json.model.Value:
    r"""Encode an Avro named type to a JSON object."""

    return cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.lists.concat(((("name", cast(hydra.json.model.Value, hydra.json.model.ValueString(n.name))),), hydra.lib.maybes.maybe((lambda : ()), (lambda ns: (("namespace", cast(hydra.json.model.Value, hydra.json.model.ValueString(ns))),)), n.namespace), hydra.lib.maybes.maybe((lambda : ()), (lambda d: (("doc", cast(hydra.json.model.Value, hydra.json.model.ValueString(d))),)), n.doc), hydra.lib.maybes.maybe((lambda : ()), (lambda als: (("aliases", cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map((lambda a: cast(hydra.json.model.Value, hydra.json.model.ValueString(a))), als)))),)), n.aliases), encode_named_type(n.type), encode_annotations(n.annotations))))))

def encode_named_type(nt: hydra.avro.schema.NamedType) -> frozenlist[tuple[str, hydra.json.model.Value]]:
    r"""Encode the specific variant of a named Avro type."""

    match nt:
        case hydra.avro.schema.NamedTypeEnum(value=e):
            return encode_enum(e)

        case hydra.avro.schema.NamedTypeFixed(value=f):
            return encode_fixed(f)

        case hydra.avro.schema.NamedTypeRecord(value=r):
            return encode_record(r)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_record(r: hydra.avro.schema.Record) -> frozenlist[tuple[str, hydra.json.model.Value]]:
    r"""Encode an Avro record type as key-value pairs."""

    return (("type", cast(hydra.json.model.Value, hydra.json.model.ValueString("record"))), ("fields", cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map((lambda x1: encode_field(x1)), r.fields)))))

def encode_schema(schema: hydra.avro.schema.Schema) -> hydra.json.model.Value:
    r"""Encode an Avro schema to a JSON value."""

    match schema:
        case hydra.avro.schema.SchemaPrimitive(value=p):
            return encode_primitive(p)

        case hydra.avro.schema.SchemaArray(value=arr):
            return encode_array(arr)

        case hydra.avro.schema.SchemaMap(value=mp):
            return encode_map(mp)

        case hydra.avro.schema.SchemaNamed(value=n):
            return encode_named(n)

        case hydra.avro.schema.SchemaReference(value=ref):
            return cast(hydra.json.model.Value, hydra.json.model.ValueString(ref))

        case hydra.avro.schema.SchemaUnion(value=u):
            return encode_union(u)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_union(u: hydra.avro.schema.Union) -> hydra.json.model.Value:
    r"""Encode an Avro union as a JSON array of schemas."""

    return cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map((lambda x1: encode_schema(x1)), u.value)))

def avro_schema_json_coder(cx: T0) -> hydra.coders.Coder[hydra.avro.schema.Schema, hydra.json.model.Value]:
    r"""Create a coder between Avro schemas and JSON values."""

    return hydra.coders.Coder((lambda _cx, schema: Right(encode_schema(schema))), (lambda cx2, json: decode_schema(cx2, json)))

def string_to_json_value(s: str):
    def _hoist_hydra_avro_schema_json_string_to_json_value_1(v1):
        match v1:
            case hydra.parsing.ParseResultSuccess(value=success):
                return Right(success.value)

            case hydra.parsing.ParseResultFailure(value=failure):
                return Left(failure.message)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return _hoist_hydra_avro_schema_json_string_to_json_value_1(hydra.json.parser.parse_json(s))

def avro_schema_string_coder(cx: T0) -> hydra.coders.Coder[hydra.avro.schema.Schema, str]:
    r"""Create a coder between Avro schemas and JSON strings."""

    return hydra.coders.Coder((lambda _cx, schema: Right(show_json_value(encode_schema(schema)))), (lambda cx2, s: hydra.lib.eithers.bind(hydra.lib.eithers.either((lambda e: err(cx2, e)), (lambda v: Right(v)), string_to_json_value(s)), (lambda json: decode_schema(cx2, json)))))

avro_array = "array"

avro_ascending = "ascending"

avro_boolean = "boolean"

avro_bytes = "bytes"

avro_descending = "descending"

avro_double = "double"

avro_enum = "enum"

avro_fixed = "fixed"

avro_float = "float"

avro_ignore = "ignore"

avro_int = "int"

avro_long = "long"

avro_map = "map"

avro_null = "null"

avro_record = "record"

avro_string = "string"

def unexpected_e(cx: T0, expected: str, found: str) -> Either[hydra.errors.Error, T1]:
    r"""Construct an error for unexpected values."""

    return err(cx, hydra.lib.strings.cat(("Expected ", expected, ", found: ", found)))
