# Note: this is an automatically generated file. Do not edit.

r"""Avro-to-Hydra adapter for converting Avro schemas and data to Hydra types and terms."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.avro.environment
import hydra.avro.schema
import hydra.coders
import hydra.core
import hydra.errors
import hydra.extract.core
import hydra.graph
import hydra.json.model
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.packaging
import hydra.strip

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")
T4 = TypeVar("T4")
T5 = TypeVar("T5")
T6 = TypeVar("T6")
T7 = TypeVar("T7")
T8 = TypeVar("T8")
T9 = TypeVar("T9")

def annotate_adapter(ann: Maybe[FrozenDict[hydra.core.Name, hydra.core.Term]], ad: hydra.coders.Adapter[T0, hydra.core.Type, T1, T2]) -> hydra.coders.Adapter[T0, hydra.core.Type, T1, T2]:
    r"""Annotate an adapter's target type with optional annotations."""

    return hydra.lib.maybes.maybe((lambda : ad), (lambda n: hydra.coders.Adapter(ad.is_lossy, ad.source, cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(ad.target, n))), ad.coder)), ann)

def avro_name_to_hydra_name(qname: hydra.avro.environment.AvroQualifiedName) -> hydra.core.Name:
    r"""Convert an Avro qualified name to a Hydra name."""

    mns = qname.namespace
    local = qname.name
    return hydra.names.unqualify_name(hydra.packaging.QualifiedName(hydra.lib.maybes.map((lambda s: hydra.packaging.Namespace(s)), mns), local))

def err(cx: T0, msg: str) -> Either[hydra.errors.Error, T1]:
    r"""Construct an error result with a message in context."""

    return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(msg))))

avro_primary_key = "@primaryKey"

def expect_string_e(cx: T0, value: hydra.json.model.Value) -> Either[T1, str]:
    r"""Extract a JSON string or return an error."""

    match value:
        case hydra.json.model.ValueString(value=v):
            return Right(v)

        case _:
            raise TypeError("Unsupported Value")

def pattern_to_name_constructor(pat: str, s: str) -> hydra.core.Name:
    r"""Create a name constructor from a pattern string."""

    return hydra.core.Name(hydra.lib.strings.intercalate(s, hydra.lib.strings.split_on("${}", pat)))

def primary_key_e(cx: T0, f: hydra.avro.schema.Field) -> Maybe[hydra.avro.environment.AvroPrimaryKey]:
    r"""Extract a primary key annotation from a field, if present."""

    return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda v: hydra.lib.eithers.either((lambda _: Nothing()), (lambda s: Just(hydra.avro.environment.AvroPrimaryKey(hydra.core.Name(f.name), (lambda v1: pattern_to_name_constructor(s, v1))))), expect_string_e(cx, v))), hydra.lib.maps.lookup(avro_primary_key, f.annotations))

def show_qname(qname: hydra.avro.environment.AvroQualifiedName) -> str:
    r"""Convert an Avro qualified name to a display string."""

    mns = qname.namespace
    local = qname.name
    return hydra.lib.strings.cat2(hydra.lib.maybes.maybe((lambda : ""), (lambda ns: hydra.lib.strings.cat2(ns, ".")), mns), local)

def find_avro_primary_key_field(cx: T0, qname: hydra.avro.environment.AvroQualifiedName, avro_fields: frozenlist[hydra.avro.schema.Field]) -> Either[hydra.errors.Error, Maybe[hydra.avro.environment.AvroPrimaryKey]]:
    r"""Find the primary key field among a list of Avro fields."""

    @lru_cache(1)
    def keys() -> frozenlist[hydra.avro.environment.AvroPrimaryKey]:
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda f: primary_key_e(cx, f)), avro_fields))
    return hydra.lib.logic.if_else(hydra.lib.lists.null(keys()), (lambda : Right(Nothing())), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(keys()), 1), (lambda : Right(hydra.lib.lists.maybe_head(keys()))), (lambda : err(cx, hydra.lib.strings.cat2("multiple primary key fields for ", show_qname(qname)))))))

def get_avro_hydra_adapter(qname: hydra.avro.environment.AvroQualifiedName, env: hydra.avro.environment.AvroEnvironment) -> Maybe[hydra.coders.Adapter[hydra.avro.schema.Schema, hydra.core.Type, hydra.json.model.Value, hydra.core.Term]]:
    r"""Look up an adapter by qualified name in the environment."""

    return hydra.lib.maps.lookup(qname, env.named_adapters)

def encode_annotation_value(v: hydra.json.model.Value) -> hydra.core.Term:
    r"""Encode a JSON value as a Hydra term for annotation purposes."""

    match v:
        case hydra.json.model.ValueArray(value=vals):
            return cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: encode_annotation_value(x1)), vals)))

        case hydra.json.model.ValueBoolean(value=b):
            return cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(b))))

        case hydra.json.model.ValueNull():
            return cast(hydra.core.Term, hydra.core.TermUnit())

        case hydra.json.model.ValueNumber(value=d):
            return cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralDecimal(d))))

        case hydra.json.model.ValueObject(value=m):
            return cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda entry: (k := hydra.lib.pairs.first(entry), v_ := hydra.lib.pairs.second(entry), (cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(k)))), encode_annotation_value(v_)))[2]), hydra.lib.maps.to_list(m)))))

        case hydra.json.model.ValueString(value=s):
            return cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def named_annotations_to_core(n: hydra.avro.schema.Named) -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    r"""Extract named type annotations and convert them to core Name/Term pairs."""

    return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda entry: (k := hydra.lib.pairs.first(entry), v := hydra.lib.pairs.second(entry), (hydra.core.Name(k), encode_annotation_value(v)))[2]), hydra.lib.maps.to_list(n.annotations)))

def parse_avro_name(mns: Maybe[str], name_: str) -> hydra.avro.environment.AvroQualifiedName:
    r"""Parse a dotted Avro name into a qualified name."""

    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", name_)
    @lru_cache(1)
    def local() -> str:
        return hydra.lib.maybes.from_maybe((lambda : name_), hydra.lib.lists.maybe_last(parts()))
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(parts()), 1), (lambda : hydra.avro.environment.AvroQualifiedName(mns, local())), (lambda : hydra.avro.environment.AvroQualifiedName(hydra.lib.maybes.map((lambda ps: hydra.lib.strings.intercalate(".", ps)), hydra.lib.lists.maybe_init(parts())), local())))

def field_annotations_to_core(f: hydra.avro.schema.Field) -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    r"""Extract field annotations and convert them to core Name/Term pairs."""

    return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda entry: (k := hydra.lib.pairs.first(entry), v := hydra.lib.pairs.second(entry), (hydra.core.Name(k), encode_annotation_value(v)))[2]), hydra.lib.maps.to_list(f.annotations)))

avro_foreign_key = "@foreignKey"

def expect_object_e(cx: T0, value: hydra.json.model.Value) -> Either[T1, FrozenDict[str, hydra.json.model.Value]]:
    r"""Extract a JSON object or return an error."""

    match value:
        case hydra.json.model.ValueObject(value=v):
            return Right(v)

        case _:
            raise TypeError("Unsupported Value")

def opt_string_e(cx: T0, fname: T1, m: FrozenDict[T1, hydra.json.model.Value]) -> Either[T2, Maybe[str]]:
    r"""Look up an optional string attribute in a JSON object map."""

    return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda v: hydra.lib.eithers.map((lambda s: hydra.lib.maybes.pure(s)), expect_string_e(cx, v))), hydra.lib.maps.lookup(fname, m))

def require_string_e(cx: T0, fname: str, m: FrozenDict[str, hydra.json.model.Value]) -> Either[hydra.errors.Error, str]:
    r"""Look up a required string attribute in a JSON object map."""

    return hydra.lib.maybes.maybe((lambda : err(cx, hydra.lib.strings.cat(("required attribute ", hydra.lib.literals.show_string(fname), " not found")))), (lambda v: expect_string_e(cx, v)), hydra.lib.maps.lookup(fname, m))

def foreign_key_e(cx: T0, f: hydra.avro.schema.Field) -> Either[hydra.errors.Error, Maybe[hydra.avro.environment.AvroForeignKey]]:
    r"""Extract a foreign key annotation from a field, if present."""

    return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda v: hydra.lib.eithers.bind(expect_object_e(cx, v), (lambda m: hydra.lib.eithers.bind(hydra.lib.eithers.map((lambda s: hydra.core.Name(s)), require_string_e(cx, "type", m)), (lambda tname: hydra.lib.eithers.bind(opt_string_e(cx, "pattern", m), (lambda pattern_: (constr := hydra.lib.maybes.maybe((lambda : (lambda s: hydra.core.Name(s))), (lambda pat, v1: pattern_to_name_constructor(pat, v1)), pattern_), Right(Just(hydra.avro.environment.AvroForeignKey(tname, (lambda x1: constr(x1))))))[1]))))))), hydra.lib.maps.lookup(avro_foreign_key, f.annotations))

def unexpected_e(cx: T0, expected: str, found: str) -> Either[hydra.errors.Error, T1]:
    r"""Construct an error for unexpected values."""

    return err(cx, hydra.lib.strings.cat(("Expected ", expected, ", found: ", found)))

def string_to_term_e(cx: T0, typ: hydra.core.Type, s: str):
    r"""Parse a string into a term of the expected type."""

    @lru_cache(1)
    def read_err() -> Either[hydra.errors.Error, T1]:
        return err(cx, "failed to read value")
    def read_and_wrap(reader: Callable[[str], Maybe[T1]], wrapper: Callable[[T1], hydra.core.Literal]) -> Either[hydra.errors.Error, hydra.core.Term]:
        return hydra.lib.maybes.maybe((lambda : read_err()), (lambda v: Right(cast(hydra.core.Term, hydra.core.TermLiteral(wrapper(v))))), reader(s))
    def _hoist_read_err_body_1(v1):
        match v1:
            case hydra.core.IntegerType.BIGINT:
                return read_and_wrap((lambda x: hydra.lib.literals.read_bigint(x)), (lambda i: cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueBigint(i))))))

            case hydra.core.IntegerType.INT8:
                return read_and_wrap((lambda x: hydra.lib.literals.read_int8(x)), (lambda i: cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt8(i))))))

            case hydra.core.IntegerType.INT16:
                return read_and_wrap((lambda x: hydra.lib.literals.read_int16(x)), (lambda i: cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt16(i))))))

            case hydra.core.IntegerType.INT32:
                return read_and_wrap((lambda x: hydra.lib.literals.read_int32(x)), (lambda i: cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(i))))))

            case hydra.core.IntegerType.INT64:
                return read_and_wrap((lambda x: hydra.lib.literals.read_int64(x)), (lambda i: cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(i))))))

            case hydra.core.IntegerType.UINT8:
                return read_and_wrap((lambda x: hydra.lib.literals.read_uint8(x)), (lambda i: cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint8(i))))))

            case hydra.core.IntegerType.UINT16:
                return read_and_wrap((lambda x: hydra.lib.literals.read_uint16(x)), (lambda i: cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint16(i))))))

            case hydra.core.IntegerType.UINT32:
                return read_and_wrap((lambda x: hydra.lib.literals.read_uint32(x)), (lambda i: cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint32(i))))))

            case hydra.core.IntegerType.UINT64:
                return read_and_wrap((lambda x: hydra.lib.literals.read_uint64(x)), (lambda i: cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint64(i))))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_read_err_body_2(v1):
        match v1:
            case hydra.core.LiteralTypeBoolean():
                return read_and_wrap((lambda x: hydra.lib.literals.read_boolean(x)), (lambda b: cast(hydra.core.Literal, hydra.core.LiteralBoolean(b))))

            case hydra.core.LiteralTypeInteger(value=it):
                return _hoist_read_err_body_1(it)

            case hydra.core.LiteralTypeString():
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s)))))

            case _:
                return unexpected_e(cx, "literal type", "other literal type")
    match hydra.strip.deannotate_type(typ):
        case hydra.core.TypeLiteral(value=lt):
            return _hoist_read_err_body_2(lt)

        case _:
            return unexpected_e(cx, "literal type", "other")

def term_to_string_e(cx: T0, term: hydra.core.Term):
    def _hoist_hydra_avro_coder_term_to_string_e_1(v1):
        match v1:
            case hydra.core.IntegerValueBigint(value=i):
                return hydra.lib.literals.show_bigint(i)

            case hydra.core.IntegerValueInt8(value=i):
                return hydra.lib.literals.show_int8(i)

            case hydra.core.IntegerValueInt16(value=i):
                return hydra.lib.literals.show_int16(i)

            case hydra.core.IntegerValueInt32(value=i):
                return hydra.lib.literals.show_int32(i)

            case hydra.core.IntegerValueInt64(value=i):
                return hydra.lib.literals.show_int64(i)

            case hydra.core.IntegerValueUint8(value=i):
                return hydra.lib.literals.show_uint8(i)

            case hydra.core.IntegerValueUint16(value=i):
                return hydra.lib.literals.show_uint16(i)

            case hydra.core.IntegerValueUint32(value=i):
                return hydra.lib.literals.show_uint32(i)

            case hydra.core.IntegerValueUint64(value=i):
                return hydra.lib.literals.show_uint64(i)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_avro_coder_term_to_string_e_2(cx, v1):
        match v1:
            case hydra.core.LiteralBoolean(value=b):
                return Right(hydra.lib.literals.show_boolean(b))

            case hydra.core.LiteralInteger(value=iv):
                return Right(_hoist_hydra_avro_coder_term_to_string_e_1(iv))

            case hydra.core.LiteralString(value=s):
                return Right(s)

            case _:
                return unexpected_e(cx, "boolean, integer, or string", "other literal")
    match hydra.strip.deannotate_term(term):
        case hydra.core.TermLiteral(value=l):
            return _hoist_hydra_avro_coder_term_to_string_e_2(cx, l)

        case hydra.core.TermMaybe(value=ot):
            return hydra.lib.maybes.maybe((lambda : unexpected_e(cx, "literal value", "Nothing")), (lambda term_: term_to_string_e(cx, term_)), ot)

        case _:
            return unexpected_e(cx, "literal value", "other")

def put_avro_hydra_adapter(qname: hydra.avro.environment.AvroQualifiedName, ad: hydra.coders.Adapter[hydra.avro.schema.Schema, hydra.core.Type, hydra.json.model.Value, hydra.core.Term], env: hydra.avro.environment.AvroEnvironment) -> hydra.avro.environment.AvroEnvironment:
    r"""Store an adapter in the environment by qualified name."""

    return hydra.avro.environment.AvroEnvironment(hydra.lib.maps.insert(qname, ad, env.named_adapters), env.namespace, env.elements)

def avro_hydra_adapter(cx: T0, schema: hydra.avro.schema.Schema, env0: hydra.avro.environment.AvroEnvironment):
    r"""Create an adapter between Avro schemas and Hydra types/terms."""

    def simple_adapter(env: T1, typ: T2, encode: Callable[[hydra.context.Context, T3], Either[hydra.errors.Error, T4]], decode: Callable[[hydra.context.Context, T4], Either[hydra.errors.Error, T3]]) -> Either[T5, tuple[hydra.coders.Adapter[hydra.avro.schema.Schema, T2, T3, T4], T1]]:
        return Right((hydra.coders.Adapter(False, schema, typ, hydra.coders.Coder(encode, decode)), env))
    def double_to_int(d: Decimal) -> int:
        return hydra.lib.literals.bigint_to_int32(hydra.lib.literals.decimal_to_bigint(d))
    def double_to_long(d: Decimal) -> int:
        return hydra.lib.literals.bigint_to_int64(hydra.lib.literals.decimal_to_bigint(d))
    def _hoist_simple_adapter_body_1(v1):
        match v1:
            case hydra.json.model.ValueString(value=s):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s)))))

            case _:
                raise TypeError("Unsupported Value")
    def _hoist_simple_adapter_body_2(v1):
        match v1:
            case hydra.json.model.ValueBoolean(value=b):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(b)))))

            case _:
                raise TypeError("Unsupported Value")
    def _hoist_simple_adapter_body_3(v1):
        match v1:
            case hydra.json.model.ValueNumber(value=d):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(double_to_int(d))))))))

            case _:
                raise TypeError("Unsupported Value")
    def _hoist_simple_adapter_body_4(v1):
        match v1:
            case hydra.json.model.ValueNumber(value=d):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(double_to_long(d))))))))

            case _:
                raise TypeError("Unsupported Value")
    def _hoist_simple_adapter_body_5(v1):
        match v1:
            case hydra.json.model.ValueNumber(value=d):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat32(hydra.lib.literals.decimal_to_float32(d))))))))

            case _:
                raise TypeError("Unsupported Value")
    def _hoist_simple_adapter_body_6(v1):
        match v1:
            case hydra.json.model.ValueNumber(value=d):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat64(hydra.lib.literals.decimal_to_float64(d))))))))

            case _:
                raise TypeError("Unsupported Value")
    def _hoist_simple_adapter_body_7(v1):
        match v1:
            case hydra.json.model.ValueString(value=s):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBinary(hydra.lib.literals.string_to_binary(s))))))

            case _:
                raise TypeError("Unsupported Value")
    def _hoist_simple_adapter_body_8(v1):
        match v1:
            case hydra.json.model.ValueString(value=s):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s)))))

            case _:
                raise TypeError("Unsupported Value")
    def _hoist_simple_adapter_body_9(v1):
        match v1:
            case hydra.avro.schema.Primitive.NULL:
                return simple_adapter(env0, cast(hydra.core.Type, hydra.core.TypeUnit()), (lambda _cx, jv: _hoist_simple_adapter_body_1(jv)), (lambda cx1, t: hydra.lib.eithers.map((lambda s: cast(hydra.json.model.Value, hydra.json.model.ValueString(s))), hydra.extract.core.string(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), t))))

            case hydra.avro.schema.Primitive.BOOLEAN:
                return simple_adapter(env0, cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeBoolean()))), (lambda _cx, jv: _hoist_simple_adapter_body_2(jv)), (lambda cx1, t: hydra.lib.eithers.map((lambda b: cast(hydra.json.model.Value, hydra.json.model.ValueBoolean(b))), hydra.extract.core.boolean(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), t))))

            case hydra.avro.schema.Primitive.INT:
                return simple_adapter(env0, cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))), (lambda _cx, jv: _hoist_simple_adapter_body_3(jv)), (lambda cx1, t: hydra.lib.eithers.map((lambda i: cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.int32_to_bigint(i))))), hydra.extract.core.int32(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), t))))

            case hydra.avro.schema.Primitive.LONG:
                return simple_adapter(env0, cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT64)))), (lambda _cx, jv: _hoist_simple_adapter_body_4(jv)), (lambda cx1, t: hydra.lib.eithers.map((lambda i: cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.int64_to_bigint(i))))), hydra.extract.core.int64(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), t))))

            case hydra.avro.schema.Primitive.FLOAT:
                return simple_adapter(env0, cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT32)))), (lambda _cx, jv: _hoist_simple_adapter_body_5(jv)), (lambda cx1, t: hydra.lib.eithers.map((lambda f: cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.float32_to_decimal(f)))), hydra.extract.core.float32(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), t))))

            case hydra.avro.schema.Primitive.DOUBLE:
                return simple_adapter(env0, cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(hydra.core.FloatType.FLOAT64)))), (lambda _cx, jv: _hoist_simple_adapter_body_6(jv)), (lambda cx1, t: hydra.lib.eithers.map((lambda d: cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.float64_to_decimal(d)))), hydra.extract.core.float64(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), t))))

            case hydra.avro.schema.Primitive.BYTES:
                return simple_adapter(env0, cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeBinary()))), (lambda _cx, jv: _hoist_simple_adapter_body_7(jv)), (lambda cx1, t: hydra.lib.eithers.map((lambda b: cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.lib.literals.binary_to_string(b)))), hydra.extract.core.binary(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), t))))

            case hydra.avro.schema.Primitive.STRING:
                return simple_adapter(env0, cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))), (lambda _cx, jv: _hoist_simple_adapter_body_8(jv)), (lambda cx1, t: hydra.lib.eithers.map((lambda s: cast(hydra.json.model.Value, hydra.json.model.ValueString(s))), hydra.extract.core.string(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), t))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match schema:
        case hydra.avro.schema.SchemaArray(value=arr):
            return hydra.lib.eithers.bind(avro_hydra_adapter(cx, arr.items, env0), (lambda ad_env: (ad := hydra.lib.pairs.first(ad_env), env1 := hydra.lib.pairs.second(ad_env), _hoist_ad_body_1 := (lambda cx1, v1: (lambda vals: hydra.lib.eithers.map((lambda ts: cast(hydra.core.Term, hydra.core.TermList(ts))), hydra.lib.eithers.map_list((lambda jv: ad.coder.encode(cx1, jv)), vals)))(v1.value) if isinstance(v1, hydra.json.model.ValueArray) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), _hoist_ad_body_2 := (lambda cx1, v1: (lambda vals: hydra.lib.eithers.map((lambda jvs: cast(hydra.json.model.Value, hydra.json.model.ValueArray(jvs))), hydra.lib.eithers.map_list((lambda tv: ad.coder.decode(cx1, tv)), vals)))(v1.value) if isinstance(v1, hydra.core.TermList) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), Right((hydra.coders.Adapter(ad.is_lossy, schema, cast(hydra.core.Type, hydra.core.TypeList(ad.target)), hydra.coders.Coder((lambda cx1, v: _hoist_ad_body_1(cx1, v)), (lambda cx1, t: _hoist_ad_body_2(cx1, t)))), env1)))[4]))

        case hydra.avro.schema.SchemaMap(value=mp):
            return hydra.lib.eithers.bind(avro_hydra_adapter(cx, mp.values, env0), (lambda ad_env: (ad := hydra.lib.pairs.first(ad_env), env1 := hydra.lib.pairs.second(ad_env), pair_to_hydra := (lambda cx1, entry: (k := hydra.lib.pairs.first(entry), v := hydra.lib.pairs.second(entry), hydra.lib.eithers.map((lambda v_: (cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(k)))), v_)), ad.coder.encode(cx1, v)))[2]), _hoist_ad_body_1 := (lambda cx1, v1: (lambda m: hydra.lib.eithers.map((lambda pairs: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(pairs)))), hydra.lib.eithers.map_list((lambda e: pair_to_hydra(cx1, e)), hydra.lib.maps.to_list(m))))(v1.value) if isinstance(v1, hydra.json.model.ValueObject) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), Right((hydra.coders.Adapter(ad.is_lossy, schema, cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))), ad.target))), hydra.coders.Coder((lambda cx1, v: _hoist_ad_body_1(cx1, v)), (lambda cx1, m: hydra.lib.eithers.map((lambda mp_: cast(hydra.json.model.Value, hydra.json.model.ValueObject(mp_))), hydra.extract.core.map((lambda t: hydra.extract.core.string(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), t)), (lambda t: ad.coder.decode(cx1, t)), hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), m))))), env1)))[4]))

        case hydra.avro.schema.SchemaNamed(value=n):
            ns = n.namespace
            @lru_cache(1)
            def manns() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                return named_annotations_to_core(n)
            @lru_cache(1)
            def ann() -> Maybe[FrozenDict[hydra.core.Name, hydra.core.Term]]:
                return hydra.lib.logic.if_else(hydra.lib.maps.null(manns()), (lambda : Nothing()), (lambda : Just(manns())))
            last_ns = env0.namespace
            @lru_cache(1)
            def next_ns() -> Maybe[str]:
                return hydra.lib.maybes.maybe((lambda : last_ns), (lambda s: Just(s)), ns)
            env1 = hydra.avro.environment.AvroEnvironment(env0.named_adapters, next_ns(), env0.elements)
            qname = hydra.avro.environment.AvroQualifiedName(next_ns(), n.name)
            @lru_cache(1)
            def hydra_name() -> hydra.core.Name:
                return avro_name_to_hydra_name(qname)
            def _hoist_ns_body_1(v1):
                match v1:
                    case hydra.json.model.ValueString(value=s):
                        return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBinary(hydra.lib.literals.string_to_binary(s))))))

                    case _:
                        raise TypeError("Unsupported Value")
            def _hoist_ns_body_2(v1):
                match v1:
                    case hydra.avro.schema.NamedTypeEnum(value=e):
                        syms = e.symbols
                        @lru_cache(1)
                        def typ() -> hydra.core.Type:
                            return cast(hydra.core.Type, hydra.core.TypeUnion(hydra.lib.lists.map((lambda s: hydra.core.FieldType(hydra.core.Name(s), cast(hydra.core.Type, hydra.core.TypeUnit()))), syms)))
                        def _hoist_syms_body_1(v12):
                            match v12:
                                case hydra.json.model.ValueString(value=s):
                                    return Right(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra_name(), hydra.core.Field(hydra.core.Name(s), cast(hydra.core.Term, hydra.core.TermUnit()))))))

                                case _:
                                    raise TypeError("Unsupported Value")
                        def _hoist_syms_body_2(v12):
                            match v12:
                                case hydra.core.TermInject(value=inj):
                                    fld = inj.field
                                    fn = fld.name
                                    return Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(fn.value)))

                                case _:
                                    raise TypeError("Unsupported Term")
                        return simple_adapter(env1, typ(), (lambda _cx, jv: _hoist_syms_body_1(jv)), (lambda _cx, t: _hoist_syms_body_2(t)))

                    case hydra.avro.schema.NamedTypeFixed():
                        return simple_adapter(env1, cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeBinary()))), (lambda _cx, jv: _hoist_ns_body_1(jv)), (lambda cx1, t: hydra.lib.eithers.map((lambda b: cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.lib.literals.binary_to_string(b)))), hydra.extract.core.binary(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), t))))

                    case hydra.avro.schema.NamedTypeRecord(value=r):
                        avro_fields = r.fields
                        return hydra.lib.eithers.bind(prepare_fields(cx, env1, avro_fields), (lambda prep_result: (adapters_by_field_name := hydra.lib.pairs.first(prep_result), env2 := hydra.lib.pairs.second(prep_result), hydra.lib.eithers.bind(find_avro_primary_key_field(cx, qname, avro_fields), (lambda pk: (encode_pair := (lambda cx1, entry: (k := hydra.lib.pairs.first(entry), v := hydra.lib.pairs.second(entry), hydra.lib.maybes.maybe((lambda : err(cx1, hydra.lib.strings.cat(("unrecognized field for ", show_qname(qname), ": ", k)))), (lambda fad: hydra.lib.eithers.map((lambda v_: hydra.core.Field(hydra.core.Name(k), v_)), hydra.lib.pairs.second(fad).coder.encode(cx1, v))), hydra.lib.maps.lookup(k, adapters_by_field_name)))[2]), decode_field := (lambda cx1, fld: (k := fld.name.value, v := fld.term, hydra.lib.maybes.maybe((lambda : err(cx1, hydra.lib.strings.cat(("unrecognized field for ", show_qname(qname), ": ", k)))), (lambda fad: hydra.lib.eithers.map((lambda v_: (k, v_)), hydra.lib.pairs.second(fad).coder.decode(cx1, v))), hydra.lib.maps.lookup(k, adapters_by_field_name)))[2]), lossy := hydra.lib.lists.foldl((lambda b, fad: hydra.lib.logic.or_(b, hydra.lib.pairs.second(fad).is_lossy)), False, hydra.lib.maps.elems(adapters_by_field_name)), hfields := hydra.lib.lists.map((lambda fad: hydra.core.FieldType(hydra.core.Name(hydra.lib.pairs.first(fad).name), hydra.lib.pairs.second(fad).target)), hydra.lib.maps.elems(adapters_by_field_name)), target := cast(hydra.core.Type, hydra.core.TypeRecord(hfields)), _hoist_encode_pair_body_1 := (lambda cx1, v12: (lambda m: hydra.lib.eithers.map((lambda fields: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra_name(), fields)))), hydra.lib.eithers.map_list((lambda e: encode_pair(cx1, e)), hydra.lib.maps.to_list(m))))(v12.value) if isinstance(v12, hydra.json.model.ValueObject) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), _hoist_encode_pair_body_2 := (lambda cx1, v12: (lambda rec: hydra.lib.eithers.map((lambda kvs: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(kvs)))), hydra.lib.eithers.map_list((lambda fld: decode_field(cx1, fld)), rec.fields)))(v12.value) if isinstance(v12, hydra.core.TermRecord) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), Right((hydra.coders.Adapter(lossy, schema, target, hydra.coders.Coder((lambda cx1, jv: _hoist_encode_pair_body_1(cx1, jv)), (lambda cx1, t: _hoist_encode_pair_body_2(cx1, t)))), env2)))[7])))[2]))

                    case _:
                        raise AssertionError("Unreachable: all variants handled")
            return hydra.lib.maybes.maybe((lambda : hydra.lib.eithers.bind(_hoist_ns_body_2(n.type), (lambda ad_env2: (ad := hydra.lib.pairs.first(ad_env2), env2 := hydra.lib.pairs.second(ad_env2), env3 := put_avro_hydra_adapter(qname, ad, env2), env4 := hydra.avro.environment.AvroEnvironment(env3.named_adapters, last_ns, env3.elements), Right((annotate_adapter(ann(), ad), env4)))[4]))), (lambda _ad: err(cx, hydra.lib.strings.cat2("Avro named type defined more than once: ", show_qname(qname)))), get_avro_hydra_adapter(qname, env1))

        case hydra.avro.schema.SchemaPrimitive(value=p):
            return _hoist_simple_adapter_body_9(p)

        case hydra.avro.schema.SchemaReference(value=name_):
            @lru_cache(1)
            def qname() -> hydra.avro.environment.AvroQualifiedName:
                return parse_avro_name(env0.namespace, name_)
            return hydra.lib.maybes.maybe((lambda : err(cx, hydra.lib.strings.cat2("Referenced Avro type has not been defined: ", show_qname(qname())))), (lambda ad: Right((ad, env0))), get_avro_hydra_adapter(qname(), env0))

        case hydra.avro.schema.SchemaUnion(value=u):
            schemas = u.value
            def is_null(s: hydra.avro.schema.Schema):
                def _hoist_is_null_1(v1):
                    match v1:
                        case hydra.avro.schema.Primitive.NULL:
                            return True

                        case _:
                            return False
                def _hoist_is_null_2(v1):
                    match v1:
                        case hydra.avro.schema.SchemaPrimitive(value=prim):
                            return _hoist_is_null_1(prim)

                        case _:
                            return False
                return _hoist_is_null_2(s)
            @lru_cache(1)
            def has_null() -> bool:
                return hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.lists.filter((lambda x1: is_null(x1)), schemas)))
            @lru_cache(1)
            def non_nulls() -> frozenlist[hydra.avro.schema.Schema]:
                return hydra.lib.lists.filter((lambda s: hydra.lib.logic.not_(is_null(s))), schemas)
            def for_optional(s: hydra.avro.schema.Schema):
                return hydra.lib.eithers.bind(avro_hydra_adapter(cx, s, env0), (lambda ad_env: (ad := hydra.lib.pairs.first(ad_env), env1 := hydra.lib.pairs.second(ad_env), _hoist_ad_body_1 := (lambda cx1, v, v1: (lambda _: Right(cast(hydra.core.Term, hydra.core.TermMaybe(Nothing()))))(v1) if isinstance(v1, hydra.json.model.ValueNull) else hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermMaybe(Just(t)))), ad.coder.encode(cx1, v))), _hoist_ad_body_2 := (lambda cx1, v1: (lambda ot: hydra.lib.maybes.maybe((lambda : Right(cast(hydra.json.model.Value, hydra.json.model.ValueNull()))), (lambda term_: ad.coder.decode(cx1, term_)), ot))(v1.value) if isinstance(v1, hydra.core.TermMaybe) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), Right((hydra.coders.Adapter(ad.is_lossy, schema, cast(hydra.core.Type, hydra.core.TypeMaybe(ad.target)), hydra.coders.Coder((lambda cx1, v: _hoist_ad_body_1(cx1, v, v)), (lambda cx1, t: _hoist_ad_body_2(cx1, t)))), env1)))[4]))
            return hydra.lib.logic.if_else(hydra.lib.equality.gt(hydra.lib.lists.length(non_nulls()), 1), (lambda : err(cx, "general-purpose unions are not yet supported")), (lambda : hydra.lib.maybes.maybe((lambda : err(cx, "cannot generate the empty type")), (lambda non_null_head: hydra.lib.logic.if_else(has_null(), (lambda : for_optional(non_null_head)), (lambda : hydra.lib.eithers.bind(avro_hydra_adapter(cx, non_null_head, env0), (lambda ad_env: (ad := hydra.lib.pairs.first(ad_env), env1 := hydra.lib.pairs.second(ad_env), Right((hydra.coders.Adapter(ad.is_lossy, schema, ad.target, ad.coder), env1)))[2]))))), hydra.lib.lists.maybe_head(non_nulls()))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def prepare_field(cx: T0, env: hydra.avro.environment.AvroEnvironment, f: hydra.avro.schema.Field):
    r"""Prepare a single field, producing an adapter and updated environment."""

    @lru_cache(1)
    def manns() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return field_annotations_to_core(f)
    @lru_cache(1)
    def ann() -> Maybe[FrozenDict[hydra.core.Name, hydra.core.Term]]:
        return hydra.lib.logic.if_else(hydra.lib.maps.null(manns()), (lambda : Nothing()), (lambda : Just(manns())))
    return hydra.lib.eithers.bind(foreign_key_e(cx, f), (lambda fk: hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : avro_hydra_adapter(cx, f.type, env)), (lambda fk_val: (fk_name := fk_val.type_name, fk_constr := fk_val.constructor, hydra.lib.eithers.bind(avro_hydra_adapter(cx, f.type, env), (lambda ad_env_pair: (ad0 := hydra.lib.pairs.first(ad_env_pair), env1 := hydra.lib.pairs.second(ad_env_pair), el_typ := cast(hydra.core.Type, hydra.core.TypeVariable(fk_name)), encode_value := (lambda cx1, v: hydra.lib.eithers.bind(ad0.coder.encode(cx1, v), (lambda encoded: hydra.lib.eithers.bind(term_to_string_e(cx1, encoded), (lambda s: Right(cast(hydra.core.Term, hydra.core.TermVariable(fk_constr(s))))))))), decode_term := (lambda cx1, t: (_hoist_decode_term_1 := (lambda cx1, v1: (lambda name_: hydra.lib.eithers.bind(string_to_term_e(cx1, ad0.target, name_.value), (lambda term_: ad0.coder.decode(cx1, term_))))(v1.value) if isinstance(v1, hydra.core.TermVariable) else err(cx1, "expected variable")), _hoist_decode_term_1(cx1, t))[1]), for_type_and_coder := (lambda env2, ad1, typ, cdr: Right((hydra.coders.Adapter(ad1.is_lossy, f.type, typ, cdr), env2))), _hoist_ad0_body_1 := (lambda v1: (lambda _: for_type_and_coder(env1, ad0, cast(hydra.core.Type, hydra.core.TypeMaybe(el_typ)), hydra.coders.Coder((lambda cx2, json: hydra.lib.eithers.map((lambda v_: cast(hydra.core.Term, hydra.core.TermMaybe(Just(v_)))), encode_value(cx2, json))), (lambda x1, x2: decode_term(x1, x2)))))(v1.value) if isinstance(v1, hydra.core.TypeLiteral) else err(cx, "expected literal type inside optional foreign key")), _hoist_ad0_body_2 := (lambda cx2, v1: (lambda vals: hydra.lib.eithers.map((lambda terms: cast(hydra.core.Term, hydra.core.TermList(terms))), hydra.lib.eithers.map_list((lambda jv: encode_value(cx2, jv)), vals)))(v1.value) if isinstance(v1, hydra.json.model.ValueArray) else err(cx2, "Expected JSON array")), _hoist_ad0_body_3 := (lambda v1: (lambda _: for_type_and_coder(env1, ad0, cast(hydra.core.Type, hydra.core.TypeList(el_typ)), hydra.coders.Coder((lambda cx2, json: _hoist_ad0_body_2(cx2, json)), (lambda x1, x2: decode_term(x1, x2)))))(v1.value) if isinstance(v1, hydra.core.TypeLiteral) else err(cx, "expected literal type inside list foreign key")), _hoist_ad0_body_4 := (lambda v1: (lambda inner_typ: _hoist_ad0_body_1(inner_typ))(v1.value) if isinstance(v1, hydra.core.TypeMaybe) else (lambda inner_typ2: _hoist_ad0_body_3(inner_typ2))(v1.value) if isinstance(v1, hydra.core.TypeList) else (lambda _: for_type_and_coder(env1, ad0, el_typ, hydra.coders.Coder((lambda x1, x2: encode_value(x1, x2)), (lambda x1, x2: decode_term(x1, x2)))))(v1.value) if isinstance(v1, hydra.core.TypeLiteral) else err(cx, hydra.lib.strings.cat2("unsupported type annotated as foreign key: ", "unknown"))), _hoist_ad0_body_4(hydra.strip.deannotate_type(ad0.target)))[10])))[2]), fk), (lambda ad_env: (ad := hydra.lib.pairs.first(ad_env), env1 := hydra.lib.pairs.second(ad_env), Right(((f.name, (f, annotate_adapter(ann(), ad))), env1)))[2]))))

def prepare_fields(cx: T0, env: hydra.avro.environment.AvroEnvironment, fields: frozenlist[hydra.avro.schema.Field]) -> Either[hydra.errors.Error, tuple[FrozenDict[str, tuple[hydra.avro.schema.Field, hydra.coders.Adapter[hydra.avro.schema.Schema, hydra.core.Type, hydra.json.model.Value, hydra.core.Term]]], hydra.avro.environment.AvroEnvironment]]:
    r"""Thread AvroEnvironment through preparing multiple fields."""

    return hydra.lib.lists.foldl((lambda acc, f: hydra.lib.eithers.bind(acc, (lambda acc_pair: (m := hydra.lib.pairs.first(acc_pair), env1 := hydra.lib.pairs.second(acc_pair), hydra.lib.eithers.bind(prepare_field(cx, env1, f), (lambda result: (kv := hydra.lib.pairs.first(result), env2 := hydra.lib.pairs.second(result), k := hydra.lib.pairs.first(kv), v := hydra.lib.pairs.second(kv), Right((hydra.lib.maps.insert(k, v, m), env2)))[4])))[2]))), Right((hydra.lib.maps.empty(), env)), fields)

@lru_cache(1)
def empty_avro_environment() -> hydra.avro.environment.AvroEnvironment:
    r"""An empty Avro environment with no named adapters, no namespace, and no elements."""

    return hydra.avro.environment.AvroEnvironment(hydra.lib.maps.empty(), Nothing(), hydra.lib.maps.empty())

def expect_array_e(cx: T0, value: hydra.json.model.Value) -> Either[T1, frozenlist[hydra.json.model.Value]]:
    r"""Extract a JSON array or return an error."""

    match value:
        case hydra.json.model.ValueArray(value=v):
            return Right(v)

        case _:
            raise TypeError("Unsupported Value")

def json_to_string_e(cx: T0, v: hydra.json.model.Value) -> Either[hydra.errors.Error, str]:
    r"""Convert a JSON value to a string, supporting booleans, strings, and numbers."""

    match v:
        case hydra.json.model.ValueBoolean(value=b):
            return Right(hydra.lib.logic.if_else(b, (lambda : "true"), (lambda : "false")))

        case hydra.json.model.ValueString(value=s):
            return Right(s)

        case hydra.json.model.ValueNumber(value=d):
            return Right(hydra.lib.literals.show_decimal(d))

        case _:
            return unexpected_e(cx, "string, number, or boolean", "other")

def rewrite_avro_schema_m(f: Callable[[
  Callable[[hydra.avro.schema.Schema], Either[T0, hydra.avro.schema.Schema]],
  hydra.avro.schema.Schema], Either[T0, hydra.avro.schema.Schema]], schema: hydra.avro.schema.Schema) -> Either[T0, hydra.avro.schema.Schema]:
    r"""Recursively rewrite an Avro schema using a monadic transformation function."""

    def recurse(v1: hydra.avro.schema.Schema) -> Either[T0, hydra.avro.schema.Schema]:
        return rewrite_avro_schema_m(f, v1)
    def fsub(s: hydra.avro.schema.Schema):
        def _hoist_fsub_1(n, v1):
            match v1:
                case hydra.avro.schema.NamedTypeRecord(value=r):
                    return hydra.lib.eithers.map((lambda fields_: cast(hydra.avro.schema.NamedType, hydra.avro.schema.NamedTypeRecord(hydra.avro.schema.Record(fields_)))), hydra.lib.eithers.map_list((lambda fld: hydra.lib.eithers.map((lambda t_: hydra.avro.schema.Field(fld.name, fld.doc, t_, fld.default, fld.order, fld.aliases, fld.annotations)), recurse(fld.type))), r.fields))

                case _:
                    return Right(n.type)
        match s:
            case hydra.avro.schema.SchemaArray(value=arr):
                return hydra.lib.eithers.map((lambda els_: cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaArray(hydra.avro.schema.Array(els_)))), recurse(arr.items))

            case hydra.avro.schema.SchemaMap(value=mp):
                return hydra.lib.eithers.map((lambda vs_: cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaMap(hydra.avro.schema.Map(vs_)))), recurse(mp.values))

            case hydra.avro.schema.SchemaNamed(value=n):
                return hydra.lib.eithers.map((lambda nt_: cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaNamed(hydra.avro.schema.Named(n.name, n.namespace, n.aliases, n.doc, nt_, n.annotations)))), _hoist_fsub_1(n, n.type))

            case hydra.avro.schema.SchemaUnion(value=u):
                return hydra.lib.eithers.map((lambda schemas_: cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaUnion(hydra.avro.schema.Union(schemas_)))), hydra.lib.eithers.map_list((lambda us: recurse(us)), u.value))

            case _:
                return Right(s)
    return f((lambda x1: fsub(x1)), schema)
