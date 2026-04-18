# Note: this is an automatically generated file. Do not edit.

r"""Hydra-to-Avro encoder: converts Hydra types and terms to Avro schemas and JSON values."""

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
import hydra.strip

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")
T4 = TypeVar("T4")
T5 = TypeVar("T5")
T6 = TypeVar("T6")

def local_name(name_: hydra.core.Name) -> str:
    r"""Extract the local part of a qualified name."""

    s = name_.value
    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", s)
    return hydra.lib.maybes.from_maybe((lambda : s), hydra.lib.lists.maybe_last(parts()))

def build_avro_field(name_ad: tuple[hydra.core.Name, hydra.coders.Adapter[T0, hydra.avro.schema.Schema, T1, T2]]) -> hydra.avro.schema.Field:
    r"""Build an Avro field from a name-adapter pair."""

    @lru_cache(1)
    def name_() -> hydra.core.Name:
        return hydra.lib.pairs.first(name_ad)
    @lru_cache(1)
    def ad() -> hydra.coders.Adapter[T0, hydra.avro.schema.Schema, T1, T2]:
        return hydra.lib.pairs.second(name_ad)
    return hydra.avro.schema.Field(local_name(name_()), Nothing(), ad().target, Nothing(), Nothing(), Nothing(), hydra.lib.maps.empty())

def empty_encode_environment(type_map: FrozenDict[hydra.core.Name, hydra.core.Type]) -> hydra.avro.environment.EncodeEnvironment:
    r"""Create an empty encode environment with the given type map."""

    return hydra.avro.environment.EncodeEnvironment(type_map, hydra.lib.maps.empty())

def err(cx: T0, msg: str) -> Either[hydra.errors.Error, T1]:
    r"""Construct an error result with a message in context."""

    return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(msg))))

def float_value_to_double(fv: hydra.core.FloatValue) -> Decimal:
    r"""Convert any float value to a JSON decimal number."""

    match fv:
        case hydra.core.FloatValueBigfloat(value=d):
            return hydra.lib.literals.float64_to_decimal(hydra.lib.literals.bigfloat_to_float64(d))

        case hydra.core.FloatValueFloat32(value=f):
            return hydra.lib.literals.float32_to_decimal(f)

        case hydra.core.FloatValueFloat64(value=d2):
            return hydra.lib.literals.float64_to_decimal(d2)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def integer_value_to_double(iv: hydra.core.IntegerValue) -> Decimal:
    r"""Convert any integer value to a JSON decimal number."""

    match iv:
        case hydra.core.IntegerValueBigint(value=i):
            return hydra.lib.literals.bigint_to_decimal(i)

        case hydra.core.IntegerValueInt8(value=i2):
            return hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.int8_to_bigint(i2))

        case hydra.core.IntegerValueInt16(value=i3):
            return hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.int16_to_bigint(i3))

        case hydra.core.IntegerValueInt32(value=i4):
            return hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.int32_to_bigint(i4))

        case hydra.core.IntegerValueInt64(value=i5):
            return hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.int64_to_bigint(i5))

        case hydra.core.IntegerValueUint8(value=i6):
            return hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.uint8_to_bigint(i6))

        case hydra.core.IntegerValueUint16(value=i7):
            return hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.uint16_to_bigint(i7))

        case hydra.core.IntegerValueUint32(value=i8):
            return hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.uint32_to_bigint(i8))

        case hydra.core.IntegerValueUint64(value=i9):
            return hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.uint64_to_bigint(i9))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def term_to_json_value(term: hydra.core.Term):
    def _hoist_hydra_avro_encoder_term_to_json_value_1(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return cast(hydra.json.model.Value, hydra.json.model.ValueString(s))

            case hydra.core.LiteralBoolean(value=b):
                return cast(hydra.json.model.Value, hydra.json.model.ValueBoolean(b))

            case hydra.core.LiteralInteger(value=iv):
                return cast(hydra.json.model.Value, hydra.json.model.ValueNumber(integer_value_to_double(iv)))

            case hydra.core.LiteralFloat(value=fv):
                return cast(hydra.json.model.Value, hydra.json.model.ValueNumber(float_value_to_double(fv)))

            case hydra.core.LiteralBinary(value=b):
                return cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.lib.literals.binary_to_string(b)))

            case _:
                raise TypeError("Unsupported Literal")
    match term:
        case hydra.core.TermLiteral(value=lit):
            return _hoist_hydra_avro_encoder_term_to_json_value_1(lit)

        case hydra.core.TermList(value=ts):
            return cast(hydra.json.model.Value, hydra.json.model.ValueArray(hydra.lib.lists.map((lambda x1: term_to_json_value(x1)), ts)))

        case hydra.core.TermMap(value=m):
            return cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda entry: (k := hydra.lib.pairs.first(entry), v := hydra.lib.pairs.second(entry), _hoist_k_body_1 := (lambda v1: (lambda s: s)(v1.value) if isinstance(v1, hydra.core.LiteralString) else "<key>"), _hoist_k_body_2 := (lambda v1: (lambda kl: _hoist_k_body_1(kl))(v1.value) if isinstance(v1, hydra.core.TermLiteral) else "<key>"), (_hoist_k_body_2(k), term_to_json_value(v)))[4]), hydra.lib.maps.to_list(m)))))

        case hydra.core.TermRecord(value=rec):
            return hydra.lib.logic.if_else(hydra.lib.lists.null(rec.fields), (lambda : cast(hydra.json.model.Value, hydra.json.model.ValueNull())), (lambda : cast(hydra.json.model.Value, hydra.json.model.ValueString("<record>"))))

        case _:
            return cast(hydra.json.model.Value, hydra.json.model.ValueString("<term>"))

def hydra_annotations_to_avro(anns: FrozenDict[hydra.core.Name, hydra.core.Term]) -> FrozenDict[str, hydra.json.model.Value]:
    r"""Convert Hydra annotations to Avro annotation map."""

    return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda entry: (k := hydra.lib.pairs.first(entry), v := hydra.lib.pairs.second(entry), (k.value, term_to_json_value(v)))[2]), hydra.lib.maps.to_list(anns)))

def name_namespace(name_: hydra.core.Name) -> Maybe[str]:
    r"""Extract the namespace from a qualified name, if any."""

    s = name_.value
    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.strings.split_on(".", s)
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(parts()), 1), (lambda : Nothing()), (lambda : hydra.lib.maybes.map((lambda ps: hydra.lib.strings.intercalate(".", ps)), hydra.lib.lists.maybe_init(parts()))))

def type_to_name(t: hydra.core.Type) -> hydra.core.Name:
    r"""Generate a default name for an anonymous type."""

    match hydra.strip.deannotate_type(t):
        case hydra.core.TypeRecord():
            return hydra.core.Name("Record")

        case hydra.core.TypeUnion():
            return hydra.core.Name("Union")

        case _:
            return hydra.core.Name("Unknown")

def enum_adapter(cx: T0, typ: hydra.core.Type, m_name: Maybe[hydra.core.Name], annotations: FrozenDict[hydra.core.Name, hydra.core.Term], field_types: frozenlist[hydra.core.FieldType], env0: hydra.avro.environment.EncodeEnvironment) -> Either[T1, tuple[hydra.coders.Adapter[hydra.core.Type, hydra.avro.schema.Schema, hydra.core.Term, hydra.json.model.Value], hydra.avro.environment.EncodeEnvironment]]:
    r"""Adapter for all-unit union types (enums)."""

    @lru_cache(1)
    def symbols() -> frozenlist[str]:
        return hydra.lib.lists.map((lambda ft: local_name(ft.name)), field_types)
    @lru_cache(1)
    def type_name() -> hydra.core.Name:
        return hydra.lib.maybes.from_maybe((lambda : type_to_name(typ)), m_name)
    @lru_cache(1)
    def avro_annotations() -> FrozenDict[str, hydra.json.model.Value]:
        return hydra_annotations_to_avro(annotations)
    @lru_cache(1)
    def avro_schema() -> hydra.avro.schema.Schema:
        return cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaNamed(hydra.avro.schema.Named(local_name(type_name()), name_namespace(type_name()), Nothing(), Nothing(), cast(hydra.avro.schema.NamedType, hydra.avro.schema.NamedTypeEnum(hydra.avro.schema.Enum(symbols(), Nothing()))), avro_annotations())))
    @lru_cache(1)
    def adapter_():
        def _hoist_adapter_1(cx1, v1):
            match v1:
                case hydra.core.TermInject(value=inj):
                    fname = inj.field
                    return Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(local_name(fname.name))))

                case _:
                    return err(cx1, "expected union term for enum")
        def _hoist_adapter_2(v1):
            match v1:
                case hydra.json.model.ValueString(value=s):
                    return Right(cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(type_name(), hydra.core.Field(hydra.core.Name(s), cast(hydra.core.Term, hydra.core.TermUnit()))))))

                case _:
                    raise TypeError("Unsupported Value")
        return hydra.coders.Adapter(False, typ, avro_schema(), hydra.coders.Coder((lambda cx1, t: _hoist_adapter_1(cx1, t)), (lambda _cx, j: _hoist_adapter_2(j))))
    @lru_cache(1)
    def env1() -> hydra.avro.environment.EncodeEnvironment:
        return hydra.avro.environment.EncodeEnvironment(env0.type_map, hydra.lib.maps.insert(type_name(), adapter_(), env0.emitted))
    return Right((adapter_(), env1()))

def extract_annotations(typ: hydra.core.Type) -> tuple[FrozenDict[hydra.core.Name, hydra.core.Term], hydra.core.Type]:
    r"""Extract annotations from a potentially annotated type."""

    match typ:
        case hydra.core.TypeAnnotated(value=at):
            inner = at.body
            anns = at.annotation
            @lru_cache(1)
            def inner_result() -> tuple[FrozenDict[hydra.core.Name, hydra.core.Term], hydra.core.Type]:
                return extract_annotations(inner)
            @lru_cache(1)
            def inner_anns() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                return hydra.lib.pairs.first(inner_result())
            @lru_cache(1)
            def bare_type() -> hydra.core.Type:
                return hydra.lib.pairs.second(inner_result())
            return (hydra.lib.maps.union(anns, inner_anns()), bare_type())

        case _:
            return (hydra.lib.maps.empty(), typ)

def float_adapter(cx: T0, typ: T1, ft: hydra.core.FloatType):
    r"""Create an adapter for float types."""

    def simple(target: T3, lossy: bool, encode: Callable[[hydra.context.Context, T4], Either[hydra.errors.Error, T5]], decode: Callable[[hydra.context.Context, T5], Either[hydra.errors.Error, T4]]) -> Either[T6, hydra.coders.Adapter[T1, T3, T4, T5]]:
        return Right(hydra.coders.Adapter(lossy, typ, target, hydra.coders.Coder(encode, decode)))
    def _hoist_simple_body_1(v1):
        match v1:
            case hydra.core.LiteralFloat(value=fv):
                return Right(cast(hydra.json.model.Value, hydra.json.model.ValueNumber(float_value_to_double(fv))))

            case _:
                raise TypeError("Unsupported Literal")
    def _hoist_simple_body_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=lit):
                return _hoist_simple_body_1(lit)

            case _:
                raise TypeError("Unsupported Term")
    def _hoist_simple_body_3(v1):
        match v1:
            case hydra.json.model.ValueNumber(value=d):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat64(hydra.lib.literals.decimal_to_float64(d))))))))

            case _:
                raise TypeError("Unsupported Value")
    def _hoist_simple_body_4(v1):
        match v1:
            case hydra.json.model.ValueNumber(value=d):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat32(hydra.lib.literals.decimal_to_float32(d))))))))

            case _:
                raise TypeError("Unsupported Value")
    def _hoist_simple_body_5(v1):
        match v1:
            case hydra.json.model.ValueNumber(value=d):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat64(hydra.lib.literals.decimal_to_float64(d))))))))

            case _:
                raise TypeError("Unsupported Value")
    match ft:
        case hydra.core.FloatType.FLOAT32:
            return simple(cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaPrimitive(hydra.avro.schema.Primitive.FLOAT)), False, (lambda _cx, t: hydra.lib.eithers.map((lambda f: cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.float32_to_decimal(f)))), hydra.extract.core.float32(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), t))), (lambda _cx, j: _hoist_simple_body_4(j)))

        case hydra.core.FloatType.FLOAT64:
            return simple(cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaPrimitive(hydra.avro.schema.Primitive.DOUBLE)), False, (lambda _cx, t: hydra.lib.eithers.map((lambda d: cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.float64_to_decimal(d)))), hydra.extract.core.float64(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), t))), (lambda _cx, j: _hoist_simple_body_5(j)))

        case _:
            return simple(cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaPrimitive(hydra.avro.schema.Primitive.DOUBLE)), True, (lambda _cx, t: _hoist_simple_body_2(t)), (lambda _cx, j: _hoist_simple_body_3(j)))

def integer_adapter(cx: T0, typ: T1, it: hydra.core.IntegerType):
    r"""Create an adapter for integer types."""

    def simple(target: T3, lossy: bool, encode: Callable[[hydra.context.Context, T4], Either[hydra.errors.Error, T5]], decode: Callable[[hydra.context.Context, T5], Either[hydra.errors.Error, T4]]) -> Either[T6, hydra.coders.Adapter[T1, T3, T4, T5]]:
        return Right(hydra.coders.Adapter(lossy, typ, target, hydra.coders.Coder(encode, decode)))
    def _hoist_simple_body_1(v1):
        match v1:
            case hydra.core.LiteralInteger(value=iv):
                return Right(cast(hydra.json.model.Value, hydra.json.model.ValueNumber(integer_value_to_double(iv))))

            case _:
                raise TypeError("Unsupported Literal")
    def _hoist_simple_body_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=lit):
                return _hoist_simple_body_1(lit)

            case _:
                raise TypeError("Unsupported Term")
    def _hoist_simple_body_3(v1):
        match v1:
            case hydra.json.model.ValueNumber(value=d):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(hydra.lib.literals.bigint_to_int64(hydra.lib.literals.decimal_to_bigint(d)))))))))

            case _:
                raise TypeError("Unsupported Value")
    def _hoist_simple_body_4(v1):
        match v1:
            case hydra.json.model.ValueNumber(value=d):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(hydra.lib.literals.bigint_to_int32(hydra.lib.literals.decimal_to_bigint(d)))))))))

            case _:
                raise TypeError("Unsupported Value")
    def _hoist_simple_body_5(v1):
        match v1:
            case hydra.json.model.ValueNumber(value=d):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(hydra.lib.literals.bigint_to_int64(hydra.lib.literals.decimal_to_bigint(d)))))))))

            case _:
                raise TypeError("Unsupported Value")
    match it:
        case hydra.core.IntegerType.INT32:
            return simple(cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaPrimitive(hydra.avro.schema.Primitive.INT)), False, (lambda _cx, t: hydra.lib.eithers.map((lambda i: cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.int32_to_bigint(i))))), hydra.extract.core.int32(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), t))), (lambda _cx, j: _hoist_simple_body_4(j)))

        case hydra.core.IntegerType.INT64:
            return simple(cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaPrimitive(hydra.avro.schema.Primitive.LONG)), False, (lambda _cx, t: hydra.lib.eithers.map((lambda i: cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_decimal(hydra.lib.literals.int64_to_bigint(i))))), hydra.extract.core.int64(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), t))), (lambda _cx, j: _hoist_simple_body_5(j)))

        case _:
            return simple(cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaPrimitive(hydra.avro.schema.Primitive.LONG)), True, (lambda _cx, t: _hoist_simple_body_2(t)), (lambda _cx, j: _hoist_simple_body_3(j)))

def literal_adapter(cx: T0, typ: T1, lt: hydra.core.LiteralType):
    r"""Create an adapter for literal types."""

    def simple(target: T3, lossy: bool, encode: Callable[[hydra.context.Context, T4], Either[hydra.errors.Error, T5]], decode: Callable[[hydra.context.Context, T5], Either[hydra.errors.Error, T4]]) -> Either[T6, hydra.coders.Adapter[T1, T3, T4, T5]]:
        return Right(hydra.coders.Adapter(lossy, typ, target, hydra.coders.Coder(encode, decode)))
    def _hoist_simple_body_1(v1):
        match v1:
            case hydra.core.LiteralBoolean(value=b):
                return Right(cast(hydra.json.model.Value, hydra.json.model.ValueBoolean(b)))

            case _:
                raise TypeError("Unsupported Literal")
    def _hoist_simple_body_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=lit):
                return _hoist_simple_body_1(lit)

            case _:
                raise TypeError("Unsupported Term")
    def _hoist_simple_body_3(v1):
        match v1:
            case hydra.json.model.ValueBoolean(value=b):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(b)))))

            case _:
                raise TypeError("Unsupported Value")
    def _hoist_simple_body_4(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(s)))

            case _:
                raise TypeError("Unsupported Literal")
    def _hoist_simple_body_5(v1):
        match v1:
            case hydra.core.TermLiteral(value=lit):
                return _hoist_simple_body_4(lit)

            case _:
                raise TypeError("Unsupported Term")
    def _hoist_simple_body_6(v1):
        match v1:
            case hydra.json.model.ValueString(value=s):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s)))))

            case _:
                raise TypeError("Unsupported Value")
    def _hoist_simple_body_7(v1):
        match v1:
            case hydra.core.LiteralBinary(value=b):
                return Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.lib.literals.binary_to_string(b))))

            case _:
                raise TypeError("Unsupported Literal")
    def _hoist_simple_body_8(v1):
        match v1:
            case hydra.core.TermLiteral(value=lit):
                return _hoist_simple_body_7(lit)

            case _:
                raise TypeError("Unsupported Term")
    def _hoist_simple_body_9(v1):
        match v1:
            case hydra.json.model.ValueString(value=s):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBinary(hydra.lib.literals.string_to_binary(s))))))

            case _:
                raise TypeError("Unsupported Value")
    match lt:
        case hydra.core.LiteralTypeBoolean():
            return simple(cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaPrimitive(hydra.avro.schema.Primitive.BOOLEAN)), False, (lambda _cx, t: _hoist_simple_body_2(t)), (lambda _cx, j: _hoist_simple_body_3(j)))

        case hydra.core.LiteralTypeString():
            return simple(cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaPrimitive(hydra.avro.schema.Primitive.STRING)), False, (lambda _cx, t: _hoist_simple_body_5(t)), (lambda _cx, j: _hoist_simple_body_6(j)))

        case hydra.core.LiteralTypeBinary():
            return simple(cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaPrimitive(hydra.avro.schema.Primitive.BYTES)), False, (lambda _cx, t: _hoist_simple_body_8(t)), (lambda _cx, j: _hoist_simple_body_9(j)))

        case hydra.core.LiteralTypeInteger(value=it):
            return integer_adapter(cx, typ, it)

        case hydra.core.LiteralTypeFloat(value=ft):
            return float_adapter(cx, typ, ft)

        case _:
            raise TypeError("Unsupported LiteralType")

def record_term_coder(cx: T0, type_name: hydra.core.Name, field_adapters: frozenlist[tuple[hydra.core.Name, hydra.coders.Adapter[T1, T2, hydra.core.Term, hydra.json.model.Value]]]) -> tuple[Callable[[hydra.context.Context, hydra.core.Term], Either[hydra.errors.Error, hydra.json.model.Value]], Callable[[hydra.context.Context, hydra.json.model.Value], Either[hydra.errors.Error, hydra.core.Term]]]:
    r"""Build a record term coder from field adapters."""

    def encode(cx1: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.json.model.Value]:
        match term:
            case hydra.core.TermRecord(value=rec):
                fields = rec.fields
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda f: (f.name, f.term)), fields))
                def encode_field(name_ad: tuple[hydra.core.Name, hydra.coders.Adapter[T3, T4, hydra.core.Term, T5]]) -> Either[hydra.errors.Error, tuple[str, T5]]:
                    @lru_cache(1)
                    def fname() -> hydra.core.Name:
                        return hydra.lib.pairs.first(name_ad)
                    @lru_cache(1)
                    def ad() -> hydra.coders.Adapter[T3, T4, hydra.core.Term, T5]:
                        return hydra.lib.pairs.second(name_ad)
                    @lru_cache(1)
                    def f_term() -> hydra.core.Term:
                        return hydra.lib.maybes.from_maybe((lambda : cast(hydra.core.Term, hydra.core.TermUnit())), hydra.lib.maps.lookup(fname(), field_map()))
                    return hydra.lib.eithers.map((lambda jv: (local_name(fname()), jv)), ad().coder.encode(cx1, f_term()))
                return hydra.lib.eithers.map((lambda pairs: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(pairs)))), hydra.lib.eithers.map_list((lambda x1: encode_field(x1)), field_adapters))

            case _:
                return err(cx, "expected record term")
    def decode(cx1: hydra.context.Context, json: hydra.json.model.Value) -> Either[hydra.errors.Error, hydra.core.Term]:
        match json:
            case hydra.json.model.ValueObject(value=m):
                def decode_field(name_ad: tuple[hydra.core.Name, hydra.coders.Adapter[T3, T4, hydra.core.Term, hydra.json.model.Value]]) -> Either[hydra.errors.Error, hydra.core.Field]:
                    @lru_cache(1)
                    def fname() -> hydra.core.Name:
                        return hydra.lib.pairs.first(name_ad)
                    @lru_cache(1)
                    def ad() -> hydra.coders.Adapter[T3, T4, hydra.core.Term, hydra.json.model.Value]:
                        return hydra.lib.pairs.second(name_ad)
                    @lru_cache(1)
                    def jv() -> hydra.json.model.Value:
                        return hydra.lib.maybes.from_maybe((lambda : cast(hydra.json.model.Value, hydra.json.model.ValueNull())), hydra.lib.maps.lookup(local_name(fname()), m))
                    return hydra.lib.eithers.map((lambda t: hydra.core.Field(fname(), t)), ad().coder.decode(cx1, jv()))
                return hydra.lib.eithers.map((lambda fields: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(type_name, fields)))), hydra.lib.eithers.map_list((lambda x1: decode_field(x1)), field_adapters))

            case _:
                return err(cx, "expected JSON object")
    return ((lambda x1, x2: encode(x1, x2)), (lambda x1, x2: decode(x1, x2)))

def encode_type_inner(cx: T0, m_name: Maybe[hydra.core.Name], typ: hydra.core.Type, env: hydra.avro.environment.EncodeEnvironment):
    r"""Core encoding logic: recursively encode a Hydra type to an Avro schema."""

    @lru_cache(1)
    def ann_result() -> tuple[FrozenDict[hydra.core.Name, hydra.core.Term], hydra.core.Type]:
        return extract_annotations(typ)
    @lru_cache(1)
    def annotations() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return hydra.lib.pairs.first(ann_result())
    @lru_cache(1)
    def bare_type() -> hydra.core.Type:
        return hydra.lib.pairs.second(ann_result())
    def simple_adapter(target: T1, lossy: bool, encode: Callable[[hydra.context.Context, T2], Either[hydra.errors.Error, T3]], decode: Callable[[hydra.context.Context, T3], Either[hydra.errors.Error, T2]]) -> Either[T4, tuple[hydra.coders.Adapter[hydra.core.Type, T1, T2, T3], hydra.avro.environment.EncodeEnvironment]]:
        return Right((hydra.coders.Adapter(lossy, typ, target, hydra.coders.Coder(encode, decode)), env))
    match bare_type():
        case hydra.core.TypeUnit():
            return simple_adapter(cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaPrimitive(hydra.avro.schema.Primitive.NULL)), False, (lambda _cx, _t: Right(cast(hydra.json.model.Value, hydra.json.model.ValueNull()))), (lambda _cx, _j: Right(cast(hydra.core.Term, hydra.core.TermUnit()))))

        case hydra.core.TypeLiteral(value=lt):
            return hydra.lib.eithers.map((lambda ad: (ad, env)), literal_adapter(cx, typ, lt))

        case hydra.core.TypeList(value=inner_type):
            return hydra.lib.eithers.bind(encode_type_inner(cx, Nothing(), inner_type, env), (lambda ad_env: (inner_ad := hydra.lib.pairs.first(ad_env), env1 := hydra.lib.pairs.second(ad_env), _hoist_inner_ad_body_1 := (lambda cx1, v1: (lambda elements: hydra.lib.eithers.map((lambda jvs: cast(hydra.json.model.Value, hydra.json.model.ValueArray(jvs))), hydra.lib.eithers.map_list((lambda el: inner_ad.coder.encode(cx1, el)), elements)))(v1.value) if isinstance(v1, hydra.core.TermList) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), _hoist_inner_ad_body_2 := (lambda cx1, v1: (lambda elements: hydra.lib.eithers.map((lambda ts: cast(hydra.core.Term, hydra.core.TermList(ts))), hydra.lib.eithers.map_list((lambda el: inner_ad.coder.decode(cx1, el)), elements)))(v1.value) if isinstance(v1, hydra.json.model.ValueArray) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), Right((hydra.coders.Adapter(inner_ad.is_lossy, typ, cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaArray(hydra.avro.schema.Array(inner_ad.target))), hydra.coders.Coder((lambda cx1, t: _hoist_inner_ad_body_1(cx1, t)), (lambda cx1, j: _hoist_inner_ad_body_2(cx1, j)))), env1)))[4]))

        case hydra.core.TypeMap(value=mt):
            key_type = mt.keys
            val_type = mt.values
            def _hoist_key_type_body_1(v1):
                match v1:
                    case hydra.core.LiteralTypeString():
                        return hydra.lib.eithers.bind(encode_type_inner(cx, Nothing(), val_type, env), (lambda ad_env: (val_ad := hydra.lib.pairs.first(ad_env), env1 := hydra.lib.pairs.second(ad_env), _hoist_val_ad_body_1 := (lambda cx1, v12: (lambda entries: (encode_entry := (lambda entry: (k := hydra.lib.pairs.first(entry), v := hydra.lib.pairs.second(entry), hydra.lib.eithers.bind(hydra.extract.core.string(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), k), (lambda k_str: hydra.lib.eithers.map((lambda v_json: (k_str, v_json)), val_ad.coder.encode(cx1, v)))))[2]), hydra.lib.eithers.map((lambda pairs: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(pairs)))), hydra.lib.eithers.map_list((lambda x1: encode_entry(x1)), hydra.lib.maps.to_list(entries))))[1])(v12.value) if isinstance(v12, hydra.core.TermMap) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), _hoist_val_ad_body_2 := (lambda cx1, v12: (lambda m: (decode_entry := (lambda entry: (k := hydra.lib.pairs.first(entry), v := hydra.lib.pairs.second(entry), hydra.lib.eithers.map((lambda v_term: (cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(k)))), v_term)), val_ad.coder.decode(cx1, v)))[2]), hydra.lib.eithers.map((lambda pairs: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(pairs)))), hydra.lib.eithers.map_list((lambda x1: decode_entry(x1)), hydra.lib.maps.to_list(m))))[1])(v12.value) if isinstance(v12, hydra.json.model.ValueObject) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), Right((hydra.coders.Adapter(val_ad.is_lossy, typ, cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaMap(hydra.avro.schema.Map(val_ad.target))), hydra.coders.Coder((lambda cx1, t: _hoist_val_ad_body_1(cx1, t)), (lambda cx1, j: _hoist_val_ad_body_2(cx1, j)))), env1)))[4]))

                    case _:
                        return err(cx, "Avro maps require string keys")
            def _hoist_key_type_body_2(v1):
                match v1:
                    case hydra.core.TypeLiteral(value=lt):
                        return _hoist_key_type_body_1(lt)

                    case _:
                        return err(cx, "Avro maps require string keys")
            return _hoist_key_type_body_2(hydra.strip.deannotate_type(key_type))

        case hydra.core.TypeRecord(value=field_types):
            return named_type_adapter(cx, typ, m_name, annotations(), field_types, env, (lambda avro_fields: cast(hydra.avro.schema.NamedType, hydra.avro.schema.NamedTypeRecord(hydra.avro.schema.Record(avro_fields)))), (lambda x1, x2, x3: record_term_coder(x1, x2, x3)))

        case hydra.core.TypeUnion(value=field_types2):
            @lru_cache(1)
            def all_unit():
                def _hoist_all_unit_1(v1):
                    match v1:
                        case hydra.core.TypeUnit():
                            return True

                        case _:
                            return False
                return hydra.lib.lists.foldl((lambda b, ft: hydra.lib.logic.and_(b, _hoist_all_unit_1(ft.type))), True, field_types2)
            return hydra.lib.logic.if_else(all_unit(), (lambda : enum_adapter(cx, typ, m_name, annotations(), field_types2, env)), (lambda : union_as_record_adapter(cx, typ, m_name, annotations(), field_types2, env)))

        case hydra.core.TypeMaybe(value=inner_type2):
            return hydra.lib.eithers.bind(encode_type_inner(cx, Nothing(), inner_type2, env), (lambda ad_env: (inner_ad := hydra.lib.pairs.first(ad_env), env1 := hydra.lib.pairs.second(ad_env), _hoist_inner_ad_body_1 := (lambda cx1, v1: (lambda ot: hydra.lib.maybes.maybe((lambda : Right(cast(hydra.json.model.Value, hydra.json.model.ValueNull()))), (lambda inner: inner_ad.coder.encode(cx1, inner)), ot))(v1.value) if isinstance(v1, hydra.core.TermMaybe) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), _hoist_inner_ad_body_2 := (lambda cx1, j, v1: (lambda _: Right(cast(hydra.core.Term, hydra.core.TermMaybe(Nothing()))))(v1) if isinstance(v1, hydra.json.model.ValueNull) else hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermMaybe(Just(t)))), inner_ad.coder.decode(cx1, j))), Right((hydra.coders.Adapter(inner_ad.is_lossy, typ, cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaUnion(hydra.avro.schema.Union((cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaPrimitive(hydra.avro.schema.Primitive.NULL)), inner_ad.target)))), hydra.coders.Coder((lambda cx1, t: _hoist_inner_ad_body_1(cx1, t)), (lambda cx1, j: _hoist_inner_ad_body_2(cx1, j, j)))), env1)))[4]))

        case hydra.core.TypeWrap(value=inner):
            return encode_type_inner(cx, m_name, inner, env)

        case hydra.core.TypeVariable(value=name_):
            return hydra.lib.maybes.maybe((lambda : hydra.lib.maybes.maybe((lambda : err(cx, hydra.lib.strings.cat2("referenced type not found: ", name_.value))), (lambda ref_type: encode_type_inner(cx, Just(name_), ref_type, env)), hydra.lib.maps.lookup(name_, env.type_map))), (lambda existing_ad: Right((hydra.coders.Adapter(existing_ad.is_lossy, existing_ad.source, cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaReference(local_name(name_))), existing_ad.coder), env))), hydra.lib.maps.lookup(name_, env.emitted))

        case _:
            return err(cx, "unsupported Hydra type for Avro encoding")

def fold_field_adapters(cx: T0, field_types: frozenlist[hydra.core.FieldType], env0: hydra.avro.environment.EncodeEnvironment) -> Either[hydra.errors.Error, tuple[frozenlist[tuple[hydra.core.Name, hydra.coders.Adapter[hydra.core.Type, hydra.avro.schema.Schema, hydra.core.Term, hydra.json.model.Value]]], hydra.avro.environment.EncodeEnvironment]]:
    r"""Fold over field types, building adapters and threading the environment."""

    return hydra.lib.lists.foldl((lambda acc, ft: hydra.lib.eithers.bind(acc, (lambda acc_pair: (so_far := hydra.lib.pairs.first(acc_pair), env1 := hydra.lib.pairs.second(acc_pair), fname := ft.name, ftype := ft.type, hydra.lib.eithers.bind(encode_type_inner(cx, Nothing(), ftype, env1), (lambda ad_env: (ad := hydra.lib.pairs.first(ad_env), env2 := hydra.lib.pairs.second(ad_env), Right((hydra.lib.lists.concat2(so_far, ((fname, ad),)), env2)))[2])))[4]))), Right(((), env0)), field_types)

def named_type_adapter(cx: T0, typ: hydra.core.Type, m_name: Maybe[hydra.core.Name], annotations: FrozenDict[hydra.core.Name, hydra.core.Term], field_types: frozenlist[hydra.core.FieldType], env0: hydra.avro.environment.EncodeEnvironment, mk_named_type: Callable[[frozenlist[hydra.avro.schema.Field]], hydra.avro.schema.NamedType], mk_coder: Callable[[
  T0,
  hydra.core.Name,
  frozenlist[tuple[hydra.core.Name, hydra.coders.Adapter[hydra.core.Type, hydra.avro.schema.Schema, hydra.core.Term, hydra.json.model.Value]]]], tuple[Callable[[hydra.context.Context, hydra.core.Term], Either[hydra.errors.Error, hydra.json.model.Value]], Callable[[hydra.context.Context, hydra.json.model.Value], Either[hydra.errors.Error, hydra.core.Term]]]]) -> Either[hydra.errors.Error, tuple[hydra.coders.Adapter[hydra.core.Type, hydra.avro.schema.Schema, hydra.core.Term, hydra.json.model.Value], hydra.avro.environment.EncodeEnvironment]]:
    r"""Build a named type adapter (shared between record and union-as-record)."""

    @lru_cache(1)
    def type_name() -> hydra.core.Name:
        return hydra.lib.maybes.from_maybe((lambda : type_to_name(typ)), m_name)
    return hydra.lib.maybes.maybe((lambda : hydra.lib.eithers.bind(fold_field_adapters(cx, field_types, env0), (lambda fa_result: (field_adapters := hydra.lib.pairs.first(fa_result), env1 := hydra.lib.pairs.second(fa_result), avro_fields := hydra.lib.lists.map((lambda x1: build_avro_field(x1)), field_adapters), avro_annotations := hydra_annotations_to_avro(annotations), avro_schema := cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaNamed(hydra.avro.schema.Named(local_name(type_name()), name_namespace(type_name()), Nothing(), Nothing(), mk_named_type(avro_fields), avro_annotations))), lossy := hydra.lib.lists.foldl((lambda b, fa: hydra.lib.logic.or_(b, hydra.lib.pairs.second(fa).is_lossy)), False, field_adapters), coder_pair := mk_coder(cx, type_name(), field_adapters), encode_fn := hydra.lib.pairs.first(coder_pair), decode_fn := hydra.lib.pairs.second(coder_pair), adapter_ := hydra.coders.Adapter(lossy, typ, avro_schema, hydra.coders.Coder((lambda x1, x2: encode_fn(x1, x2)), (lambda x1, x2: decode_fn(x1, x2)))), env2 := hydra.avro.environment.EncodeEnvironment(env1.type_map, hydra.lib.maps.insert(type_name(), adapter_, env1.emitted)), Right((adapter_, env2)))[11]))), (lambda existing_ad: Right((existing_ad, env0))), hydra.lib.maps.lookup(type_name(), env0.emitted))

def union_as_record_adapter(cx: T0, typ: hydra.core.Type, m_name: Maybe[hydra.core.Name], annotations: FrozenDict[hydra.core.Name, hydra.core.Term], field_types: frozenlist[hydra.core.FieldType], env0: hydra.avro.environment.EncodeEnvironment) -> Either[hydra.errors.Error, tuple[hydra.coders.Adapter[hydra.core.Type, hydra.avro.schema.Schema, hydra.core.Term, hydra.json.model.Value], hydra.avro.environment.EncodeEnvironment]]:
    r"""Adapter for general unions (encoded as records with optional fields)."""

    return hydra.lib.eithers.bind(fold_field_adapters(cx, field_types, env0), (lambda fa_result: (field_adapters := hydra.lib.pairs.first(fa_result), env1 := hydra.lib.pairs.second(fa_result), avro_fields := hydra.lib.lists.map((lambda name_ad: (fname := hydra.lib.pairs.first(name_ad), ad := hydra.lib.pairs.second(name_ad), hydra.avro.schema.Field(local_name(fname), Nothing(), cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaUnion(hydra.avro.schema.Union((cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaPrimitive(hydra.avro.schema.Primitive.NULL)), ad.target)))), Just(cast(hydra.json.model.Value, hydra.json.model.ValueNull())), Nothing(), Nothing(), hydra.lib.maps.empty()))[2]), field_adapters), type_name := hydra.lib.maybes.from_maybe((lambda : type_to_name(typ)), m_name), avro_annotations := hydra_annotations_to_avro(annotations), avro_schema := cast(hydra.avro.schema.Schema, hydra.avro.schema.SchemaNamed(hydra.avro.schema.Named(local_name(type_name), name_namespace(type_name), Nothing(), Nothing(), cast(hydra.avro.schema.NamedType, hydra.avro.schema.NamedTypeRecord(hydra.avro.schema.Record(avro_fields))), avro_annotations))), adapter_ := (_hoist_adapter_1 := (lambda cx1, v1: (lambda inj: (active_name := inj.field.name, active_value := inj.field.term, encode_pair := (lambda name_ad: (fname := hydra.lib.pairs.first(name_ad), ad := hydra.lib.pairs.second(name_ad), hydra.lib.logic.if_else(hydra.lib.equality.equal(fname.value, active_name.value), (lambda : hydra.lib.eithers.map((lambda jv: (local_name(fname), jv)), ad.coder.encode(cx1, active_value))), (lambda : Right((local_name(fname), cast(hydra.json.model.Value, hydra.json.model.ValueNull()))))))[2]), hydra.lib.eithers.map((lambda pairs: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(pairs)))), hydra.lib.eithers.map_list((lambda x1: encode_pair(x1)), field_adapters)))[3])(v1.value) if isinstance(v1, hydra.core.TermInject) else err(cx1, "expected union term")), _hoist_adapter_2 := (lambda cx1, v1: (lambda m: (find_active := (lambda remaining: hydra.lib.maybes.maybe((lambda : err(cx1, "no non-null field in union record")), (lambda p: (head_ := hydra.lib.pairs.first(p), rest_ := hydra.lib.pairs.second(p), fname := hydra.lib.pairs.first(head_), ad := hydra.lib.pairs.second(head_), mjv := hydra.lib.maps.lookup(local_name(fname), m), _hoist_head_body_1 := (lambda jv, v12: (lambda _: find_active(rest_))(v12) if isinstance(v12, hydra.json.model.ValueNull) else hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(type_name, hydra.core.Field(fname, t))))), ad.coder.decode(cx1, jv))), hydra.lib.maybes.maybe((lambda : find_active(rest_)), (lambda jv: _hoist_head_body_1(jv, jv)), mjv))[6]), hydra.lib.lists.uncons(remaining))), find_active(field_adapters))[1])(v1.value) if isinstance(v1, hydra.json.model.ValueObject) else err(cx1, "expected JSON object for union-as-record")), hydra.coders.Adapter(True, typ, avro_schema, hydra.coders.Coder((lambda cx1, t: _hoist_adapter_1(cx1, t)), (lambda cx1, j: _hoist_adapter_2(cx1, j)))))[2], env2 := hydra.avro.environment.EncodeEnvironment(env1.type_map, hydra.lib.maps.insert(type_name, adapter_, env1.emitted)), Right((adapter_, env2)))[8]))

def encode_type_with_env(cx: T0, name_: hydra.core.Name, env: hydra.avro.environment.EncodeEnvironment) -> Either[hydra.errors.Error, tuple[hydra.coders.Adapter[hydra.core.Type, hydra.avro.schema.Schema, hydra.core.Term, hydra.json.model.Value], hydra.avro.environment.EncodeEnvironment]]:
    r"""Encode with full environment threading. Returns the adapter and updated environment."""

    return hydra.lib.maybes.maybe((lambda : err(cx, hydra.lib.strings.cat2("type not found in type map: ", hydra.lib.literals.show_string(name_.value)))), (lambda typ: encode_type_inner(cx, Just(name_), typ, env)), hydra.lib.maps.lookup(name_, env.type_map))

def encode_type(cx: T0, type_map: FrozenDict[hydra.core.Name, hydra.core.Type], name_: hydra.core.Name) -> Either[hydra.errors.Error, hydra.coders.Adapter[hydra.core.Type, hydra.avro.schema.Schema, hydra.core.Term, hydra.json.model.Value]]:
    r"""Encode a Hydra type to an Avro schema adapter, given the type map and a root name."""

    return hydra.lib.eithers.map((lambda ad_env: hydra.lib.pairs.first(ad_env)), encode_type_with_env(cx, name_, empty_encode_environment(type_map)))

def hydra_avro_adapter(cx: T0, type_map: FrozenDict[hydra.core.Name, hydra.core.Type], typ: hydra.core.Type) -> Either[hydra.errors.Error, hydra.coders.Adapter[hydra.core.Type, hydra.avro.schema.Schema, hydra.core.Term, hydra.json.model.Value]]:
    r"""Encode a single type without a type map (for simple/anonymous types)."""

    return hydra.lib.eithers.map((lambda ad_env: hydra.lib.pairs.first(ad_env)), encode_type_inner(cx, Nothing(), typ, empty_encode_environment(type_map)))

def hydra_name_to_avro_name(name_: hydra.core.Name) -> tuple[str, Maybe[str]]:
    r"""Convert a Hydra Name to an Avro qualified name (local name, optional namespace)."""

    return (local_name(name_), name_namespace(name_))
