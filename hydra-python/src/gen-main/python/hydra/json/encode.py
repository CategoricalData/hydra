# Note: this is an automatically generated file. Do not edit.

r"""JSON encoding for Hydra terms. Converts Terms to JSON Values using Either for error handling."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
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
import hydra.show.core
import hydra.strip

T0 = TypeVar("T0")

def encode_float(fv: hydra.core.FloatValue) -> Either[T0, hydra.json.model.Value]:
    r"""Encode a float value to JSON. Float64/Bigfloat use native numbers; Float32 uses string."""

    match fv:
        case hydra.core.FloatValueBigfloat(value=bf):
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueNumber(bf)))

        case hydra.core.FloatValueFloat32(value=f):
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.lib.literals.show_float32(f))))

        case hydra.core.FloatValueFloat64(value=f2):
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.float64_to_bigfloat(f2))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_integer(iv: hydra.core.IntegerValue) -> Either[T0, hydra.json.model.Value]:
    r"""Encode an integer value to JSON. Small ints use native numbers; large ints use strings."""

    match iv:
        case hydra.core.IntegerValueBigint(value=bi):
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.lib.literals.show_bigint(bi))))

        case hydra.core.IntegerValueInt64(value=i):
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.lib.literals.show_int64(i))))

        case hydra.core.IntegerValueUint32(value=i2):
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.lib.literals.show_uint32(i2))))

        case hydra.core.IntegerValueUint64(value=i3):
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.lib.literals.show_uint64(i3))))

        case hydra.core.IntegerValueInt8(value=i4):
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(hydra.lib.literals.int8_to_bigint(i4)))))

        case hydra.core.IntegerValueInt16(value=i5):
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(hydra.lib.literals.int16_to_bigint(i5)))))

        case hydra.core.IntegerValueInt32(value=i6):
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(hydra.lib.literals.int32_to_bigint(i6)))))

        case hydra.core.IntegerValueUint8(value=i7):
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(hydra.lib.literals.uint8_to_bigint(i7)))))

        case hydra.core.IntegerValueUint16(value=i8):
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(hydra.lib.literals.uint16_to_bigint(i8)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_literal(lit: hydra.core.Literal) -> Either[T0, hydra.json.model.Value]:
    r"""Encode a Hydra literal to a JSON value."""

    match lit:
        case hydra.core.LiteralBinary(value=b):
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.lib.literals.binary_to_string(b))))

        case hydra.core.LiteralBoolean(value=b2):
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueBoolean(b2)))

        case hydra.core.LiteralFloat(value=f):
            return encode_float(f)

        case hydra.core.LiteralInteger(value=i):
            return encode_integer(i)

        case hydra.core.LiteralString(value=s):
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(s)))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def to_json_untyped(term: hydra.core.Term) -> Either[str, hydra.json.model.Value]:
    r"""Encode a Hydra term to a JSON value without type information. Falls back to array-wrapped Maybe encoding."""

    @lru_cache(1)
    def stripped() -> hydra.core.Term:
        return hydra.strip.deannotate_term(term)
    match stripped():
        case hydra.core.TermLiteral(value=lit):
            return encode_literal(lit)

        case hydra.core.TermList(value=terms):
            @lru_cache(1)
            def results() -> Either[str, frozenlist[hydra.json.model.Value]]:
                return hydra.lib.eithers.map_list((lambda t: to_json_untyped(t)), terms)
            return hydra.lib.eithers.map((lambda vs: cast(hydra.json.model.Value, hydra.json.model.ValueArray(vs))), results())

        case hydra.core.TermSet(value=vals):
            @lru_cache(1)
            def terms() -> frozenlist[hydra.core.Term]:
                return hydra.lib.sets.to_list(vals)
            @lru_cache(1)
            def results() -> Either[str, frozenlist[hydra.json.model.Value]]:
                return hydra.lib.eithers.map_list((lambda t: to_json_untyped(t)), terms())
            return hydra.lib.eithers.map((lambda vs: cast(hydra.json.model.Value, hydra.json.model.ValueArray(vs))), results())

        case hydra.core.TermMaybe(value=opt):
            return hydra.lib.maybes.maybe((lambda : Right(cast(hydra.json.model.Value, hydra.json.model.ValueNull()))), (lambda v: (encoded_maybe := to_json_untyped(v), hydra.lib.eithers.map((lambda encoded: cast(hydra.json.model.Value, hydra.json.model.ValueArray((encoded,)))), encoded_maybe))[1]), opt)

        case hydra.core.TermRecord(value=r):
            def encode_field(f: hydra.core.Field) -> Either[str, tuple[str, hydra.json.model.Value]]:
                fname = f.name.value
                fterm = f.term
                @lru_cache(1)
                def encoded_field() -> Either[str, hydra.json.model.Value]:
                    return to_json_untyped(fterm)
                return hydra.lib.eithers.map((lambda v: (fname, v)), encoded_field())
            fields = r.fields
            @lru_cache(1)
            def encoded_fields() -> Either[str, frozenlist[tuple[str, hydra.json.model.Value]]]:
                return hydra.lib.eithers.map_list((lambda x1: encode_field(x1)), fields)
            return hydra.lib.eithers.map((lambda fs: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(fs)))), encoded_fields())

        case hydra.core.TermUnion(value=inj):
            field = inj.field
            fname = field.name.value
            fterm = field.term
            @lru_cache(1)
            def encoded_union() -> Either[str, hydra.json.model.Value]:
                return to_json_untyped(fterm)
            return hydra.lib.eithers.map((lambda v: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(((fname, v),))))), encoded_union())

        case hydra.core.TermUnit():
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.empty())))

        case hydra.core.TermWrap(value=wt):
            return to_json_untyped(wt.body)

        case hydra.core.TermMap(value=m):
            def encode_entry(kv: tuple[hydra.core.Term, hydra.core.Term]) -> Either[str, hydra.json.model.Value]:
                @lru_cache(1)
                def k() -> hydra.core.Term:
                    return hydra.lib.pairs.first(kv)
                @lru_cache(1)
                def v() -> hydra.core.Term:
                    return hydra.lib.pairs.second(kv)
                @lru_cache(1)
                def encoded_k() -> Either[str, hydra.json.model.Value]:
                    return to_json_untyped(k())
                @lru_cache(1)
                def encoded_v() -> Either[str, hydra.json.model.Value]:
                    return to_json_untyped(v())
                return hydra.lib.eithers.either((lambda err: Left(err)), (lambda ek: hydra.lib.eithers.map((lambda ev: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list((("@key", ek), ("@value", ev)))))), encoded_v())), encoded_k())
            @lru_cache(1)
            def entries() -> Either[str, frozenlist[hydra.json.model.Value]]:
                return hydra.lib.eithers.map_list((lambda x1: encode_entry(x1)), hydra.lib.maps.to_list(m))
            return hydra.lib.eithers.map((lambda es: cast(hydra.json.model.Value, hydra.json.model.ValueArray(es))), entries())

        case hydra.core.TermPair(value=p):
            @lru_cache(1)
            def first() -> hydra.core.Term:
                return hydra.lib.pairs.first(p)
            @lru_cache(1)
            def second() -> hydra.core.Term:
                return hydra.lib.pairs.second(p)
            @lru_cache(1)
            def encoded_first() -> Either[str, hydra.json.model.Value]:
                return to_json_untyped(first())
            @lru_cache(1)
            def encoded_second() -> Either[str, hydra.json.model.Value]:
                return to_json_untyped(second())
            return hydra.lib.eithers.either((lambda err: Left(err)), (lambda ef: hydra.lib.eithers.map((lambda es: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list((("@first", ef), ("@second", es)))))), encoded_second())), encoded_first())

        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: (encoded_l := to_json_untyped(l), hydra.lib.eithers.map((lambda v: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list((("@left", v),))))), encoded_l))[1]), (lambda r: (encoded_r := to_json_untyped(r), hydra.lib.eithers.map((lambda v: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list((("@right", v),))))), encoded_r))[1]), e)

        case _:
            return Left(hydra.lib.strings.cat(("unsupported term variant for JSON encoding: ", hydra.show.core.term(term))))

def to_json(types: FrozenDict[hydra.core.Name, hydra.core.Type], tname: hydra.core.Name, typ: hydra.core.Type, term: hydra.core.Term):
    r"""Encode a Hydra term to a JSON value given a type and type name. Returns Left for unsupported constructs."""

    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    @lru_cache(1)
    def stripped_term() -> hydra.core.Term:
        return hydra.strip.deannotate_term(term)
    def _hoist_stripped_term_body_1(v1):
        match v1:
            case hydra.core.TermLiteral(value=lit):
                return encode_literal(lit)

            case _:
                return Left("expected literal term")
    def _hoist_stripped_term_body_2(elem_type, v1):
        match v1:
            case hydra.core.TermList(value=terms):
                @lru_cache(1)
                def results() -> Either[str, frozenlist[hydra.json.model.Value]]:
                    return hydra.lib.eithers.map_list((lambda t: to_json(types, tname, elem_type, t)), terms)
                return hydra.lib.eithers.map((lambda vs: cast(hydra.json.model.Value, hydra.json.model.ValueArray(vs))), results())

            case _:
                return Left("expected list term")
    def _hoist_stripped_term_body_3(elem_type, v1):
        match v1:
            case hydra.core.TermSet(value=vals):
                @lru_cache(1)
                def terms() -> frozenlist[hydra.core.Term]:
                    return hydra.lib.sets.to_list(vals)
                @lru_cache(1)
                def results() -> Either[str, frozenlist[hydra.json.model.Value]]:
                    return hydra.lib.eithers.map_list((lambda t: to_json(types, tname, elem_type, t)), terms())
                return hydra.lib.eithers.map((lambda vs: cast(hydra.json.model.Value, hydra.json.model.ValueArray(vs))), results())

            case _:
                return Left("expected set term")
    def _hoist_stripped_term_body_4(rt, v1):
        match v1:
            case hydra.core.TermRecord(value=r):
                def is_simple_maybe(ftype: hydra.core.Type):
                    def _hoist_is_simple_maybe_1(v12):
                        match v12:
                            case hydra.core.TypeMaybe():
                                return False

                            case _:
                                return True
                    def _hoist_is_simple_maybe_2(v12):
                        match v12:
                            case hydra.core.TypeMaybe(value=inner_t):
                                return _hoist_is_simple_maybe_1(hydra.strip.deannotate_type(inner_t))

                            case _:
                                return False
                    return _hoist_is_simple_maybe_2(hydra.strip.deannotate_type(ftype))
                def encode_field_with_type(ft: hydra.core.FieldType, f: hydra.core.Field):
                    fname = f.name.value
                    fterm = f.term
                    ftype = ft.type
                    def _hoist_ftype_body_1(v12):
                        match v12:
                            case hydra.core.TermMaybe(value=opt):
                                return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda v: (inner_type := (_hoist_inner_type_1 := (lambda v13: (lambda it: it)(v13.value) if isinstance(v13, hydra.core.TypeMaybe) else ftype), _hoist_inner_type_1(hydra.strip.deannotate_type(ftype)))[1], encoded := to_json(types, tname, inner_type, v), hydra.lib.eithers.map((lambda ev: Just((fname, ev))), encoded))[2]), opt)

                            case _:
                                return Left("expected maybe term for optional field")
                    return hydra.lib.logic.if_else(is_simple_maybe(ftype), (lambda : _hoist_ftype_body_1(hydra.strip.deannotate_term(fterm))), (lambda : (encoded := to_json(types, tname, ftype, fterm), hydra.lib.eithers.map((lambda ev: Just((fname, ev))), encoded))[1]))
                field_types = rt
                fields = r.fields
                @lru_cache(1)
                def encoded_pairs() -> Either[str, frozenlist[Maybe[tuple[str, hydra.json.model.Value]]]]:
                    return hydra.lib.eithers.map_list((lambda ftf: encode_field_with_type(hydra.lib.pairs.first(ftf), hydra.lib.pairs.second(ftf))), hydra.lib.lists.zip(field_types, fields))
                return hydra.lib.eithers.map((lambda pairs: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.maybes.cat(pairs))))), encoded_pairs())

            case _:
                return Left("expected record term")
    def _hoist_stripped_term_body_5(rt, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name.value
                fterm = field.term
                def find_field_type(fts: frozenlist[hydra.core.FieldType]) -> Either[str, hydra.core.Type]:
                    return hydra.lib.logic.if_else(hydra.lib.lists.null(fts), (lambda : Left(hydra.lib.strings.cat(("unknown variant: ", fname)))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.head(fts).name.value, fname), (lambda : Right(hydra.lib.lists.head(fts).type)), (lambda : find_field_type(hydra.lib.lists.tail(fts))))))
                @lru_cache(1)
                def ftype_result() -> Either[str, hydra.core.Type]:
                    return find_field_type(rt)
                return hydra.lib.eithers.either((lambda err: Left(err)), (lambda ftype: (encoded_union := to_json(types, tname, ftype, fterm), hydra.lib.eithers.map((lambda v: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(((fname, v),))))), encoded_union))[1]), ftype_result())

            case _:
                return Left("expected union term")
    def _hoist_stripped_term_body_6(wn, v1):
        match v1:
            case hydra.core.TermWrap(value=wt):
                return to_json(types, tname, wn, wt.body)

            case _:
                return Left("expected wrapped term")
    match stripped():
        case hydra.core.TypeLiteral():
            return _hoist_stripped_term_body_1(stripped_term())

        case hydra.core.TypeList(value=elem_type):
            return _hoist_stripped_term_body_2(elem_type, stripped_term())

        case hydra.core.TypeSet(value=elem_type2):
            return _hoist_stripped_term_body_3(elem_type2, stripped_term())

        case hydra.core.TypeMaybe(value=inner_type):
            @lru_cache(1)
            def inner_stripped() -> hydra.core.Type:
                return hydra.strip.deannotate_type(inner_type)
            @lru_cache(1)
            def is_nested_maybe():
                def _hoist_is_nested_maybe_1(v1):
                    match v1:
                        case hydra.core.TypeMaybe():
                            return True

                        case _:
                            return False
                return _hoist_is_nested_maybe_1(inner_stripped())
            def _hoist_is_nested_maybe_body_1(v1):
                match v1:
                    case hydra.core.TermMaybe(value=opt):
                        return hydra.lib.maybes.maybe((lambda : Right(cast(hydra.json.model.Value, hydra.json.model.ValueNull()))), (lambda v: (encoded := to_json(types, tname, inner_type, v), hydra.lib.logic.if_else(is_nested_maybe(), (lambda : hydra.lib.eithers.map((lambda ev: cast(hydra.json.model.Value, hydra.json.model.ValueArray((ev,)))), encoded)), (lambda : encoded)))[1]), opt)

                    case _:
                        return Left("expected maybe term")
            return _hoist_is_nested_maybe_body_1(stripped_term())

        case hydra.core.TypeRecord(value=rt):
            return _hoist_stripped_term_body_4(rt, stripped_term())

        case hydra.core.TypeUnion(value=rt2):
            return _hoist_stripped_term_body_5(rt2, stripped_term())

        case hydra.core.TypeUnit():
            return Right(cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.empty())))

        case hydra.core.TypeWrap(value=wn):
            return _hoist_stripped_term_body_6(wn, stripped_term())

        case hydra.core.TypeMap(value=mt):
            key_type = mt.keys
            val_type = mt.values
            def _hoist_val_type_body_1(v1):
                match v1:
                    case hydra.core.TermMap(value=m):
                        def encode_entry(kv: tuple[hydra.core.Term, hydra.core.Term]) -> Either[str, hydra.json.model.Value]:
                            @lru_cache(1)
                            def k() -> hydra.core.Term:
                                return hydra.lib.pairs.first(kv)
                            @lru_cache(1)
                            def v() -> hydra.core.Term:
                                return hydra.lib.pairs.second(kv)
                            @lru_cache(1)
                            def encoded_k() -> Either[str, hydra.json.model.Value]:
                                return to_json(types, tname, key_type, k())
                            @lru_cache(1)
                            def encoded_v() -> Either[str, hydra.json.model.Value]:
                                return to_json(types, tname, val_type, v())
                            return hydra.lib.eithers.either((lambda err: Left(err)), (lambda ek: hydra.lib.eithers.map((lambda ev: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list((("@key", ek), ("@value", ev)))))), encoded_v())), encoded_k())
                        @lru_cache(1)
                        def entries() -> Either[str, frozenlist[hydra.json.model.Value]]:
                            return hydra.lib.eithers.map_list((lambda x1: encode_entry(x1)), hydra.lib.maps.to_list(m))
                        return hydra.lib.eithers.map((lambda es: cast(hydra.json.model.Value, hydra.json.model.ValueArray(es))), entries())

                    case _:
                        return Left("expected map term")
            return _hoist_val_type_body_1(stripped_term())

        case hydra.core.TypePair(value=pt):
            first_type = pt.first
            second_type = pt.second
            def _hoist_second_type_body_1(v1):
                match v1:
                    case hydra.core.TermPair(value=p):
                        @lru_cache(1)
                        def first() -> hydra.core.Term:
                            return hydra.lib.pairs.first(p)
                        @lru_cache(1)
                        def second() -> hydra.core.Term:
                            return hydra.lib.pairs.second(p)
                        @lru_cache(1)
                        def encoded_first() -> Either[str, hydra.json.model.Value]:
                            return to_json(types, tname, first_type, first())
                        @lru_cache(1)
                        def encoded_second() -> Either[str, hydra.json.model.Value]:
                            return to_json(types, tname, second_type, second())
                        return hydra.lib.eithers.either((lambda err: Left(err)), (lambda ef: hydra.lib.eithers.map((lambda es: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list((("@first", ef), ("@second", es)))))), encoded_second())), encoded_first())

                    case _:
                        return Left("expected pair term")
            return _hoist_second_type_body_1(stripped_term())

        case hydra.core.TypeEither(value=et):
            left_type = et.left
            right_type = et.right
            def _hoist_right_type_body_1(v1):
                match v1:
                    case hydra.core.TermEither(value=e):
                        return hydra.lib.eithers.either((lambda l: (encoded_l := to_json(types, tname, left_type, l), hydra.lib.eithers.map((lambda v: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list((("@left", v),))))), encoded_l))[1]), (lambda r: (encoded_r := to_json(types, tname, right_type, r), hydra.lib.eithers.map((lambda v: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list((("@right", v),))))), encoded_r))[1]), e)

                    case _:
                        return Left("expected either term")
            return _hoist_right_type_body_1(stripped_term())

        case hydra.core.TypeVariable(value=name):
            @lru_cache(1)
            def looked_up() -> Maybe[hydra.core.Type]:
                return hydra.lib.maps.lookup(name, types)
            return hydra.lib.maybes.maybe((lambda : to_json_untyped(term)), (lambda resolved_type: to_json(types, name, resolved_type, term)), looked_up())

        case _:
            return Left(hydra.lib.strings.cat(("unsupported type for JSON encoding: ", hydra.show.core.type(typ))))
