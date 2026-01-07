# Note: this is an automatically generated file. Do not edit.

r"""JSON encoding for Hydra terms. Converts Terms to JSON Values using Either for error handling."""

from __future__ import annotations
from decimal import Decimal
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.json
import hydra.lib.eithers
import hydra.lib.literals
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.rewriting
import hydra.show.core

T0 = TypeVar("T0")

def encode_float(fv: hydra.core.FloatValue) -> Either[T0, hydra.json.Value]:
    match fv:
        case hydra.core.FloatValueBigfloat(value=bf):
            return cast(Either[T0, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueNumber(bf))))
        
        case hydra.core.FloatValueFloat32(value=f):
            return cast(Either[T0, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueString(hydra.lib.literals.show_float32(f)))))
        
        case hydra.core.FloatValueFloat64(value=f2):
            return cast(Either[T0, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueNumber(hydra.lib.literals.float64_to_bigfloat(f2)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_integer(iv: hydra.core.IntegerValue) -> Either[T0, hydra.json.Value]:
    match iv:
        case hydra.core.IntegerValueBigint(value=bi):
            return cast(Either[T0, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueString(hydra.lib.literals.show_bigint(bi)))))
        
        case hydra.core.IntegerValueInt64(value=i):
            return cast(Either[T0, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueString(hydra.lib.literals.show_int64(i)))))
        
        case hydra.core.IntegerValueUint32(value=i2):
            return cast(Either[T0, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueString(hydra.lib.literals.show_uint32(i2)))))
        
        case hydra.core.IntegerValueUint64(value=i3):
            return cast(Either[T0, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueString(hydra.lib.literals.show_uint64(i3)))))
        
        case hydra.core.IntegerValueInt8(value=i4):
            return cast(Either[T0, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(hydra.lib.literals.int8_to_bigint(i4))))))
        
        case hydra.core.IntegerValueInt16(value=i5):
            return cast(Either[T0, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(hydra.lib.literals.int16_to_bigint(i5))))))
        
        case hydra.core.IntegerValueInt32(value=i6):
            return cast(Either[T0, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(hydra.lib.literals.int32_to_bigint(i6))))))
        
        case hydra.core.IntegerValueUint8(value=i7):
            return cast(Either[T0, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(hydra.lib.literals.uint8_to_bigint(i7))))))
        
        case hydra.core.IntegerValueUint16(value=i8):
            return cast(Either[T0, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(hydra.lib.literals.uint16_to_bigint(i8))))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def encode_literal(lit: hydra.core.Literal) -> Either[T0, hydra.json.Value]:
    match lit:
        case hydra.core.LiteralBinary(value=b):
            return cast(Either[T0, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueString(hydra.lib.literals.binary_to_string(b)))))
        
        case hydra.core.LiteralBoolean(value=b2):
            return cast(Either[T0, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueBoolean(b2))))
        
        case hydra.core.LiteralFloat(value=f):
            return encode_float(f)
        
        case hydra.core.LiteralInteger(value=i):
            return encode_integer(i)
        
        case hydra.core.LiteralString(value=s):
            return cast(Either[T0, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueString(s))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def to_json(term: hydra.core.Term) -> Either[str, hydra.json.Value]:
    r"""Encode a Hydra term to a JSON value. Returns Left for unsupported constructs."""
    
    stripped = hydra.rewriting.deannotate_term(term)
    match stripped:
        case hydra.core.TermLiteral(value=lit):
            return encode_literal(lit)
        
        case hydra.core.TermList(value=terms):
            def results() -> Either[str, frozenlist[hydra.json.Value]]:
                return hydra.lib.eithers.map_list((lambda t: to_json(t)), terms)
            return hydra.lib.eithers.map((lambda vs: cast(hydra.json.Value, hydra.json.ValueArray(vs))), results())
        
        case hydra.core.TermSet(value=vals):
            def terms() -> frozenlist[hydra.core.Term]:
                return hydra.lib.sets.to_list(vals)
            def results() -> Either[str, frozenlist[hydra.json.Value]]:
                return hydra.lib.eithers.map_list((lambda t: to_json(t)), terms())
            return hydra.lib.eithers.map((lambda vs: cast(hydra.json.Value, hydra.json.ValueArray(vs))), results())
        
        case hydra.core.TermMaybe(value=opt):
            return hydra.lib.maybes.maybe(cast(Either[str, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueNull()))), (lambda v: (encoded_maybe := (lambda : to_json(v)), hydra.lib.eithers.map((lambda encoded: cast(hydra.json.Value, hydra.json.ValueArray((encoded,)))), encoded_maybe()))[1]), opt)
        
        case hydra.core.TermRecord(value=r):
            def encode_field(f: hydra.core.Field) -> Either[str, tuple[str, hydra.json.Value]]:
                fname = f.name.value
                fterm = f.term
                def encoded_field() -> Either[str, hydra.json.Value]:
                    return to_json(fterm)
                return hydra.lib.eithers.map((lambda v: cast(tuple[str, hydra.json.Value], (fname, v))), encoded_field())
            fields = r.fields
            def encoded_fields() -> Either[str, frozenlist[tuple[str, hydra.json.Value]]]:
                return hydra.lib.eithers.map_list(encode_field, fields)
            return hydra.lib.eithers.map((lambda fs: cast(hydra.json.Value, hydra.json.ValueObject(cast(FrozenDict[str, hydra.json.Value], hydra.lib.maps.from_list(fs))))), encoded_fields())
        
        case hydra.core.TermUnion(value=inj):
            field = inj.field
            fname = field.name.value
            fterm = field.term
            def encoded_union() -> Either[str, hydra.json.Value]:
                return to_json(fterm)
            return hydra.lib.eithers.map((lambda v: cast(hydra.json.Value, hydra.json.ValueObject(cast(FrozenDict[str, hydra.json.Value], hydra.lib.maps.from_list((cast(tuple[str, hydra.json.Value], (fname, v)),)))))), encoded_union())
        
        case hydra.core.TermUnit():
            return cast(Either[str, hydra.json.Value], Right(cast(hydra.json.Value, hydra.json.ValueObject(cast(FrozenDict[str, hydra.json.Value], hydra.lib.maps.empty())))))
        
        case hydra.core.TermWrap(value=wt):
            return to_json(wt.body)
        
        case hydra.core.TermMap(value=m):
            def encode_entry(kv: tuple[hydra.core.Term, hydra.core.Term]) -> Either[str, hydra.json.Value]:
                def k() -> hydra.core.Type:
                    return hydra.lib.pairs.first(kv)
                def v() -> hydra.core.Type:
                    return hydra.lib.pairs.second(kv)
                def encoded_k() -> Either[str, hydra.json.Value]:
                    return to_json(k())
                def encoded_v() -> Either[str, hydra.json.Value]:
                    return to_json(v())
                return hydra.lib.eithers.either((lambda err: cast(Either[str, hydra.json.Value], Left(err))), (lambda ek: hydra.lib.eithers.map((lambda ev: cast(hydra.json.Value, hydra.json.ValueObject(cast(FrozenDict[str, hydra.json.Value], hydra.lib.maps.from_list((cast(tuple[str, hydra.json.Value], ("@key", ek)), cast(tuple[str, hydra.json.Value], ("@value", ev)))))))), encoded_v())), encoded_k())
            def entries() -> Either[str, frozenlist[hydra.json.Value]]:
                return hydra.lib.eithers.map_list(encode_entry, hydra.lib.maps.to_list(m))
            return hydra.lib.eithers.map((lambda es: cast(hydra.json.Value, hydra.json.ValueArray(es))), entries())
        
        case hydra.core.TermPair(value=p):
            def first() -> hydra.core.Type:
                return hydra.lib.pairs.first(p)
            def second() -> hydra.core.Type:
                return hydra.lib.pairs.second(p)
            def encoded_first() -> Either[str, hydra.json.Value]:
                return to_json(first())
            def encoded_second() -> Either[str, hydra.json.Value]:
                return to_json(second())
            return hydra.lib.eithers.either((lambda err: cast(Either[str, hydra.json.Value], Left(err))), (lambda ef: hydra.lib.eithers.map((lambda es: cast(hydra.json.Value, hydra.json.ValueObject(cast(FrozenDict[str, hydra.json.Value], hydra.lib.maps.from_list((cast(tuple[str, hydra.json.Value], ("@first", ef)), cast(tuple[str, hydra.json.Value], ("@second", es)))))))), encoded_second())), encoded_first())
        
        case hydra.core.TermEither(value=e):
            return hydra.lib.eithers.either((lambda l: (encoded_l := (lambda : to_json(l)), hydra.lib.eithers.map((lambda v: cast(hydra.json.Value, hydra.json.ValueObject(cast(FrozenDict[str, hydra.json.Value], hydra.lib.maps.from_list((cast(tuple[str, hydra.json.Value], ("@left", v)),)))))), encoded_l()))[1]), (lambda r: (encoded_r := (lambda : to_json(r)), hydra.lib.eithers.map((lambda v: cast(hydra.json.Value, hydra.json.ValueObject(cast(FrozenDict[str, hydra.json.Value], hydra.lib.maps.from_list((cast(tuple[str, hydra.json.Value], ("@right", v)),)))))), encoded_r()))[1]), e)
        
        case _:
            return cast(Either[str, hydra.json.Value], Left(hydra.lib.strings.cat(("unsupported term variant for JSON encoding: ", hydra.show.core.term(term)))))
