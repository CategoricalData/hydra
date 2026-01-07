# Note: this is an automatically generated file. Do not edit.

r"""JSON decoding for Hydra terms. Converts JSON Values to Terms using Either for error handling."""

from __future__ import annotations
from decimal import Decimal
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import cast
import hydra.core
import hydra.json
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.sets
import hydra.lib.strings
import hydra.rewriting
import hydra.show.core

def expect_number(value: hydra.json.Value) -> Either[str, Decimal]:
    r"""Extract a number from a JSON value."""
    
    match value:
        case hydra.json.ValueNumber(value=n):
            return cast(Either[str, Decimal], Right(n))
        
        case _:
            return cast(Either[str, Decimal], Left("expected number"))

def expect_string(value: hydra.json.Value) -> Either[str, str]:
    r"""Extract a string from a JSON value."""
    
    match value:
        case hydra.json.ValueString(value=s):
            return cast(Either[str, str], Right(s))
        
        case _:
            return cast(Either[str, str], Left("expected string"))

def decode_float(ft: hydra.core.FloatType, value: hydra.json.Value) -> Either[str, hydra.core.Term]:
    r"""Decode a JSON value to a float term. Float64/Bigfloat from numbers; Float32 from string."""
    
    match ft:
        case hydra.core.FloatType.BIGFLOAT:
            def num_result() -> Either[str, Decimal]:
                return expect_number(value)
            return hydra.lib.eithers.map((lambda n: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueBigfloat(n))))))), num_result())
        
        case hydra.core.FloatType.FLOAT32:
            def str_result() -> Either[str, str]:
                return expect_string(value)
            return hydra.lib.eithers.either((lambda err: cast(Either[str, hydra.core.Term], Left(err))), (lambda s: (parsed := hydra.lib.literals.read_float32(s), hydra.lib.maybes.maybe(cast(Either[str, hydra.core.Term], Left(hydra.lib.strings.cat(("invalid float32: ", s)))), (lambda v: cast(Either[str, hydra.core.Term], Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat32(v))))))))), parsed))[1]), str_result())
        
        case hydra.core.FloatType.FLOAT64:
            def num_result() -> Either[str, Decimal]:
                return expect_number(value)
            return hydra.lib.eithers.map((lambda n: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat64(hydra.lib.literals.bigfloat_to_float64(n)))))))), num_result())
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def decode_integer(it: hydra.core.IntegerType, value: hydra.json.Value) -> Either[str, hydra.core.Term]:
    r"""Decode a JSON value to an integer term. Small ints from numbers; large ints from strings."""
    
    match it:
        case hydra.core.IntegerType.BIGINT:
            def str_result() -> Either[str, str]:
                return expect_string(value)
            return hydra.lib.eithers.either((lambda err: cast(Either[str, hydra.core.Term], Left(err))), (lambda s: (parsed := hydra.lib.literals.read_bigint(s), hydra.lib.maybes.maybe(cast(Either[str, hydra.core.Term], Left(hydra.lib.strings.cat(("invalid bigint: ", s)))), (lambda v: cast(Either[str, hydra.core.Term], Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueBigint(v))))))))), parsed))[1]), str_result())
        
        case hydra.core.IntegerType.INT64:
            def str_result() -> Either[str, str]:
                return expect_string(value)
            return hydra.lib.eithers.either((lambda err: cast(Either[str, hydra.core.Term], Left(err))), (lambda s: (parsed := hydra.lib.literals.read_int64(s), hydra.lib.maybes.maybe(cast(Either[str, hydra.core.Term], Left(hydra.lib.strings.cat(("invalid int64: ", s)))), (lambda v: cast(Either[str, hydra.core.Term], Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(v))))))))), parsed))[1]), str_result())
        
        case hydra.core.IntegerType.UINT32:
            def str_result() -> Either[str, str]:
                return expect_string(value)
            return hydra.lib.eithers.either((lambda err: cast(Either[str, hydra.core.Term], Left(err))), (lambda s: (parsed := hydra.lib.literals.read_uint32(s), hydra.lib.maybes.maybe(cast(Either[str, hydra.core.Term], Left(hydra.lib.strings.cat(("invalid uint32: ", s)))), (lambda v: cast(Either[str, hydra.core.Term], Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint32(v))))))))), parsed))[1]), str_result())
        
        case hydra.core.IntegerType.UINT64:
            def str_result() -> Either[str, str]:
                return expect_string(value)
            return hydra.lib.eithers.either((lambda err: cast(Either[str, hydra.core.Term], Left(err))), (lambda s: (parsed := hydra.lib.literals.read_uint64(s), hydra.lib.maybes.maybe(cast(Either[str, hydra.core.Term], Left(hydra.lib.strings.cat(("invalid uint64: ", s)))), (lambda v: cast(Either[str, hydra.core.Term], Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint64(v))))))))), parsed))[1]), str_result())
        
        case hydra.core.IntegerType.INT8:
            def num_result() -> Either[str, Decimal]:
                return expect_number(value)
            return hydra.lib.eithers.map((lambda n: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt8(hydra.lib.literals.bigint_to_int8(hydra.lib.literals.bigfloat_to_bigint(n))))))))), num_result())
        
        case hydra.core.IntegerType.INT16:
            def num_result() -> Either[str, Decimal]:
                return expect_number(value)
            return hydra.lib.eithers.map((lambda n: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt16(hydra.lib.literals.bigint_to_int16(hydra.lib.literals.bigfloat_to_bigint(n))))))))), num_result())
        
        case hydra.core.IntegerType.INT32:
            def num_result() -> Either[str, Decimal]:
                return expect_number(value)
            return hydra.lib.eithers.map((lambda n: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(hydra.lib.literals.bigint_to_int32(hydra.lib.literals.bigfloat_to_bigint(n))))))))), num_result())
        
        case hydra.core.IntegerType.UINT8:
            def num_result() -> Either[str, Decimal]:
                return expect_number(value)
            return hydra.lib.eithers.map((lambda n: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint8(hydra.lib.literals.bigint_to_uint8(hydra.lib.literals.bigfloat_to_bigint(n))))))))), num_result())
        
        case hydra.core.IntegerType.UINT16:
            def num_result() -> Either[str, Decimal]:
                return expect_number(value)
            return hydra.lib.eithers.map((lambda n: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint16(hydra.lib.literals.bigint_to_uint16(hydra.lib.literals.bigfloat_to_bigint(n))))))))), num_result())
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def decode_literal(lt: hydra.core.LiteralType, value: hydra.json.Value) -> Either[str, hydra.core.Term]:
    def _hoist_hydra_json_decode_decode_literal_1(v1: hydra.json.Value) -> Either[str, hydra.core.Term]:
        match v1:
            case hydra.json.ValueBoolean(value=b):
                return cast(Either[str, hydra.core.Term], Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(b))))))
            
            case _:
                return cast(Either[str, hydra.core.Term], Left("expected boolean"))
    match lt:
        case hydra.core.LiteralTypeBinary():
            def str_result() -> Either[str, str]:
                return expect_string(value)
            return hydra.lib.eithers.map((lambda s: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBinary(hydra.lib.literals.string_to_binary(s)))))), str_result())
        
        case hydra.core.LiteralTypeBoolean():
            return _hoist_hydra_json_decode_decode_literal_1(value)
        
        case hydra.core.LiteralTypeFloat(value=ft):
            return decode_float(ft, value)
        
        case hydra.core.LiteralTypeInteger(value=it):
            return decode_integer(it, value)
        
        case hydra.core.LiteralTypeString():
            def str_result() -> Either[str, str]:
                return expect_string(value)
            return hydra.lib.eithers.map((lambda s: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s))))), str_result())
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def expect_array(value: hydra.json.Value) -> Either[str, frozenlist[hydra.json.Value]]:
    r"""Extract an array from a JSON value."""
    
    match value:
        case hydra.json.ValueArray(value=arr):
            return cast(Either[str, frozenlist[hydra.json.Value]], Right(arr))
        
        case _:
            return cast(Either[str, frozenlist[hydra.json.Value]], Left("expected array"))

def expect_object(value: hydra.json.Value) -> Either[str, FrozenDict[str, hydra.json.Value]]:
    r"""Extract an object from a JSON value."""
    
    match value:
        case hydra.json.ValueObject(value=obj):
            return cast(Either[str, FrozenDict[str, hydra.json.Value]], Right(obj))
        
        case _:
            return cast(Either[str, FrozenDict[str, hydra.json.Value]], Left("expected object"))

def from_json(types: FrozenDict[hydra.core.Name, hydra.core.Type], typ: hydra.core.Type, value: hydra.json.Value) -> Either[str, hydra.core.Term]:
    r"""Decode a JSON value to a Hydra term given a type. Returns Left for type mismatches."""
    
    stripped = hydra.rewriting.deannotate_type(typ)
    def _hoist_body_1(inner_type: hydra.core.Type, v1: hydra.json.Value) -> Either[str, hydra.core.Term]:
        match v1:
            case hydra.json.ValueNull():
                return cast(Either[str, hydra.core.Term], Right(cast(hydra.core.Term, hydra.core.TermMaybe(cast(Maybe[hydra.core.Term], Nothing())))))
            
            case hydra.json.ValueArray(value=arr):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(arr), 0), (lambda : cast(Either[str, hydra.core.Term], Right(cast(hydra.core.Term, hydra.core.TermMaybe(cast(Maybe[hydra.core.Term], Nothing())))))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(arr), 1), (lambda : "let terms are not supported here"), (lambda : cast(Either[str, hydra.core.Term], Left("expected single-element array for Just"))))))
            
            case _:
                return cast(Either[str, hydra.core.Term], Left("expected null or single-element array for Maybe"))
    match stripped:
        case hydra.core.TypeLiteral(value=lt):
            return decode_literal(lt, value)
        
        case hydra.core.TypeList(value=elem_type):
            def decode_elem(v: hydra.json.Value) -> Either[str, hydra.core.Term]:
                return from_json(types, elem_type, v)
            def arr_result() -> Either[str, frozenlist[hydra.json.Value]]:
                return expect_array(value)
            return hydra.lib.eithers.either((lambda err: cast(Either[str, hydra.core.Term], Left(err))), (lambda arr: (decoded := (lambda : hydra.lib.eithers.map_list(decode_elem, arr)), hydra.lib.eithers.map((lambda ts: cast(hydra.core.Term, hydra.core.TermList(ts))), decoded()))[1]), arr_result())
        
        case hydra.core.TypeSet(value=elem_type2):
            def decode_elem(v: hydra.json.Value) -> Either[str, hydra.core.Term]:
                return from_json(types, elem_type2, v)
            def arr_result() -> Either[str, frozenlist[hydra.json.Value]]:
                return expect_array(value)
            return hydra.lib.eithers.either((lambda err: cast(Either[str, hydra.core.Term], Left(err))), (lambda arr: (decoded := (lambda : hydra.lib.eithers.map_list(decode_elem, arr)), hydra.lib.eithers.map((lambda elems: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(elems)))), decoded()))[1]), arr_result())
        
        case hydra.core.TypeMaybe(value=inner_type):
            return _hoist_body_1(inner_type, value)
        
        case hydra.core.TypeRecord(value=rt):
            def obj_result() -> Either[str, FrozenDict[str, hydra.json.Value]]:
                return expect_object(value)
            return hydra.lib.eithers.either((lambda err: cast(Either[str, hydra.core.Term], Left(err))), (lambda obj: (decode_field := (lambda ft: (fname := ft.name, ftype := ft.type, mval := (lambda : hydra.lib.maps.lookup(fname.value, obj)), default_val := cast(hydra.json.Value, hydra.json.ValueNull()), json_val := (lambda : hydra.lib.maybes.from_maybe(default_val, mval())), decoded := (lambda : from_json(types, ftype, json_val())), hydra.lib.eithers.map((lambda v: hydra.core.Field(fname, v)), decoded()))[6]), fields := rt.fields, decoded_fields := (lambda : hydra.lib.eithers.map_list(decode_field, fields)), hydra.lib.eithers.map((lambda fs: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(rt.type_name, fs)))), decoded_fields()))[3]), obj_result())
        
        case hydra.core.TypeUnion(value=rt2):
            def obj_result() -> Either[str, FrozenDict[str, hydra.json.Value]]:
                return expect_object(value)
            return hydra.lib.eithers.either((lambda err: cast(Either[str, hydra.core.Term], Left(err))), (lambda obj: (keys := (lambda : hydra.lib.maps.keys(obj)), hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(keys()), 1), (lambda : "let terms are not supported here"), (lambda : cast(Either[str, hydra.core.Term], Left("expected single-key object for union")))))[1]), obj_result())
        
        case hydra.core.TypeUnit():
            def obj_result() -> Either[str, FrozenDict[str, hydra.json.Value]]:
                return expect_object(value)
            return hydra.lib.eithers.map((lambda _2: cast(hydra.core.Term, hydra.core.TermUnit())), obj_result())
        
        case hydra.core.TypeWrap(value=wn):
            def looked_up() -> Maybe[hydra.core.Type]:
                return hydra.lib.maps.lookup(wn.type_name, types)
            return hydra.lib.maybes.maybe(cast(Either[str, hydra.core.Term], Left(hydra.lib.strings.cat(("unknown wrapped type: ", wn.type_name.value)))), (lambda lt: (inner_type := (lambda : hydra.dsl.python.unsupported("inline match expressions are not yet supported")), decoded := (lambda : from_json(types, inner_type(), value)), hydra.lib.eithers.map((lambda v: cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wn.type_name, v)))), decoded()))[2]), looked_up())
        
        case hydra.core.TypeMap(value=mt):
            key_type = mt.keys
            val_type = mt.values
            def arr_result() -> Either[str, frozenlist[hydra.json.Value]]:
                return expect_array(value)
            return hydra.lib.eithers.either((lambda err: cast(Either[str, hydra.core.Term], Left(err))), (lambda arr: (decode_entry := (lambda entry_json: (obj_result := (lambda : expect_object(entry_json)), hydra.lib.eithers.either((lambda err: cast(Either[str, tuple[hydra.core.Term, hydra.core.Term]], Left(err))), (lambda entry_obj: (key_json := (lambda : hydra.lib.maps.lookup("@key", entry_obj)), val_json := (lambda : hydra.lib.maps.lookup("@value", entry_obj)), hydra.lib.maybes.maybe(cast(Either[str, tuple[hydra.core.Term, hydra.core.Term]], Left("missing @key in map entry")), (lambda kj: hydra.lib.maybes.maybe(cast(Either[str, tuple[hydra.core.Term, hydra.core.Term]], Left("missing @value in map entry")), (lambda vj: (decoded_key := (lambda : from_json(types, key_type, kj)), decoded_val := (lambda : from_json(types, val_type, vj)), hydra.lib.eithers.either((lambda err: cast(Either[str, tuple[hydra.core.Term, hydra.core.Term]], Left(err))), (lambda k: hydra.lib.eithers.map((lambda v: cast(tuple[hydra.core.Term, hydra.core.Term], (k, v))), decoded_val())), decoded_key()))[2]), val_json())), key_json()))[2]), obj_result()))[1]), entries := (lambda : hydra.lib.eithers.map_list(decode_entry, arr)), hydra.lib.eithers.map((lambda es: cast(hydra.core.Term, hydra.core.TermMap(cast(FrozenDict[hydra.core.Term, hydra.core.Term], hydra.lib.maps.from_list(es))))), entries()))[2]), arr_result())
        
        case hydra.core.TypePair(value=pt):
            first_type = pt.first
            second_type = pt.second
            def obj_result() -> Either[str, FrozenDict[str, hydra.json.Value]]:
                return expect_object(value)
            return hydra.lib.eithers.either((lambda err: cast(Either[str, hydra.core.Term], Left(err))), (lambda obj: (first_json := (lambda : hydra.lib.maps.lookup("@first", obj)), second_json := (lambda : hydra.lib.maps.lookup("@second", obj)), hydra.lib.maybes.maybe(cast(Either[str, hydra.core.Term], Left("missing @first in pair")), (lambda fj: hydra.lib.maybes.maybe(cast(Either[str, hydra.core.Term], Left("missing @second in pair")), (lambda sj: (decoded_first := (lambda : from_json(types, first_type, fj)), decoded_second := (lambda : from_json(types, second_type, sj)), hydra.lib.eithers.either((lambda err: cast(Either[str, hydra.core.Term], Left(err))), (lambda f: hydra.lib.eithers.map((lambda s: cast(hydra.core.Term, hydra.core.TermPair(cast(tuple[hydra.core.Term, hydra.core.Term], (f, s))))), decoded_second())), decoded_first()))[2]), second_json())), first_json()))[2]), obj_result())
        
        case hydra.core.TypeEither(value=et):
            left_type = et.left
            right_type = et.right
            def obj_result() -> Either[str, FrozenDict[str, hydra.json.Value]]:
                return expect_object(value)
            return hydra.lib.eithers.either((lambda err: cast(Either[str, hydra.core.Term], Left(err))), (lambda obj: (left_json := (lambda : hydra.lib.maps.lookup("@left", obj)), right_json := (lambda : hydra.lib.maps.lookup("@right", obj)), hydra.lib.maybes.maybe(hydra.lib.maybes.maybe(cast(Either[str, hydra.core.Term], Left("expected @left or @right in Either")), (lambda rj: (decoded := (lambda : from_json(types, right_type, rj)), hydra.lib.eithers.map((lambda v: cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Right(v))))), decoded()))[1]), right_json()), (lambda lj: (decoded := (lambda : from_json(types, left_type, lj)), hydra.lib.eithers.map((lambda v: cast(hydra.core.Term, hydra.core.TermEither(cast(Either[hydra.core.Term, hydra.core.Term], Left(v))))), decoded()))[1]), left_json()))[2]), obj_result())
        
        case hydra.core.TypeVariable(value=name):
            def looked_up() -> Maybe[hydra.core.Type]:
                return hydra.lib.maps.lookup(name, types)
            return hydra.lib.maybes.maybe(cast(Either[str, hydra.core.Term], Left(hydra.lib.strings.cat(("unknown type variable: ", name.value)))), (lambda resolved_type: from_json(types, resolved_type, value)), looked_up())
        
        case _:
            return cast(Either[str, hydra.core.Term], Left(hydra.lib.strings.cat(("unsupported type for JSON decoding: ", hydra.show.core.type(typ)))))
