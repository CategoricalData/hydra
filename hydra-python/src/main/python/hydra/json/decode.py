# Note: this is an automatically generated file. Do not edit.

r"""JSON decoding for Hydra terms. Converts JSON Values to Terms using Either for error handling."""

from __future__ import annotations
from decimal import Decimal
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import cast
import hydra.core
import hydra.json.model
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

def expect_number(value: hydra.json.model.Value) -> Either[str, Decimal]:
    r"""Extract a number from a JSON value."""
    
    match value:
        case hydra.json.model.ValueNumber(value=n):
            return Right(n)
        
        case _:
            return Left("expected number")

def expect_string(value: hydra.json.model.Value) -> Either[str, str]:
    r"""Extract a string from a JSON value."""
    
    match value:
        case hydra.json.model.ValueString(value=s):
            return Right(s)
        
        case _:
            return Left("expected string")

def decode_float(ft: hydra.core.FloatType, value: hydra.json.model.Value) -> Either[str, hydra.core.Term]:
    r"""Decode a JSON value to a float term. Float64/Bigfloat from numbers; Float32 from string."""
    
    match ft:
        case hydra.core.FloatType.BIGFLOAT:
            def num_result() -> Either[str, Decimal]:
                return expect_number(value)
            return hydra.lib.eithers.map((lambda n: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueBigfloat(n))))))), num_result())
        
        case hydra.core.FloatType.FLOAT32:
            def str_result() -> Either[str, str]:
                return expect_string(value)
            return hydra.lib.eithers.either((lambda err: Left(err)), (lambda s: (parsed := hydra.lib.literals.read_float32(s), hydra.lib.maybes.maybe(Left(hydra.lib.strings.cat(("invalid float32: ", s))), (lambda v: Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat32(v)))))))), parsed))[1]), str_result())
        
        case hydra.core.FloatType.FLOAT64:
            def num_result() -> Either[str, Decimal]:
                return expect_number(value)
            return hydra.lib.eithers.map((lambda n: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueFloat64(hydra.lib.literals.bigfloat_to_float64(n)))))))), num_result())
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def decode_integer(it: hydra.core.IntegerType, value: hydra.json.model.Value) -> Either[str, hydra.core.Term]:
    r"""Decode a JSON value to an integer term. Small ints from numbers; large ints from strings."""
    
    match it:
        case hydra.core.IntegerType.BIGINT:
            def str_result() -> Either[str, str]:
                return expect_string(value)
            return hydra.lib.eithers.either((lambda err: Left(err)), (lambda s: (parsed := hydra.lib.literals.read_bigint(s), hydra.lib.maybes.maybe(Left(hydra.lib.strings.cat(("invalid bigint: ", s))), (lambda v: Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueBigint(v)))))))), parsed))[1]), str_result())
        
        case hydra.core.IntegerType.INT64:
            def str_result() -> Either[str, str]:
                return expect_string(value)
            return hydra.lib.eithers.either((lambda err: Left(err)), (lambda s: (parsed := hydra.lib.literals.read_int64(s), hydra.lib.maybes.maybe(Left(hydra.lib.strings.cat(("invalid int64: ", s))), (lambda v: Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(v)))))))), parsed))[1]), str_result())
        
        case hydra.core.IntegerType.UINT32:
            def str_result() -> Either[str, str]:
                return expect_string(value)
            return hydra.lib.eithers.either((lambda err: Left(err)), (lambda s: (parsed := hydra.lib.literals.read_uint32(s), hydra.lib.maybes.maybe(Left(hydra.lib.strings.cat(("invalid uint32: ", s))), (lambda v: Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint32(v)))))))), parsed))[1]), str_result())
        
        case hydra.core.IntegerType.UINT64:
            def str_result() -> Either[str, str]:
                return expect_string(value)
            return hydra.lib.eithers.either((lambda err: Left(err)), (lambda s: (parsed := hydra.lib.literals.read_uint64(s), hydra.lib.maybes.maybe(Left(hydra.lib.strings.cat(("invalid uint64: ", s))), (lambda v: Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint64(v)))))))), parsed))[1]), str_result())
        
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

def decode_literal(lt: hydra.core.LiteralType, value: hydra.json.model.Value) -> Either[str, hydra.core.Term]:
    def _hoist_hydra_json_decode_decode_literal_1(v1: hydra.json.model.Value) -> Either[str, hydra.core.Term]:
        match v1:
            case hydra.json.model.ValueBoolean(value=b):
                return Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(b)))))
            
            case _:
                return Left("expected boolean")
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

def expect_array(value: hydra.json.model.Value) -> Either[str, frozenlist[hydra.json.model.Value]]:
    r"""Extract an array from a JSON value."""
    
    match value:
        case hydra.json.model.ValueArray(value=arr):
            return Right(arr)
        
        case _:
            return Left("expected array")

def expect_object(value: hydra.json.model.Value) -> Either[str, FrozenDict[str, hydra.json.model.Value]]:
    r"""Extract an object from a JSON value."""
    
    match value:
        case hydra.json.model.ValueObject(value=obj):
            return Right(obj)
        
        case _:
            return Left("expected object")

def from_json(types: FrozenDict[hydra.core.Name, hydra.core.Type], typ: hydra.core.Type, value: hydra.json.model.Value) -> Either[str, hydra.core.Term]:
    r"""Decode a JSON value to a Hydra term given a type. Returns Left for type mismatches."""
    
    stripped = hydra.rewriting.deannotate_type(typ)
    match stripped:
        case hydra.core.TypeLiteral(value=lt):
            return decode_literal(lt, value)
        
        case hydra.core.TypeList(value=elem_type):
            def decode_elem(v: hydra.json.model.Value) -> Either[str, hydra.core.Term]:
                return from_json(types, elem_type, v)
            def arr_result() -> Either[str, frozenlist[hydra.json.model.Value]]:
                return expect_array(value)
            return hydra.lib.eithers.either((lambda err: Left(err)), (lambda arr: (decoded := (lambda : hydra.lib.eithers.map_list(decode_elem, arr)), hydra.lib.eithers.map((lambda ts: cast(hydra.core.Term, hydra.core.TermList(ts))), decoded()))[1]), arr_result())
        
        case hydra.core.TypeSet(value=elem_type2):
            def decode_elem(v: hydra.json.model.Value) -> Either[str, hydra.core.Term]:
                return from_json(types, elem_type2, v)
            def arr_result() -> Either[str, frozenlist[hydra.json.model.Value]]:
                return expect_array(value)
            return hydra.lib.eithers.either((lambda err: Left(err)), (lambda arr: (decoded := (lambda : hydra.lib.eithers.map_list(decode_elem, arr)), hydra.lib.eithers.map((lambda elems: cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(elems)))), decoded()))[1]), arr_result())
        
        case hydra.core.TypeMaybe(value=inner_type):
            def decode_just(arr: frozenlist[hydra.json.model.Value]) -> Either[str, hydra.core.Term]:
                return hydra.lib.eithers.map((lambda v: cast(hydra.core.Term, hydra.core.TermMaybe(Just(v)))), from_json(types, inner_type, hydra.lib.lists.head(arr)))
            def decode_maybe_array(arr: frozenlist[hydra.json.model.Value]) -> Either[str, hydra.core.Term]:
                def len() -> int:
                    return hydra.lib.lists.length(arr)
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(len(), 0), (lambda : Right(cast(hydra.core.Term, hydra.core.TermMaybe(Nothing())))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(len(), 1), (lambda : decode_just(arr)), (lambda : Left("expected single-element array for Just")))))
            match value:
                case hydra.json.model.ValueNull():
                    return Right(cast(hydra.core.Term, hydra.core.TermMaybe(Nothing())))
                
                case hydra.json.model.ValueArray(value=arr):
                    return decode_maybe_array(arr)
                
                case _:
                    return Left("expected null or single-element array for Maybe")
        
        case hydra.core.TypeRecord(value=rt):
            def obj_result() -> Either[str, FrozenDict[str, hydra.json.model.Value]]:
                return expect_object(value)
            return hydra.lib.eithers.either((lambda err: Left(err)), (lambda obj: (decode_field := (lambda ft: (fname := ft.name, ftype := ft.type, mval := (lambda : hydra.lib.maps.lookup(fname.value, obj)), default_val := cast(hydra.json.model.Value, hydra.json.model.ValueNull()), json_val := (lambda : hydra.lib.maybes.from_maybe(default_val, mval())), decoded := (lambda : from_json(types, ftype, json_val())), hydra.lib.eithers.map((lambda v: hydra.core.Field(fname, v)), decoded()))[6]), fields := rt.fields, decoded_fields := (lambda : hydra.lib.eithers.map_list(decode_field, fields)), hydra.lib.eithers.map((lambda fs: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(rt.type_name, fs)))), decoded_fields()))[3]), obj_result())
        
        case hydra.core.TypeUnion(value=rt2):
            def decode_variant(key: str, val: Maybe[hydra.json.model.Value], ftype: hydra.core.Type) -> Either[str, hydra.core.Term]:
                def json_val() -> hydra.core.Type:
                    return hydra.lib.maybes.from_maybe(cast(hydra.json.model.Value, hydra.json.model.ValueNull()), val)
                def decoded() -> Either[str, hydra.core.Term]:
                    return from_json(types, ftype, json_val())
                return hydra.lib.eithers.map((lambda v: cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(rt2.type_name, hydra.core.Field(hydra.core.Name(key), v))))), decoded())
            def try_field(key: str, val: Maybe[hydra.json.model.Value], ft: hydra.core.FieldType) -> Maybe[Either[str, hydra.core.Term]]:
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(ft.name.value, key), (lambda : Just(decode_variant(key, val, ft.type))), (lambda : Nothing()))
            def find_and_decode(key: str, val: Maybe[hydra.json.model.Value], fts: frozenlist[hydra.core.FieldType]) -> Either[str, hydra.core.Term]:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(fts), (lambda : Left(hydra.lib.strings.cat(("unknown variant: ", key)))), (lambda : hydra.lib.maybes.maybe(find_and_decode(key, val, hydra.lib.lists.tail(fts)), (lambda r: r), try_field(key, val, hydra.lib.lists.head(fts)))))
            def decode_single_key(obj: FrozenDict[str, hydra.json.model.Value]) -> Either[str, hydra.core.Term]:
                return find_and_decode(hydra.lib.lists.head(hydra.lib.maps.keys(obj)), hydra.lib.maps.lookup(hydra.lib.lists.head(hydra.lib.maps.keys(obj)), obj), rt2.fields)
            def process_union(obj: FrozenDict[str, hydra.json.model.Value]) -> Either[str, hydra.core.Term]:
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(hydra.lib.maps.keys(obj)), 1), (lambda : decode_single_key(obj)), (lambda : Left("expected single-key object for union")))
            def obj_result() -> Either[str, FrozenDict[str, hydra.json.model.Value]]:
                return expect_object(value)
            return hydra.lib.eithers.either((lambda err: Left(err)), (lambda obj: process_union(obj)), obj_result())
        
        case hydra.core.TypeUnit():
            def obj_result() -> Either[str, FrozenDict[str, hydra.json.model.Value]]:
                return expect_object(value)
            return hydra.lib.eithers.map((lambda _2: cast(hydra.core.Term, hydra.core.TermUnit())), obj_result())
        
        case hydra.core.TypeWrap(value=wn):
            def extract_inner_type(lt: hydra.core.Type) -> hydra.core.Type:
                match lt:
                    case hydra.core.TypeWrap(value=wt):
                        return wt.body
                    
                    case _:
                        return lt
            def decode_and_wrap(lt: hydra.core.Type) -> Either[str, hydra.core.Term]:
                def inner_type() -> hydra.core.Type:
                    return extract_inner_type(lt)
                def decoded() -> Either[str, hydra.core.Term]:
                    return from_json(types, inner_type(), value)
                return hydra.lib.eithers.map((lambda v: cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(wn.type_name, v)))), decoded())
            def looked_up() -> Maybe[hydra.core.Type]:
                return hydra.lib.maps.lookup(wn.type_name, types)
            return hydra.lib.maybes.maybe(Left(hydra.lib.strings.cat(("unknown wrapped type: ", wn.type_name.value))), (lambda lt: decode_and_wrap(lt)), looked_up())
        
        case hydra.core.TypeMap(value=mt):
            key_type = mt.keys
            val_type = mt.values
            def arr_result() -> Either[str, frozenlist[hydra.json.model.Value]]:
                return expect_array(value)
            return hydra.lib.eithers.either((lambda err: Left(err)), (lambda arr: (decode_entry := (lambda entry_json: (obj_result := (lambda : expect_object(entry_json)), hydra.lib.eithers.either((lambda err: Left(err)), (lambda entry_obj: (key_json := (lambda : hydra.lib.maps.lookup("@key", entry_obj)), val_json := (lambda : hydra.lib.maps.lookup("@value", entry_obj)), hydra.lib.maybes.maybe(Left("missing @key in map entry"), (lambda kj: hydra.lib.maybes.maybe(Left("missing @value in map entry"), (lambda vj: (decoded_key := (lambda : from_json(types, key_type, kj)), decoded_val := (lambda : from_json(types, val_type, vj)), hydra.lib.eithers.either((lambda err: Left(err)), (lambda k: hydra.lib.eithers.map((lambda v: (k, v)), decoded_val())), decoded_key()))[2]), val_json())), key_json()))[2]), obj_result()))[1]), entries := (lambda : hydra.lib.eithers.map_list(decode_entry, arr)), hydra.lib.eithers.map((lambda es: cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(es)))), entries()))[2]), arr_result())
        
        case hydra.core.TypePair(value=pt):
            first_type = pt.first
            second_type = pt.second
            def obj_result() -> Either[str, FrozenDict[str, hydra.json.model.Value]]:
                return expect_object(value)
            return hydra.lib.eithers.either((lambda err: Left(err)), (lambda obj: (first_json := (lambda : hydra.lib.maps.lookup("@first", obj)), second_json := (lambda : hydra.lib.maps.lookup("@second", obj)), hydra.lib.maybes.maybe(Left("missing @first in pair"), (lambda fj: hydra.lib.maybes.maybe(Left("missing @second in pair"), (lambda sj: (decoded_first := (lambda : from_json(types, first_type, fj)), decoded_second := (lambda : from_json(types, second_type, sj)), hydra.lib.eithers.either((lambda err: Left(err)), (lambda f: hydra.lib.eithers.map((lambda s: cast(hydra.core.Term, hydra.core.TermPair((f, s)))), decoded_second())), decoded_first()))[2]), second_json())), first_json()))[2]), obj_result())
        
        case hydra.core.TypeEither(value=et):
            left_type = et.left
            right_type = et.right
            def obj_result() -> Either[str, FrozenDict[str, hydra.json.model.Value]]:
                return expect_object(value)
            return hydra.lib.eithers.either((lambda err: Left(err)), (lambda obj: (left_json := (lambda : hydra.lib.maps.lookup("@left", obj)), right_json := (lambda : hydra.lib.maps.lookup("@right", obj)), hydra.lib.maybes.maybe(hydra.lib.maybes.maybe(Left("expected @left or @right in Either"), (lambda rj: (decoded := (lambda : from_json(types, right_type, rj)), hydra.lib.eithers.map((lambda v: cast(hydra.core.Term, hydra.core.TermEither(Right(v)))), decoded()))[1]), right_json()), (lambda lj: (decoded := (lambda : from_json(types, left_type, lj)), hydra.lib.eithers.map((lambda v: cast(hydra.core.Term, hydra.core.TermEither(Left(v)))), decoded()))[1]), left_json()))[2]), obj_result())
        
        case hydra.core.TypeVariable(value=name):
            def looked_up() -> Maybe[hydra.core.Type]:
                return hydra.lib.maps.lookup(name, types)
            return hydra.lib.maybes.maybe(Left(hydra.lib.strings.cat(("unknown type variable: ", name.value))), (lambda resolved_type: from_json(types, resolved_type, value)), looked_up())
        
        case _:
            return Left(hydra.lib.strings.cat(("unsupported type for JSON decoding: ", hydra.show.core.type(typ))))
