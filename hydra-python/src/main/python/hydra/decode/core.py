# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.core."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Right, frozenlist
from typing import cast
import hydra.core
import hydra.extract.helpers
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.util

def name(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Name]:
    def _hoist_hydra_decode_core_name_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)
            
            case _:
                return Left(hydra.util.DecodingError("expected string literal"))
    def _hoist_hydra_decode_core_name_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_core_name_1(v)
            
            case _:
                return Left(hydra.util.DecodingError("expected literal"))
    def _hoist_hydra_decode_core_name_3(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Name]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.core.Name(b)), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_hydra_decode_core_name_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.core.Name"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_name_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def projection(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Projection]:
    def _hoist_hydra_decode_core_projection_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Projection]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("typeName", name, field_map(), cx), (lambda field_type_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("field", name, field_map(), cx), (lambda field_field: Right(hydra.core.Projection(field_type_name, field_field))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.Projection"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_projection_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def float_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.FloatType]:
    def _hoist_hydra_decode_core_float_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.FloatType]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                def tname() -> hydra.core.Type:
                    return inj.type_name
                def field() -> hydra.core.Type:
                    return inj.field
                def fname() -> hydra.core.Type:
                    return field().name
                def fterm() -> hydra.core.Type:
                    return field().term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.core.FloatType]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("bigfloat"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.core.FloatType.BIGFLOAT), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("float32"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.core.FloatType.FLOAT32), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("float64"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.core.FloatType.FLOAT64), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.core.FloatType"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_float_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def integer_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.IntegerType]:
    def _hoist_hydra_decode_core_integer_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.IntegerType]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                def tname() -> hydra.core.Type:
                    return inj.type_name
                def field() -> hydra.core.Type:
                    return inj.field
                def fname() -> hydra.core.Type:
                    return field().name
                def fterm() -> hydra.core.Type:
                    return field().term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.core.IntegerType]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("bigint"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.core.IntegerType.BIGINT), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("int8"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.core.IntegerType.INT8), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("int16"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.core.IntegerType.INT16), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("int32"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.core.IntegerType.INT32), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("int64"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.core.IntegerType.INT64), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("uint8"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.core.IntegerType.UINT8), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("uint16"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.core.IntegerType.UINT16), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("uint32"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.core.IntegerType.UINT32), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("uint64"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.core.IntegerType.UINT64), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.core.IntegerType"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_integer_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def literal_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.LiteralType]:
    def _hoist_hydra_decode_core_literal_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.LiteralType]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                def tname() -> hydra.core.Type:
                    return inj.type_name
                def field() -> hydra.core.Type:
                    return inj.field
                def fname() -> hydra.core.Type:
                    return field().name
                def fterm() -> hydra.core.Type:
                    return field().term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.core.LiteralType]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("binary"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.LiteralType, hydra.core.LiteralTypeBinary())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("boolean"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.LiteralType, hydra.core.LiteralTypeBoolean())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("float"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(t))), float_type(cx, input)))), (hydra.core.Name("integer"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(t))), integer_type(cx, input)))), (hydra.core.Name("string"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.core.LiteralType"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_literal_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_variable_metadata(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.TypeVariableMetadata]:
    def _hoist_hydra_decode_core_type_variable_metadata_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.TypeVariableMetadata]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("classes", (lambda v1, v2: hydra.extract.helpers.decode_set(name, v1, v2)), field_map(), cx), (lambda field_classes: Right(hydra.core.TypeVariableMetadata(field_classes))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.TypeVariableMetadata"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_type_variable_metadata_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def float_value(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.FloatValue]:
    def _hoist_hydra_decode_core_float_value_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.FloatValue]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                def tname() -> hydra.core.Type:
                    return inj.type_name
                def field() -> hydra.core.Type:
                    return inj.field
                def fname() -> hydra.core.Type:
                    return field().name
                def fterm() -> hydra.core.Type:
                    return field().term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.core.FloatValue]]]:
                    def _hoist_variant_map_1(v1: hydra.core.FloatValue) -> Either[hydra.util.DecodingError, Decimal]:
                        match v1:
                            case hydra.core.FloatValueBigfloat(value=f):
                                return Right(f)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected bigfloat value"))
                    def _hoist_variant_map_2(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, Decimal]:
                        match v1:
                            case hydra.core.LiteralFloat(value=v1):
                                return _hoist_variant_map_1(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected bigfloat literal"))
                    def _hoist_variant_map_3(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, Decimal]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_2(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_4(v1: hydra.core.FloatValue) -> Either[hydra.util.DecodingError, float]:
                        match v1:
                            case hydra.core.FloatValueFloat32(value=f):
                                return Right(f)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected float32 value"))
                    def _hoist_variant_map_5(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, float]:
                        match v1:
                            case hydra.core.LiteralFloat(value=v1):
                                return _hoist_variant_map_4(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected float32 literal"))
                    def _hoist_variant_map_6(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, float]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_5(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_7(v1: hydra.core.FloatValue) -> Either[hydra.util.DecodingError, float]:
                        match v1:
                            case hydra.core.FloatValueFloat64(value=f):
                                return Right(f)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected float64 value"))
                    def _hoist_variant_map_8(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, float]:
                        match v1:
                            case hydra.core.LiteralFloat(value=v1):
                                return _hoist_variant_map_7(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected float64 literal"))
                    def _hoist_variant_map_9(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, float]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_8(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    return hydra.lib.maps.from_list(((hydra.core.Name("bigfloat"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.FloatValue, hydra.core.FloatValueBigfloat(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_3(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("float32"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.FloatValue, hydra.core.FloatValueFloat32(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_6(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("float64"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.FloatValue, hydra.core.FloatValueFloat64(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_9(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.core.FloatValue"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_float_value_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def integer_value(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.IntegerValue]:
    def _hoist_hydra_decode_core_integer_value_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.IntegerValue]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                def tname() -> hydra.core.Type:
                    return inj.type_name
                def field() -> hydra.core.Type:
                    return inj.field
                def fname() -> hydra.core.Type:
                    return field().name
                def fterm() -> hydra.core.Type:
                    return field().term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.core.IntegerValue]]]:
                    def _hoist_variant_map_1(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.IntegerValueBigint(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected bigint value"))
                    def _hoist_variant_map_2(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.LiteralInteger(value=v1):
                                return _hoist_variant_map_1(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected bigint literal"))
                    def _hoist_variant_map_3(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_2(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_4(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.IntegerValueInt8(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int8 value"))
                    def _hoist_variant_map_5(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.LiteralInteger(value=v1):
                                return _hoist_variant_map_4(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int8 literal"))
                    def _hoist_variant_map_6(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_5(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_7(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.IntegerValueInt16(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int16 value"))
                    def _hoist_variant_map_8(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.LiteralInteger(value=v1):
                                return _hoist_variant_map_7(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int16 literal"))
                    def _hoist_variant_map_9(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_8(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_10(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.IntegerValueInt32(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 value"))
                    def _hoist_variant_map_11(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.LiteralInteger(value=v1):
                                return _hoist_variant_map_10(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 literal"))
                    def _hoist_variant_map_12(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_11(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_13(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.IntegerValueInt64(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int64 value"))
                    def _hoist_variant_map_14(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.LiteralInteger(value=v1):
                                return _hoist_variant_map_13(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int64 literal"))
                    def _hoist_variant_map_15(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_14(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_16(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.IntegerValueUint8(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected uint8 value"))
                    def _hoist_variant_map_17(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.LiteralInteger(value=v1):
                                return _hoist_variant_map_16(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected uint8 literal"))
                    def _hoist_variant_map_18(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_17(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_19(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.IntegerValueUint16(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected uint16 value"))
                    def _hoist_variant_map_20(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.LiteralInteger(value=v1):
                                return _hoist_variant_map_19(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected uint16 literal"))
                    def _hoist_variant_map_21(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_20(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_22(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.IntegerValueUint32(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected uint32 value"))
                    def _hoist_variant_map_23(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.LiteralInteger(value=v1):
                                return _hoist_variant_map_22(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected uint32 literal"))
                    def _hoist_variant_map_24(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_23(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_25(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.IntegerValueUint64(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected uint64 value"))
                    def _hoist_variant_map_26(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.LiteralInteger(value=v1):
                                return _hoist_variant_map_25(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected uint64 literal"))
                    def _hoist_variant_map_27(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_26(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    return hydra.lib.maps.from_list(((hydra.core.Name("bigint"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.IntegerValue, hydra.core.IntegerValueBigint(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_3(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("int8"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt8(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_6(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("int16"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt16(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_9(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("int32"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_12(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("int64"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_15(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("uint8"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint8(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_18(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("uint16"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint16(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_21(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("uint32"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint32(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_24(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("uint64"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint64(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_27(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.core.IntegerValue"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_integer_value_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def literal(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Literal]:
    def _hoist_hydra_decode_core_literal_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Literal]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                def tname() -> hydra.core.Type:
                    return inj.type_name
                def field() -> hydra.core.Type:
                    return inj.field
                def fname() -> hydra.core.Type:
                    return field().name
                def fterm() -> hydra.core.Type:
                    return field().term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.core.Literal]]]:
                    def _hoist_variant_map_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, bytes]:
                        match v1:
                            case hydra.core.LiteralBinary(value=b):
                                return Right(b)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected binary literal"))
                    def _hoist_variant_map_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, bytes]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_1(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_3(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, bool]:
                        match v1:
                            case hydra.core.LiteralBoolean(value=b):
                                return Right(b)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected boolean literal"))
                    def _hoist_variant_map_4(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, bool]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_3(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    def _hoist_variant_map_5(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                        match v1:
                            case hydra.core.LiteralString(value=s):
                                return Right(s)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected string literal"))
                    def _hoist_variant_map_6(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_5(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    return hydra.lib.maps.from_list(((hydra.core.Name("binary"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Literal, hydra.core.LiteralBinary(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("boolean"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Literal, hydra.core.LiteralBoolean(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_4(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input)))), (hydra.core.Name("float"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Literal, hydra.core.LiteralFloat(t))), float_value(cx, input)))), (hydra.core.Name("integer"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Literal, hydra.core.LiteralInteger(t))), integer_value(cx, input)))), (hydra.core.Name("string"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Literal, hydra.core.LiteralString(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_6(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.core.Literal"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_literal_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def annotated_term(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.AnnotatedTerm]:
    def _hoist_hydra_decode_core_annotated_term_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.AnnotatedTerm]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("body", term, field_map(), cx), (lambda field_body: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("annotation", (lambda v1, v2: hydra.extract.helpers.decode_map(name, term, v1, v2)), field_map(), cx), (lambda field_annotation: Right(hydra.core.AnnotatedTerm(field_body, field_annotation))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.AnnotatedTerm"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_annotated_term_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def annotated_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.AnnotatedType]:
    def _hoist_hydra_decode_core_annotated_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.AnnotatedType]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("body", type, field_map(), cx), (lambda field_body: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("annotation", (lambda v1, v2: hydra.extract.helpers.decode_map(name, term, v1, v2)), field_map(), cx), (lambda field_annotation: Right(hydra.core.AnnotatedType(field_body, field_annotation))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.AnnotatedType"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_annotated_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def application(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Application]:
    def _hoist_hydra_decode_core_application_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Application]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("function", term, field_map(), cx), (lambda field_function: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("argument", term, field_map(), cx), (lambda field_argument: Right(hydra.core.Application(field_function, field_argument))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.Application"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_application_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def application_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.ApplicationType]:
    def _hoist_hydra_decode_core_application_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.ApplicationType]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("function", type, field_map(), cx), (lambda field_function: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("argument", type, field_map(), cx), (lambda field_argument: Right(hydra.core.ApplicationType(field_function, field_argument))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.ApplicationType"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_application_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def binding(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Binding]:
    def _hoist_hydra_decode_core_binding_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Binding]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", name, field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("term", term, field_map(), cx), (lambda field_term: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", (lambda v1, v2: hydra.extract.helpers.decode_maybe(type_scheme, v1, v2)), field_map(), cx), (lambda field_type: Right(hydra.core.Binding(field_name, field_term, field_type))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.Binding"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_binding_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def case_statement(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.CaseStatement]:
    def _hoist_hydra_decode_core_case_statement_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.CaseStatement]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("typeName", name, field_map(), cx), (lambda field_type_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("default", (lambda v1, v2: hydra.extract.helpers.decode_maybe(term, v1, v2)), field_map(), cx), (lambda field_default: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("cases", (lambda v1, v2: hydra.extract.helpers.decode_list(field, v1, v2)), field_map(), cx), (lambda field_cases: Right(hydra.core.CaseStatement(field_type_name, field_default, field_cases))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.CaseStatement"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_case_statement_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def either_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.EitherType]:
    def _hoist_hydra_decode_core_either_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.EitherType]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("left", type, field_map(), cx), (lambda field_left: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("right", type, field_map(), cx), (lambda field_right: Right(hydra.core.EitherType(field_left, field_right))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.EitherType"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_either_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def elimination(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Elimination]:
    def _hoist_hydra_decode_core_elimination_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Elimination]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                def tname() -> hydra.core.Type:
                    return inj.type_name
                def field() -> hydra.core.Type:
                    return inj.field
                def fname() -> hydra.core.Type:
                    return field().name
                def fterm() -> hydra.core.Type:
                    return field().term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.core.Elimination]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("record"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Elimination, hydra.core.EliminationRecord(t))), projection(cx, input)))), (hydra.core.Name("union"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Elimination, hydra.core.EliminationUnion(t))), case_statement(cx, input)))), (hydra.core.Name("wrap"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Elimination, hydra.core.EliminationWrap(t))), name(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.core.Elimination"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_elimination_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def field(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Field]:
    def _hoist_hydra_decode_core_field_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Field]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", name, field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("term", term, field_map(), cx), (lambda field_term: Right(hydra.core.Field(field_name, field_term))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.Field"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_field_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def field_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.FieldType]:
    def _hoist_hydra_decode_core_field_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.FieldType]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", name, field_map(), cx), (lambda field_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", type, field_map(), cx), (lambda field_type: Right(hydra.core.FieldType(field_name, field_type))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.FieldType"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_field_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def forall_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.ForallType]:
    def _hoist_hydra_decode_core_forall_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.ForallType]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("parameter", name, field_map(), cx), (lambda field_parameter: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("body", type, field_map(), cx), (lambda field_body: Right(hydra.core.ForallType(field_parameter, field_body))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.ForallType"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_forall_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def function(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Function]:
    def _hoist_hydra_decode_core_function_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Function]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                def tname() -> hydra.core.Type:
                    return inj.type_name
                def field() -> hydra.core.Type:
                    return inj.field
                def fname() -> hydra.core.Type:
                    return field().name
                def fterm() -> hydra.core.Type:
                    return field().term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.core.Function]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("elimination"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Function, hydra.core.FunctionElimination(t))), elimination(cx, input)))), (hydra.core.Name("lambda"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Function, hydra.core.FunctionLambda(t))), lambda_(cx, input)))), (hydra.core.Name("primitive"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Function, hydra.core.FunctionPrimitive(t))), name(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.core.Function"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_function_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def function_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.FunctionType]:
    def _hoist_hydra_decode_core_function_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.FunctionType]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("domain", type, field_map(), cx), (lambda field_domain: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("codomain", type, field_map(), cx), (lambda field_codomain: Right(hydra.core.FunctionType(field_domain, field_codomain))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.FunctionType"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_function_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def injection(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Injection]:
    def _hoist_hydra_decode_core_injection_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Injection]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("typeName", name, field_map(), cx), (lambda field_type_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("field", field, field_map(), cx), (lambda field_field: Right(hydra.core.Injection(field_type_name, field_field))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.Injection"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_injection_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def lambda_(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Lambda]:
    def _hoist_hydra_decode_core_lambda_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Lambda]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("parameter", name, field_map(), cx), (lambda field_parameter: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("domain", (lambda v1, v2: hydra.extract.helpers.decode_maybe(type, v1, v2)), field_map(), cx), (lambda field_domain: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("body", term, field_map(), cx), (lambda field_body: Right(hydra.core.Lambda(field_parameter, field_domain, field_body))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.Lambda"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_lambda_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def let(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Let]:
    def _hoist_hydra_decode_core_let_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Let]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("bindings", (lambda v1, v2: hydra.extract.helpers.decode_list(binding, v1, v2)), field_map(), cx), (lambda field_bindings: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("body", term, field_map(), cx), (lambda field_body: Right(hydra.core.Let(field_bindings, field_body))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.Let"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_let_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def map_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.MapType]:
    def _hoist_hydra_decode_core_map_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.MapType]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("keys", type, field_map(), cx), (lambda field_keys: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("values", type, field_map(), cx), (lambda field_values: Right(hydra.core.MapType(field_keys, field_values))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.MapType"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_map_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def pair_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.PairType]:
    def _hoist_hydra_decode_core_pair_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.PairType]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("first", type, field_map(), cx), (lambda field_first: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("second", type, field_map(), cx), (lambda field_second: Right(hydra.core.PairType(field_first, field_second))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.PairType"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_pair_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def record(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Record]:
    def _hoist_hydra_decode_core_record_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Record]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("typeName", name, field_map(), cx), (lambda field_type_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("fields", (lambda v1, v2: hydra.extract.helpers.decode_list(field, v1, v2)), field_map(), cx), (lambda field_fields: Right(hydra.core.Record(field_type_name, field_fields))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.Record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_record_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def row_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.RowType]:
    def _hoist_hydra_decode_core_row_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.RowType]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("typeName", name, field_map(), cx), (lambda field_type_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("fields", (lambda v1, v2: hydra.extract.helpers.decode_list(field_type, v1, v2)), field_map(), cx), (lambda field_fields: Right(hydra.core.RowType(field_type_name, field_fields))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.RowType"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_row_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def term(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Term]:
    def _hoist_hydra_decode_core_term_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Term]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                def tname() -> hydra.core.Type:
                    return inj.type_name
                def field() -> hydra.core.Type:
                    return inj.field
                def fname() -> hydra.core.Type:
                    return field().name
                def fterm() -> hydra.core.Type:
                    return field().term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.core.Term]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("annotated"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermAnnotated(t))), annotated_term(cx, input)))), (hydra.core.Name("application"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermApplication(t))), application(cx, input)))), (hydra.core.Name("either"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermEither(t))), hydra.extract.helpers.decode_either(term, term, cx, input)))), (hydra.core.Name("function"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermFunction(t))), function(cx, input)))), (hydra.core.Name("let"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermLet(t))), let(cx, input)))), (hydra.core.Name("list"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermList(t))), hydra.extract.helpers.decode_list(term, cx, input)))), (hydra.core.Name("literal"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermLiteral(t))), literal(cx, input)))), (hydra.core.Name("map"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermMap(t))), hydra.extract.helpers.decode_map(term, term, cx, input)))), (hydra.core.Name("maybe"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermMaybe(t))), hydra.extract.helpers.decode_maybe(term, cx, input)))), (hydra.core.Name("pair"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermPair(t))), hydra.extract.helpers.decode_pair(term, term, cx, input)))), (hydra.core.Name("record"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermRecord(t))), record(cx, input)))), (hydra.core.Name("set"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermSet(t))), hydra.extract.helpers.decode_set(term, cx, input)))), (hydra.core.Name("typeApplication"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermTypeApplication(t))), type_application_term(cx, input)))), (hydra.core.Name("typeLambda"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermTypeLambda(t))), type_lambda(cx, input)))), (hydra.core.Name("union"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermUnion(t))), injection(cx, input)))), (hydra.core.Name("unit"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermUnit())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("variable"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermVariable(t))), name(cx, input)))), (hydra.core.Name("wrap"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Term, hydra.core.TermWrap(t))), wrapped_term(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.core.Term"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_term_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Type]:
    def _hoist_hydra_decode_core_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.Type]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                def tname() -> hydra.core.Type:
                    return inj.type_name
                def field() -> hydra.core.Type:
                    return inj.field
                def fname() -> hydra.core.Type:
                    return field().name
                def fterm() -> hydra.core.Type:
                    return field().term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.core.Type]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("annotated"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypeAnnotated(t))), annotated_type(cx, input)))), (hydra.core.Name("application"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypeApplication(t))), application_type(cx, input)))), (hydra.core.Name("either"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypeEither(t))), either_type(cx, input)))), (hydra.core.Name("forall"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypeForall(t))), forall_type(cx, input)))), (hydra.core.Name("function"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypeFunction(t))), function_type(cx, input)))), (hydra.core.Name("list"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypeList(t))), type(cx, input)))), (hydra.core.Name("literal"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypeLiteral(t))), literal_type(cx, input)))), (hydra.core.Name("map"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypeMap(t))), map_type(cx, input)))), (hydra.core.Name("maybe"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypeMaybe(t))), type(cx, input)))), (hydra.core.Name("pair"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypePair(t))), pair_type(cx, input)))), (hydra.core.Name("record"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypeRecord(t))), row_type(cx, input)))), (hydra.core.Name("set"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypeSet(t))), type(cx, input)))), (hydra.core.Name("union"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypeUnion(t))), row_type(cx, input)))), (hydra.core.Name("unit"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypeUnit())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("variable"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypeVariable(t))), name(cx, input)))), (hydra.core.Name("wrap"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.core.Type, hydra.core.TypeWrap(t))), wrapped_type(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.core.Type"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_application_term(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.TypeApplicationTerm]:
    def _hoist_hydra_decode_core_type_application_term_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.TypeApplicationTerm]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("body", term, field_map(), cx), (lambda field_body: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", type, field_map(), cx), (lambda field_type: Right(hydra.core.TypeApplicationTerm(field_body, field_type))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.TypeApplicationTerm"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_type_application_term_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_lambda(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.TypeLambda]:
    def _hoist_hydra_decode_core_type_lambda_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.TypeLambda]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("parameter", name, field_map(), cx), (lambda field_parameter: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("body", term, field_map(), cx), (lambda field_body: Right(hydra.core.TypeLambda(field_parameter, field_body))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.TypeLambda"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_type_lambda_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_scheme(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.TypeScheme]:
    def _hoist_hydra_decode_core_type_scheme_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.TypeScheme]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("variables", (lambda v1, v2: hydra.extract.helpers.decode_list(name, v1, v2)), field_map(), cx), (lambda field_variables: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", type, field_map(), cx), (lambda field_type: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("constraints", (lambda v1, v2: hydra.extract.helpers.decode_maybe((lambda v1, v2: hydra.extract.helpers.decode_map(name, type_variable_metadata, v1, v2)), v1, v2)), field_map(), cx), (lambda field_constraints: Right(hydra.core.TypeScheme(field_variables, field_type, field_constraints))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.TypeScheme"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_type_scheme_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def wrapped_term(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.WrappedTerm]:
    def _hoist_hydra_decode_core_wrapped_term_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.WrappedTerm]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("typeName", name, field_map(), cx), (lambda field_type_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("body", term, field_map(), cx), (lambda field_body: Right(hydra.core.WrappedTerm(field_type_name, field_body))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.WrappedTerm"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_wrapped_term_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def wrapped_type(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.WrappedType]:
    def _hoist_hydra_decode_core_wrapped_type_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.core.WrappedType]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("typeName", name, field_map(), cx), (lambda field_type_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("body", type, field_map(), cx), (lambda field_body: Right(hydra.core.WrappedType(field_type_name, field_body))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.core.WrappedType"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_core_wrapped_type_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
