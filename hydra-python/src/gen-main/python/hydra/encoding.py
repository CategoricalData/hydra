# Note: this is an automatically generated file. Do not edit.

r"""Functions for generating term encoders from type modules."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Just, Maybe, Nothing, Right, frozenlist
from typing import cast
import hydra.annotations
import hydra.constants
import hydra.context
import hydra.core
import hydra.decode.core
import hydra.encode.core
import hydra.errors
import hydra.formatting
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.packaging
import hydra.predicates

def encode_binding_name(n: hydra.core.Name) -> hydra.core.Name:
    r"""Generate a binding name for an encoder function from a type name."""

    return hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.lists.tail(hydra.lib.strings.split_on(".", n.value)))), (lambda : hydra.core.Name(hydra.lib.strings.intercalate(".", hydra.lib.lists.concat2(("hydra", "encode"), hydra.lib.lists.concat2(hydra.lib.lists.tail(hydra.lib.lists.init(hydra.lib.strings.split_on(".", n.value))), (hydra.formatting.decapitalize(hydra.names.local_name_of(n)),)))))), (lambda : hydra.core.Name(hydra.formatting.decapitalize(hydra.names.local_name_of(n)))))

def encode_float_value(float_type: hydra.core.FloatType, val_term: hydra.core.Term) -> hydra.core.Term:
    def _hoist_hydra_encoding_encode_float_value_1(v1):
        match v1:
            case hydra.core.FloatType.BIGFLOAT:
                return hydra.core.Name("bigfloat")

            case hydra.core.FloatType.FLOAT32:
                return hydra.core.Name("float32")

            case hydra.core.FloatType.FLOAT64:
                return hydra.core.Name("float64")

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.FloatValue"), hydra.core.Field(_hoist_hydra_encoding_encode_float_value_1(float_type), val_term))))

def encode_integer_value(int_type: hydra.core.IntegerType, val_term: hydra.core.Term) -> hydra.core.Term:
    def _hoist_hydra_encoding_encode_integer_value_1(v1):
        match v1:
            case hydra.core.IntegerType.BIGINT:
                return hydra.core.Name("bigint")

            case hydra.core.IntegerType.INT8:
                return hydra.core.Name("int8")

            case hydra.core.IntegerType.INT16:
                return hydra.core.Name("int16")

            case hydra.core.IntegerType.INT32:
                return hydra.core.Name("int32")

            case hydra.core.IntegerType.INT64:
                return hydra.core.Name("int64")

            case hydra.core.IntegerType.UINT8:
                return hydra.core.Name("uint8")

            case hydra.core.IntegerType.UINT16:
                return hydra.core.Name("uint16")

            case hydra.core.IntegerType.UINT32:
                return hydra.core.Name("uint32")

            case hydra.core.IntegerType.UINT64:
                return hydra.core.Name("uint64")

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.IntegerValue"), hydra.core.Field(_hoist_hydra_encoding_encode_integer_value_1(int_type), val_term))))

def encode_literal_type(v1: hydra.core.LiteralType) -> hydra.core.Term:
    r"""Generate an encoder for a literal type."""

    match v1:
        case hydra.core.LiteralTypeBinary():
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("literal"), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("binary"), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x"))))))))))))))))

        case hydra.core.LiteralTypeBoolean():
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("literal"), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("boolean"), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x"))))))))))))))))

        case hydra.core.LiteralTypeString():
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("literal"), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("string"), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x"))))))))))))))))

        case hydra.core.LiteralTypeInteger(value=int_type):
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("literal"), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("integer"), encode_integer_value(int_type, cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x")))))))))))))))))

        case hydra.core.LiteralTypeFloat(value=float_type):
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("literal"), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Literal"), hydra.core.Field(hydra.core.Name("float"), encode_float_value(float_type, cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x")))))))))))))))))

        case _:
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), Nothing(), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x"))))))))

def encode_name(n: hydra.core.Name) -> hydra.core.Term:
    r"""Encode a Name as a term."""

    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.core.Name"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(n.value)))))))

def encode_injection(type_name: hydra.core.Name, field_name: hydra.core.Name, field_term: hydra.core.Term) -> hydra.core.Term:
    r"""Encode an Injection as a term."""

    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Injection"), (hydra.core.Field(hydra.core.Name("typeName"), encode_name(type_name)), hydra.core.Field(hydra.core.Name("field"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Field"), (hydra.core.Field(hydra.core.Name("name"), encode_name(field_name)), hydra.core.Field(hydra.core.Name("term"), field_term))))))))))

def encode_either_type(et: hydra.core.EitherType) -> hydra.core.Term:
    r"""Generate an encoder for an Either type."""

    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("e"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("either"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.eithers.bimap"))))), encode_type(et.left)))), encode_type(et.right)))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("e")))))))))))))))

def encode_field_value(type_name: hydra.core.Name, field_name: hydra.core.Name, field_type: hydra.core.Type) -> hydra.core.Term:
    r"""Generate the encoder for a field's value."""

    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("y"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("union"), encode_injection(type_name, field_name, cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(encode_type(field_type), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("y"))))))))))))))))

def encode_forall_type(ft: hydra.core.ForallType) -> hydra.core.Term:
    r"""Generate an encoder for a polymorphic (forall) type."""

    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(encode_binding_name(ft.parameter), Nothing(), encode_type(ft.body))))))

def encode_list_type(elem_type: hydra.core.Type) -> hydra.core.Term:
    r"""Generate an encoder for a list type."""

    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("xs"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("list"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.lists.map"))))), encode_type(elem_type)))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("xs")))))))))))))))

def encode_map_type(mt: hydra.core.MapType) -> hydra.core.Term:
    r"""Generate an encoder for a map type."""

    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("m"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("map"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.maps.bimap"))))), encode_type(mt.keys)))), encode_type(mt.values)))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("m")))))))))))))))

def encode_optional_type(elem_type: hydra.core.Type) -> hydra.core.Term:
    r"""Generate an encoder for a Maybe type."""

    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("opt"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("maybe"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.maybes.map"))))), encode_type(elem_type)))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("opt")))))))))))))))

def encode_pair_type(pt: hydra.core.PairType) -> hydra.core.Term:
    r"""Generate an encoder for a pair type."""

    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("p"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("pair"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.pairs.bimap"))))), encode_type(pt.first)))), encode_type(pt.second)))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("p")))))))))))))))

def encode_record_type(rt: frozenlist[hydra.core.FieldType]) -> hydra.core.Term:
    r"""Generate an encoder for a record type (unnamed — should not be called directly)."""

    return encode_record_type_named(hydra.core.Name("unknown"), rt)

def encode_record_type_named(ename: hydra.core.Name, rt: frozenlist[hydra.core.FieldType]) -> hydra.core.Term:
    r"""Generate an encoder for a record type with the given element name."""

    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("record"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Record"), (hydra.core.Field(hydra.core.Name("typeName"), encode_name(ename)), hydra.core.Field(hydra.core.Name("fields"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda ft: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Field"), (hydra.core.Field(hydra.core.Name("name"), encode_name(ft.name)), hydra.core.Field(hydra.core.Name("term"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(encode_type(ft.type), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(ename, ft.name))))))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x"))))))))))))))), rt)))))))))))))))))

def encode_set_type(elem_type: hydra.core.Type) -> hydra.core.Term:
    r"""Generate an encoder for a set type."""

    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("s"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("set"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.sets.map"))))), encode_type(elem_type)))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("s")))))))))))))))

def encode_type(v1: hydra.core.Type) -> hydra.core.Term:
    r"""Generate an encoder term for a Type."""

    match v1:
        case hydra.core.TypeAnnotated(value=at):
            return encode_type(at.body)

        case hydra.core.TypeApplication(value=app_type):
            return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(encode_type(app_type.function), encode_type(app_type.argument))))

        case hydra.core.TypeEither(value=et):
            return encode_either_type(et)

        case hydra.core.TypeForall(value=ft):
            return encode_forall_type(ft)

        case hydra.core.TypeFunction():
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), Nothing(), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x"))))))))

        case hydra.core.TypeList(value=elem_type):
            return encode_list_type(elem_type)

        case hydra.core.TypeLiteral(value=lt):
            return encode_literal_type(lt)

        case hydra.core.TypeMap(value=mt):
            return encode_map_type(mt)

        case hydra.core.TypeMaybe(value=elem_type2):
            return encode_optional_type(elem_type2)

        case hydra.core.TypePair(value=pt):
            return encode_pair_type(pt)

        case hydra.core.TypeRecord(value=rt):
            return encode_record_type(rt)

        case hydra.core.TypeSet(value=elem_type3):
            return encode_set_type(elem_type3)

        case hydra.core.TypeUnion(value=rt2):
            return encode_union_type(rt2)

        case hydra.core.TypeWrap(value=wt):
            return encode_wrapped_type(wt)

        case hydra.core.TypeUnit():
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("_"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("unit"), cast(hydra.core.Term, hydra.core.TermUnit()))))))))))

        case hydra.core.TypeVoid():
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("_"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("unit"), cast(hydra.core.Term, hydra.core.TermUnit()))))))))))

        case hydra.core.TypeVariable(value=type_name):
            return cast(hydra.core.Term, hydra.core.TermVariable(encode_binding_name(type_name)))

        case _:
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), Nothing(), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x"))))))))

def encode_union_type(rt: frozenlist[hydra.core.FieldType]) -> hydra.core.Term:
    r"""Generate an encoder for a union type (placeholder name)."""

    return encode_union_type_named(hydra.core.Name("unknown"), rt)

def encode_union_type_named(ename: hydra.core.Name, rt: frozenlist[hydra.core.FieldType]) -> hydra.core.Term:
    r"""Generate an encoder for a union type with the given element name."""

    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(ename, Nothing(), hydra.lib.lists.map((lambda ft: hydra.core.Field(ft.name, encode_field_value(ename, ft.name, ft.type))), rt))))))))

def encode_wrapped_type(wt: hydra.core.Type) -> hydra.core.Term:
    r"""Generate an encoder for a wrapped type (placeholder name)."""

    return encode_wrapped_type_named(hydra.core.Name("unknown"), wt)

def encode_wrapped_type_named(ename: hydra.core.Name, wt: hydra.core.Type) -> hydra.core.Term:
    r"""Generate an encoder for a wrapped type with the given element name."""

    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("wrap"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedTerm"), (hydra.core.Field(hydra.core.Name("typeName"), encode_name(ename)), hydra.core.Field(hydra.core.Name("body"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(encode_type(wt), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(ename)))))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x")))))))))))))))))))))))

def encode_type_named(ename: hydra.core.Name, typ: hydra.core.Type) -> hydra.core.Term:
    r"""Generate an encoder term for a Type, with the element name for nominal types."""

    match typ:
        case hydra.core.TypeAnnotated(value=at):
            return encode_type_named(ename, at.body)

        case hydra.core.TypeApplication(value=app_type):
            return cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(encode_type(app_type.function), encode_type(app_type.argument))))

        case hydra.core.TypeEither(value=et):
            return encode_either_type(et)

        case hydra.core.TypeForall(value=ft):
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(encode_binding_name(ft.parameter), Nothing(), encode_type_named(ename, ft.body))))))

        case hydra.core.TypeFunction():
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), Nothing(), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x"))))))))

        case hydra.core.TypeList(value=elem_type):
            return encode_list_type(elem_type)

        case hydra.core.TypeLiteral(value=lt):
            return encode_literal_type(lt)

        case hydra.core.TypeMap(value=mt):
            return encode_map_type(mt)

        case hydra.core.TypeMaybe(value=elem_type2):
            return encode_optional_type(elem_type2)

        case hydra.core.TypePair(value=pt):
            return encode_pair_type(pt)

        case hydra.core.TypeRecord(value=rt):
            return encode_record_type_named(ename, rt)

        case hydra.core.TypeSet(value=elem_type3):
            return encode_set_type(elem_type3)

        case hydra.core.TypeUnion(value=rt2):
            return encode_union_type_named(ename, rt2)

        case hydra.core.TypeWrap(value=wt):
            return encode_wrapped_type_named(ename, wt)

        case hydra.core.TypeUnit():
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("_"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("unit"), cast(hydra.core.Term, hydra.core.TermUnit()))))))))))

        case hydra.core.TypeVoid():
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("_"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("unit"), cast(hydra.core.Term, hydra.core.TermUnit()))))))))))

        case hydra.core.TypeVariable(value=type_name):
            return cast(hydra.core.Term, hydra.core.TermVariable(encode_binding_name(type_name)))

        case _:
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), Nothing(), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x"))))))))

def encoder_collect_forall_variables(typ: hydra.core.Type) -> frozenlist[hydra.core.Name]:
    r"""Collect forall type variable names from a type."""

    match typ:
        case hydra.core.TypeAnnotated(value=at):
            return encoder_collect_forall_variables(at.body)

        case hydra.core.TypeForall(value=ft):
            return hydra.lib.lists.cons(ft.parameter, encoder_collect_forall_variables(ft.body))

        case _:
            return ()

def encoder_collect_type_vars_from_type(typ: hydra.core.Type) -> frozenlist[hydra.core.Name]:
    r"""Collect all type variable names from a type expression."""

    match typ:
        case hydra.core.TypeAnnotated(value=at):
            return encoder_collect_type_vars_from_type(at.body)

        case hydra.core.TypeApplication(value=app_type):
            return hydra.lib.lists.concat2(encoder_collect_type_vars_from_type(app_type.function), encoder_collect_type_vars_from_type(app_type.argument))

        case hydra.core.TypeForall(value=ft):
            return encoder_collect_type_vars_from_type(ft.body)

        case hydra.core.TypeList(value=elem_type):
            return encoder_collect_type_vars_from_type(elem_type)

        case hydra.core.TypeMap(value=mt):
            return hydra.lib.lists.concat2(encoder_collect_type_vars_from_type(mt.keys), encoder_collect_type_vars_from_type(mt.values))

        case hydra.core.TypeMaybe(value=elem_type2):
            return encoder_collect_type_vars_from_type(elem_type2)

        case hydra.core.TypePair(value=pt):
            return hydra.lib.lists.concat2(encoder_collect_type_vars_from_type(pt.first), encoder_collect_type_vars_from_type(pt.second))

        case hydra.core.TypeRecord(value=rt):
            return hydra.lib.lists.concat(hydra.lib.lists.map((lambda ft: encoder_collect_type_vars_from_type(ft.type)), rt))

        case hydra.core.TypeSet(value=elem_type3):
            return encoder_collect_type_vars_from_type(elem_type3)

        case hydra.core.TypeUnion(value=rt2):
            return hydra.lib.lists.concat(hydra.lib.lists.map((lambda ft: encoder_collect_type_vars_from_type(ft.type)), rt2))

        case hydra.core.TypeVariable(value=name):
            return (name,)

        case hydra.core.TypeWrap(value=wt):
            return encoder_collect_type_vars_from_type(wt)

        case _:
            return ()

def encoder_collect_ord_vars(typ: hydra.core.Type) -> frozenlist[hydra.core.Name]:
    r"""Collect type variables needing Ord constraints."""

    match typ:
        case hydra.core.TypeAnnotated(value=at):
            return encoder_collect_ord_vars(at.body)

        case hydra.core.TypeApplication(value=app_type):
            return hydra.lib.lists.concat2(encoder_collect_ord_vars(app_type.function), encoder_collect_ord_vars(app_type.argument))

        case hydra.core.TypeEither(value=et):
            return hydra.lib.lists.concat2(encoder_collect_ord_vars(et.left), encoder_collect_ord_vars(et.right))

        case hydra.core.TypeForall(value=ft):
            return encoder_collect_ord_vars(ft.body)

        case hydra.core.TypeList(value=elem_type):
            return encoder_collect_ord_vars(elem_type)

        case hydra.core.TypeMap(value=mt):
            return hydra.lib.lists.concat((encoder_collect_type_vars_from_type(mt.keys), encoder_collect_ord_vars(mt.keys), encoder_collect_ord_vars(mt.values)))

        case hydra.core.TypeMaybe(value=elem_type2):
            return encoder_collect_ord_vars(elem_type2)

        case hydra.core.TypePair(value=pt):
            return hydra.lib.lists.concat2(encoder_collect_ord_vars(pt.first), encoder_collect_ord_vars(pt.second))

        case hydra.core.TypeRecord(value=rt):
            return hydra.lib.lists.concat(hydra.lib.lists.map((lambda ft: encoder_collect_ord_vars(ft.type)), rt))

        case hydra.core.TypeSet(value=elem_type3):
            return hydra.lib.lists.concat2(encoder_collect_type_vars_from_type(elem_type3), encoder_collect_ord_vars(elem_type3))

        case hydra.core.TypeUnion(value=rt2):
            return hydra.lib.lists.concat(hydra.lib.lists.map((lambda ft: encoder_collect_ord_vars(ft.type)), rt2))

        case hydra.core.TypeWrap(value=wt):
            return encoder_collect_ord_vars(wt)

        case _:
            return ()

def encoder_full_result_type(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Get full result type for encoder input."""

    match typ:
        case hydra.core.TypeAnnotated(value=at):
            return encoder_full_result_type(at.body)

        case hydra.core.TypeApplication(value=app_type):
            return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(encoder_full_result_type(app_type.function), app_type.argument)))

        case hydra.core.TypeEither(value=et):
            return cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(encoder_full_result_type(et.left), encoder_full_result_type(et.right))))

        case hydra.core.TypeForall(value=ft):
            return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(encoder_full_result_type(ft.body), cast(hydra.core.Type, hydra.core.TypeVariable(ft.parameter)))))

        case hydra.core.TypeList(value=elem_type):
            return cast(hydra.core.Type, hydra.core.TypeList(encoder_full_result_type(elem_type)))

        case hydra.core.TypeLiteral():
            return cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Literal")))

        case hydra.core.TypeMap(value=mt):
            return cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(encoder_full_result_type(mt.keys), encoder_full_result_type(mt.values))))

        case hydra.core.TypeMaybe(value=elem_type2):
            return cast(hydra.core.Type, hydra.core.TypeMaybe(encoder_full_result_type(elem_type2)))

        case hydra.core.TypePair(value=pt):
            return cast(hydra.core.Type, hydra.core.TypePair(hydra.core.PairType(encoder_full_result_type(pt.first), encoder_full_result_type(pt.second))))

        case hydra.core.TypeRecord():
            return cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Term")))

        case hydra.core.TypeSet(value=elem_type3):
            return cast(hydra.core.Type, hydra.core.TypeSet(encoder_full_result_type(elem_type3)))

        case hydra.core.TypeUnion():
            return cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Term")))

        case hydra.core.TypeUnit():
            return cast(hydra.core.Type, hydra.core.TypeUnit())

        case hydra.core.TypeVariable(value=name):
            return cast(hydra.core.Type, hydra.core.TypeVariable(name))

        case hydra.core.TypeVoid():
            return cast(hydra.core.Type, hydra.core.TypeVoid())

        case hydra.core.TypeWrap():
            return cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Term")))

        case _:
            return cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Term")))

def encoder_full_result_type_named(ename: hydra.core.Name, typ: hydra.core.Type) -> hydra.core.Type:
    r"""Get full result type for encoder input, using element name for nominal types."""

    match typ:
        case hydra.core.TypeAnnotated(value=at):
            return encoder_full_result_type_named(ename, at.body)

        case hydra.core.TypeApplication(value=app_type):
            return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(encoder_full_result_type(app_type.function), app_type.argument)))

        case hydra.core.TypeEither(value=et):
            return cast(hydra.core.Type, hydra.core.TypeEither(hydra.core.EitherType(encoder_full_result_type(et.left), encoder_full_result_type(et.right))))

        case hydra.core.TypeForall(value=ft):
            return cast(hydra.core.Type, hydra.core.TypeApplication(hydra.core.ApplicationType(encoder_full_result_type_named(ename, ft.body), cast(hydra.core.Type, hydra.core.TypeVariable(ft.parameter)))))

        case hydra.core.TypeList(value=elem_type):
            return cast(hydra.core.Type, hydra.core.TypeList(encoder_full_result_type(elem_type)))

        case hydra.core.TypeLiteral():
            return cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Literal")))

        case hydra.core.TypeMap(value=mt):
            return cast(hydra.core.Type, hydra.core.TypeMap(hydra.core.MapType(encoder_full_result_type(mt.keys), encoder_full_result_type(mt.values))))

        case hydra.core.TypeMaybe(value=elem_type2):
            return cast(hydra.core.Type, hydra.core.TypeMaybe(encoder_full_result_type(elem_type2)))

        case hydra.core.TypePair(value=pt):
            return cast(hydra.core.Type, hydra.core.TypePair(hydra.core.PairType(encoder_full_result_type(pt.first), encoder_full_result_type(pt.second))))

        case hydra.core.TypeRecord():
            return cast(hydra.core.Type, hydra.core.TypeVariable(ename))

        case hydra.core.TypeSet(value=elem_type3):
            return cast(hydra.core.Type, hydra.core.TypeSet(encoder_full_result_type(elem_type3)))

        case hydra.core.TypeUnion():
            return cast(hydra.core.Type, hydra.core.TypeVariable(ename))

        case hydra.core.TypeUnit():
            return cast(hydra.core.Type, hydra.core.TypeUnit())

        case hydra.core.TypeVariable(value=name):
            return cast(hydra.core.Type, hydra.core.TypeVariable(name))

        case hydra.core.TypeVoid():
            return cast(hydra.core.Type, hydra.core.TypeVoid())

        case hydra.core.TypeWrap():
            return cast(hydra.core.Type, hydra.core.TypeVariable(ename))

        case _:
            return cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Term")))

def prepend_forall_encoders(base_type: hydra.core.Type, typ: hydra.core.Type) -> hydra.core.Type:
    r"""Prepend encoder types for forall parameters to base type."""

    match typ:
        case hydra.core.TypeAnnotated(value=at):
            return prepend_forall_encoders(base_type, at.body)

        case hydra.core.TypeForall(value=ft):
            return cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(cast(hydra.core.Type, hydra.core.TypeVariable(ft.parameter)), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Term")))))), prepend_forall_encoders(base_type, ft.body))))

        case _:
            return base_type

def encoder_type_named(ename: hydra.core.Name, typ: hydra.core.Type) -> hydra.core.Type:
    r"""Build encoder function type with element name for nominal types."""

    @lru_cache(1)
    def result_type() -> hydra.core.Type:
        return encoder_full_result_type_named(ename, typ)
    @lru_cache(1)
    def base_type() -> hydra.core.Type:
        return cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(result_type(), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Term"))))))
    return prepend_forall_encoders(base_type(), typ)

def encoder_type_scheme_named(ename: hydra.core.Name, typ: hydra.core.Type) -> hydra.core.TypeScheme:
    r"""Construct a TypeScheme for an encoder function, with element name for nominal types."""

    @lru_cache(1)
    def type_vars() -> frozenlist[hydra.core.Name]:
        return encoder_collect_forall_variables(typ)
    @lru_cache(1)
    def encoder_fun_type() -> hydra.core.Type:
        return encoder_type_named(ename, typ)
    @lru_cache(1)
    def all_ord_vars() -> frozenlist[hydra.core.Name]:
        return encoder_collect_ord_vars(typ)
    @lru_cache(1)
    def ord_vars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.filter((lambda v: hydra.lib.lists.elem(v, type_vars())), all_ord_vars())
    @lru_cache(1)
    def constraints() -> Maybe[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(ord_vars()), (lambda : Nothing()), (lambda : Just(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda v: (v, hydra.core.TypeVariableMetadata(hydra.lib.sets.singleton(hydra.core.Name("ordering"))))), ord_vars())))))
    return hydra.core.TypeScheme(type_vars(), encoder_fun_type(), constraints())

def encode_binding(cx: hydra.context.Context, graph: hydra.graph.Graph, b: hydra.core.Binding) -> Either[hydra.context.InContext[hydra.errors.DecodingError], hydra.core.Binding]:
    r"""Transform a type binding into an encoder binding."""

    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _wc_e: hydra.context.InContext(_wc_e, cx)), (lambda _wc_a: _wc_a), hydra.decode.core.type(graph, b.term)), (lambda typ: Right(hydra.core.Binding(encode_binding_name(b.name), encode_type_named(b.name, typ), Just(encoder_type_scheme_named(b.name, typ))))))

def encode_namespace(ns: hydra.packaging.Namespace) -> hydra.packaging.Namespace:
    r"""Generate an encoder module namespace from a source module namespace."""

    return hydra.packaging.Namespace(hydra.lib.strings.cat(("hydra.encode.", hydra.lib.strings.intercalate(".", hydra.lib.lists.tail(hydra.lib.strings.split_on(".", ns.value))))))

def is_encodable_binding(cx: hydra.context.Context, graph: hydra.graph.Graph, b: hydra.core.Binding) -> Either[hydra.context.InContext[hydra.errors.Error], Maybe[hydra.core.Binding]]:
    r"""Check if a binding is encodable (serializable type)."""

    return hydra.lib.eithers.bind(hydra.predicates.is_serializable_by_name(cx, graph, b.name), (lambda serializable: Right(hydra.lib.logic.if_else(serializable, (lambda : Just(b)), (lambda : Nothing())))))

def filter_type_bindings(cx: hydra.context.Context, graph: hydra.graph.Graph, bindings: frozenlist[hydra.core.Binding]) -> Either[hydra.context.InContext[hydra.errors.Error], frozenlist[hydra.core.Binding]]:
    r"""Filter bindings to only encodable type definitions."""

    return hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.cat(x1)), hydra.lib.eithers.map_list((lambda v1: is_encodable_binding(cx, graph, v1)), hydra.lib.lists.filter((lambda x1: hydra.annotations.is_native_type(x1)), bindings)))

def encode_module(cx: hydra.context.Context, graph: hydra.graph.Graph, mod: hydra.packaging.Module):
    def _hoist_hydra_encoding_encode_module_1(v1):
        match v1:
            case hydra.packaging.DefinitionType(value=td):
                return Just((schema_term := cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.core.Type"))), (data_term := hydra.annotations.normalize_term_annotations(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(hydra.encode.core.type(td.type.type), hydra.lib.maps.from_list(((hydra.constants.key_type, schema_term),)))))), hydra.core.Binding(td.name, data_term, Just(hydra.core.TypeScheme((), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))), Nothing()))))[1])[1])

            case _:
                return Nothing()
    return hydra.lib.eithers.bind(filter_type_bindings(cx, graph, hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_hydra_encoding_encode_module_1(d)), mod.definitions))), (lambda type_bindings: hydra.lib.logic.if_else(hydra.lib.lists.null(type_bindings), (lambda : Right(Nothing())), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda b: hydra.lib.eithers.bimap((lambda ic: hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(ic.object.value))), ic.context)), (lambda x: x), encode_binding(cx, graph, b))), type_bindings), (lambda encoded_bindings: Right(Just(hydra.packaging.Module(encode_namespace(mod.namespace), hydra.lib.lists.map((lambda b: cast(hydra.packaging.Definition, hydra.packaging.DefinitionTerm(hydra.packaging.TermDefinition(b.name, b.term, b.type)))), encoded_bindings), hydra.lib.lists.nub(hydra.lib.lists.concat2(hydra.lib.lists.map((lambda x1: encode_namespace(x1)), mod.type_dependencies), hydra.lib.lists.map((lambda x1: encode_namespace(x1)), mod.term_dependencies))), (mod.namespace,), Just(hydra.lib.strings.cat(("Term encoders for ", mod.namespace.value))))))))))))

def encoder_type(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Build encoder function type."""

    @lru_cache(1)
    def result_type() -> hydra.core.Type:
        return encoder_full_result_type(typ)
    @lru_cache(1)
    def base_type() -> hydra.core.Type:
        return cast(hydra.core.Type, hydra.core.TypeFunction(hydra.core.FunctionType(result_type(), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Term"))))))
    return prepend_forall_encoders(base_type(), typ)

def encoder_type_scheme(typ: hydra.core.Type) -> hydra.core.TypeScheme:
    r"""Construct a TypeScheme for an encoder function from a source type."""

    @lru_cache(1)
    def type_vars() -> frozenlist[hydra.core.Name]:
        return encoder_collect_forall_variables(typ)
    @lru_cache(1)
    def encoder_fun_type() -> hydra.core.Type:
        return encoder_type(typ)
    @lru_cache(1)
    def all_ord_vars() -> frozenlist[hydra.core.Name]:
        return encoder_collect_ord_vars(typ)
    @lru_cache(1)
    def ord_vars() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.filter((lambda v: hydra.lib.lists.elem(v, type_vars())), all_ord_vars())
    @lru_cache(1)
    def constraints() -> Maybe[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(ord_vars()), (lambda : Nothing()), (lambda : Just(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda v: (v, hydra.core.TypeVariableMetadata(hydra.lib.sets.singleton(hydra.core.Name("ordering"))))), ord_vars())))))
    return hydra.core.TypeScheme(type_vars(), encoder_fun_type(), constraints())

def is_unit_type(v1: hydra.core.Type) -> bool:
    r"""Check whether a type is the unit type."""

    match v1:
        case hydra.core.TypeUnit():
            return True

        case _:
            return False
