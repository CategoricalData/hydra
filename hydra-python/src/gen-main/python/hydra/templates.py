# Note: this is an automatically generated file. Do not edit.

r"""A utility which instantiates a nonrecursive type with default values."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.context
import hydra.core
import hydra.decode.core
import hydra.error
import hydra.lib.eithers
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.sets
import hydra.lib.strings
import hydra.show.core

T0 = TypeVar("T0")

def graph_to_schema(cx: hydra.context.Context, graph: hydra.graph.Graph, els: frozenlist[hydra.core.Binding]) -> Either[hydra.context.InContext[hydra.error.DecodingError], FrozenDict[hydra.core.Name, hydra.core.Type]]:
    r"""Decode a list of type-encoding bindings into a map of named types."""

    def to_pair(el: hydra.core.Binding) -> Either[hydra.context.InContext[hydra.error.DecodingError], tuple[hydra.core.Name, hydra.core.Type]]:
        name = el.name
        return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _wc_e: hydra.context.InContext(_wc_e, cx)), (lambda _wc_a: _wc_a), hydra.decode.core.type(graph, el.term)), (lambda t: Right((name, t))))
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: to_pair(x1)), els), (lambda pairs: Right(hydra.lib.maps.from_list(pairs))))

def instantiate_template(cx: hydra.context.Context, minimal: bool, schema: FrozenDict[hydra.core.Name, hydra.core.Type], tname: hydra.core.Name, t: hydra.core.Type) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term]:
    r"""Given a graph schema and a nonrecursive type, instantiate it with default values. If the minimal flag is set, the smallest possible term is produced; otherwise, exactly one subterm is produced for constructors which do not otherwise require one, e.g. in lists and optionals. The name parameter provides the element name for nominal type construction."""

    def inst(tn: hydra.core.Name, v1: hydra.core.Type) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term]:
        return instantiate_template(cx, minimal, schema, tn, v1)
    @lru_cache(1)
    def no_poly() -> Either[hydra.context.InContext[hydra.error.Error], T0]:
        return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError("Polymorphic and function types are not currently supported"))), cx))
    def for_float(ft: hydra.core.FloatType) -> hydra.core.FloatValue:
        match ft:
            case hydra.core.FloatType.BIGFLOAT:
                return cast(hydra.core.FloatValue, hydra.core.FloatValueBigfloat(Decimal('0.0')))

            case hydra.core.FloatType.FLOAT32:
                return cast(hydra.core.FloatValue, hydra.core.FloatValueFloat32(0.0))

            case hydra.core.FloatType.FLOAT64:
                return cast(hydra.core.FloatValue, hydra.core.FloatValueFloat64(0.0))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def for_integer(it: hydra.core.IntegerType) -> hydra.core.IntegerValue:
        match it:
            case hydra.core.IntegerType.BIGINT:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueBigint(0))

            case hydra.core.IntegerType.INT8:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt8(0))

            case hydra.core.IntegerType.INT16:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt16(0))

            case hydra.core.IntegerType.INT32:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(0))

            case hydra.core.IntegerType.INT64:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(0))

            case hydra.core.IntegerType.UINT8:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint8(0))

            case hydra.core.IntegerType.UINT16:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint16(0))

            case hydra.core.IntegerType.UINT32:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint32(0))

            case hydra.core.IntegerType.UINT64:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint64(0))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def for_literal(lt: hydra.core.LiteralType) -> hydra.core.Literal:
        match lt:
            case hydra.core.LiteralTypeBinary():
                return cast(hydra.core.Literal, hydra.core.LiteralString(""))

            case hydra.core.LiteralTypeBoolean():
                return cast(hydra.core.Literal, hydra.core.LiteralBoolean(False))

            case hydra.core.LiteralTypeInteger(value=it):
                return cast(hydra.core.Literal, hydra.core.LiteralInteger(for_integer(it)))

            case hydra.core.LiteralTypeFloat(value=ft):
                return cast(hydra.core.Literal, hydra.core.LiteralFloat(for_float(ft)))

            case hydra.core.LiteralTypeString():
                return cast(hydra.core.Literal, hydra.core.LiteralString(""))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match t:
        case hydra.core.TypeAnnotated(value=at):
            return inst(tname, at.body)

        case hydra.core.TypeApplication():
            return no_poly()

        case hydra.core.TypeFunction():
            return no_poly()

        case hydra.core.TypeForall():
            return no_poly()

        case hydra.core.TypeList(value=et):
            return hydra.lib.logic.if_else(minimal, (lambda : Right(cast(hydra.core.Term, hydra.core.TermList(())))), (lambda : hydra.lib.eithers.bind(inst(tname, et), (lambda e: Right(cast(hydra.core.Term, hydra.core.TermList((e,))))))))

        case hydra.core.TypeLiteral(value=lt):
            return Right(cast(hydra.core.Term, hydra.core.TermLiteral(for_literal(lt))))

        case hydra.core.TypeMap(value=mt):
            kt = mt.keys
            vt = mt.values
            return hydra.lib.logic.if_else(minimal, (lambda : Right(cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.empty())))), (lambda : hydra.lib.eithers.bind(inst(tname, kt), (lambda ke: hydra.lib.eithers.bind(inst(tname, vt), (lambda ve: Right(cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.singleton(ke, ve))))))))))

        case hydra.core.TypeMaybe(value=ot):
            return hydra.lib.logic.if_else(minimal, (lambda : Right(cast(hydra.core.Term, hydra.core.TermMaybe(Nothing())))), (lambda : hydra.lib.eithers.bind(inst(tname, ot), (lambda e: Right(cast(hydra.core.Term, hydra.core.TermMaybe(Just(e))))))))

        case hydra.core.TypeRecord(value=rt):
            def to_field(ft: hydra.core.FieldType) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Field]:
                return hydra.lib.eithers.bind(inst(tname, ft.type), (lambda e: Right(hydra.core.Field(ft.name, e))))
            return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: to_field(x1)), rt), (lambda dfields: Right(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(tname, dfields))))))

        case hydra.core.TypeSet(value=et2):
            return hydra.lib.logic.if_else(minimal, (lambda : Right(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.empty())))), (lambda : hydra.lib.eithers.bind(inst(tname, et2), (lambda e: Right(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list((e,)))))))))

        case hydra.core.TypeVariable(value=vname):
            return hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2("Type variable ", hydra.lib.strings.cat2(hydra.show.core.term(cast(hydra.core.Term, hydra.core.TermVariable(vname))), " not found in schema"))))), cx))), (lambda v1: inst(vname, v1)), hydra.lib.maps.lookup(vname, schema))

        case hydra.core.TypeWrap(value=wt):
            return hydra.lib.eithers.bind(inst(tname, wt), (lambda e: Right(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(tname, e))))))

        case _:
            raise TypeError("Unsupported Type")
