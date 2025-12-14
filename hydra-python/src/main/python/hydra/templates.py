# Note: this is an automatically generated file. Do not edit.

r"""A utility which instantiates a nonrecursive type with default values."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.compute
import hydra.core
import hydra.decode.core
import hydra.graph
import hydra.lib.flows
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.monads
import hydra.show.core

T0 = TypeVar("T0")

def graph_to_schema(g: hydra.graph.Graph) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[hydra.core.Name, hydra.core.Type]]:
    r"""Create a graph schema from a graph which contains nothing but encoded type definitions."""
    
    def to_pair(name_and_el: tuple[T0, hydra.core.Binding]) -> hydra.compute.Flow[hydra.graph.Graph, tuple[T0, hydra.core.Type]]:
        def name() -> T0:
            return hydra.lib.pairs.first(name_and_el)
        def el() -> hydra.core.Binding:
            return hydra.lib.pairs.second(name_and_el)
        return hydra.lib.flows.bind(hydra.monads.with_trace("graph to schema", hydra.decode.core.type(el().term)), (lambda t: hydra.lib.flows.pure(cast(tuple[T0, hydra.core.Type], (name(), t)))))
    return hydra.lib.flows.bind(hydra.lib.flows.map_list(cast(Callable[[tuple[hydra.core.Name, hydra.core.Binding]], hydra.compute.Flow[hydra.graph.Graph, tuple[hydra.core.Name, hydra.core.Type]]], (lambda x1: to_pair(x1))), hydra.lib.maps.to_list(g.elements)), (lambda pairs: hydra.lib.flows.pure(cast(FrozenDict[hydra.core.Name, hydra.core.Type], hydra.lib.maps.from_list(pairs)))))

def instantiate_template(minimal: bool, schema: FrozenDict[hydra.core.Name, hydra.core.Type], t: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Term]:
    def inst(v1: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Term]:
        return instantiate_template(minimal, schema, v1)
    def no_poly() -> hydra.compute.Flow[T1, T2]:
        return hydra.lib.flows.fail("Polymorphic and function types are not currently supported")
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
            return inst(at.body)
        
        case hydra.core.TypeApplication():
            return cast(hydra.compute.Flow[T0, hydra.core.Term], no_poly())
        
        case hydra.core.TypeFunction():
            return cast(hydra.compute.Flow[T0, hydra.core.Term], no_poly())
        
        case hydra.core.TypeForall():
            return cast(hydra.compute.Flow[T0, hydra.core.Term], no_poly())
        
        case hydra.core.TypeList(value=et):
            return hydra.lib.logic.if_else(minimal, (lambda : hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermList(cast(frozenlist[hydra.core.Term], ()))))), (lambda : hydra.lib.flows.bind(inst(et), (lambda e: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermList((e,))))))))
        
        case hydra.core.TypeLiteral(value=lt):
            return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermLiteral(for_literal(lt))))
        
        case hydra.core.TypeMap(value=mt):
            kt = mt.keys
            vt = mt.values
            return hydra.lib.logic.if_else(minimal, (lambda : hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMap(cast(FrozenDict[hydra.core.Term, hydra.core.Term], hydra.lib.maps.empty()))))), (lambda : hydra.lib.flows.bind(inst(kt), (lambda ke: hydra.lib.flows.bind(inst(vt), (lambda ve: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.singleton(ke, ve))))))))))
        
        case hydra.core.TypeMaybe(value=ot):
            return hydra.lib.logic.if_else(minimal, (lambda : hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMaybe(cast(Maybe[hydra.core.Term], Nothing()))))), (lambda : hydra.lib.flows.bind(inst(ot), (lambda e: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMaybe(cast(Maybe[hydra.core.Term], Just(e)))))))))
        
        case hydra.core.TypeRecord(value=rt):
            tname = rt.type_name
            fields = rt.fields
            def to_field(ft: hydra.core.FieldType) -> hydra.compute.Flow[T0, hydra.core.Field]:
                return hydra.lib.flows.bind(inst(ft.type), (lambda e: hydra.lib.flows.pure(hydra.core.Field(ft.name, e))))
            return hydra.lib.flows.bind(hydra.lib.flows.map_list(to_field, fields), (lambda dfields: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(tname, dfields))))))
        
        case hydra.core.TypeSet(value=et2):
            return hydra.lib.logic.if_else(minimal, (lambda : hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermSet(cast(frozenset[hydra.core.Term], hydra.lib.sets.empty()))))), (lambda : hydra.lib.flows.bind(inst(et2), (lambda e: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list((e,)))))))))
        
        case hydra.core.TypeVariable(value=tname):
            return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("Type variable ", hydra.lib.strings.cat2(hydra.show.core.term(cast(hydra.core.Term, hydra.core.TermVariable(tname))), " not found in schema"))), inst, hydra.lib.maps.lookup(tname, schema))
        
        case hydra.core.TypeWrap(value=wt):
            tname = wt.type_name
            t_ = wt.body
            return hydra.lib.flows.bind(inst(t_), (lambda e: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(tname, e))))))
        
        case _:
            raise TypeError("Unsupported Type")
