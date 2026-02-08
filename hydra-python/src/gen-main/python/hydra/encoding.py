# Note: this is an automatically generated file. Do not edit.

r"""Functions for generating term encoders from type modules."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Just, Maybe, Nothing, frozenlist
from typing import cast
import hydra.annotations
import hydra.core
import hydra.decode.core
import hydra.formatting
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.strings
import hydra.module
import hydra.monads
import hydra.names
import hydra.schemas
import hydra.util

def encode_binding_name(n: hydra.core.Name) -> hydra.core.Name:
    r"""Generate a binding name for an encoder function from a type name."""
    
    return hydra.lib.logic.if_else(hydra.lib.logic.not_(hydra.lib.lists.null(hydra.lib.lists.tail(hydra.lib.strings.split_on(".", n.value)))), (lambda : hydra.core.Name(hydra.lib.strings.intercalate(".", hydra.lib.lists.concat2(("hydra", "encode"), hydra.lib.lists.concat2(hydra.lib.lists.tail(hydra.lib.lists.init(hydra.lib.strings.split_on(".", n.value))), (hydra.formatting.decapitalize(hydra.names.local_name_of(n)),)))))), (lambda : hydra.core.Name(hydra.formatting.decapitalize(hydra.names.local_name_of(n)))))

def encode_float_value(float_type: hydra.core.FloatType, val_term: hydra.core.Term) -> hydra.core.Term:
    def _hoist_hydra_encoding_encode_float_value_1(v1: hydra.core.FloatType) -> hydra.core.Name:
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
    def _hoist_hydra_encoding_encode_integer_value_1(v1: hydra.core.IntegerType) -> hydra.core.Name:
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
    r"""Generate an encoder for an optional type."""
    
    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("opt"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("maybe"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.maybes.map"))))), encode_type(elem_type)))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("opt")))))))))))))))

def encode_pair_type(pt: hydra.core.PairType) -> hydra.core.Term:
    r"""Generate an encoder for a pair type."""
    
    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("p"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("pair"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionPrimitive(hydra.core.Name("hydra.lib.pairs.bimap"))))), encode_type(pt.first)))), encode_type(pt.second)))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("p")))))))))))))))

def encode_record_type(rt: hydra.core.RowType) -> hydra.core.Term:
    r"""Generate an encoder for a record type."""
    
    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("record"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Record"), (hydra.core.Field(hydra.core.Name("typeName"), encode_name(rt.type_name)), hydra.core.Field(hydra.core.Name("fields"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda ft: cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.Field"), (hydra.core.Field(hydra.core.Name("name"), encode_name(ft.name)), hydra.core.Field(hydra.core.Name("term"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(encode_type(ft.type), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(rt.type_name, ft.name))))))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x"))))))))))))))), rt.fields)))))))))))))))))

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
        
        case hydra.core.TypeVariable(value=type_name):
            return cast(hydra.core.Term, hydra.core.TermVariable(encode_binding_name(type_name)))
        
        case _:
            return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), Nothing(), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x"))))))))

def encode_union_type(rt: hydra.core.RowType) -> hydra.core.Term:
    r"""Generate an encoder for a union type."""
    
    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationUnion(hydra.core.CaseStatement(rt.type_name, Nothing(), hydra.lib.lists.map((lambda ft: hydra.core.Field(ft.name, encode_field_value(rt.type_name, ft.name, ft.type))), rt.fields))))))))

def encode_wrapped_type(wt: hydra.core.WrappedType) -> hydra.core.Term:
    r"""Generate an encoder for a wrapped type."""
    
    return cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionLambda(hydra.core.Lambda(hydra.core.Name("x"), Nothing(), cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.core.Term"), hydra.core.Field(hydra.core.Name("wrap"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.core.WrappedTerm"), (hydra.core.Field(hydra.core.Name("typeName"), encode_name(wt.type_name)), hydra.core.Field(hydra.core.Name("body"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(encode_type(wt.body), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(wt.type_name)))))), cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("x")))))))))))))))))))))))

def encode_binding(b: hydra.core.Binding) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Binding]:
    r"""Transform a type binding into an encoder binding."""
    
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: hydra.lib.flows.bind(hydra.monads.either_to_flow((lambda v1: v1.value), hydra.decode.core.type(cx, b.term)), (lambda typ: hydra.lib.flows.pure(hydra.core.Binding(encode_binding_name(b.name), encode_type(typ), Nothing()))))))

def encode_namespace(ns: hydra.module.Namespace) -> hydra.module.Namespace:
    r"""Generate an encoder module namespace from a source module namespace."""
    
    return hydra.module.Namespace(hydra.lib.strings.cat(("hydra.encode.", hydra.lib.strings.intercalate(".", hydra.lib.lists.tail(hydra.lib.strings.split_on(".", ns.value))))))

def is_encodable_binding(b: hydra.core.Binding) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Binding]]:
    r"""Check if a binding is encodable (serializable type)."""
    
    return hydra.lib.flows.map((lambda serializable: hydra.lib.logic.if_else(serializable, (lambda : Just(b)), (lambda : Nothing()))), hydra.schemas.is_serializable_by_name(b.name))

def filter_type_bindings(bindings: frozenlist[hydra.core.Binding]) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.core.Binding]]:
    r"""Filter bindings to only encodable type definitions."""
    
    return hydra.lib.flows.map((lambda x1: hydra.lib.maybes.cat(x1)), hydra.lib.flows.map_list((lambda x1: is_encodable_binding(x1)), hydra.lib.lists.filter((lambda x1: hydra.annotations.is_native_type(x1)), bindings)))

def encode_module(mod: hydra.module.Module) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.module.Module]]:
    r"""Transform a type module into an encoder module."""
    
    return hydra.lib.flows.bind(filter_type_bindings(mod.elements), (lambda type_bindings: hydra.lib.logic.if_else(hydra.lib.lists.null(type_bindings), (lambda : hydra.lib.flows.pure(Nothing())), (lambda : hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda x1: encode_binding(x1)), type_bindings), (lambda encoded_bindings: hydra.lib.flows.pure(Just(hydra.module.Module(encode_namespace(mod.namespace), encoded_bindings, hydra.lib.lists.map((lambda x1: encode_namespace(x1)), mod.type_dependencies), (mod.namespace,), Just(hydra.lib.strings.cat(("Term encoders for ", mod.namespace.value))))))))))))

def is_unit_type(v1: hydra.core.Type) -> bool:
    r"""Check whether a type is the unit type."""
    
    match v1:
        case hydra.core.TypeUnit():
            return True
        
        case _:
            return False
