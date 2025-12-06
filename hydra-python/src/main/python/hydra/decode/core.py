# Note: this is an automatically generated file. Do not edit.

r"""Decode hydra.core types from the hydra.core.Term type."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.compute
import hydra.core
import hydra.extract.core
import hydra.graph
import hydra.lexical
import hydra.lib.flows
import hydra.monads
import hydra.rewriting
import hydra.show.core

def name(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Name]:
    r"""Decode a name from a term."""
    
    return hydra.lib.flows.map((lambda x: hydra.core.Name(x)), hydra.lib.flows.bind(hydra.extract.core.wrap(hydra.core.Name("hydra.core.Name"), term), hydra.extract.core.string))

def float_type(term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.FloatType]:
    r"""Decode a floating-point type from a term."""
    
    return hydra.monads.with_trace("dbg 1", hydra.lexical.match_enum(hydra.core.Name("hydra.core.FloatType"), (cast(Tuple[hydra.core.Name, hydra.core.FloatType], (hydra.core.Name("bigfloat"), hydra.core.FloatType.BIGFLOAT)), cast(Tuple[hydra.core.Name, hydra.core.FloatType], (hydra.core.Name("float32"), hydra.core.FloatType.FLOAT32)), cast(Tuple[hydra.core.Name, hydra.core.FloatType], (hydra.core.Name("float64"), hydra.core.FloatType.FLOAT64))), term0))

def integer_type(term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.IntegerType]:
    r"""Decode an integer type from a term."""
    
    return hydra.monads.with_trace("dbg 1", hydra.lexical.match_enum(hydra.core.Name("hydra.core.IntegerType"), (cast(Tuple[hydra.core.Name, hydra.core.IntegerType], (hydra.core.Name("bigint"), hydra.core.IntegerType.BIGINT)), cast(Tuple[hydra.core.Name, hydra.core.IntegerType], (hydra.core.Name("int8"), hydra.core.IntegerType.INT8)), cast(Tuple[hydra.core.Name, hydra.core.IntegerType], (hydra.core.Name("int16"), hydra.core.IntegerType.INT16)), cast(Tuple[hydra.core.Name, hydra.core.IntegerType], (hydra.core.Name("int32"), hydra.core.IntegerType.INT32)), cast(Tuple[hydra.core.Name, hydra.core.IntegerType], (hydra.core.Name("int64"), hydra.core.IntegerType.INT64)), cast(Tuple[hydra.core.Name, hydra.core.IntegerType], (hydra.core.Name("uint8"), hydra.core.IntegerType.UINT8)), cast(Tuple[hydra.core.Name, hydra.core.IntegerType], (hydra.core.Name("uint16"), hydra.core.IntegerType.UINT16)), cast(Tuple[hydra.core.Name, hydra.core.IntegerType], (hydra.core.Name("uint32"), hydra.core.IntegerType.UINT32)), cast(Tuple[hydra.core.Name, hydra.core.IntegerType], (hydra.core.Name("uint64"), hydra.core.IntegerType.UINT64))), term0))

def literal_type(term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.LiteralType]:
    r"""Decode a literal type from a term."""
    
    return hydra.monads.with_trace("dbg 3", hydra.lexical.match_union(hydra.core.Name("hydra.core.LiteralType"), (hydra.lexical.match_unit_field(hydra.core.Name("binary"), cast(hydra.core.LiteralType, hydra.core.LiteralTypeBinary())), hydra.lexical.match_unit_field(hydra.core.Name("boolean"), cast(hydra.core.LiteralType, hydra.core.LiteralTypeBoolean())), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.LiteralType]]], (hydra.core.Name("float"), (lambda ft: hydra.lib.flows.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(x))), float_type(ft))))), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.LiteralType]]], (hydra.core.Name("integer"), (lambda it: hydra.lib.flows.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(x))), integer_type(it))))), hydra.lexical.match_unit_field(hydra.core.Name("string"), cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))), term0))

def application_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.ApplicationType]:
    r"""Decode an application type from a term."""
    
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("function"), type), (lambda function: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("argument"), type), (lambda argument: hydra.lib.flows.pure(hydra.core.ApplicationType(function, argument))))))), v1)

def either_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.EitherType]:
    r"""Decode an either type from a term."""
    
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("left"), type), (lambda left: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("right"), type), (lambda right: hydra.lib.flows.pure(hydra.core.EitherType(left, right))))))), v1)

def field_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.FieldType]:
    r"""Decode a field type from a term."""
    
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("name"), name), (lambda name: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("type"), type), (lambda typ: hydra.lib.flows.pure(hydra.core.FieldType(name, typ))))))), v1)

def field_types(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.core.FieldType]]:
    r"""Decode a list of field types from a term."""
    
    stripped = hydra.rewriting.deannotate_and_detype_term(term)
    match stripped:
        case hydra.core.TermList(value=els):
            return hydra.lib.flows.map_list(field_type, els)
        
        case _:
            return hydra.monads.unexpected("list", hydra.show.core.term(term))

def forall_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.ForallType]:
    r"""Decode a forall type from a term."""
    
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("parameter"), name), (lambda parameter: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("body"), type), (lambda body: hydra.lib.flows.pure(hydra.core.ForallType(parameter, body))))))), v1)

def function_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.FunctionType]:
    r"""Decode a function type from a term."""
    
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("domain"), type), (lambda domain: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("codomain"), type), (lambda codomain: hydra.lib.flows.pure(hydra.core.FunctionType(domain, codomain))))))), v1)

def map_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.MapType]:
    r"""Decode a map type from a term."""
    
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("keys"), type), (lambda keys: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("values"), type), (lambda values: hydra.lib.flows.pure(hydra.core.MapType(keys, values))))))), v1)

def pair_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.PairType]:
    r"""Decode a pair type from a term."""
    
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("first"), type), (lambda first: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("second"), type), (lambda second: hydra.lib.flows.pure(hydra.core.PairType(first, second))))))), v1)

def row_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.RowType]:
    r"""Decode a row type from a term."""
    
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("typeName"), name), (lambda type_name: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("fields"), field_types), (lambda fields: hydra.lib.flows.pure(hydra.core.RowType(type_name, fields))))))), v1)

def type(dat: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
    r"""Decode a type from a term."""
    
    match dat:
        case hydra.core.TermAnnotated(value=annotated_term):
            return hydra.lib.flows.map((lambda t: cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(t, annotated_term.annotation)))), type(annotated_term.body))
        
        case _:
            return hydra.monads.with_trace("dbg 4", hydra.lexical.match_union(hydra.core.Name("hydra.core.Type"), (cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]]], (hydra.core.Name("application"), (lambda at: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeApplication(x))), application_type(at))))), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]]], (hydra.core.Name("either"), (lambda et: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeEither(x))), either_type(et))))), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]]], (hydra.core.Name("forall"), (lambda ft: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeForall(x))), forall_type(ft))))), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]]], (hydra.core.Name("function"), (lambda ft: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeFunction(x))), function_type(ft))))), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]]], (hydra.core.Name("list"), (lambda et: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeList(x))), type(et))))), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]]], (hydra.core.Name("literal"), (lambda lt: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeLiteral(x))), literal_type(lt))))), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]]], (hydra.core.Name("map"), (lambda mt: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeMap(x))), map_type(mt))))), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]]], (hydra.core.Name("maybe"), (lambda et: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeMaybe(x))), type(et))))), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]]], (hydra.core.Name("pair"), (lambda pt: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypePair(x))), pair_type(pt))))), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]]], (hydra.core.Name("record"), (lambda rt: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeRecord(x))), row_type(rt))))), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]]], (hydra.core.Name("set"), (lambda et: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeSet(x))), type(et))))), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]]], (hydra.core.Name("union"), (lambda rt: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeUnion(x))), row_type(rt))))), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]]], (hydra.core.Name("unit"), (lambda _: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeUnit()))))), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]]], (hydra.core.Name("variable"), (lambda n: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), name(n))))), cast(Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]]], (hydra.core.Name("wrap"), (lambda wt: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeWrap(x))), wrapped_type(wt)))))), dat))

def wrapped_type(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.WrappedType]:
    r"""Decode a wrapped type from a term."""
    
    return hydra.lib.flows.bind(hydra.extract.core.record(hydra.core.Name("hydra.core.WrappedType"), term), (lambda fields: hydra.lib.flows.bind(hydra.extract.core.field(hydra.core.Name("typeName"), name, fields), (lambda name: hydra.lib.flows.bind(hydra.extract.core.field(hydra.core.Name("body"), type, fields), (lambda obj: hydra.lib.flows.pure(hydra.core.WrappedType(name, obj))))))))

def string(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, str]:
    r"""Decode a string from a term."""
    
    return hydra.extract.core.string(hydra.rewriting.deannotate_and_detype_term(term))

def type_scheme(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.TypeScheme]:
    r"""Decode a type scheme from a term."""
    
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("variables"), (lambda v12: hydra.extract.core.list_of(name, v12))), (lambda vars: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("type"), type), (lambda body: hydra.lib.flows.pure(hydra.core.TypeScheme(vars, body))))))), v1)
