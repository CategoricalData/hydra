# Note: this is an automatically generated file. Do not edit.

r"""Decode hydra.core types from the hydra.core.Term type."""

from __future__ import annotations
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
    return hydra.lib.flows.map((lambda x: hydra.core.Name(x)), hydra.lib.flows.bind(hydra.extract.core.wrap(hydra.core.Name("hydra.core.Name"), term), hydra.extract.core.string))

def float_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.FloatType]:
    return hydra.lexical.match_enum(hydra.core.Name("hydra.core.FloatType"), ((hydra.core.Name("bigfloat"), hydra.core.FloatType.BIGFLOAT), (hydra.core.Name("float32"), hydra.core.FloatType.FLOAT32), (hydra.core.Name("float64"), hydra.core.FloatType.FLOAT64)), v1)

def integer_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.IntegerType]:
    return hydra.lexical.match_enum(hydra.core.Name("hydra.core.IntegerType"), ((hydra.core.Name("bigint"), hydra.core.IntegerType.BIGINT), (hydra.core.Name("int8"), hydra.core.IntegerType.INT8), (hydra.core.Name("int16"), hydra.core.IntegerType.INT16), (hydra.core.Name("int32"), hydra.core.IntegerType.INT32), (hydra.core.Name("int64"), hydra.core.IntegerType.INT64), (hydra.core.Name("uint8"), hydra.core.IntegerType.UINT8), (hydra.core.Name("uint16"), hydra.core.IntegerType.UINT16), (hydra.core.Name("uint32"), hydra.core.IntegerType.UINT32), (hydra.core.Name("uint64"), hydra.core.IntegerType.UINT64)), v1)

def literal_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.LiteralType]:
    return hydra.lexical.match_union(hydra.core.Name("hydra.core.LiteralType"), (hydra.lexical.match_unit_field(hydra.core.Name("binary"), cast(hydra.core.LiteralType, hydra.core.LiteralTypeBinary(None))), hydra.lexical.match_unit_field(hydra.core.Name("boolean"), cast(hydra.core.LiteralType, hydra.core.LiteralTypeBoolean(None))), (hydra.core.Name("float"), (lambda ft: hydra.lib.flows.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(x))), float_type(ft)))), (hydra.core.Name("integer"), (lambda it: hydra.lib.flows.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(x))), integer_type(it)))), hydra.lexical.match_unit_field(hydra.core.Name("string"), cast(hydra.core.LiteralType, hydra.core.LiteralTypeString(None)))), v1)

def application_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.ApplicationType]:
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("function"), type), (lambda function: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("argument"), type), (lambda argument: hydra.lib.flows.pure(hydra.core.ApplicationType(function, argument))))))), v1)

def field_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.FieldType]:
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("name"), name), (lambda name: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("type"), type), (lambda typ: hydra.lib.flows.pure(hydra.core.FieldType(name, typ))))))), v1)

def field_types(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.core.FieldType]]:
    stripped = hydra.rewriting.deannotate_and_detype_term(term)
    match stripped:
        case hydra.core.TermList(value=els):
            return hydra.lib.flows.map_list(field_type, els)
        
        case _:
            return hydra.monads.unexpected("list", hydra.show.core.term(term))

def forall_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.ForallType]:
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("parameter"), name), (lambda parameter: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("body"), type), (lambda body: hydra.lib.flows.pure(hydra.core.ForallType(parameter, body))))))), v1)

def function_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.FunctionType]:
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("domain"), type), (lambda domain: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("codomain"), type), (lambda codomain: hydra.lib.flows.pure(hydra.core.FunctionType(domain, codomain))))))), v1)

def map_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.MapType]:
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("keys"), type), (lambda keys: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("values"), type), (lambda values: hydra.lib.flows.pure(hydra.core.MapType(keys, values))))))), v1)

def row_type(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.RowType]:
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("typeName"), name), (lambda type_name: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("fields"), field_types), (lambda fields: hydra.lib.flows.pure(hydra.core.RowType(type_name, fields))))))), v1)

def type(dat: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
    match dat:
        case hydra.core.TermAnnotated(value=annotated_term):
            return hydra.lib.flows.map((lambda t: cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(t, annotated_term.annotation)))), type(annotated_term.body))
        
        case _:
            return hydra.lexical.match_union(hydra.core.Name("hydra.core.Type"), ((hydra.core.Name("application"), (lambda at: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeApplication(x))), application_type(at)))), (hydra.core.Name("forall"), (lambda ft: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeForall(x))), forall_type(ft)))), (hydra.core.Name("function"), (lambda ft: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeFunction(x))), function_type(ft)))), (hydra.core.Name("list"), (lambda et: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeList(x))), type(et)))), (hydra.core.Name("literal"), (lambda lt: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeLiteral(x))), literal_type(lt)))), (hydra.core.Name("map"), (lambda mt: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeMap(x))), map_type(mt)))), (hydra.core.Name("optional"), (lambda et: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeOptional(x))), type(et)))), (hydra.core.Name("product"), (lambda types: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeProduct(x))), hydra.extract.core.list_of(type, types)))), (hydra.core.Name("record"), (lambda rt: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeRecord(x))), row_type(rt)))), (hydra.core.Name("set"), (lambda et: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeSet(x))), type(et)))), (hydra.core.Name("sum"), (lambda types: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeSum(x))), hydra.extract.core.list_of(type, types)))), (hydra.core.Name("union"), (lambda rt: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeUnion(x))), row_type(rt)))), (hydra.core.Name("unit"), (lambda _: hydra.lib.flows.pure(cast(hydra.core.Type, hydra.core.TypeUnit(None))))), (hydra.core.Name("variable"), (lambda n: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeVariable(x))), name(n)))), (hydra.core.Name("wrap"), (lambda wt: hydra.lib.flows.map((lambda x: cast(hydra.core.Type, hydra.core.TypeWrap(x))), wrapped_type(wt))))), dat)

def wrapped_type(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.WrappedType]:
    return hydra.lib.flows.bind(hydra.extract.core.record(hydra.core.Name("hydra.core.WrappedType"), term), (lambda fields: hydra.lib.flows.bind(hydra.extract.core.field(hydra.core.Name("typeName"), name, fields), (lambda name: hydra.lib.flows.bind(hydra.extract.core.field(hydra.core.Name("body"), type, fields), (lambda obj: hydra.lib.flows.pure(hydra.core.WrappedType(name, obj))))))))

def string(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, str]:
    return hydra.extract.core.string(hydra.rewriting.deannotate_and_detype_term(term))

def type_scheme(v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.TypeScheme]:
    return hydra.lexical.match_record((lambda m: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("variables"), (lambda v12: hydra.extract.core.list_of(name, v12))), (lambda vars: hydra.lib.flows.bind(hydra.lexical.get_field(m, hydra.core.Name("type"), type), (lambda body: hydra.lib.flows.pure(hydra.core.TypeScheme(vars, body))))))), v1)
