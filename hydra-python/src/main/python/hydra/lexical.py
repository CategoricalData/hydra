# Note: this is an automatically generated file. Do not edit.

r"""A module for lexical operations over graphs."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import Tuple, cast
import hydra.compute
import hydra.core
import hydra.graph
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.monads
import hydra.rewriting
import hydra.show.core
import hydra.typing

def lookup_element(g: hydra.graph.Graph, name: hydra.core.Name) -> Maybe[hydra.core.Binding]:
    return hydra.lib.maps.lookup(name, g.elements)

def dereference_element(name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Binding]]:
    r"""Look up an element in the current graph context."""
    
    return hydra.lib.flows.map((lambda g: lookup_element(g, name)), cast(hydra.compute.Flow[hydra.graph.Graph, hydra.graph.Graph], hydra.monads.get_state))

def dereference_schema_type(name: hydra.core.Name, types: FrozenDict[hydra.core.Name, hydra.core.TypeScheme]) -> Maybe[hydra.core.TypeScheme]:
    r"""Resolve a schema type through a chain of zero or more typedefs."""
    
    def for_type(t: hydra.core.Type) -> Maybe[hydra.core.TypeScheme]:
        match t:
            case hydra.core.TypeAnnotated(value=at):
                return for_type(at.body)
            
            case hydra.core.TypeForall(value=ft):
                return hydra.lib.maybes.map((lambda ts: hydra.core.TypeScheme(hydra.lib.lists.cons(ft.parameter, ts.variables), ts.type)), for_type(ft.body))
            
            case hydra.core.TypeVariable(value=v):
                return dereference_schema_type(v, types)
            
            case _:
                return cast(Maybe[hydra.core.TypeScheme], Just(hydra.core.TypeScheme(cast(frozenlist[hydra.core.Name], ()), t)))
    return hydra.lib.maybes.bind(hydra.lib.maps.lookup(name, types), (lambda ts: hydra.lib.maybes.map((lambda ts2: hydra.core.TypeScheme(hydra.lib.lists.concat2(ts.variables, ts2.variables), ts2.type)), for_type(ts.type))))

def elements_to_graph(parent: hydra.graph.Graph, schema: Maybe[hydra.graph.Graph], elements: frozenlist[hydra.core.Binding]) -> hydra.graph.Graph:
    r"""Create a graph from a parent graph, optional schema, and list of element bindings."""
    
    def to_pair(el: hydra.core.Binding) -> Tuple[hydra.core.Name, hydra.core.Binding]:
        return (el.name, el)
    return hydra.graph.Graph(cast(FrozenDict[hydra.core.Name, hydra.core.Binding], hydra.lib.maps.from_list(hydra.lib.lists.map(to_pair, elements))), parent.environment, parent.types, parent.body, parent.primitives, schema)

# An empty graph; no elements, no primitives, no schema, and an arbitrary body.
empty_graph = hydra.graph.Graph(cast(FrozenDict[hydra.core.Name, hydra.core.Binding], hydra.lib.maps.empty()), cast(FrozenDict[hydra.core.Name, Maybe[hydra.core.Term]], hydra.lib.maps.empty()), cast(FrozenDict[hydra.core.Name, hydra.core.TypeScheme], hydra.lib.maps.empty()), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString("empty graph")))), cast(FrozenDict[hydra.core.Name, hydra.graph.Primitive], hydra.lib.maps.empty()), cast(Maybe[hydra.graph.Graph], Nothing()))

def extend_graph_with_bindings(bindings: frozenlist[hydra.core.Binding], g: hydra.graph.Graph) -> hydra.graph.Graph:
    r"""Add bindings to an existing graph."""
    
    def to_el(binding: hydra.core.Binding) -> Tuple[hydra.core.Name, hydra.core.Binding]:
        name = binding.name
        term = binding.term
        mts = binding.type
        return (name, hydra.core.Binding(name, term, mts))
    new_els = cast(FrozenDict[hydra.core.Name, hydra.core.Binding], hydra.lib.maps.from_list(hydra.lib.lists.map(to_el, bindings)))
    return hydra.graph.Graph(hydra.lib.maps.union(new_els, g.elements), g.environment, g.types, g.body, g.primitives, g.schema)

def fields_of(t: hydra.core.Type) -> frozenlist[hydra.core.FieldType]:
    r"""Extract the fields of a record or union type."""
    
    stripped = hydra.rewriting.deannotate_type(t)
    match stripped:
        case hydra.core.TypeForall(value=forall_type):
            return fields_of(forall_type.body)
        
        case hydra.core.TypeRecord(value=rt):
            return rt.fields
        
        case hydra.core.TypeUnion(value=rt2):
            return rt2.fields
        
        case _:
            return cast(frozenlist[hydra.core.FieldType], ())

def get_field[T0, T1, T2](m: FrozenDict[hydra.core.Name, T0], fname: hydra.core.Name, decode: Callable[[T0], hydra.compute.Flow[T1, T2]]) -> hydra.compute.Flow[T1, T2]:
    return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat((hydra.lib.strings.cat(("expected field ", fname.value)), " not found"))), decode, hydra.lib.maps.lookup(fname, m))

def lookup_primitive(g: hydra.graph.Graph, name: hydra.core.Name) -> Maybe[hydra.graph.Primitive]:
    return hydra.lib.maps.lookup(name, g.primitives)

def require_element(name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Binding]:
    show_all = False
    def ellipsis(strings: frozenlist[str]) -> frozenlist[str]:
        return hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.equality.gt(hydra.lib.lists.length(strings), 3), hydra.lib.logic.not_(show_all)), hydra.lib.lists.concat2(hydra.lib.lists.take(3, strings), ("...",)), strings)
    def err[T0, T1](g: hydra.graph.Graph) -> hydra.compute.Flow[T0, T1]:
        return hydra.lib.flows.fail(hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat(("no such element: ", name.value)), ". Available elements: {")), hydra.lib.strings.intercalate(", ", ellipsis(hydra.lib.lists.map((lambda el: el.name.value), hydra.lib.maps.elems(g.elements)))))), "}")))
    return hydra.lib.flows.bind(dereference_element(name), (lambda mel: hydra.lib.maybes.maybe(hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.graph.Graph, hydra.graph.Graph], hydra.monads.get_state), (lambda g: err(g))), cast(Callable[[hydra.core.Binding], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Binding]], hydra.lib.flows.pure), mel)))

def match_union[T0](tname: hydra.core.Name, pairs: frozenlist[Tuple[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, T0]]]], term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, T0]:
    stripped = hydra.rewriting.deannotate_and_detype_term(term)
    mapping = cast(FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, T0]]], hydra.lib.maps.from_list(pairs))
    match stripped:
        case hydra.core.TermVariable(value=name):
            return hydra.lib.flows.bind(require_element(name), (lambda el: match_union(tname, pairs, el.term)))
        
        case hydra.core.TermUnion(value=injection):
            def exp() -> hydra.compute.Flow[hydra.graph.Graph, T0]:
                fname = injection.field.name
                val = injection.field.term
                return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat((hydra.lib.strings.cat((hydra.lib.strings.cat(("no matching case for field ", fname.value)), " in union type ")), tname.value))), (lambda f: f(val)), hydra.lib.maps.lookup(fname, mapping))
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(injection.type_name.value, tname.value), exp(), hydra.monads.unexpected(hydra.lib.strings.cat(("injection for type ", tname.value)), hydra.show.core.term(term)))
        
        case _:
            return hydra.monads.unexpected(hydra.lib.strings.cat(("inject(", tname.value, ") with one of {", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda tuple2: tuple2[0].value), pairs)), "}")), hydra.show.core.term(stripped))

def match_unit_field[T0, T1, T2, T3](fname: T0, x: T1) -> Tuple[T0, Callable[[T2], hydra.compute.Flow[T3, T1]]]:
    return (fname, (lambda ignored: hydra.lib.flows.pure(x)))

def match_enum[T0](tname: hydra.core.Name, pairs: frozenlist[Tuple[hydra.core.Name, T0]], v1: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, T0]:
    return match_union(tname, hydra.lib.lists.map((lambda tuple2: match_unit_field(tuple2[0], tuple2[1])), pairs), v1)

def match_record[T0, T1](decode: Callable[[FrozenDict[hydra.core.Name, hydra.core.Term]], hydra.compute.Flow[T0, T1]], term: hydra.core.Term) -> hydra.compute.Flow[T0, T1]:
    stripped = hydra.rewriting.deannotate_and_detype_term(term)
    match stripped:
        case hydra.core.TermRecord(value=record):
            return decode(cast(FrozenDict[hydra.core.Name, hydra.core.Term], hydra.lib.maps.from_list(hydra.lib.lists.map((lambda field: (field.name, field.term)), record.fields))))
        
        case _:
            return hydra.monads.unexpected("record", hydra.show.core.term(term))

def require_primitive(name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.graph.Primitive]:
    return hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.graph.Graph, hydra.graph.Graph], hydra.monads.get_state), (lambda g: hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("no such primitive function: ", name.value))), cast(Callable[[hydra.graph.Primitive], hydra.compute.Flow[hydra.graph.Graph, hydra.graph.Primitive]], hydra.lib.flows.pure), lookup_primitive(g, name))))

def require_primitive_type[T0](tx: hydra.typing.TypeContext, name: hydra.core.Name) -> hydra.compute.Flow[T0, hydra.core.TypeScheme]:
    mts = hydra.lib.maps.lookup(name, tx.inference_context.primitive_types)
    return hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("no such primitive function: ", name.value))), (lambda ts: hydra.lib.flows.pure(ts)), mts)

def resolve_term(name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Term]]:
    r"""TODO: distinguish between lambda-bound and let-bound variables."""
    
    def recurse(el: hydra.core.Binding) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Term]]:
        stripped = hydra.rewriting.deannotate_term(el.term)
        match stripped:
            case hydra.core.TermVariable(value=name_):
                return resolve_term(name_)
            
            case _:
                return hydra.lib.flows.pure(cast(Maybe[hydra.core.Term], Just(el.term)))
    return hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.graph.Graph, hydra.graph.Graph], hydra.monads.get_state), (lambda g: hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[hydra.core.Term], Nothing())), recurse, hydra.lib.maps.lookup(name, g.elements))))

def require_term(name: hydra.core.Name) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
    return hydra.lib.flows.bind(resolve_term(name), (lambda mt: hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("no such element: ", name.value))), cast(Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]], hydra.lib.flows.pure), mt)))

def schema_context(g: hydra.graph.Graph) -> hydra.graph.Graph:
    r"""Note: assuming for now that primitive functions are the same in the schema graph."""
    
    return hydra.lib.maybes.from_maybe(g, g.schema)

def strip_and_dereference_term(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
    stripped = hydra.rewriting.deannotate_and_detype_term(term)
    match stripped:
        case hydra.core.TermVariable(value=v):
            return hydra.lib.flows.bind(require_term(v), (lambda t: strip_and_dereference_term(t)))
        
        case _:
            return hydra.lib.flows.pure(stripped)

def with_empty_graph[T0, T1](v1: hydra.compute.Flow[hydra.graph.Graph, T0]) -> hydra.compute.Flow[T1, T0]:
    return hydra.monads.with_state(empty_graph, v1)

def with_schema_context[T0](f: hydra.compute.Flow[hydra.graph.Graph, T0]) -> hydra.compute.Flow[hydra.graph.Graph, T0]:
    return hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.graph.Graph, hydra.graph.Graph], hydra.monads.get_state), (lambda g: hydra.monads.with_state(schema_context(g), f)))
