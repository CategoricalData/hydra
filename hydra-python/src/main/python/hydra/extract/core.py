# Note: this is an automatically generated file. Do not edit.

r"""A DSL for decoding and validating Hydra terms at runtime. This module provides functions to extract typed values from Hydra terms with appropriate error handling."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from hydra.dsl.python import FrozenDict, Maybe, Nothing, frozenlist
from typing import Tuple, cast
import hydra.compute
import hydra.core
import hydra.graph
import hydra.lexical
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.optionals
import hydra.lib.strings
import hydra.monads
import hydra.rewriting
import hydra.show.core

def bigfloat_value[T0](v: hydra.core.FloatValue) -> hydra.compute.Flow[T0, Decimal]:
    match v:
        case hydra.core.FloatValueBigfloat(value=f):
            return hydra.lib.flows.pure(f)
        
        case _:
            return hydra.monads.unexpected("bigfloat", hydra.show.core.float(v))

def float_literal[T0](lit: hydra.core.Literal) -> hydra.compute.Flow[T0, hydra.core.FloatValue]:
    match lit:
        case hydra.core.LiteralFloat(value=v):
            return hydra.lib.flows.pure(v)
        
        case _:
            return hydra.monads.unexpected("floating-point value", hydra.show.core.literal(lit))

def literal(term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Literal]:
    r"""Extract a literal value from a term."""
    
    def extract[T0](term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Literal]:
        match term:
            case hydra.core.TermLiteral(value=lit):
                return hydra.lib.flows.pure(lit)
            
            case _:
                return hydra.monads.unexpected("literal", hydra.show.core.term(term))
    return hydra.lib.flows.bind(hydra.lexical.strip_and_dereference_term(term0), (lambda term: extract(term)))

def bigfloat(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, Decimal]:
    r"""Extract an arbitrary-precision floating-point value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: hydra.lib.flows.bind(float_literal(l), (lambda f: bigfloat_value(f)))))

def bigint_value[T0](v: hydra.core.IntegerValue) -> hydra.compute.Flow[T0, int]:
    match v:
        case hydra.core.IntegerValueBigint(value=i):
            return hydra.lib.flows.pure(i)
        
        case _:
            return hydra.monads.unexpected("bigint", hydra.show.core.integer(v))

def integer_literal[T0](lit: hydra.core.Literal) -> hydra.compute.Flow[T0, hydra.core.IntegerValue]:
    match lit:
        case hydra.core.LiteralInteger(value=v):
            return hydra.lib.flows.pure(v)
        
        case _:
            return hydra.monads.unexpected("integer value", hydra.show.core.literal(lit))

def bigint(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, int]:
    r"""Extract an arbitrary-precision integer value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: hydra.lib.flows.bind(integer_literal(l), (lambda i: bigint_value(i)))))

def binary_literal[T0](v: hydra.core.Literal) -> hydra.compute.Flow[T0, bytes]:
    match v:
        case hydra.core.LiteralBinary(value=b):
            return hydra.lib.flows.pure(b)
        
        case _:
            return hydra.monads.unexpected("binary", hydra.show.core.literal(v))

def binary(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, bytes]:
    r"""Extract a binary data value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: binary_literal(l)))

def boolean_literal[T0](v: hydra.core.Literal) -> hydra.compute.Flow[T0, bool]:
    match v:
        case hydra.core.LiteralBoolean(value=b):
            return hydra.lib.flows.pure(b)
        
        case _:
            return hydra.monads.unexpected("boolean", hydra.show.core.literal(v))

def boolean(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, bool]:
    r"""Extract a boolean value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: boolean_literal(l)))

def cases(name: hydra.core.Name, term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.CaseStatement]:
    r"""Extract case statement from a term."""
    
    def extract[T0](term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.CaseStatement]:
        match term:
            case hydra.core.TermFunction(value=function):
                match function:
                    case hydra.core.FunctionElimination(value=elimination):
                        match elimination:
                            case hydra.core.EliminationUnion(value=cs):
                                return hydra.lib.logic.if_else(hydra.lib.equality.equal(cs.type_name.value, name.value), hydra.lib.flows.pure(cs), hydra.monads.unexpected(hydra.lib.strings.cat(("case statement for type ", name.value)), hydra.show.core.term(term)))
                            
                            case _:
                                return hydra.monads.unexpected("case statement", hydra.show.core.term(term))
                    
                    case _:
                        return hydra.monads.unexpected("case statement", hydra.show.core.term(term))
            
            case _:
                return hydra.monads.unexpected("case statement", hydra.show.core.term(term))
    return hydra.lib.flows.bind(hydra.lexical.strip_and_dereference_term(term0), (lambda term: extract(term)))

def case_field(name: hydra.core.Name, n: str, term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Field]:
    r"""Extract a specific case handler from a case statement term."""
    
    field_name = hydra.core.Name(n)
    return hydra.lib.flows.bind(cases(name, term), (lambda cs: (matching := hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name.value, field_name.value)), cs.cases), hydra.lib.logic.if_else(hydra.lib.lists.null(matching), hydra.lib.flows.fail("not enough cases"), hydra.lib.flows.pure(hydra.lib.lists.head(matching))))[1]))

def field[T0](fname: hydra.core.Name, mapping: Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, T0]], fields: frozenlist[hydra.core.Field]) -> hydra.compute.Flow[hydra.graph.Graph, T0]:
    matching_fields = hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name.value, fname.value)), fields)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(matching_fields), hydra.lib.flows.fail(hydra.lib.strings.cat((hydra.lib.strings.cat(("field ", fname.value)), " not found"))), hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(matching_fields), 1), hydra.lib.flows.bind(hydra.lexical.strip_and_dereference_term(hydra.lib.lists.head(matching_fields).term), (lambda stripped: mapping(stripped))), hydra.lib.flows.fail(hydra.lib.strings.cat(("multiple fields named ", fname.value)))))

def float32_value[T0](v: hydra.core.FloatValue) -> hydra.compute.Flow[T0, float]:
    match v:
        case hydra.core.FloatValueFloat32(value=f):
            return hydra.lib.flows.pure(f)
        
        case _:
            return hydra.monads.unexpected("float32", hydra.show.core.float(v))

def float32(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, float]:
    r"""Extract a 32-bit floating-point value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: hydra.lib.flows.bind(float_literal(l), (lambda f: float32_value(f)))))

def float64_value[T0](v: hydra.core.FloatValue) -> hydra.compute.Flow[T0, float]:
    match v:
        case hydra.core.FloatValueFloat64(value=f):
            return hydra.lib.flows.pure(f)
        
        case _:
            return hydra.monads.unexpected("float64", hydra.show.core.float(v))

def float64(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, float]:
    r"""Extract a 64-bit floating-point value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: hydra.lib.flows.bind(float_literal(l), (lambda f: float64_value(f)))))

def float_value(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.FloatValue]:
    r"""Extract a float value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: float_literal(l)))

def function_type[T0](typ: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.FunctionType]:
    stripped = hydra.rewriting.deannotate_type(typ)
    match stripped:
        case hydra.core.TypeFunction(value=ft):
            return hydra.lib.flows.pure(ft)
        
        case _:
            return hydra.monads.unexpected("function type", hydra.show.core.type(typ))

def injection(expected: hydra.core.Name, term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Field]:
    r"""Extract a field from a union term."""
    
    def extract[T0](term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Field]:
        match term:
            case hydra.core.TermUnion(value=injection):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(injection.type_name.value, expected.value), hydra.lib.flows.pure(injection.field), hydra.monads.unexpected(hydra.lib.strings.cat(("injection of type ", expected.value)), injection.type_name.value))
            
            case _:
                return hydra.monads.unexpected("injection", hydra.show.core.term(term))
    return hydra.lib.flows.bind(hydra.lexical.strip_and_dereference_term(term0), (lambda term: extract(term)))

def int16_value[T0](v: hydra.core.IntegerValue) -> hydra.compute.Flow[T0, int]:
    match v:
        case hydra.core.IntegerValueInt16(value=i):
            return hydra.lib.flows.pure(i)
        
        case _:
            return hydra.monads.unexpected("int16", hydra.show.core.integer(v))

def int16(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, int]:
    r"""Extract a 16-bit signed integer value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: hydra.lib.flows.bind(integer_literal(l), (lambda i: int16_value(i)))))

def int32_value[T0](v: hydra.core.IntegerValue) -> hydra.compute.Flow[T0, int]:
    match v:
        case hydra.core.IntegerValueInt32(value=i):
            return hydra.lib.flows.pure(i)
        
        case _:
            return hydra.monads.unexpected("int32", hydra.show.core.integer(v))

def int32(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, int]:
    r"""Extract a 32-bit signed integer value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: hydra.lib.flows.bind(integer_literal(l), (lambda i: int32_value(i)))))

def int64_value[T0](v: hydra.core.IntegerValue) -> hydra.compute.Flow[T0, int]:
    match v:
        case hydra.core.IntegerValueInt64(value=i):
            return hydra.lib.flows.pure(i)
        
        case _:
            return hydra.monads.unexpected("int64", hydra.show.core.integer(v))

def int64(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, int]:
    r"""Extract a 64-bit signed integer value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: hydra.lib.flows.bind(integer_literal(l), (lambda i: int64_value(i)))))

def int8_value[T0](v: hydra.core.IntegerValue) -> hydra.compute.Flow[T0, int]:
    match v:
        case hydra.core.IntegerValueInt8(value=i):
            return hydra.lib.flows.pure(i)
        
        case _:
            return hydra.monads.unexpected("int8", hydra.show.core.integer(v))

def int8(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, int]:
    r"""Extract an 8-bit signed integer value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: hydra.lib.flows.bind(integer_literal(l), (lambda i: int8_value(i)))))

def integer_value(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.IntegerValue]:
    r"""Extract an integer value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: integer_literal(l)))

def lambda_(term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Lambda]:
    r"""Extract a lambda from a term."""
    
    def extract[T0](term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Lambda]:
        match term:
            case hydra.core.TermFunction(value=function):
                match function:
                    case hydra.core.FunctionLambda(value=l):
                        return hydra.lib.flows.pure(l)
                    
                    case _:
                        return hydra.monads.unexpected("lambda", hydra.show.core.term(term))
            
            case _:
                return hydra.monads.unexpected("lambda", hydra.show.core.term(term))
    return hydra.lib.flows.bind(hydra.lexical.strip_and_dereference_term(term0), (lambda term: extract(term)))

def lambda_body(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
    r"""Extract the body of a lambda term."""
    
    return hydra.lib.flows.map((lambda v1: v1.body), lambda_(term))

def let_term(term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Let]:
    r"""Extract a let expression from a term."""
    
    def extract[T0](term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Let]:
        match term:
            case hydra.core.TermLet(value=lt):
                return hydra.lib.flows.pure(lt)
            
            case _:
                return hydra.monads.unexpected("let term", hydra.show.core.term(term))
    return hydra.lib.flows.bind(hydra.lexical.strip_and_dereference_term(term0), (lambda term: extract(term)))

def let_binding(n: str, term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
    r"""Extract a binding with the given name from a let term."""
    
    name = hydra.core.Name(n)
    return hydra.lib.flows.bind(let_term(term), (lambda let_expr: (matching_bindings := hydra.lib.lists.filter((lambda b: hydra.lib.equality.equal(b.name.value, name.value)), let_expr.bindings), hydra.lib.logic.if_else(hydra.lib.lists.null(matching_bindings), hydra.lib.flows.fail(hydra.lib.strings.cat(("no such binding: ", n))), hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(matching_bindings), 1), hydra.lib.flows.pure(hydra.lib.lists.head(matching_bindings).term), hydra.lib.flows.fail(hydra.lib.strings.cat(("multiple bindings named ", n))))))[1]))

def list(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.core.Term]]:
    r"""Extract a list of terms from a term."""
    
    def extract[T0](stripped: hydra.core.Term) -> hydra.compute.Flow[T0, frozenlist[hydra.core.Term]]:
        match stripped:
            case hydra.core.TermList(value=l):
                return hydra.lib.flows.pure(l)
            
            case _:
                return hydra.monads.unexpected("list", hydra.show.core.term(stripped))
    return hydra.lib.flows.bind(hydra.lexical.strip_and_dereference_term(term), (lambda stripped: extract(stripped)))

def list_head(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
    r"""Extract the first element of a list term."""
    
    return hydra.lib.flows.bind(list(term), (lambda l: hydra.lib.logic.if_else(hydra.lib.lists.null(l), hydra.lib.flows.fail("empty list"), hydra.lib.flows.pure(hydra.lib.lists.head(l)))))

def list_of[T0](f: Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, T0]], term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[T0]]:
    return hydra.lib.flows.bind(list(term), (lambda els: hydra.lib.flows.map_list(f, els)))

def list_type[T0](typ: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
    stripped = hydra.rewriting.deannotate_type(typ)
    match stripped:
        case hydra.core.TypeList(value=t):
            return hydra.lib.flows.pure(t)
        
        case _:
            return hydra.monads.unexpected("list type", hydra.show.core.type(typ))

def map[T0, T1](fk: Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, T0]], fv: Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, T1]], term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[T0, T1]]:
    def pair(kv_pair: Tuple[hydra.core.Term, hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, Tuple[T0, T1]]:
        kterm = kv_pair[0]
        vterm = kv_pair[1]
        return hydra.lib.flows.bind(fk(kterm), (lambda kval: hydra.lib.flows.bind(fv(vterm), (lambda vval: hydra.lib.flows.pure((kval, vval))))))
    def extract(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[T0, T1]]:
        match term:
            case hydra.core.TermMap(value=m):
                return hydra.lib.flows.map(hydra.lib.maps.from_list, hydra.lib.flows.map_list(pair, hydra.lib.maps.to_list(m)))
            
            case _:
                return hydra.monads.unexpected("map", hydra.show.core.term(term))
    return hydra.lib.flows.bind(hydra.lexical.strip_and_dereference_term(term0), (lambda term: extract(term)))

def map_type[T0](typ: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.MapType]:
    stripped = hydra.rewriting.deannotate_type(typ)
    match stripped:
        case hydra.core.TypeMap(value=mt):
            return hydra.lib.flows.pure(mt)
        
        case _:
            return hydra.monads.unexpected("map type", hydra.show.core.type(typ))

def n_args[T0, T1](name: hydra.core.Name, n: int, args: frozenlist[T0]) -> hydra.compute.Flow[T1, None]:
    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(args), n), hydra.lib.flows.pure(None), hydra.monads.unexpected(hydra.lib.strings.cat((hydra.lib.literals.show_int32(n), " arguments to primitive ", hydra.lib.literals.show_string(name.value))), hydra.lib.literals.show_int32(hydra.lib.lists.length(args))))

def optional[T0](f: Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, T0]], term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[T0]]:
    def extract(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[T0]]:
        match term:
            case hydra.core.TermOptional(value=mt):
                return hydra.lib.optionals.maybe(hydra.lib.flows.pure(cast(Maybe[T0], Nothing())), (lambda t: hydra.lib.flows.map(hydra.lib.optionals.pure, f(t))), mt)
            
            case _:
                return hydra.monads.unexpected("optional value", hydra.show.core.term(term))
    return hydra.lib.flows.bind(hydra.lexical.strip_and_dereference_term(term0), (lambda term: extract(term)))

def optional_type[T0](typ: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
    stripped = hydra.rewriting.deannotate_type(typ)
    match stripped:
        case hydra.core.TypeOptional(value=t):
            return hydra.lib.flows.pure(t)
        
        case _:
            return hydra.monads.unexpected("optional type", hydra.show.core.type(typ))

def pair[T0, T1](kf: Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, T0]], vf: Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, T1]], term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, Tuple[T0, T1]]:
    def extract(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, Tuple[T0, T1]]:
        match term:
            case hydra.core.TermProduct(value=terms):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(terms), 2), hydra.lib.flows.bind(kf(hydra.lib.lists.head(terms)), (lambda k_val: hydra.lib.flows.bind(vf(hydra.lib.lists.head(hydra.lib.lists.tail(terms))), (lambda v_val: hydra.lib.flows.pure((k_val, v_val)))))), hydra.monads.unexpected("pair", hydra.show.core.term(term)))
            
            case _:
                return hydra.monads.unexpected("product", hydra.show.core.term(term))
    return hydra.lib.flows.bind(hydra.lexical.strip_and_dereference_term(term0), (lambda term: extract(term)))

def product_type[T0](typ: hydra.core.Type) -> hydra.compute.Flow[T0, frozenlist[hydra.core.Type]]:
    stripped = hydra.rewriting.deannotate_type(typ)
    match stripped:
        case hydra.core.TypeProduct(value=types):
            return hydra.lib.flows.pure(types)
        
        case _:
            return hydra.monads.unexpected("product type", hydra.show.core.type(typ))

def term_record(term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Record]:
    r"""Extract a record from a term."""
    
    def extract[T0](term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Record]:
        match term:
            case hydra.core.TermRecord(value=record):
                return hydra.lib.flows.pure(record)
            
            case _:
                return hydra.monads.unexpected("record", hydra.show.core.term(term))
    return hydra.lib.flows.bind(hydra.lexical.strip_and_dereference_term(term0), (lambda term: extract(term)))

def record(expected: hydra.core.Name, term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.core.Field]]:
    r"""Extract a record's fields from a term."""
    
    return hydra.lib.flows.bind(term_record(term0), (lambda record: hydra.lib.logic.if_else(hydra.lib.equality.equal(record.type_name, expected), hydra.lib.flows.pure(record.fields), hydra.monads.unexpected(hydra.lib.strings.cat(("record of type ", expected.value)), record.type_name.value))))

def record_type[T0](ename: hydra.core.Name, typ: hydra.core.Type) -> hydra.compute.Flow[T0, frozenlist[hydra.core.FieldType]]:
    stripped = hydra.rewriting.deannotate_type(typ)
    match stripped:
        case hydra.core.TypeRecord(value=row_type):
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(row_type.type_name.value, ename.value), hydra.lib.flows.pure(row_type.fields), hydra.monads.unexpected(hydra.lib.strings.cat(("record of type ", ename.value)), hydra.lib.strings.cat(("record of type ", row_type.type_name.value))))
        
        case _:
            return hydra.monads.unexpected("record type", hydra.show.core.type(typ))

def set(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, frozenset[hydra.core.Term]]:
    r"""Extract a set of terms from a term."""
    
    def extract[T0](stripped: hydra.core.Term) -> hydra.compute.Flow[T0, frozenset[hydra.core.Term]]:
        match stripped:
            case hydra.core.TermSet(value=s):
                return hydra.lib.flows.pure(s)
            
            case _:
                return hydra.monads.unexpected("set", hydra.show.core.term(stripped))
    return hydra.lib.flows.bind(hydra.lexical.strip_and_dereference_term(term), (lambda stripped: extract(stripped)))

def set_of[T0](f: Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, T0]], term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, frozenset[T0]]:
    return hydra.lib.flows.bind(set(term), (lambda els: hydra.lib.flows.map_set(f, els)))

def set_type[T0](typ: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
    stripped = hydra.rewriting.deannotate_type(typ)
    match stripped:
        case hydra.core.TypeSet(value=t):
            return hydra.lib.flows.pure(t)
        
        case _:
            return hydra.monads.unexpected("set type", hydra.show.core.type(typ))

def string_literal[T0](v: hydra.core.Literal) -> hydra.compute.Flow[T0, str]:
    match v:
        case hydra.core.LiteralString(value=s):
            return hydra.lib.flows.pure(s)
        
        case _:
            return hydra.monads.unexpected("string", hydra.show.core.literal(v))

def string(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, str]:
    r"""Extract a string value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: string_literal(l)))

def sum_type[T0](typ: hydra.core.Type) -> hydra.compute.Flow[T0, frozenlist[hydra.core.Type]]:
    stripped = hydra.rewriting.deannotate_type(typ)
    match stripped:
        case hydra.core.TypeSum(value=types):
            return hydra.lib.flows.pure(types)
        
        case _:
            return hydra.monads.unexpected("sum type", hydra.show.core.type(typ))

def uint16_value[T0](v: hydra.core.IntegerValue) -> hydra.compute.Flow[T0, int]:
    match v:
        case hydra.core.IntegerValueUint16(value=i):
            return hydra.lib.flows.pure(i)
        
        case _:
            return hydra.monads.unexpected("uint16", hydra.show.core.integer(v))

def uint16(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, int]:
    r"""Extract a 16-bit unsigned integer value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: hydra.lib.flows.bind(integer_literal(l), (lambda i: uint16_value(i)))))

def uint32_value[T0](v: hydra.core.IntegerValue) -> hydra.compute.Flow[T0, int]:
    match v:
        case hydra.core.IntegerValueUint32(value=i):
            return hydra.lib.flows.pure(i)
        
        case _:
            return hydra.monads.unexpected("uint32", hydra.show.core.integer(v))

def uint32(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, int]:
    r"""Extract a 32-bit unsigned integer value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: hydra.lib.flows.bind(integer_literal(l), (lambda i: uint32_value(i)))))

def uint64_value[T0](v: hydra.core.IntegerValue) -> hydra.compute.Flow[T0, int]:
    match v:
        case hydra.core.IntegerValueUint64(value=i):
            return hydra.lib.flows.pure(i)
        
        case _:
            return hydra.monads.unexpected("uint64", hydra.show.core.integer(v))

def uint64(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, int]:
    r"""Extract a 64-bit unsigned integer value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: hydra.lib.flows.bind(integer_literal(l), (lambda i: uint64_value(i)))))

def uint8_value[T0](v: hydra.core.IntegerValue) -> hydra.compute.Flow[T0, int]:
    match v:
        case hydra.core.IntegerValueUint8(value=i):
            return hydra.lib.flows.pure(i)
        
        case _:
            return hydra.monads.unexpected("uint8", hydra.show.core.integer(v))

def uint8(t: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, int]:
    r"""Extract an 8-bit unsigned integer value from a term."""
    
    return hydra.lib.flows.bind(literal(t), (lambda l: hydra.lib.flows.bind(integer_literal(l), (lambda i: uint8_value(i)))))

def union_type[T0](ename: hydra.core.Name, typ: hydra.core.Type) -> hydra.compute.Flow[T0, frozenlist[hydra.core.FieldType]]:
    stripped = hydra.rewriting.deannotate_type(typ)
    match stripped:
        case hydra.core.TypeUnion(value=row_type):
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(row_type.type_name, ename), hydra.lib.flows.pure(row_type.fields), hydra.monads.unexpected(hydra.lib.strings.cat(("union of type ", ename.value)), hydra.lib.strings.cat(("union of type ", row_type.type_name.value))))
        
        case _:
            return hydra.monads.unexpected("union type", hydra.show.core.type(typ))

def unit[T0](term: hydra.core.Term) -> hydra.compute.Flow[T0, None]:
    match term:
        case hydra.core.TermUnit():
            return hydra.lib.flows.pure(None)
        
        case _:
            return hydra.monads.unexpected("unit", hydra.show.core.term(term))

# Extract a field from a union term (alias for injection).
variant = injection

def unit_variant(tname: hydra.core.Name, term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Name]:
    r"""Extract a unit variant (a variant with an empty record value) from a union term."""
    
    return hydra.lib.flows.bind(variant(tname, term), (lambda field: hydra.lib.flows.bind(unit(field.term), (lambda ignored: hydra.lib.flows.pure(field.name)))))

def wrap(expected: hydra.core.Name, term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
    r"""Extract the wrapped value from a wrapped term."""
    
    def extract[T0](term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
        match term:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(wrapped_term.type_name.value, expected.value), hydra.lib.flows.pure(wrapped_term.body), hydra.monads.unexpected(hydra.lib.strings.cat(("wrapper of type ", expected.value)), wrapped_term.type_name.value))
            
            case _:
                return hydra.monads.unexpected(hydra.lib.strings.cat((hydra.lib.strings.cat(("wrap(", expected.value)), ")")), hydra.show.core.term(term))
    return hydra.lib.flows.bind(hydra.lexical.strip_and_dereference_term(term0), (lambda term: extract(term)))

def wrapped_type[T0](ename: hydra.core.Name, typ: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
    stripped = hydra.rewriting.deannotate_type(typ)
    match stripped:
        case hydra.core.TypeWrap(value=wrapped_type):
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(wrapped_type.type_name.value, ename.value), hydra.lib.flows.pure(wrapped_type.body), hydra.monads.unexpected(hydra.lib.strings.cat(("wrapped type ", ename.value)), hydra.lib.strings.cat(("wrapped type ", wrapped_type.type_name.value))))
        
        case _:
            return hydra.monads.unexpected("wrapped type", hydra.show.core.type(typ))
