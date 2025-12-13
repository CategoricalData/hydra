# Note: this is an automatically generated file. Do not edit.

r"""JSON encoding and decoding for Hydra terms."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import cast
import hydra.adapt.modules
import hydra.adapt.utils
import hydra.compute
import hydra.core
import hydra.encode.core
import hydra.ext.org.json.language
import hydra.extract.core
import hydra.graph
import hydra.json
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.literals
import hydra.monads
import hydra.rewriting
import hydra.show.core

def show_value[T0](value: T0) -> str:
    return "TODO: implement showValue"

def decode_record[T0, T1](rt: hydra.core.RowType, coders: frozenlist[tuple[hydra.core.FieldType, hydra.compute.Coder[T0, T1, hydra.core.Term, hydra.json.Value]]], n: hydra.json.Value) -> hydra.compute.Flow[T1, hydra.core.Term]:
    def decode_object_body(m: FrozenDict[str, hydra.json.Value]) -> hydra.compute.Flow[T1, hydra.core.Term]:
        def decode_field[T2, T3](coder: tuple[hydra.core.FieldType, hydra.compute.Coder[T2, T3, hydra.core.Term, hydra.json.Value]]) -> hydra.compute.Flow[T3, hydra.core.Field]:
            def ft() -> hydra.core.FieldType:
                return hydra.lib.pairs.first(coder)
            def coder_() -> hydra.compute.Coder[T2, T3, hydra.core.Term, hydra.json.Value]:
                return hydra.lib.pairs.second(coder)
            def fname() -> hydra.core.Name:
                return ft().name
            default_value = cast(hydra.json.Value, hydra.json.ValueNull())
            def json_value() -> hydra.json.Value:
                return hydra.lib.maybes.from_maybe(default_value, hydra.lib.maps.lookup(fname().value, m))
            return hydra.lib.flows.bind(coder_().decode(json_value()), (lambda v: hydra.lib.flows.pure(hydra.core.Field(fname(), v))))
        return hydra.lib.flows.bind(hydra.lib.flows.map_list(cast(Callable[[
          tuple[hydra.core.FieldType, hydra.compute.Coder[T0, T1, hydra.core.Term, hydra.json.Value]]], hydra.compute.Flow[T1, hydra.core.Field]], (lambda x1: decode_field(x1))), coders), (lambda fields: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(rt.type_name, fields))))))
    def result() -> hydra.compute.Flow[T1, hydra.core.Term]:
        match n:
            case hydra.json.ValueObject(value=v1):
                return decode_object_body(v1)
            
            case _:
                return hydra.monads.unexpected("object", show_value(n))
    return result()

def encode_record[T0](coders: frozenlist[tuple[hydra.core.FieldType, hydra.compute.Coder[hydra.graph.Graph, T0, hydra.core.Term, hydra.json.Value]]], term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.json.Value]:
    stripped = hydra.rewriting.deannotate_term(term)
    def match_maybe_term[T1, T2, T3](fvalue: hydra.core.Term, coder_: hydra.compute.Coder[T1, T2, hydra.core.Term, T3], fname: hydra.core.Name, dflt: hydra.compute.Flow[T1, Maybe[tuple[str, T3]]]) -> hydra.compute.Flow[T1, Maybe[tuple[str, T3]]]:
        match fvalue:
            case hydra.core.TermMaybe(value=opt):
                return hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[tuple[str, T3]], Nothing())), (lambda v: hydra.lib.flows.bind(coder_.encode(v), (lambda encoded: hydra.lib.flows.pure(cast(Maybe[tuple[str, T3]], Just(cast(tuple[str, T3], (fname.value, encoded)))))))), opt)
            
            case _:
                return dflt
    def match_type_for_maybe[T1](ft: hydra.core.FieldType, for_maybe: Callable[[hydra.core.Type], T1], dflt: T1) -> T1:
        match ft.type:
            case hydra.core.TypeMaybe(value=ot):
                return for_maybe(ot)
            
            case _:
                return dflt
    def encode_field[T1, T2, T3](coder_and_field: tuple[tuple[hydra.core.FieldType, hydra.compute.Coder[T1, T2, hydra.core.Term, T3]], hydra.core.Field]) -> hydra.compute.Flow[T1, Maybe[tuple[str, T3]]]:
        def coder() -> tuple[hydra.core.FieldType, hydra.compute.Coder[T1, T2, hydra.core.Term, T3]]:
            return hydra.lib.pairs.first(coder_and_field)
        def field() -> hydra.core.Field:
            return hydra.lib.pairs.second(coder_and_field)
        def ft() -> hydra.core.FieldType:
            return hydra.lib.pairs.first(coder())
        def coder_() -> hydra.compute.Coder[T1, T2, hydra.core.Term, T3]:
            return hydra.lib.pairs.second(coder())
        def fname() -> hydra.core.Name:
            return field().name
        def fvalue() -> hydra.core.Term:
            return field().term
        def for_maybe[T4](ot: T4) -> hydra.compute.Flow[T1, Maybe[tuple[str, T3]]]:
            def dflt() -> hydra.compute.Flow[T1, Maybe[tuple[str, T3]]]:
                return hydra.lib.flows.bind(coder_().encode(fvalue()), (lambda encoded: hydra.lib.flows.pure(cast(Maybe[tuple[str, T3]], Just(cast(tuple[str, T3], (fname().value, encoded)))))))
            return match_maybe_term(fvalue(), coder_(), fname(), dflt())
        def dflt() -> hydra.compute.Flow[T1, Maybe[tuple[str, T3]]]:
            return hydra.lib.flows.bind(coder_().encode(fvalue()), (lambda encoded: hydra.lib.flows.pure(cast(Maybe[tuple[str, T3]], Just(cast(tuple[str, T3], (fname().value, encoded)))))))
        return match_type_for_maybe(ft(), cast(Callable[[hydra.core.Type], hydra.compute.Flow[T1, Maybe[tuple[str, T3]]]], (lambda x1: for_maybe(x1))), dflt())
    return hydra.lib.flows.bind(hydra.extract.core.term_record(stripped), (lambda record: (fields := record.fields, hydra.lib.flows.bind(hydra.lib.flows.map_list(cast(Callable[[
      tuple[tuple[hydra.core.FieldType, hydra.compute.Coder[hydra.graph.Graph, T0, hydra.core.Term, hydra.json.Value]], hydra.core.Field]], hydra.compute.Flow[hydra.graph.Graph, Maybe[tuple[str, hydra.json.Value]]]], (lambda x1: encode_field(x1))), hydra.lib.lists.zip(coders, fields)), (lambda maybe_fields: hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueObject(cast(FrozenDict[str, hydra.json.Value], hydra.lib.maps.from_list(hydra.lib.maybes.cat(maybe_fields)))))))))[1]))

def literal_json_coder[T0, T1, T2](lt: hydra.core.LiteralType) -> hydra.compute.Flow[T0, hydra.compute.Coder[T1, T2, hydra.core.Literal, hydra.json.Value]]:
    def decode_bool[T3](s: hydra.json.Value) -> hydra.compute.Flow[T3, hydra.core.Literal]:
        match s:
            case hydra.json.ValueBoolean(value=b):
                return hydra.lib.flows.pure(cast(hydra.core.Literal, hydra.core.LiteralBoolean(b)))
            
            case _:
                return hydra.monads.unexpected("boolean", show_value(s))
    def decode_float[T3](s: hydra.json.Value) -> hydra.compute.Flow[T3, hydra.core.Literal]:
        match s:
            case hydra.json.ValueNumber(value=f):
                return hydra.lib.flows.pure(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueBigfloat(f)))))
            
            case _:
                return hydra.monads.unexpected("number", show_value(s))
    def decode_integer[T3](s: hydra.json.Value) -> hydra.compute.Flow[T3, hydra.core.Literal]:
        match s:
            case hydra.json.ValueNumber(value=f):
                bi = hydra.lib.literals.bigfloat_to_bigint(f)
                return hydra.lib.flows.pure(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueBigint(bi)))))
            
            case _:
                return hydra.monads.unexpected("number", show_value(s))
    def decode_string[T3](s: hydra.json.Value) -> hydra.compute.Flow[T3, hydra.core.Literal]:
        match s:
            case hydra.json.ValueString(value=s_):
                return hydra.lib.flows.pure(cast(hydra.core.Literal, hydra.core.LiteralString(s_)))
            
            case _:
                return hydra.monads.unexpected("string", show_value(s))
    def encoded[T3, T4]() -> hydra.compute.Coder[T3, T4, hydra.core.Literal, hydra.json.Value]:
        match lt:
            case hydra.core.LiteralTypeBoolean():
                return cast(hydra.compute.Coder[T3, T4, hydra.core.Literal, hydra.json.Value], hydra.compute.Coder((lambda lit: hydra.lib.flows.bind(hydra.extract.core.boolean_literal(lit), (lambda b: hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueBoolean(b)))))), cast(Callable[[hydra.json.Value], hydra.compute.Flow[T4, hydra.core.Literal]], (lambda x1: decode_bool(x1)))))
            
            case hydra.core.LiteralTypeFloat():
                return cast(hydra.compute.Coder[T3, T4, hydra.core.Literal, hydra.json.Value], hydra.compute.Coder((lambda lit: hydra.lib.flows.bind(hydra.extract.core.float_literal(lit), (lambda f: hydra.lib.flows.bind(hydra.extract.core.bigfloat_value(f), (lambda bf: hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueNumber(bf)))))))), cast(Callable[[hydra.json.Value], hydra.compute.Flow[T4, hydra.core.Literal]], (lambda x1: decode_float(x1)))))
            
            case hydra.core.LiteralTypeInteger():
                return cast(hydra.compute.Coder[T3, T4, hydra.core.Literal, hydra.json.Value], hydra.compute.Coder((lambda lit: hydra.lib.flows.bind(hydra.extract.core.integer_literal(lit), (lambda i: hydra.lib.flows.bind(hydra.extract.core.bigint_value(i), (lambda bi: hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(bi))))))))), cast(Callable[[hydra.json.Value], hydra.compute.Flow[T4, hydra.core.Literal]], (lambda x1: decode_integer(x1)))))
            
            case hydra.core.LiteralTypeString():
                return cast(hydra.compute.Coder[T3, T4, hydra.core.Literal, hydra.json.Value], hydra.compute.Coder((lambda lit: hydra.lib.flows.bind(hydra.extract.core.string_literal(lit), (lambda s: hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueString(s)))))), cast(Callable[[hydra.json.Value], hydra.compute.Flow[T4, hydra.core.Literal]], (lambda x1: decode_string(x1)))))
            
            case _:
                raise TypeError("Unsupported LiteralType")
    return hydra.lib.flows.pure(cast(hydra.compute.Coder[T1, T2, hydra.core.Literal, hydra.json.Value], encoded()))

def read_string_stub(s: str) -> hydra.core.Term:
    r"""Placeholder for reading a string into a term (to be implemented)."""
    
    return cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.strings.cat2("TODO: read ", s)))))

def unit_coder[T0, T1]() -> hydra.compute.Coder[T0, T1, hydra.core.Term, hydra.json.Value]:
    def encode_unit[T2](term: hydra.core.Term) -> hydra.compute.Flow[T2, hydra.json.Value]:
        match hydra.rewriting.deannotate_term(term):
            case hydra.core.TermUnit():
                return hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueNull()))
            
            case _:
                return hydra.monads.unexpected("unit", hydra.show.core.term(term))
    def decode_unit[T2](n: hydra.json.Value) -> hydra.compute.Flow[T2, hydra.core.Term]:
        match n:
            case hydra.json.ValueNull():
                return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermUnit()))
            
            case _:
                return hydra.monads.unexpected("null", show_value(n))
    return cast(hydra.compute.Coder[T0, T1, hydra.core.Term, hydra.json.Value], hydra.compute.Coder(cast(Callable[[hydra.core.Term], hydra.compute.Flow[T0, hydra.json.Value]], (lambda x1: encode_unit(x1))), cast(Callable[[hydra.json.Value], hydra.compute.Flow[T1, hydra.core.Term]], (lambda x1: decode_unit(x1)))))

def record_coder[T0, T1](rt: hydra.core.RowType) -> hydra.compute.Flow[T0, hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value]]:
    fields = rt.fields
    def get_coder(f: hydra.core.FieldType) -> hydra.compute.Flow[T0, tuple[hydra.core.FieldType, hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value]]]:
        return hydra.lib.flows.bind(term_coder(f.type), (lambda coder: hydra.lib.flows.pure(cast(tuple[hydra.core.FieldType, hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value]], (f, coder)))))
    return hydra.lib.flows.bind(hydra.lib.flows.map_list(get_coder, fields), (lambda coders: hydra.lib.flows.pure(cast(hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value], hydra.compute.Coder((lambda v1: encode_record(coders, v1)), (lambda v1: decode_record(rt, coders, v1)))))))

def term_coder[T0, T1](typ: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value]]:
    stripped = hydra.rewriting.deannotate_type(typ)
    def encode_literal[T2, T3, T4](ac: hydra.compute.Coder[T2, T3, hydra.core.Literal, T4], term: hydra.core.Term) -> hydra.compute.Flow[T2, T4]:
        match term:
            case hydra.core.TermLiteral(value=av):
                return ac.encode(av)
            
            case _:
                return hydra.monads.unexpected("literal term", hydra.show.core.term(term))
    def encode_list[T2, T3](lc: hydra.compute.Coder[T2, T3, hydra.core.Term, hydra.json.Value], term: hydra.core.Term) -> hydra.compute.Flow[T2, hydra.json.Value]:
        match term:
            case hydra.core.TermList(value=els):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(lc.encode, els), (lambda encoded_els: hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueArray(encoded_els)))))
            
            case _:
                return hydra.monads.unexpected("list term", hydra.show.core.term(term))
    def decode_list[T2, T3](lc: hydra.compute.Coder[T2, T3, hydra.core.Term, hydra.json.Value], n: hydra.json.Value) -> hydra.compute.Flow[T3, hydra.core.Term]:
        match n:
            case hydra.json.ValueArray(value=nodes):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(lc.decode, nodes), (lambda decoded_nodes: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermList(decoded_nodes)))))
            
            case _:
                return hydra.monads.unexpected("sequence", show_value(n))
    def match_literal_string(v: hydra.core.Term, lit: hydra.core.Literal) -> str:
        match lit:
            case hydra.core.LiteralString(value=s):
                return s
            
            case _:
                return hydra.show.core.term(v)
    def match_term_literal(v: hydra.core.Term) -> str:
        match hydra.rewriting.deannotate_term(v):
            case hydra.core.TermLiteral(value=lit):
                return match_literal_string(v, lit)
            
            case _:
                return hydra.show.core.term(v)
    def encode_map[T2](encode_entry: Callable[[tuple[hydra.core.Term, hydra.core.Term]], hydra.compute.Flow[T2, tuple[str, hydra.json.Value]]], term: hydra.core.Term) -> hydra.compute.Flow[T2, hydra.json.Value]:
        match term:
            case hydra.core.TermMap(value=m):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(encode_entry, hydra.lib.maps.to_list(m)), (lambda entries: hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueObject(cast(FrozenDict[str, hydra.json.Value], hydra.lib.maps.from_list(entries)))))))
            
            case _:
                return hydra.monads.unexpected("map term", hydra.show.core.term(term))
    def decode_map[T2](decode_entry: Callable[[tuple[str, hydra.json.Value]], hydra.compute.Flow[T2, tuple[hydra.core.Term, hydra.core.Term]]], n: hydra.json.Value) -> hydra.compute.Flow[T2, hydra.core.Term]:
        match n:
            case hydra.json.ValueObject(value=m):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(decode_entry, hydra.lib.maps.to_list(m)), (lambda entries: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMap(cast(FrozenDict[hydra.core.Term, hydra.core.Term], hydra.lib.maps.from_list(entries)))))))
            
            case _:
                return hydra.monads.unexpected("mapping", show_value(n))
    def encode_maybe[T2, T3](maybe_element_coder: hydra.compute.Coder[T2, T3, hydra.core.Term, hydra.json.Value], maybe_term: hydra.core.Term) -> hydra.compute.Flow[T2, hydra.json.Value]:
        stripped_maybe_term = hydra.rewriting.deannotate_term(maybe_term)
        match stripped_maybe_term:
            case hydra.core.TermMaybe(value=maybe_contents):
                return hydra.lib.logic.if_else(hydra.lib.maybes.is_nothing(maybe_contents), (lambda : hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueNull()))), (lambda : hydra.lib.flows.bind(maybe_element_coder.encode(hydra.lib.maybes.from_just(maybe_contents)), (lambda encoded_inner: hydra.lib.flows.pure(encoded_inner)))))
            
            case _:
                return hydra.monads.unexpected("optional term", hydra.show.core.term(maybe_term))
    def decode_maybe[T2, T3](maybe_element_coder: hydra.compute.Coder[T2, T3, hydra.core.Term, hydra.json.Value], json_val: hydra.json.Value) -> hydra.compute.Flow[T3, hydra.core.Term]:
        match json_val:
            case hydra.json.ValueNull():
                return hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMaybe(cast(Maybe[hydra.core.Term], Nothing()))))
            
            case _:
                return hydra.lib.flows.bind(maybe_element_coder.decode(json_val), (lambda decoded_inner: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermMaybe(cast(Maybe[hydra.core.Term], Just(decoded_inner)))))))
    def result() -> hydra.compute.Flow[T0, hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value]]:
        match stripped:
            case hydra.core.TypeLiteral(value=at):
                return hydra.lib.flows.bind(literal_json_coder(at), (lambda ac: hydra.lib.flows.pure(cast(hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value], hydra.compute.Coder((lambda v1: encode_literal(ac, v1)), (lambda n: hydra.lib.flows.bind(ac.decode(n), (lambda lit: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermLiteral(lit)))))))))))
            
            case hydra.core.TypeList(value=lt):
                return hydra.lib.flows.bind(term_coder(lt), (lambda lc: hydra.lib.flows.pure(cast(hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value], hydra.compute.Coder((lambda v1: encode_list(lc, v1)), (lambda v1: decode_list(lc, v1)))))))
            
            case hydra.core.TypeMap(value=mt):
                kt = mt.keys
                vt = mt.values
                return hydra.lib.flows.bind(term_coder(kt), (lambda kc: hydra.lib.flows.bind(term_coder(vt), (lambda vc: hydra.lib.flows.bind(cast(hydra.compute.Flow[T0, T0], hydra.monads.get_state()), (lambda cx: (is_string_key := (lambda : hydra.lib.equality.equal(hydra.rewriting.deannotate_type(kt), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))))), to_string := (lambda v: hydra.lib.logic.if_else(is_string_key(), (lambda : match_term_literal(v)), (lambda : hydra.show.core.term(v)))), from_string := (lambda s: hydra.lib.logic.if_else(is_string_key(), (lambda : cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s))))), (lambda : read_string_stub(s)))), encode_entry := (lambda kv: (k := (lambda : hydra.lib.pairs.first(kv)), v := (lambda : hydra.lib.pairs.second(kv)), hydra.lib.flows.bind(vc.encode(v()), (lambda encoded_v: hydra.lib.flows.pure(cast(tuple[str, hydra.json.Value], (to_string(k()), encoded_v))))))[2]), decode_entry := (lambda kv: (k := (lambda : hydra.lib.pairs.first(kv)), v := (lambda : hydra.lib.pairs.second(kv)), hydra.lib.flows.bind(vc.decode(v()), (lambda decoded_v: hydra.lib.flows.pure(cast(tuple[hydra.core.Term, hydra.core.Term], (from_string(k()), decoded_v))))))[2]), hydra.lib.flows.pure(cast(hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value], hydra.compute.Coder((lambda v1: encode_map(encode_entry, v1)), (lambda v1: decode_map(decode_entry, v1))))))[5]))))))
            
            case hydra.core.TypeMaybe(value=maybe_element_type):
                return hydra.lib.flows.bind(term_coder(maybe_element_type), (lambda maybe_element_coder: hydra.lib.flows.pure(cast(hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value], hydra.compute.Coder((lambda v1: encode_maybe(maybe_element_coder, v1)), (lambda v1: decode_maybe(maybe_element_coder, v1)))))))
            
            case hydra.core.TypeRecord(value=rt):
                return record_coder(rt)
            
            case hydra.core.TypeUnit():
                return hydra.lib.flows.pure(cast(hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value], unit_coder()))
            
            case hydra.core.TypeVariable(value=name):
                return hydra.lib.flows.pure(cast(hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value], hydra.compute.Coder((lambda term: hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueString(hydra.lib.strings.cat(("variable '", name.value, "' for: ", hydra.show.core.term(term))))))), (lambda term: hydra.lib.flows.fail(hydra.lib.strings.cat(("type variable ", name.value, " does not support decoding")))))))
            
            case _:
                return hydra.lib.flows.fail(hydra.lib.strings.cat(("unsupported type in JSON: ", hydra.show.core.type(typ))))
    return result()

def json_coder[T0](typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.compute.Coder[hydra.graph.Graph, T0, hydra.core.Term, hydra.json.Value]]:
    return hydra.lib.flows.bind(hydra.adapt.modules.language_adapter(hydra.ext.org.json.language.json_language(), typ), (lambda adapter: hydra.lib.flows.bind(term_coder(adapter.target), (lambda coder: hydra.lib.flows.pure(hydra.adapt.utils.compose_coders(adapter.coder, coder))))))

def untyped_term_to_json[T0](term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.json.Value]:
    def unexp[T1](msg: str) -> hydra.compute.Flow[T1, hydra.json.Value]:
        return hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueString(hydra.lib.strings.cat2("FAIL: ", msg))))
    def as_record(fields: frozenlist[hydra.core.Field]) -> hydra.compute.Flow[T0, hydra.json.Value]:
        return untyped_term_to_json(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name(""), fields))))
    def as_variant(name: str, term2: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.json.Value]:
        return untyped_term_to_json(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name(""), hydra.core.Field(hydra.core.Name(name), term2)))))
    def match_term_maybe(for_term: Callable[[hydra.core.Term], hydra.compute.Flow[T0, Maybe[hydra.json.Value]]], t: hydra.core.Term) -> hydra.compute.Flow[T0, Maybe[hydra.json.Value]]:
        match t:
            case hydra.core.TermMaybe(value=mt):
                return hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[hydra.json.Value], Nothing())), for_term, mt)
            
            case _:
                return hydra.lib.flows.map(cast(Callable[[hydra.json.Value], Maybe[hydra.json.Value]], (lambda x1: hydra.lib.maybes.pure(x1))), untyped_term_to_json(t))
    def match_elimination[T1](unexp: Callable[[str], T1], as_variant: Callable[[str, hydra.core.Term], T1], elm: hydra.core.Elimination) -> T1:
        match elm:
            case hydra.core.EliminationRecord(value=proj):
                return as_variant("project", cast(hydra.core.Term, hydra.core.TermVariable(proj.field)))
            
            case _:
                return unexp(hydra.lib.strings.cat(("unexpected elimination variant: ", hydra.show.core.elimination(elm))))
    def match_function[T1](unexp: Callable[[str], hydra.compute.Flow[T1, hydra.json.Value]], as_record: Callable[[frozenlist[hydra.core.Field]], hydra.compute.Flow[T1, hydra.json.Value]], as_variant: Callable[[str, hydra.core.Term], hydra.compute.Flow[T1, hydra.json.Value]], f: hydra.core.Function) -> hydra.compute.Flow[T1, hydra.json.Value]:
        match f:
            case hydra.core.FunctionElimination(value=elm):
                return match_elimination(unexp, as_variant, elm)
            
            case hydra.core.FunctionLambda(value=l):
                return as_record((hydra.core.Field(hydra.core.Name("parameter"), cast(hydra.core.Term, hydra.core.TermVariable(l.parameter))), hydra.core.Field(hydra.core.Name("domain"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(hydra.encode.core.type, l.domain)))), hydra.core.Field(hydra.core.Name("body"), l.body)))
            
            case hydra.core.FunctionPrimitive(value=name):
                return hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueString(name.value)))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def match_literal(lit: hydra.core.Literal) -> hydra.json.Value:
        match lit:
            case hydra.core.LiteralBinary(value=b):
                return cast(hydra.json.Value, hydra.json.ValueString(hydra.lib.literals.binary_to_string(b)))
            
            case hydra.core.LiteralBoolean(value=b2):
                return cast(hydra.json.Value, hydra.json.ValueBoolean(b2))
            
            case hydra.core.LiteralFloat(value=f):
                return cast(hydra.json.Value, hydra.json.ValueNumber(hydra.literals.float_value_to_bigfloat(f)))
            
            case hydra.core.LiteralInteger(value=i):
                bf = hydra.literals.integer_value_to_bigint(i)
                f = hydra.lib.literals.bigint_to_bigfloat(bf)
                return cast(hydra.json.Value, hydra.json.ValueNumber(f))
            
            case hydra.core.LiteralString(value=s):
                return cast(hydra.json.Value, hydra.json.ValueString(s))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def field_to_keyval(f: hydra.core.Field) -> hydra.compute.Flow[T0, Maybe[tuple[str, hydra.json.Value]]]:
        def for_term(t: hydra.core.Term) -> hydra.compute.Flow[T0, Maybe[hydra.json.Value]]:
            return match_term_maybe(for_term, t)
        return hydra.lib.flows.bind(for_term(f.term), (lambda mjson: hydra.lib.flows.pure(hydra.lib.maybes.map((lambda j: cast(tuple[str, hydra.json.Value], (f.name.value, j))), mjson))))
    def result() -> hydra.compute.Flow[T0, hydra.json.Value]:
        match term:
            case hydra.core.TermAnnotated(value=at):
                term1 = at.body
                ann = at.annotation
                def encode_pair(kv: tuple[hydra.core.Name, hydra.core.Term]) -> hydra.compute.Flow[T0, tuple[str, hydra.json.Value]]:
                    def k() -> str:
                        return hydra.lib.pairs.first(kv).value
                    def v() -> hydra.core.Term:
                        return hydra.lib.pairs.second(kv)
                    return hydra.lib.flows.bind(untyped_term_to_json(v()), (lambda json: hydra.lib.flows.pure(cast(tuple[str, hydra.json.Value], (k(), json)))))
                return hydra.lib.flows.bind(untyped_term_to_json(term1), (lambda json: hydra.lib.flows.bind(hydra.lib.flows.map_list(encode_pair, hydra.lib.maps.to_list(ann)), (lambda pairs: hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueObject(cast(FrozenDict[str, hydra.json.Value], hydra.lib.maps.from_list((cast(tuple[str, hydra.json.Value], ("term", json)), cast(tuple[str, hydra.json.Value], ("annotations", cast(hydra.json.Value, hydra.json.ValueObject(cast(FrozenDict[str, hydra.json.Value], hydra.lib.maps.from_list(pairs))))))))))))))))
            
            case hydra.core.TermApplication(value=app):
                return as_record((hydra.core.Field(hydra.core.Name("function"), app.function), hydra.core.Field(hydra.core.Name("argument"), app.argument)))
            
            case hydra.core.TermFunction(value=f):
                return match_function(cast(Callable[[str], hydra.compute.Flow[T0, hydra.json.Value]], (lambda x1: unexp(x1))), as_record, as_variant, f)
            
            case hydra.core.TermLet(value=lt):
                bindings = lt.bindings
                env = lt.body
                def from_binding(b: hydra.core.Binding) -> hydra.core.Field:
                    return hydra.core.Field(b.name, b.term)
                return as_record((hydra.core.Field(hydra.core.Name("bindings"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name(""), hydra.lib.lists.map(from_binding, bindings))))), hydra.core.Field(hydra.core.Name("environment"), env)))
            
            case hydra.core.TermList(value=terms):
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(cast(Callable[[hydra.core.Term], hydra.compute.Flow[T0, hydra.json.Value]], (lambda x1: untyped_term_to_json(x1))), terms), (lambda json_terms: hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueArray(json_terms)))))
            
            case hydra.core.TermLiteral(value=lit):
                return hydra.lib.flows.pure(match_literal(lit))
            
            case hydra.core.TermMaybe(value=mt):
                return hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueNull())), cast(Callable[[hydra.core.Term], hydra.compute.Flow[T0, hydra.json.Value]], (lambda x1: untyped_term_to_json(x1))), mt)
            
            case hydra.core.TermRecord(value=r):
                fields = r.fields
                return hydra.lib.flows.bind(hydra.lib.flows.map_list(field_to_keyval, fields), (lambda keyvals: hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueObject(cast(FrozenDict[str, hydra.json.Value], hydra.lib.maps.from_list(hydra.lib.maybes.cat(keyvals))))))))
            
            case hydra.core.TermSet(value=vals):
                return untyped_term_to_json(cast(hydra.core.Term, hydra.core.TermList(hydra.lib.sets.to_list(vals))))
            
            case hydra.core.TermTypeLambda(value=ta):
                return as_record((hydra.core.Field(hydra.core.Name("parameter"), cast(hydra.core.Term, hydra.core.TermVariable(ta.parameter))), hydra.core.Field(hydra.core.Name("body"), ta.body)))
            
            case hydra.core.TermTypeApplication(value=tt):
                return as_record((hydra.core.Field(hydra.core.Name("term"), tt.body), hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type(tt.type))))
            
            case hydra.core.TermUnion(value=i):
                field = i.field
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(field.term, cast(hydra.core.Term, hydra.core.TermUnit())), (lambda : hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueString(field.name.value)))), (lambda : hydra.lib.flows.bind(field_to_keyval(field), (lambda mkeyval: hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueObject(cast(FrozenDict[str, hydra.json.Value], hydra.lib.maps.from_list(hydra.lib.maybes.maybe(cast(frozenlist[tuple[str, hydra.json.Value]], ()), (lambda keyval: (keyval,)), mkeyval))))))))))
            
            case hydra.core.TermVariable(value=v):
                return hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueString(v.value)))
            
            case hydra.core.TermWrap(value=wt):
                return untyped_term_to_json(wt.body)
            
            case _:
                return unexp(hydra.lib.strings.cat(("unsupported term variant: ", hydra.show.core.term(term))))
    return result()
