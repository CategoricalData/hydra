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
    match n:
        case hydra.json.ValueObject(value=m):
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
        
        case _:
            return hydra.monads.unexpected("object", show_value(n))

def encode_record[T0](coders: frozenlist[tuple[hydra.core.FieldType, hydra.compute.Coder[hydra.graph.Graph, T0, hydra.core.Term, hydra.json.Value]]], term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.json.Value]:
    stripped = hydra.rewriting.deannotate_term(term)
    return hydra.lib.flows.bind(hydra.extract.core.term_record(stripped), (lambda record: (fields := record.fields, encode_field := (lambda coder_and_field: (coder := (lambda : hydra.lib.pairs.first(coder_and_field)), field := (lambda : hydra.lib.pairs.second(coder_and_field)), ft := (lambda : hydra.lib.pairs.first(coder())), coder_ := (lambda : hydra.lib.pairs.second(coder())), fname := (lambda : field().name), fvalue := (lambda : field().term), for_maybe := (lambda ot: (dflt := (lambda : hydra.lib.flows.bind(coder_().encode(fvalue()), (lambda encoded: hydra.lib.flows.pure(cast(Maybe[tuple[str, T3]], Just(cast(tuple[str, T3], (fname().value, encoded)))))))), "inline match expressions are unsupported")[1]), dflt := (lambda : hydra.lib.flows.bind(coder_().encode(fvalue()), (lambda encoded: hydra.lib.flows.pure(cast(Maybe[tuple[str, T3]], Just(cast(tuple[str, T3], (fname().value, encoded)))))))), "inline match expressions are unsupported")[8]), hydra.lib.flows.bind(hydra.lib.flows.map_list(cast(Callable[[
      tuple[tuple[hydra.core.FieldType, hydra.compute.Coder[hydra.graph.Graph, T0, hydra.core.Term, hydra.json.Value]], hydra.core.Field]], hydra.compute.Flow[hydra.graph.Graph, Maybe[tuple[str, hydra.json.Value]]]], (lambda x1: encode_field(x1))), hydra.lib.lists.zip(coders, fields)), (lambda maybe_fields: hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueObject(cast(FrozenDict[str, hydra.json.Value], hydra.lib.maps.from_list(hydra.lib.maybes.cat(maybe_fields)))))))))[2]))

def literal_json_coder[T0, T1, T2](lt: hydra.core.LiteralType) -> hydra.compute.Flow[T0, hydra.compute.Coder[T1, T2, hydra.core.Literal, hydra.json.Value]]:
    return hydra.lib.flows.pure("inline match expressions are unsupported")

def read_string_stub(s: str) -> hydra.core.Term:
    r"""Placeholder for reading a string into a term (to be implemented)."""
    
    return cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.strings.cat2("TODO: read ", s)))))

def unit_coder[T0, T1]() -> hydra.compute.Coder[T0, T1, hydra.core.Term, hydra.json.Value]:
    return cast(hydra.compute.Coder[T0, T1, hydra.core.Term, hydra.json.Value], hydra.compute.Coder((lambda term: "inline match expressions are unsupported"), (lambda n: "inline match expressions are unsupported")))

def record_coder[T0, T1](rt: hydra.core.RowType) -> hydra.compute.Flow[T0, hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value]]:
    fields = rt.fields
    def get_coder(f: hydra.core.FieldType) -> hydra.compute.Flow[T0, tuple[hydra.core.FieldType, hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value]]]:
        return hydra.lib.flows.bind(term_coder(f.type), (lambda coder: hydra.lib.flows.pure(cast(tuple[hydra.core.FieldType, hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value]], (f, coder)))))
    return hydra.lib.flows.bind(hydra.lib.flows.map_list(get_coder, fields), (lambda coders: hydra.lib.flows.pure(cast(hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value], hydra.compute.Coder((lambda v1: encode_record(coders, v1)), (lambda v1: decode_record(rt, coders, v1)))))))

def term_coder[T0, T1](typ: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value]]:
    stripped = hydra.rewriting.deannotate_type(typ)
    match stripped:
        case hydra.core.TypeLiteral(value=at):
            return hydra.lib.flows.bind(literal_json_coder(at), (lambda ac: hydra.lib.flows.pure(cast(hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value], hydra.compute.Coder((lambda term: "inline match expressions are unsupported"), (lambda n: hydra.lib.flows.bind(ac.decode(n), (lambda lit: hydra.lib.flows.pure(cast(hydra.core.Term, hydra.core.TermLiteral(lit)))))))))))
        
        case hydra.core.TypeList(value=lt):
            return hydra.lib.flows.bind(term_coder(lt), (lambda lc: hydra.lib.flows.pure(cast(hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value], hydra.compute.Coder((lambda term: "inline match expressions are unsupported"), (lambda n: "inline match expressions are unsupported"))))))
        
        case hydra.core.TypeMap(value=mt):
            kt = mt.keys
            vt = mt.values
            return hydra.lib.flows.bind(term_coder(kt), (lambda kc: hydra.lib.flows.bind(term_coder(vt), (lambda vc: hydra.lib.flows.bind(cast(hydra.compute.Flow[T0, T0], hydra.monads.get_state()), (lambda cx: (is_string_key := (lambda : hydra.lib.equality.equal(hydra.rewriting.deannotate_type(kt), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))))), to_string := (lambda v: hydra.lib.logic.if_else(is_string_key(), (lambda : "inline match expressions are unsupported"), (lambda : hydra.show.core.term(v)))), from_string := (lambda s: hydra.lib.logic.if_else(is_string_key(), (lambda : cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s))))), (lambda : read_string_stub(s)))), encode_entry := (lambda kv: (k := (lambda : hydra.lib.pairs.first(kv)), v := (lambda : hydra.lib.pairs.second(kv)), hydra.lib.flows.bind(vc.encode(v()), (lambda encoded_v: hydra.lib.flows.pure(cast(tuple[str, hydra.json.Value], (to_string(k()), encoded_v))))))[2]), decode_entry := (lambda kv: (k := (lambda : hydra.lib.pairs.first(kv)), v := (lambda : hydra.lib.pairs.second(kv)), hydra.lib.flows.bind(vc.decode(v()), (lambda decoded_v: hydra.lib.flows.pure(cast(tuple[hydra.core.Term, hydra.core.Term], (from_string(k()), decoded_v))))))[2]), hydra.lib.flows.pure(cast(hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value], hydra.compute.Coder((lambda term: "inline match expressions are unsupported"), (lambda n: "inline match expressions are unsupported")))))[5]))))))
        
        case hydra.core.TypeMaybe(value=maybe_element_type):
            return hydra.lib.flows.bind(term_coder(maybe_element_type), (lambda maybe_element_coder: hydra.lib.flows.pure(cast(hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value], hydra.compute.Coder((lambda maybe_term: (stripped_maybe_term := hydra.rewriting.deannotate_term(maybe_term), "inline match expressions are unsupported")[1]), (lambda json_val: "inline match expressions are unsupported"))))))
        
        case hydra.core.TypeRecord(value=rt):
            return record_coder(rt)
        
        case hydra.core.TypeUnit():
            return hydra.lib.flows.pure(cast(hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value], unit_coder()))
        
        case hydra.core.TypeVariable(value=name):
            return hydra.lib.flows.pure(cast(hydra.compute.Coder[hydra.graph.Graph, T1, hydra.core.Term, hydra.json.Value], hydra.compute.Coder((lambda term: hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueString(hydra.lib.strings.cat(("variable '", name.value, "' for: ", hydra.show.core.term(term))))))), (lambda term: hydra.lib.flows.fail(hydra.lib.strings.cat(("type variable ", name.value, " does not support decoding")))))))
        
        case _:
            return hydra.lib.flows.fail(hydra.lib.strings.cat(("unsupported type in JSON: ", hydra.show.core.type(typ))))

def json_coder[T0](typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.compute.Coder[hydra.graph.Graph, T0, hydra.core.Term, hydra.json.Value]]:
    return hydra.lib.flows.bind(hydra.adapt.modules.language_adapter(hydra.ext.org.json.language.json_language(), typ), (lambda adapter: hydra.lib.flows.bind(term_coder(adapter.target), (lambda coder: hydra.lib.flows.pure(hydra.adapt.utils.compose_coders(adapter.coder, coder))))))

def untyped_term_to_json[T0](term: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.json.Value]:
    def unexp[T1](msg: str) -> hydra.compute.Flow[T1, hydra.json.Value]:
        return hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueString(hydra.lib.strings.cat2("FAIL: ", msg))))
    def as_record(fields: frozenlist[hydra.core.Field]) -> hydra.compute.Flow[T0, hydra.json.Value]:
        return untyped_term_to_json(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name(""), fields))))
    def as_variant(name: str, term2: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.json.Value]:
        return untyped_term_to_json(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name(""), hydra.core.Field(hydra.core.Name(name), term2)))))
    def field_to_keyval(f: hydra.core.Field) -> hydra.compute.Flow[T0, Maybe[tuple[str, hydra.json.Value]]]:
        def for_term(t: hydra.core.Term) -> hydra.compute.Flow[T0, Maybe[hydra.json.Value]]:
            match t:
                case hydra.core.TermMaybe(value=mt):
                    return hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[hydra.json.Value], Nothing())), for_term, mt)
                
                case _:
                    return hydra.lib.flows.map(cast(Callable[[hydra.json.Value], Maybe[hydra.json.Value]], (lambda x1: hydra.lib.maybes.pure(x1))), untyped_term_to_json(t))
        return hydra.lib.flows.bind(for_term(f.term), (lambda mjson: hydra.lib.flows.pure(hydra.lib.maybes.map((lambda j: cast(tuple[str, hydra.json.Value], (f.name.value, j))), mjson))))
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
            match f:
                case hydra.core.FunctionElimination(value=elm):
                    match elm:
                        case hydra.core.EliminationRecord(value=proj):
                            return as_variant("project", cast(hydra.core.Term, hydra.core.TermVariable(proj.field)))
                        
                        case _:
                            return unexp(hydra.lib.strings.cat(("unexpected elimination variant: ", hydra.show.core.elimination(elm))))
                
                case hydra.core.FunctionLambda(value=l):
                    return as_record((hydra.core.Field(hydra.core.Name("parameter"), cast(hydra.core.Term, hydra.core.TermVariable(l.parameter))), hydra.core.Field(hydra.core.Name("domain"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map(hydra.encode.core.type, l.domain)))), hydra.core.Field(hydra.core.Name("body"), l.body)))
                
                case hydra.core.FunctionPrimitive(value=name):
                    return hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueString(name.value)))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        
        case hydra.core.TermLet(value=lt):
            bindings = lt.bindings
            env = lt.body
            def from_binding(b: hydra.core.Binding) -> hydra.core.Field:
                return hydra.core.Field(b.name, b.term)
            return as_record((hydra.core.Field(hydra.core.Name("bindings"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name(""), hydra.lib.lists.map(from_binding, bindings))))), hydra.core.Field(hydra.core.Name("environment"), env)))
        
        case hydra.core.TermList(value=terms):
            return hydra.lib.flows.bind(hydra.lib.flows.map_list(cast(Callable[[hydra.core.Term], hydra.compute.Flow[T0, hydra.json.Value]], (lambda x1: untyped_term_to_json(x1))), terms), (lambda json_terms: hydra.lib.flows.pure(cast(hydra.json.Value, hydra.json.ValueArray(json_terms)))))
        
        case hydra.core.TermLiteral(value=lit):
            return hydra.lib.flows.pure("inline match expressions are unsupported")
        
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
