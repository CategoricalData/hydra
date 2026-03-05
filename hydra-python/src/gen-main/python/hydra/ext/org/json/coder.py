# Note: this is an automatically generated file. Do not edit.

r"""JSON encoding and decoding for Hydra terms."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.adapt.modules
import hydra.adapt.utils
import hydra.compute
import hydra.context
import hydra.core
import hydra.encode.core
import hydra.error
import hydra.ext.org.json.language
import hydra.extract.core
import hydra.json.model
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.literals
import hydra.rewriting
import hydra.show.core

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def show_value(value: T0) -> str:
    r"""Show a JSON value as a string (placeholder implementation)."""
    
    return "TODO: implement showValue"

def decode_record(rt: hydra.core.RowType, coders: frozenlist[tuple[hydra.core.FieldType, hydra.compute.Coder[hydra.core.Term, hydra.json.model.Value]]], cx: hydra.context.Context, n: hydra.json.model.Value) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
    r"""Decode a JSON value to a record term."""
    
    def decode_object_body(m: FrozenDict[str, hydra.json.model.Value]) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        def decode_field(coder: tuple[hydra.core.FieldType, hydra.compute.Coder[hydra.core.Term, hydra.json.model.Value]]) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Field]:
            @lru_cache(1)
            def ft() -> hydra.core.FieldType:
                return hydra.lib.pairs.first(coder)
            @lru_cache(1)
            def coder_() -> hydra.compute.Coder[hydra.core.Term, hydra.json.model.Value]:
                return hydra.lib.pairs.second(coder)
            fname = ft().name
            default_value = cast(hydra.json.model.Value, hydra.json.model.ValueNull())
            @lru_cache(1)
            def json_value() -> hydra.json.model.Value:
                return hydra.lib.maybes.from_maybe(default_value, hydra.lib.maps.lookup(fname.value, m))
            return hydra.lib.eithers.bind(coder_().decode(cx, json_value()), (lambda v: Right(hydra.core.Field(fname, v))))
        return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: decode_field(x1)), coders), (lambda fields: Right(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(rt.type_name, fields))))))
    match n:
        case hydra.json.model.ValueObject(value=v1):
            return decode_object_body(v1)
        
        case _:
            return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("expected object, found: ", show_value(n)))), cx))

def encode_record(coders: frozenlist[tuple[hydra.core.FieldType, hydra.compute.Coder[hydra.core.Term, hydra.json.model.Value]]], cx: hydra.context.Context, graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.json.model.Value]:
    r"""Encode a record term to JSON."""
    
    @lru_cache(1)
    def stripped() -> hydra.core.Term:
        return hydra.rewriting.deannotate_term(term)
    def match_maybe_term(fvalue: hydra.core.Term, coder_: hydra.compute.Coder[hydra.core.Term, T0], fname: hydra.core.Name, dflt: Either[hydra.context.InContext[hydra.error.OtherError], Maybe[tuple[str, T0]]]) -> Either[hydra.context.InContext[hydra.error.OtherError], Maybe[tuple[str, T0]]]:
        match fvalue:
            case hydra.core.TermMaybe(value=opt):
                return hydra.lib.maybes.maybe(Right(Nothing()), (lambda v: hydra.lib.eithers.bind(coder_.encode(cx, v), (lambda encoded: Right(Just((fname.value, encoded)))))), opt)
            
            case _:
                return dflt
    def match_type_for_maybe(ft: hydra.core.FieldType, for_maybe: Callable[[hydra.core.Type], T0], dflt: T0) -> T0:
        match ft.type:
            case hydra.core.TypeMaybe(value=ot):
                return for_maybe(ot)
            
            case _:
                return dflt
    def encode_field(coder_and_field: tuple[tuple[hydra.core.FieldType, hydra.compute.Coder[hydra.core.Term, T0]], hydra.core.Field]) -> Either[hydra.context.InContext[hydra.error.OtherError], Maybe[tuple[str, T0]]]:
        @lru_cache(1)
        def coder() -> tuple[hydra.core.FieldType, hydra.compute.Coder[hydra.core.Term, T0]]:
            return hydra.lib.pairs.first(coder_and_field)
        @lru_cache(1)
        def field() -> hydra.core.Field:
            return hydra.lib.pairs.second(coder_and_field)
        @lru_cache(1)
        def ft() -> hydra.core.FieldType:
            return hydra.lib.pairs.first(coder())
        @lru_cache(1)
        def coder_() -> hydra.compute.Coder[hydra.core.Term, T0]:
            return hydra.lib.pairs.second(coder())
        fname = field().name
        fvalue = field().term
        def for_maybe(ot: T1) -> Either[hydra.context.InContext[hydra.error.OtherError], Maybe[tuple[str, T0]]]:
            @lru_cache(1)
            def dflt() -> Either[hydra.context.InContext[hydra.error.OtherError], Maybe[tuple[str, T0]]]:
                return hydra.lib.eithers.bind(coder_().encode(cx, fvalue), (lambda encoded: Right(Just((fname.value, encoded)))))
            return match_maybe_term(fvalue, coder_(), fname, dflt())
        @lru_cache(1)
        def dflt() -> Either[hydra.context.InContext[hydra.error.OtherError], Maybe[tuple[str, T0]]]:
            return hydra.lib.eithers.bind(coder_().encode(cx, fvalue), (lambda encoded: Right(Just((fname.value, encoded)))))
        return match_type_for_maybe(ft(), (lambda x1: for_maybe(x1)), dflt())
    return hydra.lib.eithers.bind(hydra.extract.core.term_record(cx, graph, stripped()), (lambda record: (fields := record.fields, hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: encode_field(x1)), hydra.lib.lists.zip(coders, fields)), (lambda maybe_fields: Right(cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.maybes.cat(maybe_fields))))))))[1]))

def literal_json_coder(lt: hydra.core.LiteralType) -> Either[T0, hydra.compute.Coder[hydra.core.Literal, hydra.json.model.Value]]:
    r"""Create a JSON coder for literal types."""
    
    def decode_bool(cx: hydra.context.Context, s: hydra.json.model.Value) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Literal]:
        match s:
            case hydra.json.model.ValueBoolean(value=b):
                return Right(cast(hydra.core.Literal, hydra.core.LiteralBoolean(b)))
            
            case _:
                return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("expected boolean, found: ", show_value(s)))), cx))
    def decode_float(cx: hydra.context.Context, s: hydra.json.model.Value) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Literal]:
        match s:
            case hydra.json.model.ValueNumber(value=f):
                return Right(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueBigfloat(f)))))
            
            case _:
                return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("expected number, found: ", show_value(s)))), cx))
    def decode_integer(cx: hydra.context.Context, s: hydra.json.model.Value) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Literal]:
        match s:
            case hydra.json.model.ValueNumber(value=f):
                @lru_cache(1)
                def bi() -> int:
                    return hydra.lib.literals.bigfloat_to_bigint(f)
                return Right(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueBigint(bi())))))
            
            case _:
                return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("expected number, found: ", show_value(s)))), cx))
    def decode_string(cx: hydra.context.Context, s: hydra.json.model.Value) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Literal]:
        match s:
            case hydra.json.model.ValueString(value=s_):
                return Right(cast(hydra.core.Literal, hydra.core.LiteralString(s_)))
            
            case _:
                return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("expected string, found: ", show_value(s)))), cx))
    @lru_cache(1)
    def encoded() -> hydra.compute.Coder[hydra.core.Literal, hydra.json.model.Value]:
        match lt:
            case hydra.core.LiteralTypeBoolean():
                return hydra.compute.Coder((lambda cx, lit: hydra.lib.eithers.bind(hydra.extract.core.boolean_literal(cx, lit), (lambda b: Right(cast(hydra.json.model.Value, hydra.json.model.ValueBoolean(b)))))), (lambda x1, x2: decode_bool(x1, x2)))
            
            case hydra.core.LiteralTypeFloat():
                return hydra.compute.Coder((lambda cx, lit: hydra.lib.eithers.bind(hydra.extract.core.float_literal(cx, lit), (lambda f: hydra.lib.eithers.bind(hydra.extract.core.bigfloat_value(cx, f), (lambda bf: Right(cast(hydra.json.model.Value, hydra.json.model.ValueNumber(bf)))))))), (lambda x1, x2: decode_float(x1, x2)))
            
            case hydra.core.LiteralTypeInteger():
                return hydra.compute.Coder((lambda cx, lit: hydra.lib.eithers.bind(hydra.extract.core.integer_literal(cx, lit), (lambda i: hydra.lib.eithers.bind(hydra.extract.core.bigint_value(cx, i), (lambda bi: Right(cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.lib.literals.bigint_to_bigfloat(bi))))))))), (lambda x1, x2: decode_integer(x1, x2)))
            
            case hydra.core.LiteralTypeString():
                return hydra.compute.Coder((lambda cx, lit: hydra.lib.eithers.bind(hydra.extract.core.string_literal(cx, lit), (lambda s: Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(s)))))), (lambda x1, x2: decode_string(x1, x2)))
            
            case _:
                raise TypeError("Unsupported LiteralType")
    return Right(encoded())

def read_string_stub(s: str) -> hydra.core.Term:
    r"""Placeholder for reading a string into a term (to be implemented)."""
    
    return cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.strings.cat2("TODO: read ", s)))))

@lru_cache(1)
def unit_coder() -> hydra.compute.Coder[hydra.core.Term, hydra.json.model.Value]:
    r"""JSON coder for unit values."""
    
    def encode_unit(cx: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.json.model.Value]:
        match hydra.rewriting.deannotate_term(term):
            case hydra.core.TermUnit():
                return Right(cast(hydra.json.model.Value, hydra.json.model.ValueNull()))
            
            case _:
                return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("expected unit, found: ", hydra.show.core.term(term)))), cx))
    def decode_unit(cx: hydra.context.Context, n: hydra.json.model.Value) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        match n:
            case hydra.json.model.ValueNull():
                return Right(cast(hydra.core.Term, hydra.core.TermUnit()))
            
            case _:
                return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("expected null, found: ", show_value(n)))), cx))
    return hydra.compute.Coder((lambda x1, x2: encode_unit(x1, x2)), (lambda x1, x2: decode_unit(x1, x2)))

def record_coder(rt: hydra.core.RowType, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.compute.Coder[hydra.core.Term, hydra.json.model.Value]]:
    r"""Create a JSON coder for record types."""
    
    fields = rt.fields
    def get_coder(f: hydra.core.FieldType) -> Either[hydra.context.InContext[hydra.error.OtherError], tuple[hydra.core.FieldType, hydra.compute.Coder[hydra.core.Term, hydra.json.model.Value]]]:
        return hydra.lib.eithers.bind(term_coder(f.type, cx, g), (lambda coder: Right((f, coder))))
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: get_coder(x1)), fields), (lambda coders: Right(hydra.compute.Coder((lambda cx2, term: encode_record(coders, cx2, g, term)), (lambda cx2, val: decode_record(rt, coders, cx2, val))))))

def term_coder(typ: hydra.core.Type, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.compute.Coder[hydra.core.Term, hydra.json.model.Value]]:
    r"""Create a JSON coder for term types."""
    
    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.rewriting.deannotate_type(typ)
    def encode_literal(ac: hydra.compute.Coder[hydra.core.Literal, T0], cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], T0]:
        match term:
            case hydra.core.TermLiteral(value=av):
                return ac.encode(cx2, av)
            
            case _:
                return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("expected literal term, found: ", hydra.show.core.term(term)))), cx2))
    def encode_list(lc: hydra.compute.Coder[hydra.core.Term, hydra.json.model.Value], cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.json.model.Value]:
        match term:
            case hydra.core.TermList(value=els):
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda el: lc.encode(cx2, el)), els), (lambda encoded_els: Right(cast(hydra.json.model.Value, hydra.json.model.ValueArray(encoded_els)))))
            
            case _:
                return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("expected list term, found: ", hydra.show.core.term(term)))), cx2))
    def decode_list(lc: hydra.compute.Coder[hydra.core.Term, hydra.json.model.Value], cx2: hydra.context.Context, n: hydra.json.model.Value) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        match n:
            case hydra.json.model.ValueArray(value=nodes):
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda node: lc.decode(cx2, node)), nodes), (lambda decoded_nodes: Right(cast(hydra.core.Term, hydra.core.TermList(decoded_nodes)))))
            
            case _:
                return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("expected sequence, found: ", show_value(n)))), cx2))
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
    def encode_maybe(maybe_element_coder: hydra.compute.Coder[hydra.core.Term, hydra.json.model.Value], cx2: hydra.context.Context, maybe_term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.json.model.Value]:
        @lru_cache(1)
        def stripped_maybe_term() -> hydra.core.Term:
            return hydra.rewriting.deannotate_term(maybe_term)
        match stripped_maybe_term():
            case hydra.core.TermMaybe(value=maybe_contents):
                return hydra.lib.logic.if_else(hydra.lib.maybes.is_nothing(maybe_contents), (lambda : Right(cast(hydra.json.model.Value, hydra.json.model.ValueNull()))), (lambda : hydra.lib.eithers.bind(maybe_element_coder.encode(cx2, hydra.lib.maybes.from_just(maybe_contents)), (lambda encoded_inner: Right(encoded_inner)))))
            
            case _:
                return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("expected optional term, found: ", hydra.show.core.term(maybe_term)))), cx2))
    def decode_maybe(maybe_element_coder: hydra.compute.Coder[hydra.core.Term, hydra.json.model.Value], cx2: hydra.context.Context, json_val: hydra.json.model.Value) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.core.Term]:
        match json_val:
            case hydra.json.model.ValueNull():
                return Right(cast(hydra.core.Term, hydra.core.TermMaybe(Nothing())))
            
            case _:
                return hydra.lib.eithers.bind(maybe_element_coder.decode(cx2, json_val), (lambda decoded_inner: Right(cast(hydra.core.Term, hydra.core.TermMaybe(Just(decoded_inner))))))
    @lru_cache(1)
    def result():
        match stripped():
            case hydra.core.TypeLiteral(value=at):
                return hydra.lib.eithers.bind(literal_json_coder(at), (lambda ac: Right(hydra.compute.Coder((lambda v1, v2: encode_literal(ac, v1, v2)), (lambda cx2, n: hydra.lib.eithers.bind(ac.decode(cx2, n), (lambda lit: Right(cast(hydra.core.Term, hydra.core.TermLiteral(lit))))))))))
            
            case hydra.core.TypeList(value=lt):
                return hydra.lib.eithers.bind(term_coder(lt, cx, g), (lambda lc: Right(hydra.compute.Coder((lambda v1, v2: encode_list(lc, v1, v2)), (lambda v1, v2: decode_list(lc, v1, v2))))))
            
            case hydra.core.TypeMap(value=mt):
                kt = mt.keys
                vt = mt.values
                return hydra.lib.eithers.bind(term_coder(kt, cx, g), (lambda kc: hydra.lib.eithers.bind(term_coder(vt, cx, g), (lambda vc: (is_string_key := hydra.lib.equality.equal(hydra.rewriting.deannotate_type(kt), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))), to_string := (lambda v: hydra.lib.logic.if_else(is_string_key, (lambda : match_term_literal(v)), (lambda : hydra.show.core.term(v)))), from_string := (lambda s: hydra.lib.logic.if_else(is_string_key, (lambda : cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s))))), (lambda : read_string_stub(s)))), encode_entry := (lambda cx2, kv: (k := hydra.lib.pairs.first(kv), v := hydra.lib.pairs.second(kv), hydra.lib.eithers.bind(vc.encode(cx2, v), (lambda encoded_v: Right((to_string(k), encoded_v)))))[2]), decode_entry := (lambda cx2, kv: (k := hydra.lib.pairs.first(kv), v := hydra.lib.pairs.second(kv), hydra.lib.eithers.bind(vc.decode(cx2, v), (lambda decoded_v: Right((from_string(k), decoded_v)))))[2]), _hoist_body_1 := (lambda cx2, term, v1: (lambda m: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda entry: encode_entry(cx2, entry)), hydra.lib.maps.to_list(m)), (lambda entries: Right(cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(entries)))))))(v1.value) if isinstance(v1, hydra.core.TermMap) else Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("expected map term, found: ", hydra.show.core.term(term)))), cx2))), _hoist_body_2 := (lambda cx2, n, v1: (lambda m: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda entry: decode_entry(cx2, entry)), hydra.lib.maps.to_list(m)), (lambda entries: Right(cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(entries)))))))(v1.value) if isinstance(v1, hydra.json.model.ValueObject) else Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("expected mapping, found: ", show_value(n)))), cx2))), Right(hydra.compute.Coder((lambda cx2, term: _hoist_body_1(cx2, term, term)), (lambda cx2, n: _hoist_body_2(cx2, n, n)))))[7]))))
            
            case hydra.core.TypeMaybe(value=maybe_element_type):
                return hydra.lib.eithers.bind(term_coder(maybe_element_type, cx, g), (lambda maybe_element_coder: Right(hydra.compute.Coder((lambda v1, v2: encode_maybe(maybe_element_coder, v1, v2)), (lambda v1, v2: decode_maybe(maybe_element_coder, v1, v2))))))
            
            case hydra.core.TypeRecord(value=rt):
                return record_coder(rt, cx, g)
            
            case hydra.core.TypeUnit():
                return Right(unit_coder())
            
            case hydra.core.TypeVariable(value=name):
                return Right(hydra.compute.Coder((lambda _cx, term: Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.lib.strings.cat(("variable '", name.value, "' for: ", hydra.show.core.term(term))))))), (lambda cx2, _term: Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("type variable ", name.value, " does not support decoding"))), cx2)))))
            
            case _:
                return Left(hydra.context.InContext(hydra.error.OtherError(hydra.lib.strings.cat(("unsupported type in JSON: ", hydra.show.core.type(typ)))), cx))
    return result()

def json_coder(typ: hydra.core.Type, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.compute.Coder[hydra.core.Term, hydra.json.model.Value]]:
    r"""Create a JSON coder for a given type."""
    
    def mk_term_coder(t: hydra.core.Type) -> Either[hydra.context.InContext[hydra.error.OtherError], hydra.compute.Coder[hydra.core.Term, hydra.json.model.Value]]:
        return term_coder(t, cx, g)
    return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _s: hydra.context.InContext(hydra.error.OtherError(_s), cx)), (lambda _x: _x), hydra.adapt.modules.language_adapter(hydra.ext.org.json.language.json_language(), cx, g, typ)), (lambda adapter: hydra.lib.eithers.bind(mk_term_coder(adapter.target), (lambda coder: Right(hydra.adapt.utils.compose_coders(adapter.coder, coder))))))

def untyped_term_to_json(term: hydra.core.Term) -> Either[T0, hydra.json.model.Value]:
    r"""A simplistic, unidirectional encoding for terms as JSON values. Not type-aware; best used for human consumption."""
    
    def recurse(t: hydra.core.Term) -> Either[T0, hydra.json.model.Value]:
        return untyped_term_to_json(t)
    def unexp(msg: str) -> Either[T1, hydra.json.model.Value]:
        return Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.lib.strings.cat2("FAIL: ", msg))))
    def as_record(fields: frozenlist[hydra.core.Field]) -> Either[T0, hydra.json.model.Value]:
        return recurse(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name(""), fields))))
    def as_variant(name: str, term2: hydra.core.Term) -> Either[T0, hydra.json.model.Value]:
        return recurse(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name(""), hydra.core.Field(hydra.core.Name(name), term2)))))
    def match_term_maybe(for_term: Callable[[hydra.core.Term], Either[T0, Maybe[hydra.json.model.Value]]], t: hydra.core.Term) -> Either[T0, Maybe[hydra.json.model.Value]]:
        match t:
            case hydra.core.TermMaybe(value=mt):
                return hydra.lib.maybes.maybe(Right(Nothing()), for_term, mt)
            
            case _:
                return hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), recurse(t))
    def match_elimination(unexp2: Callable[[str], T1], as_variant2: Callable[[str, hydra.core.Term], T1], elm: hydra.core.Elimination) -> T1:
        match elm:
            case hydra.core.EliminationRecord(value=proj):
                return as_variant2("project", cast(hydra.core.Term, hydra.core.TermVariable(proj.field)))
            
            case _:
                return unexp2(hydra.lib.strings.cat(("unexpected elimination variant: ", hydra.show.core.elimination(elm))))
    def match_function(unexp2: Callable[[str], Either[T1, hydra.json.model.Value]], as_record2: Callable[[frozenlist[hydra.core.Field]], Either[T1, hydra.json.model.Value]], as_variant2: Callable[[str, hydra.core.Term], Either[T1, hydra.json.model.Value]], f: hydra.core.Function) -> Either[T1, hydra.json.model.Value]:
        match f:
            case hydra.core.FunctionElimination(value=elm):
                return match_elimination(unexp2, as_variant2, elm)
            
            case hydra.core.FunctionLambda(value=l):
                return as_record2((hydra.core.Field(hydra.core.Name("parameter"), cast(hydra.core.Term, hydra.core.TermVariable(l.parameter))), hydra.core.Field(hydra.core.Name("domain"), cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.maybes.map((lambda x1: hydra.encode.core.type(x1)), l.domain)))), hydra.core.Field(hydra.core.Name("body"), l.body)))
            
            case hydra.core.FunctionPrimitive(value=name):
                return Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(name.value)))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def match_literal(lit: hydra.core.Literal) -> hydra.json.model.Value:
        match lit:
            case hydra.core.LiteralBinary(value=b):
                return cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.lib.literals.binary_to_string(b)))
            
            case hydra.core.LiteralBoolean(value=b2):
                return cast(hydra.json.model.Value, hydra.json.model.ValueBoolean(b2))
            
            case hydra.core.LiteralFloat(value=f):
                return cast(hydra.json.model.Value, hydra.json.model.ValueNumber(hydra.literals.float_value_to_bigfloat(f)))
            
            case hydra.core.LiteralInteger(value=i):
                @lru_cache(1)
                def bf() -> int:
                    return hydra.literals.integer_value_to_bigint(i)
                @lru_cache(1)
                def f() -> Decimal:
                    return hydra.lib.literals.bigint_to_bigfloat(bf())
                return cast(hydra.json.model.Value, hydra.json.model.ValueNumber(f()))
            
            case hydra.core.LiteralString(value=s):
                return cast(hydra.json.model.Value, hydra.json.model.ValueString(s))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def field_to_keyval(f: hydra.core.Field) -> Either[T0, Maybe[tuple[str, hydra.json.model.Value]]]:
        def for_term(t: hydra.core.Term) -> Either[T0, Maybe[hydra.json.model.Value]]:
            return match_term_maybe((lambda x1: for_term(x1)), t)
        return hydra.lib.eithers.bind(for_term(f.term), (lambda mjson: Right(hydra.lib.maybes.map((lambda j: (f.name.value, j)), mjson))))
    @lru_cache(1)
    def result() -> Either[T0, hydra.json.model.Value]:
        match term:
            case hydra.core.TermAnnotated(value=at):
                term1 = at.body
                ann = at.annotation
                def encode_pair(kv: tuple[hydra.core.Name, hydra.core.Term]) -> Either[T0, tuple[str, hydra.json.model.Value]]:
                    @lru_cache(1)
                    def k() -> str:
                        return hydra.lib.pairs.first(kv).value
                    @lru_cache(1)
                    def v() -> hydra.core.Term:
                        return hydra.lib.pairs.second(kv)
                    return hydra.lib.eithers.bind(recurse(v()), (lambda json: Right((k(), json))))
                return hydra.lib.eithers.bind(recurse(term1), (lambda json: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: encode_pair(x1)), hydra.lib.maps.to_list(ann)), (lambda pairs: Right(cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list((("term", json), ("annotations", cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(pairs)))))))))))))
            
            case hydra.core.TermApplication(value=app):
                return as_record((hydra.core.Field(hydra.core.Name("function"), app.function), hydra.core.Field(hydra.core.Name("argument"), app.argument)))
            
            case hydra.core.TermFunction(value=f):
                return match_function((lambda x1: unexp(x1)), (lambda x1: as_record(x1)), (lambda x1, x2: as_variant(x1, x2)), f)
            
            case hydra.core.TermLet(value=lt):
                bindings = lt.bindings
                env = lt.body
                def from_binding(b: hydra.core.Binding) -> hydra.core.Field:
                    return hydra.core.Field(b.name, b.term)
                return as_record((hydra.core.Field(hydra.core.Name("bindings"), cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name(""), hydra.lib.lists.map((lambda x1: from_binding(x1)), bindings))))), hydra.core.Field(hydra.core.Name("environment"), env)))
            
            case hydra.core.TermList(value=terms):
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: recurse(x1)), terms), (lambda json_terms: Right(cast(hydra.json.model.Value, hydra.json.model.ValueArray(json_terms)))))
            
            case hydra.core.TermLiteral(value=lit):
                return Right(match_literal(lit))
            
            case hydra.core.TermMaybe(value=mt):
                return hydra.lib.maybes.maybe(Right(cast(hydra.json.model.Value, hydra.json.model.ValueNull())), (lambda x1: recurse(x1)), mt)
            
            case hydra.core.TermRecord(value=r):
                fields = r.fields
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: field_to_keyval(x1)), fields), (lambda keyvals: Right(cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.maybes.cat(keyvals)))))))
            
            case hydra.core.TermSet(value=vals):
                return recurse(cast(hydra.core.Term, hydra.core.TermList(hydra.lib.sets.to_list(vals))))
            
            case hydra.core.TermTypeLambda(value=ta):
                return as_record((hydra.core.Field(hydra.core.Name("parameter"), cast(hydra.core.Term, hydra.core.TermVariable(ta.parameter))), hydra.core.Field(hydra.core.Name("body"), ta.body)))
            
            case hydra.core.TermTypeApplication(value=tt):
                return as_record((hydra.core.Field(hydra.core.Name("term"), tt.body), hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type(tt.type))))
            
            case hydra.core.TermUnion(value=i):
                field = i.field
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(field.term, cast(hydra.core.Term, hydra.core.TermUnit())), (lambda : Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(field.name.value)))), (lambda : hydra.lib.eithers.bind(field_to_keyval(field), (lambda mkeyval: Right(cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.maybes.maybe((), (lambda keyval: (keyval,)), mkeyval)))))))))
            
            case hydra.core.TermVariable(value=v):
                return Right(cast(hydra.json.model.Value, hydra.json.model.ValueString(v.value)))
            
            case hydra.core.TermWrap(value=wt):
                return recurse(wt.body)
            
            case _:
                return unexp(hydra.lib.strings.cat(("unsupported term variant: ", hydra.show.core.term(term))))
    return result()
