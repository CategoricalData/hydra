# Note: this is an automatically generated file. Do not edit.

r"""Utilities for reading and writing type and term annotations."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.classes
import hydra.compute
import hydra.constants
import hydra.core
import hydra.decode.core
import hydra.encode.core
import hydra.extract.core
import hydra.graph
import hydra.lexical
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.monads
import hydra.rewriting
import hydra.show.core
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def aggregate_annotations(get_value: Callable[[T0], Maybe[T1]], get_x: Callable[[T1], T0], get_anns: Callable[[T1], FrozenDict[T2, T3]], t: T0) -> FrozenDict[T2, T3]:
    def to_pairs(rest: frozenlist[frozenlist[tuple[T2, T3]]], t2: T0) -> frozenlist[frozenlist[tuple[T2, T3]]]:
        return hydra.lib.maybes.maybe(rest, (lambda yy: to_pairs(hydra.lib.lists.cons(hydra.lib.maps.to_list(get_anns(yy)), rest), get_x(yy))), get_value(t2))
    return cast(FrozenDict[T2, T3], hydra.lib.maps.from_list(hydra.lib.lists.concat(to_pairs(cast(frozenlist[frozenlist[tuple[T2, T3]]], ()), t))))

def get_attr(key: hydra.core.Name) -> hydra.compute.Flow[T0, Maybe[hydra.core.Term]]:
    return cast(hydra.compute.Flow[T0, Maybe[hydra.core.Term]], hydra.compute.Flow((lambda s0, t0: cast(hydra.compute.FlowState[T0, Maybe[hydra.core.Term]], hydra.compute.FlowState(cast(Maybe[Maybe[hydra.core.Term]], Just(hydra.lib.maps.lookup(key, t0.other))), s0, t0)))))

def get_debug_id() -> hydra.compute.Flow[T0, Maybe[str]]:
    return hydra.lexical.with_empty_graph(hydra.lib.flows.bind(get_attr(hydra.constants.key_debug_id), (lambda desc: hydra.lib.flows.map_maybe(hydra.extract.core.string, desc))))

def debug_if(debug_id: T0, message: str) -> hydra.compute.Flow[T1, None]:
    def check_and_fail(desc: Maybe[str]) -> hydra.compute.Flow[T2, None]:
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(desc, cast(Maybe[str], Just("debugId"))), (lambda : hydra.lib.flows.fail(message)), (lambda : hydra.lib.flows.pure(None)))
    return hydra.lib.flows.bind(cast(hydra.compute.Flow[T1, Maybe[str]], get_debug_id()), cast(Callable[[Maybe[str]], hydra.compute.Flow[T1, None]], (lambda x1: check_and_fail(x1))))

def get_attr_with_default(key: hydra.core.Name, def_: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
    return hydra.lib.flows.map((lambda mval: hydra.lib.maybes.from_maybe(def_, mval)), get_attr(key))

def has_flag(flag: hydra.core.Name) -> hydra.compute.Flow[T0, bool]:
    return hydra.lexical.with_empty_graph(hydra.lib.flows.bind(get_attr_with_default(flag, cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(False))))), (lambda term: hydra.extract.core.boolean(term))))

def fail_on_flag(flag: hydra.core.Name, msg: str) -> hydra.compute.Flow[T0, None]:
    return hydra.lib.flows.bind(has_flag(flag), (lambda val: hydra.lib.logic.if_else(val, (lambda : hydra.lib.flows.fail(msg)), (lambda : hydra.lib.flows.pure(None)))))

def get_count(key: hydra.core.Name) -> hydra.compute.Flow[T0, int]:
    return hydra.lexical.with_empty_graph(hydra.lib.flows.bind(get_attr_with_default(key, cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(0))))))), hydra.extract.core.int32))

def get_description(anns: FrozenDict[hydra.core.Name, hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[str]]:
    r"""Get description from annotations map."""
    
    return hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[str], Nothing())), (lambda term: hydra.lib.flows.map(cast(Callable[[str], Maybe[str]], (lambda x1: hydra.lib.maybes.pure(x1))), hydra.extract.core.string(term))), hydra.lib.maps.lookup(hydra.core.Name("description"), anns))

def term_annotation_internal(term: hydra.core.Term) -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    r"""Get internal term annotations."""
    
    def get_ann(t: hydra.core.Term) -> Maybe[hydra.core.AnnotatedTerm]:
        match t:
            case hydra.core.TermAnnotated(value=a):
                return cast(Maybe[hydra.core.AnnotatedTerm], Just(a))
            
            case _:
                return cast(Maybe[hydra.core.AnnotatedTerm], Nothing())
    return aggregate_annotations(get_ann, (lambda at: at.body), (lambda at: at.annotation), term)

def get_term_annotation(key: hydra.core.Name, term: hydra.core.Term) -> Maybe[hydra.core.Term]:
    r"""Get a term annotation."""
    
    return hydra.lib.maps.lookup(key, term_annotation_internal(term))

def get_term_description(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[str]]:
    r"""Get term description."""
    
    return get_description(term_annotation_internal(term))

def get_type(anns: FrozenDict[hydra.core.Name, hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Type]]:
    r"""Get type from annotations."""
    
    return hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.graph.Graph, hydra.graph.Graph], hydra.monads.get_state()), (lambda cx: hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[hydra.core.Type], Nothing())), (lambda dat: hydra.lib.flows.map(cast(Callable[[hydra.core.Type], Maybe[hydra.core.Type]], (lambda x1: hydra.lib.maybes.pure(x1))), hydra.monads.with_trace("get type", hydra.monads.either_to_flow((lambda v1: v1.value), hydra.decode.core.type(cx, dat))))), hydra.lib.maps.lookup(hydra.constants.key_type, anns))))

def type_annotation_internal(typ: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    r"""Get internal type annotations."""
    
    def get_ann(t: hydra.core.Type) -> Maybe[hydra.core.AnnotatedType]:
        match t:
            case hydra.core.TypeAnnotated(value=a):
                return cast(Maybe[hydra.core.AnnotatedType], Just(a))
            
            case _:
                return cast(Maybe[hydra.core.AnnotatedType], Nothing())
    return aggregate_annotations(get_ann, (lambda at: at.body), (lambda at: at.annotation), typ)

def get_type_annotation(key: hydra.core.Name, typ: hydra.core.Type) -> Maybe[hydra.core.Term]:
    r"""Get a type annotation."""
    
    return hydra.lib.maps.lookup(key, type_annotation_internal(typ))

def get_type_classes(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[hydra.core.Name, frozenset[hydra.classes.TypeClass]]]:
    r"""Get type classes from term."""
    
    return hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.graph.Graph, hydra.graph.Graph], hydra.monads.get_state()), (lambda cx: (decode_class := (lambda term2: (by_name := (lambda : cast(FrozenDict[hydra.core.Name, hydra.classes.TypeClass], hydra.lib.maps.from_list((cast(tuple[hydra.core.Name, hydra.classes.TypeClass], (hydra.core.Name("equality"), hydra.classes.TypeClass.EQUALITY)), cast(tuple[hydra.core.Name, hydra.classes.TypeClass], (hydra.core.Name("ordering"), hydra.classes.TypeClass.ORDERING)))))), hydra.lib.flows.bind(hydra.extract.core.unit_variant(hydra.core.Name("hydra.classes.TypeClass"), term2), (lambda fn: hydra.lib.maybes.maybe(hydra.monads.unexpected("type class", hydra.show.core.term(term2)), cast(Callable[[hydra.classes.TypeClass], hydra.compute.Flow[hydra.graph.Graph, hydra.classes.TypeClass]], (lambda x1: hydra.lib.flows.pure(x1))), hydra.lib.maps.lookup(fn, by_name())))))[1]), hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(FrozenDict[hydra.core.Name, frozenset[hydra.classes.TypeClass]], hydra.lib.maps.empty())), (lambda term2: hydra.extract.core.map((lambda t: hydra.monads.either_to_flow((lambda v1: v1.value), hydra.decode.core.name(cx, t))), (lambda v1: hydra.extract.core.set_of(decode_class, v1)), term2)), get_term_annotation(hydra.constants.key_classes, term)))[1]))

def get_type_description(typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[str]]:
    r"""Get type description."""
    
    return get_description(type_annotation_internal(typ))

def has_description(anns: FrozenDict[hydra.core.Name, T0]) -> bool:
    return hydra.lib.maybes.is_just(hydra.lib.maps.lookup(hydra.constants.key_description, anns))

def has_type_description(typ: hydra.core.Type) -> bool:
    r"""Check if type has description."""
    
    return has_description(type_annotation_internal(typ))

def is_native_type(el: hydra.core.Binding) -> bool:
    r"""For a typed term, decide whether a coder should encode it as a native type expression, or as a Hydra type expression."""
    
    def is_flagged_as_first_class_type() -> bool:
        return hydra.lib.maybes.from_maybe(False, hydra.lib.maybes.map((lambda _: True), get_term_annotation(hydra.constants.key_first_class_type, el.term)))
    return hydra.lib.maybes.maybe(False, (lambda ts: hydra.lib.logic.and_(hydra.lib.equality.equal(ts, hydra.core.TypeScheme(cast(frozenlist[hydra.core.Name], ()), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))), cast(Maybe[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]], Nothing()))), hydra.lib.logic.not_(is_flagged_as_first_class_type()))), el.type)

def put_attr(key: hydra.core.Name, val: hydra.core.Term) -> hydra.compute.Flow[T0, None]:
    return cast(hydra.compute.Flow[T0, None], hydra.compute.Flow((lambda s0, t0: cast(hydra.compute.FlowState[T0, None], hydra.compute.FlowState(cast(Maybe[None], Just(None)), s0, hydra.compute.Trace(t0.stack, t0.messages, hydra.lib.maps.insert(key, val, t0.other)))))))

def put_count(key: hydra.core.Name, count: int) -> hydra.compute.Flow[T0, None]:
    return put_attr(key, cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(count)))))))

def next_count(key: hydra.core.Name) -> hydra.compute.Flow[T0, int]:
    return hydra.lib.flows.bind(get_count(key), (lambda count: hydra.lib.flows.map((lambda _: count), put_count(key, hydra.lib.math.add(count, 1)))))

def normalize_term_annotations(term: hydra.core.Term) -> hydra.core.Type:
    r"""Normalize term annotations."""
    
    def anns() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return term_annotation_internal(term)
    stripped = hydra.rewriting.deannotate_term(term)
    return hydra.lib.logic.if_else(hydra.lib.maps.null(anns()), (lambda : stripped), (lambda : cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(stripped, anns())))))

def normalize_type_annotations(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Normalize type annotations."""
    
    def anns() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return type_annotation_internal(typ)
    stripped = hydra.rewriting.deannotate_type(typ)
    return hydra.lib.logic.if_else(hydra.lib.maps.null(anns()), (lambda : stripped), (lambda : cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(stripped, anns())))))

def reset_count(key: hydra.core.Name) -> hydra.compute.Flow[T0, None]:
    return put_attr(key, cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(0)))))))

def set_annotation(key: T0, val: Maybe[T1], m: FrozenDict[T0, T1]) -> FrozenDict[T0, T1]:
    return hydra.lib.maps.alter((lambda _: val), key, m)

def set_description(d: Maybe[str], v1: FrozenDict[hydra.core.Name, hydra.core.Term]) -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    r"""Set description in annotations."""
    
    return set_annotation(hydra.constants.key_description, hydra.lib.maybes.map((lambda arg_: (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(x)))((lambda x: cast(hydra.core.Literal, hydra.core.LiteralString(x)))(arg_))), d), v1)

def set_term_annotation(key: hydra.core.Name, val: Maybe[hydra.core.Term], term: hydra.core.Term) -> hydra.core.Type:
    r"""Set term annotation."""
    
    term_ = hydra.rewriting.deannotate_term(term)
    def anns() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return set_annotation(key, val, term_annotation_internal(term))
    return hydra.lib.logic.if_else(hydra.lib.maps.null(anns()), (lambda : term_), (lambda : cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(term_, anns())))))

def set_term_description(d: Maybe[str], v1: hydra.core.Term) -> hydra.core.Type:
    r"""Set term description."""
    
    return set_term_annotation(hydra.constants.key_description, hydra.lib.maybes.map((lambda s: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s))))), d), v1)

def set_type(mt: Maybe[hydra.core.Type], v1: FrozenDict[hydra.core.Name, hydra.core.Term]) -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    r"""Set type in annotations."""
    
    return set_annotation(hydra.constants.key_type, hydra.lib.maybes.map(hydra.encode.core.type, mt), v1)

def set_type_annotation(key: hydra.core.Name, val: Maybe[hydra.core.Term], typ: hydra.core.Type) -> hydra.core.Type:
    r"""Set type annotation."""
    
    typ_ = hydra.rewriting.deannotate_type(typ)
    def anns() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return set_annotation(key, val, type_annotation_internal(typ))
    return hydra.lib.logic.if_else(hydra.lib.maps.null(anns()), (lambda : typ_), (lambda : cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(typ_, anns())))))

def set_type_classes(m: FrozenDict[hydra.core.Name, frozenset[hydra.classes.TypeClass]], term: hydra.core.Term) -> hydra.core.Type:
    r"""Set type classes on term."""
    
    def encode_class(tc: hydra.classes.TypeClass) -> hydra.core.Type:
        match tc:
            case hydra.classes.TypeClass.EQUALITY:
                return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.classes.TypeClass"), hydra.core.Field(hydra.core.Name("equality"), cast(hydra.core.Term, hydra.core.TermUnit())))))
            
            case hydra.classes.TypeClass.ORDERING:
                return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.classes.TypeClass"), hydra.core.Field(hydra.core.Name("ordering"), cast(hydra.core.Term, hydra.core.TermUnit())))))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def encode_pair(name_classes: tuple[hydra.core.Name, frozenset[hydra.classes.TypeClass]]) -> tuple[hydra.core.Term, hydra.core.Term]:
        def name() -> hydra.core.Type:
            return hydra.lib.pairs.first(name_classes)
        def classes() -> frozenset[hydra.classes.TypeClass]:
            return hydra.lib.pairs.second(name_classes)
        return cast(tuple[hydra.core.Term, hydra.core.Term], (hydra.encode.core.name(name()), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(hydra.lib.lists.map(encode_class, hydra.lib.sets.to_list(classes())))))))
    def encoded() -> Maybe[hydra.core.Term]:
        return hydra.lib.logic.if_else(hydra.lib.maps.null(m), (lambda : cast(Maybe[hydra.core.Term], Nothing())), (lambda : cast(Maybe[hydra.core.Term], Just(cast(hydra.core.Term, hydra.core.TermMap(cast(FrozenDict[hydra.core.Term, hydra.core.Term], hydra.lib.maps.from_list(hydra.lib.lists.map(encode_pair, hydra.lib.maps.to_list(m))))))))))
    return set_term_annotation(hydra.constants.key_classes, encoded(), term)

def set_type_description(d: Maybe[str], v1: hydra.core.Type) -> hydra.core.Type:
    r"""Set type description."""
    
    return set_type_annotation(hydra.constants.key_description, hydra.lib.maybes.map((lambda arg_: (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(x)))((lambda x: cast(hydra.core.Literal, hydra.core.LiteralString(x)))(arg_))), d), v1)

def type_element(name: hydra.core.Name, typ: hydra.core.Type) -> hydra.core.Type:
    r"""Create a type element with proper annotations."""
    
    schema_term = cast(hydra.core.Term, hydra.core.TermVariable(hydra.core.Name("hydra.core.Type")))
    def data_term() -> hydra.core.Type:
        return normalize_term_annotations(cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(hydra.encode.core.type(typ), cast(FrozenDict[hydra.core.Name, hydra.core.Term], hydra.lib.maps.from_list((cast(tuple[hydra.core.Name, hydra.core.Term], (hydra.constants.key_type, schema_term)),)))))))
    return hydra.core.Binding(name, data_term(), cast(Maybe[hydra.core.TypeScheme], Just(hydra.core.TypeScheme(cast(frozenlist[hydra.core.Name], ()), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))), cast(Maybe[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]], Nothing())))))

def when_flag(flag: hydra.core.Name, fthen: hydra.compute.Flow[T0, T1], felse: hydra.compute.Flow[T0, T1]) -> hydra.compute.Flow[T0, T1]:
    return hydra.lib.flows.bind(has_flag(flag), (lambda b: hydra.lib.logic.if_else(b, (lambda : fthen), (lambda : felse))))

def with_depth(key: hydra.core.Name, f: Callable[[int], hydra.compute.Flow[T0, T1]]) -> hydra.compute.Flow[T0, T1]:
    return hydra.lib.flows.bind(get_count(key), (lambda count: (inc := hydra.lib.math.add(count, 1), hydra.lib.flows.bind(put_count(key, inc), (lambda _: hydra.lib.flows.bind(f(inc), (lambda r: hydra.lib.flows.bind(put_count(key, count), (lambda _2: hydra.lib.flows.pure(r))))))))[1]))
