# Note: this is an automatically generated file. Do not edit.

r"""Utilities for reading and writing type and term annotations."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.classes
import hydra.constants
import hydra.context
import hydra.core
import hydra.decode.core
import hydra.encode.core
import hydra.errors
import hydra.extract.core
import hydra.graph
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.show.core
import hydra.strip

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def aggregate_annotations(get_value: Callable[[T0], Maybe[T1]], get_x: Callable[[T1], T0], get_anns: Callable[[T1], FrozenDict[T2, T3]], t: T0) -> FrozenDict[T2, T3]:
    r"""Aggregate annotations from nested structures."""

    def to_pairs(rest: frozenlist[frozenlist[tuple[T2, T3]]], t2: T0) -> frozenlist[frozenlist[tuple[T2, T3]]]:
        return hydra.lib.maybes.maybe((lambda : rest), (lambda yy: to_pairs(hydra.lib.lists.cons(hydra.lib.maps.to_list(get_anns(yy)), rest), get_x(yy))), get_value(t2))
    return hydra.lib.maps.from_list(hydra.lib.lists.concat(to_pairs((), t)))

def get_description(cx: T0, graph: hydra.graph.Graph, anns: FrozenDict[hydra.core.Name, hydra.core.Term]) -> Either[hydra.errors.Error, Maybe[str]]:
    r"""Get description from annotations map (Either version)."""

    return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda term: hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), hydra.extract.core.string(graph, term))), hydra.lib.maps.lookup(hydra.core.Name("description"), anns))

def term_annotation_internal(term: hydra.core.Term) -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    r"""Get internal term annotations."""

    def get_ann(t: hydra.core.Term) -> Maybe[hydra.core.AnnotatedTerm]:
        match t:
            case hydra.core.TermAnnotated(value=a):
                return Just(a)

            case _:
                return Nothing()
    return aggregate_annotations((lambda x1: get_ann(x1)), (lambda at: at.body), (lambda at: at.annotation), term)

def get_term_description(cx: T0, graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.errors.Error, Maybe[str]]:
    r"""Get term description (Either version)."""

    def peel(t: hydra.core.Term) -> hydra.core.Term:
        while True:
            match t:
                case hydra.core.TermTypeLambda(value=tl):
                    t = tl.body
                    continue

                case hydra.core.TermTypeApplication(value=ta):
                    t = ta.body
                    continue

                case _:
                    return t
    return get_description(cx, graph, term_annotation_internal(peel(term)))

def comments_from_binding(cx: T0, g: hydra.graph.Graph, b: hydra.core.Binding) -> Either[hydra.errors.Error, Maybe[str]]:
    r"""Extract comments/description from a Binding."""

    return get_term_description(cx, g, b.term)

def type_annotation_internal(typ: hydra.core.Type) -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    r"""Get internal type annotations."""

    def get_ann(t: hydra.core.Type) -> Maybe[hydra.core.AnnotatedType]:
        match t:
            case hydra.core.TypeAnnotated(value=a):
                return Just(a)

            case _:
                return Nothing()
    return aggregate_annotations((lambda x1: get_ann(x1)), (lambda at: at.body), (lambda at: at.annotation), typ)

def get_type_description(cx: T0, graph: hydra.graph.Graph, typ: hydra.core.Type) -> Either[hydra.errors.Error, Maybe[str]]:
    r"""Get type description (Either version)."""

    return get_description(cx, graph, type_annotation_internal(typ))

def comments_from_field_type(cx: T0, g: hydra.graph.Graph, ft: hydra.core.FieldType) -> Either[hydra.errors.Error, Maybe[str]]:
    r"""Extract comments/description from a FieldType."""

    return get_type_description(cx, g, ft.type)

def get_attr(key: hydra.core.Name, cx: hydra.context.Context) -> Maybe[hydra.core.Term]:
    r"""Get an attribute from a context (pure version)."""

    return hydra.lib.maps.lookup(key, cx.other)

def get_debug_id(cx: hydra.context.Context) -> Either[hydra.errors.Error, Maybe[str]]:
    r"""Get the debug ID from context (Either version)."""

    return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda term: hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), hydra.extract.core.string(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), term))), get_attr(hydra.constants.key_debug_id, cx))

def debug_if(cx: hydra.context.Context, debug_id: str, message: str) -> Either[hydra.errors.Error, None]:
    r"""Debug if the debug ID matches (Either version)."""

    return hydra.lib.eithers.bind(get_debug_id(cx), (lambda mid: hydra.lib.logic.if_else(hydra.lib.equality.equal(mid, Just(debug_id)), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(message))))), (lambda : Right(None)))))

def get_attr_with_default(key: hydra.core.Name, def_: hydra.core.Term, cx: hydra.context.Context) -> hydra.core.Term:
    r"""Get an attribute with a default value from context (pure version)."""

    return hydra.lib.maybes.from_maybe((lambda : def_), get_attr(key, cx))

def has_flag(cx: hydra.context.Context, flag: hydra.core.Name) -> Either[hydra.errors.Error, bool]:
    r"""Check if flag is set (Either version)."""

    @lru_cache(1)
    def term() -> hydra.core.Term:
        return get_attr_with_default(flag, cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(False)))), cx)
    return hydra.extract.core.boolean(hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), term())

def fail_on_flag(cx: hydra.context.Context, flag: hydra.core.Name, msg: str) -> Either[hydra.errors.Error, None]:
    r"""Fail if the given flag is set (Either version)."""

    return hydra.lib.eithers.bind(has_flag(cx, flag), (lambda val: hydra.lib.logic.if_else(val, (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(msg))))), (lambda : Right(None)))))

def get_count(key: hydra.core.Name, cx: hydra.context.Context):
    def _hoist_hydra_annotations_get_count_1(v1):
        match v1:
            case hydra.core.IntegerValueInt32(value=i):
                return i

            case _:
                return 0
    def _hoist_hydra_annotations_get_count_2(v1):
        match v1:
            case hydra.core.LiteralInteger(value=iv):
                return _hoist_hydra_annotations_get_count_1(iv)

            case _:
                return 0
    def _hoist_hydra_annotations_get_count_3(v1):
        match v1:
            case hydra.core.TermLiteral(value=lit):
                return _hoist_hydra_annotations_get_count_2(lit)

            case _:
                return 0
    return hydra.lib.maybes.maybe((lambda : 0), (lambda term: _hoist_hydra_annotations_get_count_3(term)), hydra.lib.maps.lookup(key, cx.other))

def get_term_annotation(key: hydra.core.Name, term: hydra.core.Term) -> Maybe[hydra.core.Term]:
    r"""Get a term annotation."""

    return hydra.lib.maps.lookup(key, term_annotation_internal(term))

def get_type(graph: hydra.graph.Graph, anns: FrozenDict[hydra.core.Name, hydra.core.Term]) -> Either[hydra.errors.DecodingError, Maybe[hydra.core.Type]]:
    r"""Get type from annotations."""

    return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda dat: hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), hydra.decode.core.type(graph, dat))), hydra.lib.maps.lookup(hydra.constants.key_type, anns))

def get_type_annotation(key: hydra.core.Name, typ: hydra.core.Type) -> Maybe[hydra.core.Term]:
    r"""Get a type annotation."""

    return hydra.lib.maps.lookup(key, type_annotation_internal(typ))

def get_type_classes(cx: T0, graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.errors.Error, FrozenDict[hydra.core.Name, frozenset[hydra.classes.TypeClass]]]:
    r"""Get type classes from term."""

    def decode_class(term2: hydra.core.Term) -> Either[hydra.errors.Error, hydra.classes.TypeClass]:
        @lru_cache(1)
        def by_name() -> FrozenDict[hydra.core.Name, hydra.classes.TypeClass]:
            return hydra.lib.maps.from_list(((hydra.core.Name("equality"), hydra.classes.TypeClass.EQUALITY), (hydra.core.Name("ordering"), hydra.classes.TypeClass.ORDERING)))
        return hydra.lib.eithers.bind(hydra.extract.core.unit_variant(hydra.core.Name("hydra.classes.TypeClass"), graph, term2), (lambda fn: hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("type class", hydra.show.core.term(term2)))))))), (lambda x: Right(x)), hydra.lib.maps.lookup(fn, by_name()))))
    return hydra.lib.maybes.maybe((lambda : Right(hydra.lib.maps.empty())), (lambda term2: hydra.extract.core.map((lambda t: hydra.lib.eithers.bimap((lambda de: cast(hydra.errors.Error, hydra.errors.ErrorDecoding(de))), (lambda x: x), hydra.decode.core.name(graph, t))), (lambda v1: hydra.extract.core.set_of((lambda x1: decode_class(x1)), graph, v1)), graph, term2)), get_term_annotation(hydra.constants.key_classes, term))

def has_description(anns: FrozenDict[hydra.core.Name, T0]) -> bool:
    r"""Check if annotations contain description."""

    return hydra.lib.maybes.is_just(hydra.lib.maps.lookup(hydra.constants.key_description, anns))

def has_type_description(typ: hydra.core.Type) -> bool:
    r"""Check if type has description."""

    return has_description(type_annotation_internal(typ))

def is_native_type(el: hydra.core.Binding) -> bool:
    r"""For a typed term, decide whether a coder should encode it as a native type expression, or as a Hydra type expression."""

    @lru_cache(1)
    def is_flagged_as_first_class_type() -> bool:
        return hydra.lib.maybes.from_maybe((lambda : False), hydra.lib.maybes.map((lambda _: True), get_term_annotation(hydra.constants.key_first_class_type, el.term)))
    return hydra.lib.maybes.maybe((lambda : False), (lambda ts: hydra.lib.logic.and_(hydra.lib.equality.equal(ts, hydra.core.TypeScheme((), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("hydra.core.Type"))), Nothing())), hydra.lib.logic.not_(is_flagged_as_first_class_type()))), el.type)

def put_attr(key: hydra.core.Name, val: hydra.core.Term, cx: hydra.context.Context) -> hydra.context.Context:
    r"""Set an attribute in a context."""

    return hydra.context.Context(cx.trace, cx.messages, hydra.lib.maps.insert(key, val, cx.other))

def put_count(key: hydra.core.Name, count: int, cx: hydra.context.Context) -> hydra.context.Context:
    r"""Set counter value in context."""

    return put_attr(key, cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(count)))))), cx)

def next_count(key: hydra.core.Name, cx: hydra.context.Context) -> tuple[int, hydra.context.Context]:
    r"""Return a zero-indexed counter for the given key and updated context (pure version)."""

    @lru_cache(1)
    def count() -> int:
        return get_count(key, cx)
    return (count(), put_count(key, hydra.lib.math.add(count(), 1), cx))

def normalize_term_annotations(term: hydra.core.Term) -> hydra.core.Term:
    r"""Normalize term annotations."""

    @lru_cache(1)
    def anns() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return term_annotation_internal(term)
    @lru_cache(1)
    def stripped() -> hydra.core.Term:
        return hydra.strip.deannotate_term(term)
    return hydra.lib.logic.if_else(hydra.lib.maps.null(anns()), (lambda : stripped()), (lambda : cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(stripped(), anns())))))

def normalize_type_annotations(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Normalize type annotations."""

    @lru_cache(1)
    def anns() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return type_annotation_internal(typ)
    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    return hydra.lib.logic.if_else(hydra.lib.maps.null(anns()), (lambda : stripped()), (lambda : cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(stripped(), anns())))))

def reset_count(key: hydra.core.Name, cx: hydra.context.Context) -> hydra.context.Context:
    r"""Reset counter to zero in context."""

    return put_attr(key, cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(0)))))), cx)

def set_annotation(key: T0, val: Maybe[T1], m: FrozenDict[T0, T1]) -> FrozenDict[T0, T1]:
    r"""Set annotation in map."""

    return hydra.lib.maps.alter((lambda _: val), key, m)

def set_description(d: Maybe[str], v1: FrozenDict[hydra.core.Name, hydra.core.Term]) -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    r"""Set description in annotations."""

    return set_annotation(hydra.constants.key_description, hydra.lib.maybes.map((lambda arg_: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(arg_))))), d), v1)

def set_term_annotation(key: hydra.core.Name, val: Maybe[hydra.core.Term], term: hydra.core.Term) -> hydra.core.Term:
    r"""Set term annotation."""

    @lru_cache(1)
    def term_() -> hydra.core.Term:
        return hydra.strip.deannotate_term(term)
    @lru_cache(1)
    def anns() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return set_annotation(key, val, term_annotation_internal(term))
    return hydra.lib.logic.if_else(hydra.lib.maps.null(anns()), (lambda : term_()), (lambda : cast(hydra.core.Term, hydra.core.TermAnnotated(hydra.core.AnnotatedTerm(term_(), anns())))))

def set_term_description(d: Maybe[str], v1: hydra.core.Term) -> hydra.core.Term:
    r"""Set term description."""

    return set_term_annotation(hydra.constants.key_description, hydra.lib.maybes.map((lambda s: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s))))), d), v1)

def set_type(mt: Maybe[hydra.core.Type], v1: FrozenDict[hydra.core.Name, hydra.core.Term]) -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    r"""Set type in annotations."""

    return set_annotation(hydra.constants.key_type, hydra.lib.maybes.map((lambda x1: hydra.encode.core.type(x1)), mt), v1)

def set_type_annotation(key: hydra.core.Name, val: Maybe[hydra.core.Term], typ: hydra.core.Type) -> hydra.core.Type:
    r"""Set type annotation."""

    @lru_cache(1)
    def typ_() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    @lru_cache(1)
    def anns() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
        return set_annotation(key, val, type_annotation_internal(typ))
    return hydra.lib.logic.if_else(hydra.lib.maps.null(anns()), (lambda : typ_()), (lambda : cast(hydra.core.Type, hydra.core.TypeAnnotated(hydra.core.AnnotatedType(typ_(), anns())))))

def set_type_classes(m: FrozenDict[hydra.core.Name, frozenset[hydra.classes.TypeClass]], term: hydra.core.Term) -> hydra.core.Term:
    r"""Set type classes on term."""

    def encode_class(tc: hydra.classes.TypeClass) -> hydra.core.Term:
        match tc:
            case hydra.classes.TypeClass.EQUALITY:
                return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.classes.TypeClass"), hydra.core.Field(hydra.core.Name("equality"), cast(hydra.core.Term, hydra.core.TermUnit())))))

            case hydra.classes.TypeClass.ORDERING:
                return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.classes.TypeClass"), hydra.core.Field(hydra.core.Name("ordering"), cast(hydra.core.Term, hydra.core.TermUnit())))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def encode_pair(name_classes: tuple[hydra.core.Name, frozenset[hydra.classes.TypeClass]]) -> tuple[hydra.core.Term, hydra.core.Term]:
        @lru_cache(1)
        def name() -> hydra.core.Name:
            return hydra.lib.pairs.first(name_classes)
        @lru_cache(1)
        def classes() -> frozenset[hydra.classes.TypeClass]:
            return hydra.lib.pairs.second(name_classes)
        return (hydra.encode.core.name(name()), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.from_list(hydra.lib.lists.map((lambda x1: encode_class(x1)), hydra.lib.sets.to_list(classes()))))))
    @lru_cache(1)
    def encoded() -> Maybe[hydra.core.Term]:
        return hydra.lib.logic.if_else(hydra.lib.maps.null(m), (lambda : Nothing()), (lambda : Just(cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda x1: encode_pair(x1)), hydra.lib.maps.to_list(m))))))))
    return set_term_annotation(hydra.constants.key_classes, encoded(), term)

def set_type_description(d: Maybe[str], v1: hydra.core.Type) -> hydra.core.Type:
    r"""Set type description."""

    return set_type_annotation(hydra.constants.key_description, hydra.lib.maybes.map((lambda arg_: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(arg_))))), d), v1)

def when_flag(cx: hydra.context.Context, flag: hydra.core.Name, ethen: Either[hydra.errors.Error, T0], eelse: Either[hydra.errors.Error, T0]) -> Either[hydra.errors.Error, T0]:
    r"""Execute different branches based on flag (Either version)."""

    return hydra.lib.eithers.bind(has_flag(cx, flag), (lambda b: hydra.lib.logic.if_else(b, (lambda : ethen), (lambda : eelse))))
