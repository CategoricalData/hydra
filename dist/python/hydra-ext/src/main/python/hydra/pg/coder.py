# Note: this is an automatically generated file. Do not edit.

r"""Property graph element coders for mapping Hydra terms to property graph elements."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.coders
import hydra.core
import hydra.errors
import hydra.extract.core
import hydra.graph
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.pg.mapping
import hydra.pg.model
import hydra.pg.terms_to_elements
import hydra.resolution
import hydra.strip

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T10 = TypeVar("T10")
T11 = TypeVar("T11")
T12 = TypeVar("T12")
T13 = TypeVar("T13")
T14 = TypeVar("T14")
T15 = TypeVar("T15")
T2 = TypeVar("T2")
T3 = TypeVar("T3")
T4 = TypeVar("T4")
T5 = TypeVar("T5")
T6 = TypeVar("T6")
T7 = TypeVar("T7")
T8 = TypeVar("T8")
T9 = TypeVar("T9")

def check(_cx: T0, b: bool, e: Either[T1, None]) -> Either[T1, None]:
    r"""Check a condition, returning an error if false."""

    return hydra.lib.logic.if_else(b, (lambda : Right(None)), (lambda : e))

def check_record_name(cx: T0, expected: hydra.core.Name, actual: hydra.core.Name) -> Either[hydra.errors.Error, None]:
    r"""Check that a record name matches the expected name."""

    return check(cx, hydra.lib.logic.or_(hydra.lib.equality.equal(expected.value, "placeholder"), hydra.lib.equality.equal(actual.value, expected.value)), Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("Expected record of type ", expected.value), ", found record of type "), actual.value))))))

def element_tree_edge(edge: hydra.pg.model.Edge[T0], deps: frozenlist[hydra.pg.model.ElementTree[T0]]) -> hydra.pg.model.ElementTree[T0]:
    r"""Create an element tree for an edge."""

    return hydra.pg.model.ElementTree(cast(hydra.pg.model.Element, hydra.pg.model.ElementEdge(edge)), deps)

def element_type_tree_edge(etype: hydra.pg.model.EdgeType[T0], deps: frozenlist[hydra.pg.model.ElementTypeTree[T0]]) -> hydra.pg.model.ElementTypeTree[T0]:
    r"""Create an element type tree for an edge type."""

    return hydra.pg.model.ElementTypeTree(cast(hydra.pg.model.ElementType, hydra.pg.model.ElementTypeEdge(etype)), deps)

def encode_property(cx: hydra.context.Context, fields: FrozenDict[hydra.core.Name, hydra.core.Term], adapter: hydra.coders.Adapter[hydra.core.FieldType, T0, hydra.core.Field, T1]):
    r"""Encode a single property from a field map using a property adapter."""

    @lru_cache(1)
    def fname() -> hydra.core.Name:
        return adapter.source.name
    @lru_cache(1)
    def ftyp() -> hydra.core.Type:
        return hydra.strip.deannotate_type(adapter.source.type)
    @lru_cache(1)
    def is_maybe() -> bool:
        match ftyp():
            case hydra.core.TypeMaybe():
                return True

            case _:
                return False
    def encode_value(v: hydra.core.Term) -> Either[hydra.errors.Error, Maybe[T1]]:
        return hydra.lib.eithers.map((lambda x: Just(x)), adapter.coder.encode(cx, hydra.core.Field(fname(), v)))
    def _hoist_fname_body_1(value, v1):
        match v1:
            case hydra.core.TermMaybe(value=ov):
                return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda x1: encode_value(x1)), ov)

            case _:
                return encode_value(value)
    return hydra.lib.maybes.maybe((lambda : hydra.lib.logic.if_else(is_maybe(), (lambda : Right(Nothing())), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("expected field not found in record: ", fname().value)))))))), (lambda value: hydra.lib.logic.if_else(is_maybe(), (lambda : _hoist_fname_body_1(value, hydra.strip.deannotate_term(value))), (lambda : encode_value(value)))), hydra.lib.maps.lookup(fname(), fields))

def encode_properties(cx: hydra.context.Context, fields: FrozenDict[hydra.core.Name, hydra.core.Term], adapters: frozenlist[hydra.coders.Adapter[hydra.core.FieldType, T0, hydra.core.Field, hydra.pg.model.Property[T1]]]) -> Either[hydra.errors.Error, FrozenDict[hydra.pg.model.PropertyKey, T1]]:
    r"""Encode all properties from a field map using property adapters."""

    return hydra.lib.eithers.map((lambda props: hydra.lib.maps.from_list(hydra.lib.lists.map((lambda prop: (prop.key, prop.value)), props))), hydra.lib.eithers.map((lambda xs: hydra.lib.maybes.cat(xs)), hydra.lib.eithers.map_list((lambda v1: encode_property(cx, fields, v1)), adapters)))

def property_types(prop_adapters: frozenlist[hydra.coders.Adapter[T0, hydra.pg.model.PropertyType[T1], T2, T3]]) -> frozenlist[hydra.pg.model.PropertyType[T1]]:
    r"""Extract property types from property adapters."""

    return hydra.lib.lists.map((lambda a: hydra.pg.model.PropertyType(a.target.key, a.target.value, True)), prop_adapters)

def select_edge_id(cx: hydra.context.Context, fields: FrozenDict[hydra.core.Name, T0], ad: tuple[hydra.core.Name, hydra.coders.Adapter[T1, T2, T0, T3]]) -> Either[hydra.errors.Error, T3]:
    r"""Select an edge id from record fields using an id adapter."""

    @lru_cache(1)
    def fname() -> hydra.core.Name:
        return hydra.lib.pairs.first(ad)
    @lru_cache(1)
    def adapter() -> hydra.coders.Adapter[T1, T2, T0, T3]:
        return hydra.lib.pairs.second(ad)
    return hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2("no ", fname().value), " in record")))))), (lambda t: adapter().coder.encode(cx, t)), hydra.lib.maps.lookup(fname(), fields))

def select_vertex_id(cx: hydra.context.Context, fields: FrozenDict[hydra.core.Name, T0], ad: tuple[hydra.core.Name, hydra.coders.Adapter[T1, T2, T0, T3]]) -> Either[hydra.errors.Error, T3]:
    r"""Select a vertex id from record fields using an id adapter."""

    @lru_cache(1)
    def fname() -> hydra.core.Name:
        return hydra.lib.pairs.first(ad)
    @lru_cache(1)
    def adapter() -> hydra.coders.Adapter[T1, T2, T0, T3]:
        return hydra.lib.pairs.second(ad)
    return hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2("no ", fname().value), " in record")))))), (lambda t: adapter().coder.encode(cx, t)), hydra.lib.maps.lookup(fname(), fields))

def edge_coder(g: T0, dir: hydra.pg.model.Direction, schema: hydra.pg.mapping.Schema[T1, T2, T3], source: T4, eid_type: T5, tname: hydra.core.Name, label: hydra.pg.model.EdgeLabel, out_label: hydra.pg.model.VertexLabel, in_label: hydra.pg.model.VertexLabel, m_id_adapter: Maybe[tuple[hydra.core.Name, hydra.coders.Adapter[T6, T7, hydra.core.Term, T3]]], out_adapter: Maybe[tuple[hydra.core.Name, hydra.coders.Adapter[T8, T9, hydra.core.Term, T3]]], in_adapter: Maybe[tuple[hydra.core.Name, hydra.coders.Adapter[T10, T11, hydra.core.Term, T3]]], prop_adapters: frozenlist[hydra.coders.Adapter[hydra.core.FieldType, hydra.pg.model.PropertyType[T5], hydra.core.Field, hydra.pg.model.Property[T3]]], vertex_adapters: frozenlist[tuple[hydra.core.Name, hydra.coders.Adapter[T12, T13, hydra.core.Term, hydra.pg.model.ElementTree[T3]]]]) -> hydra.coders.Adapter[T4, hydra.pg.model.ElementTypeTree[T5], hydra.core.Term, hydra.pg.model.ElementTree[T3]]:
    r"""Create an edge coder given all components."""

    @lru_cache(1)
    def et() -> hydra.pg.model.EdgeType[T5]:
        return hydra.pg.model.EdgeType(label, eid_type, out_label, in_label, property_types(prop_adapters))
    return hydra.coders.Adapter(True, source, element_type_tree_edge(et(), ()), hydra.coders.Coder((lambda cx, term: (deannot := hydra.strip.deannotate_term(term), unwrapped := (_hoist_unwrapped_1 := (lambda v1: (lambda mt: hydra.lib.maybes.from_maybe((lambda : deannot), mt))(v1.value) if isinstance(v1, hydra.core.TermMaybe) else deannot), _hoist_unwrapped_1(deannot))[1], rec := (_hoist_rec_1 := (lambda v1: (lambda r: r)(v1.value) if isinstance(v1, hydra.core.TermRecord) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), _hoist_rec_1(unwrapped))[1], hydra.lib.eithers.bind(check_record_name(cx, tname, rec.type_name), (lambda _chk: (fieldsm := hydra.resolution.field_map(rec.fields), hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(schema.default_edge_id)), (lambda v1: select_edge_id(cx, fieldsm, v1)), m_id_adapter), (lambda edge_id: hydra.lib.eithers.bind(encode_properties(cx, fieldsm, prop_adapters), (lambda props: (get_vertex_id := (lambda dir_check, adapter: hydra.lib.maybes.maybe((lambda : Right(schema.default_vertex_id)), (lambda v1: select_vertex_id(cx, fieldsm, v1)), hydra.lib.logic.if_else(hydra.lib.equality.equal(dir, dir_check), (lambda : Nothing()), (lambda : adapter)))), hydra.lib.eithers.bind(get_vertex_id(hydra.pg.model.Direction.OUT, out_adapter), (lambda out_id: hydra.lib.eithers.bind(get_vertex_id(hydra.pg.model.Direction.IN, in_adapter), (lambda in_id: hydra.lib.eithers.bind(hydra.lib.eithers.map((lambda xs: hydra.lib.maybes.cat(xs)), hydra.lib.eithers.map_list((lambda va: (fname := hydra.lib.pairs.first(va), ad := hydra.lib.pairs.second(va), hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda fterm: hydra.lib.eithers.map((lambda x: Just(x)), ad.coder.encode(cx, fterm))), hydra.lib.maps.lookup(fname, fieldsm)))[2]), vertex_adapters)), (lambda deps: Right(element_tree_edge(hydra.pg.model.Edge(label, edge_id, out_id, in_id, props), deps)))))))))[1])))))[1])))[3]), (lambda cx, _: Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("edge decoding is not yet supported")))))))

def find_single_field_with_annotation_key(cx: T0, tname: hydra.core.Name, key: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> Either[hydra.errors.Error, Maybe[hydra.core.FieldType]]:
    r"""Find a single field with a given annotation key."""

    @lru_cache(1)
    def matches() -> frozenlist[hydra.core.FieldType]:
        return hydra.lib.lists.filter((lambda f: hydra.lib.maybes.is_just(hydra.annotations.get_type_annotation(key, f.type))), fields)
    return hydra.lib.logic.if_else(hydra.lib.equality.gt(hydra.lib.lists.length(matches()), 1), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("Multiple fields marked as '", key.value), "' in record type "), tname.value)))))), (lambda : Right(hydra.lib.lists.maybe_head(matches()))))

def find_id_projection_spec(cx: T0, required: bool, tname: hydra.core.Name, id_key: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> Either[hydra.errors.Error, Maybe[tuple[hydra.core.FieldType, tuple[hydra.pg.mapping.ValueSpec, Maybe[str]]]]]:
    r"""Find an id projection spec for a field."""

    return hydra.lib.eithers.bind(find_single_field_with_annotation_key(cx, tname, id_key, fields), (lambda mid: hydra.lib.maybes.maybe((lambda : hydra.lib.logic.if_else(required, (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2("no ", id_key.value), " field")))))), (lambda : Right(Nothing())))), (lambda mi: hydra.lib.eithers.map((lambda spec: Just((mi, (spec, hydra.lib.maybes.map((lambda s: hydra.lib.strings.to_upper(s)), Nothing()))))), hydra.lib.maybes.maybe((lambda : Right(cast(hydra.pg.mapping.ValueSpec, hydra.pg.mapping.ValueSpecValue()))), (lambda v1: hydra.pg.terms_to_elements.decode_value_spec(cx, hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), v1)), hydra.annotations.get_type_annotation(id_key, mi.type)))), mid)))

def traverse_to_single_term(cx: T0, desc: str, traversal: Callable[[T1], Either[hydra.errors.Error, frozenlist[T2]]], term: T1) -> Either[hydra.errors.Error, T2]:
    r"""Traverse to a single term, failing if zero or multiple terms are found."""

    return hydra.lib.eithers.bind(traversal(term), (lambda terms: hydra.lib.logic.if_else(hydra.lib.lists.null(terms), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2(desc, " did not resolve to a term")))))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(terms), 1), (lambda : hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2(desc, " resolved to multiple terms")))))), (lambda x: Right(x)), hydra.lib.lists.maybe_head(terms))), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2(desc, " resolved to multiple terms")))))))))))

def projection_adapter(cx: T0, g: T1, idtype: T2, coder: hydra.coders.Coder[hydra.core.Term, T3], spec: tuple[hydra.core.FieldType, tuple[hydra.pg.mapping.ValueSpec, T4]], key: str) -> Either[T5, tuple[hydra.core.Name, hydra.coders.Adapter[hydra.core.Type, T2, hydra.core.Term, T3]]]:
    r"""Create a projection adapter from a projection spec."""

    @lru_cache(1)
    def field() -> hydra.core.FieldType:
        return hydra.lib.pairs.first(spec)
    @lru_cache(1)
    def values() -> hydra.pg.mapping.ValueSpec:
        return hydra.lib.pairs.first(hydra.lib.pairs.second(spec))
    return hydra.lib.eithers.bind(hydra.pg.terms_to_elements.parse_value_spec(cx, g, values()), (lambda traversal: Right((field().name, hydra.coders.Adapter(True, field().type, idtype, hydra.coders.Coder((lambda cx_, typ: hydra.lib.eithers.bind(traverse_to_single_term(cx_, hydra.lib.strings.cat2(key, "-projection"), (lambda v1: traversal(cx_, v1)), typ), (lambda t: coder.encode(cx_, t)))), (lambda cx_, _: Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2("edge '", key), "' decoding is not yet supported"))))))))))))

def edge_id_adapter(cx: T0, g: T1, schema: hydra.pg.mapping.Schema[T2, T3, T4], eid_type: T5, name: hydra.core.Name, id_key: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> Either[hydra.errors.Error, Maybe[tuple[hydra.core.Name, hydra.coders.Adapter[hydra.core.Type, T5, hydra.core.Term, T4]]]]:
    r"""Create an edge id adapter."""

    return hydra.lib.eithers.bind(find_id_projection_spec(cx, False, name, id_key, fields), (lambda m_id_spec: hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda id_spec: hydra.lib.eithers.map((lambda x: Just(x)), projection_adapter(cx, g, eid_type, schema.edge_ids, id_spec, "id"))), m_id_spec)))

def extract_string(cx: T0, g: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, str]:
    r"""Extract a string from a term."""

    return hydra.extract.core.string(g, t)

def find_label_string(cx: T0, g: hydra.graph.Graph, source: hydra.core.Type, tname: hydra.core.Name, label_key: hydra.core.Name) -> Either[hydra.errors.Error, str]:
    r"""Find a label string from annotations or the type name."""

    return hydra.lib.maybes.maybe((lambda : Right(tname.value)), (lambda v1: extract_string(cx, g, v1)), hydra.annotations.get_type_annotation(label_key, source))

def element_tree_vertex(vertex: hydra.pg.model.Vertex[T0], deps: frozenlist[hydra.pg.model.ElementTree[T0]]) -> hydra.pg.model.ElementTree[T0]:
    r"""Create an element tree for a vertex."""

    return hydra.pg.model.ElementTree(cast(hydra.pg.model.Element, hydra.pg.model.ElementVertex(vertex)), deps)

def element_type_tree_vertex(vtype: hydra.pg.model.VertexType[T0], deps: frozenlist[hydra.pg.model.ElementTypeTree[T0]]) -> hydra.pg.model.ElementTypeTree[T0]:
    r"""Create an element type tree for a vertex type."""

    return hydra.pg.model.ElementTypeTree(cast(hydra.pg.model.ElementType, hydra.pg.model.ElementTypeVertex(vtype)), deps)

def vertex_coder(g: T0, schema: hydra.pg.mapping.Schema[T1, T2, T3], source: T4, vid_type: T5, tname: T6, vlabel: hydra.pg.model.VertexLabel, id_adapter: tuple[hydra.core.Name, hydra.coders.Adapter[T7, T8, hydra.core.Term, T3]], prop_adapters: frozenlist[hydra.coders.Adapter[hydra.core.FieldType, hydra.pg.model.PropertyType[T5], hydra.core.Field, hydra.pg.model.Property[T3]]], edge_adapters: frozenlist[tuple[hydra.pg.model.Direction, tuple[hydra.core.FieldType, tuple[hydra.pg.model.EdgeLabel, hydra.coders.Adapter[T9, hydra.pg.model.ElementTypeTree[T5], hydra.core.Term, hydra.pg.model.ElementTree[T3]]]]]]):
    r"""Create a vertex coder given all components."""

    @lru_cache(1)
    def vtype() -> hydra.pg.model.VertexType[T5]:
        return hydra.pg.model.VertexType(vlabel, vid_type, property_types(prop_adapters))
    @lru_cache(1)
    def dep_types() -> frozenlist[hydra.pg.model.ElementTypeTree[T5]]:
        return hydra.lib.lists.map((lambda ea: hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(ea))).target), edge_adapters)
    @lru_cache(1)
    def target() -> hydra.pg.model.ElementTypeTree[T5]:
        return element_type_tree_vertex(vtype(), dep_types())
    return hydra.coders.Adapter(True, source, target(), hydra.coders.Coder((lambda cx, term: (deannot := hydra.strip.deannotate_term(term), unwrapped := (_hoist_unwrapped_1 := (lambda v1: (lambda mt: hydra.lib.maybes.from_maybe((lambda : deannot), mt))(v1.value) if isinstance(v1, hydra.core.TermMaybe) else deannot), _hoist_unwrapped_1(deannot))[1], rec := (_hoist_rec_1 := (lambda v1: (lambda r: r)(v1.value) if isinstance(v1, hydra.core.TermRecord) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), _hoist_rec_1(unwrapped))[1], fmap := hydra.resolution.field_map(rec.fields), hydra.lib.eithers.bind(select_vertex_id(cx, fmap, id_adapter), (lambda vid: hydra.lib.eithers.bind(encode_properties(cx, fmap, prop_adapters), (lambda props: hydra.lib.eithers.bind(hydra.lib.eithers.map((lambda xs: hydra.lib.lists.concat(xs)), hydra.lib.eithers.map_list((lambda ea: (ea_dir := hydra.lib.pairs.first(ea), ea_field := hydra.lib.pairs.first(hydra.lib.pairs.second(ea)), ea_label := hydra.lib.pairs.first(hydra.lib.pairs.second(hydra.lib.pairs.second(ea))), ea_adapter := hydra.lib.pairs.second(hydra.lib.pairs.second(hydra.lib.pairs.second(ea))), _hoist_ea_dir_body_1 := (lambda tree, v1: (lambda vtx: (otherid := vtx.id, edgeid := schema.default_edge_id, out_id := (_hoist_out_id_1 := (lambda v12: (lambda _: vid)(v12) if v12 else (lambda _: otherid)(v12) if v12 else hydra.dsl.python.unsupported("no matching case in inline union elimination")), _hoist_out_id_1(ea_dir))[1], in_id := (_hoist_in_id_1 := (lambda v12: (lambda _: otherid)(v12) if v12 else (lambda _: vid)(v12) if v12 else hydra.dsl.python.unsupported("no matching case in inline union elimination")), _hoist_in_id_1(ea_dir))[1], edge := cast(hydra.pg.model.Element, hydra.pg.model.ElementEdge(hydra.pg.model.Edge(ea_label, edgeid, out_id, in_id, hydra.lib.maps.empty()))), (hydra.pg.model.ElementTree(edge, (tree,)),))[5])(v1.value) if isinstance(v1, hydra.pg.model.ElementVertex) else (lambda edg: (fixed_edge := (_hoist_fixed_edge_1 := (lambda v12: (lambda _: hydra.pg.model.Edge(edg.label, edg.id, vid, edg.in_, edg.properties))(v12) if v12 else (lambda _: hydra.pg.model.Edge(edg.label, edg.id, edg.out, vid, edg.properties))(v12) if v12 else hydra.dsl.python.unsupported("no matching case in inline union elimination")), _hoist_fixed_edge_1(ea_dir))[1], (hydra.pg.model.ElementTree(cast(hydra.pg.model.Element, hydra.pg.model.ElementEdge(fixed_edge)), tree.dependencies),))[1])(v1.value) if isinstance(v1, hydra.pg.model.ElementEdge) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), hydra.lib.maybes.maybe((lambda : Right(())), (lambda fterm: hydra.lib.eithers.map((lambda tree: _hoist_ea_dir_body_1(tree, tree.self)), ea_adapter.coder.encode(cx, fterm))), hydra.lib.maps.lookup(ea_field.name, fmap)))[5]), edge_adapters)), (lambda deps: Right(element_tree_vertex(hydra.pg.model.Vertex(vlabel, vid, props), deps)))))))))[4]), (lambda cx, _: Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("vertex decoding is not yet supported")))))))

def vertex_id_adapter(cx: T0, g: T1, schema: hydra.pg.mapping.Schema[T2, T3, T4], vid_type: T5, name: hydra.core.Name, id_key: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> Either[hydra.errors.Error, tuple[hydra.core.Name, hydra.coders.Adapter[hydra.core.Type, T5, hydra.core.Term, T4]]]:
    r"""Create a vertex id adapter."""

    return hydra.lib.eithers.bind(find_id_projection_spec(cx, True, name, id_key, fields), (lambda m_id_spec: hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("vertexIdAdapter: no id projection spec"))))), (lambda id_spec: projection_adapter(cx, g, vid_type, schema.vertex_ids, id_spec, "id")), m_id_spec)))

def find_projection_spec(cx: T0, g: hydra.graph.Graph, tname: hydra.core.Name, key: hydra.core.Name, alias_key: hydra.core.Name, fields: frozenlist[hydra.core.FieldType]) -> Either[hydra.errors.Error, Maybe[tuple[hydra.core.FieldType, tuple[hydra.pg.mapping.ValueSpec, Maybe[str]]]]]:
    r"""Find a projection spec for a field."""

    return hydra.lib.eithers.bind(find_single_field_with_annotation_key(cx, tname, key, fields), (lambda mfield: hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda field: hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("findProjectionSpec: missing type annotation for key"))))), (lambda annot: hydra.lib.eithers.bind(hydra.pg.terms_to_elements.decode_value_spec(cx, g, annot), (lambda spec: hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda t: hydra.lib.eithers.map((lambda x: Just(x)), extract_string(cx, g, t))), hydra.annotations.get_type_annotation(alias_key, field.type)), (lambda alias: Right(Just((field, (spec, alias))))))))), hydra.annotations.get_type_annotation(key, field.type))), mfield)))

def find_property_specs(cx: T0, g: hydra.graph.Graph, schema: hydra.pg.mapping.Schema[T1, T2, T3], kind: hydra.pg.model.ElementKind, fields: frozenlist[hydra.core.FieldType]) -> Either[hydra.errors.Error, frozenlist[tuple[hydra.core.FieldType, tuple[hydra.pg.mapping.ValueSpec, Maybe[str]]]]]:
    r"""Find property specs for element fields."""

    return hydra.lib.eithers.map_list((lambda field: (prop_key_key := hydra.core.Name(schema.annotations.property_key), prop_value_key := hydra.core.Name(schema.annotations.property_value), hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda a: hydra.lib.eithers.map((lambda x: Just(x)), extract_string(cx, g, a))), hydra.annotations.get_type_annotation(prop_key_key, field.type)), (lambda alias: hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(cast(hydra.pg.mapping.ValueSpec, hydra.pg.mapping.ValueSpecValue()))), (lambda v1: hydra.pg.terms_to_elements.decode_value_spec(cx, g, v1)), hydra.annotations.get_type_annotation(prop_value_key, field.type)), (lambda values: Right((field, (values, alias))))))))[2]), hydra.lib.lists.filter((lambda field: (annots := schema.annotations, ignore_key := hydra.core.Name(annots.ignore), special_keys := (_hoist_special_keys_1 := (lambda v1: (lambda _: (hydra.core.Name(annots.vertex_id), hydra.core.Name(annots.out_edge_label), hydra.core.Name(annots.in_edge_label)))(v1) if v1 else (lambda _: (hydra.core.Name(annots.edge_id), hydra.core.Name(annots.out_vertex), hydra.core.Name(annots.in_vertex)))(v1) if v1 else hydra.dsl.python.unsupported("no matching case in inline union elimination")), _hoist_special_keys_1(kind))[1], all_keys := hydra.lib.lists.concat(((ignore_key,), special_keys)), has_special_annotation := hydra.lib.lists.foldl((lambda b, k: hydra.lib.logic.or_(b, hydra.lib.maybes.is_just(hydra.annotations.get_type_annotation(k, field.type)))), False, all_keys), has_special_field_name := hydra.lib.lists.foldl((lambda b, k: hydra.lib.logic.or_(b, hydra.lib.equality.equal(field.name, k))), False, special_keys), hydra.lib.logic.not_(hydra.lib.logic.or_(has_special_annotation, has_special_field_name)))[6]), fields))

def has_vertex_adapters(dir: hydra.pg.model.Direction, m_out_spec: Maybe[T0], m_in_spec: Maybe[T1]) -> bool:
    r"""Determine whether the spec has vertex adapters based on direction and out/in specs."""

    match dir:
        case hydra.pg.model.Direction.OUT:
            return hydra.lib.maybes.is_just(m_in_spec)

        case hydra.pg.model.Direction.IN:
            return hydra.lib.maybes.is_just(m_out_spec)

        case hydra.pg.model.Direction.BOTH:
            return hydra.lib.logic.and_(hydra.lib.maybes.is_just(m_out_spec), hydra.lib.maybes.is_just(m_in_spec))

        case _:
            raise TypeError("Unsupported Direction")

def property_adapter(cx: hydra.context.Context, g: T0, schema: hydra.pg.mapping.Schema[T1, T2, T3], spec: tuple[hydra.core.FieldType, tuple[hydra.pg.mapping.ValueSpec, Maybe[str]]]) -> Either[hydra.errors.Error, hydra.coders.Adapter[hydra.core.FieldType, hydra.pg.model.PropertyType[T2], hydra.core.Field, hydra.pg.model.Property[T3]]]:
    r"""Create a property adapter from a property spec."""

    @lru_cache(1)
    def tfield() -> hydra.core.FieldType:
        return hydra.lib.pairs.first(spec)
    @lru_cache(1)
    def values() -> hydra.pg.mapping.ValueSpec:
        return hydra.lib.pairs.first(hydra.lib.pairs.second(spec))
    @lru_cache(1)
    def alias() -> Maybe[str]:
        return hydra.lib.pairs.second(hydra.lib.pairs.second(spec))
    @lru_cache(1)
    def key() -> hydra.pg.model.PropertyKey:
        return hydra.pg.model.PropertyKey(hydra.lib.maybes.from_maybe((lambda : tfield().name.value), alias()))
    return hydra.lib.eithers.bind(schema.property_types.encode(cx, tfield().type), (lambda pt: hydra.lib.eithers.bind(hydra.pg.terms_to_elements.parse_value_spec(cx, g, values()), (lambda traversal: Right(hydra.coders.Adapter(True, tfield(), hydra.pg.model.PropertyType(key(), pt, True), hydra.coders.Coder((lambda cx_, dfield: hydra.lib.eithers.bind(traverse_to_single_term(cx_, "property traversal", (lambda v1: traversal(cx_, v1)), dfield.term), (lambda result: hydra.lib.eithers.bind(schema.property_values.encode(cx_, result), (lambda value: Right(hydra.pg.model.Property(key(), value))))))), (lambda cx_, _: Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("property decoding is not yet supported"))))))))))))

def construct_edge_coder(cx: hydra.context.Context, g: hydra.graph.Graph, parent_label: hydra.pg.model.VertexLabel, schema: hydra.pg.mapping.Schema[T0, T1, T2], source: hydra.core.Type, vid_type: T1, eid_type: T1, dir: hydra.pg.model.Direction, name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType], prop_adapters: frozenlist[hydra.coders.Adapter[hydra.core.FieldType, hydra.pg.model.PropertyType[T1], hydra.core.Field, hydra.pg.model.Property[T2]]], m_out_spec: Maybe[tuple[hydra.core.FieldType, tuple[hydra.pg.mapping.ValueSpec, Maybe[str]]]], m_in_spec: Maybe[tuple[hydra.core.FieldType, tuple[hydra.pg.mapping.ValueSpec, Maybe[str]]]]) -> Either[hydra.errors.Error, hydra.coders.Adapter[hydra.core.Type, hydra.pg.model.ElementTypeTree[T1], hydra.core.Term, hydra.pg.model.ElementTree[T2]]]:
    r"""Construct an edge coder from components."""

    return hydra.lib.eithers.bind(find_label_string(cx, g, source, name, hydra.core.Name(schema.annotations.edge_label)), (lambda label_str: (label := hydra.pg.model.EdgeLabel(label_str), vertex_ids_schema := schema.vertex_ids, hydra.lib.eithers.bind(edge_id_adapter(cx, g, schema, eid_type, name, hydra.core.Name(schema.annotations.edge_id), fields), (lambda id_adapter: hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda s: hydra.lib.eithers.map((lambda x: Just(x)), projection_adapter(cx, g, vid_type, vertex_ids_schema, s, "out"))), m_out_spec), (lambda out_id_adapter: hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda s: hydra.lib.eithers.map((lambda x: Just(x)), projection_adapter(cx, g, vid_type, vertex_ids_schema, s, "in"))), m_in_spec), (lambda in_id_adapter: hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda s: hydra.lib.eithers.map((lambda x: Just(x)), find_incident_vertex_adapter(cx, g, schema, vid_type, eid_type, s))), m_out_spec), (lambda out_vertex_adapter: hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda s: hydra.lib.eithers.map((lambda x: Just(x)), find_incident_vertex_adapter(cx, g, schema, vid_type, eid_type, s))), m_in_spec), (lambda in_vertex_adapter: (vertex_adapters := hydra.lib.maybes.cat((out_vertex_adapter, in_vertex_adapter)), hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(parent_label)), (lambda spec: hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("no out-vertex label"))))), (lambda a: Right(hydra.pg.model.VertexLabel(a))), hydra.lib.pairs.second(hydra.lib.pairs.second(spec)))), m_out_spec), (lambda out_label: hydra.lib.eithers.bind(hydra.lib.maybes.maybe((lambda : Right(parent_label)), (lambda spec: hydra.lib.maybes.maybe((lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("no in-vertex label"))))), (lambda a: Right(hydra.pg.model.VertexLabel(a))), hydra.lib.pairs.second(hydra.lib.pairs.second(spec)))), m_in_spec), (lambda in_label: Right(edge_coder(g, dir, schema, source, eid_type, name, label, out_label, in_label, id_adapter, out_id_adapter, in_id_adapter, prop_adapters, vertex_adapters)))))))[1])))))))))))[2]))

def construct_vertex_coder(cx: hydra.context.Context, g: hydra.graph.Graph, schema: hydra.pg.mapping.Schema[T0, T1, T2], source: hydra.core.Type, vid_type: T1, eid_type: T1, name: hydra.core.Name, fields: frozenlist[hydra.core.FieldType], prop_adapters: frozenlist[hydra.coders.Adapter[hydra.core.FieldType, hydra.pg.model.PropertyType[T1], hydra.core.Field, hydra.pg.model.Property[T2]]]) -> Either[hydra.errors.Error, hydra.coders.Adapter[hydra.core.Type, hydra.pg.model.ElementTypeTree[T1], hydra.core.Term, hydra.pg.model.ElementTree[T2]]]:
    r"""Construct a vertex coder from components."""

    return hydra.lib.eithers.bind(find_label_string(cx, g, source, name, hydra.core.Name(schema.annotations.vertex_label)), (lambda label_str: (label := hydra.pg.model.VertexLabel(label_str), hydra.lib.eithers.bind(vertex_id_adapter(cx, g, schema, vid_type, name, hydra.core.Name(schema.annotations.vertex_id), fields), (lambda id_adapter: hydra.lib.eithers.bind(find_adjacen_edge_adapters(cx, g, schema, vid_type, eid_type, label, hydra.pg.model.Direction.OUT, fields), (lambda out_edge_adapters: hydra.lib.eithers.bind(find_adjacen_edge_adapters(cx, g, schema, vid_type, eid_type, label, hydra.pg.model.Direction.IN, fields), (lambda in_edge_adapters: Right(vertex_coder(g, schema, source, vid_type, name, label, id_adapter, prop_adapters, hydra.lib.lists.concat2(out_edge_adapters, in_edge_adapters))))))))))[1]))

def element_coder(mparent: Maybe[tuple[hydra.pg.model.Direction, hydra.pg.model.VertexLabel]], schema: hydra.pg.mapping.Schema[T0, T1, T2], source: hydra.core.Type, vid_type: T1, eid_type: T1, cx: hydra.context.Context, g: hydra.graph.Graph):
    r"""Construct an element adapter for a given type, interpreting it either as a vertex specification or an edge specification."""

    while True:
        @lru_cache(1)
        def dir() -> hydra.pg.model.Direction:
            return hydra.lib.maybes.maybe((lambda : hydra.pg.model.Direction.BOTH), (lambda p: hydra.lib.pairs.first(p)), mparent)
        @lru_cache(1)
        def parent_label() -> hydra.pg.model.VertexLabel:
            return hydra.lib.maybes.maybe((lambda : hydra.pg.model.VertexLabel("NOLABEL")), (lambda p: hydra.lib.pairs.second(p)), mparent)
        match hydra.strip.deannotate_type(source):
            case hydra.core.TypeMaybe(value=ot):
                mparent = mparent
                schema = schema
                source = ot
                vid_type = vid_type
                eid_type = eid_type
                cx = cx
                g = g
                continue

            case hydra.core.TypeRecord(value=fields):
                return (name := hydra.core.Name("placeholder"), out_vertex_key := hydra.core.Name(schema.annotations.out_vertex), out_vertex_label_key := hydra.core.Name(schema.annotations.out_vertex_label), in_vertex_key := hydra.core.Name(schema.annotations.in_vertex), in_vertex_label_key := hydra.core.Name(schema.annotations.in_vertex_label), hydra.lib.eithers.bind(find_projection_spec(cx, g, name, out_vertex_key, out_vertex_label_key, fields), (lambda m_out_spec: hydra.lib.eithers.bind(find_projection_spec(cx, g, name, in_vertex_key, in_vertex_label_key, fields), (lambda m_in_spec: (kind := hydra.lib.logic.if_else(has_vertex_adapters(dir(), m_out_spec, m_in_spec), (lambda : hydra.pg.model.ElementKind.EDGE), (lambda : hydra.pg.model.ElementKind.VERTEX)), _hoist_kind_body_1 := (lambda prop_adapters, v1: (lambda _: construct_vertex_coder(cx, g, schema, source, vid_type, eid_type, name, fields, prop_adapters))(v1) if v1 else (lambda _: construct_edge_coder(cx, g, parent_label(), schema, source, vid_type, eid_type, dir(), name, fields, prop_adapters, m_out_spec, m_in_spec))(v1) if v1 else hydra.dsl.python.unsupported("no matching case in inline union elimination")), hydra.lib.eithers.bind(find_property_specs(cx, g, schema, kind, fields), (lambda prop_specs: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: property_adapter(cx, g, schema, v1)), prop_specs), (lambda prop_adapters: _hoist_kind_body_1(prop_adapters, kind))))))[2])))))[5]

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("Expected ", "record type"), ", found: "), "other type")))))

def find_adjacen_edge_adapters(cx: hydra.context.Context, g: hydra.graph.Graph, schema: hydra.pg.mapping.Schema[T0, T1, T2], vid_type: T1, eid_type: T1, parent_label: hydra.pg.model.VertexLabel, dir: hydra.pg.model.Direction, fields: frozenlist[hydra.core.FieldType]) -> Either[hydra.errors.Error, frozenlist[tuple[hydra.pg.model.Direction, tuple[hydra.core.FieldType, tuple[hydra.pg.model.EdgeLabel, hydra.coders.Adapter[hydra.core.Type, hydra.pg.model.ElementTypeTree[T1], hydra.core.Term, hydra.pg.model.ElementTree[T2]]]]]]]:
    r"""Find adjacent edge adapters for a given direction."""

    return hydra.lib.eithers.map((lambda xs: hydra.lib.maybes.cat(xs)), hydra.lib.eithers.map_list((lambda field: (key := (_hoist_key_1 := (lambda v1: (lambda _: schema.annotations.out_edge_label)(v1) if v1 else (lambda _: schema.annotations.in_edge_label)(v1) if v1 else hydra.dsl.python.unsupported("no matching case in inline union elimination")), hydra.core.Name(_hoist_key_1(dir)))[1], hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda a: hydra.lib.eithers.bind(extract_string(cx, g, a), (lambda label_str: hydra.lib.eithers.bind(element_coder(Just((dir, parent_label)), schema, field.type, vid_type, eid_type, cx, g), (lambda elad: Right(Just((dir, (field, (hydra.pg.model.EdgeLabel(label_str), elad)))))))))), hydra.annotations.get_type_annotation(key, field.type)))[1]), fields))

def find_incident_vertex_adapter(cx: hydra.context.Context, g: hydra.graph.Graph, schema: hydra.pg.mapping.Schema[T0, T1, T2], vid_type: T1, eid_type: T1, spec: tuple[hydra.core.FieldType, tuple[hydra.pg.mapping.ValueSpec, Maybe[str]]]) -> Either[hydra.errors.Error, tuple[hydra.core.Name, hydra.coders.Adapter[hydra.core.Type, hydra.pg.model.ElementTypeTree[T1], hydra.core.Term, hydra.pg.model.ElementTree[T2]]]]:
    r"""Find an incident vertex adapter for a projection spec."""

    @lru_cache(1)
    def field() -> hydra.core.FieldType:
        return hydra.lib.pairs.first(spec)
    return hydra.lib.eithers.bind(element_coder(Nothing(), schema, field().type, vid_type, eid_type, cx, g), (lambda adapter: Right((field().name, adapter))))
