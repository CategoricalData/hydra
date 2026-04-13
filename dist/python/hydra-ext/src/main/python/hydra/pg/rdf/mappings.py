# Note: this is an automatically generated file. Do not edit.

r"""Mappings from property graph schemas to SHACL shapes graphs, and from property graph data to RDF graphs."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Nothing, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.pairs
import hydra.lib.sets
import hydra.pg.model
import hydra.pg.rdf.environment
import hydra.rdf.syntax
import hydra.rdf.utils
import hydra.shacl.model

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def edge_types_to_property_shapes(encode_vertex_label: T0, encode_edge_label: Callable[[hydra.pg.model.EdgeLabel], hydra.rdf.syntax.Iri], vertex_label: hydra.pg.model.VertexLabel, edge_types: frozenlist[hydra.pg.model.EdgeType[T1]]) -> frozenlist[hydra.shacl.model.CommonConstraint]:
    r"""Convert edge types into property shape constraints for a given vertex label."""

    return hydra.lib.lists.concat(hydra.lib.lists.map((lambda et: (out_label := et.out, matches_vertex := hydra.lib.equality.equal(out_label.value, vertex_label.value), edge_shape := cast(hydra.shacl.model.CommonConstraint, hydra.shacl.model.CommonConstraintProperty(hydra.lib.sets.singleton(cast(hydra.shacl.model.Reference, hydra.shacl.model.ReferenceAnonymous(hydra.shacl.model.PropertyShape(hydra.shacl.model.CommonProperties(hydra.lib.sets.singleton(cast(hydra.shacl.model.CommonConstraint, hydra.shacl.model.CommonConstraintClass(hydra.lib.sets.singleton(hydra.rdf.syntax.RdfsClass(None))))), Nothing(), hydra.rdf.utils.empty_lang_strings(), hydra.shacl.model.Severity.VIOLATION, hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty()), hydra.lib.sets.empty(), Nothing(), hydra.rdf.utils.empty_lang_strings(), hydra.rdf.utils.empty_lang_strings(), Nothing(), encode_edge_label(et.label))))))), hydra.lib.logic.if_else(matches_vertex, (lambda : (edge_shape,)), (lambda : ())))[3]), edge_types))

def encode_edge(env: hydra.pg.rdf.environment.PgRdfEnvironment[T0], edge: hydra.pg.model.Edge[T0]) -> hydra.rdf.syntax.Description:
    r"""Encode a property graph edge as an RDF description."""

    @lru_cache(1)
    def elab() -> hydra.pg.model.EdgeLabel:
        return edge.label
    @lru_cache(1)
    def eout() -> T0:
        return edge.out
    @lru_cache(1)
    def ein() -> T0:
        return edge.in_
    @lru_cache(1)
    def subj() -> hydra.rdf.syntax.Resource:
        return cast(hydra.rdf.syntax.Resource, hydra.rdf.syntax.ResourceIri(env.encode_vertex_id(eout())))
    @lru_cache(1)
    def obj() -> hydra.rdf.syntax.Node_:
        return cast(hydra.rdf.syntax.Node_, hydra.rdf.syntax.NodeIri(env.encode_vertex_id(ein())))
    @lru_cache(1)
    def pred() -> hydra.rdf.syntax.Iri:
        return env.encode_edge_label(elab())
    return hydra.rdf.syntax.Description(hydra.rdf.utils.resource_to_node(subj()), hydra.rdf.syntax.Graph(hydra.lib.sets.singleton(hydra.rdf.syntax.Triple(subj(), pred(), obj()))))

def encode_vertex(env: hydra.pg.rdf.environment.PgRdfEnvironment[T0], vertex: hydra.pg.model.Vertex[T0]) -> hydra.rdf.syntax.Description:
    r"""Encode a property graph vertex as an RDF description."""

    @lru_cache(1)
    def vlab() -> hydra.pg.model.VertexLabel:
        return vertex.label
    @lru_cache(1)
    def vid() -> T0:
        return vertex.id
    @lru_cache(1)
    def vprops() -> FrozenDict[hydra.pg.model.PropertyKey, T0]:
        return vertex.properties
    @lru_cache(1)
    def subj() -> hydra.rdf.syntax.Resource:
        return cast(hydra.rdf.syntax.Resource, hydra.rdf.syntax.ResourceIri(env.encode_vertex_id(vid())))
    @lru_cache(1)
    def rtype() -> hydra.rdf.syntax.Node_:
        return cast(hydra.rdf.syntax.Node_, hydra.rdf.syntax.NodeIri(env.encode_vertex_label(vlab())))
    @lru_cache(1)
    def type_triple() -> hydra.rdf.syntax.Triple:
        return hydra.rdf.syntax.Triple(subj(), hydra.rdf.utils.rdf_iri("type"), rtype())
    @lru_cache(1)
    def prop_triples() -> frozenlist[hydra.rdf.syntax.Triple]:
        return hydra.lib.lists.map((lambda kv: (key := hydra.lib.pairs.first(kv), val := hydra.lib.pairs.second(kv), pred := env.encode_property_key(key), obj := cast(hydra.rdf.syntax.Node_, hydra.rdf.syntax.NodeLiteral(env.encode_property_value(val))), hydra.rdf.syntax.Triple(subj(), pred, obj))[4]), hydra.lib.maps.to_list(vprops()))
    @lru_cache(1)
    def all_triples() -> frozenlist[hydra.rdf.syntax.Triple]:
        return hydra.lib.lists.cons(type_triple(), prop_triples())
    return hydra.rdf.syntax.Description(hydra.rdf.utils.resource_to_node(subj()), hydra.rdf.syntax.Graph(hydra.lib.sets.from_list(all_triples())))

def encode_lazy_graph(env: hydra.pg.rdf.environment.PgRdfEnvironment[T0], lg: hydra.pg.model.LazyGraph[T0]) -> hydra.rdf.syntax.Graph:
    r"""Encode a lazy property graph as an RDF graph."""

    @lru_cache(1)
    def vertex_descs() -> frozenlist[hydra.rdf.syntax.Description]:
        return hydra.lib.lists.map((lambda v1: encode_vertex(env, v1)), lg.vertices)
    @lru_cache(1)
    def edge_descs() -> frozenlist[hydra.rdf.syntax.Description]:
        return hydra.lib.lists.map((lambda v1: encode_edge(env, v1)), lg.edges)
    @lru_cache(1)
    def all_descs() -> frozenlist[hydra.rdf.syntax.Description]:
        return hydra.lib.lists.concat((vertex_descs(), edge_descs()))
    return hydra.rdf.utils.descriptions_to_graph(all_descs())

def property_type_to_property_shape(encode_type: Callable[[T0], hydra.rdf.syntax.Iri], encode_key: Callable[[hydra.pg.model.PropertyKey], hydra.rdf.syntax.Iri], pt: hydra.pg.model.PropertyType[T0]) -> hydra.shacl.model.PropertyShape:
    r"""Convert a property type to a SHACL property shape."""

    @lru_cache(1)
    def key() -> hydra.pg.model.PropertyKey:
        return pt.key
    @lru_cache(1)
    def path() -> hydra.rdf.syntax.Iri:
        return encode_key(key())
    @lru_cache(1)
    def required_() -> bool:
        return pt.required
    @lru_cache(1)
    def dt_iri() -> hydra.rdf.syntax.Iri:
        return encode_type(pt.value)
    @lru_cache(1)
    def constraints() -> frozenset[hydra.shacl.model.CommonConstraint]:
        return hydra.lib.sets.singleton(cast(hydra.shacl.model.CommonConstraint, hydra.shacl.model.CommonConstraintDatatype(dt_iri())))
    @lru_cache(1)
    def prop_constraints() -> frozenset[hydra.shacl.model.PropertyShapeConstraint]:
        return hydra.lib.logic.if_else(required_(), (lambda : hydra.lib.sets.singleton(cast(hydra.shacl.model.PropertyShapeConstraint, hydra.shacl.model.PropertyShapeConstraintMinCount(1)))), (lambda : hydra.lib.sets.empty()))
    return hydra.shacl.model.PropertyShape(hydra.shacl.model.CommonProperties(constraints(), Nothing(), hydra.rdf.utils.empty_lang_strings(), hydra.shacl.model.Severity.VIOLATION, hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty()), prop_constraints(), Nothing(), hydra.rdf.utils.empty_lang_strings(), hydra.rdf.utils.empty_lang_strings(), Nothing(), path())

def vertex_type_to_node_shape(encode_type: Callable[[T0], hydra.rdf.syntax.Iri], encode_label: Callable[[hydra.pg.model.VertexLabel], hydra.rdf.syntax.Iri], encode_key: Callable[[hydra.pg.model.PropertyKey], hydra.rdf.syntax.Iri], vt: hydra.pg.model.VertexType[T0]) -> hydra.shacl.model.Definition[hydra.shacl.model.Shape]:
    r"""Convert a vertex type to a SHACL node shape definition."""

    @lru_cache(1)
    def label() -> hydra.pg.model.VertexLabel:
        return vt.label
    @lru_cache(1)
    def label_iri() -> hydra.rdf.syntax.Iri:
        return encode_label(label())
    @lru_cache(1)
    def prop_types() -> frozenlist[hydra.pg.model.PropertyType[T0]]:
        return vt.properties
    @lru_cache(1)
    def prop_shapes() -> frozenlist[hydra.shacl.model.CommonConstraint]:
        return hydra.lib.lists.map((lambda pt: cast(hydra.shacl.model.CommonConstraint, hydra.shacl.model.CommonConstraintProperty(hydra.lib.sets.singleton(cast(hydra.shacl.model.Reference, hydra.shacl.model.ReferenceAnonymous(property_type_to_property_shape(encode_type, encode_key, pt))))))), prop_types())
    @lru_cache(1)
    def common() -> hydra.shacl.model.CommonProperties:
        return hydra.shacl.model.CommonProperties(hydra.lib.sets.from_list(prop_shapes()), Nothing(), hydra.rdf.utils.empty_lang_strings(), hydra.shacl.model.Severity.VIOLATION, hydra.lib.sets.singleton(hydra.rdf.syntax.RdfsClass(None)), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty())
    return hydra.shacl.model.Definition(label_iri(), cast(hydra.shacl.model.Shape, hydra.shacl.model.ShapeNode(hydra.shacl.model.NodeShape(common()))))

def graph_schema_to_shapes_graph(encode_type: Callable[[T0], hydra.rdf.syntax.Iri], encode_vertex_label: Callable[[hydra.pg.model.VertexLabel], hydra.rdf.syntax.Iri], encode_edge_label: Callable[[hydra.pg.model.EdgeLabel], hydra.rdf.syntax.Iri], encode_key: Callable[[hydra.pg.model.PropertyKey], hydra.rdf.syntax.Iri], schema: hydra.pg.model.GraphSchema[T0]) -> hydra.shacl.model.ShapesGraph:
    r"""Convert a property graph schema to a SHACL shapes graph."""

    @lru_cache(1)
    def vertex_types() -> frozenlist[hydra.pg.model.VertexType[T0]]:
        return hydra.lib.maps.elems(schema.vertices)
    @lru_cache(1)
    def edge_types() -> frozenlist[hydra.pg.model.EdgeType[T0]]:
        return hydra.lib.maps.elems(schema.edges)
    @lru_cache(1)
    def defs() -> frozenlist[hydra.shacl.model.Definition[hydra.shacl.model.Shape]]:
        return hydra.lib.lists.map((lambda vt: (base_def := vertex_type_to_node_shape(encode_type, encode_vertex_label, encode_key, vt), edge_shapes := edge_types_to_property_shapes(encode_vertex_label, encode_edge_label, vt.label, edge_types()), base_shape := base_def.target, base_node := (_hoist_base_node_1 := (lambda v1: (lambda ns: ns)(v1.value) if isinstance(v1, hydra.shacl.model.ShapeNode) else (lambda _: hydra.shacl.model.NodeShape(hydra.shacl.model.CommonProperties(hydra.lib.sets.empty(), Nothing(), hydra.rdf.utils.empty_lang_strings(), hydra.shacl.model.Severity.VIOLATION, hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty())))(v1.value) if isinstance(v1, hydra.shacl.model.ShapeProperty) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), _hoist_base_node_1(base_shape))[1], base_common := base_node.common, merged_constraints := hydra.lib.sets.union(base_common.constraints, hydra.lib.sets.from_list(edge_shapes)), updated_common := hydra.shacl.model.CommonProperties(merged_constraints, base_common.deactivated, base_common.message, base_common.severity, base_common.target_class, base_common.target_node, base_common.target_objects_of, base_common.target_subjects_of), updated_shape := cast(hydra.shacl.model.Shape, hydra.shacl.model.ShapeNode(hydra.shacl.model.NodeShape(updated_common))), hydra.shacl.model.Definition(base_def.iri, updated_shape))[8]), vertex_types())
    return hydra.shacl.model.ShapesGraph(hydra.lib.sets.from_list(defs()))
