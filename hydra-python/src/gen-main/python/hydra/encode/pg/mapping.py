# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.pg.mapping."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.encode.pg.model
import hydra.lib.lists
import hydra.pg.mapping

def annotation_schema(x: hydra.pg.mapping.AnnotationSchema) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.mapping.AnnotationSchema"), (hydra.core.Field(hydra.core.Name("vertexLabel"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.vertex_label))))), hydra.core.Field(hydra.core.Name("edgeLabel"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.edge_label))))), hydra.core.Field(hydra.core.Name("vertexId"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.vertex_id))))), hydra.core.Field(hydra.core.Name("edgeId"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.edge_id))))), hydra.core.Field(hydra.core.Name("propertyKey"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.property_key))))), hydra.core.Field(hydra.core.Name("propertyValue"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.property_value))))), hydra.core.Field(hydra.core.Name("outVertex"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.out_vertex))))), hydra.core.Field(hydra.core.Name("outVertexLabel"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.out_vertex_label))))), hydra.core.Field(hydra.core.Name("inVertex"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.in_vertex))))), hydra.core.Field(hydra.core.Name("inVertexLabel"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.in_vertex_label))))), hydra.core.Field(hydra.core.Name("outEdge"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.out_edge))))), hydra.core.Field(hydra.core.Name("outEdgeLabel"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.out_edge_label))))), hydra.core.Field(hydra.core.Name("inEdge"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.in_edge))))), hydra.core.Field(hydra.core.Name("inEdgeLabel"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.in_edge_label))))), hydra.core.Field(hydra.core.Name("ignore"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.ignore)))))))))

def value_spec(v1: hydra.pg.mapping.ValueSpec) -> hydra.core.Term:
    match v1:
        case hydra.pg.mapping.ValueSpecValue():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.pg.mapping.ValueSpec"), hydra.core.Field(hydra.core.Name("value"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.pg.mapping.ValueSpecPattern(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.pg.mapping.ValueSpec"), hydra.core.Field(hydra.core.Name("pattern"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(y2))))))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def property_spec(x: hydra.pg.mapping.PropertySpec) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.mapping.PropertySpec"), (hydra.core.Field(hydra.core.Name("key"), hydra.encode.pg.model.property_key(x.key)), hydra.core.Field(hydra.core.Name("value"), value_spec(x.value))))))

def edge_spec(x: hydra.pg.mapping.EdgeSpec) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.mapping.EdgeSpec"), (hydra.core.Field(hydra.core.Name("label"), hydra.encode.pg.model.edge_label(x.label)), hydra.core.Field(hydra.core.Name("id"), value_spec(x.id)), hydra.core.Field(hydra.core.Name("out"), value_spec(x.out)), hydra.core.Field(hydra.core.Name("in"), value_spec(x.in_)), hydra.core.Field(hydra.core.Name("properties"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: property_spec(x1)), x.properties))))))))

def vertex_spec(x: hydra.pg.mapping.VertexSpec) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.mapping.VertexSpec"), (hydra.core.Field(hydra.core.Name("label"), hydra.encode.pg.model.vertex_label(x.label)), hydra.core.Field(hydra.core.Name("id"), value_spec(x.id)), hydra.core.Field(hydra.core.Name("properties"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: property_spec(x1)), x.properties))))))))

def element_spec(v1: hydra.pg.mapping.ElementSpec) -> hydra.core.Term:
    match v1:
        case hydra.pg.mapping.ElementSpecVertex(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.pg.mapping.ElementSpec"), hydra.core.Field(hydra.core.Name("vertex"), vertex_spec(y)))))

        case hydra.pg.mapping.ElementSpecEdge(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.pg.mapping.ElementSpec"), hydra.core.Field(hydra.core.Name("edge"), edge_spec(y2)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")
