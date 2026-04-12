// Note: this is an automatically generated file. Do not edit.

package hydra.encode.pg;

/**
 * Term encoders for hydra.pg.mapping
 */
public interface Mapping {
  static hydra.core.Term annotationSchema(hydra.pg.mapping.AnnotationSchema x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.mapping.AnnotationSchema"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("vertexLabel"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).vertexLabel))),
      new hydra.core.Field(new hydra.core.Name("edgeLabel"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).edgeLabel))),
      new hydra.core.Field(new hydra.core.Name("vertexId"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).vertexId))),
      new hydra.core.Field(new hydra.core.Name("edgeId"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).edgeId))),
      new hydra.core.Field(new hydra.core.Name("propertyKey"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).propertyKey))),
      new hydra.core.Field(new hydra.core.Name("propertyValue"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).propertyValue))),
      new hydra.core.Field(new hydra.core.Name("outVertex"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).outVertex))),
      new hydra.core.Field(new hydra.core.Name("outVertexLabel"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).outVertexLabel))),
      new hydra.core.Field(new hydra.core.Name("inVertex"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).inVertex))),
      new hydra.core.Field(new hydra.core.Name("inVertexLabel"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).inVertexLabel))),
      new hydra.core.Field(new hydra.core.Name("outEdge"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).outEdge))),
      new hydra.core.Field(new hydra.core.Name("outEdgeLabel"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).outEdgeLabel))),
      new hydra.core.Field(new hydra.core.Name("inEdge"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).inEdge))),
      new hydra.core.Field(new hydra.core.Name("inEdgeLabel"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).inEdgeLabel))),
      new hydra.core.Field(new hydra.core.Name("ignore"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).ignore))))));
  }

  static hydra.core.Term edgeSpec(hydra.pg.mapping.EdgeSpec x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.mapping.EdgeSpec"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("label"), hydra.encode.pg.Model.edgeLabel((x).label)),
      new hydra.core.Field(new hydra.core.Name("id"), hydra.encode.pg.Mapping.valueSpec((x).id)),
      new hydra.core.Field(new hydra.core.Name("out"), hydra.encode.pg.Mapping.valueSpec((x).out)),
      new hydra.core.Field(new hydra.core.Name("in"), hydra.encode.pg.Mapping.valueSpec((x).in)),
      new hydra.core.Field(new hydra.core.Name("properties"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.pg.Mapping::propertySpec,
        (x).properties))))));
  }

  static hydra.core.Term elementSpec(hydra.pg.mapping.ElementSpec v1) {
    return (v1).accept(new hydra.pg.mapping.ElementSpec.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.pg.mapping.ElementSpec.Vertex y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.mapping.ElementSpec"), new hydra.core.Field(new hydra.core.Name("vertex"), hydra.encode.pg.Mapping.vertexSpec((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.pg.mapping.ElementSpec.Edge y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.mapping.ElementSpec"), new hydra.core.Field(new hydra.core.Name("edge"), hydra.encode.pg.Mapping.edgeSpec((y).value))));
      }
    });
  }

  static hydra.core.Term propertySpec(hydra.pg.mapping.PropertySpec x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.mapping.PropertySpec"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("key"), hydra.encode.pg.Model.propertyKey((x).key)),
      new hydra.core.Field(new hydra.core.Name("value"), hydra.encode.pg.Mapping.valueSpec((x).value)))));
  }

  static hydra.core.Term valueSpec(hydra.pg.mapping.ValueSpec v1) {
    return (v1).accept(new hydra.pg.mapping.ValueSpec.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.pg.mapping.ValueSpec.Value y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.mapping.ValueSpec"), new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.pg.mapping.ValueSpec.Pattern y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.mapping.ValueSpec"), new hydra.core.Field(new hydra.core.Name("pattern"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((y).value)))));
      }
    });
  }

  static hydra.core.Term vertexSpec(hydra.pg.mapping.VertexSpec x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.mapping.VertexSpec"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("label"), hydra.encode.pg.Model.vertexLabel((x).label)),
      new hydra.core.Field(new hydra.core.Name("id"), hydra.encode.pg.Mapping.valueSpec((x).id)),
      new hydra.core.Field(new hydra.core.Name("properties"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        hydra.encode.pg.Mapping::propertySpec,
        (x).properties))))));
  }
}
