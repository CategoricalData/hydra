// Note: this is an automatically generated file. Do not edit.

package hydra.encode.pg;

/**
 * Term encoders for hydra.pg.model
 */
public interface Model {
  static <T0> hydra.core.Term adjacentEdge(java.util.function.Function<T0, hydra.core.Term> v, hydra.pg.model.AdjacentEdge<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.model.AdjacentEdge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("label"), hydra.encode.pg.Model.edgeLabel(((java.util.function.Function<hydra.pg.model.AdjacentEdge<T0>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("id"), (v).apply(((java.util.function.Function<hydra.pg.model.AdjacentEdge<T0>, T0>) (projected -> projected.id)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("vertex"), (v).apply(((java.util.function.Function<hydra.pg.model.AdjacentEdge<T0>, T0>) (projected -> projected.vertex)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("properties"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.pg.Model::propertyKey,
        v,
        ((java.util.function.Function<hydra.pg.model.AdjacentEdge<T0>, java.util.Map<hydra.pg.model.PropertyKey, T0>>) (projected -> projected.properties)).apply(x)))))));
  }

  static hydra.core.Term direction(hydra.pg.model.Direction v1) {
    return (v1).accept(new hydra.pg.model.Direction.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.pg.model.Direction.Out y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.model.Direction"), new hydra.core.Field(new hydra.core.Name("out"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.pg.model.Direction.In y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.model.Direction"), new hydra.core.Field(new hydra.core.Name("in"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.pg.model.Direction.Both y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.model.Direction"), new hydra.core.Field(new hydra.core.Name("both"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.pg.model.Direction.Undirected y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.model.Direction"), new hydra.core.Field(new hydra.core.Name("undirected"), new hydra.core.Term.Unit())));
      }
    });
  }

  static <T0> hydra.core.Term edge(java.util.function.Function<T0, hydra.core.Term> v, hydra.pg.model.Edge<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.model.Edge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("label"), hydra.encode.pg.Model.edgeLabel(((java.util.function.Function<hydra.pg.model.Edge<T0>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("id"), (v).apply(((java.util.function.Function<hydra.pg.model.Edge<T0>, T0>) (projected -> projected.id)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("out"), (v).apply(((java.util.function.Function<hydra.pg.model.Edge<T0>, T0>) (projected -> projected.out)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("in"), (v).apply(((java.util.function.Function<hydra.pg.model.Edge<T0>, T0>) (projected -> projected.in)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("properties"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.pg.Model::propertyKey,
        v,
        ((java.util.function.Function<hydra.pg.model.Edge<T0>, java.util.Map<hydra.pg.model.PropertyKey, T0>>) (projected -> projected.properties)).apply(x)))))));
  }

  static hydra.core.Term edgeLabel(hydra.pg.model.EdgeLabel x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.pg.model.EdgeLabel"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }

  static <T0> hydra.core.Term edgeType(java.util.function.Function<T0, hydra.core.Term> t, hydra.pg.model.EdgeType<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.model.EdgeType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("label"), hydra.encode.pg.Model.edgeLabel(((java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("id"), (t).apply(((java.util.function.Function<hydra.pg.model.EdgeType<T0>, T0>) (projected -> projected.id)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("out"), hydra.encode.pg.Model.vertexLabel(((java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.out)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("in"), hydra.encode.pg.Model.vertexLabel(((java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.in)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("properties"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.pg.model.PropertyType<T0>, hydra.core.Term>) (v1 -> hydra.encode.pg.Model.<T0>propertyType(
          t,
          v1)),
        ((java.util.function.Function<hydra.pg.model.EdgeType<T0>, java.util.List<hydra.pg.model.PropertyType<T0>>>) (projected -> projected.properties)).apply(x)))))));
  }

  static <T0> hydra.core.Term element(java.util.function.Function<T0, hydra.core.Term> v, hydra.pg.model.Element<T0> v1) {
    return ((java.util.function.Function<hydra.pg.model.Element<T0>, hydra.core.Term>) ((java.util.function.Function<hydra.pg.model.Element<T0>, hydra.core.Term>) (u -> (u).accept(new hydra.pg.model.Element.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.pg.model.Element.Vertex<T0> y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.model.Element"), new hydra.core.Field(new hydra.core.Name("vertex"), hydra.encode.pg.Model.<T0>vertex(
          v,
          (y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.pg.model.Element.Edge<T0> y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.model.Element"), new hydra.core.Field(new hydra.core.Name("edge"), hydra.encode.pg.Model.<T0>edge(
          v,
          (y).value))));
      }
    })))).apply(v1);
  }

  static hydra.core.Term elementKind(hydra.pg.model.ElementKind v1) {
    return (v1).accept(new hydra.pg.model.ElementKind.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.pg.model.ElementKind.Vertex y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.model.ElementKind"), new hydra.core.Field(new hydra.core.Name("vertex"), new hydra.core.Term.Unit())));
      }

      @Override
      public hydra.core.Term visit(hydra.pg.model.ElementKind.Edge y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.model.ElementKind"), new hydra.core.Field(new hydra.core.Name("edge"), new hydra.core.Term.Unit())));
      }
    });
  }

  static <T0> hydra.core.Term elementTree(java.util.function.Function<T0, hydra.core.Term> v, hydra.pg.model.ElementTree<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.model.ElementTree"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("self"), hydra.encode.pg.Model.<T0>element(
        v,
        ((java.util.function.Function<hydra.pg.model.ElementTree<T0>, hydra.pg.model.Element<T0>>) (projected -> projected.self)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("dependencies"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.pg.model.ElementTree<T0>, hydra.core.Term>) (v1 -> hydra.encode.pg.Model.<T0>elementTree(
          v,
          v1)),
        ((java.util.function.Function<hydra.pg.model.ElementTree<T0>, java.util.List<hydra.pg.model.ElementTree<T0>>>) (projected -> projected.dependencies)).apply(x)))))));
  }

  static <T0> hydra.core.Term elementType(java.util.function.Function<T0, hydra.core.Term> t, hydra.pg.model.ElementType<T0> v1) {
    return ((java.util.function.Function<hydra.pg.model.ElementType<T0>, hydra.core.Term>) ((java.util.function.Function<hydra.pg.model.ElementType<T0>, hydra.core.Term>) (u -> (u).accept(new hydra.pg.model.ElementType.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.pg.model.ElementType.Vertex<T0> y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.model.ElementType"), new hydra.core.Field(new hydra.core.Name("vertex"), hydra.encode.pg.Model.<T0>vertexType(
          t,
          (y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.pg.model.ElementType.Edge<T0> y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.model.ElementType"), new hydra.core.Field(new hydra.core.Name("edge"), hydra.encode.pg.Model.<T0>edgeType(
          t,
          (y).value))));
      }
    })))).apply(v1);
  }

  static <T0> hydra.core.Term elementTypeTree(java.util.function.Function<T0, hydra.core.Term> t, hydra.pg.model.ElementTypeTree<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.model.ElementTypeTree"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("self"), hydra.encode.pg.Model.<T0>elementType(
        t,
        ((java.util.function.Function<hydra.pg.model.ElementTypeTree<T0>, hydra.pg.model.ElementType<T0>>) (projected -> projected.self)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("dependencies"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.pg.model.ElementTypeTree<T0>, hydra.core.Term>) (v1 -> hydra.encode.pg.Model.<T0>elementTypeTree(
          t,
          v1)),
        ((java.util.function.Function<hydra.pg.model.ElementTypeTree<T0>, java.util.List<hydra.pg.model.ElementTypeTree<T0>>>) (projected -> projected.dependencies)).apply(x)))))));
  }

  static <T0> hydra.core.Term graph(java.util.function.Function<T0, hydra.core.Term> v, hydra.pg.model.Graph<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.model.Graph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("vertices"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        v,
        (java.util.function.Function<hydra.pg.model.Vertex<T0>, hydra.core.Term>) (v1 -> hydra.encode.pg.Model.<T0>vertex(
          v,
          v1)),
        ((java.util.function.Function<hydra.pg.model.Graph<T0>, java.util.Map<T0, hydra.pg.model.Vertex<T0>>>) (projected -> projected.vertices)).apply(x)))),
      new hydra.core.Field(new hydra.core.Name("edges"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        v,
        (java.util.function.Function<hydra.pg.model.Edge<T0>, hydra.core.Term>) (v1 -> hydra.encode.pg.Model.<T0>edge(
          v,
          v1)),
        ((java.util.function.Function<hydra.pg.model.Graph<T0>, java.util.Map<T0, hydra.pg.model.Edge<T0>>>) (projected -> projected.edges)).apply(x)))))));
  }

  static <T0> hydra.core.Term graphSchema(java.util.function.Function<T0, hydra.core.Term> t, hydra.pg.model.GraphSchema<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.model.GraphSchema"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("vertices"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.pg.Model::vertexLabel,
        (java.util.function.Function<hydra.pg.model.VertexType<T0>, hydra.core.Term>) (v1 -> hydra.encode.pg.Model.<T0>vertexType(
          t,
          v1)),
        ((java.util.function.Function<hydra.pg.model.GraphSchema<T0>, java.util.Map<hydra.pg.model.VertexLabel, hydra.pg.model.VertexType<T0>>>) (projected -> projected.vertices)).apply(x)))),
      new hydra.core.Field(new hydra.core.Name("edges"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.pg.Model::edgeLabel,
        (java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.core.Term>) (v1 -> hydra.encode.pg.Model.<T0>edgeType(
          t,
          v1)),
        ((java.util.function.Function<hydra.pg.model.GraphSchema<T0>, java.util.Map<hydra.pg.model.EdgeLabel, hydra.pg.model.EdgeType<T0>>>) (projected -> projected.edges)).apply(x)))))));
  }

  static hydra.core.Term label(hydra.pg.model.Label v1) {
    return (v1).accept(new hydra.pg.model.Label.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.pg.model.Label.Vertex y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.model.Label"), new hydra.core.Field(new hydra.core.Name("vertex"), hydra.encode.pg.Model.vertexLabel((y).value))));
      }

      @Override
      public hydra.core.Term visit(hydra.pg.model.Label.Edge y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.pg.model.Label"), new hydra.core.Field(new hydra.core.Name("edge"), hydra.encode.pg.Model.edgeLabel((y).value))));
      }
    });
  }

  static <T0> hydra.core.Term lazyGraph(java.util.function.Function<T0, hydra.core.Term> v, hydra.pg.model.LazyGraph<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.model.LazyGraph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("vertices"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.pg.model.Vertex<T0>, hydra.core.Term>) (v1 -> hydra.encode.pg.Model.<T0>vertex(
          v,
          v1)),
        ((java.util.function.Function<hydra.pg.model.LazyGraph<T0>, java.util.List<hydra.pg.model.Vertex<T0>>>) (projected -> projected.vertices)).apply(x)))),
      new hydra.core.Field(new hydra.core.Name("edges"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.pg.model.Edge<T0>, hydra.core.Term>) (v1 -> hydra.encode.pg.Model.<T0>edge(
          v,
          v1)),
        ((java.util.function.Function<hydra.pg.model.LazyGraph<T0>, java.util.List<hydra.pg.model.Edge<T0>>>) (projected -> projected.edges)).apply(x)))))));
  }

  static <T0> hydra.core.Term property(java.util.function.Function<T0, hydra.core.Term> v, hydra.pg.model.Property<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.model.Property"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("key"), hydra.encode.pg.Model.propertyKey(((java.util.function.Function<hydra.pg.model.Property<T0>, hydra.pg.model.PropertyKey>) (projected -> projected.key)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("value"), (v).apply(((java.util.function.Function<hydra.pg.model.Property<T0>, T0>) (projected -> projected.value)).apply(x))))));
  }

  static hydra.core.Term propertyKey(hydra.pg.model.PropertyKey x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.pg.model.PropertyKey"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }

  static <T0> hydra.core.Term propertyType(java.util.function.Function<T0, hydra.core.Term> t, hydra.pg.model.PropertyType<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.model.PropertyType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("key"), hydra.encode.pg.Model.propertyKey(((java.util.function.Function<hydra.pg.model.PropertyType<T0>, hydra.pg.model.PropertyKey>) (projected -> projected.key)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("value"), (t).apply(((java.util.function.Function<hydra.pg.model.PropertyType<T0>, T0>) (projected -> projected.value)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("required"), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(((java.util.function.Function<hydra.pg.model.PropertyType<T0>, Boolean>) (projected -> projected.required)).apply(x)))))));
  }

  static <T0> hydra.core.Term vertex(java.util.function.Function<T0, hydra.core.Term> v, hydra.pg.model.Vertex<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.model.Vertex"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("label"), hydra.encode.pg.Model.vertexLabel(((java.util.function.Function<hydra.pg.model.Vertex<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("id"), (v).apply(((java.util.function.Function<hydra.pg.model.Vertex<T0>, T0>) (projected -> projected.id)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("properties"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
        hydra.encode.pg.Model::propertyKey,
        v,
        ((java.util.function.Function<hydra.pg.model.Vertex<T0>, java.util.Map<hydra.pg.model.PropertyKey, T0>>) (projected -> projected.properties)).apply(x)))))));
  }

  static hydra.core.Term vertexLabel(hydra.pg.model.VertexLabel x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.pg.model.VertexLabel"), new hydra.core.Term.Literal(new hydra.core.Literal.String_((x).value))));
  }

  static <T0> hydra.core.Term vertexType(java.util.function.Function<T0, hydra.core.Term> t, hydra.pg.model.VertexType<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.model.VertexType"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("label"), hydra.encode.pg.Model.vertexLabel(((java.util.function.Function<hydra.pg.model.VertexType<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("id"), (t).apply(((java.util.function.Function<hydra.pg.model.VertexType<T0>, T0>) (projected -> projected.id)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("properties"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.pg.model.PropertyType<T0>, hydra.core.Term>) (v1 -> hydra.encode.pg.Model.<T0>propertyType(
          t,
          v1)),
        ((java.util.function.Function<hydra.pg.model.VertexType<T0>, java.util.List<hydra.pg.model.PropertyType<T0>>>) (projected -> projected.properties)).apply(x)))))));
  }

  static <T0> hydra.core.Term vertexWithAdjacentEdges(java.util.function.Function<T0, hydra.core.Term> v, hydra.pg.model.VertexWithAdjacentEdges<T0> x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.pg.model.VertexWithAdjacentEdges"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("vertex"), hydra.encode.pg.Model.<T0>vertex(
        v,
        ((java.util.function.Function<hydra.pg.model.VertexWithAdjacentEdges<T0>, hydra.pg.model.Vertex<T0>>) (projected -> projected.vertex)).apply(x))),
      new hydra.core.Field(new hydra.core.Name("ins"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.pg.model.AdjacentEdge<T0>, hydra.core.Term>) (v1 -> hydra.encode.pg.Model.<T0>adjacentEdge(
          v,
          v1)),
        ((java.util.function.Function<hydra.pg.model.VertexWithAdjacentEdges<T0>, java.util.List<hydra.pg.model.AdjacentEdge<T0>>>) (projected -> projected.ins)).apply(x)))),
      new hydra.core.Field(new hydra.core.Name("outs"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.pg.model.AdjacentEdge<T0>, hydra.core.Term>) (v1 -> hydra.encode.pg.Model.<T0>adjacentEdge(
          v,
          v1)),
        ((java.util.function.Function<hydra.pg.model.VertexWithAdjacentEdges<T0>, java.util.List<hydra.pg.model.AdjacentEdge<T0>>>) (projected -> projected.outs)).apply(x)))))));
  }
}
