// Note: this is an automatically generated file. Do not edit.

package hydra.validate;

/**
 * Validation functions for property graphs
 */
public interface Pg {
  static <T0> hydra.util.Maybe<T0> checkAll(java.util.List<hydra.util.Maybe<T0>> checks) {
    return hydra.lib.lists.MaybeHead.apply(hydra.validate.Pg.<T0>checkAll_errors(checks));
  }

  static <T0> java.util.List<T0> checkAll_errors(java.util.List<hydra.util.Maybe<T0>> checks) {
    return hydra.lib.maybes.Cat.apply(checks);
  }

  static <T0, T1> hydra.util.Maybe<hydra.error.pg.InvalidEdgeError> validateEdge(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<hydra.error.pg.InvalidValueError>>> checkValue, hydra.util.Maybe<java.util.function.Function<T1, hydra.util.Maybe<hydra.pg.model.VertexLabel>>> labelForVertexId, hydra.pg.model.EdgeType<T0> typ, hydra.pg.model.Edge<T1> el) {
    hydra.util.Lazy<hydra.pg.model.EdgeLabel> actual = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Edge<T1>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(el));
    hydra.util.Lazy<hydra.util.Maybe<hydra.error.pg.InvalidEdgeError>> checkId = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.error.pg.InvalidValueError, hydra.error.pg.InvalidEdgeError>) (err -> new hydra.error.pg.InvalidEdgeError.Id(err)),
      (checkValue).apply(((java.util.function.Function<hydra.pg.model.EdgeType<T0>, T0>) (projected -> projected.id)).apply(typ)).apply(((java.util.function.Function<hydra.pg.model.Edge<T1>, T1>) (projected -> projected.id)).apply(el))));
    hydra.util.Lazy<hydra.util.Maybe<hydra.error.pg.InvalidEdgeError>> checkIn = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.Maybe<hydra.error.pg.InvalidEdgeError>) (hydra.util.Maybe.<hydra.error.pg.InvalidEdgeError>nothing()),
      (java.util.function.Function<java.util.function.Function<T1, hydra.util.Maybe<hydra.pg.model.VertexLabel>>, hydra.util.Maybe<hydra.error.pg.InvalidEdgeError>>) (f -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Maybe.just(new hydra.error.pg.InvalidEdgeError.InVertexNotFound()),
        (java.util.function.Function<hydra.pg.model.VertexLabel, hydra.util.Maybe<hydra.error.pg.InvalidEdgeError>>) (label -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (label).value,
            ((java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.in)).apply(typ).value),
          () -> (hydra.util.Maybe<hydra.error.pg.InvalidEdgeError>) (hydra.util.Maybe.<hydra.error.pg.InvalidEdgeError>nothing()),
          () -> hydra.util.Maybe.just(new hydra.error.pg.InvalidEdgeError.InVertexLabel(new hydra.error.pg.WrongVertexLabelError(((java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.in)).apply(typ), label))))),
        (f).apply(((java.util.function.Function<hydra.pg.model.Edge<T1>, T1>) (projected -> projected.in)).apply(el)))),
      labelForVertexId));
    hydra.util.Lazy<hydra.pg.model.EdgeLabel> expected = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(typ));
    hydra.util.Lazy<hydra.util.Maybe<hydra.error.pg.InvalidEdgeError>> checkLabel = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        actual.get().value,
        expected.get().value),
      () -> (hydra.util.Maybe<hydra.error.pg.InvalidEdgeError>) (hydra.util.Maybe.<hydra.error.pg.InvalidEdgeError>nothing()),
      () -> hydra.util.Maybe.just(new hydra.error.pg.InvalidEdgeError.Label(new hydra.error.pg.NoSuchEdgeLabelError(actual.get())))));
    hydra.util.Lazy<hydra.util.Maybe<hydra.error.pg.InvalidEdgeError>> checkOut = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.Maybe<hydra.error.pg.InvalidEdgeError>) (hydra.util.Maybe.<hydra.error.pg.InvalidEdgeError>nothing()),
      (java.util.function.Function<java.util.function.Function<T1, hydra.util.Maybe<hydra.pg.model.VertexLabel>>, hydra.util.Maybe<hydra.error.pg.InvalidEdgeError>>) (f -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Maybe.just(new hydra.error.pg.InvalidEdgeError.OutVertexNotFound()),
        (java.util.function.Function<hydra.pg.model.VertexLabel, hydra.util.Maybe<hydra.error.pg.InvalidEdgeError>>) (label -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (label).value,
            ((java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.out)).apply(typ).value),
          () -> (hydra.util.Maybe<hydra.error.pg.InvalidEdgeError>) (hydra.util.Maybe.<hydra.error.pg.InvalidEdgeError>nothing()),
          () -> hydra.util.Maybe.just(new hydra.error.pg.InvalidEdgeError.OutVertexLabel(new hydra.error.pg.WrongVertexLabelError(((java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.out)).apply(typ), label))))),
        (f).apply(((java.util.function.Function<hydra.pg.model.Edge<T1>, T1>) (projected -> projected.out)).apply(el)))),
      labelForVertexId));
    hydra.util.Lazy<hydra.util.Maybe<hydra.error.pg.InvalidEdgeError>> checkProperties = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.error.pg.InvalidElementPropertyError, hydra.error.pg.InvalidEdgeError>) (err -> new hydra.error.pg.InvalidEdgeError.Property(err)),
      hydra.validate.Pg.<T0, T1>validateProperties(
        checkValue,
        ((java.util.function.Function<hydra.pg.model.EdgeType<T0>, java.util.List<hydra.pg.model.PropertyType<T0>>>) (projected -> projected.properties)).apply(typ),
        ((java.util.function.Function<hydra.pg.model.Edge<T1>, java.util.Map<hydra.pg.model.PropertyKey, T1>>) (projected -> projected.properties)).apply(el))));
    return hydra.validate.Pg.checkAll(java.util.Arrays.asList(
      checkLabel.get(),
      checkId.get(),
      checkProperties.get(),
      checkOut.get(),
      checkIn.get()));
  }

  static <T0, T1> hydra.util.Maybe<hydra.error.pg.InvalidGraphError<T1>> validateGraph(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<hydra.error.pg.InvalidValueError>>> checkValue, hydra.pg.model.GraphSchema<T0> schema, hydra.pg.model.Graph<T1> graph) {
    return hydra.validate.Pg.checkAll(java.util.Arrays.asList(
      hydra.validate.Pg.<T0, T1>validateGraph_checkVertices(
        checkValue,
        graph,
        schema),
      hydra.validate.Pg.<T0, T1>validateGraph_checkEdges(
        checkValue,
        graph,
        schema)));
  }

  static <T0, T1> hydra.util.Maybe<hydra.error.pg.InvalidGraphError<T1>> validateGraph_checkEdge(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<hydra.error.pg.InvalidValueError>>> checkValue, hydra.pg.model.Graph<T1> graph, hydra.pg.model.GraphSchema<T0> schema, hydra.pg.model.Edge<T1> el) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Maybe.just((hydra.error.pg.InvalidGraphError<T1>) (new hydra.error.pg.InvalidGraphError.Edge((hydra.error.pg.InvalidGraphEdgeError<T1>) (new hydra.error.pg.InvalidGraphEdgeError<T1>(((java.util.function.Function<hydra.pg.model.Edge<T1>, T1>) (projected -> projected.id)).apply(el), new hydra.error.pg.InvalidEdgeError.Label(new hydra.error.pg.NoSuchEdgeLabelError(((java.util.function.Function<hydra.pg.model.Edge<T1>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(el)))))))),
      (java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.util.Maybe<hydra.error.pg.InvalidGraphError<T1>>>) (t -> hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.error.pg.InvalidEdgeError, hydra.error.pg.InvalidGraphError<T1>>) (err -> (hydra.error.pg.InvalidGraphError<T1>) (new hydra.error.pg.InvalidGraphError.Edge((hydra.error.pg.InvalidGraphEdgeError<T1>) (new hydra.error.pg.InvalidGraphEdgeError<T1>(((java.util.function.Function<hydra.pg.model.Edge<T1>, T1>) (projected -> projected.id)).apply(el), err))))),
        hydra.validate.Pg.<T0, T1>validateEdge(
          checkValue,
          hydra.validate.Pg.<T1>validateGraph_labelForVertexId(graph),
          t,
          el))),
      hydra.lib.maps.Lookup.apply(
        ((java.util.function.Function<hydra.pg.model.Edge<T1>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(el),
        ((java.util.function.Function<hydra.pg.model.GraphSchema<T0>, java.util.Map<hydra.pg.model.EdgeLabel, hydra.pg.model.EdgeType<T0>>>) (projected -> projected.edges)).apply(schema)));
  }

  static <T0, T1> hydra.util.Maybe<hydra.error.pg.InvalidGraphError<T1>> validateGraph_checkEdges(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<hydra.error.pg.InvalidValueError>>> checkValue, hydra.pg.model.Graph<T1> graph, hydra.pg.model.GraphSchema<T0> schema) {
    return hydra.validate.Pg.checkAll(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.pg.model.Edge<T1>, hydra.util.Maybe<hydra.error.pg.InvalidGraphError<T1>>>) (v1 -> hydra.validate.Pg.<T0, T1>validateGraph_checkEdge(
        checkValue,
        graph,
        schema,
        v1)),
      hydra.lib.maps.Elems.apply(((java.util.function.Function<hydra.pg.model.Graph<T1>, java.util.Map<T1, hydra.pg.model.Edge<T1>>>) (projected -> projected.edges)).apply(graph))));
  }

  static <T0, T1> hydra.util.Maybe<hydra.error.pg.InvalidGraphError<T1>> validateGraph_checkVertex(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<hydra.error.pg.InvalidValueError>>> checkValue, hydra.pg.model.GraphSchema<T0> schema, hydra.pg.model.Vertex<T1> el) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Maybe.just((hydra.error.pg.InvalidGraphError<T1>) (new hydra.error.pg.InvalidGraphError.Vertex((hydra.error.pg.InvalidGraphVertexError<T1>) (new hydra.error.pg.InvalidGraphVertexError<T1>(((java.util.function.Function<hydra.pg.model.Vertex<T1>, T1>) (projected -> projected.id)).apply(el), new hydra.error.pg.InvalidVertexError.Label(new hydra.error.pg.NoSuchVertexLabelError(((java.util.function.Function<hydra.pg.model.Vertex<T1>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(el)))))))),
      (java.util.function.Function<hydra.pg.model.VertexType<T0>, hydra.util.Maybe<hydra.error.pg.InvalidGraphError<T1>>>) (t -> hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.error.pg.InvalidVertexError, hydra.error.pg.InvalidGraphError<T1>>) (err -> (hydra.error.pg.InvalidGraphError<T1>) (new hydra.error.pg.InvalidGraphError.Vertex((hydra.error.pg.InvalidGraphVertexError<T1>) (new hydra.error.pg.InvalidGraphVertexError<T1>(((java.util.function.Function<hydra.pg.model.Vertex<T1>, T1>) (projected -> projected.id)).apply(el), err))))),
        hydra.validate.Pg.<T0, T1>validateVertex(
          checkValue,
          t,
          el))),
      hydra.lib.maps.Lookup.apply(
        ((java.util.function.Function<hydra.pg.model.Vertex<T1>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(el),
        ((java.util.function.Function<hydra.pg.model.GraphSchema<T0>, java.util.Map<hydra.pg.model.VertexLabel, hydra.pg.model.VertexType<T0>>>) (projected -> projected.vertices)).apply(schema)));
  }

  static <T0, T1> hydra.util.Maybe<hydra.error.pg.InvalidGraphError<T1>> validateGraph_checkVertices(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<hydra.error.pg.InvalidValueError>>> checkValue, hydra.pg.model.Graph<T1> graph, hydra.pg.model.GraphSchema<T0> schema) {
    return hydra.validate.Pg.checkAll(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.pg.model.Vertex<T1>, hydra.util.Maybe<hydra.error.pg.InvalidGraphError<T1>>>) (v1 -> hydra.validate.Pg.<T0, T1>validateGraph_checkVertex(
        checkValue,
        schema,
        v1)),
      hydra.lib.maps.Elems.apply(((java.util.function.Function<hydra.pg.model.Graph<T1>, java.util.Map<T1, hydra.pg.model.Vertex<T1>>>) (projected -> projected.vertices)).apply(graph))));
  }

  static <T1> hydra.util.Maybe<java.util.function.Function<T1, hydra.util.Maybe<hydra.pg.model.VertexLabel>>> validateGraph_labelForVertexId(hydra.pg.model.Graph<T1> graph) {
    return hydra.util.Maybe.just((java.util.function.Function<T1, hydra.util.Maybe<hydra.pg.model.VertexLabel>>) (i -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.pg.model.Vertex<T1>, hydra.pg.model.VertexLabel>) (projected -> projected.label),
      hydra.lib.maps.Lookup.apply(
        i,
        ((java.util.function.Function<hydra.pg.model.Graph<T1>, java.util.Map<T1, hydra.pg.model.Vertex<T1>>>) (projected -> projected.vertices)).apply(graph)))));
  }

  static <T0, T1> hydra.util.Maybe<hydra.error.pg.InvalidElementPropertyError> validateProperties(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<hydra.error.pg.InvalidValueError>>> checkValue, java.util.List<hydra.pg.model.PropertyType<T0>> types, java.util.Map<hydra.pg.model.PropertyKey, T1> props) {
    hydra.util.Lazy<hydra.util.Maybe<hydra.error.pg.InvalidElementPropertyError>> checkTypes = new hydra.util.Lazy<>(() -> hydra.validate.Pg.checkAll(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.pg.model.PropertyType<T0>, hydra.util.Maybe<hydra.error.pg.InvalidElementPropertyError>>) (v1 -> hydra.validate.Pg.<T1, T0>validateProperties_checkType(
        props,
        v1)),
      types)));
    hydra.util.Lazy<hydra.util.Maybe<hydra.error.pg.InvalidElementPropertyError>> checkValues = new hydra.util.Lazy<>(() -> hydra.validate.Pg.checkAll(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.pg.model.PropertyKey, T1>, hydra.util.Maybe<hydra.error.pg.InvalidElementPropertyError>>) (v1 -> hydra.validate.Pg.<T0, T1>validateProperties_checkPair(
        checkValue,
        types,
        v1)),
      hydra.lib.maps.ToList.apply(props))));
    return hydra.validate.Pg.checkAll(java.util.Arrays.asList(
      checkTypes.get(),
      checkValues.get()));
  }

  static <T0, T1> hydra.util.Maybe<hydra.error.pg.InvalidElementPropertyError> validateProperties_checkPair(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<hydra.error.pg.InvalidValueError>>> checkValue, java.util.List<hydra.pg.model.PropertyType<T0>> types, hydra.util.Pair<hydra.pg.model.PropertyKey, T1> pair) {
    hydra.util.Lazy<hydra.pg.model.PropertyKey> key = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Maybe.just(new hydra.error.pg.InvalidElementPropertyError(key.get(), new hydra.error.pg.InvalidPropertyError.UnexpectedKey(key.get()))),
      (java.util.function.Function<T0, hydra.util.Maybe<hydra.error.pg.InvalidElementPropertyError>>) (typ -> hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.error.pg.InvalidValueError, hydra.error.pg.InvalidElementPropertyError>) (err -> new hydra.error.pg.InvalidElementPropertyError(key.get(), new hydra.error.pg.InvalidPropertyError.InvalidValue(err))),
        (checkValue).apply(typ).apply(hydra.validate.Pg.<T1>validateProperties_val(pair)))),
      hydra.lib.maps.Lookup.apply(
        key.get(),
        hydra.validate.Pg.<T0>validateProperties_m(types)));
  }

  static <T1, T2> hydra.util.Maybe<hydra.error.pg.InvalidElementPropertyError> validateProperties_checkType(java.util.Map<hydra.pg.model.PropertyKey, T1> props, hydra.pg.model.PropertyType<T2> t) {
    return hydra.lib.logic.IfElse.lazy(
      ((java.util.function.Function<hydra.pg.model.PropertyType<T2>, Boolean>) (projected -> projected.required)).apply(t),
      () -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Maybe.just(new hydra.error.pg.InvalidElementPropertyError(((java.util.function.Function<hydra.pg.model.PropertyType<T2>, hydra.pg.model.PropertyKey>) (projected -> projected.key)).apply(t), new hydra.error.pg.InvalidPropertyError.MissingRequired(((java.util.function.Function<hydra.pg.model.PropertyType<T2>, hydra.pg.model.PropertyKey>) (projected -> projected.key)).apply(t)))),
        (java.util.function.Function<T1, hydra.util.Maybe<hydra.error.pg.InvalidElementPropertyError>>) (ignored -> (hydra.util.Maybe<hydra.error.pg.InvalidElementPropertyError>) (hydra.util.Maybe.<hydra.error.pg.InvalidElementPropertyError>nothing())),
        hydra.lib.maps.Lookup.apply(
          ((java.util.function.Function<hydra.pg.model.PropertyType<T2>, hydra.pg.model.PropertyKey>) (projected -> projected.key)).apply(t),
          props)),
      () -> (hydra.util.Maybe<hydra.error.pg.InvalidElementPropertyError>) (hydra.util.Maybe.<hydra.error.pg.InvalidElementPropertyError>nothing()));
  }

  static <T0> java.util.Map<hydra.pg.model.PropertyKey, T0> validateProperties_m(java.util.List<hydra.pg.model.PropertyType<T0>> types) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.pg.model.PropertyType<T0>, hydra.util.Pair<hydra.pg.model.PropertyKey, T0>>) (p -> (hydra.util.Pair<hydra.pg.model.PropertyKey, T0>) ((hydra.util.Pair<hydra.pg.model.PropertyKey, T0>) (new hydra.util.Pair<hydra.pg.model.PropertyKey, T0>(((java.util.function.Function<hydra.pg.model.PropertyType<T0>, hydra.pg.model.PropertyKey>) (projected -> projected.key)).apply(p), ((java.util.function.Function<hydra.pg.model.PropertyType<T0>, T0>) (projected -> projected.value)).apply(p))))),
      types));
  }

  static <T1> T1 validateProperties_val(hydra.util.Pair<hydra.pg.model.PropertyKey, T1> pair) {
    return hydra.lib.pairs.Second.apply(pair);
  }

  static <T0, T1> hydra.util.Maybe<hydra.error.pg.InvalidVertexError> validateVertex(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<hydra.error.pg.InvalidValueError>>> checkValue, hydra.pg.model.VertexType<T0> typ, hydra.pg.model.Vertex<T1> el) {
    hydra.util.Lazy<hydra.pg.model.VertexLabel> actual = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Vertex<T1>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(el));
    hydra.util.Lazy<hydra.util.Maybe<hydra.error.pg.InvalidVertexError>> checkId = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.error.pg.InvalidValueError, hydra.error.pg.InvalidVertexError>) (err -> new hydra.error.pg.InvalidVertexError.Id(err)),
      (checkValue).apply(((java.util.function.Function<hydra.pg.model.VertexType<T0>, T0>) (projected -> projected.id)).apply(typ)).apply(((java.util.function.Function<hydra.pg.model.Vertex<T1>, T1>) (projected -> projected.id)).apply(el))));
    hydra.util.Lazy<hydra.pg.model.VertexLabel> expected = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.VertexType<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(typ));
    hydra.util.Lazy<hydra.util.Maybe<hydra.error.pg.InvalidVertexError>> checkLabel = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        actual.get().value,
        expected.get().value),
      () -> (hydra.util.Maybe<hydra.error.pg.InvalidVertexError>) (hydra.util.Maybe.<hydra.error.pg.InvalidVertexError>nothing()),
      () -> hydra.util.Maybe.just(new hydra.error.pg.InvalidVertexError.Label(new hydra.error.pg.NoSuchVertexLabelError(actual.get())))));
    hydra.util.Lazy<hydra.util.Maybe<hydra.error.pg.InvalidVertexError>> checkProperties = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.error.pg.InvalidElementPropertyError, hydra.error.pg.InvalidVertexError>) (err -> new hydra.error.pg.InvalidVertexError.Property(err)),
      hydra.validate.Pg.<T0, T1>validateProperties(
        checkValue,
        ((java.util.function.Function<hydra.pg.model.VertexType<T0>, java.util.List<hydra.pg.model.PropertyType<T0>>>) (projected -> projected.properties)).apply(typ),
        ((java.util.function.Function<hydra.pg.model.Vertex<T1>, java.util.Map<hydra.pg.model.PropertyKey, T1>>) (projected -> projected.properties)).apply(el))));
    return hydra.validate.Pg.checkAll(java.util.Arrays.asList(
      checkLabel.get(),
      checkId.get(),
      checkProperties.get()));
  }
}
