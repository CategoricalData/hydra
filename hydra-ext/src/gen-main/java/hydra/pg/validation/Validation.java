// Note: this is an automatically generated file. Do not edit.

package hydra.pg.validation;

/**
 * Utilities for validating property graphs against property graph schemas
 */
public interface Validation {
  static <T0, T1> hydra.util.Maybe<String> validateEdge(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<String>>> checkValue, java.util.function.Function<T1, String> showValue, hydra.util.Maybe<java.util.function.Function<T1, hydra.util.Maybe<hydra.pg.model.VertexLabel>>> labelForVertexId, hydra.pg.model.EdgeType<T0> typ, hydra.pg.model.Edge<T1> el) {
    hydra.util.Lazy<hydra.pg.model.EdgeLabel> actual = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Edge<T1>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(el));
    java.util.function.Function<String, String> failWith = (java.util.function.Function<String, String>) (v1 -> hydra.pg.validation.Validation.<T1>edgeError(
      showValue,
      el,
      v1));
    hydra.util.Lazy<hydra.util.Maybe<String>> checkId = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<String, String>) (arg_ -> (failWith).apply(hydra.pg.validation.Validation.prepend(
        "Invalid id",
        arg_))),
      ((checkValue).apply(((java.util.function.Function<hydra.pg.model.EdgeType<T0>, T0>) (projected -> projected.id)).apply(typ))).apply(((java.util.function.Function<hydra.pg.model.Edge<T1>, T1>) (projected -> projected.id)).apply(el))));
    hydra.util.Lazy<hydra.util.Maybe<String>> checkIn = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      (java.util.function.Function<java.util.function.Function<T1, hydra.util.Maybe<hydra.pg.model.VertexLabel>>, hydra.util.Maybe<String>>) (f -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Maybe.just((failWith).apply(hydra.pg.validation.Validation.prepend(
          "In-vertex does not exist",
          (showValue).apply(((java.util.function.Function<hydra.pg.model.Edge<T1>, T1>) (projected -> projected.in)).apply(el))))),
        (java.util.function.Function<hydra.pg.model.VertexLabel, hydra.util.Maybe<String>>) (label -> hydra.pg.validation.Validation.<String>verify(
          hydra.lib.equality.Equal.apply(
            (label).value,
            (((java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.in)).apply(typ)).value),
          (failWith).apply(hydra.pg.validation.Validation.prepend(
            "Wrong in-vertex label",
            hydra.pg.validation.Validation.vertexLabelMismatch(
              ((java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.in)).apply(typ),
              label))))),
        (f).apply(((java.util.function.Function<hydra.pg.model.Edge<T1>, T1>) (projected -> projected.in)).apply(el)))),
      labelForVertexId));
    hydra.util.Lazy<hydra.pg.model.EdgeLabel> expected = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(typ));
    hydra.util.Lazy<hydra.util.Maybe<String>> checkLabel = new hydra.util.Lazy<>(() -> hydra.pg.validation.Validation.<String>verify(
      hydra.lib.equality.Equal.apply(
        (actual.get()).value,
        (expected.get()).value),
      (failWith).apply(hydra.pg.validation.Validation.prepend(
        "Wrong label",
        hydra.pg.validation.Validation.edgeLabelMismatch(
          expected.get(),
          actual.get())))));
    hydra.util.Lazy<hydra.util.Maybe<String>> checkOut = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()),
      (java.util.function.Function<java.util.function.Function<T1, hydra.util.Maybe<hydra.pg.model.VertexLabel>>, hydra.util.Maybe<String>>) (f -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Maybe.just((failWith).apply(hydra.pg.validation.Validation.prepend(
          "Out-vertex does not exist",
          (showValue).apply(((java.util.function.Function<hydra.pg.model.Edge<T1>, T1>) (projected -> projected.out)).apply(el))))),
        (java.util.function.Function<hydra.pg.model.VertexLabel, hydra.util.Maybe<String>>) (label -> hydra.pg.validation.Validation.<String>verify(
          hydra.lib.equality.Equal.apply(
            (label).value,
            (((java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.out)).apply(typ)).value),
          (failWith).apply(hydra.pg.validation.Validation.prepend(
            "Wrong out-vertex label",
            hydra.pg.validation.Validation.vertexLabelMismatch(
              ((java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.out)).apply(typ),
              label))))),
        (f).apply(((java.util.function.Function<hydra.pg.model.Edge<T1>, T1>) (projected -> projected.out)).apply(el)))),
      labelForVertexId));
    hydra.util.Lazy<hydra.util.Maybe<String>> checkProperties = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<String, String>) (arg_ -> (failWith).apply(hydra.pg.validation.Validation.prepend(
        "Invalid property",
        arg_))),
      hydra.pg.validation.Validation.<T0, T1>validateProperties(
        checkValue,
        ((java.util.function.Function<hydra.pg.model.EdgeType<T0>, java.util.List<hydra.pg.model.PropertyType<T0>>>) (projected -> projected.properties)).apply(typ),
        ((java.util.function.Function<hydra.pg.model.Edge<T1>, java.util.Map<hydra.pg.model.PropertyKey, T1>>) (projected -> projected.properties)).apply(el))));
    return hydra.pg.validation.Validation.<String>checkAll(java.util.List.of(
      checkLabel.get(),
      checkId.get(),
      checkProperties.get(),
      checkOut.get(),
      checkIn.get()));
  }
  
  static <T0, T1> hydra.util.Maybe<String> validateElement(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<String>>> checkValue, java.util.function.Function<T1, String> showValue, hydra.util.Maybe<java.util.function.Function<T1, hydra.util.Maybe<hydra.pg.model.VertexLabel>>> labelForVertexId, hydra.pg.model.ElementType<T0> typ, hydra.pg.model.Element<T1> el) {
    return ((java.util.function.Function<hydra.pg.model.ElementType, hydra.util.Maybe<String>>) (v1 -> ((java.util.function.Function<hydra.pg.model.ElementType<T0>, hydra.util.Maybe<String>>) ((java.util.function.Function<hydra.pg.model.ElementType<T0>, hydra.util.Maybe<String>>) (u -> (u).accept(new hydra.pg.model.ElementType.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<String> visit(hydra.pg.model.ElementType.Vertex<T0> vt) {
        return ((java.util.function.Function<hydra.pg.model.Element, hydra.util.Maybe<String>>) (v12 -> ((java.util.function.Function<hydra.pg.model.Element<T1>, hydra.util.Maybe<String>>) ((java.util.function.Function<hydra.pg.model.Element<T1>, hydra.util.Maybe<String>>) (u -> (u).accept(new hydra.pg.model.Element.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<String> visit(hydra.pg.model.Element.Edge<T1> e) {
            return hydra.util.Maybe.just(hydra.pg.validation.Validation.prepend(
              "Edge instead of vertex",
              (showValue).apply(((java.util.function.Function<hydra.pg.model.Edge<T1>, T1>) (projected -> projected.id)).apply((e).value))));
          }
          
          @Override
          public hydra.util.Maybe<String> visit(hydra.pg.model.Element.Vertex<T1> vertex) {
            return hydra.pg.validation.Validation.<T0, T1>validateVertex(
              checkValue,
              showValue,
              (vt).value,
              (vertex).value);
          }
        })))).apply(v12))).apply(el);
      }
      
      @Override
      public hydra.util.Maybe<String> visit(hydra.pg.model.ElementType.Edge<T0> et) {
        return ((java.util.function.Function<hydra.pg.model.Element, hydra.util.Maybe<String>>) (v12 -> ((java.util.function.Function<hydra.pg.model.Element<T1>, hydra.util.Maybe<String>>) ((java.util.function.Function<hydra.pg.model.Element<T1>, hydra.util.Maybe<String>>) (u -> (u).accept(new hydra.pg.model.Element.PartialVisitor<>() {
          @Override
          public hydra.util.Maybe<String> visit(hydra.pg.model.Element.Vertex<T1> v) {
            return hydra.util.Maybe.just(hydra.pg.validation.Validation.prepend(
              "Vertex instead of edge",
              (showValue).apply(((java.util.function.Function<hydra.pg.model.Vertex<T1>, T1>) (projected -> projected.id)).apply((v).value))));
          }
          
          @Override
          public hydra.util.Maybe<String> visit(hydra.pg.model.Element.Edge<T1> edge) {
            return hydra.pg.validation.Validation.<T0, T1>validateEdge(
              checkValue,
              showValue,
              labelForVertexId,
              (et).value,
              (edge).value);
          }
        })))).apply(v12))).apply(el);
      }
    })))).apply(v1))).apply(typ);
  }
  
  static <T0, T1> hydra.util.Maybe<String> validateGraph(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<String>>> checkValue, java.util.function.Function<T1, String> showValue, hydra.pg.model.GraphSchema<T0> schema, hydra.pg.model.Graph<T1> graph) {
    hydra.util.Lazy<hydra.util.Maybe<String>> checkEdges = new hydra.util.Lazy<>(() -> hydra.pg.validation.Validation.<String>checkAll(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.pg.model.Edge<T1>, hydra.util.Maybe<String>>) (v1 -> hydra.pg.validation.Validation.<T0, T1>validateGraph_checkEdge(
        checkValue,
        graph,
        (java.util.function.Function<String, java.util.function.Function<String, String>>) (p0 -> p1 -> hydra.pg.validation.Validation.prepend(
          p0,
          p1)),
        schema,
        showValue,
        v1)),
      hydra.lib.maps.Elems.apply(((java.util.function.Function<hydra.pg.model.Graph<T1>, java.util.Map<T1, hydra.pg.model.Edge<T1>>>) (projected -> projected.edges)).apply(graph)))));
    hydra.util.Lazy<hydra.util.Maybe<String>> checkVertices = new hydra.util.Lazy<>(() -> hydra.pg.validation.Validation.<String>checkAll(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.pg.model.Vertex<T1>, hydra.util.Maybe<String>>) (v1 -> hydra.pg.validation.Validation.<T0, T1>validateGraph_checkVertex(
        checkValue,
        (java.util.function.Function<String, java.util.function.Function<String, String>>) (p0 -> p1 -> hydra.pg.validation.Validation.prepend(
          p0,
          p1)),
        schema,
        showValue,
        v1)),
      hydra.lib.maps.Elems.apply(((java.util.function.Function<hydra.pg.model.Graph<T1>, java.util.Map<T1, hydra.pg.model.Vertex<T1>>>) (projected -> projected.vertices)).apply(graph)))));
    return hydra.pg.validation.Validation.<String>checkAll(java.util.List.of(
      checkVertices.get(),
      checkEdges.get()));
  }
  
  static <T0, T1> hydra.util.Maybe<String> validateGraph_checkVertex(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<String>>> checkValue, java.util.function.Function<String, java.util.function.Function<String, String>> hydra_pg_validation_prepend2, hydra.pg.model.GraphSchema<T0> schema, java.util.function.Function<T1, String> showValue, hydra.pg.model.Vertex<T1> el) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Maybe.just(hydra.pg.validation.Validation.<T1>vertexError(
        showValue,
        el,
        ((hydra_pg_validation_prepend2).apply("Unexpected label")).apply((((java.util.function.Function<hydra.pg.model.Vertex<T1>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(el)).value))),
      (java.util.function.Function<hydra.pg.model.VertexType<T0>, hydra.util.Maybe<String>>) (t -> hydra.pg.validation.Validation.<T0, T1>validateVertex(
        checkValue,
        showValue,
        t,
        el)),
      hydra.lib.maps.Lookup.apply(
        ((java.util.function.Function<hydra.pg.model.Vertex<T1>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(el),
        ((java.util.function.Function<hydra.pg.model.GraphSchema<T0>, java.util.Map<hydra.pg.model.VertexLabel, hydra.pg.model.VertexType<T0>>>) (projected -> projected.vertices)).apply(schema)));
  }
  
  static <T0, T1> hydra.util.Maybe<String> validateGraph_checkEdge(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<String>>> checkValue, hydra.pg.model.Graph<T1> graph, java.util.function.Function<String, java.util.function.Function<String, String>> hydra_pg_validation_prepend2, hydra.pg.model.GraphSchema<T0> schema, java.util.function.Function<T1, String> showValue, hydra.pg.model.Edge<T1> el) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Maybe.just(hydra.pg.validation.Validation.<T1>edgeError(
        showValue,
        el,
        ((hydra_pg_validation_prepend2).apply("Unexpected label")).apply((((java.util.function.Function<hydra.pg.model.Edge<T1>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(el)).value))),
      (java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.util.Maybe<String>>) (t -> hydra.pg.validation.Validation.<T0, T1>validateEdge(
        checkValue,
        showValue,
        hydra.pg.validation.Validation.<T1>validateGraph_labelForVertexId(graph),
        t,
        el)),
      hydra.lib.maps.Lookup.apply(
        ((java.util.function.Function<hydra.pg.model.Edge<T1>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(el),
        ((java.util.function.Function<hydra.pg.model.GraphSchema<T0>, java.util.Map<hydra.pg.model.EdgeLabel, hydra.pg.model.EdgeType<T0>>>) (projected -> projected.edges)).apply(schema)));
  }
  
  static <T1> hydra.util.Maybe<java.util.function.Function<T1, hydra.util.Maybe<hydra.pg.model.VertexLabel>>> validateGraph_labelForVertexId(hydra.pg.model.Graph<T1> graph) {
    return hydra.util.Maybe.just((java.util.function.Function<T1, hydra.util.Maybe<hydra.pg.model.VertexLabel>>) (i -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<hydra.pg.model.Vertex<T1>, hydra.pg.model.VertexLabel>) (projected -> projected.label),
      hydra.lib.maps.Lookup.apply(
        i,
        ((java.util.function.Function<hydra.pg.model.Graph<T1>, java.util.Map<T1, hydra.pg.model.Vertex<T1>>>) (projected -> projected.vertices)).apply(graph)))));
  }
  
  static <T0, T1> hydra.util.Maybe<String> validateProperties(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<String>>> checkValue, java.util.List<hydra.pg.model.PropertyType<T0>> types, java.util.Map<hydra.pg.model.PropertyKey, T1> props) {
    hydra.util.Lazy<hydra.util.Maybe<String>> checkTypes = new hydra.util.Lazy<>(() -> hydra.pg.validation.Validation.<String>checkAll(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.pg.model.PropertyType<T0>, hydra.util.Maybe<String>>) (v1 -> hydra.pg.validation.Validation.<T1, T0>validateProperties_checkType(
        (java.util.function.Function<String, java.util.function.Function<String, String>>) (p0 -> p1 -> hydra.pg.validation.Validation.prepend(
          p0,
          p1)),
        props,
        v1)),
      types)));
    hydra.util.Lazy<hydra.util.Maybe<String>> checkValues = new hydra.util.Lazy<>(() -> hydra.pg.validation.Validation.<String>checkAll(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.pg.model.PropertyKey, T1>, hydra.util.Maybe<String>>) (v1 -> hydra.pg.validation.Validation.<T0, T1>validateProperties_checkPair(
        checkValue,
        (java.util.function.Function<String, java.util.function.Function<String, String>>) (p0 -> p1 -> hydra.pg.validation.Validation.prepend(
          p0,
          p1)),
        types,
        v1)),
      hydra.lib.maps.ToList.apply(props))));
    return hydra.pg.validation.Validation.<String>checkAll(java.util.List.of(
      checkTypes.get(),
      checkValues.get()));
  }
  
  static <T1, T2> hydra.util.Maybe<String> validateProperties_checkType(java.util.function.Function<String, java.util.function.Function<String, String>> hydra_pg_validation_prepend2, java.util.Map<hydra.pg.model.PropertyKey, T1> props, hydra.pg.model.PropertyType<T2> t) {
    return hydra.lib.logic.IfElse.lazy(
      ((java.util.function.Function<hydra.pg.model.PropertyType<T2>, Boolean>) (projected -> projected.required)).apply(t),
      () -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Maybe.just(((hydra_pg_validation_prepend2).apply("Missing value for ")).apply((((java.util.function.Function<hydra.pg.model.PropertyType<T2>, hydra.pg.model.PropertyKey>) (projected -> projected.key)).apply(t)).value)),
        (java.util.function.Function<T1, hydra.util.Maybe<String>>) (ignored -> (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
        hydra.lib.maps.Lookup.apply(
          ((java.util.function.Function<hydra.pg.model.PropertyType<T2>, hydra.pg.model.PropertyKey>) (projected -> projected.key)).apply(t),
          props)),
      () -> (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()));
  }
  
  static <T0> java.util.Map<hydra.pg.model.PropertyKey, T0> validateProperties_m(java.util.List<hydra.pg.model.PropertyType<T0>> types) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.pg.model.PropertyType<T0>, hydra.util.Pair<hydra.pg.model.PropertyKey, T0>>) (p -> (hydra.util.Pair<hydra.pg.model.PropertyKey, T0>) ((hydra.util.Pair<hydra.pg.model.PropertyKey, T0>) (new hydra.util.Pair<hydra.pg.model.PropertyKey, T0>(((java.util.function.Function<hydra.pg.model.PropertyType<T0>, hydra.pg.model.PropertyKey>) (projected -> projected.key)).apply(p), ((java.util.function.Function<hydra.pg.model.PropertyType<T0>, T0>) (projected -> projected.value)).apply(p))))),
      types));
  }
  
  static <T0, T1> hydra.util.Maybe<String> validateProperties_checkPair(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<String>>> checkValue, java.util.function.Function<String, java.util.function.Function<String, String>> hydra_pg_validation_prepend2, java.util.List<hydra.pg.model.PropertyType<T0>> types, hydra.util.Pair<hydra.pg.model.PropertyKey, T1> pair) {
    hydra.util.Lazy<hydra.pg.model.PropertyKey> key = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair));
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Maybe.just(((hydra_pg_validation_prepend2).apply("Unexpected key")).apply((key.get()).value)),
      (java.util.function.Function<T0, hydra.util.Maybe<String>>) (typ -> hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, String>) (v1 -> ((hydra_pg_validation_prepend2).apply("Invalid value")).apply(v1)),
        ((checkValue).apply(typ)).apply(hydra.pg.validation.Validation.<T1>validateProperties_val(pair)))),
      hydra.lib.maps.Lookup.apply(
        key.get(),
        hydra.pg.validation.Validation.<T0>validateProperties_m(types)));
  }
  
  static <T1> T1 validateProperties_val(hydra.util.Pair<hydra.pg.model.PropertyKey, T1> pair) {
    return hydra.lib.pairs.Second.apply(pair);
  }
  
  static <T0, T1> hydra.util.Maybe<String> validateVertex(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Maybe<String>>> checkValue, java.util.function.Function<T1, String> showValue, hydra.pg.model.VertexType<T0> typ, hydra.pg.model.Vertex<T1> el) {
    hydra.util.Lazy<hydra.pg.model.VertexLabel> actual = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Vertex<T1>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(el));
    java.util.function.Function<String, String> failWith = (java.util.function.Function<String, String>) (v1 -> hydra.pg.validation.Validation.<T1>vertexError(
      showValue,
      el,
      v1));
    hydra.util.Lazy<hydra.util.Maybe<String>> checkId = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<String, String>) (arg_ -> (failWith).apply(hydra.pg.validation.Validation.prepend(
        "Invalid id",
        arg_))),
      ((checkValue).apply(((java.util.function.Function<hydra.pg.model.VertexType<T0>, T0>) (projected -> projected.id)).apply(typ))).apply(((java.util.function.Function<hydra.pg.model.Vertex<T1>, T1>) (projected -> projected.id)).apply(el))));
    hydra.util.Lazy<hydra.pg.model.VertexLabel> expected = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.VertexType<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(typ));
    hydra.util.Lazy<hydra.util.Maybe<String>> checkLabel = new hydra.util.Lazy<>(() -> hydra.pg.validation.Validation.<String>verify(
      hydra.lib.equality.Equal.apply(
        (actual.get()).value,
        (expected.get()).value),
      (failWith).apply(hydra.pg.validation.Validation.prepend(
        "Wrong label",
        hydra.pg.validation.Validation.vertexLabelMismatch(
          expected.get(),
          actual.get())))));
    hydra.util.Lazy<hydra.util.Maybe<String>> checkProperties = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
      (java.util.function.Function<String, String>) (arg_ -> (failWith).apply(hydra.pg.validation.Validation.prepend(
        "Invalid property",
        arg_))),
      hydra.pg.validation.Validation.<T0, T1>validateProperties(
        checkValue,
        ((java.util.function.Function<hydra.pg.model.VertexType<T0>, java.util.List<hydra.pg.model.PropertyType<T0>>>) (projected -> projected.properties)).apply(typ),
        ((java.util.function.Function<hydra.pg.model.Vertex<T1>, java.util.Map<hydra.pg.model.PropertyKey, T1>>) (projected -> projected.properties)).apply(el))));
    return hydra.pg.validation.Validation.<String>checkAll(java.util.List.of(
      checkLabel.get(),
      checkId.get(),
      checkProperties.get()));
  }
  
  static <T0> hydra.util.Maybe<T0> checkAll(java.util.List<hydra.util.Maybe<T0>> checks) {
    return hydra.lib.lists.SafeHead.apply(hydra.pg.validation.Validation.<T0>checkAll_errors(checks));
  }
  
  static <T0> java.util.List<T0> checkAll_errors(java.util.List<hydra.util.Maybe<T0>> checks) {
    return hydra.lib.maybes.Cat.apply(checks);
  }
  
  static <T0> String edgeError(java.util.function.Function<T0, String> showValue, hydra.pg.model.Edge<T0> e, String v1) {
    return hydra.pg.validation.Validation.prepend(
      hydra.lib.strings.Cat2.apply(
        "Invalid edge with id ",
        (showValue).apply(((java.util.function.Function<hydra.pg.model.Edge<T0>, T0>) (projected -> projected.id)).apply(e))),
      v1);
  }
  
  static String edgeLabelMismatch(hydra.pg.model.EdgeLabel expected, hydra.pg.model.EdgeLabel actual) {
    return hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "expected ",
          (expected).value),
        ", found "),
      (actual).value);
  }
  
  static String prepend(String prefix, String msg) {
    return hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        prefix,
        ": "),
      msg);
  }
  
  static <T0> hydra.util.Maybe<T0> verify(Boolean b, T0 err) {
    return hydra.lib.logic.IfElse.lazy(
      b,
      () -> (hydra.util.Maybe<T0>) (hydra.util.Maybe.<T0>nothing()),
      () -> hydra.util.Maybe.just(err));
  }
  
  static <T0> String vertexError(java.util.function.Function<T0, String> showValue, hydra.pg.model.Vertex<T0> v, String v1) {
    return hydra.pg.validation.Validation.prepend(
      hydra.lib.strings.Cat2.apply(
        "Invalid vertex with id ",
        (showValue).apply(((java.util.function.Function<hydra.pg.model.Vertex<T0>, T0>) (projected -> projected.id)).apply(v))),
      v1);
  }
  
  static String vertexLabelMismatch(hydra.pg.model.VertexLabel expected, hydra.pg.model.VertexLabel actual) {
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      "expected ",
      (expected).value,
      ", found ",
      (actual).value));
  }
}
