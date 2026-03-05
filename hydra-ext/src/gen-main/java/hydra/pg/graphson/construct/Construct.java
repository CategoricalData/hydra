// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.construct;

/**
 * Functions for constructing GraphSON vertices from property graph vertices.
 */
public interface Construct {
  static <T0, T1> java.util.Map<T0, java.util.List<T1>> aggregateMap(java.util.List<hydra.util.Pair<T0, T1>> pairs) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Map<T0, java.util.List<T1>>, java.util.function.Function<hydra.util.Pair<T0, T1>, java.util.Map<T0, java.util.List<T1>>>>) (m -> (java.util.function.Function<hydra.util.Pair<T0, T1>, java.util.Map<T0, java.util.List<T1>>>) (p -> {
        hydra.util.Lazy<T0> k = new hydra.util.Lazy<>(() -> hydra.pg.graphson.construct.Construct.<T0, T1>aggregateMap_k(p));
        hydra.util.Lazy<T1> v = new hydra.util.Lazy<>(() -> hydra.pg.graphson.construct.Construct.<T0, T1>aggregateMap_v(p));
        return hydra.lib.maps.Insert.apply(
          k.get(),
          hydra.lib.maybes.Maybe.apply(
            hydra.lib.lists.Pure.apply(v.get()),
            (java.util.function.Function<java.util.List<T1>, java.util.List<T1>>) (vs -> hydra.lib.lists.Cons.apply(
              v.get(),
              vs)),
            hydra.pg.graphson.construct.Construct.<T0, T1>aggregateMap_existing(
              k.get(),
              m)),
          m);
      })),
      (java.util.Map<T0, java.util.List<T1>>) ((java.util.Map<T0, java.util.List<T1>>) (hydra.lib.maps.Empty.<T0, java.util.List<T1>>apply())),
      pairs);
  }
  
  static <T0, T1> T0 aggregateMap_k(hydra.util.Pair<T0, T1> p) {
    return hydra.lib.pairs.First.apply(p);
  }
  
  static <T0, T1> T1 aggregateMap_v(hydra.util.Pair<T0, T1> p) {
    return hydra.lib.pairs.Second.apply(p);
  }
  
  static <T0, T1> hydra.util.Maybe<java.util.List<T1>> aggregateMap_existing(T0 k, java.util.Map<T0, java.util.List<T1>> m) {
    return hydra.lib.maps.Lookup.apply(
      k,
      m);
  }
  
  static <T0, T1> hydra.util.Either<T1, hydra.util.Pair<hydra.pg.graphson.syntax.EdgeLabel, hydra.pg.graphson.syntax.AdjacentEdge>> adjacentEdgeToGraphson(java.util.function.Function<T0, hydra.util.Either<T1, hydra.pg.graphson.syntax.Value>> encodeValue, hydra.pg.model.AdjacentEdge<T0> edge) {
    hydra.util.Lazy<hydra.pg.model.EdgeLabel> label = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.AdjacentEdge<T0>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(edge));
    return hydra.lib.eithers.Bind.apply(
      (encodeValue).apply(hydra.pg.graphson.construct.Construct.<T0>adjacentEdgeToGraphson_edgeId(edge)),
      (java.util.function.Function<hydra.pg.graphson.syntax.Value, hydra.util.Either<T1, hydra.util.Pair<hydra.pg.graphson.syntax.EdgeLabel, hydra.pg.graphson.syntax.AdjacentEdge>>>) (gid -> hydra.lib.eithers.Bind.apply(
        (encodeValue).apply(hydra.pg.graphson.construct.Construct.<T0>adjacentEdgeToGraphson_vertexId(edge)),
        (java.util.function.Function<hydra.pg.graphson.syntax.Value, hydra.util.Either<T1, hydra.util.Pair<hydra.pg.graphson.syntax.EdgeLabel, hydra.pg.graphson.syntax.AdjacentEdge>>>) (gv -> hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.pg.model.PropertyKey, T0>, hydra.util.Either<T1, hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.Value>>>) (v1 -> hydra.pg.graphson.construct.Construct.edgePropertyToGraphson(
              encodeValue,
              v1)),
            hydra.lib.maps.ToList.apply(hydra.pg.graphson.construct.Construct.<T0>adjacentEdgeToGraphson_props(edge))),
          (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.Value>>, hydra.util.Either<T1, hydra.util.Pair<hydra.pg.graphson.syntax.EdgeLabel, hydra.pg.graphson.syntax.AdjacentEdge>>>) (propPairs -> (hydra.util.Either<T1, hydra.util.Pair<hydra.pg.graphson.syntax.EdgeLabel, hydra.pg.graphson.syntax.AdjacentEdge>>) ((hydra.util.Either<T1, hydra.util.Pair<hydra.pg.graphson.syntax.EdgeLabel, hydra.pg.graphson.syntax.AdjacentEdge>>) (hydra.util.Either.<T1, hydra.util.Pair<hydra.pg.graphson.syntax.EdgeLabel, hydra.pg.graphson.syntax.AdjacentEdge>>right((hydra.util.Pair<hydra.pg.graphson.syntax.EdgeLabel, hydra.pg.graphson.syntax.AdjacentEdge>) ((hydra.util.Pair<hydra.pg.graphson.syntax.EdgeLabel, hydra.pg.graphson.syntax.AdjacentEdge>) (new hydra.util.Pair<hydra.pg.graphson.syntax.EdgeLabel, hydra.pg.graphson.syntax.AdjacentEdge>(new hydra.pg.graphson.syntax.EdgeLabel((label.get()).value), new hydra.pg.graphson.syntax.AdjacentEdge(gid, gv, hydra.lib.maps.FromList.apply(propPairs))))))))))))));
  }
  
  static <T0> T0 adjacentEdgeToGraphson_edgeId(hydra.pg.model.AdjacentEdge<T0> edge) {
    return ((java.util.function.Function<hydra.pg.model.AdjacentEdge<T0>, T0>) (projected -> projected.id)).apply(edge);
  }
  
  static <T0> T0 adjacentEdgeToGraphson_vertexId(hydra.pg.model.AdjacentEdge<T0> edge) {
    return ((java.util.function.Function<hydra.pg.model.AdjacentEdge<T0>, T0>) (projected -> projected.vertex)).apply(edge);
  }
  
  static <T0> java.util.Map<hydra.pg.model.PropertyKey, T0> adjacentEdgeToGraphson_props(hydra.pg.model.AdjacentEdge<T0> edge) {
    return ((java.util.function.Function<hydra.pg.model.AdjacentEdge<T0>, java.util.Map<hydra.pg.model.PropertyKey, T0>>) (projected -> projected.properties)).apply(edge);
  }
  
  static <T0, T1, T2> hydra.util.Either<T1, hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, T2>> edgePropertyToGraphson(java.util.function.Function<T0, hydra.util.Either<T1, T2>> encodeValue, hydra.util.Pair<hydra.pg.model.PropertyKey, T0> prop) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<T2, hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, T2>>) (gv -> (hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, T2>) ((hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, T2>) (new hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, T2>(new hydra.pg.graphson.syntax.PropertyKey((hydra.lib.pairs.First.apply(prop)).value), gv)))),
      (encodeValue).apply(hydra.lib.pairs.Second.apply(prop)));
  }
  
  static hydra.compute.Coder<hydra.pg.graphson.syntax.Vertex, hydra.json.model.Value> graphsonVertexToJsonCoder() {
    return new hydra.compute.Coder<hydra.pg.graphson.syntax.Vertex, hydra.json.model.Value>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.pg.graphson.syntax.Vertex, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.json.model.Value>>>) (_cx -> (java.util.function.Function<hydra.pg.graphson.syntax.Vertex, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.json.model.Value>>) (v -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.json.model.Value>right(hydra.pg.graphson.coder.Coder.vertexToJson(v)))))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Vertex>>>) (_cx -> (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Vertex>>) (ignored -> (hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Vertex>) ((hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Vertex>) (hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Vertex>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError("decoding GraphSON JSON is currently unsupported"), _cx))))))));
  }
  
  static <T0, T1> hydra.util.Either<T1, hydra.pg.graphson.syntax.Vertex> pgVertexWithAdjacentEdgesToGraphsonVertex(java.util.function.Function<T0, hydra.util.Either<T1, hydra.pg.graphson.syntax.Value>> encodeValue, hydra.pg.model.VertexWithAdjacentEdges<T0> vae) {
    hydra.util.Lazy<hydra.pg.model.Vertex<T0>> vertex = new hydra.util.Lazy<>(() -> hydra.pg.graphson.construct.Construct.<T0>pgVertexWithAdjacentEdgesToGraphsonVertex_vertex(vae));
    hydra.util.Lazy<hydra.pg.model.VertexLabel> label = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Vertex<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(vertex.get()));
    return hydra.lib.eithers.Bind.apply(
      (encodeValue).apply(hydra.pg.graphson.construct.Construct.<T0>pgVertexWithAdjacentEdgesToGraphsonVertex_vertexId(vertex.get())),
      (java.util.function.Function<hydra.pg.graphson.syntax.Value, hydra.util.Either<T1, hydra.pg.graphson.syntax.Vertex>>) (gid -> hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.pg.model.PropertyKey, T0>, hydra.util.Either<T1, hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.VertexPropertyValue>>>) (v1 -> hydra.pg.graphson.construct.Construct.<T0, T1>vertexPropertyToGraphson(
            encodeValue,
            v1)),
          hydra.lib.maps.ToList.apply(hydra.pg.graphson.construct.Construct.<T0>pgVertexWithAdjacentEdgesToGraphsonVertex_props(vertex.get()))),
        (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.VertexPropertyValue>>, hydra.util.Either<T1, hydra.pg.graphson.syntax.Vertex>>) (propPairs -> hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.pg.model.AdjacentEdge<T0>, hydra.util.Either<T1, hydra.util.Pair<hydra.pg.graphson.syntax.EdgeLabel, hydra.pg.graphson.syntax.AdjacentEdge>>>) (v1 -> hydra.pg.graphson.construct.Construct.<T0, T1>adjacentEdgeToGraphson(
              encodeValue,
              v1)),
            hydra.pg.graphson.construct.Construct.<T0>pgVertexWithAdjacentEdgesToGraphsonVertex_ins(vae)),
          (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.pg.graphson.syntax.EdgeLabel, hydra.pg.graphson.syntax.AdjacentEdge>>, hydra.util.Either<T1, hydra.pg.graphson.syntax.Vertex>>) (inPairs -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.pg.model.AdjacentEdge<T0>, hydra.util.Either<T1, hydra.util.Pair<hydra.pg.graphson.syntax.EdgeLabel, hydra.pg.graphson.syntax.AdjacentEdge>>>) (v1 -> hydra.pg.graphson.construct.Construct.<T0, T1>adjacentEdgeToGraphson(
                encodeValue,
                v1)),
              hydra.pg.graphson.construct.Construct.<T0>pgVertexWithAdjacentEdgesToGraphsonVertex_outs(vae)),
            (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.pg.graphson.syntax.EdgeLabel, hydra.pg.graphson.syntax.AdjacentEdge>>, hydra.util.Either<T1, hydra.pg.graphson.syntax.Vertex>>) (outPairs -> (hydra.util.Either<T1, hydra.pg.graphson.syntax.Vertex>) ((hydra.util.Either<T1, hydra.pg.graphson.syntax.Vertex>) (hydra.util.Either.<T1, hydra.pg.graphson.syntax.Vertex>right(new hydra.pg.graphson.syntax.Vertex(gid, hydra.util.Maybe.just(new hydra.pg.graphson.syntax.VertexLabel((label.get()).value)), hydra.pg.graphson.construct.Construct.aggregateMap(inPairs), hydra.pg.graphson.construct.Construct.aggregateMap(outPairs), hydra.pg.graphson.construct.Construct.aggregateMap(propPairs)))))))))))));
  }
  
  static <T0> hydra.pg.model.Vertex<T0> pgVertexWithAdjacentEdgesToGraphsonVertex_vertex(hydra.pg.model.VertexWithAdjacentEdges<T0> vae) {
    return ((java.util.function.Function<hydra.pg.model.VertexWithAdjacentEdges<T0>, hydra.pg.model.Vertex<T0>>) (projected -> projected.vertex)).apply(vae);
  }
  
  static <T0> java.util.List<hydra.pg.model.AdjacentEdge<T0>> pgVertexWithAdjacentEdgesToGraphsonVertex_ins(hydra.pg.model.VertexWithAdjacentEdges<T0> vae) {
    return ((java.util.function.Function<hydra.pg.model.VertexWithAdjacentEdges<T0>, java.util.List<hydra.pg.model.AdjacentEdge<T0>>>) (projected -> projected.ins)).apply(vae);
  }
  
  static <T0> java.util.List<hydra.pg.model.AdjacentEdge<T0>> pgVertexWithAdjacentEdgesToGraphsonVertex_outs(hydra.pg.model.VertexWithAdjacentEdges<T0> vae) {
    return ((java.util.function.Function<hydra.pg.model.VertexWithAdjacentEdges<T0>, java.util.List<hydra.pg.model.AdjacentEdge<T0>>>) (projected -> projected.outs)).apply(vae);
  }
  
  static <T0> T0 pgVertexWithAdjacentEdgesToGraphsonVertex_vertexId(hydra.pg.model.Vertex<T0> vertex) {
    return ((java.util.function.Function<hydra.pg.model.Vertex<T0>, T0>) (projected -> projected.id)).apply(vertex);
  }
  
  static <T0> java.util.Map<hydra.pg.model.PropertyKey, T0> pgVertexWithAdjacentEdgesToGraphsonVertex_props(hydra.pg.model.Vertex<T0> vertex) {
    return ((java.util.function.Function<hydra.pg.model.Vertex<T0>, java.util.Map<hydra.pg.model.PropertyKey, T0>>) (projected -> projected.properties)).apply(vertex);
  }
  
  static <T0, T1> hydra.util.Either<T1, hydra.json.model.Value> pgVertexWithAdjacentEdgesToJson(java.util.function.Function<T0, hydra.util.Either<T1, hydra.pg.graphson.syntax.Value>> encodeValue, hydra.pg.model.VertexWithAdjacentEdges<T0> vertex) {
    return hydra.lib.eithers.Bind.apply(
      hydra.pg.graphson.construct.Construct.<T0, T1>pgVertexWithAdjacentEdgesToGraphsonVertex(
        encodeValue,
        vertex),
      (java.util.function.Function<hydra.pg.graphson.syntax.Vertex, hydra.util.Either<T1, hydra.json.model.Value>>) (gVertex -> (hydra.util.Either<T1, hydra.json.model.Value>) ((hydra.util.Either<T1, hydra.json.model.Value>) (hydra.util.Either.<T1, hydra.json.model.Value>right(hydra.pg.graphson.coder.Coder.vertexToJson(gVertex))))));
  }
  
  static <T0, T1> hydra.util.Either<T1, hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.VertexPropertyValue>> vertexPropertyToGraphson(java.util.function.Function<T0, hydra.util.Either<T1, hydra.pg.graphson.syntax.Value>> encodeValue, hydra.util.Pair<hydra.pg.model.PropertyKey, T0> prop) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.pg.graphson.syntax.Value, hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.VertexPropertyValue>>) (gv -> (hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.VertexPropertyValue>) ((hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.VertexPropertyValue>) (new hydra.util.Pair<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.VertexPropertyValue>(new hydra.pg.graphson.syntax.PropertyKey((hydra.lib.pairs.First.apply(prop)).value), new hydra.pg.graphson.syntax.VertexPropertyValue((hydra.util.Maybe<hydra.pg.graphson.syntax.Value>) (hydra.util.Maybe.<hydra.pg.graphson.syntax.Value>nothing()), gv))))),
      (encodeValue).apply(hydra.lib.pairs.Second.apply(prop)));
  }
}
