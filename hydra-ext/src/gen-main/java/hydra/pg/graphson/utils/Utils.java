// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.utils;

/**
 * Utility functions for GraphSON encoding and property graph conversion.
 */
public interface Utils {
  static <T0> java.util.List<hydra.pg.model.VertexWithAdjacentEdges<T0>> elementsToVerticesWithAdjacentEdges(java.util.List<hydra.pg.model.Element<T0>> els) {
    hydra.util.Lazy<hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>> partitioned = new hydra.util.Lazy<>(() -> hydra.pg.graphson.utils.Utils.<T0>elementsToVerticesWithAdjacentEdges_partitioned(els));
    return hydra.lib.maps.Elems.apply(hydra.pg.graphson.utils.Utils.<T0>elementsToVerticesWithAdjacentEdges_vertexMap1(
      hydra.pg.graphson.utils.Utils.<T0>elementsToVerticesWithAdjacentEdges_edges(partitioned.get()),
      hydra.pg.graphson.utils.Utils.<T0>elementsToVerticesWithAdjacentEdges_vertexMap0(hydra.pg.graphson.utils.Utils.<T0>elementsToVerticesWithAdjacentEdges_vertices(partitioned.get()))));
  }
  
  static <T0> hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>> elementsToVerticesWithAdjacentEdges_partitioned(java.util.List<hydra.pg.model.Element<T0>> els) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>, java.util.function.Function<hydra.pg.model.Element<T0>, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>>>) (acc -> (java.util.function.Function<hydra.pg.model.Element<T0>, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>>) (el -> ((java.util.function.Function<hydra.pg.model.Element, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>>) (v1 -> ((java.util.function.Function<hydra.pg.model.Element<T0>, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>>) ((java.util.function.Function<hydra.pg.model.Element<T0>, hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>>) (u -> (u).accept(new hydra.pg.model.Element.PartialVisitor<>() {
        @Override
        public hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>> visit(hydra.pg.model.Element.Vertex<T0> v) {
          return (hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>) ((hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>) (new hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>(hydra.lib.lists.Cons.apply(
            (v).value,
            hydra.lib.pairs.First.apply(acc)), hydra.lib.pairs.Second.apply(acc))));
        }
        
        @Override
        public hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>> visit(hydra.pg.model.Element.Edge<T0> e) {
          return (hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>) ((hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>) (new hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>(hydra.lib.pairs.First.apply(acc), hydra.lib.lists.Cons.apply(
            (e).value,
            hydra.lib.pairs.Second.apply(acc)))));
        }
      })))).apply(v1))).apply(el))),
      (hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>) ((hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>) (new hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>>((java.util.List<hydra.pg.model.Vertex<T0>>) (java.util.List.<hydra.pg.model.Vertex<T0>>of()), (java.util.List<hydra.pg.model.Edge<T0>>) (java.util.List.<hydra.pg.model.Edge<T0>>of())))),
      els);
  }
  
  static <T0> java.util.List<hydra.pg.model.Vertex<T0>> elementsToVerticesWithAdjacentEdges_vertices(hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>> partitioned) {
    return hydra.lib.lists.Reverse.apply(hydra.lib.pairs.First.apply(partitioned));
  }
  
  static <T0> java.util.List<hydra.pg.model.Edge<T0>> elementsToVerticesWithAdjacentEdges_edges(hydra.util.Pair<java.util.List<hydra.pg.model.Vertex<T0>>, java.util.List<hydra.pg.model.Edge<T0>>> partitioned) {
    return hydra.lib.lists.Reverse.apply(hydra.lib.pairs.Second.apply(partitioned));
  }
  
  static <T0> java.util.Map<T0, hydra.pg.model.VertexWithAdjacentEdges<T0>> elementsToVerticesWithAdjacentEdges_vertexMap0(java.util.List<hydra.pg.model.Vertex<T0>> vertices) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.pg.model.Vertex<T0>, hydra.util.Pair<T0, hydra.pg.model.VertexWithAdjacentEdges<T0>>>) (v -> (hydra.util.Pair<T0, hydra.pg.model.VertexWithAdjacentEdges<T0>>) ((hydra.util.Pair<T0, hydra.pg.model.VertexWithAdjacentEdges<T0>>) (new hydra.util.Pair<T0, hydra.pg.model.VertexWithAdjacentEdges<T0>>(((java.util.function.Function<hydra.pg.model.Vertex<T0>, T0>) (projected -> projected.id)).apply(v), (hydra.pg.model.VertexWithAdjacentEdges<T0>) (new hydra.pg.model.VertexWithAdjacentEdges<T0>(v, (java.util.List<hydra.pg.model.AdjacentEdge<T0>>) (java.util.List.<hydra.pg.model.AdjacentEdge<T0>>of()), (java.util.List<hydra.pg.model.AdjacentEdge<T0>>) (java.util.List.<hydra.pg.model.AdjacentEdge<T0>>of()))))))),
      vertices));
  }
  
  static <T0> java.util.Map<T0, hydra.pg.model.VertexWithAdjacentEdges<T0>> elementsToVerticesWithAdjacentEdges_vertexMap1(java.util.List<hydra.pg.model.Edge<T0>> edges, java.util.Map<T0, hydra.pg.model.VertexWithAdjacentEdges<T0>> vertexMap0) {
    return hydra.lib.lists.Foldl.apply(
      (java.util.function.Function<java.util.Map<T0, hydra.pg.model.VertexWithAdjacentEdges<T0>>, java.util.function.Function<hydra.pg.model.Edge<T0>, java.util.Map<T0, hydra.pg.model.VertexWithAdjacentEdges<T0>>>>) (vmap -> (java.util.function.Function<hydra.pg.model.Edge<T0>, java.util.Map<T0, hydra.pg.model.VertexWithAdjacentEdges<T0>>>) (edge -> {
        hydra.util.Lazy<T0> edgeId = new hydra.util.Lazy<>(() -> hydra.pg.graphson.utils.Utils.<T0>elementsToVerticesWithAdjacentEdges_edgeId(edge));
        hydra.util.Lazy<T0> inV = new hydra.util.Lazy<>(() -> hydra.pg.graphson.utils.Utils.<T0>elementsToVerticesWithAdjacentEdges_inV(edge));
        hydra.util.Lazy<hydra.pg.model.EdgeLabel> label = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Edge<T0>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(edge));
        hydra.util.Lazy<T0> outV = new hydra.util.Lazy<>(() -> hydra.pg.graphson.utils.Utils.<T0>elementsToVerticesWithAdjacentEdges_outV(edge));
        hydra.util.Lazy<java.util.Map<hydra.pg.model.PropertyKey, T0>> props = new hydra.util.Lazy<>(() -> hydra.pg.graphson.utils.Utils.<T0>elementsToVerticesWithAdjacentEdges_props(edge));
        hydra.util.Lazy<java.util.Map<T0, hydra.pg.model.VertexWithAdjacentEdges<T0>>> vmap1 = new hydra.util.Lazy<>(() -> hydra.pg.graphson.utils.Utils.<T0>elementsToVerticesWithAdjacentEdges_vmap1(
          hydra.pg.graphson.utils.Utils.<T0>elementsToVerticesWithAdjacentEdges_adjEdgeOut(
            edgeId.get(),
            inV.get(),
            label.get(),
            props.get()),
          outV.get(),
          vmap));
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> vmap1.get(),
          (java.util.function.Function<hydra.pg.model.VertexWithAdjacentEdges<T0>, java.util.Map<T0, hydra.pg.model.VertexWithAdjacentEdges<T0>>>) (vae -> hydra.lib.maps.Insert.apply(
            inV.get(),
            (hydra.pg.model.VertexWithAdjacentEdges<T0>) (new hydra.pg.model.VertexWithAdjacentEdges<T0>(((java.util.function.Function<hydra.pg.model.VertexWithAdjacentEdges<T0>, hydra.pg.model.Vertex<T0>>) (projected -> projected.vertex)).apply(vae), hydra.lib.lists.Cons.apply(
              hydra.pg.graphson.utils.Utils.<T0>elementsToVerticesWithAdjacentEdges_adjEdgeIn(
                edgeId.get(),
                label.get(),
                outV.get(),
                props.get()),
              ((java.util.function.Function<hydra.pg.model.VertexWithAdjacentEdges<T0>, java.util.List<hydra.pg.model.AdjacentEdge<T0>>>) (projected -> projected.ins)).apply(vae)), ((java.util.function.Function<hydra.pg.model.VertexWithAdjacentEdges<T0>, java.util.List<hydra.pg.model.AdjacentEdge<T0>>>) (projected -> projected.outs)).apply(vae))),
            vmap1.get())),
          hydra.lib.maps.Lookup.apply(
            inV.get(),
            vmap1.get()));
      })),
      vertexMap0,
      edges);
  }
  
  static <T0> T0 elementsToVerticesWithAdjacentEdges_edgeId(hydra.pg.model.Edge<T0> edge) {
    return ((java.util.function.Function<hydra.pg.model.Edge<T0>, T0>) (projected -> projected.id)).apply(edge);
  }
  
  static <T0> T0 elementsToVerticesWithAdjacentEdges_outV(hydra.pg.model.Edge<T0> edge) {
    return ((java.util.function.Function<hydra.pg.model.Edge<T0>, T0>) (projected -> projected.out)).apply(edge);
  }
  
  static <T0> T0 elementsToVerticesWithAdjacentEdges_inV(hydra.pg.model.Edge<T0> edge) {
    return ((java.util.function.Function<hydra.pg.model.Edge<T0>, T0>) (projected -> projected.in)).apply(edge);
  }
  
  static <T0> java.util.Map<hydra.pg.model.PropertyKey, T0> elementsToVerticesWithAdjacentEdges_props(hydra.pg.model.Edge<T0> edge) {
    return ((java.util.function.Function<hydra.pg.model.Edge<T0>, java.util.Map<hydra.pg.model.PropertyKey, T0>>) (projected -> projected.properties)).apply(edge);
  }
  
  static <T0> hydra.pg.model.AdjacentEdge<T0> elementsToVerticesWithAdjacentEdges_adjEdgeOut(T0 edgeId, T0 inV, hydra.pg.model.EdgeLabel label, java.util.Map<hydra.pg.model.PropertyKey, T0> props) {
    return (hydra.pg.model.AdjacentEdge<T0>) (new hydra.pg.model.AdjacentEdge<T0>(label, edgeId, inV, props));
  }
  
  static <T0> hydra.pg.model.AdjacentEdge<T0> elementsToVerticesWithAdjacentEdges_adjEdgeIn(T0 edgeId, hydra.pg.model.EdgeLabel label, T0 outV, java.util.Map<hydra.pg.model.PropertyKey, T0> props) {
    return (hydra.pg.model.AdjacentEdge<T0>) (new hydra.pg.model.AdjacentEdge<T0>(label, edgeId, outV, props));
  }
  
  static <T0> java.util.Map<T0, hydra.pg.model.VertexWithAdjacentEdges<T0>> elementsToVerticesWithAdjacentEdges_vmap1(hydra.pg.model.AdjacentEdge<T0> adjEdgeOut, T0 outV, java.util.Map<T0, hydra.pg.model.VertexWithAdjacentEdges<T0>> vmap) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> vmap,
      (java.util.function.Function<hydra.pg.model.VertexWithAdjacentEdges<T0>, java.util.Map<T0, hydra.pg.model.VertexWithAdjacentEdges<T0>>>) (vae -> hydra.lib.maps.Insert.apply(
        outV,
        (hydra.pg.model.VertexWithAdjacentEdges<T0>) (new hydra.pg.model.VertexWithAdjacentEdges<T0>(((java.util.function.Function<hydra.pg.model.VertexWithAdjacentEdges<T0>, hydra.pg.model.Vertex<T0>>) (projected -> projected.vertex)).apply(vae), ((java.util.function.Function<hydra.pg.model.VertexWithAdjacentEdges<T0>, java.util.List<hydra.pg.model.AdjacentEdge<T0>>>) (projected -> projected.ins)).apply(vae), hydra.lib.lists.Cons.apply(
          adjEdgeOut,
          ((java.util.function.Function<hydra.pg.model.VertexWithAdjacentEdges<T0>, java.util.List<hydra.pg.model.AdjacentEdge<T0>>>) (projected -> projected.outs)).apply(vae)))),
        vmap)),
      hydra.lib.maps.Lookup.apply(
        outV,
        vmap));
  }
  
  static <T0> hydra.util.Either<T0, hydra.pg.graphson.syntax.Value> encodeStringValue(String s) {
    return hydra.util.Either.<T0, hydra.pg.graphson.syntax.Value>right(new hydra.pg.graphson.syntax.Value.String_(s));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> encodeTermValue(hydra.core.Term term) {
    return (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> otherwise(hydra.core.Term instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError("unsupported term variant for GraphSON encoding"), hydra.monads.Monads.emptyContext())));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> visit(hydra.core.Term.Literal lit) {
        return ((lit).value).accept(new hydra.core.Literal.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> otherwise(hydra.core.Literal instance) {
            return hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError("unsupported literal type for GraphSON encoding"), hydra.monads.Monads.emptyContext())));
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> visit(hydra.core.Literal.Binary b) {
            return hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value>right(new hydra.pg.graphson.syntax.Value.Binary(hydra.lib.literals.BinaryToString.apply((b).value)));
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> visit(hydra.core.Literal.Boolean_ b) {
            return hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value>right(new hydra.pg.graphson.syntax.Value.Boolean_((b).value));
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> visit(hydra.core.Literal.Float_ fv) {
            return ((fv).value).accept(new hydra.core.FloatValue.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> otherwise(hydra.core.FloatValue instance) {
                return hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError("unsupported float type"), hydra.monads.Monads.emptyContext())));
              }
              
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> visit(hydra.core.FloatValue.Bigfloat f) {
                return hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value>right(new hydra.pg.graphson.syntax.Value.BigDecimal(new hydra.pg.graphson.syntax.BigDecimalValue(hydra.lib.literals.ShowBigfloat.apply((f).value))));
              }
              
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> visit(hydra.core.FloatValue.Float32 f) {
                return hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value>right(new hydra.pg.graphson.syntax.Value.Float_(new hydra.pg.graphson.syntax.FloatValue.Finite((f).value)));
              }
              
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> visit(hydra.core.FloatValue.Float64 f) {
                return hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value>right(new hydra.pg.graphson.syntax.Value.Double_(new hydra.pg.graphson.syntax.DoubleValue.Finite((f).value)));
              }
            });
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> visit(hydra.core.Literal.Integer_ iv) {
            return ((iv).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> otherwise(hydra.core.IntegerValue instance) {
                return hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value>left((hydra.context.InContext<hydra.error.OtherError>) (new hydra.context.InContext<hydra.error.OtherError>(new hydra.error.OtherError("unsupported integer type"), hydra.monads.Monads.emptyContext())));
              }
              
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> visit(hydra.core.IntegerValue.Bigint i) {
                return hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value>right(new hydra.pg.graphson.syntax.Value.BigInteger((i).value));
              }
              
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> visit(hydra.core.IntegerValue.Int32 i) {
                return hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value>right(new hydra.pg.graphson.syntax.Value.Integer_((i).value));
              }
              
              @Override
              public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> visit(hydra.core.IntegerValue.Int64 i) {
                return hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value>right(new hydra.pg.graphson.syntax.Value.Long_((i).value));
              }
            });
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> visit(hydra.core.Literal.String_ s) {
            return hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value>right(new hydra.pg.graphson.syntax.Value.String_((s).value));
          }
        });
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value> visit(hydra.core.Term.Unit ignored) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.OtherError>, hydra.pg.graphson.syntax.Value>right(new hydra.pg.graphson.syntax.Value.Null());
      }
    });
  }
  
  static <T0, T1> hydra.util.Either<T1, java.util.List<hydra.json.model.Value>> pgElementsToGraphson(java.util.function.Function<T0, hydra.util.Either<T1, hydra.pg.graphson.syntax.Value>> encodeValue, java.util.List<hydra.pg.model.Element<T0>> els) {
    return hydra.lib.eithers.MapList.apply(
      (java.util.function.Function<hydra.pg.model.VertexWithAdjacentEdges<T0>, hydra.util.Either<T1, hydra.json.model.Value>>) (v1 -> hydra.pg.graphson.construct.Construct.<T0, T1>pgVertexWithAdjacentEdgesToJson(
        encodeValue,
        v1)),
      hydra.pg.graphson.utils.Utils.<T0>elementsToVerticesWithAdjacentEdges(els));
  }
}
