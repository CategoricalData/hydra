// Note: this is an automatically generated file. Do not edit.

package hydra.pg.coder;

/**
 * Property graph element coders for mapping Hydra terms to property graph elements
 */
public interface Coder {
  static <T0, T1> hydra.util.Either<T1, java.lang.Void> check(T0 _cx, Boolean b, hydra.util.Either<T1, java.lang.Void> e) {
    return hydra.lib.logic.IfElse.lazy(
      b,
      () -> hydra.util.Either.<T1, java.lang.Void>right(null),
      () -> e);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.lang.Void> checkRecordName(hydra.context.Context cx, hydra.core.Name expected, hydra.core.Name actual) {
    return hydra.pg.coder.Coder.check(
      cx,
      hydra.lib.equality.Equal.apply(
        (actual).value,
        (expected).value),
      hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.lang.Void>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            "Expected record of type ",
            (expected).value),
          ", found record of type "),
        (actual).value))), cx))));
  }
  
  static <T0, T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>> constructEdgeCoder(hydra.context.Context cx, hydra.graph.Graph g, hydra.pg.model.VertexLabel parentLabel, hydra.pg.mapping.Schema<T0, T1, T2> schema, hydra.core.Type source, T1 vidType, T1 eidType, hydra.pg.model.Direction dir, hydra.core.Name name, java.util.List<hydra.core.FieldType> fields, java.util.List<hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T1>, hydra.core.Field, hydra.pg.model.Property<T2>>> propAdapters, hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>> mOutSpec, hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>> mInSpec) {
    return hydra.lib.eithers.Bind.apply(
      hydra.pg.coder.Coder.findLabelString(
        cx,
        g,
        source,
        name,
        new hydra.core.Name((((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) (projected -> projected.annotations)))).apply(schema)).edgeLabel)),
      (java.util.function.Function<String, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (labelStr -> {
        hydra.pg.model.EdgeLabel label = new hydra.pg.model.EdgeLabel(labelStr);
        hydra.util.Lazy<hydra.util.Coder<hydra.core.Term, T2>> vertexIdsSchema = new hydra.util.Lazy<>(() -> hydra.pg.coder.Coder.<T0, T1, T2>constructEdgeCoder_vertexIdsSchema(schema));
        return hydra.lib.eithers.Bind.apply(
          hydra.pg.coder.Coder.edgeIdAdapter(
            cx,
            g,
            schema,
            eidType,
            name,
            new hydra.core.Name((((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) (projected -> projected.annotations)))).apply(schema)).edgeId),
            fields),
          (java.util.function.Function<hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (idAdapter -> hydra.lib.eithers.Bind.apply(
            hydra.lib.maybes.Maybe.applyLazy(
              () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>>>right((hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>>nothing())),
              (java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>>>>) (s -> hydra.lib.eithers.Map.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>>>) (x -> hydra.util.Maybe.just(x)),
                hydra.pg.coder.Coder.projectionAdapter(
                  cx,
                  g,
                  vidType,
                  vertexIdsSchema.get(),
                  s,
                  "out"))),
              mOutSpec),
            (java.util.function.Function<hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (outIdAdapter -> hydra.lib.eithers.Bind.apply(
              hydra.lib.maybes.Maybe.applyLazy(
                () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>>>right((hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>>nothing())),
                (java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>>>>) (s -> hydra.lib.eithers.Map.apply(
                  (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>>>) (x -> hydra.util.Maybe.just(x)),
                  hydra.pg.coder.Coder.projectionAdapter(
                    cx,
                    g,
                    vidType,
                    vertexIdsSchema.get(),
                    s,
                    "in"))),
                mInSpec),
              (java.util.function.Function<hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (inIdAdapter -> hydra.lib.eithers.Bind.apply(
                hydra.lib.maybes.Maybe.applyLazy(
                  () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>right((hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>nothing())),
                  (java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>>) (s -> hydra.lib.eithers.Map.apply(
                    (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>) (x -> hydra.util.Maybe.just(x)),
                    hydra.pg.coder.Coder.<T0, T1, T2>findIncidentVertexAdapter(
                      cx,
                      g,
                      schema,
                      vidType,
                      eidType,
                      s))),
                  mOutSpec),
                (java.util.function.Function<hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (outVertexAdapter -> hydra.lib.eithers.Bind.apply(
                  hydra.lib.maybes.Maybe.applyLazy(
                    () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>right((hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>nothing())),
                    (java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>>) (s -> hydra.lib.eithers.Map.apply(
                      (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>) (x -> hydra.util.Maybe.just(x)),
                      hydra.pg.coder.Coder.<T0, T1, T2>findIncidentVertexAdapter(
                        cx,
                        g,
                        schema,
                        vidType,
                        eidType,
                        s))),
                    mInSpec),
                  (java.util.function.Function<hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (inVertexAdapter -> hydra.lib.eithers.Bind.apply(
                    hydra.lib.maybes.Maybe.applyLazy(
                      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.VertexLabel>right(parentLabel),
                      (java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.VertexLabel>>) (spec -> hydra.lib.maybes.Maybe.applyLazy(
                        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.VertexLabel>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError("no out-vertex label")), cx))),
                        (java.util.function.Function<String, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.VertexLabel>>) (a -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.VertexLabel>right(new hydra.pg.model.VertexLabel(a))),
                        hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(spec)))),
                      mOutSpec),
                    (java.util.function.Function<hydra.pg.model.VertexLabel, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (outLabel -> hydra.lib.eithers.Bind.apply(
                      hydra.lib.maybes.Maybe.applyLazy(
                        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.VertexLabel>right(parentLabel),
                        (java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.VertexLabel>>) (spec -> hydra.lib.maybes.Maybe.applyLazy(
                          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.VertexLabel>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError("no in-vertex label")), cx))),
                          (java.util.function.Function<String, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.VertexLabel>>) (a -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.VertexLabel>right(new hydra.pg.model.VertexLabel(a))),
                          hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(spec)))),
                        mInSpec),
                      (java.util.function.Function<hydra.pg.model.VertexLabel, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (inLabel -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>right(hydra.pg.coder.Coder.edgeCoder(
                        g,
                        dir,
                        schema,
                        source,
                        eidType,
                        name,
                        label,
                        outLabel,
                        inLabel,
                        idAdapter,
                        outIdAdapter,
                        inIdAdapter,
                        propAdapters,
                        hydra.pg.coder.Coder.<T1, T2>constructEdgeCoder_vertexAdapters(
                          inVertexAdapter,
                          outVertexAdapter)))))))))))))))));
      }));
  }
  
  static <T0, T1, T2> hydra.util.Coder<hydra.core.Term, T2> constructEdgeCoder_vertexIdsSchema(hydra.pg.mapping.Schema<T0, T1, T2> schema) {
    return ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.util.Coder<hydra.core.Term, T2>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.util.Coder<hydra.core.Term, T2>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.util.Coder<hydra.core.Term, T2>>) (projected -> projected.vertexIds)))).apply(schema);
  }
  
  static <T1, T2> java.util.List<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>> constructEdgeCoder_vertexAdapters(hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>> inVertexAdapter, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>> outVertexAdapter) {
    return hydra.lib.maybes.Cat.apply(java.util.List.of(
      outVertexAdapter,
      inVertexAdapter));
  }
  
  static <T0, T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>> constructVertexCoder(hydra.context.Context cx, hydra.graph.Graph g, hydra.pg.mapping.Schema<T0, T1, T2> schema, hydra.core.Type source, T1 vidType, T1 eidType, hydra.core.Name name, java.util.List<hydra.core.FieldType> fields, java.util.List<hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T1>, hydra.core.Field, hydra.pg.model.Property<T2>>> propAdapters) {
    return hydra.lib.eithers.Bind.apply(
      hydra.pg.coder.Coder.findLabelString(
        cx,
        g,
        source,
        name,
        new hydra.core.Name((((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) (projected -> projected.annotations)))).apply(schema)).vertexLabel)),
      (java.util.function.Function<String, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (labelStr -> {
        hydra.pg.model.VertexLabel label = new hydra.pg.model.VertexLabel(labelStr);
        return hydra.lib.eithers.Bind.apply(
          hydra.pg.coder.Coder.vertexIdAdapter(
            cx,
            g,
            schema,
            vidType,
            name,
            new hydra.core.Name((((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) (projected -> projected.annotations)))).apply(schema)).vertexId),
            fields),
          (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T1, hydra.core.Term, T2>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (idAdapter -> hydra.lib.eithers.Bind.apply(
            hydra.pg.coder.Coder.<T0, T1, T2>findAdjacenEdgeAdapters(
              cx,
              g,
              schema,
              vidType,
              eidType,
              label,
              new hydra.pg.model.Direction.Out(),
              fields),
            (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (outEdgeAdapters -> hydra.lib.eithers.Bind.apply(
              hydra.pg.coder.Coder.<T0, T1, T2>findAdjacenEdgeAdapters(
                cx,
                g,
                schema,
                vidType,
                eidType,
                label,
                new hydra.pg.model.Direction.In(),
                fields),
              (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (inEdgeAdapters -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>right(hydra.pg.coder.Coder.vertexCoder(
                g,
                schema,
                source,
                vidType,
                name,
                label,
                idAdapter,
                propAdapters,
                hydra.lib.lists.Concat2.apply(
                  outEdgeAdapters,
                  inEdgeAdapters)))))))));
      }));
  }
  
  static <T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13> hydra.util.Adapter<T4, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>> edgeCoder(T0 g, hydra.pg.model.Direction dir, hydra.pg.mapping.Schema<T1, T2, T3> schema, T4 source, T5 eidType, hydra.core.Name tname, hydra.pg.model.EdgeLabel label, hydra.pg.model.VertexLabel outLabel, hydra.pg.model.VertexLabel inLabel, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T6, T7, hydra.core.Term, T3>>> mIdAdapter, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T8, T9, hydra.core.Term, T3>>> outAdapter, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T10, T11, hydra.core.Term, T3>>> inAdapter, java.util.List<hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T5>, hydra.core.Field, hydra.pg.model.Property<T3>>> propAdapters, java.util.List<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T12, T13, hydra.core.Term, hydra.pg.model.ElementTree<T3>>>> vertexAdapters) {
    return (hydra.util.Adapter<T4, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>) ((hydra.util.Adapter<T4, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>) ((hydra.util.Adapter<T4, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>) ((hydra.util.Adapter<T4, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>) (new hydra.util.Adapter<T4, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>(true, source, hydra.pg.coder.Coder.<T5>elementTypeTreeEdge(
      hydra.pg.coder.Coder.<T3, T5>edgeCoder_et(
        eidType,
        inLabel,
        label,
        outLabel,
        propAdapters),
      (java.util.List<hydra.pg.model.ElementTypeTree<T5>>) (java.util.List.<hydra.pg.model.ElementTypeTree<T5>>of())), (hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>) ((hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>) (new hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>>) (cx -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>) (term -> {
      hydra.core.Term deannot = hydra.rewriting.Rewriting.deannotateTerm(term);
      hydra.util.Lazy<hydra.core.Term> unwrapped = new hydra.util.Lazy<>(() -> (deannot).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return deannot;
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Maybe mt) {
          return hydra.lib.maybes.FromMaybe.applyLazy(
            () -> deannot,
            (mt).value);
        }
      }));
      hydra.core.Record rec = (unwrapped.get()).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Record visit(hydra.core.Term.Record r) {
          return (r).value;
        }
      });
      return hydra.lib.eithers.Bind.apply(
        hydra.pg.coder.Coder.checkRecordName(
          cx,
          tname,
          (rec).typeName),
        (java.util.function.Function<java.lang.Void, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>) (_chk -> {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldsm = hydra.schemas.Schemas.fieldMap((rec).fields);
          return hydra.lib.eithers.Bind.apply(
            hydra.lib.maybes.Maybe.applyLazy(
              () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T3>right(((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, T3>) ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, T3>) ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, T3>) (projected -> projected.defaultEdgeId)))).apply(schema)),
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T6, T7, hydra.core.Term, T3>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3>>) (v1 -> hydra.pg.coder.Coder.selectEdgeId(
                cx,
                fieldsm,
                v1)),
              mIdAdapter),
            (java.util.function.Function<T3, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>) (edgeId -> hydra.lib.eithers.Bind.apply(
              hydra.pg.coder.Coder.encodeProperties(
                cx,
                fieldsm,
                propAdapters),
              (java.util.function.Function<java.util.Map<hydra.pg.model.PropertyKey, T3>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>) (props -> hydra.lib.eithers.Bind.apply(
                hydra.pg.coder.Coder.<T1, T2, T3, T8, T9>edgeCoder_getVertexId(
                  cx,
                  dir,
                  fieldsm,
                  schema,
                  new hydra.pg.model.Direction.Out(),
                  outAdapter),
                (java.util.function.Function<T3, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>) (outId -> hydra.lib.eithers.Bind.apply(
                  hydra.pg.coder.Coder.<T1, T2, T3, T10, T11>edgeCoder_getVertexId(
                    cx,
                    dir,
                    fieldsm,
                    schema,
                    new hydra.pg.model.Direction.In(),
                    inAdapter),
                  (java.util.function.Function<T3, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>) (inId -> hydra.lib.eithers.Bind.apply(
                    hydra.lib.eithers.Map.apply(
                      (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.pg.model.ElementTree<T3>>>, java.util.List<hydra.pg.model.ElementTree<T3>>>) (xs -> hydra.lib.maybes.Cat.apply(xs)),
                      hydra.lib.eithers.MapList.apply(
                        (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T12, T13, hydra.core.Term, hydra.pg.model.ElementTree<T3>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.pg.model.ElementTree<T3>>>>) (va -> {
                          hydra.util.Lazy<hydra.core.Name> fname = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(va));
                          return hydra.lib.maybes.Maybe.applyLazy(
                            () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.pg.model.ElementTree<T3>>>right((hydra.util.Maybe<hydra.pg.model.ElementTree<T3>>) (hydra.util.Maybe.<hydra.pg.model.ElementTree<T3>>nothing())),
                            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.pg.model.ElementTree<T3>>>>) (fterm -> hydra.lib.eithers.Map.apply(
                              (java.util.function.Function<hydra.pg.model.ElementTree<T3>, hydra.util.Maybe<hydra.pg.model.ElementTree<T3>>>) (x -> hydra.util.Maybe.just(x)),
                              ((((java.util.function.Function<hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>>>) ((java.util.function.Function<hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>>>) (projected -> projected.encode))).apply(((java.util.function.Function<hydra.util.Adapter<T12, T13, hydra.core.Term, hydra.pg.model.ElementTree<T3>>, hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>>) ((java.util.function.Function<hydra.util.Adapter<T12, T13, hydra.core.Term, hydra.pg.model.ElementTree<T3>>, hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>>) ((java.util.function.Function<hydra.util.Adapter<T12, T13, hydra.core.Term, hydra.pg.model.ElementTree<T3>>, hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>>) ((java.util.function.Function<hydra.util.Adapter<T12, T13, hydra.core.Term, hydra.pg.model.ElementTree<T3>>, hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>>) (projected -> projected.coder))))).apply(hydra.pg.coder.Coder.<T12, T13, T3>edgeCoder_ad(va)))).apply(cx)).apply(fterm))),
                            hydra.lib.maps.Lookup.apply(
                              fname.get(),
                              fieldsm));
                        }),
                        vertexAdapters)),
                    (java.util.function.Function<java.util.List<hydra.pg.model.ElementTree<T3>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>) (deps -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>right(hydra.pg.coder.Coder.<T3>elementTreeEdge(
                      (hydra.pg.model.Edge<T3>) (new hydra.pg.model.Edge<T3>(label, edgeId, outId, inId, props)),
                      deps))))))))))));
        }));
    })), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.pg.model.ElementTree<T3>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>) (cx -> (java.util.function.Function<hydra.pg.model.ElementTree<T3>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (ignored -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError("edge decoding is not yet supported")), cx)))))))))))));
  }
  
  static <T3, T5> hydra.pg.model.EdgeType<T5> edgeCoder_et(T5 eidType, hydra.pg.model.VertexLabel inLabel, hydra.pg.model.EdgeLabel label, hydra.pg.model.VertexLabel outLabel, java.util.List<hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T5>, hydra.core.Field, hydra.pg.model.Property<T3>>> propAdapters) {
    return (hydra.pg.model.EdgeType<T5>) (new hydra.pg.model.EdgeType<T5>(label, eidType, outLabel, inLabel, hydra.pg.coder.Coder.propertyTypes(propAdapters)));
  }
  
  static <T1, T2, T3, T14, T15> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3> edgeCoder_getVertexId(hydra.context.Context cx, hydra.pg.model.Direction dir, java.util.Map<hydra.core.Name, hydra.core.Term> fieldsm, hydra.pg.mapping.Schema<T1, T2, T3> schema, hydra.pg.model.Direction dirCheck, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T14, T15, hydra.core.Term, T3>>> adapter) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T3>right(((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, T3>) ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, T3>) ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, T3>) (projected -> projected.defaultVertexId)))).apply(schema)),
      (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T14, T15, hydra.core.Term, T3>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3>>) (v1 -> hydra.pg.coder.Coder.selectVertexId(
        cx,
        fieldsm,
        v1)),
      hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          dir,
          dirCheck),
        () -> (hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T14, T15, hydra.core.Term, T3>>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T14, T15, hydra.core.Term, T3>>>nothing()),
        () -> adapter));
  }
  
  static <T12, T13, T3> hydra.util.Adapter<T12, T13, hydra.core.Term, hydra.pg.model.ElementTree<T3>> edgeCoder_ad(hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T12, T13, hydra.core.Term, hydra.pg.model.ElementTree<T3>>> va) {
    return hydra.lib.pairs.Second.apply(va);
  }
  
  static <T0, T1, T2, T3, T4> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T4, hydra.core.Term, T3>>>> edgeIdAdapter(hydra.context.Context cx, T0 g, hydra.pg.mapping.Schema<T1, T2, T3> schema, T4 eidType, hydra.core.Name name, hydra.core.Name idKey, java.util.List<hydra.core.FieldType> fields) {
    return hydra.lib.eithers.Bind.apply(
      hydra.pg.coder.Coder.findIdProjectionSpec(
        cx,
        false,
        name,
        idKey,
        fields),
      (java.util.function.Function<hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T4, hydra.core.Term, T3>>>>>) (mIdSpec -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T4, hydra.core.Term, T3>>>>right((hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T4, hydra.core.Term, T3>>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T4, hydra.core.Term, T3>>>nothing())),
        (java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T4, hydra.core.Term, T3>>>>>) (idSpec -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T4, hydra.core.Term, T3>>, hydra.util.Maybe<hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T4, hydra.core.Term, T3>>>>) (x -> hydra.util.Maybe.just(x)),
          hydra.pg.coder.Coder.projectionAdapter(
            cx,
            g,
            eidType,
            ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, hydra.util.Coder<hydra.core.Term, T3>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, hydra.util.Coder<hydra.core.Term, T3>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, hydra.util.Coder<hydra.core.Term, T3>>) (projected -> projected.edgeIds)))).apply(schema),
            idSpec,
            "id"))),
        mIdSpec)));
  }
  
  static <T0, T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>> elementCoder(hydra.util.Maybe<hydra.util.Pair<hydra.pg.model.Direction, hydra.pg.model.VertexLabel>> mparent, hydra.pg.mapping.Schema<T0, T1, T2> schema, hydra.core.Type source, T1 vidType, T1 eidType, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.util.Lazy<hydra.pg.model.Direction> dir = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> new hydra.pg.model.Direction.Both(),
      (java.util.function.Function<hydra.util.Pair<hydra.pg.model.Direction, hydra.pg.model.VertexLabel>, hydra.pg.model.Direction>) (p -> hydra.lib.pairs.First.apply(p)),
      mparent));
    hydra.util.Lazy<hydra.pg.model.VertexLabel> parentLabel = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Maybe.applyLazy(
      () -> new hydra.pg.model.VertexLabel("NOLABEL"),
      (java.util.function.Function<hydra.util.Pair<hydra.pg.model.Direction, hydra.pg.model.VertexLabel>, hydra.pg.model.VertexLabel>) (p -> hydra.lib.pairs.Second.apply(p)),
      mparent));
    return (hydra.rewriting.Rewriting.deannotateType(source)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>> otherwise(hydra.core.Type instance) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "Expected ",
              "record type"),
            ", found: "),
          "other type"))), cx)));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>> visit(hydra.core.Type.Maybe ot) {
        return hydra.pg.coder.Coder.<T0, T1, T2>elementCoder(
          mparent,
          schema,
          (ot).value,
          vidType,
          eidType,
          cx,
          g);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>> visit(hydra.core.Type.Record rt) {
        java.util.List<hydra.core.FieldType> fields = ((rt).value).fields;
        hydra.util.Lazy<hydra.core.Name> inVertexKey = new hydra.util.Lazy<>(() -> new hydra.core.Name((((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) (projected -> projected.annotations)))).apply(schema)).inVertex));
        hydra.util.Lazy<hydra.core.Name> inVertexLabelKey = new hydra.util.Lazy<>(() -> new hydra.core.Name((((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) (projected -> projected.annotations)))).apply(schema)).inVertexLabel));
        hydra.core.Name name = ((rt).value).typeName;
        hydra.util.Lazy<hydra.core.Name> outVertexKey = new hydra.util.Lazy<>(() -> new hydra.core.Name((((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) (projected -> projected.annotations)))).apply(schema)).outVertex));
        hydra.util.Lazy<hydra.core.Name> outVertexLabelKey = new hydra.util.Lazy<>(() -> new hydra.core.Name((((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) (projected -> projected.annotations)))).apply(schema)).outVertexLabel));
        return hydra.lib.eithers.Bind.apply(
          hydra.pg.coder.Coder.findProjectionSpec(
            cx,
            g,
            name,
            outVertexKey.get(),
            outVertexLabelKey.get(),
            fields),
          (java.util.function.Function<hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (mOutSpec -> hydra.lib.eithers.Bind.apply(
            hydra.pg.coder.Coder.findProjectionSpec(
              cx,
              g,
              name,
              inVertexKey.get(),
              inVertexLabelKey.get(),
              fields),
            (java.util.function.Function<hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (mInSpec -> {
              hydra.util.Lazy<hydra.pg.model.ElementKind> kind = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
                hydra.pg.coder.Coder.hasVertexAdapters(
                  dir.get(),
                  mOutSpec,
                  mInSpec),
                () -> new hydra.pg.model.ElementKind.Edge(),
                () -> new hydra.pg.model.ElementKind.Vertex()));
              return hydra.lib.eithers.Bind.apply(
                hydra.pg.coder.Coder.<T0, T1, T2>findPropertySpecs(
                  cx,
                  g,
                  schema,
                  kind.get(),
                  fields),
                (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (propSpecs -> hydra.lib.eithers.Bind.apply(
                  hydra.lib.eithers.MapList.apply(
                    (java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T1>, hydra.core.Field, hydra.pg.model.Property<T2>>>>) (v1 -> hydra.pg.coder.Coder.propertyAdapter(
                      cx,
                      g,
                      schema,
                      v1)),
                    propSpecs),
                  (java.util.function.Function<java.util.List<hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T1>, hydra.core.Field, hydra.pg.model.Property<T2>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (propAdapters -> (kind.get()).accept(new hydra.pg.model.ElementKind.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>> visit(hydra.pg.model.ElementKind.Vertex ignored) {
                      return hydra.pg.coder.Coder.<T0, T1, T2>constructVertexCoder(
                        cx,
                        g,
                        schema,
                        source,
                        vidType,
                        eidType,
                        name,
                        fields,
                        propAdapters);
                    }
                    
                    @Override
                    public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>> visit(hydra.pg.model.ElementKind.Edge ignored) {
                      return hydra.pg.coder.Coder.<T0, T1, T2>constructEdgeCoder(
                        cx,
                        g,
                        parentLabel.get(),
                        schema,
                        source,
                        vidType,
                        eidType,
                        dir.get(),
                        name,
                        fields,
                        propAdapters,
                        mOutSpec,
                        mInSpec);
                    }
                  })))));
            }))));
      }
    });
  }
  
  static <T0> hydra.pg.model.ElementTree<T0> elementTreeEdge(hydra.pg.model.Edge<T0> edge, java.util.List<hydra.pg.model.ElementTree<T0>> deps) {
    return (hydra.pg.model.ElementTree<T0>) (new hydra.pg.model.ElementTree<T0>((hydra.pg.model.Element<T0>) (new hydra.pg.model.Element.Edge(edge)), deps));
  }
  
  static <T0> hydra.pg.model.ElementTree<T0> elementTreeVertex(hydra.pg.model.Vertex<T0> vertex, java.util.List<hydra.pg.model.ElementTree<T0>> deps) {
    return (hydra.pg.model.ElementTree<T0>) (new hydra.pg.model.ElementTree<T0>((hydra.pg.model.Element<T0>) (new hydra.pg.model.Element.Vertex(vertex)), deps));
  }
  
  static <T0> hydra.pg.model.ElementTypeTree<T0> elementTypeTreeEdge(hydra.pg.model.EdgeType<T0> etype, java.util.List<hydra.pg.model.ElementTypeTree<T0>> deps) {
    return (hydra.pg.model.ElementTypeTree<T0>) (new hydra.pg.model.ElementTypeTree<T0>((hydra.pg.model.ElementType<T0>) (new hydra.pg.model.ElementType.Edge(etype)), deps));
  }
  
  static <T0> hydra.pg.model.ElementTypeTree<T0> elementTypeTreeVertex(hydra.pg.model.VertexType<T0> vtype, java.util.List<hydra.pg.model.ElementTypeTree<T0>> deps) {
    return (hydra.pg.model.ElementTypeTree<T0>) (new hydra.pg.model.ElementTypeTree<T0>((hydra.pg.model.ElementType<T0>) (new hydra.pg.model.ElementType.Vertex(vtype)), deps));
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.Map<hydra.pg.model.PropertyKey, T1>> encodeProperties(hydra.context.Context cx, java.util.Map<hydra.core.Name, hydra.core.Term> fields, java.util.List<hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, hydra.pg.model.Property<T1>>> adapters) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<java.util.List<hydra.pg.model.Property<T1>>, java.util.Map<hydra.pg.model.PropertyKey, T1>>) (props -> hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.pg.model.Property<T1>, hydra.util.Pair<hydra.pg.model.PropertyKey, T1>>) (prop -> (hydra.util.Pair<hydra.pg.model.PropertyKey, T1>) ((hydra.util.Pair<hydra.pg.model.PropertyKey, T1>) (new hydra.util.Pair<hydra.pg.model.PropertyKey, T1>(((java.util.function.Function<hydra.pg.model.Property<T1>, hydra.pg.model.PropertyKey>) (projected -> projected.key)).apply(prop), ((java.util.function.Function<hydra.pg.model.Property<T1>, T1>) (projected -> projected.value)).apply(prop))))),
        props))),
      hydra.lib.eithers.Map.apply(
        (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.pg.model.Property<T1>>>, java.util.List<hydra.pg.model.Property<T1>>>) (xs -> hydra.lib.maybes.Cat.apply(xs)),
        hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, hydra.pg.model.Property<T1>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.pg.model.Property<T1>>>>) (v1 -> hydra.pg.coder.Coder.encodeProperty(
            cx,
            fields,
            v1)),
          adapters)));
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T1>> encodeProperty(hydra.context.Context cx, java.util.Map<hydra.core.Name, hydra.core.Term> fields, hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, T1> adapter) {
    hydra.util.Lazy<hydra.core.Name> fname = new hydra.util.Lazy<>(() -> (((java.util.function.Function<hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, T1>, hydra.core.FieldType>) ((java.util.function.Function<hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, T1>, hydra.core.FieldType>) ((java.util.function.Function<hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, T1>, hydra.core.FieldType>) ((java.util.function.Function<hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, T1>, hydra.core.FieldType>) (projected -> projected.source))))).apply(adapter)).name);
    java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T1>>> encodeValue = (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T1>>>) (v1 -> hydra.pg.coder.Coder.<T0, T1>encodeProperty_encodeValue(
      adapter,
      cx,
      fname.get(),
      v1));
    hydra.util.Lazy<hydra.core.Type> ftyp = new hydra.util.Lazy<>(() -> hydra.rewriting.Rewriting.deannotateType((((java.util.function.Function<hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, T1>, hydra.core.FieldType>) ((java.util.function.Function<hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, T1>, hydra.core.FieldType>) ((java.util.function.Function<hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, T1>, hydra.core.FieldType>) ((java.util.function.Function<hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, T1>, hydra.core.FieldType>) (projected -> projected.source))))).apply(adapter)).type));
    Boolean isMaybe = (ftyp.get()).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.Type instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Maybe ignored) {
        return true;
      }
    });
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.lib.logic.IfElse.lazy(
        isMaybe,
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T1>>right((hydra.util.Maybe<T1>) (hydra.util.Maybe.<T1>nothing())),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T1>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          "expected field not found in record: ",
          (fname.get()).value))), cx)))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T1>>>) (value -> hydra.lib.logic.IfElse.lazy(
        isMaybe,
        () -> (hydra.rewriting.Rewriting.deannotateTerm(value)).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T1>> otherwise(hydra.core.Term instance) {
            return (encodeValue).apply(value);
          }
          
          @Override
          public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T1>> visit(hydra.core.Term.Maybe ov) {
            return hydra.lib.maybes.Maybe.applyLazy(
              () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T1>>right((hydra.util.Maybe<T1>) (hydra.util.Maybe.<T1>nothing())),
              encodeValue,
              (ov).value);
          }
        }),
        () -> (encodeValue).apply(value))),
      hydra.lib.maps.Lookup.apply(
        fname.get(),
        fields));
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<T1>> encodeProperty_encodeValue(hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, T1> adapter, hydra.context.Context cx, hydra.core.Name fname, hydra.core.Term v) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<T1, hydra.util.Maybe<T1>>) (x -> hydra.util.Maybe.just(x)),
      ((((java.util.function.Function<hydra.util.Coder<hydra.core.Field, T1>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T1>>>>) ((java.util.function.Function<hydra.util.Coder<hydra.core.Field, T1>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T1>>>>) (projected -> projected.encode))).apply(((java.util.function.Function<hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, T1>, hydra.util.Coder<hydra.core.Field, T1>>) ((java.util.function.Function<hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, T1>, hydra.util.Coder<hydra.core.Field, T1>>) ((java.util.function.Function<hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, T1>, hydra.util.Coder<hydra.core.Field, T1>>) ((java.util.function.Function<hydra.util.Adapter<hydra.core.FieldType, T0, hydra.core.Field, T1>, hydra.util.Coder<hydra.core.Field, T1>>) (projected -> projected.coder))))).apply(adapter))).apply(cx)).apply(new hydra.core.Field(fname, v)));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, String> extractString(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Term t) {
    return hydra.extract.core.Core.string(
      cx,
      g,
      t);
  }
  
  static <T0, T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>>> findAdjacenEdgeAdapters(hydra.context.Context cx, hydra.graph.Graph g, hydra.pg.mapping.Schema<T0, T1, T2> schema, T1 vidType, T1 eidType, hydra.pg.model.VertexLabel parentLabel, hydra.pg.model.Direction dir, java.util.List<hydra.core.FieldType> fields) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>>>, java.util.List<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>>>) (xs -> hydra.lib.maybes.Cat.apply(xs)),
      hydra.lib.eithers.MapList.apply(
        (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>>>>) (field -> {
          hydra.util.Lazy<hydra.core.Name> key = new hydra.util.Lazy<>(() -> new hydra.core.Name((dir).accept(new hydra.pg.model.Direction.PartialVisitor<>() {
            @Override
            public String visit(hydra.pg.model.Direction.Out ignored) {
              return (((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) (projected -> projected.annotations)))).apply(schema)).outEdgeLabel;
            }
            
            @Override
            public String visit(hydra.pg.model.Direction.In ignored) {
              return (((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) (projected -> projected.annotations)))).apply(schema)).inEdgeLabel;
            }
          })));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>>>right((hydra.util.Maybe<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>>nothing())),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>>>>) (a -> hydra.lib.eithers.Bind.apply(
              hydra.pg.coder.Coder.extractString(
                cx,
                g,
                a),
              (java.util.function.Function<String, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>>>>) (labelStr -> hydra.lib.eithers.Bind.apply(
                hydra.pg.coder.Coder.<T0, T1, T2>elementCoder(
                  hydra.util.Maybe.just((hydra.util.Pair<hydra.pg.model.Direction, hydra.pg.model.VertexLabel>) ((hydra.util.Pair<hydra.pg.model.Direction, hydra.pg.model.VertexLabel>) (new hydra.util.Pair<hydra.pg.model.Direction, hydra.pg.model.VertexLabel>(dir, parentLabel)))),
                  schema,
                  (field).type,
                  vidType,
                  eidType,
                  cx,
                  g),
                (java.util.function.Function<hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>>>>) (elad -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>>>right(hydra.util.Maybe.just((hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>) ((hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>) (new hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>(dir, (hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) ((hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>) (new hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>(field, (hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>) ((hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>) (new hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>(new hydra.pg.model.EdgeLabel(labelStr), elad)))))))))))))))),
            hydra.annotations.Annotations.getTypeAnnotation(
              key.get(),
              (field).type));
        }),
        fields));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>> findIdProjectionSpec(hydra.context.Context cx, Boolean required, hydra.core.Name tname, hydra.core.Name idKey, java.util.List<hydra.core.FieldType> fields) {
    return hydra.lib.eithers.Bind.apply(
      hydra.pg.coder.Coder.findSingleFieldWithAnnotationKey(
        cx,
        tname,
        idKey,
        fields),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>>>) (mid -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.lib.logic.IfElse.lazy(
          required,
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            hydra.lib.strings.Cat2.apply(
              "no ",
              (idKey).value),
            " field"))), cx))),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>>right((hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>nothing()))),
        (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>>>) (mi -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>>) (spec -> hydra.util.Maybe.just((hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>) ((hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>) (new hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>(mi, (hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>) ((hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>) (new hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>(spec, hydra.lib.maybes.Map.apply(
            (java.util.function.Function<String, String>) (s -> hydra.lib.strings.ToUpper.apply(s)),
            (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())))))))))),
          hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>right(new hydra.pg.mapping.ValueSpec.Value()),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>) (v1 -> hydra.pg.termsToElements.TermsToElements.decodeValueSpec(
              cx,
              new hydra.graph.Graph((java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) ((java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeVariableMetadata>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply()), (java.util.Map<hydra.core.Name, hydra.core.Term>) ((java.util.Map<hydra.core.Name, hydra.core.Term>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Term>apply())), (java.util.Map<hydra.core.Name, hydra.graph.Primitive>) ((java.util.Map<hydra.core.Name, hydra.graph.Primitive>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.graph.Primitive>apply())), (java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) ((java.util.Map<hydra.core.Name, hydra.core.TypeScheme>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.TypeScheme>apply())), (java.util.Set<hydra.core.Name>) (hydra.lib.sets.Empty.<hydra.core.Name>apply())),
              v1)),
            hydra.annotations.Annotations.getTypeAnnotation(
              idKey,
              (mi).type)))),
        mid)));
  }
  
  static <T0, T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>> findIncidentVertexAdapter(hydra.context.Context cx, hydra.graph.Graph g, hydra.pg.mapping.Schema<T0, T1, T2> schema, T1 vidType, T1 eidType, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>> spec) {
    hydra.util.Lazy<hydra.core.FieldType> field = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(spec));
    return hydra.lib.eithers.Bind.apply(
      hydra.pg.coder.Coder.<T0, T1, T2>elementCoder(
        (hydra.util.Maybe<hydra.util.Pair<hydra.pg.model.Direction, hydra.pg.model.VertexLabel>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.pg.model.Direction, hydra.pg.model.VertexLabel>>nothing()),
        schema,
        (field.get()).type,
        vidType,
        eidType,
        cx,
        g),
      (java.util.function.Function<hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>>) (adapter -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>>right((hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>) ((hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>) (new hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, hydra.pg.model.ElementTypeTree<T1>, hydra.core.Term, hydra.pg.model.ElementTree<T2>>>((field.get()).name, adapter))))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, String> findLabelString(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Type source, hydra.core.Name tname, hydra.core.Name labelKey) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, String>right((tname).value),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, String>>) (v1 -> hydra.pg.coder.Coder.extractString(
        cx,
        g,
        v1)),
      hydra.annotations.Annotations.getTypeAnnotation(
        labelKey,
        source));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>> findProjectionSpec(hydra.context.Context cx, hydra.graph.Graph g, hydra.core.Name tname, hydra.core.Name key, hydra.core.Name aliasKey, java.util.List<hydra.core.FieldType> fields) {
    return hydra.lib.eithers.Bind.apply(
      hydra.pg.coder.Coder.findSingleFieldWithAnnotationKey(
        cx,
        tname,
        key,
        fields),
      (java.util.function.Function<hydra.util.Maybe<hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>>>) (mfield -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>>right((hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>) (hydra.util.Maybe.<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>nothing())),
        (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>>>) (field -> hydra.lib.eithers.Bind.apply(
          hydra.pg.termsToElements.TermsToElements.decodeValueSpec(
            cx,
            g,
            hydra.lib.maybes.FromJust.apply(hydra.annotations.Annotations.getTypeAnnotation(
              key,
              (field).type))),
          (java.util.function.Function<hydra.pg.mapping.ValueSpec, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>>>) (spec -> hydra.lib.eithers.Bind.apply(
            hydra.lib.maybes.Maybe.applyLazy(
              () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<String>>right((hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<String>>>) (t -> hydra.lib.eithers.Map.apply(
                (java.util.function.Function<String, hydra.util.Maybe<String>>) (x -> hydra.util.Maybe.just(x)),
                hydra.pg.coder.Coder.extractString(
                  cx,
                  g,
                  t))),
              hydra.annotations.Annotations.getTypeAnnotation(
                aliasKey,
                (field).type)),
            (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>>>) (alias -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>>right(hydra.util.Maybe.just((hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>) ((hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>) (new hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>(field, (hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>) ((hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>) (new hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>(spec, alias))))))))))))),
        mfield)));
  }
  
  static <T0, T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>> findPropertySpecs(hydra.context.Context cx, hydra.graph.Graph g, hydra.pg.mapping.Schema<T0, T1, T2> schema, hydra.pg.model.ElementKind kind, java.util.List<hydra.core.FieldType> fields) {
    return hydra.lib.eithers.MapList.apply(
      (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>>) (field -> {
        hydra.util.Lazy<hydra.core.Name> propKeyKey = new hydra.util.Lazy<>(() -> new hydra.core.Name((((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) (projected -> projected.annotations)))).apply(schema)).propertyKey));
        hydra.util.Lazy<hydra.core.Name> propValueKey = new hydra.util.Lazy<>(() -> new hydra.core.Name((((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) (projected -> projected.annotations)))).apply(schema)).propertyValue));
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<String>>right((hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<String>>>) (a -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<String, hydra.util.Maybe<String>>) (x -> hydra.util.Maybe.just(x)),
              hydra.pg.coder.Coder.extractString(
                cx,
                g,
                a))),
            hydra.annotations.Annotations.getTypeAnnotation(
              propKeyKey.get(),
              (field).type)),
          (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>>) (alias -> hydra.lib.eithers.Bind.apply(
            hydra.lib.maybes.Maybe.applyLazy(
              () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>right(new hydra.pg.mapping.ValueSpec.Value()),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.mapping.ValueSpec>>) (v1 -> hydra.pg.termsToElements.TermsToElements.decodeValueSpec(
                cx,
                g,
                v1)),
              hydra.annotations.Annotations.getTypeAnnotation(
                propValueKey.get(),
                (field).type)),
            (java.util.function.Function<hydra.pg.mapping.ValueSpec, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>>) (values -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>right((hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>) ((hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>) (new hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>(field, (hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>) ((hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>) (new hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>(values, alias)))))))))));
      }),
      hydra.lib.lists.Filter.apply(
        (java.util.function.Function<hydra.core.FieldType, Boolean>) (field -> {
          hydra.util.Lazy<hydra.pg.mapping.AnnotationSchema> annots = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) ((java.util.function.Function<hydra.pg.mapping.Schema<T0, T1, T2>, hydra.pg.mapping.AnnotationSchema>) (projected -> projected.annotations)))).apply(schema));
          hydra.core.Name ignoreKey = new hydra.core.Name((annots.get()).ignore);
          java.util.List<hydra.core.Name> specialKeys = (kind).accept(new hydra.pg.model.ElementKind.PartialVisitor<>() {
            @Override
            public java.util.List<hydra.core.Name> visit(hydra.pg.model.ElementKind.Vertex ignored) {
              return java.util.List.of(
                new hydra.core.Name((annots.get()).vertexId),
                new hydra.core.Name((annots.get()).outEdgeLabel),
                new hydra.core.Name((annots.get()).inEdgeLabel));
            }
            
            @Override
            public java.util.List<hydra.core.Name> visit(hydra.pg.model.ElementKind.Edge ignored) {
              return java.util.List.of(
                new hydra.core.Name((annots.get()).edgeId),
                new hydra.core.Name((annots.get()).outVertex),
                new hydra.core.Name((annots.get()).inVertex));
            }
          });
          hydra.util.Lazy<java.util.List<hydra.core.Name>> allKeys = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(java.util.List.of(
            java.util.List.of(ignoreKey),
            specialKeys)));
          hydra.util.Lazy<Boolean> hasSpecialAnnotation = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Name, Boolean>>) (b -> (java.util.function.Function<hydra.core.Name, Boolean>) (k -> hydra.lib.logic.Or.apply(
              b,
              hydra.lib.maybes.IsJust.apply(hydra.annotations.Annotations.getTypeAnnotation(
                k,
                (field).type))))),
            false,
            allKeys.get()));
          hydra.util.Lazy<Boolean> hasSpecialFieldName = new hydra.util.Lazy<>(() -> hydra.lib.lists.Foldl.apply(
            (java.util.function.Function<Boolean, java.util.function.Function<hydra.core.Name, Boolean>>) (b -> (java.util.function.Function<hydra.core.Name, Boolean>) (k -> hydra.lib.logic.Or.apply(
              b,
              hydra.lib.equality.Equal.apply(
                (field).name,
                k)))),
            false,
            specialKeys));
          return hydra.lib.logic.Not.apply(hydra.lib.logic.Or.apply(
            hasSpecialAnnotation.get(),
            hasSpecialFieldName.get()));
        }),
        fields));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.FieldType>> findSingleFieldWithAnnotationKey(hydra.context.Context cx, hydra.core.Name tname, hydra.core.Name key, java.util.List<hydra.core.FieldType> fields) {
    hydra.util.Lazy<java.util.List<hydra.core.FieldType>> matches = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.FieldType, Boolean>) (f -> hydra.lib.maybes.IsJust.apply(hydra.annotations.Annotations.getTypeAnnotation(
        key,
        (f).type))),
      fields));
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Gt.apply(
        hydra.lib.lists.Length.apply(matches.get()),
        1),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.FieldType>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            "Multiple fields marked as '",
            (key).value),
          "' in record type "),
        (tname).value))), cx))),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.core.FieldType>>right(hydra.lib.lists.SafeHead.apply(matches.get())));
  }
  
  static <T0, T1> Boolean hasVertexAdapters(hydra.pg.model.Direction dir, hydra.util.Maybe<T0> mOutSpec, hydra.util.Maybe<T1> mInSpec) {
    return (dir).accept(new hydra.pg.model.Direction.PartialVisitor<>() {
      @Override
      public Boolean visit(hydra.pg.model.Direction.Out ignored) {
        return hydra.lib.maybes.IsJust.apply(mInSpec);
      }
      
      @Override
      public Boolean visit(hydra.pg.model.Direction.In ignored) {
        return hydra.lib.maybes.IsJust.apply(mOutSpec);
      }
      
      @Override
      public Boolean visit(hydra.pg.model.Direction.Both ignored) {
        return hydra.lib.logic.And.apply(
          hydra.lib.maybes.IsJust.apply(mOutSpec),
          hydra.lib.maybes.IsJust.apply(mInSpec));
      }
    });
  }
  
  static <T0, T1, T2, T3, T4, T5> hydra.util.Either<T5, hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T2, hydra.core.Term, T3>>> projectionAdapter(T0 cx, T1 g, T2 idtype, hydra.util.Coder<hydra.core.Term, T3> coder, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, T4>> spec, String key) {
    hydra.util.Lazy<hydra.core.FieldType> field = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(spec));
    hydra.util.Lazy<hydra.pg.mapping.ValueSpec> values = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(spec)));
    return hydra.lib.eithers.Bind.apply(
      hydra.pg.termsToElements.TermsToElements.<T0, T1, T5>parseValueSpec(
        cx,
        g,
        values.get()),
      (java.util.function.Function<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>>, hydra.util.Either<T5, hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T2, hydra.core.Term, T3>>>>) (traversal -> hydra.util.Either.<T5, hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T2, hydra.core.Term, T3>>>right((hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T2, hydra.core.Term, T3>>) ((hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T2, hydra.core.Term, T3>>) (new hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T2, hydra.core.Term, T3>>((field.get()).name, (hydra.util.Adapter<hydra.core.Type, T2, hydra.core.Term, T3>) ((hydra.util.Adapter<hydra.core.Type, T2, hydra.core.Term, T3>) ((hydra.util.Adapter<hydra.core.Type, T2, hydra.core.Term, T3>) ((hydra.util.Adapter<hydra.core.Type, T2, hydra.core.Term, T3>) (new hydra.util.Adapter<hydra.core.Type, T2, hydra.core.Term, T3>(true, (field.get()).type, idtype, (hydra.util.Coder<hydra.core.Term, T3>) ((hydra.util.Coder<hydra.core.Term, T3>) (new hydra.util.Coder<hydra.core.Term, T3>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3>>>) (cx_ -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3>>) (typ -> hydra.lib.eithers.Bind.apply(
        hydra.pg.coder.Coder.traverseToSingleTerm(
          cx_,
          hydra.lib.strings.Cat2.apply(
            key,
            "-projection"),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>) (v1 -> ((traversal).apply(cx_)).apply(v1)),
          typ),
        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3>>) (t -> ((((java.util.function.Function<hydra.util.Coder<hydra.core.Term, T3>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3>>>>) ((java.util.function.Function<hydra.util.Coder<hydra.core.Term, T3>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3>>>>) (projected -> projected.encode))).apply(coder)).apply(cx_)).apply(t))))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<T3, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>) (cx_ -> (java.util.function.Function<T3, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (ignored -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "edge '",
          key),
        "' decoding is not yet supported"))), cx_)))))))))))))))))));
  }
  
  static <T0, T1, T2, T3> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T2>, hydra.core.Field, hydra.pg.model.Property<T3>>> propertyAdapter(hydra.context.Context cx, T0 g, hydra.pg.mapping.Schema<T1, T2, T3> schema, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>> spec) {
    hydra.util.Lazy<hydra.util.Maybe<String>> alias = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(spec)));
    hydra.util.Lazy<hydra.core.FieldType> tfield = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(spec));
    hydra.util.Lazy<hydra.pg.model.PropertyKey> key = new hydra.util.Lazy<>(() -> new hydra.pg.model.PropertyKey(hydra.lib.maybes.FromMaybe.applyLazy(
      () -> ((tfield.get()).name).value,
      alias.get())));
    hydra.util.Lazy<hydra.pg.mapping.ValueSpec> values = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(spec)));
    return hydra.lib.eithers.Bind.apply(
      ((((java.util.function.Function<hydra.util.Coder<hydra.core.Type, T2>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T2>>>>) ((java.util.function.Function<hydra.util.Coder<hydra.core.Type, T2>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T2>>>>) (projected -> projected.encode))).apply(((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, hydra.util.Coder<hydra.core.Type, T2>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, hydra.util.Coder<hydra.core.Type, T2>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, hydra.util.Coder<hydra.core.Type, T2>>) (projected -> projected.propertyTypes)))).apply(schema))).apply(cx)).apply((tfield.get()).type),
      (java.util.function.Function<T2, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T2>, hydra.core.Field, hydra.pg.model.Property<T3>>>>) (pt -> hydra.lib.eithers.Bind.apply(
        hydra.pg.termsToElements.TermsToElements.parseValueSpec(
          cx,
          g,
          values.get()),
        (java.util.function.Function<java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T2>, hydra.core.Field, hydra.pg.model.Property<T3>>>>) (traversal -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T2>, hydra.core.Field, hydra.pg.model.Property<T3>>>right((hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T2>, hydra.core.Field, hydra.pg.model.Property<T3>>) ((hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T2>, hydra.core.Field, hydra.pg.model.Property<T3>>) ((hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T2>, hydra.core.Field, hydra.pg.model.Property<T3>>) ((hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T2>, hydra.core.Field, hydra.pg.model.Property<T3>>) (new hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T2>, hydra.core.Field, hydra.pg.model.Property<T3>>(true, tfield.get(), (hydra.pg.model.PropertyType<T2>) (new hydra.pg.model.PropertyType<T2>(key.get(), pt, true)), (hydra.util.Coder<hydra.core.Field, hydra.pg.model.Property<T3>>) ((hydra.util.Coder<hydra.core.Field, hydra.pg.model.Property<T3>>) (new hydra.util.Coder<hydra.core.Field, hydra.pg.model.Property<T3>>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.Property<T3>>>>) (cx_ -> (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.Property<T3>>>) (dfield -> hydra.lib.eithers.Bind.apply(
          hydra.pg.coder.Coder.traverseToSingleTerm(
            cx_,
            "property traversal",
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.core.Term>>>) (v1 -> ((traversal).apply(cx_)).apply(v1)),
            (dfield).term),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.Property<T3>>>) (result -> hydra.lib.eithers.Bind.apply(
            ((((java.util.function.Function<hydra.util.Coder<hydra.core.Term, T3>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3>>>>) ((java.util.function.Function<hydra.util.Coder<hydra.core.Term, T3>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3>>>>) (projected -> projected.encode))).apply(((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, hydra.util.Coder<hydra.core.Term, T3>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, hydra.util.Coder<hydra.core.Term, T3>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, hydra.util.Coder<hydra.core.Term, T3>>) (projected -> projected.propertyValues)))).apply(schema))).apply(cx_)).apply(result),
            (java.util.function.Function<T3, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.Property<T3>>>) (value -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.Property<T3>>right((hydra.pg.model.Property<T3>) (new hydra.pg.model.Property<T3>(key.get(), value))))))))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.pg.model.Property<T3>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>>>) (cx_ -> (java.util.function.Function<hydra.pg.model.Property<T3>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>>) (ignored -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError("property decoding is not yet supported")), cx_))))))))))))))))));
  }
  
  static <T0, T1, T2, T3> java.util.List<hydra.pg.model.PropertyType<T1>> propertyTypes(java.util.List<hydra.util.Adapter<T0, hydra.pg.model.PropertyType<T1>, T2, T3>> propAdapters) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Adapter<T0, hydra.pg.model.PropertyType<T1>, T2, T3>, hydra.pg.model.PropertyType<T1>>) (a -> (hydra.pg.model.PropertyType<T1>) (new hydra.pg.model.PropertyType<T1>(((java.util.function.Function<hydra.pg.model.PropertyType<T1>, hydra.pg.model.PropertyKey>) (projected -> projected.key)).apply(((java.util.function.Function<hydra.util.Adapter<T0, hydra.pg.model.PropertyType<T1>, T2, T3>, hydra.pg.model.PropertyType<T1>>) ((java.util.function.Function<hydra.util.Adapter<T0, hydra.pg.model.PropertyType<T1>, T2, T3>, hydra.pg.model.PropertyType<T1>>) ((java.util.function.Function<hydra.util.Adapter<T0, hydra.pg.model.PropertyType<T1>, T2, T3>, hydra.pg.model.PropertyType<T1>>) ((java.util.function.Function<hydra.util.Adapter<T0, hydra.pg.model.PropertyType<T1>, T2, T3>, hydra.pg.model.PropertyType<T1>>) (projected -> projected.target))))).apply(a)), ((java.util.function.Function<hydra.pg.model.PropertyType<T1>, T1>) (projected -> projected.value)).apply(((java.util.function.Function<hydra.util.Adapter<T0, hydra.pg.model.PropertyType<T1>, T2, T3>, hydra.pg.model.PropertyType<T1>>) ((java.util.function.Function<hydra.util.Adapter<T0, hydra.pg.model.PropertyType<T1>, T2, T3>, hydra.pg.model.PropertyType<T1>>) ((java.util.function.Function<hydra.util.Adapter<T0, hydra.pg.model.PropertyType<T1>, T2, T3>, hydra.pg.model.PropertyType<T1>>) ((java.util.function.Function<hydra.util.Adapter<T0, hydra.pg.model.PropertyType<T1>, T2, T3>, hydra.pg.model.PropertyType<T1>>) (projected -> projected.target))))).apply(a)), true))),
      propAdapters);
  }
  
  static <T0, T1, T2, T3> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3> selectEdgeId(hydra.context.Context cx, java.util.Map<hydra.core.Name, T0> fields, hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T1, T2, T0, T3>> ad) {
    hydra.util.Lazy<hydra.core.Name> fname = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(ad));
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T3>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "no ",
          (fname.get()).value),
        " in record"))), cx))),
      (java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3>>) (t -> ((((java.util.function.Function<hydra.util.Coder<T0, T3>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3>>>>) ((java.util.function.Function<hydra.util.Coder<T0, T3>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3>>>>) (projected -> projected.encode))).apply(((java.util.function.Function<hydra.util.Adapter<T1, T2, T0, T3>, hydra.util.Coder<T0, T3>>) ((java.util.function.Function<hydra.util.Adapter<T1, T2, T0, T3>, hydra.util.Coder<T0, T3>>) ((java.util.function.Function<hydra.util.Adapter<T1, T2, T0, T3>, hydra.util.Coder<T0, T3>>) ((java.util.function.Function<hydra.util.Adapter<T1, T2, T0, T3>, hydra.util.Coder<T0, T3>>) (projected -> projected.coder))))).apply(hydra.pg.coder.Coder.<T0, T1, T2, T3>selectEdgeId_adapter(ad)))).apply(cx)).apply(t)),
      hydra.lib.maps.Lookup.apply(
        fname.get(),
        fields));
  }
  
  static <T0, T1, T2, T3> hydra.util.Adapter<T1, T2, T0, T3> selectEdgeId_adapter(hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T1, T2, T0, T3>> ad) {
    return hydra.lib.pairs.Second.apply(ad);
  }
  
  static <T0, T1, T2, T3> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3> selectVertexId(hydra.context.Context cx, java.util.Map<hydra.core.Name, T0> fields, hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T1, T2, T0, T3>> ad) {
    hydra.util.Lazy<hydra.core.Name> fname = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(ad));
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T3>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
        hydra.lib.strings.Cat2.apply(
          "no ",
          (fname.get()).value),
        " in record"))), cx))),
      (java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3>>) (t -> ((((java.util.function.Function<hydra.util.Coder<T0, T3>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3>>>>) ((java.util.function.Function<hydra.util.Coder<T0, T3>, java.util.function.Function<hydra.context.Context, java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T3>>>>) (projected -> projected.encode))).apply(((java.util.function.Function<hydra.util.Adapter<T1, T2, T0, T3>, hydra.util.Coder<T0, T3>>) ((java.util.function.Function<hydra.util.Adapter<T1, T2, T0, T3>, hydra.util.Coder<T0, T3>>) ((java.util.function.Function<hydra.util.Adapter<T1, T2, T0, T3>, hydra.util.Coder<T0, T3>>) ((java.util.function.Function<hydra.util.Adapter<T1, T2, T0, T3>, hydra.util.Coder<T0, T3>>) (projected -> projected.coder))))).apply(hydra.pg.coder.Coder.<T0, T1, T2, T3>selectVertexId_adapter(ad)))).apply(cx)).apply(t)),
      hydra.lib.maps.Lookup.apply(
        fname.get(),
        fields));
  }
  
  static <T0, T1, T2, T3> hydra.util.Adapter<T1, T2, T0, T3> selectVertexId_adapter(hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T1, T2, T0, T3>> ad) {
    return hydra.lib.pairs.Second.apply(ad);
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T1> traverseToSingleTerm(hydra.context.Context cx, String desc, java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<T1>>> traversal, T0 term) {
    return hydra.lib.eithers.Bind.apply(
      (traversal).apply(term),
      (java.util.function.Function<java.util.List<T1>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T1>>) (terms -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.lists.Null.apply(terms),
        () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T1>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          desc,
          " did not resolve to a term"))), cx))),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            hydra.lib.lists.Length.apply(terms),
            1),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T1>right(hydra.lib.lists.Head.apply(terms)),
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T1>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            desc,
            " resolved to multiple terms"))), cx)))))));
  }
  
  static <T0, T1, T2, T3, T4, T5, T6, T7, T8, T9> hydra.util.Adapter<T4, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>> vertexCoder(T0 g, hydra.pg.mapping.Schema<T1, T2, T3> schema, T4 source, T5 vidType, T6 tname, hydra.pg.model.VertexLabel vlabel, hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<T7, T8, hydra.core.Term, T3>> idAdapter, java.util.List<hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T5>, hydra.core.Field, hydra.pg.model.Property<T3>>> propAdapters, java.util.List<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<T9, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>>>>> edgeAdapters) {
    return (hydra.util.Adapter<T4, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>) ((hydra.util.Adapter<T4, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>) ((hydra.util.Adapter<T4, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>) ((hydra.util.Adapter<T4, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>) (new hydra.util.Adapter<T4, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>(true, source, hydra.pg.coder.Coder.<T3, T5, T9>vertexCoder_target(
      edgeAdapters,
      propAdapters,
      vidType,
      vlabel), (hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>) ((hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>) (new hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>>) (cx -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>) (term -> {
      hydra.core.Term deannot = hydra.rewriting.Rewriting.deannotateTerm(term);
      hydra.util.Lazy<hydra.core.Term> unwrapped = new hydra.util.Lazy<>(() -> (deannot).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Term otherwise(hydra.core.Term instance) {
          return deannot;
        }
        
        @Override
        public hydra.core.Term visit(hydra.core.Term.Maybe mt) {
          return hydra.lib.maybes.FromMaybe.applyLazy(
            () -> deannot,
            (mt).value);
        }
      }));
      hydra.core.Record rec = (unwrapped.get()).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.core.Record visit(hydra.core.Term.Record r) {
          return (r).value;
        }
      });
      java.util.Map<hydra.core.Name, hydra.core.Term> fmap = hydra.schemas.Schemas.fieldMap((rec).fields);
      return hydra.lib.eithers.Bind.apply(
        hydra.pg.coder.Coder.selectVertexId(
          cx,
          fmap,
          idAdapter),
        (java.util.function.Function<T3, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>) (vid -> hydra.lib.eithers.Bind.apply(
          hydra.pg.coder.Coder.encodeProperties(
            cx,
            fmap,
            propAdapters),
          (java.util.function.Function<java.util.Map<hydra.pg.model.PropertyKey, T3>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>) (props -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<java.util.List<hydra.pg.model.ElementTree<T3>>>, java.util.List<hydra.pg.model.ElementTree<T3>>>) (xs -> hydra.lib.lists.Concat.apply(xs)),
              hydra.lib.eithers.MapList.apply(
                (java.util.function.Function<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<T9, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.ElementTree<T3>>>>) (ea -> {
                  hydra.util.Lazy<hydra.pg.model.Direction> eaDir = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(ea));
                  hydra.util.Lazy<hydra.core.FieldType> eaField = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(ea)));
                  hydra.util.Lazy<hydra.pg.model.EdgeLabel> eaLabel = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(ea))));
                  return hydra.lib.maybes.Maybe.applyLazy(
                    () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.ElementTree<T3>>>right((java.util.List<hydra.pg.model.ElementTree<T3>>) (java.util.List.<hydra.pg.model.ElementTree<T3>>of())),
                    (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, java.util.List<hydra.pg.model.ElementTree<T3>>>>) (fterm -> hydra.lib.eithers.Map.apply(
                      (java.util.function.Function<hydra.pg.model.ElementTree<T3>, java.util.List<hydra.pg.model.ElementTree<T3>>>) (tree -> ((java.util.function.Function<hydra.pg.model.Element, java.util.List<hydra.pg.model.ElementTree<T3>>>) (v1 -> ((java.util.function.Function<hydra.pg.model.Element<T3>, java.util.List<hydra.pg.model.ElementTree<T3>>>) ((java.util.function.Function<hydra.pg.model.Element<T3>, java.util.List<hydra.pg.model.ElementTree<T3>>>) (u -> (u).accept(new hydra.pg.model.Element.PartialVisitor<>() {
                        @Override
                        public java.util.List<hydra.pg.model.ElementTree<T3>> visit(hydra.pg.model.Element.Vertex<T3> vtx) {
                          return java.util.List.of((hydra.pg.model.ElementTree<T3>) (new hydra.pg.model.ElementTree<T3>(hydra.pg.coder.Coder.<T1, T2, T3>vertexCoder_edge(
                            eaDir.get(),
                            eaLabel.get(),
                            schema,
                            vid,
                            (vtx).value), java.util.List.of(tree))));
                        }
                        
                        @Override
                        public java.util.List<hydra.pg.model.ElementTree<T3>> visit(hydra.pg.model.Element.Edge<T3> edg) {
                          return java.util.List.of((hydra.pg.model.ElementTree<T3>) (new hydra.pg.model.ElementTree<T3>((hydra.pg.model.Element<T3>) (new hydra.pg.model.Element.Edge(hydra.pg.coder.Coder.<T3>vertexCoder_fixedEdge(
                            eaDir.get(),
                            (edg).value,
                            vid))), ((java.util.function.Function<hydra.pg.model.ElementTree<T3>, java.util.List<hydra.pg.model.ElementTree<T3>>>) (projected -> projected.dependencies)).apply(tree))));
                        }
                      })))).apply(v1))).apply(((java.util.function.Function<hydra.pg.model.ElementTree<T3>, hydra.pg.model.Element<T3>>) (projected -> projected.self)).apply(tree))),
                      ((((java.util.function.Function<hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>>>) ((java.util.function.Function<hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>>>) (projected -> projected.encode))).apply(((java.util.function.Function<hydra.util.Adapter<T9, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>, hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>>) ((java.util.function.Function<hydra.util.Adapter<T9, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>, hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>>) ((java.util.function.Function<hydra.util.Adapter<T9, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>, hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>>) ((java.util.function.Function<hydra.util.Adapter<T9, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>, hydra.util.Coder<hydra.core.Term, hydra.pg.model.ElementTree<T3>>>) (projected -> projected.coder))))).apply(hydra.pg.coder.Coder.<T3, T5, T9>vertexCoder_eaAdapter(ea)))).apply(cx)).apply(fterm))),
                    hydra.lib.maps.Lookup.apply(
                      (eaField.get()).name,
                      fmap));
                }),
                edgeAdapters)),
            (java.util.function.Function<java.util.List<hydra.pg.model.ElementTree<T3>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>>) (deps -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.pg.model.ElementTree<T3>>right(hydra.pg.coder.Coder.<T3>elementTreeVertex(
              (hydra.pg.model.Vertex<T3>) (new hydra.pg.model.Vertex<T3>(vlabel, vid, props)),
              deps))))))));
    })), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.pg.model.ElementTree<T3>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>) (cx -> (java.util.function.Function<hydra.pg.model.ElementTree<T3>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (ignored -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError("vertex decoding is not yet supported")), cx)))))))))))));
  }
  
  static <T3, T5> hydra.pg.model.VertexType<T5> vertexCoder_vtype(java.util.List<hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T5>, hydra.core.Field, hydra.pg.model.Property<T3>>> propAdapters, T5 vidType, hydra.pg.model.VertexLabel vlabel) {
    return (hydra.pg.model.VertexType<T5>) (new hydra.pg.model.VertexType<T5>(vlabel, vidType, hydra.pg.coder.Coder.propertyTypes(propAdapters)));
  }
  
  static <T3, T5, T9> java.util.List<hydra.pg.model.ElementTypeTree<T5>> vertexCoder_depTypes(java.util.List<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<T9, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>>>>> edgeAdapters) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<T9, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>>>>, hydra.pg.model.ElementTypeTree<T5>>) (ea -> ((java.util.function.Function<hydra.util.Adapter<T9, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>, hydra.pg.model.ElementTypeTree<T5>>) ((java.util.function.Function<hydra.util.Adapter<T9, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>, hydra.pg.model.ElementTypeTree<T5>>) ((java.util.function.Function<hydra.util.Adapter<T9, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>, hydra.pg.model.ElementTypeTree<T5>>) ((java.util.function.Function<hydra.util.Adapter<T9, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>, hydra.pg.model.ElementTypeTree<T5>>) (projected -> projected.target))))).apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(ea))))),
      edgeAdapters);
  }
  
  static <T3, T5, T9> hydra.pg.model.ElementTypeTree<T5> vertexCoder_target(java.util.List<hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<T9, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>>>>> edgeAdapters, java.util.List<hydra.util.Adapter<hydra.core.FieldType, hydra.pg.model.PropertyType<T5>, hydra.core.Field, hydra.pg.model.Property<T3>>> propAdapters, T5 vidType, hydra.pg.model.VertexLabel vlabel) {
    return hydra.pg.coder.Coder.<T5>elementTypeTreeVertex(
      hydra.pg.coder.Coder.<T3, T5>vertexCoder_vtype(
        propAdapters,
        vidType,
        vlabel),
      hydra.pg.coder.Coder.<T3, T5, T9>vertexCoder_depTypes(edgeAdapters));
  }
  
  static <T3, T5, T9> hydra.util.Adapter<T9, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>> vertexCoder_eaAdapter(hydra.util.Pair<hydra.pg.model.Direction, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.model.EdgeLabel, hydra.util.Adapter<T9, hydra.pg.model.ElementTypeTree<T5>, hydra.core.Term, hydra.pg.model.ElementTree<T3>>>>> ea) {
    return hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(hydra.lib.pairs.Second.apply(ea)));
  }
  
  static <T3> T3 vertexCoder_otherid(hydra.pg.model.Vertex<T3> vtx) {
    return ((java.util.function.Function<hydra.pg.model.Vertex<T3>, T3>) (projected -> projected.id)).apply(vtx);
  }
  
  static <T1, T2, T3> T3 vertexCoder_edgeid(hydra.pg.mapping.Schema<T1, T2, T3> schema) {
    return ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, T3>) ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, T3>) ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, T3>) (projected -> projected.defaultEdgeId)))).apply(schema);
  }
  
  static <T3> T3 vertexCoder_outId(hydra.pg.model.Direction eaDir, T3 vid, hydra.pg.model.Vertex<T3> vtx) {
    return (eaDir).accept(new hydra.pg.model.Direction.PartialVisitor<>() {
      @Override
      public T3 visit(hydra.pg.model.Direction.Out ignored) {
        return vid;
      }
      
      @Override
      public T3 visit(hydra.pg.model.Direction.In ignored) {
        return hydra.pg.coder.Coder.<T3>vertexCoder_otherid(vtx);
      }
    });
  }
  
  static <T3> T3 vertexCoder_inId(hydra.pg.model.Direction eaDir, T3 vid, hydra.pg.model.Vertex<T3> vtx) {
    return (eaDir).accept(new hydra.pg.model.Direction.PartialVisitor<>() {
      @Override
      public T3 visit(hydra.pg.model.Direction.Out ignored) {
        return hydra.pg.coder.Coder.<T3>vertexCoder_otherid(vtx);
      }
      
      @Override
      public T3 visit(hydra.pg.model.Direction.In ignored) {
        return vid;
      }
    });
  }
  
  static <T1, T2, T3> hydra.pg.model.Element<T3> vertexCoder_edge(hydra.pg.model.Direction eaDir, hydra.pg.model.EdgeLabel eaLabel, hydra.pg.mapping.Schema<T1, T2, T3> schema, T3 vid, hydra.pg.model.Vertex<T3> vtx) {
    return (hydra.pg.model.Element<T3>) (new hydra.pg.model.Element.Edge((hydra.pg.model.Edge<T3>) (new hydra.pg.model.Edge<T3>(eaLabel, hydra.pg.coder.Coder.<T1, T2, T3>vertexCoder_edgeid(schema), hydra.pg.coder.Coder.<T3>vertexCoder_outId(
      eaDir,
      vid,
      vtx), hydra.pg.coder.Coder.<T3>vertexCoder_inId(
      eaDir,
      vid,
      vtx), (java.util.Map<hydra.pg.model.PropertyKey, T3>) ((java.util.Map<hydra.pg.model.PropertyKey, T3>) (hydra.lib.maps.Empty.<hydra.pg.model.PropertyKey, T3>apply()))))));
  }
  
  static <T3> hydra.pg.model.Edge<T3> vertexCoder_fixedEdge(hydra.pg.model.Direction eaDir, hydra.pg.model.Edge<T3> edg, T3 vid) {
    return (eaDir).accept(new hydra.pg.model.Direction.PartialVisitor<>() {
      @Override
      public hydra.pg.model.Edge<T3> visit(hydra.pg.model.Direction.Out ignored) {
        return (hydra.pg.model.Edge<T3>) (new hydra.pg.model.Edge<T3>(((java.util.function.Function<hydra.pg.model.Edge<T3>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(edg), ((java.util.function.Function<hydra.pg.model.Edge<T3>, T3>) (projected -> projected.id)).apply(edg), vid, ((java.util.function.Function<hydra.pg.model.Edge<T3>, T3>) (projected -> projected.in)).apply(edg), ((java.util.function.Function<hydra.pg.model.Edge<T3>, java.util.Map<hydra.pg.model.PropertyKey, T3>>) (projected -> projected.properties)).apply(edg)));
      }
      
      @Override
      public hydra.pg.model.Edge<T3> visit(hydra.pg.model.Direction.In ignored) {
        return (hydra.pg.model.Edge<T3>) (new hydra.pg.model.Edge<T3>(((java.util.function.Function<hydra.pg.model.Edge<T3>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(edg), ((java.util.function.Function<hydra.pg.model.Edge<T3>, T3>) (projected -> projected.id)).apply(edg), ((java.util.function.Function<hydra.pg.model.Edge<T3>, T3>) (projected -> projected.out)).apply(edg), vid, ((java.util.function.Function<hydra.pg.model.Edge<T3>, java.util.Map<hydra.pg.model.PropertyKey, T3>>) (projected -> projected.properties)).apply(edg)));
      }
    });
  }
  
  static <T0, T1, T2, T3, T4> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T4, hydra.core.Term, T3>>> vertexIdAdapter(hydra.context.Context cx, T0 g, hydra.pg.mapping.Schema<T1, T2, T3> schema, T4 vidType, hydra.core.Name name, hydra.core.Name idKey, java.util.List<hydra.core.FieldType> fields) {
    return hydra.lib.eithers.Bind.apply(
      hydra.pg.coder.Coder.findIdProjectionSpec(
        cx,
        true,
        name,
        idKey,
        fields),
      (java.util.function.Function<hydra.util.Maybe<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T4, hydra.core.Term, T3>>>>) (mIdSpec -> hydra.lib.eithers.Bind.apply(
        hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>>right(hydra.lib.maybes.FromJust.apply(mIdSpec)),
        (java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.util.Pair<hydra.pg.mapping.ValueSpec, hydra.util.Maybe<String>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Name, hydra.util.Adapter<hydra.core.Type, T4, hydra.core.Term, T3>>>>) (idSpec -> hydra.pg.coder.Coder.projectionAdapter(
          cx,
          g,
          vidType,
          ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, hydra.util.Coder<hydra.core.Term, T3>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, hydra.util.Coder<hydra.core.Term, T3>>) ((java.util.function.Function<hydra.pg.mapping.Schema<T1, T2, T3>, hydra.util.Coder<hydra.core.Term, T3>>) (projected -> projected.vertexIds)))).apply(schema),
          idSpec,
          "id")))));
  }
}
