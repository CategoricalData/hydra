// Note: this is an automatically generated file. Do not edit.

package hydra.decode.pg;

/**
 * Term decoders for hydra.pg.model
 */
public interface Model {
  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>> adjacentEdge(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> v, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "label",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeLabel>>>) (p0 -> p1 -> hydra.decode.pg.Model.edgeLabel(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.pg.model.EdgeLabel, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>>>) (field_label -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "id",
                v,
                fieldMap,
                cx),
              (java.util.function.Function<T0, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>>>) (field_id -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Helpers.requireField(
                  "vertex",
                  v,
                  fieldMap,
                  cx),
                (java.util.function.Function<T0, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>>>) (field_vertex -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.Helpers.requireField(
                    "properties",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.pg.model.PropertyKey, T0>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.pg.model.PropertyKey, T0>>>) (v2 -> hydra.extract.Helpers.decodeMap(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyKey>>>) (p0 -> p1 -> hydra.decode.pg.Model.propertyKey(
                        p0,
                        p1)),
                      v,
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<hydra.util.PersistentMap<hydra.pg.model.PropertyKey, T0>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>>>) (field_properties -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>>right((hydra.pg.model.AdjacentEdge<T0>) (new hydra.pg.model.AdjacentEdge<T0>(field_label, field_id, field_vertex, field_properties)))))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction> direction(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Direction>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Direction>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.util.ConsList.of(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>>(new hydra.core.Name("out"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.pg.model.Direction>) (t -> new hydra.pg.model.Direction.Out()),
              hydra.extract.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>>(new hydra.core.Name("in"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.pg.model.Direction>) (t -> new hydra.pg.model.Direction.In()),
              hydra.extract.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>>(new hydra.core.Name("both"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.pg.model.Direction>) (t -> new hydra.pg.model.Direction.Both()),
              hydra.extract.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>>(new hydra.core.Name("undirected"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.pg.model.Direction>) (t -> new hydra.pg.model.Direction.Undirected()),
              hydra.extract.Helpers.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Direction>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Direction>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>> edge(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> v, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "label",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeLabel>>>) (p0 -> p1 -> hydra.decode.pg.Model.edgeLabel(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.pg.model.EdgeLabel, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>>>) (field_label -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "id",
                v,
                fieldMap,
                cx),
              (java.util.function.Function<T0, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>>>) (field_id -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Helpers.requireField(
                  "out",
                  v,
                  fieldMap,
                  cx),
                (java.util.function.Function<T0, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>>>) (field_out -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.Helpers.requireField(
                    "in",
                    v,
                    fieldMap,
                    cx),
                  (java.util.function.Function<T0, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>>>) (field_in -> hydra.lib.eithers.Bind.apply(
                    hydra.extract.Helpers.requireField(
                      "properties",
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.pg.model.PropertyKey, T0>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.pg.model.PropertyKey, T0>>>) (v2 -> hydra.extract.Helpers.decodeMap(
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyKey>>>) (p0 -> p1 -> hydra.decode.pg.Model.propertyKey(
                          p0,
                          p1)),
                        v,
                        v1,
                        v2))),
                      fieldMap,
                      cx),
                    (java.util.function.Function<hydra.util.PersistentMap<hydra.pg.model.PropertyKey, T0>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>>>) (field_properties -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>>right((hydra.pg.model.Edge<T0>) (new hydra.pg.model.Edge<T0>(field_label, field_id, field_out, field_in, field_properties)))))))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeLabel> edgeLabel(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeLabel>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.EdgeLabel>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeLabel>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeLabel> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.EdgeLabel>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeLabel> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.pg.model.EdgeLabel>) (b -> new hydra.pg.model.EdgeLabel(b)),
            hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, String>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError(err))),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.errors.DecodingError, String> otherwise(hydra.core.Term instance) {
                  return hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError("expected literal"));
                }

                @Override
                public hydra.util.Either<hydra.errors.DecodingError, String> visit(hydra.core.Term.Literal v) {
                  return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.errors.DecodingError, String> otherwise(hydra.core.Literal instance) {
                      return hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError("expected string literal"));
                    }

                    @Override
                    public hydra.util.Either<hydra.errors.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                      return hydra.util.Either.<hydra.errors.DecodingError, String>right((s).value);
                    }
                  });
                }
              })),
              hydra.Lexical.stripAndDereferenceTermEither(
                cx,
                (wrappedTerm).value.body)));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeType<T0>> edgeType(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> t, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeType<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.EdgeType<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeType<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeType<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.EdgeType<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeType<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "label",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeLabel>>>) (p0 -> p1 -> hydra.decode.pg.Model.edgeLabel(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.pg.model.EdgeLabel, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeType<T0>>>) (field_label -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "id",
                t,
                fieldMap,
                cx),
              (java.util.function.Function<T0, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeType<T0>>>) (field_id -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Helpers.requireField(
                  "out",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexLabel>>>) (p0 -> p1 -> hydra.decode.pg.Model.vertexLabel(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.pg.model.VertexLabel, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeType<T0>>>) (field_out -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.Helpers.requireField(
                    "in",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexLabel>>>) (p0 -> p1 -> hydra.decode.pg.Model.vertexLabel(
                      p0,
                      p1)),
                    fieldMap,
                    cx),
                  (java.util.function.Function<hydra.pg.model.VertexLabel, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeType<T0>>>) (field_in -> hydra.lib.eithers.Bind.apply(
                    hydra.extract.Helpers.requireField(
                      "properties",
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.PropertyType<T0>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.PropertyType<T0>>>>) (v2 -> hydra.extract.Helpers.decodeList(
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyType<T0>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyType<T0>>>) (v22 -> hydra.decode.pg.Model.<T0>propertyType(
                          t,
                          v12,
                          v22))),
                        v1,
                        v2))),
                      fieldMap,
                      cx),
                    (java.util.function.Function<hydra.util.ConsList<hydra.pg.model.PropertyType<T0>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeType<T0>>>) (field_properties -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.EdgeType<T0>>right((hydra.pg.model.EdgeType<T0>) (new hydra.pg.model.EdgeType<T0>(field_label, field_id, field_out, field_in, field_properties)))))))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>> element(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> v, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              hydra.decode.pg.Model.<T0>element_variantMap(
                cx,
                v)));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.PersistentMap<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>>> element_variantMap(hydra.graph.Graph cx, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> v) {
    return hydra.lib.maps.FromList.apply(hydra.util.ConsList.of(
      (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>>>(new hydra.core.Name("vertex"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>>) (input -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<hydra.pg.model.Vertex<T0>, hydra.pg.model.Element<T0>>) (t -> (hydra.pg.model.Element<T0>) (new hydra.pg.model.Element.Vertex(t))),
        hydra.decode.pg.Model.<T0>vertex(
          v,
          cx,
          input)))))),
      (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>>>(new hydra.core.Name("edge"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>>) (input -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<hydra.pg.model.Edge<T0>, hydra.pg.model.Element<T0>>) (t -> (hydra.pg.model.Element<T0>) (new hydra.pg.model.Element.Edge(t))),
        hydra.decode.pg.Model.<T0>edge(
          v,
          cx,
          input))))))));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind> elementKind(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.ElementKind>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.ElementKind>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.util.ConsList.of(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind>>>(new hydra.core.Name("vertex"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.pg.model.ElementKind>) (t -> new hydra.pg.model.ElementKind.Vertex()),
              hydra.extract.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind>>>(new hydra.core.Name("edge"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.pg.model.ElementKind>) (t -> new hydra.pg.model.ElementKind.Edge()),
              hydra.extract.Helpers.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.ElementKind>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementKind>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTree<T0>> elementTree(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> v, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTree<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.ElementTree<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTree<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTree<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.ElementTree<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTree<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "self",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Element<T0>>>) (v2 -> hydra.decode.pg.Model.<T0>element(
                v,
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.pg.model.Element<T0>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTree<T0>>>) (field_self -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "dependencies",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.ElementTree<T0>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.ElementTree<T0>>>>) (v2 -> hydra.extract.Helpers.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTree<T0>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTree<T0>>>) (v22 -> hydra.decode.pg.Model.<T0>elementTree(
                    v,
                    v12,
                    v22))),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.ConsList<hydra.pg.model.ElementTree<T0>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTree<T0>>>) (field_dependencies -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.ElementTree<T0>>right((hydra.pg.model.ElementTree<T0>) (new hydra.pg.model.ElementTree<T0>(field_self, field_dependencies)))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>> elementType(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> t, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              hydra.decode.pg.Model.<T0>elementType_variantMap(
                cx,
                t)));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.PersistentMap<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>>> elementType_variantMap(hydra.graph.Graph cx, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> t) {
    return hydra.lib.maps.FromList.apply(hydra.util.ConsList.of(
      (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>>>(new hydra.core.Name("vertex"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>>) (input -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<hydra.pg.model.VertexType<T0>, hydra.pg.model.ElementType<T0>>) (t2 -> (hydra.pg.model.ElementType<T0>) (new hydra.pg.model.ElementType.Vertex(t2))),
        hydra.decode.pg.Model.<T0>vertexType(
          t,
          cx,
          input)))))),
      (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>>>(new hydra.core.Name("edge"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>>) (input -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<hydra.pg.model.EdgeType<T0>, hydra.pg.model.ElementType<T0>>) (t2 -> (hydra.pg.model.ElementType<T0>) (new hydra.pg.model.ElementType.Edge(t2))),
        hydra.decode.pg.Model.<T0>edgeType(
          t,
          cx,
          input))))))));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTypeTree<T0>> elementTypeTree(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> t, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTypeTree<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.ElementTypeTree<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTypeTree<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTypeTree<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.ElementTypeTree<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTypeTree<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "self",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementType<T0>>>) (v2 -> hydra.decode.pg.Model.<T0>elementType(
                t,
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.pg.model.ElementType<T0>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTypeTree<T0>>>) (field_self -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "dependencies",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.ElementTypeTree<T0>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.ElementTypeTree<T0>>>>) (v2 -> hydra.extract.Helpers.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTypeTree<T0>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTypeTree<T0>>>) (v22 -> hydra.decode.pg.Model.<T0>elementTypeTree(
                    t,
                    v12,
                    v22))),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.ConsList<hydra.pg.model.ElementTypeTree<T0>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.ElementTypeTree<T0>>>) (field_dependencies -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.ElementTypeTree<T0>>right((hydra.pg.model.ElementTypeTree<T0>) (new hydra.pg.model.ElementTypeTree<T0>(field_self, field_dependencies)))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Graph<T0>> graph(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> v, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Graph<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Graph<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Graph<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Graph<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Graph<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Graph<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "vertices",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<T0, hydra.pg.model.Vertex<T0>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<T0, hydra.pg.model.Vertex<T0>>>>) (v2 -> hydra.extract.Helpers.decodeMap(
                v,
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>>>) (v22 -> hydra.decode.pg.Model.<T0>vertex(
                  v,
                  v12,
                  v22))),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.util.PersistentMap<T0, hydra.pg.model.Vertex<T0>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Graph<T0>>>) (field_vertices -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "edges",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<T0, hydra.pg.model.Edge<T0>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<T0, hydra.pg.model.Edge<T0>>>>) (v2 -> hydra.extract.Helpers.decodeMap(
                  v,
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>>>) (v22 -> hydra.decode.pg.Model.<T0>edge(
                    v,
                    v12,
                    v22))),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.PersistentMap<T0, hydra.pg.model.Edge<T0>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Graph<T0>>>) (field_edges -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Graph<T0>>right((hydra.pg.model.Graph<T0>) (new hydra.pg.model.Graph<T0>(field_vertices, field_edges)))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.GraphSchema<T0>> graphSchema(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> t, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.GraphSchema<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.GraphSchema<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.GraphSchema<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.GraphSchema<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.GraphSchema<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.GraphSchema<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "vertices",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.pg.model.VertexLabel, hydra.pg.model.VertexType<T0>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.pg.model.VertexLabel, hydra.pg.model.VertexType<T0>>>>) (v2 -> hydra.extract.Helpers.decodeMap(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexLabel>>>) (p0 -> p1 -> hydra.decode.pg.Model.vertexLabel(
                  p0,
                  p1)),
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexType<T0>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexType<T0>>>) (v22 -> hydra.decode.pg.Model.<T0>vertexType(
                  t,
                  v12,
                  v22))),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.util.PersistentMap<hydra.pg.model.VertexLabel, hydra.pg.model.VertexType<T0>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.GraphSchema<T0>>>) (field_vertices -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "edges",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.pg.model.EdgeLabel, hydra.pg.model.EdgeType<T0>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.pg.model.EdgeLabel, hydra.pg.model.EdgeType<T0>>>>) (v2 -> hydra.extract.Helpers.decodeMap(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeLabel>>>) (p0 -> p1 -> hydra.decode.pg.Model.edgeLabel(
                    p0,
                    p1)),
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeType<T0>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeType<T0>>>) (v22 -> hydra.decode.pg.Model.<T0>edgeType(
                    t,
                    v12,
                    v22))),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.PersistentMap<hydra.pg.model.EdgeLabel, hydra.pg.model.EdgeType<T0>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.GraphSchema<T0>>>) (field_edges -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.GraphSchema<T0>>right((hydra.pg.model.GraphSchema<T0>) (new hydra.pg.model.GraphSchema<T0>(field_vertices, field_edges)))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label> label(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Label>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Label>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.util.ConsList.of(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label>>>(new hydra.core.Name("vertex"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.pg.model.VertexLabel, hydra.pg.model.Label>) (t -> new hydra.pg.model.Label.Vertex(t)),
              hydra.decode.pg.Model.vertexLabel(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label>>>(new hydra.core.Name("edge"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.pg.model.EdgeLabel, hydra.pg.model.Label>) (t -> new hydra.pg.model.Label.Edge(t)),
              hydra.decode.pg.Model.edgeLabel(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Label>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Label>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.LazyGraph<T0>> lazyGraph(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> v, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.LazyGraph<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.LazyGraph<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.LazyGraph<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.LazyGraph<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.LazyGraph<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.LazyGraph<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "vertices",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.Vertex<T0>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.Vertex<T0>>>>) (v2 -> hydra.extract.Helpers.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>>>) (v22 -> hydra.decode.pg.Model.<T0>vertex(
                  v,
                  v12,
                  v22))),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.util.ConsList<hydra.pg.model.Vertex<T0>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.LazyGraph<T0>>>) (field_vertices -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "edges",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.Edge<T0>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.Edge<T0>>>>) (v2 -> hydra.extract.Helpers.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Edge<T0>>>) (v22 -> hydra.decode.pg.Model.<T0>edge(
                    v,
                    v12,
                    v22))),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.ConsList<hydra.pg.model.Edge<T0>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.LazyGraph<T0>>>) (field_edges -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.LazyGraph<T0>>right((hydra.pg.model.LazyGraph<T0>) (new hydra.pg.model.LazyGraph<T0>(field_vertices, field_edges)))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Property<T0>> property(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> v, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Property<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Property<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Property<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Property<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Property<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Property<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "key",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyKey>>>) (p0 -> p1 -> hydra.decode.pg.Model.propertyKey(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.pg.model.PropertyKey, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Property<T0>>>) (field_key -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "value",
                v,
                fieldMap,
                cx),
              (java.util.function.Function<T0, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Property<T0>>>) (field_value -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Property<T0>>right((hydra.pg.model.Property<T0>) (new hydra.pg.model.Property<T0>(field_key, field_value)))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyKey> propertyKey(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyKey>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.PropertyKey>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyKey>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyKey> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.PropertyKey>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyKey> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.pg.model.PropertyKey>) (b -> new hydra.pg.model.PropertyKey(b)),
            hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, String>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError(err))),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.errors.DecodingError, String> otherwise(hydra.core.Term instance) {
                  return hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError("expected literal"));
                }

                @Override
                public hydra.util.Either<hydra.errors.DecodingError, String> visit(hydra.core.Term.Literal v) {
                  return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.errors.DecodingError, String> otherwise(hydra.core.Literal instance) {
                      return hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError("expected string literal"));
                    }

                    @Override
                    public hydra.util.Either<hydra.errors.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                      return hydra.util.Either.<hydra.errors.DecodingError, String>right((s).value);
                    }
                  });
                }
              })),
              hydra.Lexical.stripAndDereferenceTermEither(
                cx,
                (wrappedTerm).value.body)));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyType<T0>> propertyType(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> t, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyType<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.PropertyType<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyType<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyType<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.PropertyType<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyType<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "key",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyKey>>>) (p0 -> p1 -> hydra.decode.pg.Model.propertyKey(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.pg.model.PropertyKey, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyType<T0>>>) (field_key -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "value",
                t,
                fieldMap,
                cx),
              (java.util.function.Function<T0, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyType<T0>>>) (field_value -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Helpers.requireField(
                  "required",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Boolean>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Boolean>>) (raw2 -> hydra.lib.eithers.Either.apply(
                    (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, Boolean>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Boolean>left(new hydra.errors.DecodingError(err))),
                    (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Boolean>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Boolean> otherwise(hydra.core.Term instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, Boolean>left(new hydra.errors.DecodingError("expected literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Boolean> visit(hydra.core.Term.Literal v) {
                        return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Boolean> otherwise(hydra.core.Literal instance) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Boolean>left(new hydra.errors.DecodingError("expected boolean literal"));
                          }

                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Boolean> visit(hydra.core.Literal.Boolean_ b) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Boolean>right((b).value);
                          }
                        });
                      }
                    })),
                    hydra.Lexical.stripAndDereferenceTermEither(
                      cx2,
                      raw2)))),
                  fieldMap,
                  cx),
                (java.util.function.Function<Boolean, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyType<T0>>>) (field_required -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.PropertyType<T0>>right((hydra.pg.model.PropertyType<T0>) (new hydra.pg.model.PropertyType<T0>(field_key, field_value, field_required)))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>> vertex(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> v, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "label",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexLabel>>>) (p0 -> p1 -> hydra.decode.pg.Model.vertexLabel(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.pg.model.VertexLabel, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>>>) (field_label -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "id",
                v,
                fieldMap,
                cx),
              (java.util.function.Function<T0, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>>>) (field_id -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Helpers.requireField(
                  "properties",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.pg.model.PropertyKey, T0>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.pg.model.PropertyKey, T0>>>) (v2 -> hydra.extract.Helpers.decodeMap(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyKey>>>) (p0 -> p1 -> hydra.decode.pg.Model.propertyKey(
                      p0,
                      p1)),
                    v,
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.PersistentMap<hydra.pg.model.PropertyKey, T0>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>>>) (field_properties -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>>right((hydra.pg.model.Vertex<T0>) (new hydra.pg.model.Vertex<T0>(field_label, field_id, field_properties)))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexLabel> vertexLabel(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexLabel>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.VertexLabel>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexLabel>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexLabel> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.VertexLabel>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexLabel> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.pg.model.VertexLabel>) (b -> new hydra.pg.model.VertexLabel(b)),
            hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, String>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError(err))),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.errors.DecodingError, String> otherwise(hydra.core.Term instance) {
                  return hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError("expected literal"));
                }

                @Override
                public hydra.util.Either<hydra.errors.DecodingError, String> visit(hydra.core.Term.Literal v) {
                  return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.errors.DecodingError, String> otherwise(hydra.core.Literal instance) {
                      return hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError("expected string literal"));
                    }

                    @Override
                    public hydra.util.Either<hydra.errors.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                      return hydra.util.Either.<hydra.errors.DecodingError, String>right((s).value);
                    }
                  });
                }
              })),
              hydra.Lexical.stripAndDereferenceTermEither(
                cx,
                (wrappedTerm).value.body)));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexType<T0>> vertexType(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> t, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexType<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.VertexType<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexType<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexType<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.VertexType<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexType<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "label",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexLabel>>>) (p0 -> p1 -> hydra.decode.pg.Model.vertexLabel(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.pg.model.VertexLabel, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexType<T0>>>) (field_label -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "id",
                t,
                fieldMap,
                cx),
              (java.util.function.Function<T0, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexType<T0>>>) (field_id -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Helpers.requireField(
                  "properties",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.PropertyType<T0>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.PropertyType<T0>>>>) (v2 -> hydra.extract.Helpers.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyType<T0>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyType<T0>>>) (v22 -> hydra.decode.pg.Model.<T0>propertyType(
                      t,
                      v12,
                      v22))),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.ConsList<hydra.pg.model.PropertyType<T0>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexType<T0>>>) (field_properties -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.VertexType<T0>>right((hydra.pg.model.VertexType<T0>) (new hydra.pg.model.VertexType<T0>(field_label, field_id, field_properties)))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexWithAdjacentEdges<T0>> vertexWithAdjacentEdges(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> v, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexWithAdjacentEdges<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.VertexWithAdjacentEdges<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexWithAdjacentEdges<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexWithAdjacentEdges<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.VertexWithAdjacentEdges<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexWithAdjacentEdges<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "vertex",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.Vertex<T0>>>) (v2 -> hydra.decode.pg.Model.<T0>vertex(
                v,
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.pg.model.Vertex<T0>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexWithAdjacentEdges<T0>>>) (field_vertex -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "ins",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.AdjacentEdge<T0>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.AdjacentEdge<T0>>>>) (v2 -> hydra.extract.Helpers.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>>>) (v22 -> hydra.decode.pg.Model.<T0>adjacentEdge(
                    v,
                    v12,
                    v22))),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.ConsList<hydra.pg.model.AdjacentEdge<T0>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexWithAdjacentEdges<T0>>>) (field_ins -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Helpers.requireField(
                  "outs",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.AdjacentEdge<T0>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.pg.model.AdjacentEdge<T0>>>>) (v2 -> hydra.extract.Helpers.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.AdjacentEdge<T0>>>) (v22 -> hydra.decode.pg.Model.<T0>adjacentEdge(
                      v,
                      v12,
                      v22))),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.ConsList<hydra.pg.model.AdjacentEdge<T0>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexWithAdjacentEdges<T0>>>) (field_outs -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.model.VertexWithAdjacentEdges<T0>>right((hydra.pg.model.VertexWithAdjacentEdges<T0>) (new hydra.pg.model.VertexWithAdjacentEdges<T0>(field_vertex, field_ins, field_outs)))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
