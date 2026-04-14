// Note: this is an automatically generated file. Do not edit.

package hydra.decode;

/**
 * Term decoders for hydra.query
 */
public interface Query {
  static hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint> comparisonConstraint(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint> visit(hydra.core.Term.Inject inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>(new hydra.core.Name("equal"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.query.ComparisonConstraint>) (t -> new hydra.query.ComparisonConstraint.Equal()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>(new hydra.core.Name("notEqual"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.query.ComparisonConstraint>) (t -> new hydra.query.ComparisonConstraint.NotEqual()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>(new hydra.core.Name("lessThan"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.query.ComparisonConstraint>) (t -> new hydra.query.ComparisonConstraint.LessThan()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>(new hydra.core.Name("greaterThan"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.query.ComparisonConstraint>) (t -> new hydra.query.ComparisonConstraint.GreaterThan()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>(new hydra.core.Name("lessThanOrEqual"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.query.ComparisonConstraint>) (t -> new hydra.query.ComparisonConstraint.LessThanOrEqual()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>>(new hydra.core.Name("greaterThanOrEqual"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.query.ComparisonConstraint>) (t -> new hydra.query.ComparisonConstraint.GreaterThanOrEqual()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>, hydra.util.Either<hydra.errors.DecodingError, hydra.query.ComparisonConstraint>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.query.Edge> edge(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Edge>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Edge>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Edge>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Edge> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Edge>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Edge> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "type",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Edge>>) (field_type -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "out",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.core.Name>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.core.Name>>>) (v2 -> hydra.extract.Core.decodeMaybe(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.Maybe<hydra.core.Name>, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Edge>>) (field_out -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "in",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.core.Name>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.core.Name>>>) (v2 -> hydra.extract.Core.decodeMaybe(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.Maybe<hydra.core.Name>, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Edge>>) (field_in -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Edge>right(new hydra.query.Edge(field_type, field_out, field_in))))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.query.GraphPattern> graphPattern(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.query.GraphPattern>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.GraphPattern>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.GraphPattern>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.GraphPattern> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.query.GraphPattern>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.GraphPattern> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "graph",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.query.GraphPattern>>) (field_graph -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "patterns",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.query.Pattern>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.query.Pattern>>>) (v2 -> hydra.extract.Core.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) (p0 -> p1 -> hydra.decode.Query.pattern(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.List<hydra.query.Pattern>, hydra.util.Either<hydra.errors.DecodingError, hydra.query.GraphPattern>>) (field_patterns -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.GraphPattern>right(new hydra.query.GraphPattern(field_graph, field_patterns))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node> node(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Node>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Node>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node> visit(hydra.core.Term.Inject inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>>(new hydra.core.Name("term"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Term, hydra.query.Node>) (t -> new hydra.query.Node.Term(t)),
              hydra.decode.Core.term(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>>(new hydra.core.Name("variable"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.query.Variable, hydra.query.Node>) (t -> new hydra.query.Node.Variable(t)),
              hydra.decode.Query.variable(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>>(new hydra.core.Name("wildcard"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.query.Node>) (t -> new hydra.query.Node.Wildcard()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Node>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path> path(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Path>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Path>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path> visit(hydra.core.Term.Inject inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>>(new hydra.core.Name("step"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.query.Step, hydra.query.Path>) (t -> new hydra.query.Path.Step(t)),
              hydra.decode.Query.step(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>>(new hydra.core.Name("regex"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.query.RegexSequence, hydra.query.Path>) (t -> new hydra.query.Path.Regex(t)),
              hydra.decode.Query.regexSequence(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>>(new hydra.core.Name("inverse"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.query.Path, hydra.query.Path>) (t -> new hydra.query.Path.Inverse(t)),
              hydra.decode.Query.path(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Path>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.query.PathEquation> pathEquation(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.query.PathEquation>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.PathEquation>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.PathEquation>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.PathEquation> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.query.PathEquation>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.PathEquation> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "left",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>>) (p0 -> p1 -> hydra.decode.Query.path(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.query.Path, hydra.util.Either<hydra.errors.DecodingError, hydra.query.PathEquation>>) (field_left -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "right",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>>) (p0 -> p1 -> hydra.decode.Query.path(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.query.Path, hydra.util.Either<hydra.errors.DecodingError, hydra.query.PathEquation>>) (field_right -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.PathEquation>right(new hydra.query.PathEquation(field_left, field_right))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern> pattern(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Pattern>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Pattern>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern> visit(hydra.core.Term.Inject inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>(new hydra.core.Name("triple"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.query.TriplePattern, hydra.query.Pattern>) (t -> new hydra.query.Pattern.Triple(t)),
              hydra.decode.Query.triplePattern(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>(new hydra.core.Name("negation"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.query.Pattern, hydra.query.Pattern>) (t -> new hydra.query.Pattern.Negation(t)),
              hydra.decode.Query.pattern(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>(new hydra.core.Name("conjunction"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.query.Pattern>, hydra.query.Pattern>) (t -> new hydra.query.Pattern.Conjunction(t)),
              hydra.extract.Core.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) (p0 -> p1 -> hydra.decode.Query.pattern(
                  p0,
                  p1)),
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>(new hydra.core.Name("disjunction"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.query.Pattern>, hydra.query.Pattern>) (t -> new hydra.query.Pattern.Disjunction(t)),
              hydra.extract.Core.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) (p0 -> p1 -> hydra.decode.Query.pattern(
                  p0,
                  p1)),
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>(new hydra.core.Name("graph"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.query.GraphPattern, hydra.query.Pattern>) (t -> new hydra.query.Pattern.Graph(t)),
              hydra.decode.Query.graphPattern(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Pattern>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.query.PatternImplication> patternImplication(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.query.PatternImplication>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.PatternImplication>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.PatternImplication>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.PatternImplication> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.query.PatternImplication>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.PatternImplication> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "antecedent",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) (p0 -> p1 -> hydra.decode.Query.pattern(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.query.Pattern, hydra.util.Either<hydra.errors.DecodingError, hydra.query.PatternImplication>>) (field_antecedent -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "consequent",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) (p0 -> p1 -> hydra.decode.Query.pattern(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.query.Pattern, hydra.util.Either<hydra.errors.DecodingError, hydra.query.PatternImplication>>) (field_consequent -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.PatternImplication>right(new hydra.query.PatternImplication(field_antecedent, field_consequent))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.query.Query> query(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Query>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Query>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Query>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Query> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Query>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Query> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "variables",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.query.Variable>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.query.Variable>>>) (v2 -> hydra.extract.Core.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Variable>>>) (p0 -> p1 -> hydra.decode.Query.variable(
                  p0,
                  p1)),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<java.util.List<hydra.query.Variable>, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Query>>) (field_variables -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "patterns",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.query.Pattern>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.query.Pattern>>>) (v2 -> hydra.extract.Core.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Pattern>>>) (p0 -> p1 -> hydra.decode.Query.pattern(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.List<hydra.query.Pattern>, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Query>>) (field_patterns -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Query>right(new hydra.query.Query(field_variables, field_patterns))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.query.Range> range(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Range>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Range>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Range>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Range> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Range>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Range> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "min",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (raw2 -> hydra.lib.eithers.Either.apply(
                (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Integer>left(err)),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError("expected literal"));
                  }

                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                    return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError("expected int32 literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v1) {
                        return (v1).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError("expected int32 value"));
                          }

                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Integer>right((i).value);
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.extract.Core.stripWithDecodingError(
                  cx2,
                  raw2)))),
              fieldMap,
              cx),
            (java.util.function.Function<Integer, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Range>>) (field_min -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "max",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (raw2 -> hydra.lib.eithers.Either.apply(
                  (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Integer>left(err)),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.errors.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                      return hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError("expected literal"));
                    }

                    @Override
                    public hydra.util.Either<hydra.errors.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                      return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.errors.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                          return hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError("expected int32 literal"));
                        }

                        @Override
                        public hydra.util.Either<hydra.errors.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v1) {
                          return (v1).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                            @Override
                            public hydra.util.Either<hydra.errors.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                              return hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError("expected int32 value"));
                            }

                            @Override
                            public hydra.util.Either<hydra.errors.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                              return hydra.util.Either.<hydra.errors.DecodingError, Integer>right((i).value);
                            }
                          });
                        }
                      });
                    }
                  })),
                  hydra.extract.Core.stripWithDecodingError(
                    cx2,
                    raw2)))),
                fieldMap,
                cx),
              (java.util.function.Function<Integer, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Range>>) (field_max -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Range>right(new hydra.query.Range(field_min, field_max))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier> regexQuantifier(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.RegexQuantifier>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.query.RegexQuantifier>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier> visit(hydra.core.Term.Inject inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>(new hydra.core.Name("one"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.query.RegexQuantifier>) (t -> new hydra.query.RegexQuantifier.One()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>(new hydra.core.Name("zeroOrOne"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.query.RegexQuantifier>) (t -> new hydra.query.RegexQuantifier.ZeroOrOne()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>(new hydra.core.Name("zeroOrMore"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.query.RegexQuantifier>) (t -> new hydra.query.RegexQuantifier.ZeroOrMore()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>(new hydra.core.Name("oneOrMore"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.query.RegexQuantifier>) (t -> new hydra.query.RegexQuantifier.OneOrMore()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>(new hydra.core.Name("exactly"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Integer, hydra.query.RegexQuantifier>) (t -> new hydra.query.RegexQuantifier.Exactly(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Integer>left(err)),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError("expected literal"));
                  }

                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                    return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError("expected int32 literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v1) {
                        return (v1).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError("expected int32 value"));
                          }

                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Integer>right((i).value);
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.extract.Core.stripWithDecodingError(
                  cx,
                  input))))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>(new hydra.core.Name("atLeast"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Integer, hydra.query.RegexQuantifier>) (t -> new hydra.query.RegexQuantifier.AtLeast(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Integer>left(err)),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError("expected literal"));
                  }

                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                    return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError("expected int32 literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v1) {
                        return (v1).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError("expected int32 value"));
                          }

                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Integer>right((i).value);
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.extract.Core.stripWithDecodingError(
                  cx,
                  input))))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>(new hydra.core.Name("range"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.query.Range, hydra.query.RegexQuantifier>) (t -> new hydra.query.RegexQuantifier.Range(t)),
              hydra.decode.Query.range(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.RegexQuantifier>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexSequence> regexSequence(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexSequence>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.RegexSequence>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexSequence>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexSequence> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.query.RegexSequence>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexSequence> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "path",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>>) (p0 -> p1 -> hydra.decode.Query.path(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.query.Path, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexSequence>>) (field_path -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "quantifier",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexQuantifier>>>) (p0 -> p1 -> hydra.decode.Query.regexQuantifier(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.query.RegexQuantifier, hydra.util.Either<hydra.errors.DecodingError, hydra.query.RegexSequence>>) (field_quantifier -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.RegexSequence>right(new hydra.query.RegexSequence(field_path, field_quantifier))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step> step(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Step>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Step>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step> visit(hydra.core.Term.Inject inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>>(new hydra.core.Name("edge"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.query.Edge, hydra.query.Step>) (t -> new hydra.query.Step.Edge(t)),
              hydra.decode.Query.edge(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>>(new hydra.core.Name("project"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Projection, hydra.query.Step>) (t -> new hydra.query.Step.Project(t)),
              hydra.decode.Core.projection(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>>(new hydra.core.Name("compare"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.query.ComparisonConstraint, hydra.query.Step>) (t -> new hydra.query.Step.Compare(t)),
              hydra.decode.Query.comparisonConstraint(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Step>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Step>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.query.TriplePattern> triplePattern(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.query.TriplePattern>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.TriplePattern>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.TriplePattern>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.TriplePattern> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.query.TriplePattern>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.TriplePattern> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "subject",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>>) (p0 -> p1 -> hydra.decode.Query.node(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.query.Node, hydra.util.Either<hydra.errors.DecodingError, hydra.query.TriplePattern>>) (field_subject -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "predicate",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Path>>>) (p0 -> p1 -> hydra.decode.Query.path(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.query.Path, hydra.util.Either<hydra.errors.DecodingError, hydra.query.TriplePattern>>) (field_predicate -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "object",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Node>>>) (p0 -> p1 -> hydra.decode.Query.node(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.query.Node, hydra.util.Either<hydra.errors.DecodingError, hydra.query.TriplePattern>>) (field_object -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.TriplePattern>right(new hydra.query.TriplePattern(field_subject, field_predicate, field_object))))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.query.Variable> variable(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Variable>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Variable>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.query.Variable>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Variable> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.query.Variable>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.query.Variable> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.query.Variable>) (b -> new hydra.query.Variable(b)),
            hydra.lib.eithers.Either.apply(
              (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, String>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, String>left(err)),
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
              hydra.extract.Core.stripWithDecodingError(
                cx,
                (wrappedTerm).value.body)));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }
}
