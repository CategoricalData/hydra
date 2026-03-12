// Note: this is an automatically generated file. Do not edit.

package hydra.decode.context;

/**
 * Term decoders for hydra.context
 */
public interface Context {
  static hydra.util.Either<hydra.error.DecodingError, hydra.context.Context> context(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.error.DecodingError, hydra.context.Context>>) (err -> hydra.util.Either.<hydra.error.DecodingError, hydra.context.Context>left(new hydra.error.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.context.Context>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.error.DecodingError, hydra.context.Context> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.error.DecodingError, hydra.context.Context>left(new hydra.error.DecodingError("expected record of type hydra.context.Context"));
        }
        
        @Override
        public hydra.util.Either<hydra.error.DecodingError, hydra.context.Context> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "trace",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, java.util.List<String>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, java.util.List<String>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
                  (java.util.function.Function<String, hydra.util.Either<hydra.error.DecodingError, String>>) (err -> hydra.util.Either.<hydra.error.DecodingError, String>left(new hydra.error.DecodingError(err))),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.error.DecodingError, String> otherwise(hydra.core.Term instance) {
                      return hydra.util.Either.<hydra.error.DecodingError, String>left(new hydra.error.DecodingError("expected literal"));
                    }
                    
                    @Override
                    public hydra.util.Either<hydra.error.DecodingError, String> visit(hydra.core.Term.Literal v) {
                      return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.error.DecodingError, String> otherwise(hydra.core.Literal instance) {
                          return hydra.util.Either.<hydra.error.DecodingError, String>left(new hydra.error.DecodingError("expected string literal"));
                        }
                        
                        @Override
                        public hydra.util.Either<hydra.error.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                          return hydra.util.Either.<hydra.error.DecodingError, String>right((s).value);
                        }
                      });
                    }
                  })),
                  hydra.lexical.Lexical.stripAndDereferenceTermEither(
                    cx2,
                    raw2)))),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<java.util.List<String>, hydra.util.Either<hydra.error.DecodingError, hydra.context.Context>>) (field_trace -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "messages",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, java.util.List<String>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, java.util.List<String>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
                    (java.util.function.Function<String, hydra.util.Either<hydra.error.DecodingError, String>>) (err -> hydra.util.Either.<hydra.error.DecodingError, String>left(new hydra.error.DecodingError(err))),
                    (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.error.DecodingError, String> otherwise(hydra.core.Term instance) {
                        return hydra.util.Either.<hydra.error.DecodingError, String>left(new hydra.error.DecodingError("expected literal"));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.error.DecodingError, String> visit(hydra.core.Term.Literal v) {
                        return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.error.DecodingError, String> otherwise(hydra.core.Literal instance) {
                            return hydra.util.Either.<hydra.error.DecodingError, String>left(new hydra.error.DecodingError("expected string literal"));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.error.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                            return hydra.util.Either.<hydra.error.DecodingError, String>right((s).value);
                          }
                        });
                      }
                    })),
                    hydra.lexical.Lexical.stripAndDereferenceTermEither(
                      cx2,
                      raw2)))),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.List<String>, hydra.util.Either<hydra.error.DecodingError, hydra.context.Context>>) (field_messages -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "other",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, java.util.Map<hydra.core.Name, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, java.util.Map<hydra.core.Name, hydra.core.Term>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMap(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                      p0,
                      p1)),
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, hydra.util.Either<hydra.error.DecodingError, hydra.context.Context>>) (field_other -> hydra.util.Either.<hydra.error.DecodingError, hydra.context.Context>right(new hydra.context.Context(field_trace, field_messages, field_other))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static <T0> hydra.util.Either<hydra.error.DecodingError, hydra.context.InContext<T0>> inContext(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, T0>>> e, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.error.DecodingError, hydra.context.InContext<T0>>>) (err -> hydra.util.Either.<hydra.error.DecodingError, hydra.context.InContext<T0>>left(new hydra.error.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.context.InContext<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.error.DecodingError, hydra.context.InContext<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.error.DecodingError, hydra.context.InContext<T0>>left(new hydra.error.DecodingError("expected record of type hydra.context.InContext"));
        }
        
        @Override
        public hydra.util.Either<hydra.error.DecodingError, hydra.context.InContext<T0>> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "object",
              e,
              fieldMap,
              cx),
            (java.util.function.Function<T0, hydra.util.Either<hydra.error.DecodingError, hydra.context.InContext<T0>>>) (field_object -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "context",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.context.Context>>>) (p0 -> p1 -> hydra.decode.context.Context.context(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.context.Context, hydra.util.Either<hydra.error.DecodingError, hydra.context.InContext<T0>>>) (field_context -> hydra.util.Either.<hydra.error.DecodingError, hydra.context.InContext<T0>>right((hydra.context.InContext<T0>) (new hydra.context.InContext<T0>(field_object, field_context)))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
