// Note: this is an automatically generated file. Do not edit.

package hydra.decode.compute;

/**
 * Term decoders for hydra.compute
 */
public interface Compute {
  static <T0, T1> hydra.util.Either<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>> flowState(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> s, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T1>>> v, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>>left(new hydra.util.DecodingError("expected record of type hydra.compute.FlowState"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap(((record)).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "value",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<T1>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<T1>>>) (v2 -> hydra.extract.helpers.Helpers.<T1>decodeMaybe(
                (v),
                (v1),
                (v2)))),
              (fieldMap),
              (cx)),
            (java.util.function.Function<hydra.util.Maybe<T1>, hydra.util.Either<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>>>) (field_value -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "state",
                (s),
                (fieldMap),
                (cx)),
              (java.util.function.Function<T0, hydra.util.Either<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>>>) (field_state -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "trace",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.compute.Trace>>>) (p0 -> p1 -> hydra.decode.compute.Compute.trace(
                    (p0),
                    (p1))),
                  (fieldMap),
                  (cx)),
                (java.util.function.Function<hydra.compute.Trace, hydra.util.Either<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>>>) (field_trace -> (hydra.util.Either<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.compute.FlowState<T0, T1>>right((hydra.compute.FlowState<T0, T1>) ((hydra.compute.FlowState<T0, T1>) (new hydra.compute.FlowState<T0, T1>((field_value), (field_state), (field_trace)))))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.compute.Trace> trace(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.compute.Trace>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.compute.Trace>) ((hydra.util.Either<hydra.util.DecodingError, hydra.compute.Trace>) (hydra.util.Either.<hydra.util.DecodingError, hydra.compute.Trace>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.compute.Trace>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.compute.Trace> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.compute.Trace>) ((hydra.util.Either<hydra.util.DecodingError, hydra.compute.Trace>) (hydra.util.Either.<hydra.util.DecodingError, hydra.compute.Trace>left(new hydra.util.DecodingError("expected record of type hydra.compute.Trace"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.compute.Trace> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap(((record)).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "stack",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<String>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<String>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
                  (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError((err)))))),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                    }
                    
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                      return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                          return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                        }
                        
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                          return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right(((s)).value)));
                        }
                      });
                    }
                  })),
                  hydra.lexical.Lexical.stripAndDereferenceTermEither(
                    (cx2),
                    (raw2))))),
                (v1),
                (v2)))),
              (fieldMap),
              (cx)),
            (java.util.function.Function<java.util.List<String>, hydra.util.Either<hydra.util.DecodingError, hydra.compute.Trace>>) (field_stack -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "messages",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<String>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<String>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
                    (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError((err)))))),
                    (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                        return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                            return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right(((s)).value)));
                          }
                        });
                      }
                    })),
                    hydra.lexical.Lexical.stripAndDereferenceTermEither(
                      (cx2),
                      (raw2))))),
                  (v1),
                  (v2)))),
                (fieldMap),
                (cx)),
              (java.util.function.Function<java.util.List<String>, hydra.util.Either<hydra.util.DecodingError, hydra.compute.Trace>>) (field_messages -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "other",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.Term>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMap(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                      (p0),
                      (p1))),
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                      (p0),
                      (p1))),
                    (v1),
                    (v2)))),
                  (fieldMap),
                  (cx)),
                (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, hydra.util.Either<hydra.util.DecodingError, hydra.compute.Trace>>) (field_other -> (hydra.util.Either<hydra.util.DecodingError, hydra.compute.Trace>) ((hydra.util.Either<hydra.util.DecodingError, hydra.compute.Trace>) (hydra.util.Either.<hydra.util.DecodingError, hydra.compute.Trace>right(new hydra.compute.Trace((field_stack), (field_messages), (field_other)))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
}
