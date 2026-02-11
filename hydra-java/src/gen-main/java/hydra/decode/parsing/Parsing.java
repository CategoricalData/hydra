// Note: this is an automatically generated file. Do not edit.

package hydra.decode.parsing;

/**
 * Term decoders for hydra.parsing
 */
public interface Parsing {
  static hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseError> parseError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseError>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseError>) ((hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseError>) (hydra.util.Either.<hydra.util.DecodingError, hydra.parsing.ParseError>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseError> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseError>) ((hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseError>) (hydra.util.Either.<hydra.util.DecodingError, hydra.parsing.ParseError>left(new hydra.util.DecodingError("expected record of type hydra.parsing.ParseError"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "message",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError(err))))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                  }
                  
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                    return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                        return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right((s).value)));
                      }
                    });
                  }
                })),
                hydra.lexical.Lexical.stripAndDereferenceTermEither(
                  cx2,
                  raw2)))),
              fieldMap,
              cx),
            (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseError>>) (field_message -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "remainder",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
                  (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError(err))))),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                    }
                    
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                      return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                          return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                        }
                        
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                          return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right((s).value)));
                        }
                      });
                    }
                  })),
                  hydra.lexical.Lexical.stripAndDereferenceTermEither(
                    cx2,
                    raw2)))),
                fieldMap,
                cx),
              (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseError>>) (field_remainder -> (hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseError>) ((hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseError>) (hydra.util.Either.<hydra.util.DecodingError, hydra.parsing.ParseError>right(new hydra.parsing.ParseError(field_message, field_remainder))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static <T0> hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>> parseResult(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> a, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>left(new hydra.util.DecodingError("expected union of type hydra.parsing.ParseResult"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = ((inj).value).field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.core.Name tname = ((inj).value).typeName;
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              (fname).value,
              " in union type ",
              (tname).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>>, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              hydra.decode.parsing.Parsing.<T0>parseResult_variantMap(
                a,
                cx,
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseError>>>) (p0 -> p1 -> hydra.decode.parsing.Parsing.parseError(
                  p0,
                  p1)))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static <T0> java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>>> parseResult_variantMap(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> a, hydra.graph.Graph cx, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseError>>> hydra_decode_parsing_parseError2) {
    return hydra.lib.maps.FromList.apply(java.util.List.of(
      (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>>>(new hydra.core.Name("success"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>>) (input -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<hydra.parsing.ParseSuccess<T0>, hydra.parsing.ParseResult<T0>>) (t -> (hydra.parsing.ParseResult<T0>) (new hydra.parsing.ParseResult.Success(t))),
        hydra.decode.parsing.Parsing.<T0>parseSuccess(
          a,
          cx,
          input)))))),
      (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>>>(new hydra.core.Name("failure"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseResult<T0>>>) (input -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<hydra.parsing.ParseError, hydra.parsing.ParseResult<T0>>) (t -> (hydra.parsing.ParseResult<T0>) (new hydra.parsing.ParseResult.Failure(t))),
        ((hydra_decode_parsing_parseError2).apply(cx)).apply(input))))))));
  }
  
  static <T0> hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>> parseSuccess(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> a, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>>left(new hydra.util.DecodingError("expected record of type hydra.parsing.ParseSuccess"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "value",
              a,
              fieldMap,
              cx),
            (java.util.function.Function<T0, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>>>) (field_value -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "remainder",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
                  (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError(err))))),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                    }
                    
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                      return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                          return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                        }
                        
                        @Override
                        public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                          return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right((s).value)));
                        }
                      });
                    }
                  })),
                  hydra.lexical.Lexical.stripAndDereferenceTermEither(
                    cx2,
                    raw2)))),
                fieldMap,
                cx),
              (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>>>) (field_remainder -> (hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.parsing.ParseSuccess<T0>>right((hydra.parsing.ParseSuccess<T0>) (new hydra.parsing.ParseSuccess<T0>(field_value, field_remainder)))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
