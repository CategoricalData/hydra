// Note: this is an automatically generated file. Do not edit.

package hydra.decode.util;

/**
 * Term decoders for hydra.util
 */
public interface Util {
  static hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention> caseConvention(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>) ((hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>) (hydra.util.Either.<hydra.util.DecodingError, hydra.util.CaseConvention>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>) ((hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>) (hydra.util.Either.<hydra.util.DecodingError, hydra.util.CaseConvention>left(new hydra.util.DecodingError("expected union of type hydra.util.CaseConvention"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = ((inj).value).field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.core.Name tname = ((inj).value).typeName;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>>(new hydra.core.Name("camel"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.util.CaseConvention>) (t -> new hydra.util.CaseConvention.Camel()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>>(new hydra.core.Name("pascal"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.util.CaseConvention>) (t -> new hydra.util.CaseConvention.Pascal()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>>(new hydra.core.Name("lowerSnake"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.util.CaseConvention>) (t -> new hydra.util.CaseConvention.LowerSnake()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>>(new hydra.core.Name("upperSnake"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.util.CaseConvention>) (t -> new hydra.util.CaseConvention.UpperSnake()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>) ((hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>) (hydra.util.Either.<hydra.util.DecodingError, hydra.util.CaseConvention>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              (fname).value,
              " in union type ",
              (tname).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>, hydra.util.Either<hydra.util.DecodingError, hydra.util.CaseConvention>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison> comparison(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>) ((hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>) (hydra.util.Either.<hydra.util.DecodingError, hydra.util.Comparison>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>) ((hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>) (hydra.util.Either.<hydra.util.DecodingError, hydra.util.Comparison>left(new hydra.util.DecodingError("expected union of type hydra.util.Comparison"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = ((inj).value).field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.core.Name tname = ((inj).value).typeName;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>>(new hydra.core.Name("lessThan"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.util.Comparison>) (t -> new hydra.util.Comparison.LessThan()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>>(new hydra.core.Name("equalTo"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.util.Comparison>) (t -> new hydra.util.Comparison.EqualTo()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>>(new hydra.core.Name("greaterThan"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.util.Comparison>) (t -> new hydra.util.Comparison.GreaterThan()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>) ((hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>) (hydra.util.Either.<hydra.util.DecodingError, hydra.util.Comparison>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              (fname).value,
              " in union type ",
              (tname).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>, hydra.util.Either<hydra.util.DecodingError, hydra.util.Comparison>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.util.DecodingError> decodingError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.util.DecodingError>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.util.DecodingError>) ((hydra.util.Either<hydra.util.DecodingError, hydra.util.DecodingError>) (hydra.util.Either.<hydra.util.DecodingError, hydra.util.DecodingError>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.DecodingError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.util.DecodingError> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.util.DecodingError>) ((hydra.util.Either<hydra.util.DecodingError, hydra.util.DecodingError>) (hydra.util.Either.<hydra.util.DecodingError, hydra.util.DecodingError>left(new hydra.util.DecodingError("expected wrapped type hydra.util.DecodingError"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.util.DecodingError> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.util.DecodingError>) (b -> new hydra.util.DecodingError(b)),
            hydra.lib.eithers.Either.apply(
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
                cx,
                ((wrappedTerm).value).body)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision> precision(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>) ((hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>) (hydra.util.Either.<hydra.util.DecodingError, hydra.util.Precision>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>) ((hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>) (hydra.util.Either.<hydra.util.DecodingError, hydra.util.Precision>left(new hydra.util.DecodingError("expected union of type hydra.util.Precision"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = ((inj).value).field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.core.Name tname = ((inj).value).typeName;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>>>(new hydra.core.Name("arbitrary"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.util.Precision>) (t -> new hydra.util.Precision.Arbitrary()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>>>(new hydra.core.Name("bits"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Integer, hydra.util.Precision>) (t -> new hydra.util.Precision.Bits(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Integer>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError(err))))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected literal"))));
                  }
                  
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Term.Literal v) {
                    return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.Literal instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v1) {
                        return ((v1).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> otherwise(hydra.core.IntegerValue instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError("expected int32 value"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.IntegerValue.Int32 i) {
                            return (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>right((i).value)));
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.lexical.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>) ((hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>) (hydra.util.Either.<hydra.util.DecodingError, hydra.util.Precision>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              (fname).value,
              " in union type ",
              (tname).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>>, hydra.util.Either<hydra.util.DecodingError, hydra.util.Precision>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
