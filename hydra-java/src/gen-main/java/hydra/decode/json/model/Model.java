// Note: this is an automatically generated file. Do not edit.

package hydra.decode.json.model;

/**
 * Term decoders for hydra.json.model
 */
public interface Model {
  static hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value> value(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>) ((hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>) (hydra.util.Either.<hydra.util.DecodingError, hydra.json.model.Value>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>) ((hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>) (hydra.util.Either.<hydra.util.DecodingError, hydra.json.model.Value>left(new hydra.util.DecodingError("expected union of type hydra.json.model.Value"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (((inj)).value).field;
          hydra.core.Name fname = ((field)).name;
          hydra.core.Term fterm = ((field)).term;
          hydra.core.Name tname = (((inj)).value).typeName;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>(new hydra.core.Name("array"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.json.model.Value>) (t -> new hydra.json.model.Value.Array((t))),
              hydra.extract.helpers.Helpers.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) (p0 -> p1 -> hydra.decode.json.model.Model.value(
                  (p0),
                  (p1))),
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>(new hydra.core.Name("boolean"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.json.model.Value>) (t -> new hydra.json.model.Value.Boolean_((t))),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Boolean>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>left(new hydra.util.DecodingError((err)))))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Boolean>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, Boolean> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>left(new hydra.util.DecodingError("expected literal"))));
                  }
                  
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, Boolean> visit(hydra.core.Term.Literal v) {
                    return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Boolean> otherwise(hydra.core.Literal instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>left(new hydra.util.DecodingError("expected boolean literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, Boolean> visit(hydra.core.Literal.Boolean_ b) {
                        return (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>right(((b)).value)));
                      }
                    });
                  }
                })),
                hydra.lexical.Lexical.stripAndDereferenceTermEither(
                  (cx),
                  (input)))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>(new hydra.core.Name("null"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.json.model.Value>) (t -> new hydra.json.model.Value.Null()),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>(new hydra.core.Name("number"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.math.BigDecimal, hydra.json.model.Value>) (t -> new hydra.json.model.Value.Number_((t))),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal>>) (err -> (hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal>) ((hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal>) (hydra.util.Either.<hydra.util.DecodingError, java.math.BigDecimal>left(new hydra.util.DecodingError((err)))))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal> otherwise(hydra.core.Term instance) {
                    return (hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal>) ((hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal>) (hydra.util.Either.<hydra.util.DecodingError, java.math.BigDecimal>left(new hydra.util.DecodingError("expected literal"))));
                  }
                  
                  @Override
                  public hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal> visit(hydra.core.Term.Literal v) {
                    return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal> otherwise(hydra.core.Literal instance) {
                        return (hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal>) ((hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal>) (hydra.util.Either.<hydra.util.DecodingError, java.math.BigDecimal>left(new hydra.util.DecodingError("expected bigfloat literal"))));
                      }
                      
                      @Override
                      public hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal> visit(hydra.core.Literal.Float_ v1) {
                        return (((v1)).value).accept(new hydra.core.FloatValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal> otherwise(hydra.core.FloatValue instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal>) ((hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal>) (hydra.util.Either.<hydra.util.DecodingError, java.math.BigDecimal>left(new hydra.util.DecodingError("expected bigfloat value"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal> visit(hydra.core.FloatValue.Bigfloat f) {
                            return (hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal>) ((hydra.util.Either<hydra.util.DecodingError, java.math.BigDecimal>) (hydra.util.Either.<hydra.util.DecodingError, java.math.BigDecimal>right(((f)).value)));
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.lexical.Lexical.stripAndDereferenceTermEither(
                  (cx),
                  (input)))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>(new hydra.core.Name("object"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.Map<String, hydra.json.model.Value>, hydra.json.model.Value>) (t -> new hydra.json.model.Value.Object_((t))),
              hydra.extract.helpers.Helpers.decodeMap(
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
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) (p0 -> p1 -> hydra.decode.json.model.Model.value(
                  (p0),
                  (p1))),
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>>(new hydra.core.Name("string"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<String, hydra.json.model.Value>) (t -> new hydra.json.model.Value.String_((t))),
              hydra.lib.eithers.Either.apply(
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
                  (cx),
                  (input)))))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>) ((hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>) (hydra.util.Either.<hydra.util.DecodingError, hydra.json.model.Value>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              ((fname)).value,
              " in union type ",
              ((tname)).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>, hydra.util.Either<hydra.util.DecodingError, hydra.json.model.Value>>) (f -> ((f)).apply((fterm))),
            hydra.lib.maps.Lookup.apply(
              (fname),
              variantMap.get()));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
}
