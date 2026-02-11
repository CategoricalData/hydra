// Note: this is an automatically generated file. Do not edit.

package hydra.decode.topology;

/**
 * Term decoders for hydra.topology
 */
public interface Topology {
  static hydra.util.Either<hydra.util.DecodingError, java.util.Map<Integer, java.util.List<Integer>>> graph(hydra.graph.Graph v1, hydra.core.Term v2) {
    return hydra.extract.helpers.Helpers.decodeMap(
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (p0 -> p1 -> hydra.decode.topology.Topology.vertex(
        p0,
        p1)),
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>) (v22 -> hydra.extract.helpers.Helpers.decodeList(
        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (p0 -> p1 -> hydra.decode.topology.Topology.vertex(
          p0,
          p1)),
        v12,
        v22))),
      v1,
      v2);
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState> tarjanState(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState>) ((hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState>) (hydra.util.Either.<hydra.util.DecodingError, hydra.topology.TarjanState>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState>) ((hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState>) (hydra.util.Either.<hydra.util.DecodingError, hydra.topology.TarjanState>left(new hydra.util.DecodingError("expected record of type hydra.topology.TarjanState"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "counter",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                  cx2,
                  raw2)))),
              fieldMap,
              cx),
            (java.util.function.Function<Integer, hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState>>) (field_counter -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "indices",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<Integer, Integer>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<Integer, Integer>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMap(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (p0 -> p1 -> hydra.decode.topology.Topology.vertex(
                    p0,
                    p1)),
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                          public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v12) {
                            return ((v12).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
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
                      cx2,
                      raw2)))),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.Map<Integer, Integer>, hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState>>) (field_indices -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "lowLinks",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<Integer, Integer>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<Integer, Integer>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMap(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (p0 -> p1 -> hydra.decode.topology.Topology.vertex(
                      p0,
                      p1)),
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                            public hydra.util.Either<hydra.util.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v12) {
                              return ((v12).value).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
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
                        cx2,
                        raw2)))),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<java.util.Map<Integer, Integer>, hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState>>) (field_lowLinks -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.helpers.Helpers.requireField(
                    "stack",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (p0 -> p1 -> hydra.decode.topology.Topology.vertex(
                        p0,
                        p1)),
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<java.util.List<Integer>, hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState>>) (field_stack -> hydra.lib.eithers.Bind.apply(
                    hydra.extract.helpers.Helpers.requireField(
                      "onStack",
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Set<Integer>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Set<Integer>>>) (v2 -> hydra.extract.helpers.Helpers.decodeSet(
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (p0 -> p1 -> hydra.decode.topology.Topology.vertex(
                          p0,
                          p1)),
                        v1,
                        v2))),
                      fieldMap,
                      cx),
                    (java.util.function.Function<java.util.Set<Integer>, hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState>>) (field_onStack -> hydra.lib.eithers.Bind.apply(
                      hydra.extract.helpers.Helpers.requireField(
                        "sccs",
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<java.util.List<Integer>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<java.util.List<Integer>>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                          (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<Integer>>>) (v22 -> hydra.extract.helpers.Helpers.decodeList(
                            (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>>) (p0 -> p1 -> hydra.decode.topology.Topology.vertex(
                              p0,
                              p1)),
                            v12,
                            v22))),
                          v1,
                          v2))),
                        fieldMap,
                        cx),
                      (java.util.function.Function<java.util.List<java.util.List<Integer>>, hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState>>) (field_sccs -> (hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState>) ((hydra.util.Either<hydra.util.DecodingError, hydra.topology.TarjanState>) (hydra.util.Either.<hydra.util.DecodingError, hydra.topology.TarjanState>right(new hydra.topology.TarjanState(field_counter, field_indices, field_lowLinks, field_stack, field_onStack, field_sccs))))))))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, Integer> vertex(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Integer>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Integer>) ((hydra.util.Either<hydra.util.DecodingError, Integer>) (hydra.util.Either.<hydra.util.DecodingError, Integer>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Integer>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
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
        raw));
  }
}
