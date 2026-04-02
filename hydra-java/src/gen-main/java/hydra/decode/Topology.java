// Note: this is an automatically generated file. Do not edit.

package hydra.decode;

/**
 * Term decoders for hydra.topology
 */
public interface Topology {
  static hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<Integer, hydra.util.ConsList<Integer>>> graph(hydra.graph.Graph v1, hydra.core.Term v2) {
    return hydra.extract.Core.decodeMap(
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>>) (p0 -> p1 -> hydra.decode.Topology.vertex(
        p0,
        p1)),
      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<Integer>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<Integer>>>) (v22 -> hydra.extract.Core.decodeList(
        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>>) (p0 -> p1 -> hydra.decode.Topology.vertex(
          p0,
          p1)),
        v12,
        v22))),
      v1,
      v2);
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.topology.TarjanState> tarjanState(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.topology.TarjanState>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.topology.TarjanState>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.topology.TarjanState>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.topology.TarjanState> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.topology.TarjanState>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.topology.TarjanState> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "counter",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (raw2 -> hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError(err))),
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
                hydra.Lexical.stripAndDereferenceTermEither(
                  cx2,
                  raw2)))),
              fieldMap,
              cx),
            (java.util.function.Function<Integer, hydra.util.Either<hydra.errors.DecodingError, hydra.topology.TarjanState>>) (field_counter -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "indices",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<Integer, Integer>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<Integer, Integer>>>) (v2 -> hydra.extract.Core.decodeMap(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>>) (p0 -> p1 -> hydra.decode.Topology.vertex(
                    p0,
                    p1)),
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (raw2 -> hydra.lib.eithers.Either.apply(
                    (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError(err))),
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
                          public hydra.util.Either<hydra.errors.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v12) {
                            return (v12).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
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
                    hydra.Lexical.stripAndDereferenceTermEither(
                      cx2,
                      raw2)))),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.PersistentMap<Integer, Integer>, hydra.util.Either<hydra.errors.DecodingError, hydra.topology.TarjanState>>) (field_indices -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "lowLinks",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<Integer, Integer>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<Integer, Integer>>>) (v2 -> hydra.extract.Core.decodeMap(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>>) (p0 -> p1 -> hydra.decode.Topology.vertex(
                      p0,
                      p1)),
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (raw2 -> hydra.lib.eithers.Either.apply(
                      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError(err))),
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
                            public hydra.util.Either<hydra.errors.DecodingError, Integer> visit(hydra.core.Literal.Integer_ v12) {
                              return (v12).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
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
                      hydra.Lexical.stripAndDereferenceTermEither(
                        cx2,
                        raw2)))),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.PersistentMap<Integer, Integer>, hydra.util.Either<hydra.errors.DecodingError, hydra.topology.TarjanState>>) (field_lowLinks -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.Core.requireField(
                    "stack",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<Integer>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<Integer>>>) (v2 -> hydra.extract.Core.decodeList(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>>) (p0 -> p1 -> hydra.decode.Topology.vertex(
                        p0,
                        p1)),
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<hydra.util.ConsList<Integer>, hydra.util.Either<hydra.errors.DecodingError, hydra.topology.TarjanState>>) (field_stack -> hydra.lib.eithers.Bind.apply(
                    hydra.extract.Core.requireField(
                      "onStack",
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentSet<Integer>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentSet<Integer>>>) (v2 -> hydra.extract.Core.decodeSet(
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>>) (p0 -> p1 -> hydra.decode.Topology.vertex(
                          p0,
                          p1)),
                        v1,
                        v2))),
                      fieldMap,
                      cx),
                    (java.util.function.Function<hydra.util.PersistentSet<Integer>, hydra.util.Either<hydra.errors.DecodingError, hydra.topology.TarjanState>>) (field_onStack -> hydra.lib.eithers.Bind.apply(
                      hydra.extract.Core.requireField(
                        "sccs",
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.util.ConsList<Integer>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.util.ConsList<Integer>>>>) (v2 -> hydra.extract.Core.decodeList(
                          (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<Integer>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<Integer>>>) (v22 -> hydra.extract.Core.decodeList(
                            (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>>) (p0 -> p1 -> hydra.decode.Topology.vertex(
                              p0,
                              p1)),
                            v12,
                            v22))),
                          v1,
                          v2))),
                        fieldMap,
                        cx),
                      (java.util.function.Function<hydra.util.ConsList<hydra.util.ConsList<Integer>>, hydra.util.Either<hydra.errors.DecodingError, hydra.topology.TarjanState>>) (field_sccs -> hydra.util.Either.<hydra.errors.DecodingError, hydra.topology.TarjanState>right(new hydra.topology.TarjanState(field_counter, field_indices, field_lowLinks, field_stack, field_onStack, field_sccs))))))))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, Integer> vertex(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Integer>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Integer>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
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
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
