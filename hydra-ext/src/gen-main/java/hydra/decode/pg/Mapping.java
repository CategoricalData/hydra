// Note: this is an automatically generated file. Do not edit.

package hydra.decode.pg;

/**
 * Term decoders for hydra.pg.mapping
 */
public interface Mapping {
  static hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema> annotationSchema(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "vertexLabel",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                  cx2,
                  raw2)))),
              fieldMap,
              cx),
            (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (field_vertexLabel -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "edgeLabel",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                    cx2,
                    raw2)))),
                fieldMap,
                cx),
              (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (field_edgeLabel -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "vertexId",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                      cx2,
                      raw2)))),
                  fieldMap,
                  cx),
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (field_vertexId -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.Core.requireField(
                    "edgeId",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                        cx2,
                        raw2)))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (field_edgeId -> hydra.lib.eithers.Bind.apply(
                    hydra.extract.Core.requireField(
                      "propertyKey",
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                          cx2,
                          raw2)))),
                      fieldMap,
                      cx),
                    (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (field_propertyKey -> hydra.lib.eithers.Bind.apply(
                      hydra.extract.Core.requireField(
                        "propertyValue",
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                            cx2,
                            raw2)))),
                        fieldMap,
                        cx),
                      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (field_propertyValue -> hydra.lib.eithers.Bind.apply(
                        hydra.extract.Core.requireField(
                          "outVertex",
                          (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                              cx2,
                              raw2)))),
                          fieldMap,
                          cx),
                        (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (field_outVertex -> hydra.lib.eithers.Bind.apply(
                          hydra.extract.Core.requireField(
                            "outVertexLabel",
                            (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                                cx2,
                                raw2)))),
                            fieldMap,
                            cx),
                          (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (field_outVertexLabel -> hydra.lib.eithers.Bind.apply(
                            hydra.extract.Core.requireField(
                              "inVertex",
                              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                                  cx2,
                                  raw2)))),
                              fieldMap,
                              cx),
                            (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (field_inVertex -> hydra.lib.eithers.Bind.apply(
                              hydra.extract.Core.requireField(
                                "inVertexLabel",
                                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                                    cx2,
                                    raw2)))),
                                fieldMap,
                                cx),
                              (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (field_inVertexLabel -> hydra.lib.eithers.Bind.apply(
                                hydra.extract.Core.requireField(
                                  "outEdge",
                                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                                      cx2,
                                      raw2)))),
                                  fieldMap,
                                  cx),
                                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (field_outEdge -> hydra.lib.eithers.Bind.apply(
                                  hydra.extract.Core.requireField(
                                    "outEdgeLabel",
                                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                                        cx2,
                                        raw2)))),
                                    fieldMap,
                                    cx),
                                  (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (field_outEdgeLabel -> hydra.lib.eithers.Bind.apply(
                                    hydra.extract.Core.requireField(
                                      "inEdge",
                                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                                          cx2,
                                          raw2)))),
                                      fieldMap,
                                      cx),
                                    (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (field_inEdge -> hydra.lib.eithers.Bind.apply(
                                      hydra.extract.Core.requireField(
                                        "inEdgeLabel",
                                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                                            cx2,
                                            raw2)))),
                                        fieldMap,
                                        cx),
                                      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (field_inEdgeLabel -> hydra.lib.eithers.Bind.apply(
                                        hydra.extract.Core.requireField(
                                          "ignore",
                                          (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
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
                                              cx2,
                                              raw2)))),
                                          fieldMap,
                                          cx),
                                        (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>>) (field_ignore -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.AnnotationSchema>right(new hydra.pg.mapping.AnnotationSchema(field_vertexLabel, field_edgeLabel, field_vertexId, field_edgeId, field_propertyKey, field_propertyValue, field_outVertex, field_outVertexLabel, field_inVertex, field_inVertexLabel, field_outEdge, field_outEdgeLabel, field_inEdge, field_inEdgeLabel, field_ignore))))))))))))))))))))))))))))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.EdgeSpec> edgeSpec(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.EdgeSpec>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.EdgeSpec>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.EdgeSpec>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.EdgeSpec> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.EdgeSpec>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.EdgeSpec> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "label",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.EdgeLabel>>>) (p0 -> p1 -> hydra.decode.pg.Model.edgeLabel(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.pg.model.EdgeLabel, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.EdgeSpec>>) (field_label -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "id",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>>) (p0 -> p1 -> hydra.decode.pg.Mapping.valueSpec(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.pg.mapping.ValueSpec, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.EdgeSpec>>) (field_id -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "out",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>>) (p0 -> p1 -> hydra.decode.pg.Mapping.valueSpec(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.pg.mapping.ValueSpec, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.EdgeSpec>>) (field_out -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.Core.requireField(
                    "in",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>>) (p0 -> p1 -> hydra.decode.pg.Mapping.valueSpec(
                      p0,
                      p1)),
                    fieldMap,
                    cx),
                  (java.util.function.Function<hydra.pg.mapping.ValueSpec, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.EdgeSpec>>) (field_in -> hydra.lib.eithers.Bind.apply(
                    hydra.extract.Core.requireField(
                      "properties",
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.pg.mapping.PropertySpec>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.pg.mapping.PropertySpec>>>) (v2 -> hydra.extract.Core.decodeList(
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.PropertySpec>>>) (p0 -> p1 -> hydra.decode.pg.Mapping.propertySpec(
                          p0,
                          p1)),
                        v1,
                        v2))),
                      fieldMap,
                      cx),
                    (java.util.function.Function<java.util.List<hydra.pg.mapping.PropertySpec>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.EdgeSpec>>) (field_properties -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.EdgeSpec>right(new hydra.pg.mapping.EdgeSpec(field_label, field_id, field_out, field_in, field_properties))))))))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec> elementSpec(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>>>(new hydra.core.Name("vertex"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.pg.mapping.VertexSpec, hydra.pg.mapping.ElementSpec>) (t -> new hydra.pg.mapping.ElementSpec.Vertex(t)),
              hydra.decode.pg.Mapping.vertexSpec(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>>>(new hydra.core.Name("edge"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.pg.mapping.EdgeSpec, hydra.pg.mapping.ElementSpec>) (t -> new hydra.pg.mapping.ElementSpec.Edge(t)),
              hydra.decode.pg.Mapping.edgeSpec(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.PropertySpec> propertySpec(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.PropertySpec>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.PropertySpec>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.PropertySpec>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.PropertySpec> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.PropertySpec>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.PropertySpec> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "key",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.PropertyKey>>>) (p0 -> p1 -> hydra.decode.pg.Model.propertyKey(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.pg.model.PropertyKey, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.PropertySpec>>) (field_key -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "value",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>>) (p0 -> p1 -> hydra.decode.pg.Mapping.valueSpec(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.pg.mapping.ValueSpec, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.PropertySpec>>) (field_value -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.PropertySpec>right(new hydra.pg.mapping.PropertySpec(field_key, field_value))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec> valueSpec(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>>(new hydra.core.Name("value"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.pg.mapping.ValueSpec>) (t -> new hydra.pg.mapping.ValueSpec.Value()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>>(new hydra.core.Name("pattern"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<String, hydra.pg.mapping.ValueSpec>) (t -> new hydra.pg.mapping.ValueSpec.Pattern(t)),
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
                  input))))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.VertexSpec> vertexSpec(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.VertexSpec>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.VertexSpec>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.VertexSpec>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.VertexSpec> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.VertexSpec>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.VertexSpec> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "label",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.model.VertexLabel>>>) (p0 -> p1 -> hydra.decode.pg.Model.vertexLabel(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.pg.model.VertexLabel, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.VertexSpec>>) (field_label -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "id",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.ValueSpec>>>) (p0 -> p1 -> hydra.decode.pg.Mapping.valueSpec(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.pg.mapping.ValueSpec, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.VertexSpec>>) (field_id -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "properties",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.pg.mapping.PropertySpec>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.pg.mapping.PropertySpec>>>) (v2 -> hydra.extract.Core.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.PropertySpec>>>) (p0 -> p1 -> hydra.decode.pg.Mapping.propertySpec(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<java.util.List<hydra.pg.mapping.PropertySpec>, hydra.util.Either<hydra.errors.DecodingError, hydra.pg.mapping.VertexSpec>>) (field_properties -> hydra.util.Either.<hydra.errors.DecodingError, hydra.pg.mapping.VertexSpec>right(new hydra.pg.mapping.VertexSpec(field_label, field_id, field_properties))))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }
}
