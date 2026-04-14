// Note: this is an automatically generated file. Do not edit.

package hydra.decode;

/**
 * Term decoders for hydra.testing
 */
public interface Testing {
  static hydra.util.Either<hydra.errors.DecodingError, hydra.testing.Tag> tag(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.Tag>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.testing.Tag>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.Tag>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.testing.Tag> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.testing.Tag>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.testing.Tag> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.testing.Tag>) (b -> new hydra.testing.Tag(b)),
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

  static hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCase> testCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCase>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.testing.TestCase>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCase> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.testing.TestCase>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCase> visit(hydra.core.Term.Inject inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCase>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCase>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCase>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCase>>>(new hydra.core.Name("universal"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCase>>) (input -> hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.testing.UniversalTestCase, hydra.testing.TestCase>) (t -> new hydra.testing.TestCase.Universal(t)),
            hydra.decode.Testing.universalTestCase(
              cx,
              input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.testing.TestCase>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCase>>, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCase>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCaseWithMetadata> testCaseWithMetadata(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCaseWithMetadata>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.testing.TestCaseWithMetadata>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCaseWithMetadata>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCaseWithMetadata> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.testing.TestCaseWithMetadata>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCaseWithMetadata> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "name",
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
            (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCaseWithMetadata>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "case",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCase>>>) (p0 -> p1 -> hydra.decode.Testing.testCase(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.testing.TestCase, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCaseWithMetadata>>) (field_case -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "description",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<String>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<String>>>) (v2 -> hydra.extract.Core.decodeMaybe(
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
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCaseWithMetadata>>) (field_description -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.Core.requireField(
                    "tags",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.testing.Tag>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.testing.Tag>>>) (v2 -> hydra.extract.Core.decodeList(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.Tag>>>) (p0 -> p1 -> hydra.decode.Testing.tag(
                        p0,
                        p1)),
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<java.util.List<hydra.testing.Tag>, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCaseWithMetadata>>) (field_tags -> hydra.util.Either.<hydra.errors.DecodingError, hydra.testing.TestCaseWithMetadata>right(new hydra.testing.TestCaseWithMetadata(field_name, field_case, field_description, field_tags))))))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestGroup> testGroup(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestGroup>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.testing.TestGroup>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestGroup>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestGroup> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.testing.TestGroup>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestGroup> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "name",
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
            (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestGroup>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "description",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<String>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<String>>>) (v2 -> hydra.extract.Core.decodeMaybe(
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
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestGroup>>) (field_description -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "subgroups",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.testing.TestGroup>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.testing.TestGroup>>>) (v2 -> hydra.extract.Core.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestGroup>>>) (p0 -> p1 -> hydra.decode.Testing.testGroup(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<java.util.List<hydra.testing.TestGroup>, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestGroup>>) (field_subgroups -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.Core.requireField(
                    "cases",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.testing.TestCaseWithMetadata>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.testing.TestCaseWithMetadata>>>) (v2 -> hydra.extract.Core.decodeList(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestCaseWithMetadata>>>) (p0 -> p1 -> hydra.decode.Testing.testCaseWithMetadata(
                        p0,
                        p1)),
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<java.util.List<hydra.testing.TestCaseWithMetadata>, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.TestGroup>>) (field_cases -> hydra.util.Either.<hydra.errors.DecodingError, hydra.testing.TestGroup>right(new hydra.testing.TestGroup(field_name, field_description, field_subgroups, field_cases))))))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.testing.UniversalTestCase> universalTestCase(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.UniversalTestCase>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.testing.UniversalTestCase>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.UniversalTestCase>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.testing.UniversalTestCase> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.testing.UniversalTestCase>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.testing.UniversalTestCase> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "actual",
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
            (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.UniversalTestCase>>) (field_actual -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "expected",
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
              (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.testing.UniversalTestCase>>) (field_expected -> hydra.util.Either.<hydra.errors.DecodingError, hydra.testing.UniversalTestCase>right(new hydra.testing.UniversalTestCase(field_actual, field_expected))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }
}
