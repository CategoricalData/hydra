// Note: this is an automatically generated file. Do not edit.

package hydra.decode;

/**
 * Term decoders for hydra.module
 */
public interface Module {
  static hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition> definition(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.module.Definition>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.module.Definition>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.util.ConsList.of(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition>>>(new hydra.core.Name("term"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.module.TermDefinition, hydra.module.Definition>) (t -> new hydra.module.Definition.Term(t)),
              hydra.decode.Module.termDefinition(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition>>>(new hydra.core.Name("type"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.module.TypeDefinition, hydra.module.Definition>) (t -> new hydra.module.Definition.Type(t)),
              hydra.decode.Module.typeDefinition(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.module.Definition>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition>>, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Definition>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.module.FileExtension> fileExtension(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.module.FileExtension>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.module.FileExtension>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.FileExtension>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.FileExtension> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.module.FileExtension>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.FileExtension> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.module.FileExtension>) (b -> new hydra.module.FileExtension(b)),
            hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, String>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError(err))),
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
              hydra.Lexical.stripAndDereferenceTermEither(
                cx,
                (wrappedTerm).value.body)));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.module.Module> module(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Module>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.module.Module>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Module>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.Module> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.module.Module>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.Module> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "namespace",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespace>>>) (p0 -> p1 -> hydra.decode.Module.namespace(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.module.Namespace, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Module>>) (field_namespace -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "elements",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Binding>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Binding>>>) (v2 -> hydra.extract.Helpers.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Binding>>>) (p0 -> p1 -> hydra.decode.Core.binding(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.ConsList<hydra.core.Binding>, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Module>>) (field_elements -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Helpers.requireField(
                  "termDependencies",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.module.Namespace>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.module.Namespace>>>) (v2 -> hydra.extract.Helpers.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespace>>>) (p0 -> p1 -> hydra.decode.Module.namespace(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.ConsList<hydra.module.Namespace>, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Module>>) (field_termDependencies -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.Helpers.requireField(
                    "typeDependencies",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.module.Namespace>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.module.Namespace>>>) (v2 -> hydra.extract.Helpers.decodeList(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespace>>>) (p0 -> p1 -> hydra.decode.Module.namespace(
                        p0,
                        p1)),
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<hydra.util.ConsList<hydra.module.Namespace>, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Module>>) (field_typeDependencies -> hydra.lib.eithers.Bind.apply(
                    hydra.extract.Helpers.requireField(
                      "description",
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<String>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<String>>>) (v2 -> hydra.extract.Helpers.decodeMaybe(
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
                          (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, String>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError(err))),
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
                          hydra.Lexical.stripAndDereferenceTermEither(
                            cx2,
                            raw2)))),
                        v1,
                        v2))),
                      fieldMap,
                      cx),
                    (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Module>>) (field_description -> hydra.util.Either.<hydra.errors.DecodingError, hydra.module.Module>right(new hydra.module.Module(field_namespace, field_elements, field_termDependencies, field_typeDependencies, field_description))))))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespace> namespace(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespace>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.module.Namespace>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespace>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespace> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.module.Namespace>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespace> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.module.Namespace>) (b -> new hydra.module.Namespace(b)),
            hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, String>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError(err))),
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
              hydra.Lexical.stripAndDereferenceTermEither(
                cx,
                (wrappedTerm).value.body)));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespaces<T0>> namespaces(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> n, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespaces<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.module.Namespaces<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespaces<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespaces<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.module.Namespaces<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespaces<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "focus",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Pair<hydra.module.Namespace, T0>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Pair<hydra.module.Namespace, T0>>>) (v2 -> hydra.extract.Helpers.decodePair(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespace>>>) (p0 -> p1 -> hydra.decode.Module.namespace(
                  p0,
                  p1)),
                n,
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.util.Pair<hydra.module.Namespace, T0>, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespaces<T0>>>) (field_focus -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "mapping",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.module.Namespace, T0>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.module.Namespace, T0>>>) (v2 -> hydra.extract.Helpers.decodeMap(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespace>>>) (p0 -> p1 -> hydra.decode.Module.namespace(
                    p0,
                    p1)),
                  n,
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.PersistentMap<hydra.module.Namespace, T0>, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespaces<T0>>>) (field_mapping -> hydra.util.Either.<hydra.errors.DecodingError, hydra.module.Namespaces<T0>>right((hydra.module.Namespaces<T0>) (new hydra.module.Namespaces<T0>(field_focus, field_mapping)))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.module.QualifiedName> qualifiedName(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.module.QualifiedName>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.module.QualifiedName>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.QualifiedName>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.QualifiedName> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.module.QualifiedName>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.QualifiedName> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "namespace",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.module.Namespace>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.module.Namespace>>>) (v2 -> hydra.extract.Helpers.decodeMaybe(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.Namespace>>>) (p0 -> p1 -> hydra.decode.Module.namespace(
                  p0,
                  p1)),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.util.Maybe<hydra.module.Namespace>, hydra.util.Either<hydra.errors.DecodingError, hydra.module.QualifiedName>>) (field_namespace -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "local",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, String>>) (raw2 -> hydra.lib.eithers.Either.apply(
                  (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, String>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, String>left(new hydra.errors.DecodingError(err))),
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
                  hydra.Lexical.stripAndDereferenceTermEither(
                    cx2,
                    raw2)))),
                fieldMap,
                cx),
              (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.module.QualifiedName>>) (field_local -> hydra.util.Either.<hydra.errors.DecodingError, hydra.module.QualifiedName>right(new hydra.module.QualifiedName(field_namespace, field_local))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.module.TermDefinition> termDefinition(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.module.TermDefinition>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.module.TermDefinition>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.TermDefinition>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.TermDefinition> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.module.TermDefinition>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.TermDefinition> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.module.TermDefinition>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "term",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.TermDefinition>>) (field_term -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Helpers.requireField(
                  "type",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeScheme>>>) (p0 -> p1 -> hydra.decode.Core.typeScheme(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.DecodingError, hydra.module.TermDefinition>>) (field_type -> hydra.util.Either.<hydra.errors.DecodingError, hydra.module.TermDefinition>right(new hydra.module.TermDefinition(field_name, field_term, field_type))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.module.TypeDefinition> typeDefinition(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.module.TypeDefinition>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.module.TypeDefinition>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.module.TypeDefinition>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.TypeDefinition> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.module.TypeDefinition>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.module.TypeDefinition> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.module.TypeDefinition>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "type",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.module.TypeDefinition>>) (field_type -> hydra.util.Either.<hydra.errors.DecodingError, hydra.module.TypeDefinition>right(new hydra.module.TypeDefinition(field_name, field_type))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
