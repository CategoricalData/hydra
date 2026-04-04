// Note: this is an automatically generated file. Do not edit.

package hydra.decode;

/**
 * Term decoders for hydra.packaging
 */
public interface Packaging {
  static hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition> definition(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.Definition>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.Definition>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.util.ConsList.of(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition>>>(new hydra.core.Name("term"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.packaging.TermDefinition, hydra.packaging.Definition>) (t -> new hydra.packaging.Definition.Term(t)),
              hydra.decode.Packaging.termDefinition(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition>>>(new hydra.core.Name("type"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.packaging.TypeDefinition, hydra.packaging.Definition>) (t -> new hydra.packaging.Definition.Type(t)),
              hydra.decode.Packaging.typeDefinition(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.Definition>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition>>, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.FileExtension> fileExtension(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.FileExtension>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.FileExtension>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.FileExtension>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.FileExtension> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.FileExtension>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.FileExtension> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.packaging.FileExtension>) (b -> new hydra.packaging.FileExtension(b)),
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

  static hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Module> module(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Module>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.Module>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Module>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Module> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.Module>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Module> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "namespace",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespace>>>) (p0 -> p1 -> hydra.decode.Packaging.namespace(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.packaging.Namespace, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Module>>) (field_namespace -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "definitions",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.packaging.Definition>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.packaging.Definition>>>) (v2 -> hydra.extract.Core.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Definition>>>) (p0 -> p1 -> hydra.decode.Packaging.definition(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.ConsList<hydra.packaging.Definition>, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Module>>) (field_definitions -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "termDependencies",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.packaging.Namespace>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.packaging.Namespace>>>) (v2 -> hydra.extract.Core.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespace>>>) (p0 -> p1 -> hydra.decode.Packaging.namespace(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.ConsList<hydra.packaging.Namespace>, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Module>>) (field_termDependencies -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.Core.requireField(
                    "typeDependencies",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.packaging.Namespace>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.packaging.Namespace>>>) (v2 -> hydra.extract.Core.decodeList(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespace>>>) (p0 -> p1 -> hydra.decode.Packaging.namespace(
                        p0,
                        p1)),
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<hydra.util.ConsList<hydra.packaging.Namespace>, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Module>>) (field_typeDependencies -> hydra.lib.eithers.Bind.apply(
                    hydra.extract.Core.requireField(
                      "description",
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<String>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<String>>>) (v2 -> hydra.extract.Core.decodeMaybe(
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
                    (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Module>>) (field_description -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.Module>right(new hydra.packaging.Module(field_namespace, field_definitions, field_termDependencies, field_typeDependencies, field_description))))))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespace> namespace(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespace>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.Namespace>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespace>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespace> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.Namespace>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespace> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.packaging.Namespace>) (b -> new hydra.packaging.Namespace(b)),
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

  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespaces<T0>> namespaces(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> n, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespaces<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.Namespaces<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespaces<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespaces<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.Namespaces<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespaces<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "focus",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Pair<hydra.packaging.Namespace, T0>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Pair<hydra.packaging.Namespace, T0>>>) (v2 -> hydra.extract.Core.decodePair(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespace>>>) (p0 -> p1 -> hydra.decode.Packaging.namespace(
                  p0,
                  p1)),
                n,
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.util.Pair<hydra.packaging.Namespace, T0>, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespaces<T0>>>) (field_focus -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "mapping",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.packaging.Namespace, T0>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.packaging.Namespace, T0>>>) (v2 -> hydra.extract.Core.decodeMap(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespace>>>) (p0 -> p1 -> hydra.decode.Packaging.namespace(
                    p0,
                    p1)),
                  n,
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.PersistentMap<hydra.packaging.Namespace, T0>, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespaces<T0>>>) (field_mapping -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.Namespaces<T0>>right((hydra.packaging.Namespaces<T0>) (new hydra.packaging.Namespaces<T0>(field_focus, field_mapping)))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Package_> package_(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Package_>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.Package_>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Package_>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Package_> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.Package_>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Package_> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.PackageName>>>) (p0 -> p1 -> hydra.decode.Packaging.packageName(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.packaging.PackageName, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Package_>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "modules",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.packaging.Module>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.packaging.Module>>>) (v2 -> hydra.extract.Core.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Module>>>) (p0 -> p1 -> hydra.decode.Packaging.module(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.ConsList<hydra.packaging.Module>, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Package_>>) (field_modules -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "dependencies",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.packaging.PackageName>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.packaging.PackageName>>>) (v2 -> hydra.extract.Core.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.PackageName>>>) (p0 -> p1 -> hydra.decode.Packaging.packageName(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.ConsList<hydra.packaging.PackageName>, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Package_>>) (field_dependencies -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.Core.requireField(
                    "description",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<String>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<String>>>) (v2 -> hydra.extract.Core.decodeMaybe(
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
                  (java.util.function.Function<hydra.util.Maybe<String>, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Package_>>) (field_description -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.Package_>right(new hydra.packaging.Package_(field_name, field_modules, field_dependencies, field_description))))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.PackageName> packageName(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.PackageName>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.PackageName>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.PackageName>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.PackageName> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.PackageName>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.PackageName> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.packaging.PackageName>) (b -> new hydra.packaging.PackageName(b)),
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

  static hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.QualifiedName> qualifiedName(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.QualifiedName>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.QualifiedName>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.QualifiedName>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.QualifiedName> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.QualifiedName>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.QualifiedName> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "namespace",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.packaging.Namespace>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.packaging.Namespace>>>) (v2 -> hydra.extract.Core.decodeMaybe(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.Namespace>>>) (p0 -> p1 -> hydra.decode.Packaging.namespace(
                  p0,
                  p1)),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.util.Maybe<hydra.packaging.Namespace>, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.QualifiedName>>) (field_namespace -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
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
              (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.QualifiedName>>) (field_local -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.QualifiedName>right(new hydra.packaging.QualifiedName(field_namespace, field_local))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.TermDefinition> termDefinition(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.TermDefinition>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.TermDefinition>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.TermDefinition>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.TermDefinition> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.TermDefinition>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.TermDefinition> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.TermDefinition>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "term",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.TermDefinition>>) (field_term -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "type",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.core.TypeScheme>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.core.TypeScheme>>>) (v2 -> hydra.extract.Core.decodeMaybe(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeScheme>>>) (p0 -> p1 -> hydra.decode.Core.typeScheme(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.Maybe<hydra.core.TypeScheme>, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.TermDefinition>>) (field_type -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.TermDefinition>right(new hydra.packaging.TermDefinition(field_name, field_term, field_type))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.TypeDefinition> typeDefinition(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.TypeDefinition>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.TypeDefinition>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.TypeDefinition>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.TypeDefinition> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.TypeDefinition>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.TypeDefinition> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.TypeDefinition>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "type",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeScheme>>>) (p0 -> p1 -> hydra.decode.Core.typeScheme(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.TypeScheme, hydra.util.Either<hydra.errors.DecodingError, hydra.packaging.TypeDefinition>>) (field_type -> hydra.util.Either.<hydra.errors.DecodingError, hydra.packaging.TypeDefinition>right(new hydra.packaging.TypeDefinition(field_name, field_type))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
