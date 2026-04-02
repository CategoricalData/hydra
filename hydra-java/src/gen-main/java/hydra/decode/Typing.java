// Note: this is an automatically generated file. Do not edit.

package hydra.decode;

/**
 * Term decoders for hydra.typing
 */
public interface Typing {
  static <T0> hydra.util.Either<hydra.errors.DecodingError, hydra.typing.FunctionStructure<T0>> functionStructure(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, T0>>> env, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.FunctionStructure<T0>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.typing.FunctionStructure<T0>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.FunctionStructure<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.typing.FunctionStructure<T0>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.typing.FunctionStructure<T0>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "typeParams",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Name>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Name>>>) (v2 -> hydra.extract.Core.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.FunctionStructure<T0>>>) (field_typeParams -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "params",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Name>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Name>>>) (v2 -> hydra.extract.Core.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.ConsList<hydra.core.Name>, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.FunctionStructure<T0>>>) (field_params -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "bindings",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Binding>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Binding>>>) (v2 -> hydra.extract.Core.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Binding>>>) (p0 -> p1 -> hydra.decode.Core.binding(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.ConsList<hydra.core.Binding>, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.FunctionStructure<T0>>>) (field_bindings -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.Core.requireField(
                    "body",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                      p0,
                      p1)),
                    fieldMap,
                    cx),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.FunctionStructure<T0>>>) (field_body -> hydra.lib.eithers.Bind.apply(
                    hydra.extract.Core.requireField(
                      "domains",
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Type>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Type>>>) (v2 -> hydra.extract.Core.decodeList(
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                          p0,
                          p1)),
                        v1,
                        v2))),
                      fieldMap,
                      cx),
                    (java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.FunctionStructure<T0>>>) (field_domains -> hydra.lib.eithers.Bind.apply(
                      hydra.extract.Core.requireField(
                        "codomain",
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.core.Type>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.core.Type>>>) (v2 -> hydra.extract.Core.decodeMaybe(
                          (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                            p0,
                            p1)),
                          v1,
                          v2))),
                        fieldMap,
                        cx),
                      (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.FunctionStructure<T0>>>) (field_codomain -> hydra.lib.eithers.Bind.apply(
                        hydra.extract.Core.requireField(
                          "environment",
                          env,
                          fieldMap,
                          cx),
                        (java.util.function.Function<T0, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.FunctionStructure<T0>>>) (field_environment -> hydra.util.Either.<hydra.errors.DecodingError, hydra.typing.FunctionStructure<T0>>right((hydra.typing.FunctionStructure<T0>) (new hydra.typing.FunctionStructure<T0>(field_typeParams, field_params, field_bindings, field_body, field_domains, field_codomain, field_environment)))))))))))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.typing.InferenceResult> inferenceResult(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.InferenceResult>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.typing.InferenceResult>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.InferenceResult>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.typing.InferenceResult> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.typing.InferenceResult>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.typing.InferenceResult> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "term",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.InferenceResult>>) (field_term -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "type",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.InferenceResult>>) (field_type -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "subst",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TypeSubst>>>) (p0 -> p1 -> hydra.decode.Typing.typeSubst(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.InferenceResult>>) (field_subst -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.Core.requireField(
                    "classConstraints",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (v2 -> hydra.extract.Core.decodeMap(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                        p0,
                        p1)),
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeVariableMetadata>>>) (p0 -> p1 -> hydra.decode.Core.typeVariableMetadata(
                        p0,
                        p1)),
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.TypeVariableMetadata>, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.InferenceResult>>) (field_classConstraints -> hydra.lib.eithers.Bind.apply(
                    hydra.extract.Core.requireField(
                      "context",
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.context.Context>>>) (p0 -> p1 -> hydra.decode.Context.context(
                        p0,
                        p1)),
                      fieldMap,
                      cx),
                    (java.util.function.Function<hydra.context.Context, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.InferenceResult>>) (field_context -> hydra.util.Either.<hydra.errors.DecodingError, hydra.typing.InferenceResult>right(new hydra.typing.InferenceResult(field_term, field_type, field_subst, field_classConstraints, field_context))))))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TermSubst> termSubst(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TermSubst>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.typing.TermSubst>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TermSubst>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TermSubst> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.typing.TermSubst>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TermSubst> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term>, hydra.typing.TermSubst>) (b -> new hydra.typing.TermSubst(b)),
            hydra.extract.Core.decodeMap(
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                p0,
                p1)),
              cx,
              (wrappedTerm).value.body));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TypeConstraint> typeConstraint(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TypeConstraint>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.typing.TypeConstraint>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TypeConstraint>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TypeConstraint> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.typing.TypeConstraint>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TypeConstraint> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "left",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TypeConstraint>>) (field_left -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "right",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TypeConstraint>>) (field_right -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "comment",
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
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TypeConstraint>>) (field_comment -> hydra.util.Either.<hydra.errors.DecodingError, hydra.typing.TypeConstraint>right(new hydra.typing.TypeConstraint(field_left, field_right, field_comment))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TypeSubst> typeSubst(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TypeSubst>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.typing.TypeSubst>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TypeSubst>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TypeSubst> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.typing.TypeSubst>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TypeSubst> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type>, hydra.typing.TypeSubst>) (b -> new hydra.typing.TypeSubst(b)),
            hydra.extract.Core.decodeMap(
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                p0,
                p1)),
              cx,
              (wrappedTerm).value.body));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
