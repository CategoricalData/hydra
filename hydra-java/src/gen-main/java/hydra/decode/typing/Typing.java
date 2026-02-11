// Note: this is an automatically generated file. Do not edit.

package hydra.decode.typing;

/**
 * Term decoders for hydra.typing
 */
public interface Typing {
  static <T0> hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>> functionStructure(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> env, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>left(new hydra.util.DecodingError("expected record of type hydra.typing.FunctionStructure"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "typeParams",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.core.Name>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.core.Name>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                  p0,
                  p1)),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<java.util.List<hydra.core.Name>, hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>>) (field_typeParams -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "params",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.core.Name>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.core.Name>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.List<hydra.core.Name>, hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>>) (field_params -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "bindings",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.core.Binding>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.core.Binding>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Binding>>>) (p0 -> p1 -> hydra.decode.core.Core.binding(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>>) (field_bindings -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.helpers.Helpers.requireField(
                    "body",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                      p0,
                      p1)),
                    fieldMap,
                    cx),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>>) (field_body -> hydra.lib.eithers.Bind.apply(
                    hydra.extract.helpers.Helpers.requireField(
                      "domains",
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.core.Type>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<hydra.core.Type>>>) (v2 -> hydra.extract.helpers.Helpers.decodeList(
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                          p0,
                          p1)),
                        v1,
                        v2))),
                      fieldMap,
                      cx),
                    (java.util.function.Function<java.util.List<hydra.core.Type>, hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>>) (field_domains -> hydra.lib.eithers.Bind.apply(
                      hydra.extract.helpers.Helpers.requireField(
                        "codomain",
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<hydra.core.Type>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<hydra.core.Type>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMaybe(
                          (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                            p0,
                            p1)),
                          v1,
                          v2))),
                        fieldMap,
                        cx),
                      (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>>) (field_codomain -> hydra.lib.eithers.Bind.apply(
                        hydra.extract.helpers.Helpers.requireField(
                          "environment",
                          env,
                          fieldMap,
                          cx),
                        (java.util.function.Function<T0, hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>>) (field_environment -> (hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.FunctionStructure<T0>>right((hydra.typing.FunctionStructure<T0>) (new hydra.typing.FunctionStructure<T0>(field_typeParams, field_params, field_bindings, field_body, field_domains, field_codomain, field_environment)))))))))))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext> inferenceContext(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.InferenceContext>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.InferenceContext>left(new hydra.util.DecodingError("expected record of type hydra.typing.InferenceContext"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "schemaTypes",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMap(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                  p0,
                  p1)),
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.TypeScheme>>>) (p0 -> p1 -> hydra.decode.core.Core.typeScheme(
                  p0,
                  p1)),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>, hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext>>) (field_schemaTypes -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "primitiveTypes",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMap(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                    p0,
                    p1)),
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.TypeScheme>>>) (p0 -> p1 -> hydra.decode.core.Core.typeScheme(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>, hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext>>) (field_primitiveTypes -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "dataTypes",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.TypeScheme>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMap(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                      p0,
                      p1)),
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.TypeScheme>>>) (p0 -> p1 -> hydra.decode.core.Core.typeScheme(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeScheme>, hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext>>) (field_dataTypes -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.helpers.Helpers.requireField(
                    "classConstraints",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMap(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                        p0,
                        p1)),
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.TypeVariableMetadata>>>) (p0 -> p1 -> hydra.decode.core.Core.typeVariableMetadata(
                        p0,
                        p1)),
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext>>) (field_classConstraints -> hydra.lib.eithers.Bind.apply(
                    hydra.extract.helpers.Helpers.requireField(
                      "debug",
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Boolean>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Boolean>>) (raw2 -> hydra.lib.eithers.Either.apply(
                        (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, Boolean>>) (err -> (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>left(new hydra.util.DecodingError(err))))),
                        (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, Boolean>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Boolean> otherwise(hydra.core.Term instance) {
                            return (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>left(new hydra.util.DecodingError("expected literal"))));
                          }
                          
                          @Override
                          public hydra.util.Either<hydra.util.DecodingError, Boolean> visit(hydra.core.Term.Literal v) {
                            return ((v).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                              @Override
                              public hydra.util.Either<hydra.util.DecodingError, Boolean> otherwise(hydra.core.Literal instance) {
                                return (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>left(new hydra.util.DecodingError("expected boolean literal"))));
                              }
                              
                              @Override
                              public hydra.util.Either<hydra.util.DecodingError, Boolean> visit(hydra.core.Literal.Boolean_ b) {
                                return (hydra.util.Either<hydra.util.DecodingError, Boolean>) ((hydra.util.Either<hydra.util.DecodingError, Boolean>) (hydra.util.Either.<hydra.util.DecodingError, Boolean>right((b).value)));
                              }
                            });
                          }
                        })),
                        hydra.lexical.Lexical.stripAndDereferenceTermEither(
                          cx2,
                          raw2)))),
                      fieldMap,
                      cx),
                    (java.util.function.Function<Boolean, hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext>>) (field_debug -> (hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.InferenceContext>right(new hydra.typing.InferenceContext(field_schemaTypes, field_primitiveTypes, field_dataTypes, field_classConstraints, field_debug))))))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceResult> inferenceResult(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceResult>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.InferenceResult>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceResult>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceResult> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.InferenceResult>left(new hydra.util.DecodingError("expected record of type hydra.typing.InferenceResult"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceResult> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "term",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceResult>>) (field_term -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "type",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceResult>>) (field_type -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "subst",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeSubst>>>) (p0 -> p1 -> hydra.decode.typing.Typing.typeSubst(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceResult>>) (field_subst -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.helpers.Helpers.requireField(
                    "classConstraints",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMap(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                        p0,
                        p1)),
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.TypeVariableMetadata>>>) (p0 -> p1 -> hydra.decode.core.Core.typeVariableMetadata(
                        p0,
                        p1)),
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>, hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceResult>>) (field_classConstraints -> (hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceResult>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceResult>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.InferenceResult>right(new hydra.typing.InferenceResult(field_term, field_type, field_subst, field_classConstraints))))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.typing.TermSubst> termSubst(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TermSubst>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.typing.TermSubst>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.TermSubst>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.TermSubst>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TermSubst>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.typing.TermSubst> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.typing.TermSubst>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.TermSubst>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.TermSubst>left(new hydra.util.DecodingError("expected wrapped type hydra.typing.TermSubst"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.typing.TermSubst> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, hydra.typing.TermSubst>) (b -> new hydra.typing.TermSubst(b)),
            hydra.extract.helpers.Helpers.decodeMap(
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                p0,
                p1)),
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                p0,
                p1)),
              cx,
              ((wrappedTerm).value).body));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeConstraint> typeConstraint(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeConstraint>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeConstraint>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeConstraint>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.TypeConstraint>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeConstraint>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeConstraint> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeConstraint>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeConstraint>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.TypeConstraint>left(new hydra.util.DecodingError("expected record of type hydra.typing.TypeConstraint"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeConstraint> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "left",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeConstraint>>) (field_left -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "right",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeConstraint>>) (field_right -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "comment",
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
                (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeConstraint>>) (field_comment -> (hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeConstraint>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeConstraint>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.TypeConstraint>right(new hydra.typing.TypeConstraint(field_left, field_right, field_comment))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext> typeContext(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.TypeContext>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.TypeContext>left(new hydra.util.DecodingError("expected record of type hydra.typing.TypeContext"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.helpers.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.helpers.Helpers.requireField(
              "types",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.Type>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.Type>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMap(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                  p0,
                  p1)),
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                  p0,
                  p1)),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext>>) (field_types -> hydra.lib.eithers.Bind.apply(
              hydra.extract.helpers.Helpers.requireField(
                "metadata",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<hydra.core.Name, hydra.core.Term>>>) (v2 -> hydra.extract.helpers.Helpers.decodeMap(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                    p0,
                    p1)),
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.core.Core.term(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext>>) (field_metadata -> hydra.lib.eithers.Bind.apply(
                hydra.extract.helpers.Helpers.requireField(
                  "typeVariables",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Set<hydra.core.Name>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Set<hydra.core.Name>>>) (v2 -> hydra.extract.helpers.Helpers.decodeSet(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<java.util.Set<hydra.core.Name>, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext>>) (field_typeVariables -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.helpers.Helpers.requireField(
                    "lambdaVariables",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Set<hydra.core.Name>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Set<hydra.core.Name>>>) (v2 -> hydra.extract.helpers.Helpers.decodeSet(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                        p0,
                        p1)),
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<java.util.Set<hydra.core.Name>, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext>>) (field_lambdaVariables -> hydra.lib.eithers.Bind.apply(
                    hydra.extract.helpers.Helpers.requireField(
                      "letVariables",
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Set<hydra.core.Name>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Set<hydra.core.Name>>>) (v2 -> hydra.extract.helpers.Helpers.decodeSet(
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                          p0,
                          p1)),
                        v1,
                        v2))),
                      fieldMap,
                      cx),
                    (java.util.function.Function<java.util.Set<hydra.core.Name>, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext>>) (field_letVariables -> hydra.lib.eithers.Bind.apply(
                      hydra.extract.helpers.Helpers.requireField(
                        "inferenceContext",
                        (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.typing.InferenceContext>>>) (p0 -> p1 -> hydra.decode.typing.Typing.inferenceContext(
                          p0,
                          p1)),
                        fieldMap,
                        cx),
                      (java.util.function.Function<hydra.typing.InferenceContext, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext>>) (field_inferenceContext -> (hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeContext>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.TypeContext>right(new hydra.typing.TypeContext(field_types, field_metadata, field_typeVariables, field_lambdaVariables, field_letVariables, field_inferenceContext))))))))))))))));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeSubst> typeSubst(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeSubst>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeSubst>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeSubst>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.TypeSubst>left(new hydra.util.DecodingError(err))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeSubst>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeSubst> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeSubst>) ((hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeSubst>) (hydra.util.Either.<hydra.util.DecodingError, hydra.typing.TypeSubst>left(new hydra.util.DecodingError("expected wrapped type hydra.typing.TypeSubst"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.typing.TypeSubst> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Type>, hydra.typing.TypeSubst>) (b -> new hydra.typing.TypeSubst(b)),
            hydra.extract.helpers.Helpers.decodeMap(
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.core.Core.name(
                p0,
                p1)),
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.core.Core.type(
                p0,
                p1)),
              cx,
              ((wrappedTerm).value).body));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
