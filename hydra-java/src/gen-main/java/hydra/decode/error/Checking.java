// Note: this is an automatically generated file. Do not edit.

package hydra.decode.error;

/**
 * Term decoders for hydra.error.checking
 */
public interface Checking {
  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError> checkingError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.CheckingError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.CheckingError>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.util.ConsList.of(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>(new hydra.core.Name("incorrectUnification"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.checking.IncorrectUnificationError, hydra.error.checking.CheckingError>) (t -> new hydra.error.checking.CheckingError.IncorrectUnification(t)),
              hydra.decode.error.Checking.incorrectUnificationError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>(new hydra.core.Name("notAForallType"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.checking.NotAForallTypeError, hydra.error.checking.CheckingError>) (t -> new hydra.error.checking.CheckingError.NotAForallType(t)),
              hydra.decode.error.Checking.notAForallTypeError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>(new hydra.core.Name("notAFunctionType"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.checking.NotAFunctionTypeError, hydra.error.checking.CheckingError>) (t -> new hydra.error.checking.CheckingError.NotAFunctionType(t)),
              hydra.decode.error.Checking.notAFunctionTypeError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>(new hydra.core.Name("typeArityMismatch"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.checking.TypeArityMismatchError, hydra.error.checking.CheckingError>) (t -> new hydra.error.checking.CheckingError.TypeArityMismatch(t)),
              hydra.decode.error.Checking.typeArityMismatchError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>(new hydra.core.Name("typeMismatch"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.checking.TypeMismatchError, hydra.error.checking.CheckingError>) (t -> new hydra.error.checking.CheckingError.TypeMismatch(t)),
              hydra.decode.error.Checking.typeMismatchError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>(new hydra.core.Name("unboundTypeVariables"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.checking.UnboundTypeVariablesError, hydra.error.checking.CheckingError>) (t -> new hydra.error.checking.CheckingError.UnboundTypeVariables(t)),
              hydra.decode.error.Checking.unboundTypeVariablesError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>(new hydra.core.Name("unequalTypes"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.checking.UnequalTypesError, hydra.error.checking.CheckingError>) (t -> new hydra.error.checking.CheckingError.UnequalTypes(t)),
              hydra.decode.error.Checking.unequalTypesError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>(new hydra.core.Name("unsupportedTermVariant"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.checking.UnsupportedTermVariantError, hydra.error.checking.CheckingError>) (t -> new hydra.error.checking.CheckingError.UnsupportedTermVariant(t)),
              hydra.decode.error.Checking.unsupportedTermVariantError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>(new hydra.core.Name("untypedLambda"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.checking.UntypedLambdaError, hydra.error.checking.CheckingError>) (t -> new hydra.error.checking.CheckingError.UntypedLambda(t)),
              hydra.decode.error.Checking.untypedLambdaError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>>(new hydra.core.Name("untypedLetBinding"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.checking.UntypedLetBindingError, hydra.error.checking.CheckingError>) (t -> new hydra.error.checking.CheckingError.UntypedLetBinding(t)),
              hydra.decode.error.Checking.untypedLetBindingError(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.CheckingError>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.CheckingError>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.IncorrectUnificationError> incorrectUnificationError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.IncorrectUnificationError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.IncorrectUnificationError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.IncorrectUnificationError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.IncorrectUnificationError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.IncorrectUnificationError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.IncorrectUnificationError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "substitution",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.typing.TypeSubst>>>) (p0 -> p1 -> hydra.decode.Typing.typeSubst(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.typing.TypeSubst, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.IncorrectUnificationError>>) (field_substitution -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.IncorrectUnificationError>right(new hydra.error.checking.IncorrectUnificationError(field_substitution))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.NotAForallTypeError> notAForallTypeError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.NotAForallTypeError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.NotAForallTypeError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.NotAForallTypeError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.NotAForallTypeError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.NotAForallTypeError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.NotAForallTypeError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "type",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.NotAForallTypeError>>) (field_type -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "typeArguments",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Type>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Type>>>) (v2 -> hydra.extract.Helpers.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.NotAForallTypeError>>) (field_typeArguments -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.NotAForallTypeError>right(new hydra.error.checking.NotAForallTypeError(field_type, field_typeArguments))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.NotAFunctionTypeError> notAFunctionTypeError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.NotAFunctionTypeError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.NotAFunctionTypeError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.NotAFunctionTypeError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.NotAFunctionTypeError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.NotAFunctionTypeError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.NotAFunctionTypeError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "type",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.NotAFunctionTypeError>>) (field_type -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.NotAFunctionTypeError>right(new hydra.error.checking.NotAFunctionTypeError(field_type))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeArityMismatchError> typeArityMismatchError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeArityMismatchError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.TypeArityMismatchError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeArityMismatchError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeArityMismatchError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.TypeArityMismatchError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeArityMismatchError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "type",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeArityMismatchError>>) (field_type -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "expectedArity",
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
              (java.util.function.Function<Integer, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeArityMismatchError>>) (field_expectedArity -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Helpers.requireField(
                  "actualArity",
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
                (java.util.function.Function<Integer, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeArityMismatchError>>) (field_actualArity -> hydra.lib.eithers.Bind.apply(
                  hydra.extract.Helpers.requireField(
                    "typeArguments",
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Type>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Type>>>) (v2 -> hydra.extract.Helpers.decodeList(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                        p0,
                        p1)),
                      v1,
                      v2))),
                    fieldMap,
                    cx),
                  (java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeArityMismatchError>>) (field_typeArguments -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.TypeArityMismatchError>right(new hydra.error.checking.TypeArityMismatchError(field_type, field_expectedArity, field_actualArity, field_typeArguments))))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeMismatchError> typeMismatchError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeMismatchError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.TypeMismatchError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeMismatchError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeMismatchError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.TypeMismatchError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeMismatchError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "expectedType",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeMismatchError>>) (field_expectedType -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "actualType",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.TypeMismatchError>>) (field_actualType -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.TypeMismatchError>right(new hydra.error.checking.TypeMismatchError(field_expectedType, field_actualType))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnboundTypeVariablesError> unboundTypeVariablesError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnboundTypeVariablesError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.UnboundTypeVariablesError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnboundTypeVariablesError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnboundTypeVariablesError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.UnboundTypeVariablesError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnboundTypeVariablesError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "variables",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentSet<hydra.core.Name>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.PersistentSet<hydra.core.Name>>>) (v2 -> hydra.extract.Helpers.decodeSet(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.util.PersistentSet<hydra.core.Name>, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnboundTypeVariablesError>>) (field_variables -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "type",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnboundTypeVariablesError>>) (field_type -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.UnboundTypeVariablesError>right(new hydra.error.checking.UnboundTypeVariablesError(field_variables, field_type))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnequalTypesError> unequalTypesError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnequalTypesError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.UnequalTypesError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnequalTypesError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnequalTypesError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.UnequalTypesError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnequalTypesError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "types",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Type>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.ConsList<hydra.core.Type>>>) (v2 -> hydra.extract.Helpers.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.util.ConsList<hydra.core.Type>, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnequalTypesError>>) (field_types -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "description",
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
              (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnequalTypesError>>) (field_description -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.UnequalTypesError>right(new hydra.error.checking.UnequalTypesError(field_types, field_description))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnsupportedTermVariantError> unsupportedTermVariantError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnsupportedTermVariantError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.UnsupportedTermVariantError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnsupportedTermVariantError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnsupportedTermVariantError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.UnsupportedTermVariantError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnsupportedTermVariantError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "termVariant",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.variants.TermVariant>>>) (p0 -> p1 -> hydra.decode.Variants.termVariant(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.variants.TermVariant, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UnsupportedTermVariantError>>) (field_termVariant -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.UnsupportedTermVariantError>right(new hydra.error.checking.UnsupportedTermVariantError(field_termVariant))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UntypedLambdaError> untypedLambdaError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UntypedLambdaError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.UntypedLambdaError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UntypedLambdaError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UntypedLambdaError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.UntypedLambdaError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UntypedLambdaError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.UntypedLambdaError>right(new hydra.error.checking.UntypedLambdaError());
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UntypedLetBindingError> untypedLetBindingError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UntypedLetBindingError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.UntypedLetBindingError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UntypedLetBindingError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UntypedLetBindingError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.UntypedLetBindingError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UntypedLetBindingError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "binding",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Binding>>>) (p0 -> p1 -> hydra.decode.Core.binding(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.errors.DecodingError, hydra.error.checking.UntypedLetBindingError>>) (field_binding -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.checking.UntypedLetBindingError>right(new hydra.error.checking.UntypedLetBindingError(field_binding))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
