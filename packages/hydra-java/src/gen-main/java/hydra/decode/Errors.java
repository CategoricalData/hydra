// Note: this is an automatically generated file. Do not edit.

package hydra.decode;

/**
 * Term decoders for hydra.errors
 */
public interface Errors {
  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.DecodingError> decodingError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.DecodingError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.DecodingError>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.DecodingError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.DecodingError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.DecodingError>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.DecodingError> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.errors.DecodingError>) (b -> new hydra.errors.DecodingError(b)),
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

  static hydra.util.Either<hydra.errors.DecodingError, java.lang.Void> emptyListError(hydra.graph.Graph cx, hydra.core.Term t) {
    return hydra.extract.Core.decodeUnit(
      cx,
      t);
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_> error(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.Error_>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.Error_>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>(new hydra.core.Name("checking"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.checking.CheckingError, hydra.errors.Error_>) (t -> new hydra.errors.Error_.Checking(t)),
              hydra.decode.error.Checking.checkingError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>(new hydra.core.Name("decoding"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.DecodingError, hydra.errors.Error_>) (t -> new hydra.errors.Error_.Decoding(t)),
              hydra.decode.Errors.decodingError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>(new hydra.core.Name("duplicateBinding"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.DuplicateBindingError, hydra.errors.Error_>) (t -> new hydra.errors.Error_.DuplicateBinding(t)),
              hydra.decode.error.Core.duplicateBindingError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>(new hydra.core.Name("duplicateField"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.DuplicateFieldError, hydra.errors.Error_>) (t -> new hydra.errors.Error_.DuplicateField(t)),
              hydra.decode.error.Core.duplicateFieldError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>(new hydra.core.Name("extraction"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.ExtractionError, hydra.errors.Error_>) (t -> new hydra.errors.Error_.Extraction(t)),
              hydra.decode.Errors.extractionError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>(new hydra.core.Name("inference"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.InferenceError, hydra.errors.Error_>) (t -> new hydra.errors.Error_.Inference(t)),
              hydra.decode.Errors.inferenceError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>(new hydra.core.Name("other"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.OtherError, hydra.errors.Error_>) (t -> new hydra.errors.Error_.Other(t)),
              hydra.decode.Errors.otherError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>(new hydra.core.Name("resolution"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.ResolutionError, hydra.errors.Error_>) (t -> new hydra.errors.Error_.Resolution(t)),
              hydra.decode.Errors.resolutionError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>(new hydra.core.Name("undefinedField"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.UndefinedFieldError, hydra.errors.Error_>) (t -> new hydra.errors.Error_.UndefinedField(t)),
              hydra.decode.error.Core.undefinedFieldError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>(new hydra.core.Name("undefinedTermVariable"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.UndefinedTermVariableError, hydra.errors.Error_>) (t -> new hydra.errors.Error_.UndefinedTermVariable(t)),
              hydra.decode.error.Core.undefinedTermVariableError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>(new hydra.core.Name("untypedTermVariable"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.UntypedTermVariableError, hydra.errors.Error_>) (t -> new hydra.errors.Error_.UntypedTermVariable(t)),
              hydra.decode.error.Core.untypedTermVariableError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>(new hydra.core.Name("unexpectedTermVariant"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.UnexpectedTermVariantError, hydra.errors.Error_>) (t -> new hydra.errors.Error_.UnexpectedTermVariant(t)),
              hydra.decode.error.Core.unexpectedTermVariantError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>(new hydra.core.Name("unexpectedTypeVariant"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.UnexpectedTypeVariantError, hydra.errors.Error_>) (t -> new hydra.errors.Error_.UnexpectedTypeVariant(t)),
              hydra.decode.error.Core.unexpectedTypeVariantError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>>(new hydra.core.Name("unification"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.UnificationError, hydra.errors.Error_>) (t -> new hydra.errors.Error_.Unification(t)),
              hydra.decode.Errors.unificationError(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.Error_>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.Error_>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError> extractionError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.ExtractionError>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.ExtractionError>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>(new hydra.core.Name("emptyList"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.errors.ExtractionError>) (t -> new hydra.errors.ExtractionError.EmptyList(t)),
              hydra.decode.Errors.emptyListError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>(new hydra.core.Name("multipleBindings"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.MultipleBindingsError, hydra.errors.ExtractionError>) (t -> new hydra.errors.ExtractionError.MultipleBindings(t)),
              hydra.decode.Errors.multipleBindingsError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>(new hydra.core.Name("multipleFields"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.MultipleFieldsError, hydra.errors.ExtractionError>) (t -> new hydra.errors.ExtractionError.MultipleFields(t)),
              hydra.decode.Errors.multipleFieldsError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>(new hydra.core.Name("noMatchingField"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.NoMatchingFieldError, hydra.errors.ExtractionError>) (t -> new hydra.errors.ExtractionError.NoMatchingField(t)),
              hydra.decode.Errors.noMatchingFieldError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>(new hydra.core.Name("noSuchBinding"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.NoSuchBindingError, hydra.errors.ExtractionError>) (t -> new hydra.errors.ExtractionError.NoSuchBinding(t)),
              hydra.decode.Errors.noSuchBindingError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>(new hydra.core.Name("notEnoughCases"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.errors.ExtractionError>) (t -> new hydra.errors.ExtractionError.NotEnoughCases(t)),
              hydra.decode.Errors.notEnoughCasesError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>>(new hydra.core.Name("unexpectedShape"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.UnexpectedShapeError, hydra.errors.ExtractionError>) (t -> new hydra.errors.ExtractionError.UnexpectedShape(t)),
              hydra.decode.Errors.unexpectedShapeError(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.ExtractionError>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ExtractionError>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError> inferenceError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.InferenceError>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.InferenceError>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>>(new hydra.core.Name("checking"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.checking.CheckingError, hydra.errors.InferenceError>) (t -> new hydra.errors.InferenceError.Checking(t)),
              hydra.decode.error.Checking.checkingError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>>(new hydra.core.Name("other"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.OtherInferenceError, hydra.errors.InferenceError>) (t -> new hydra.errors.InferenceError.Other(t)),
              hydra.decode.Errors.otherInferenceError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>>(new hydra.core.Name("unification"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.UnificationInferenceError, hydra.errors.InferenceError>) (t -> new hydra.errors.InferenceError.Unification(t)),
              hydra.decode.Errors.unificationInferenceError(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.InferenceError>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.InferenceError>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.MultipleBindingsError> multipleBindingsError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.MultipleBindingsError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.MultipleBindingsError>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.MultipleBindingsError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.MultipleBindingsError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.MultipleBindingsError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.MultipleBindingsError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.MultipleBindingsError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.MultipleBindingsError>right(new hydra.errors.MultipleBindingsError(field_name))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.MultipleFieldsError> multipleFieldsError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.MultipleFieldsError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.MultipleFieldsError>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.MultipleFieldsError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.MultipleFieldsError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.MultipleFieldsError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.MultipleFieldsError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "fieldName",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.MultipleFieldsError>>) (field_fieldName -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.MultipleFieldsError>right(new hydra.errors.MultipleFieldsError(field_fieldName))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoMatchingFieldError> noMatchingFieldError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoMatchingFieldError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.NoMatchingFieldError>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoMatchingFieldError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoMatchingFieldError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.NoMatchingFieldError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoMatchingFieldError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "fieldName",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoMatchingFieldError>>) (field_fieldName -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.NoMatchingFieldError>right(new hydra.errors.NoMatchingFieldError(field_fieldName))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoSuchBindingError> noSuchBindingError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoSuchBindingError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.NoSuchBindingError>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoSuchBindingError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoSuchBindingError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.NoSuchBindingError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoSuchBindingError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoSuchBindingError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.NoSuchBindingError>right(new hydra.errors.NoSuchBindingError(field_name))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoSuchPrimitiveError> noSuchPrimitiveError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoSuchPrimitiveError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.NoSuchPrimitiveError>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoSuchPrimitiveError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoSuchPrimitiveError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.NoSuchPrimitiveError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoSuchPrimitiveError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.NoSuchPrimitiveError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.NoSuchPrimitiveError>right(new hydra.errors.NoSuchPrimitiveError(field_name))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, java.lang.Void> notEnoughCasesError(hydra.graph.Graph cx, hydra.core.Term t) {
    return hydra.extract.Core.decodeUnit(
      cx,
      t);
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherError> otherError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.OtherError>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.OtherError>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherError> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.errors.OtherError>) (b -> new hydra.errors.OtherError(b)),
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

  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherInferenceError> otherInferenceError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherInferenceError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.OtherInferenceError>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherInferenceError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherInferenceError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.OtherInferenceError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherInferenceError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "path",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherInferenceError>>) (field_path -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "message",
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
              (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherInferenceError>>) (field_message -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.OtherInferenceError>right(new hydra.errors.OtherInferenceError(field_path, field_message))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherResolutionError> otherResolutionError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherResolutionError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.OtherResolutionError>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherResolutionError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherResolutionError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.OtherResolutionError>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.OtherResolutionError> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.errors.OtherResolutionError>) (b -> new hydra.errors.OtherResolutionError(b)),
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

  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError> resolutionError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.ResolutionError>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.ResolutionError>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>(new hydra.core.Name("noSuchBinding"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.NoSuchBindingError, hydra.errors.ResolutionError>) (t -> new hydra.errors.ResolutionError.NoSuchBinding(t)),
              hydra.decode.Errors.noSuchBindingError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>(new hydra.core.Name("noSuchPrimitive"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.NoSuchPrimitiveError, hydra.errors.ResolutionError>) (t -> new hydra.errors.ResolutionError.NoSuchPrimitive(t)),
              hydra.decode.Errors.noSuchPrimitiveError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>(new hydra.core.Name("noMatchingField"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.NoMatchingFieldError, hydra.errors.ResolutionError>) (t -> new hydra.errors.ResolutionError.NoMatchingField(t)),
              hydra.decode.Errors.noMatchingFieldError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>(new hydra.core.Name("other"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.OtherResolutionError, hydra.errors.ResolutionError>) (t -> new hydra.errors.ResolutionError.Other(t)),
              hydra.decode.Errors.otherResolutionError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>>(new hydra.core.Name("unexpectedShape"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.errors.UnexpectedShapeError, hydra.errors.ResolutionError>) (t -> new hydra.errors.ResolutionError.UnexpectedShape(t)),
              hydra.decode.Errors.unexpectedShapeError(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.ResolutionError>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.ResolutionError>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnexpectedShapeError> unexpectedShapeError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnexpectedShapeError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.UnexpectedShapeError>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnexpectedShapeError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnexpectedShapeError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.UnexpectedShapeError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnexpectedShapeError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
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
            (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnexpectedShapeError>>) (field_expected -> hydra.lib.eithers.Bind.apply(
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
              (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnexpectedShapeError>>) (field_actual -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.UnexpectedShapeError>right(new hydra.errors.UnexpectedShapeError(field_expected, field_actual))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationError> unificationError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.UnificationError>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.UnificationError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "leftType",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationError>>) (field_leftType -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "rightType",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationError>>) (field_rightType -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "message",
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
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationError>>) (field_message -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.UnificationError>right(new hydra.errors.UnificationError(field_leftType, field_rightType, field_message))))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationInferenceError> unificationInferenceError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationInferenceError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.UnificationInferenceError>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationInferenceError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationInferenceError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.UnificationInferenceError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationInferenceError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "path",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationInferenceError>>) (field_path -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "cause",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationError>>>) (p0 -> p1 -> hydra.decode.Errors.unificationError(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.errors.UnificationError, hydra.util.Either<hydra.errors.DecodingError, hydra.errors.UnificationInferenceError>>) (field_cause -> hydra.util.Either.<hydra.errors.DecodingError, hydra.errors.UnificationInferenceError>right(new hydra.errors.UnificationInferenceError(field_path, field_cause))))));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }
}
