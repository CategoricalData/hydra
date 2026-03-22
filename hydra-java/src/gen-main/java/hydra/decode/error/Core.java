// Note: this is an automatically generated file. Do not edit.

package hydra.decode.error;

/**
 * Term decoders for hydra.error.core
 */
public interface Core {
  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateBindingError> duplicateBindingError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateBindingError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.DuplicateBindingError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateBindingError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateBindingError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.DuplicateBindingError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateBindingError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.accessors.AccessorPath>>>) (p0 -> p1 -> hydra.decode.Accessors.accessorPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.accessors.AccessorPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateBindingError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateBindingError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.DuplicateBindingError>right(new hydra.error.core.DuplicateBindingError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateFieldError> duplicateFieldError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateFieldError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.DuplicateFieldError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateFieldError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateFieldError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.DuplicateFieldError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateFieldError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.accessors.AccessorPath>>>) (p0 -> p1 -> hydra.decode.Accessors.accessorPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.accessors.AccessorPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateFieldError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateFieldError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.DuplicateFieldError>right(new hydra.error.core.DuplicateFieldError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError> invalidTermError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.util.ConsList.of(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("duplicateBinding"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.DuplicateBindingError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.DuplicateBinding(t)),
              hydra.decode.error.Core.duplicateBindingError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("duplicateField"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.DuplicateFieldError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.DuplicateField(t)),
              hydra.decode.error.Core.duplicateFieldError(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedFieldError> undefinedFieldError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedFieldError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedFieldError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedFieldError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedFieldError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedFieldError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedFieldError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "fieldName",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedFieldError>>) (field_fieldName -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "typeName",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedFieldError>>) (field_typeName -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedFieldError>right(new hydra.error.core.UndefinedFieldError(field_fieldName, field_typeName))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTermError> undefinedTermError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTermError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTermError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTermError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTermError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTermError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTermError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTermError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTermError>right(new hydra.error.core.UndefinedTermError(field_name))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeError> undefinedTypeError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeError>right(new hydra.error.core.UndefinedTypeError(field_name))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTermVariantError> unexpectedTermVariantError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTermVariantError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UnexpectedTermVariantError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTermVariantError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTermVariantError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UnexpectedTermVariantError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTermVariantError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "expectedVariant",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.variants.TermVariant>>>) (p0 -> p1 -> hydra.decode.Variants.termVariant(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.variants.TermVariant, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTermVariantError>>) (field_expectedVariant -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "actualTerm",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTermVariantError>>) (field_actualTerm -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UnexpectedTermVariantError>right(new hydra.error.core.UnexpectedTermVariantError(field_expectedVariant, field_actualTerm))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTypeVariantError> unexpectedTypeVariantError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTypeVariantError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UnexpectedTypeVariantError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTypeVariantError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTypeVariantError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UnexpectedTypeVariantError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTypeVariantError> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "expectedVariant",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.variants.TypeVariant>>>) (p0 -> p1 -> hydra.decode.Variants.typeVariant(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.variants.TypeVariant, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTypeVariantError>>) (field_expectedVariant -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "actualType",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTypeVariantError>>) (field_actualType -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UnexpectedTypeVariantError>right(new hydra.error.core.UnexpectedTypeVariantError(field_expectedVariant, field_actualType))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
