// Note: this is an automatically generated file. Do not edit.

package hydra.decode.error;

/**
 * Term decoders for hydra.error.core
 */
public interface Core {
  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.ConstantConditionError> constantConditionError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.ConstantConditionError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.ConstantConditionError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.ConstantConditionError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.ConstantConditionError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.ConstantConditionError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.ConstantConditionError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.ConstantConditionError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "value",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Boolean>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Boolean>>) (raw2 -> hydra.lib.eithers.Either.apply(
                  (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, Boolean>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Boolean>left(new hydra.errors.DecodingError(err))),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Boolean>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.errors.DecodingError, Boolean> otherwise(hydra.core.Term instance) {
                      return hydra.util.Either.<hydra.errors.DecodingError, Boolean>left(new hydra.errors.DecodingError("expected literal"));
                    }

                    @Override
                    public hydra.util.Either<hydra.errors.DecodingError, Boolean> visit(hydra.core.Term.Literal v) {
                      return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                        @Override
                        public hydra.util.Either<hydra.errors.DecodingError, Boolean> otherwise(hydra.core.Literal instance) {
                          return hydra.util.Either.<hydra.errors.DecodingError, Boolean>left(new hydra.errors.DecodingError("expected boolean literal"));
                        }

                        @Override
                        public hydra.util.Either<hydra.errors.DecodingError, Boolean> visit(hydra.core.Literal.Boolean_ b) {
                          return hydra.util.Either.<hydra.errors.DecodingError, Boolean>right((b).value);
                        }
                      });
                    }
                  })),
                  hydra.Lexical.stripAndDereferenceTermEither(
                    cx2,
                    raw2)))),
                fieldMap,
                cx),
              (java.util.function.Function<Boolean, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.ConstantConditionError>>) (field_value -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.ConstantConditionError>right(new hydra.error.core.ConstantConditionError(field_location, field_value))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

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
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateBindingError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
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
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateFieldError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
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

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateRecordTypeFieldNamesError> duplicateRecordTypeFieldNamesError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateRecordTypeFieldNamesError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.DuplicateRecordTypeFieldNamesError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateRecordTypeFieldNamesError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateRecordTypeFieldNamesError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.DuplicateRecordTypeFieldNamesError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateRecordTypeFieldNamesError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateRecordTypeFieldNamesError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateRecordTypeFieldNamesError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.DuplicateRecordTypeFieldNamesError>right(new hydra.error.core.DuplicateRecordTypeFieldNamesError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateUnionTypeFieldNamesError> duplicateUnionTypeFieldNamesError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateUnionTypeFieldNamesError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.DuplicateUnionTypeFieldNamesError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateUnionTypeFieldNamesError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateUnionTypeFieldNamesError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.DuplicateUnionTypeFieldNamesError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateUnionTypeFieldNamesError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateUnionTypeFieldNamesError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.DuplicateUnionTypeFieldNamesError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.DuplicateUnionTypeFieldNamesError>right(new hydra.error.core.DuplicateUnionTypeFieldNamesError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyCaseStatementError> emptyCaseStatementError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyCaseStatementError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyCaseStatementError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyCaseStatementError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyCaseStatementError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyCaseStatementError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyCaseStatementError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyCaseStatementError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "typeName",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyCaseStatementError>>) (field_typeName -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyCaseStatementError>right(new hydra.error.core.EmptyCaseStatementError(field_location, field_typeName))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyLetBindingsError> emptyLetBindingsError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyLetBindingsError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyLetBindingsError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyLetBindingsError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyLetBindingsError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyLetBindingsError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyLetBindingsError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyLetBindingsError>>) (field_location -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyLetBindingsError>right(new hydra.error.core.EmptyLetBindingsError(field_location))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyRecordTypeError> emptyRecordTypeError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyRecordTypeError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyRecordTypeError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyRecordTypeError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyRecordTypeError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyRecordTypeError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyRecordTypeError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyRecordTypeError>>) (field_location -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyRecordTypeError>right(new hydra.error.core.EmptyRecordTypeError(field_location))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTermAnnotationError> emptyTermAnnotationError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTermAnnotationError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyTermAnnotationError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTermAnnotationError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTermAnnotationError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyTermAnnotationError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTermAnnotationError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTermAnnotationError>>) (field_location -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyTermAnnotationError>right(new hydra.error.core.EmptyTermAnnotationError(field_location))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTypeAnnotationError> emptyTypeAnnotationError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTypeAnnotationError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyTypeAnnotationError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTypeAnnotationError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTypeAnnotationError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyTypeAnnotationError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTypeAnnotationError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTypeAnnotationError>>) (field_location -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyTypeAnnotationError>right(new hydra.error.core.EmptyTypeAnnotationError(field_location))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTypeNameInTermError> emptyTypeNameInTermError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTypeNameInTermError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyTypeNameInTermError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTypeNameInTermError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTypeNameInTermError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyTypeNameInTermError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTypeNameInTermError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyTypeNameInTermError>>) (field_location -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyTypeNameInTermError>right(new hydra.error.core.EmptyTypeNameInTermError(field_location))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyUnionTypeError> emptyUnionTypeError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyUnionTypeError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyUnionTypeError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyUnionTypeError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyUnionTypeError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyUnionTypeError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyUnionTypeError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.EmptyUnionTypeError>>) (field_location -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.EmptyUnionTypeError>right(new hydra.error.core.EmptyUnionTypeError(field_location))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidForallParameterNameError> invalidForallParameterNameError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidForallParameterNameError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidForallParameterNameError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidForallParameterNameError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidForallParameterNameError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidForallParameterNameError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidForallParameterNameError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidForallParameterNameError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidForallParameterNameError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidForallParameterNameError>right(new hydra.error.core.InvalidForallParameterNameError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidLambdaParameterNameError> invalidLambdaParameterNameError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidLambdaParameterNameError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidLambdaParameterNameError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidLambdaParameterNameError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidLambdaParameterNameError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidLambdaParameterNameError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidLambdaParameterNameError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidLambdaParameterNameError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidLambdaParameterNameError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidLambdaParameterNameError>right(new hydra.error.core.InvalidLambdaParameterNameError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidLetBindingNameError> invalidLetBindingNameError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidLetBindingNameError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidLetBindingNameError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidLetBindingNameError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidLetBindingNameError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidLetBindingNameError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidLetBindingNameError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidLetBindingNameError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidLetBindingNameError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidLetBindingNameError>right(new hydra.error.core.InvalidLetBindingNameError(field_location, field_name))))));
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
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("constantCondition"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.ConstantConditionError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.ConstantCondition(t)),
              hydra.decode.error.Core.constantConditionError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("duplicateBinding"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.DuplicateBindingError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.DuplicateBinding(t)),
              hydra.decode.error.Core.duplicateBindingError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("duplicateField"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.DuplicateFieldError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.DuplicateField(t)),
              hydra.decode.error.Core.duplicateFieldError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("emptyCaseStatement"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.EmptyCaseStatementError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.EmptyCaseStatement(t)),
              hydra.decode.error.Core.emptyCaseStatementError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("emptyLetBindings"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.EmptyLetBindingsError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.EmptyLetBindings(t)),
              hydra.decode.error.Core.emptyLetBindingsError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("emptyTermAnnotation"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.EmptyTermAnnotationError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.EmptyTermAnnotation(t)),
              hydra.decode.error.Core.emptyTermAnnotationError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("emptyTypeNameInTerm"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.EmptyTypeNameInTermError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.EmptyTypeNameInTerm(t)),
              hydra.decode.error.Core.emptyTypeNameInTermError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("invalidLambdaParameterName"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.InvalidLambdaParameterNameError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.InvalidLambdaParameterName(t)),
              hydra.decode.error.Core.invalidLambdaParameterNameError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("invalidLetBindingName"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.InvalidLetBindingNameError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.InvalidLetBindingName(t)),
              hydra.decode.error.Core.invalidLetBindingNameError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("invalidTypeLambdaParameterName"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.InvalidTypeLambdaParameterNameError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.InvalidTypeLambdaParameterName(t)),
              hydra.decode.error.Core.invalidTypeLambdaParameterNameError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("nestedTermAnnotation"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.NestedTermAnnotationError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.NestedTermAnnotation(t)),
              hydra.decode.error.Core.nestedTermAnnotationError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("redundantWrapUnwrap"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.RedundantWrapUnwrapError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.RedundantWrapUnwrap(t)),
              hydra.decode.error.Core.redundantWrapUnwrapError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("selfApplication"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.SelfApplicationError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.SelfApplication(t)),
              hydra.decode.error.Core.selfApplicationError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("termVariableShadowing"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.TermVariableShadowingError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.TermVariableShadowing(t)),
              hydra.decode.error.Core.termVariableShadowingError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("typeVariableShadowingInTypeLambda"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.TypeVariableShadowingInTypeLambdaError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.TypeVariableShadowingInTypeLambda(t)),
              hydra.decode.error.Core.typeVariableShadowingInTypeLambdaError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("undefinedTermVariable"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.UndefinedTermVariableError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.UndefinedTermVariable(t)),
              hydra.decode.error.Core.undefinedTermVariableError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("undefinedTypeVariableInBindingType"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.UndefinedTypeVariableInBindingTypeError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.UndefinedTypeVariableInBindingType(t)),
              hydra.decode.error.Core.undefinedTypeVariableInBindingTypeError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("undefinedTypeVariableInLambdaDomain"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.UndefinedTypeVariableInLambdaDomainError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.UndefinedTypeVariableInLambdaDomain(t)),
              hydra.decode.error.Core.undefinedTypeVariableInLambdaDomainError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("undefinedTypeVariableInTypeApplication"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.UndefinedTypeVariableInTypeApplicationError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.UndefinedTypeVariableInTypeApplication(t)),
              hydra.decode.error.Core.undefinedTypeVariableInTypeApplicationError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("unknownPrimitiveName"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.UnknownPrimitiveNameError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.UnknownPrimitiveName(t)),
              hydra.decode.error.Core.unknownPrimitiveNameError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("unnecessaryIdentityApplication"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.UnnecessaryIdentityApplicationError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.UnnecessaryIdentityApplication(t)),
              hydra.decode.error.Core.unnecessaryIdentityApplicationError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>>(new hydra.core.Name("untypedTermVariable"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.UntypedTermVariableError, hydra.error.core.InvalidTermError>) (t -> new hydra.error.core.InvalidTermError.UntypedTermVariable(t)),
              hydra.decode.error.Core.untypedTermVariableError(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidTermError>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
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

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError> invalidTypeError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>(new hydra.core.Name("duplicateRecordTypeFieldNames"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.DuplicateRecordTypeFieldNamesError, hydra.error.core.InvalidTypeError>) (t -> new hydra.error.core.InvalidTypeError.DuplicateRecordTypeFieldNames(t)),
              hydra.decode.error.Core.duplicateRecordTypeFieldNamesError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>(new hydra.core.Name("duplicateUnionTypeFieldNames"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.DuplicateUnionTypeFieldNamesError, hydra.error.core.InvalidTypeError>) (t -> new hydra.error.core.InvalidTypeError.DuplicateUnionTypeFieldNames(t)),
              hydra.decode.error.Core.duplicateUnionTypeFieldNamesError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>(new hydra.core.Name("emptyRecordType"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.EmptyRecordTypeError, hydra.error.core.InvalidTypeError>) (t -> new hydra.error.core.InvalidTypeError.EmptyRecordType(t)),
              hydra.decode.error.Core.emptyRecordTypeError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>(new hydra.core.Name("emptyTypeAnnotation"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.EmptyTypeAnnotationError, hydra.error.core.InvalidTypeError>) (t -> new hydra.error.core.InvalidTypeError.EmptyTypeAnnotation(t)),
              hydra.decode.error.Core.emptyTypeAnnotationError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>(new hydra.core.Name("emptyUnionType"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.EmptyUnionTypeError, hydra.error.core.InvalidTypeError>) (t -> new hydra.error.core.InvalidTypeError.EmptyUnionType(t)),
              hydra.decode.error.Core.emptyUnionTypeError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>(new hydra.core.Name("invalidForallParameterName"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.InvalidForallParameterNameError, hydra.error.core.InvalidTypeError>) (t -> new hydra.error.core.InvalidTypeError.InvalidForallParameterName(t)),
              hydra.decode.error.Core.invalidForallParameterNameError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>(new hydra.core.Name("invalidTypeSchemeVariableName"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.InvalidTypeSchemeVariableNameError, hydra.error.core.InvalidTypeError>) (t -> new hydra.error.core.InvalidTypeError.InvalidTypeSchemeVariableName(t)),
              hydra.decode.error.Core.invalidTypeSchemeVariableNameError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>(new hydra.core.Name("nestedTypeAnnotation"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.NestedTypeAnnotationError, hydra.error.core.InvalidTypeError>) (t -> new hydra.error.core.InvalidTypeError.NestedTypeAnnotation(t)),
              hydra.decode.error.Core.nestedTypeAnnotationError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>(new hydra.core.Name("nonComparableMapKeyType"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.NonComparableMapKeyTypeError, hydra.error.core.InvalidTypeError>) (t -> new hydra.error.core.InvalidTypeError.NonComparableMapKeyType(t)),
              hydra.decode.error.Core.nonComparableMapKeyTypeError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>(new hydra.core.Name("nonComparableSetElementType"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.NonComparableSetElementTypeError, hydra.error.core.InvalidTypeError>) (t -> new hydra.error.core.InvalidTypeError.NonComparableSetElementType(t)),
              hydra.decode.error.Core.nonComparableSetElementTypeError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>(new hydra.core.Name("singleVariantUnion"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.SingleVariantUnionError, hydra.error.core.InvalidTypeError>) (t -> new hydra.error.core.InvalidTypeError.SingleVariantUnion(t)),
              hydra.decode.error.Core.singleVariantUnionError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>(new hydra.core.Name("typeVariableShadowingInForall"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.TypeVariableShadowingInForallError, hydra.error.core.InvalidTypeError>) (t -> new hydra.error.core.InvalidTypeError.TypeVariableShadowingInForall(t)),
              hydra.decode.error.Core.typeVariableShadowingInForallError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>(new hydra.core.Name("undefinedTypeVariable"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.UndefinedTypeVariableError, hydra.error.core.InvalidTypeError>) (t -> new hydra.error.core.InvalidTypeError.UndefinedTypeVariable(t)),
              hydra.decode.error.Core.undefinedTypeVariableError(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>>(new hydra.core.Name("voidInNonBottomPosition"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.error.core.VoidInNonBottomPositionError, hydra.error.core.InvalidTypeError>) (t -> new hydra.error.core.InvalidTypeError.VoidInNonBottomPosition(t)),
              hydra.decode.error.Core.voidInNonBottomPositionError(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeError>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeLambdaParameterNameError> invalidTypeLambdaParameterNameError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeLambdaParameterNameError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidTypeLambdaParameterNameError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeLambdaParameterNameError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeLambdaParameterNameError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidTypeLambdaParameterNameError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeLambdaParameterNameError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeLambdaParameterNameError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeLambdaParameterNameError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidTypeLambdaParameterNameError>right(new hydra.error.core.InvalidTypeLambdaParameterNameError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeSchemeVariableNameError> invalidTypeSchemeVariableNameError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeSchemeVariableNameError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidTypeSchemeVariableNameError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeSchemeVariableNameError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeSchemeVariableNameError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidTypeSchemeVariableNameError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeSchemeVariableNameError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeSchemeVariableNameError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.InvalidTypeSchemeVariableNameError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.InvalidTypeSchemeVariableNameError>right(new hydra.error.core.InvalidTypeSchemeVariableNameError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NestedTermAnnotationError> nestedTermAnnotationError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NestedTermAnnotationError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.NestedTermAnnotationError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NestedTermAnnotationError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NestedTermAnnotationError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.NestedTermAnnotationError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NestedTermAnnotationError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NestedTermAnnotationError>>) (field_location -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.NestedTermAnnotationError>right(new hydra.error.core.NestedTermAnnotationError(field_location))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NestedTypeAnnotationError> nestedTypeAnnotationError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NestedTypeAnnotationError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.NestedTypeAnnotationError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NestedTypeAnnotationError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NestedTypeAnnotationError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.NestedTypeAnnotationError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NestedTypeAnnotationError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NestedTypeAnnotationError>>) (field_location -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.NestedTypeAnnotationError>right(new hydra.error.core.NestedTypeAnnotationError(field_location))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NonComparableMapKeyTypeError> nonComparableMapKeyTypeError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NonComparableMapKeyTypeError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.NonComparableMapKeyTypeError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NonComparableMapKeyTypeError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NonComparableMapKeyTypeError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.NonComparableMapKeyTypeError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NonComparableMapKeyTypeError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NonComparableMapKeyTypeError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "keyType",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NonComparableMapKeyTypeError>>) (field_keyType -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.NonComparableMapKeyTypeError>right(new hydra.error.core.NonComparableMapKeyTypeError(field_location, field_keyType))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NonComparableSetElementTypeError> nonComparableSetElementTypeError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NonComparableSetElementTypeError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.NonComparableSetElementTypeError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NonComparableSetElementTypeError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NonComparableSetElementTypeError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.NonComparableSetElementTypeError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NonComparableSetElementTypeError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NonComparableSetElementTypeError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "elementType",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.NonComparableSetElementTypeError>>) (field_elementType -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.NonComparableSetElementTypeError>right(new hydra.error.core.NonComparableSetElementTypeError(field_location, field_elementType))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.RedundantWrapUnwrapError> redundantWrapUnwrapError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.RedundantWrapUnwrapError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.RedundantWrapUnwrapError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.RedundantWrapUnwrapError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.RedundantWrapUnwrapError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.RedundantWrapUnwrapError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.RedundantWrapUnwrapError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.RedundantWrapUnwrapError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "typeName",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.RedundantWrapUnwrapError>>) (field_typeName -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.RedundantWrapUnwrapError>right(new hydra.error.core.RedundantWrapUnwrapError(field_location, field_typeName))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.SelfApplicationError> selfApplicationError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.SelfApplicationError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.SelfApplicationError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.SelfApplicationError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.SelfApplicationError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.SelfApplicationError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.SelfApplicationError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.SelfApplicationError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.SelfApplicationError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.SelfApplicationError>right(new hydra.error.core.SelfApplicationError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.SingleVariantUnionError> singleVariantUnionError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.SingleVariantUnionError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.SingleVariantUnionError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.SingleVariantUnionError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.SingleVariantUnionError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.SingleVariantUnionError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.SingleVariantUnionError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.SingleVariantUnionError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "fieldName",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.SingleVariantUnionError>>) (field_fieldName -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.SingleVariantUnionError>right(new hydra.error.core.SingleVariantUnionError(field_location, field_fieldName))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TermVariableShadowingError> termVariableShadowingError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TermVariableShadowingError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.TermVariableShadowingError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TermVariableShadowingError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TermVariableShadowingError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.TermVariableShadowingError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TermVariableShadowingError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TermVariableShadowingError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TermVariableShadowingError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.TermVariableShadowingError>right(new hydra.error.core.TermVariableShadowingError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInForallError> typeVariableShadowingInForallError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInForallError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInForallError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInForallError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInForallError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInForallError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInForallError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInForallError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInForallError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInForallError>right(new hydra.error.core.TypeVariableShadowingInForallError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInTypeLambdaError> typeVariableShadowingInTypeLambdaError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInTypeLambdaError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInTypeLambdaError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInTypeLambdaError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInTypeLambdaError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInTypeLambdaError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInTypeLambdaError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInTypeLambdaError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInTypeLambdaError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.TypeVariableShadowingInTypeLambdaError>right(new hydra.error.core.TypeVariableShadowingInTypeLambdaError(field_location, field_name))))));
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
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "fieldName",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedFieldError>>) (field_fieldName -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
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

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTermVariableError> undefinedTermVariableError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTermVariableError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTermVariableError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTermVariableError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTermVariableError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTermVariableError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTermVariableError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTermVariableError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTermVariableError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTermVariableError>right(new hydra.error.core.UndefinedTermVariableError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableError> undefinedTypeVariableError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableError>right(new hydra.error.core.UndefinedTypeVariableError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInBindingTypeError> undefinedTypeVariableInBindingTypeError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInBindingTypeError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInBindingTypeError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInBindingTypeError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInBindingTypeError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInBindingTypeError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInBindingTypeError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInBindingTypeError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInBindingTypeError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInBindingTypeError>right(new hydra.error.core.UndefinedTypeVariableInBindingTypeError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInLambdaDomainError> undefinedTypeVariableInLambdaDomainError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInLambdaDomainError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInLambdaDomainError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInLambdaDomainError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInLambdaDomainError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInLambdaDomainError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInLambdaDomainError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInLambdaDomainError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInLambdaDomainError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInLambdaDomainError>right(new hydra.error.core.UndefinedTypeVariableInLambdaDomainError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInTypeApplicationError> undefinedTypeVariableInTypeApplicationError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInTypeApplicationError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInTypeApplicationError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInTypeApplicationError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInTypeApplicationError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInTypeApplicationError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInTypeApplicationError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInTypeApplicationError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInTypeApplicationError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UndefinedTypeVariableInTypeApplicationError>right(new hydra.error.core.UndefinedTypeVariableInTypeApplicationError(field_location, field_name))))));
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
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "expectedVariant",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.variants.TermVariant>>>) (p0 -> p1 -> hydra.decode.Variants.termVariant(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.variants.TermVariant, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTermVariantError>>) (field_expectedVariant -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
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
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "expectedVariant",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.variants.TypeVariant>>>) (p0 -> p1 -> hydra.decode.Variants.typeVariant(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.variants.TypeVariant, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnexpectedTypeVariantError>>) (field_expectedVariant -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
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

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnknownPrimitiveNameError> unknownPrimitiveNameError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnknownPrimitiveNameError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UnknownPrimitiveNameError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnknownPrimitiveNameError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnknownPrimitiveNameError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UnknownPrimitiveNameError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnknownPrimitiveNameError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnknownPrimitiveNameError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnknownPrimitiveNameError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UnknownPrimitiveNameError>right(new hydra.error.core.UnknownPrimitiveNameError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnnecessaryIdentityApplicationError> unnecessaryIdentityApplicationError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnnecessaryIdentityApplicationError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UnnecessaryIdentityApplicationError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnnecessaryIdentityApplicationError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnnecessaryIdentityApplicationError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UnnecessaryIdentityApplicationError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnnecessaryIdentityApplicationError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UnnecessaryIdentityApplicationError>>) (field_location -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UnnecessaryIdentityApplicationError>right(new hydra.error.core.UnnecessaryIdentityApplicationError(field_location))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UntypedTermVariableError> untypedTermVariableError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UntypedTermVariableError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UntypedTermVariableError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UntypedTermVariableError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UntypedTermVariableError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UntypedTermVariableError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UntypedTermVariableError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UntypedTermVariableError>>) (field_location -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "name",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.UntypedTermVariableError>>) (field_name -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.UntypedTermVariableError>right(new hydra.error.core.UntypedTermVariableError(field_location, field_name))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.VoidInNonBottomPositionError> voidInNonBottomPositionError(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.VoidInNonBottomPositionError>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.VoidInNonBottomPositionError>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.VoidInNonBottomPositionError>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.VoidInNonBottomPositionError> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.VoidInNonBottomPositionError>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.VoidInNonBottomPositionError> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "location",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.paths.SubtermPath>>>) (p0 -> p1 -> hydra.decode.Paths.subtermPath(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.paths.SubtermPath, hydra.util.Either<hydra.errors.DecodingError, hydra.error.core.VoidInNonBottomPositionError>>) (field_location -> hydra.util.Either.<hydra.errors.DecodingError, hydra.error.core.VoidInNonBottomPositionError>right(new hydra.error.core.VoidInNonBottomPositionError(field_location))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
