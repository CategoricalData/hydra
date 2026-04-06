// Note: this is an automatically generated file. Do not edit.

package hydra.decode;

/**
 * Term decoders for hydra.core
 */
public interface Core {
  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.AnnotatedTerm> annotatedTerm(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.AnnotatedTerm>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.AnnotatedTerm>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.AnnotatedTerm>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.AnnotatedTerm> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.AnnotatedTerm>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.AnnotatedTerm> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "body",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.AnnotatedTerm>>) (field_body -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "annotation",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.Map<hydra.core.Name, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.Map<hydra.core.Name, hydra.core.Term>>>) (v2 -> hydra.extract.Core.decodeMap(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                    p0,
                    p1)),
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.AnnotatedTerm>>) (field_annotation -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.AnnotatedTerm>right(new hydra.core.AnnotatedTerm(field_body, field_annotation))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.AnnotatedType> annotatedType(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.AnnotatedType>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.AnnotatedType>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.AnnotatedType>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.AnnotatedType> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.AnnotatedType>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.AnnotatedType> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "body",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.AnnotatedType>>) (field_body -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "annotation",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.Map<hydra.core.Name, hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.Map<hydra.core.Name, hydra.core.Term>>>) (v2 -> hydra.extract.Core.decodeMap(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                    p0,
                    p1)),
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.Map<hydra.core.Name, hydra.core.Term>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.AnnotatedType>>) (field_annotation -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.AnnotatedType>right(new hydra.core.AnnotatedType(field_body, field_annotation))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.Application> application(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Application>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Application>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Application>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Application> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Application>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Application> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "function",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Application>>) (field_function -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "argument",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Application>>) (field_argument -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Application>right(new hydra.core.Application(field_function, field_argument))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.ApplicationType> applicationType(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.ApplicationType>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.ApplicationType>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.ApplicationType>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.ApplicationType> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.ApplicationType>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.ApplicationType> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "function",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.ApplicationType>>) (field_function -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "argument",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.ApplicationType>>) (field_argument -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.ApplicationType>right(new hydra.core.ApplicationType(field_function, field_argument))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.Binding> binding(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Binding>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Binding>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Binding>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Binding> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Binding>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Binding> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Binding>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "term",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Binding>>) (field_term -> hydra.lib.eithers.Bind.apply(
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
                (java.util.function.Function<hydra.util.Maybe<hydra.core.TypeScheme>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Binding>>) (field_type -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Binding>right(new hydra.core.Binding(field_name, field_term, field_type))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.CaseStatement> caseStatement(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.CaseStatement>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.CaseStatement>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.CaseStatement>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.CaseStatement> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.CaseStatement>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.CaseStatement> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "typeName",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.core.CaseStatement>>) (field_typeName -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "default",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.core.Term>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.core.Term>>>) (v2 -> hydra.extract.Core.decodeMaybe(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.CaseStatement>>) (field_default -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "cases",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.core.Field>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.core.Field>>>) (v2 -> hydra.extract.Core.decodeList(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Field>>>) (p0 -> p1 -> hydra.decode.Core.field(
                      p0,
                      p1)),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.CaseStatement>>) (field_cases -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.CaseStatement>right(new hydra.core.CaseStatement(field_typeName, field_default, field_cases))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.EitherType> eitherType(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.EitherType>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.EitherType>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.EitherType>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.EitherType> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.EitherType>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.EitherType> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "left",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.EitherType>>) (field_left -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "right",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.EitherType>>) (field_right -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.EitherType>right(new hydra.core.EitherType(field_left, field_right))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination> elimination(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Elimination>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Elimination>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>>(new hydra.core.Name("record"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Projection, hydra.core.Elimination>) (t -> new hydra.core.Elimination.Record(t)),
              hydra.decode.Core.projection(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>>(new hydra.core.Name("union"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.CaseStatement, hydra.core.Elimination>) (t -> new hydra.core.Elimination.Union(t)),
              hydra.decode.Core.caseStatement(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>>(new hydra.core.Name("wrap"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.core.Elimination>) (t -> new hydra.core.Elimination.Wrap(t)),
              hydra.decode.Core.name(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Elimination>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Elimination>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.Field> field(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Field>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Field>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Field>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Field> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Field>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Field> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Field>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "term",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Field>>) (field_term -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Field>right(new hydra.core.Field(field_name, field_term))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.FieldType> fieldType(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FieldType>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.FieldType>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FieldType>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.FieldType> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.FieldType>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.FieldType> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FieldType>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "type",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FieldType>>) (field_type -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.FieldType>right(new hydra.core.FieldType(field_name, field_type))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType> floatType(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.FloatType>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.FloatType>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>>(new hydra.core.Name("bigfloat"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.FloatType>) (t -> new hydra.core.FloatType.Bigfloat()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>>(new hydra.core.Name("float32"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.FloatType>) (t -> new hydra.core.FloatType.Float32()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>>(new hydra.core.Name("float64"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.FloatType>) (t -> new hydra.core.FloatType.Float64()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.FloatType>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatType>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue> floatValue(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.FloatValue>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.FloatValue>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>>(new hydra.core.Name("bigfloat"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.math.BigDecimal, hydra.core.FloatValue>) (t -> new hydra.core.FloatValue.Bigfloat(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, java.math.BigDecimal>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, java.math.BigDecimal>left(new hydra.errors.DecodingError(err))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.math.BigDecimal>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, java.math.BigDecimal> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<hydra.errors.DecodingError, java.math.BigDecimal>left(new hydra.errors.DecodingError("expected literal"));
                  }

                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, java.math.BigDecimal> visit(hydra.core.Term.Literal v) {
                    return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, java.math.BigDecimal> otherwise(hydra.core.Literal instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, java.math.BigDecimal>left(new hydra.errors.DecodingError("expected bigfloat literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, java.math.BigDecimal> visit(hydra.core.Literal.Float_ v1) {
                        return (v1).value.accept(new hydra.core.FloatValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, java.math.BigDecimal> otherwise(hydra.core.FloatValue instance) {
                            return hydra.util.Either.<hydra.errors.DecodingError, java.math.BigDecimal>left(new hydra.errors.DecodingError("expected bigfloat value"));
                          }

                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, java.math.BigDecimal> visit(hydra.core.FloatValue.Bigfloat f) {
                            return hydra.util.Either.<hydra.errors.DecodingError, java.math.BigDecimal>right((f).value);
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>>(new hydra.core.Name("float32"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Float, hydra.core.FloatValue>) (t -> new hydra.core.FloatValue.Float32(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, Float>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Float>left(new hydra.errors.DecodingError(err))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Float>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Float> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<hydra.errors.DecodingError, Float>left(new hydra.errors.DecodingError("expected literal"));
                  }

                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Float> visit(hydra.core.Term.Literal v) {
                    return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Float> otherwise(hydra.core.Literal instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, Float>left(new hydra.errors.DecodingError("expected float32 literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Float> visit(hydra.core.Literal.Float_ v1) {
                        return (v1).value.accept(new hydra.core.FloatValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Float> otherwise(hydra.core.FloatValue instance) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Float>left(new hydra.errors.DecodingError("expected float32 value"));
                          }

                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Float> visit(hydra.core.FloatValue.Float32 f) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Float>right((f).value);
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>>(new hydra.core.Name("float64"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Double, hydra.core.FloatValue>) (t -> new hydra.core.FloatValue.Float64(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, Double>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Double>left(new hydra.errors.DecodingError(err))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Double>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Double> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<hydra.errors.DecodingError, Double>left(new hydra.errors.DecodingError("expected literal"));
                  }

                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Double> visit(hydra.core.Term.Literal v) {
                    return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Double> otherwise(hydra.core.Literal instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, Double>left(new hydra.errors.DecodingError("expected float64 literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Double> visit(hydra.core.Literal.Float_ v1) {
                        return (v1).value.accept(new hydra.core.FloatValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Double> otherwise(hydra.core.FloatValue instance) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Double>left(new hydra.errors.DecodingError("expected float64 value"));
                          }

                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Double> visit(hydra.core.FloatValue.Float64 f) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Double>right((f).value);
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.FloatValue>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FloatValue>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.ForallType> forallType(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.ForallType>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.ForallType>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.ForallType>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.ForallType> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.ForallType>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.ForallType> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "parameter",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.core.ForallType>>) (field_parameter -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "body",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.ForallType>>) (field_body -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.ForallType>right(new hydra.core.ForallType(field_parameter, field_body))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function> function(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Function>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Function>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function>>>(new hydra.core.Name("elimination"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Elimination, hydra.core.Function>) (t -> new hydra.core.Function.Elimination(t)),
              hydra.decode.Core.elimination(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function>>>(new hydra.core.Name("lambda"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Lambda, hydra.core.Function>) (t -> new hydra.core.Function.Lambda(t)),
              hydra.decode.Core.lambda(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Function>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function>>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Function>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.FunctionType> functionType(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FunctionType>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.FunctionType>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FunctionType>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.FunctionType> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.FunctionType>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.FunctionType> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "domain",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FunctionType>>) (field_domain -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "codomain",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FunctionType>>) (field_codomain -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.FunctionType>right(new hydra.core.FunctionType(field_domain, field_codomain))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.Injection> injection(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Injection>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Injection>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Injection>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Injection> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Injection>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Injection> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "typeName",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Injection>>) (field_typeName -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "field",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Field>>>) (p0 -> p1 -> hydra.decode.Core.field(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Injection>>) (field_field -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Injection>right(new hydra.core.Injection(field_typeName, field_field))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType> integerType(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.IntegerType>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.IntegerType>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>(new hydra.core.Name("bigint"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.IntegerType>) (t -> new hydra.core.IntegerType.Bigint()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>(new hydra.core.Name("int8"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.IntegerType>) (t -> new hydra.core.IntegerType.Int8()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>(new hydra.core.Name("int16"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.IntegerType>) (t -> new hydra.core.IntegerType.Int16()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>(new hydra.core.Name("int32"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.IntegerType>) (t -> new hydra.core.IntegerType.Int32()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>(new hydra.core.Name("int64"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.IntegerType>) (t -> new hydra.core.IntegerType.Int64()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>(new hydra.core.Name("uint8"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.IntegerType>) (t -> new hydra.core.IntegerType.Uint8()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>(new hydra.core.Name("uint16"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.IntegerType>) (t -> new hydra.core.IntegerType.Uint16()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>(new hydra.core.Name("uint32"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.IntegerType>) (t -> new hydra.core.IntegerType.Uint32()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>>(new hydra.core.Name("uint64"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.IntegerType>) (t -> new hydra.core.IntegerType.Uint64()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.IntegerType>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerType>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue> integerValue(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.IntegerValue>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.IntegerValue>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>(new hydra.core.Name("bigint"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.math.BigInteger, hydra.core.IntegerValue>) (t -> new hydra.core.IntegerValue.Bigint(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, java.math.BigInteger>left(new hydra.errors.DecodingError(err))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<hydra.errors.DecodingError, java.math.BigInteger>left(new hydra.errors.DecodingError("expected literal"));
                  }

                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger> visit(hydra.core.Term.Literal v) {
                    return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger> otherwise(hydra.core.Literal instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, java.math.BigInteger>left(new hydra.errors.DecodingError("expected bigint literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger> visit(hydra.core.Literal.Integer_ v1) {
                        return (v1).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger> otherwise(hydra.core.IntegerValue instance) {
                            return hydra.util.Either.<hydra.errors.DecodingError, java.math.BigInteger>left(new hydra.errors.DecodingError("expected bigint value"));
                          }

                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger> visit(hydra.core.IntegerValue.Bigint i) {
                            return hydra.util.Either.<hydra.errors.DecodingError, java.math.BigInteger>right((i).value);
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>(new hydra.core.Name("int8"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Byte, hydra.core.IntegerValue>) (t -> new hydra.core.IntegerValue.Int8(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, Byte>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Byte>left(new hydra.errors.DecodingError(err))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Byte>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Byte> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<hydra.errors.DecodingError, Byte>left(new hydra.errors.DecodingError("expected literal"));
                  }

                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Byte> visit(hydra.core.Term.Literal v) {
                    return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Byte> otherwise(hydra.core.Literal instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, Byte>left(new hydra.errors.DecodingError("expected int8 literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Byte> visit(hydra.core.Literal.Integer_ v1) {
                        return (v1).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Byte> otherwise(hydra.core.IntegerValue instance) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Byte>left(new hydra.errors.DecodingError("expected int8 value"));
                          }

                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Byte> visit(hydra.core.IntegerValue.Int8 i) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Byte>right((i).value);
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>(new hydra.core.Name("int16"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Short, hydra.core.IntegerValue>) (t -> new hydra.core.IntegerValue.Int16(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, Short>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Short>left(new hydra.errors.DecodingError(err))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Short>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Short> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<hydra.errors.DecodingError, Short>left(new hydra.errors.DecodingError("expected literal"));
                  }

                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Short> visit(hydra.core.Term.Literal v) {
                    return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Short> otherwise(hydra.core.Literal instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, Short>left(new hydra.errors.DecodingError("expected int16 literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Short> visit(hydra.core.Literal.Integer_ v1) {
                        return (v1).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Short> otherwise(hydra.core.IntegerValue instance) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Short>left(new hydra.errors.DecodingError("expected int16 value"));
                          }

                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Short> visit(hydra.core.IntegerValue.Int16 i) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Short>right((i).value);
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>(new hydra.core.Name("int32"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Integer, hydra.core.IntegerValue>) (t -> new hydra.core.IntegerValue.Int32(t)),
              hydra.lib.eithers.Either.apply(
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
                  cx,
                  input))))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>(new hydra.core.Name("int64"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Long, hydra.core.IntegerValue>) (t -> new hydra.core.IntegerValue.Int64(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, Long>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Long>left(new hydra.errors.DecodingError(err))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Long>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Long> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<hydra.errors.DecodingError, Long>left(new hydra.errors.DecodingError("expected literal"));
                  }

                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Long> visit(hydra.core.Term.Literal v) {
                    return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Long> otherwise(hydra.core.Literal instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, Long>left(new hydra.errors.DecodingError("expected int64 literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Long> visit(hydra.core.Literal.Integer_ v1) {
                        return (v1).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Long> otherwise(hydra.core.IntegerValue instance) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Long>left(new hydra.errors.DecodingError("expected int64 value"));
                          }

                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Long> visit(hydra.core.IntegerValue.Int64 i) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Long>right((i).value);
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>(new hydra.core.Name("uint8"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Short, hydra.core.IntegerValue>) (t -> new hydra.core.IntegerValue.Uint8(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, Short>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Short>left(new hydra.errors.DecodingError(err))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Short>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Short> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<hydra.errors.DecodingError, Short>left(new hydra.errors.DecodingError("expected literal"));
                  }

                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Short> visit(hydra.core.Term.Literal v) {
                    return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Short> otherwise(hydra.core.Literal instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, Short>left(new hydra.errors.DecodingError("expected uint8 literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Short> visit(hydra.core.Literal.Integer_ v1) {
                        return (v1).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Short> otherwise(hydra.core.IntegerValue instance) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Short>left(new hydra.errors.DecodingError("expected uint8 value"));
                          }

                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Short> visit(hydra.core.IntegerValue.Uint8 i) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Short>right((i).value);
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>(new hydra.core.Name("uint16"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Character, hydra.core.IntegerValue>) (t -> new hydra.core.IntegerValue.Uint16(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, Character>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Character>left(new hydra.errors.DecodingError(err))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Character>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Character> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<hydra.errors.DecodingError, Character>left(new hydra.errors.DecodingError("expected literal"));
                  }

                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Character> visit(hydra.core.Term.Literal v) {
                    return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Character> otherwise(hydra.core.Literal instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, Character>left(new hydra.errors.DecodingError("expected uint16 literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Character> visit(hydra.core.Literal.Integer_ v1) {
                        return (v1).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Character> otherwise(hydra.core.IntegerValue instance) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Character>left(new hydra.errors.DecodingError("expected uint16 value"));
                          }

                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Character> visit(hydra.core.IntegerValue.Uint16 i) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Character>right((i).value);
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>(new hydra.core.Name("uint32"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Long, hydra.core.IntegerValue>) (t -> new hydra.core.IntegerValue.Uint32(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, Long>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, Long>left(new hydra.errors.DecodingError(err))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, Long>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Long> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<hydra.errors.DecodingError, Long>left(new hydra.errors.DecodingError("expected literal"));
                  }

                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, Long> visit(hydra.core.Term.Literal v) {
                    return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Long> otherwise(hydra.core.Literal instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, Long>left(new hydra.errors.DecodingError("expected uint32 literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, Long> visit(hydra.core.Literal.Integer_ v1) {
                        return (v1).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Long> otherwise(hydra.core.IntegerValue instance) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Long>left(new hydra.errors.DecodingError("expected uint32 value"));
                          }

                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, Long> visit(hydra.core.IntegerValue.Uint32 i) {
                            return hydra.util.Either.<hydra.errors.DecodingError, Long>right((i).value);
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>>(new hydra.core.Name("uint64"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.math.BigInteger, hydra.core.IntegerValue>) (t -> new hydra.core.IntegerValue.Uint64(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, java.math.BigInteger>left(new hydra.errors.DecodingError(err))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<hydra.errors.DecodingError, java.math.BigInteger>left(new hydra.errors.DecodingError("expected literal"));
                  }

                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger> visit(hydra.core.Term.Literal v) {
                    return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger> otherwise(hydra.core.Literal instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, java.math.BigInteger>left(new hydra.errors.DecodingError("expected uint64 literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger> visit(hydra.core.Literal.Integer_ v1) {
                        return (v1).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger> otherwise(hydra.core.IntegerValue instance) {
                            return hydra.util.Either.<hydra.errors.DecodingError, java.math.BigInteger>left(new hydra.errors.DecodingError("expected uint64 value"));
                          }

                          @Override
                          public hydra.util.Either<hydra.errors.DecodingError, java.math.BigInteger> visit(hydra.core.IntegerValue.Uint64 i) {
                            return hydra.util.Either.<hydra.errors.DecodingError, java.math.BigInteger>right((i).value);
                          }
                        });
                      }
                    });
                  }
                })),
                hydra.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.IntegerValue>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.IntegerValue>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.Lambda> lambda(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Lambda>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Lambda>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Lambda>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Lambda> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Lambda>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Lambda> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "parameter",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Lambda>>) (field_parameter -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "domain",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.core.Type>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<hydra.core.Type>>>) (v2 -> hydra.extract.Core.decodeMaybe(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.util.Maybe<hydra.core.Type>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Lambda>>) (field_domain -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "body",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                    p0,
                    p1)),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Lambda>>) (field_body -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Lambda>right(new hydra.core.Lambda(field_parameter, field_domain, field_body))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.Let> let(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Let>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Let>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Let>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Let> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Let>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Let> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "bindings",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.core.Binding>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.core.Binding>>>) (v2 -> hydra.extract.Core.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Binding>>>) (p0 -> p1 -> hydra.decode.Core.binding(
                  p0,
                  p1)),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<java.util.List<hydra.core.Binding>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Let>>) (field_bindings -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "body",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Let>>) (field_body -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Let>right(new hydra.core.Let(field_bindings, field_body))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal> literal(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Literal>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Literal>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>(new hydra.core.Name("binary"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<byte[], hydra.core.Literal>) (t -> new hydra.core.Literal.Binary(t)),
              hydra.lib.eithers.Either.apply(
                (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, byte[]>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, byte[]>left(new hydra.errors.DecodingError(err))),
                (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, byte[]>>) (stripped2 -> (stripped2).accept(new hydra.core.Term.PartialVisitor<>() {
                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, byte[]> otherwise(hydra.core.Term instance) {
                    return hydra.util.Either.<hydra.errors.DecodingError, byte[]>left(new hydra.errors.DecodingError("expected literal"));
                  }

                  @Override
                  public hydra.util.Either<hydra.errors.DecodingError, byte[]> visit(hydra.core.Term.Literal v) {
                    return (v).value.accept(new hydra.core.Literal.PartialVisitor<>() {
                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, byte[]> otherwise(hydra.core.Literal instance) {
                        return hydra.util.Either.<hydra.errors.DecodingError, byte[]>left(new hydra.errors.DecodingError("expected binary literal"));
                      }

                      @Override
                      public hydra.util.Either<hydra.errors.DecodingError, byte[]> visit(hydra.core.Literal.Binary b) {
                        return hydra.util.Either.<hydra.errors.DecodingError, byte[]>right((b).value);
                      }
                    });
                  }
                })),
                hydra.Lexical.stripAndDereferenceTermEither(
                  cx,
                  input))))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>(new hydra.core.Name("boolean"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.core.Literal>) (t -> new hydra.core.Literal.Boolean_(t)),
              hydra.lib.eithers.Either.apply(
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
                  cx,
                  input))))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>(new hydra.core.Name("float"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.FloatValue, hydra.core.Literal>) (t -> new hydra.core.Literal.Float_(t)),
              hydra.decode.Core.floatValue(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>(new hydra.core.Name("integer"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.IntegerValue, hydra.core.Literal>) (t -> new hydra.core.Literal.Integer_(t)),
              hydra.decode.Core.integerValue(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>>(new hydra.core.Name("string"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<String, hydra.core.Literal>) (t -> new hydra.core.Literal.String_(t)),
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
                  input))))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Literal>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Literal>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType> literalType(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.LiteralType>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.LiteralType>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>(new hydra.core.Name("binary"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.LiteralType>) (t -> new hydra.core.LiteralType.Binary()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>(new hydra.core.Name("boolean"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.LiteralType>) (t -> new hydra.core.LiteralType.Boolean_()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>(new hydra.core.Name("float"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.FloatType, hydra.core.LiteralType>) (t -> new hydra.core.LiteralType.Float_(t)),
              hydra.decode.Core.floatType(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>(new hydra.core.Name("integer"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.IntegerType, hydra.core.LiteralType>) (t -> new hydra.core.LiteralType.Integer_(t)),
              hydra.decode.Core.integerType(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>>(new hydra.core.Name("string"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.LiteralType>) (t -> new hydra.core.LiteralType.String_()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.LiteralType>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.LiteralType>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.MapType> mapType(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.MapType>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.MapType>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.MapType>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.MapType> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.MapType>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.MapType> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "keys",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.MapType>>) (field_keys -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "values",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.MapType>>) (field_values -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.MapType>right(new hydra.core.MapType(field_keys, field_values))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name> name(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Name>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Name>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.core.Name>) (b -> new hydra.core.Name(b)),
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

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.PairType> pairType(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.PairType>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.PairType>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.PairType>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.PairType> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.PairType>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.PairType> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "first",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.PairType>>) (field_first -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "second",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.PairType>>) (field_second -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.PairType>right(new hydra.core.PairType(field_first, field_second))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.Projection> projection(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Projection>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Projection>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Projection>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Projection> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Projection>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Projection> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "typeName",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Projection>>) (field_typeName -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "field",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Projection>>) (field_field -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Projection>right(new hydra.core.Projection(field_typeName, field_field))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.Record> record(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Record>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Record>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Record>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Record> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Record>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Record> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "typeName",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Record>>) (field_typeName -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "fields",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.core.Field>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.core.Field>>>) (v2 -> hydra.extract.Core.decodeList(
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Field>>>) (p0 -> p1 -> hydra.decode.Core.field(
                    p0,
                    p1)),
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Record>>) (field_fields -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Record>right(new hydra.core.Record(field_typeName, field_fields))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term> term(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Term>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Term>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("annotated"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.AnnotatedTerm, hydra.core.Term>) (t -> new hydra.core.Term.Annotated(t)),
              hydra.decode.Core.annotatedTerm(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("application"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Application, hydra.core.Term>) (t -> new hydra.core.Term.Application(t)),
              hydra.decode.Core.application(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("either"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.util.Either<hydra.core.Term, hydra.core.Term>, hydra.core.Term>) (t -> new hydra.core.Term.Either(t)),
              hydra.extract.Core.decodeEither(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("function"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Function, hydra.core.Term>) (t -> new hydra.core.Term.Function(t)),
              hydra.decode.Core.function(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("let"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Let, hydra.core.Term>) (t -> new hydra.core.Term.Let(t)),
              hydra.decode.Core.let(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("list"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.core.Term>) (t -> new hydra.core.Term.List(t)),
              hydra.extract.Core.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("literal"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Literal, hydra.core.Term>) (t -> new hydra.core.Term.Literal(t)),
              hydra.decode.Core.literal(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("map"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.Map<hydra.core.Term, hydra.core.Term>, hydra.core.Term>) (t -> new hydra.core.Term.Map(t)),
              hydra.extract.Core.decodeMap(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("maybe"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.util.Maybe<hydra.core.Term>, hydra.core.Term>) (t -> new hydra.core.Term.Maybe(t)),
              hydra.extract.Core.decodeMaybe(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("pair"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.core.Term>) (t -> new hydra.core.Term.Pair(t)),
              hydra.extract.Core.decodePair(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("record"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Record, hydra.core.Term>) (t -> new hydra.core.Term.Record(t)),
              hydra.decode.Core.record(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("set"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.Set<hydra.core.Term>, hydra.core.Term>) (t -> new hydra.core.Term.Set(t)),
              hydra.extract.Core.decodeSet(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("typeApplication"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.TypeApplicationTerm, hydra.core.Term>) (t -> new hydra.core.Term.TypeApplication(t)),
              hydra.decode.Core.typeApplicationTerm(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("typeLambda"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.TypeLambda, hydra.core.Term>) (t -> new hydra.core.Term.TypeLambda(t)),
              hydra.decode.Core.typeLambda(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("union"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Injection, hydra.core.Term>) (t -> new hydra.core.Term.Union(t)),
              hydra.decode.Core.injection(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("unit"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.Term>) (t -> new hydra.core.Term.Unit()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("variable"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.core.Term>) (t -> new hydra.core.Term.Variable(t)),
              hydra.decode.Core.name(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>(new hydra.core.Name("wrap"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.WrappedTerm, hydra.core.Term>) (t -> new hydra.core.Term.Wrap(t)),
              hydra.decode.Core.wrappedTerm(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Term>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type> type(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Type>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Type>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("annotated"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.AnnotatedType, hydra.core.Type>) (t -> new hydra.core.Type.Annotated(t)),
              hydra.decode.Core.annotatedType(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("application"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.ApplicationType, hydra.core.Type>) (t -> new hydra.core.Type.Application(t)),
              hydra.decode.Core.applicationType(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("either"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.EitherType, hydra.core.Type>) (t -> new hydra.core.Type.Either(t)),
              hydra.decode.Core.eitherType(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("forall"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.ForallType, hydra.core.Type>) (t -> new hydra.core.Type.Forall(t)),
              hydra.decode.Core.forallType(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("function"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.FunctionType, hydra.core.Type>) (t -> new hydra.core.Type.Function(t)),
              hydra.decode.Core.functionType(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("list"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> new hydra.core.Type.List(t)),
              hydra.decode.Core.type(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("literal"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.LiteralType, hydra.core.Type>) (t -> new hydra.core.Type.Literal(t)),
              hydra.decode.Core.literalType(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("map"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.MapType, hydra.core.Type>) (t -> new hydra.core.Type.Map(t)),
              hydra.decode.Core.mapType(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("maybe"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> new hydra.core.Type.Maybe(t)),
              hydra.decode.Core.type(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("pair"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.PairType, hydra.core.Type>) (t -> new hydra.core.Type.Pair(t)),
              hydra.decode.Core.pairType(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("record"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.core.Type>) (t -> new hydra.core.Type.Record(t)),
              hydra.extract.Core.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FieldType>>>) (p0 -> p1 -> hydra.decode.Core.fieldType(
                  p0,
                  p1)),
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("set"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> new hydra.core.Type.Set(t)),
              hydra.decode.Core.type(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("union"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.util.List<hydra.core.FieldType>, hydra.core.Type>) (t -> new hydra.core.Type.Union(t)),
              hydra.extract.Core.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.FieldType>>>) (p0 -> p1 -> hydra.decode.Core.fieldType(
                  p0,
                  p1)),
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("unit"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.Type>) (t -> new hydra.core.Type.Unit()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("variable"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Name, hydra.core.Type>) (t -> new hydra.core.Type.Variable(t)),
              hydra.decode.Core.name(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("void"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.core.Type>) (t -> new hydra.core.Type.Void_()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>(new hydra.core.Name("wrap"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (t -> new hydra.core.Type.Wrap(t)),
              hydra.decode.Core.type(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.Type>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeApplicationTerm> typeApplicationTerm(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeApplicationTerm>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.TypeApplicationTerm>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeApplicationTerm>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeApplicationTerm> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.TypeApplicationTerm>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeApplicationTerm> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "body",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeApplicationTerm>>) (field_body -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "type",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeApplicationTerm>>) (field_type -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.TypeApplicationTerm>right(new hydra.core.TypeApplicationTerm(field_body, field_type))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeLambda> typeLambda(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeLambda>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.TypeLambda>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeLambda>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeLambda> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.TypeLambda>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeLambda> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "parameter",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeLambda>>) (field_parameter -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "body",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeLambda>>) (field_body -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.TypeLambda>right(new hydra.core.TypeLambda(field_parameter, field_body))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeScheme> typeScheme(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeScheme>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.TypeScheme>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeScheme>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeScheme> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.TypeScheme>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeScheme> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "variables",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.core.Name>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.List<hydra.core.Name>>>) (v2 -> hydra.extract.Core.decodeList(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<java.util.List<hydra.core.Name>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeScheme>>) (field_variables -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "type",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Type>>>) (p0 -> p1 -> hydra.decode.Core.type(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeScheme>>) (field_type -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.requireField(
                  "constraints",
                  (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) (v2 -> hydra.extract.Core.decodeMaybe(
                    (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>>) (v22 -> hydra.extract.Core.decodeMap(
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                        p0,
                        p1)),
                      (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeVariableMetadata>>>) (p0 -> p1 -> hydra.decode.Core.typeVariableMetadata(
                        p0,
                        p1)),
                      v12,
                      v22))),
                    v1,
                    v2))),
                  fieldMap,
                  cx),
                (java.util.function.Function<hydra.util.Maybe<java.util.Map<hydra.core.Name, hydra.core.TypeVariableMetadata>>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeScheme>>) (field_constraints -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.TypeScheme>right(new hydra.core.TypeScheme(field_variables, field_type, field_constraints))))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeVariableMetadata> typeVariableMetadata(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeVariableMetadata>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.TypeVariableMetadata>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeVariableMetadata>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeVariableMetadata> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.TypeVariableMetadata>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeVariableMetadata> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "classes",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.Set<hydra.core.Name>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, java.util.Set<hydra.core.Name>>>) (v2 -> hydra.extract.Core.decodeSet(
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                  p0,
                  p1)),
                v1,
                v2))),
              fieldMap,
              cx),
            (java.util.function.Function<java.util.Set<hydra.core.Name>, hydra.util.Either<hydra.errors.DecodingError, hydra.core.TypeVariableMetadata>>) (field_classes -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.TypeVariableMetadata>right(new hydra.core.TypeVariableMetadata(field_classes))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static hydra.util.Either<hydra.errors.DecodingError, hydra.core.WrappedTerm> wrappedTerm(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.core.WrappedTerm>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.WrappedTerm>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.WrappedTerm>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.WrappedTerm> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.core.WrappedTerm>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.core.WrappedTerm> visit(hydra.core.Term.Record record) {
          java.util.Map<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Core.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.requireField(
              "typeName",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.core.WrappedTerm>>) (field_typeName -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Core.requireField(
                "body",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Term>>>) (p0 -> p1 -> hydra.decode.Core.term(
                  p0,
                  p1)),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.WrappedTerm>>) (field_body -> hydra.util.Either.<hydra.errors.DecodingError, hydra.core.WrappedTerm>right(new hydra.core.WrappedTerm(field_typeName, field_body))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
