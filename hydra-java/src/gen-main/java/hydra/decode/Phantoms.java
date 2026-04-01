// Note: this is an automatically generated file. Do not edit.

package hydra.decode;

/**
 * Term decoders for hydra.phantoms
 */
public interface Phantoms {
  static <T0, T1> hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TBinding<T1>> tBinding(T0 a, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TBinding<T1>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.phantoms.TBinding<T1>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TBinding<T1>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TBinding<T1>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.phantoms.TBinding<T1>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TBinding<T1>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TBinding<T1>>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "term",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTerm<T1>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTerm<T1>>>) (v2 -> hydra.decode.Phantoms.<T0, T1>tTerm(
                  a,
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.phantoms.TTerm<T1>, hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TBinding<T1>>>) (field_term -> hydra.util.Either.<hydra.errors.DecodingError, hydra.phantoms.TBinding<T1>>right((hydra.phantoms.TBinding<T1>) (new hydra.phantoms.TBinding<T1>(field_name, field_term)))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0, T1> hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTerm<T1>> tTerm(T0 a, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTerm<T1>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.phantoms.TTerm<T1>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTerm<T1>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTerm<T1>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.phantoms.TTerm<T1>>left(new hydra.errors.DecodingError("expected wrapped type"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTerm<T1>> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<hydra.core.Term, hydra.phantoms.TTerm<T1>>) (b -> (hydra.phantoms.TTerm<T1>) (new hydra.phantoms.TTerm(b))),
            hydra.decode.Core.term(
              cx,
              (wrappedTerm).value.body));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }

  static <T0, T1> hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTermDefinition<T1>> tTermDefinition(T0 a, hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTermDefinition<T1>>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.phantoms.TTermDefinition<T1>>left(new hydra.errors.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTermDefinition<T1>>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTermDefinition<T1>> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.phantoms.TTermDefinition<T1>>left(new hydra.errors.DecodingError("expected record"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTermDefinition<T1>> visit(hydra.core.Term.Record record) {
          hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> fieldMap = hydra.extract.Helpers.toFieldMap((record).value);
          return hydra.lib.eithers.Bind.apply(
            hydra.extract.Helpers.requireField(
              "name",
              (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.core.Name>>>) (p0 -> p1 -> hydra.decode.Core.name(
                p0,
                p1)),
              fieldMap,
              cx),
            (java.util.function.Function<hydra.core.Name, hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTermDefinition<T1>>>) (field_name -> hydra.lib.eithers.Bind.apply(
              hydra.extract.Helpers.requireField(
                "term",
                (java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTerm<T1>>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTerm<T1>>>) (v2 -> hydra.decode.Phantoms.<T0, T1>tTerm(
                  a,
                  v1,
                  v2))),
                fieldMap,
                cx),
              (java.util.function.Function<hydra.phantoms.TTerm<T1>, hydra.util.Either<hydra.errors.DecodingError, hydra.phantoms.TTermDefinition<T1>>>) (field_term -> hydra.util.Either.<hydra.errors.DecodingError, hydra.phantoms.TTermDefinition<T1>>right((hydra.phantoms.TTermDefinition<T1>) (new hydra.phantoms.TTermDefinition<T1>(field_name, field_term)))))));
        }
      })),
      hydra.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
