// Note: this is an automatically generated file. Do not edit.

package hydra.decode.classes;

/**
 * Term decoders for hydra.classes
 */
public interface Classes {
  static hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass> typeClass(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass>>) (err -> hydra.util.Either.<hydra.error.DecodingError, hydra.classes.TypeClass>left(new hydra.error.DecodingError(err))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.error.DecodingError, hydra.classes.TypeClass>left(new hydra.error.DecodingError("expected union"));
        }
        
        @Override
        public hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = ((inj).value).field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<hydra.util.PersistentMap<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(hydra.util.ConsList.of(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass>>>(new hydra.core.Name("equality"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.classes.TypeClass>) (t -> new hydra.classes.TypeClass.Equality()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass>>>(new hydra.core.Name("ordering"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.classes.TypeClass>) (t -> new hydra.classes.TypeClass.Ordering()),
              hydra.extract.helpers.Helpers.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.error.DecodingError, hydra.classes.TypeClass>left(new hydra.error.DecodingError(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass>>, hydra.util.Either<hydra.error.DecodingError, hydra.classes.TypeClass>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        cx,
        raw));
  }
}
