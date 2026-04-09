// Note: this is an automatically generated file. Do not edit.

package hydra.decode;

/**
 * Term decoders for hydra.classes
 */
public interface Classes {
  static hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass> typeClass(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<hydra.errors.DecodingError, hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass>>) (err -> hydra.util.Either.<hydra.errors.DecodingError, hydra.classes.TypeClass>left(err)),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass>>) (stripped -> (stripped).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass> otherwise(hydra.core.Term instance) {
          return hydra.util.Either.<hydra.errors.DecodingError, hydra.classes.TypeClass>left(new hydra.errors.DecodingError("expected union"));
        }

        @Override
        public hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (inj).value.field;
          hydra.core.Name fname = (field).name;
          hydra.core.Term fterm = (field).term;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.Arrays.asList(
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass>>>(new hydra.core.Name("equality"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.classes.TypeClass>) (t -> new hydra.classes.TypeClass.Equality()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))),
            (hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass>>>) ((hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass>>>) (new hydra.util.Pair<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass>>>(new hydra.core.Name("ordering"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.classes.TypeClass>) (t -> new hydra.classes.TypeClass.Ordering()),
              hydra.extract.Core.decodeUnit(
                cx,
                input)))))))));
          return hydra.lib.maybes.Maybe.applyLazy(
            () -> hydra.util.Either.<hydra.errors.DecodingError, hydra.classes.TypeClass>left(new hydra.errors.DecodingError(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
              "no such field ",
              (fname).value,
              " in union")))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass>>, hydra.util.Either<hydra.errors.DecodingError, hydra.classes.TypeClass>>) (f -> (f).apply(fterm)),
            hydra.lib.maps.Lookup.apply(
              fname,
              variantMap.get()));
        }
      })),
      hydra.extract.Core.stripWithDecodingError(
        cx,
        raw));
  }
}
