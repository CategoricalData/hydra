// Note: this is an automatically generated file. Do not edit.

package hydra.decode.classes;

/**
 * Term decoders for hydra.classes
 */
public interface Classes {
  static hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass> typeClass(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>) ((hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>) (hydra.util.Either.<hydra.util.DecodingError, hydra.classes.TypeClass>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>) ((hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>) (hydra.util.Either.<hydra.util.DecodingError, hydra.classes.TypeClass>left(new hydra.util.DecodingError("expected union of type hydra.classes.TypeClass"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (((inj)).value).field;
          hydra.core.Name fname = ((field)).name;
          hydra.core.Term fterm = ((field)).term;
          hydra.core.Name tname = (((inj)).value).typeName;
          java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>>> variantMap = hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>>>(new hydra.core.Name("equality"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.classes.TypeClass>) (t -> new hydra.classes.TypeClass.Equality((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>>>(new hydra.core.Name("ordering"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.classes.TypeClass>) (t -> new hydra.classes.TypeClass.Ordering((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>) ((hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>) (hydra.util.Either.<hydra.util.DecodingError, hydra.classes.TypeClass>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              ((fname)).value,
              " in union type ",
              ((tname)).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>>, hydra.util.Either<hydra.util.DecodingError, hydra.classes.TypeClass>>) (f -> ((f)).apply((fterm))),
            hydra.lib.maps.Lookup.apply(
              (fname),
              (variantMap)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
}
