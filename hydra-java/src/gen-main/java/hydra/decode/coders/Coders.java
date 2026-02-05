// Note: this is an automatically generated file. Do not edit.

package hydra.decode.coders;

/**
 * Term decoders for hydra.coders
 */
public interface Coders {
  static hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection> coderDirection(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>) ((hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>) (hydra.util.Either.<hydra.util.DecodingError, hydra.coders.CoderDirection>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>) ((hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>) (hydra.util.Either.<hydra.util.DecodingError, hydra.coders.CoderDirection>left(new hydra.util.DecodingError("expected union of type hydra.coders.CoderDirection"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (((inj)).value).field;
          hydra.core.Name fname = ((field)).name;
          hydra.core.Term fterm = ((field)).term;
          hydra.core.Name tname = (((inj)).value).typeName;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>>>(new hydra.core.Name("encode"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.coders.CoderDirection>) (t -> new hydra.coders.CoderDirection.Encode()),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>>>(new hydra.core.Name("decode"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.coders.CoderDirection>) (t -> new hydra.coders.CoderDirection.Decode()),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>) ((hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>) (hydra.util.Either.<hydra.util.DecodingError, hydra.coders.CoderDirection>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              ((fname)).value,
              " in union type ",
              ((tname)).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>>, hydra.util.Either<hydra.util.DecodingError, hydra.coders.CoderDirection>>) (f -> ((f)).apply((fterm))),
            hydra.lib.maps.Lookup.apply(
              (fname),
              variantMap.get()));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.coders.LanguageName> languageName(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.coders.LanguageName>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.coders.LanguageName>) ((hydra.util.Either<hydra.util.DecodingError, hydra.coders.LanguageName>) (hydra.util.Either.<hydra.util.DecodingError, hydra.coders.LanguageName>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.LanguageName>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.coders.LanguageName> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.coders.LanguageName>) ((hydra.util.Either<hydra.util.DecodingError, hydra.coders.LanguageName>) (hydra.util.Either.<hydra.util.DecodingError, hydra.coders.LanguageName>left(new hydra.util.DecodingError("expected wrapped type hydra.coders.LanguageName"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.coders.LanguageName> visit(hydra.core.Term.Wrap wrappedTerm) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<String, hydra.coders.LanguageName>) (b -> new hydra.coders.LanguageName((b))),
            hydra.lib.eithers.Either.apply(
              (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, String>>) (err -> (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError((err)))))),
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, String>>) (stripped2 -> ((stripped2)).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Term instance) {
                  return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected literal"))));
                }
                
                @Override
                public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Term.Literal v) {
                  return (((v)).value).accept(new hydra.core.Literal.PartialVisitor<>() {
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> otherwise(hydra.core.Literal instance) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>left(new hydra.util.DecodingError("expected string literal"))));
                    }
                    
                    @Override
                    public hydra.util.Either<hydra.util.DecodingError, String> visit(hydra.core.Literal.String_ s) {
                      return (hydra.util.Either<hydra.util.DecodingError, String>) ((hydra.util.Either<hydra.util.DecodingError, String>) (hydra.util.Either.<hydra.util.DecodingError, String>right(((s)).value)));
                    }
                  });
                }
              })),
              hydra.lexical.Lexical.stripAndDereferenceTermEither(
                (cx),
                (((wrappedTerm)).value).body)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder> traversalOrder(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>) ((hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>) (hydra.util.Either.<hydra.util.DecodingError, hydra.coders.TraversalOrder>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>) ((hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>) (hydra.util.Either.<hydra.util.DecodingError, hydra.coders.TraversalOrder>left(new hydra.util.DecodingError("expected union of type hydra.coders.TraversalOrder"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (((inj)).value).field;
          hydra.core.Name fname = ((field)).name;
          hydra.core.Term fterm = ((field)).term;
          hydra.core.Name tname = (((inj)).value).typeName;
          hydra.util.Lazy<java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>>>> variantMap = new hydra.util.Lazy<>(() -> hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>>>(new hydra.core.Name("pre"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.coders.TraversalOrder>) (t -> new hydra.coders.TraversalOrder.Pre()),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>>>(new hydra.core.Name("post"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<java.lang.Void, hydra.coders.TraversalOrder>) (t -> new hydra.coders.TraversalOrder.Post()),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>) ((hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>) (hydra.util.Either.<hydra.util.DecodingError, hydra.coders.TraversalOrder>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              ((fname)).value,
              " in union type ",
              ((tname)).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>>, hydra.util.Either<hydra.util.DecodingError, hydra.coders.TraversalOrder>>) (f -> ((f)).apply((fterm))),
            hydra.lib.maps.Lookup.apply(
              (fname),
              variantMap.get()));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
}
