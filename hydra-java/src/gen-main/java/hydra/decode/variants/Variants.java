// Note: this is an automatically generated file. Do not edit.

package hydra.decode.variants;

/**
 * Term decoders for hydra.variants
 */
public interface Variants {
  static hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant> eliminationVariant(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.variants.EliminationVariant>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.variants.EliminationVariant>left(new hydra.util.DecodingError("expected union of type hydra.variants.EliminationVariant"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (((inj)).value).field;
          hydra.core.Name fname = ((field)).name;
          hydra.core.Term fterm = ((field)).term;
          hydra.core.Name tname = (((inj)).value).typeName;
          java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>> variantMap = hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>>(new hydra.core.Name("record"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.EliminationVariant>) (t -> new hydra.variants.EliminationVariant.Record((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>>(new hydra.core.Name("union"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.EliminationVariant>) (t -> new hydra.variants.EliminationVariant.Union((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>>(new hydra.core.Name("wrap"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.EliminationVariant>) (t -> new hydra.variants.EliminationVariant.Wrap((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.variants.EliminationVariant>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              ((fname)).value,
              " in union type ",
              ((tname)).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>, hydra.util.Either<hydra.util.DecodingError, hydra.variants.EliminationVariant>>) (f -> ((f)).apply((fterm))),
            hydra.lib.maps.Lookup.apply(
              (fname),
              (variantMap)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant> functionVariant(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.variants.FunctionVariant>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.variants.FunctionVariant>left(new hydra.util.DecodingError("expected union of type hydra.variants.FunctionVariant"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (((inj)).value).field;
          hydra.core.Name fname = ((field)).name;
          hydra.core.Term fterm = ((field)).term;
          hydra.core.Name tname = (((inj)).value).typeName;
          java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>> variantMap = hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>>(new hydra.core.Name("elimination"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.FunctionVariant>) (t -> new hydra.variants.FunctionVariant.Elimination((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>>(new hydra.core.Name("lambda"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.FunctionVariant>) (t -> new hydra.variants.FunctionVariant.Lambda((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>>(new hydra.core.Name("primitive"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.FunctionVariant>) (t -> new hydra.variants.FunctionVariant.Primitive((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.variants.FunctionVariant>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              ((fname)).value,
              " in union type ",
              ((tname)).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>, hydra.util.Either<hydra.util.DecodingError, hydra.variants.FunctionVariant>>) (f -> ((f)).apply((fterm))),
            hydra.lib.maps.Lookup.apply(
              (fname),
              (variantMap)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant> literalVariant(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.variants.LiteralVariant>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.variants.LiteralVariant>left(new hydra.util.DecodingError("expected union of type hydra.variants.LiteralVariant"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (((inj)).value).field;
          hydra.core.Name fname = ((field)).name;
          hydra.core.Term fterm = ((field)).term;
          hydra.core.Name tname = (((inj)).value).typeName;
          java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>> variantMap = hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>>(new hydra.core.Name("binary"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.LiteralVariant>) (t -> new hydra.variants.LiteralVariant.Binary((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>>(new hydra.core.Name("boolean"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.LiteralVariant>) (t -> new hydra.variants.LiteralVariant.Boolean_((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>>(new hydra.core.Name("float"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.LiteralVariant>) (t -> new hydra.variants.LiteralVariant.Float_((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>>(new hydra.core.Name("integer"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.LiteralVariant>) (t -> new hydra.variants.LiteralVariant.Integer_((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>>(new hydra.core.Name("string"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.LiteralVariant>) (t -> new hydra.variants.LiteralVariant.String_((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.variants.LiteralVariant>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              ((fname)).value,
              " in union type ",
              ((tname)).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>, hydra.util.Either<hydra.util.DecodingError, hydra.variants.LiteralVariant>>) (f -> ((f)).apply((fterm))),
            hydra.lib.maps.Lookup.apply(
              (fname),
              (variantMap)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant> termVariant(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.variants.TermVariant>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.variants.TermVariant>left(new hydra.util.DecodingError("expected union of type hydra.variants.TermVariant"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (((inj)).value).field;
          hydra.core.Name fname = ((field)).name;
          hydra.core.Term fterm = ((field)).term;
          hydra.core.Name tname = (((inj)).value).typeName;
          java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>> variantMap = hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("annotated"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.Annotated((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("application"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.Application((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("either"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.Either((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("function"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.Function((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("let"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.Let((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("list"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.List((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("literal"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.Literal((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("map"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.Map((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("maybe"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.Maybe((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("pair"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.Pair((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("record"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.Record((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("set"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.Set((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("typeApplication"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.TypeApplication((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("typeLambda"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.TypeLambda((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("union"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.Union((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("unit"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.Unit((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("variable"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.Variable((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>>(new hydra.core.Name("wrap"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TermVariant>) (t -> new hydra.variants.TermVariant.Wrap((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.variants.TermVariant>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              ((fname)).value,
              " in union type ",
              ((tname)).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TermVariant>>) (f -> ((f)).apply((fterm))),
            hydra.lib.maps.Lookup.apply(
              (fname),
              (variantMap)));
        }
      })),
      hydra.lexical.Lexical.stripAndDereferenceTermEither(
        (cx),
        (raw)));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant> typeVariant(hydra.graph.Graph cx, hydra.core.Term raw) {
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (err -> (hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.variants.TypeVariant>left(new hydra.util.DecodingError((err)))))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.variants.TypeVariant>left(new hydra.util.DecodingError("expected union of type hydra.variants.TypeVariant"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant> visit(hydra.core.Term.Union inj) {
          hydra.core.Field field = (((inj)).value).field;
          hydra.core.Name fname = ((field)).name;
          hydra.core.Term fterm = ((field)).term;
          hydra.core.Name tname = (((inj)).value).typeName;
          java.util.Map<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>> variantMap = hydra.lib.maps.FromList.apply(java.util.List.of(
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("annotated"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.Annotated((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("application"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.Application((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("either"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.Either((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("forall"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.Forall((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("function"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.Function((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("list"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.List((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("literal"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.Literal((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("map"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.Map((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("maybe"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.Maybe((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("pair"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.Pair((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("record"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.Record((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("set"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.Set((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("union"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.Union((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("unit"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.Unit((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("variable"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.Variable((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input))))))),
            (hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>>(new hydra.core.Name("wrap"), (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (input -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<Boolean, hydra.variants.TypeVariant>) (t -> new hydra.variants.TypeVariant.Wrap((t))),
              hydra.extract.helpers.Helpers.decodeUnit(
                (cx),
                (input)))))))));
          return hydra.lib.maybes.Maybe.apply(
            (hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>) ((hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>) (hydra.util.Either.<hydra.util.DecodingError, hydra.variants.TypeVariant>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
              "no such field ",
              ((fname)).value,
              " in union type ",
              ((tname)).value)))))),
            (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>, hydra.util.Either<hydra.util.DecodingError, hydra.variants.TypeVariant>>) (f -> ((f)).apply((fterm))),
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
