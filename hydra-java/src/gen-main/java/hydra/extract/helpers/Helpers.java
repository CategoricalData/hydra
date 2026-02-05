// Note: this is an automatically generated file. Do not edit.

package hydra.extract.helpers;

/**
 * Helper functions for decoding terms to domain types
 */
public interface Helpers {
  static <T0, T1> hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<T0, T1>> decodeEither(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> leftDecoder, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T1>>> rightDecoder, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<String, hydra.util.DecodingError>) (x -> new hydra.util.DecodingError((x))),
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> (x)),
        hydra.lexical.Lexical.stripAndDereferenceTermEither(
          (g),
          (term))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<T0, T1>>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<T0, T1>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<T0, T1>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<T0, T1>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.util.Either<T0, T1>>left(new hydra.util.DecodingError("expected either value"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<T0, T1>> visit(hydra.core.Term.Either e) {
          return hydra.lib.eithers.Either.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<T0, T1>>>) (lv -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<T0, hydra.util.Either<T0, T1>>) (x -> (hydra.util.Either<T0, T1>) ((hydra.util.Either<T0, T1>) (hydra.util.Either.<T0, T1>left((x))))),
              (((leftDecoder)).apply((g))).apply((lv)))),
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Either<T0, T1>>>) (rv -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<T1, hydra.util.Either<T0, T1>>) (x -> (hydra.util.Either<T0, T1>) ((hydra.util.Either<T0, T1>) (hydra.util.Either.<T0, T1>right((x))))),
              (((rightDecoder)).apply((g))).apply((rv)))),
            ((e)).value);
        }
      })));
  }
  
  static <T0> hydra.util.Either<hydra.util.DecodingError, java.util.List<T0>> decodeList(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> elemDecoder, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<String, hydra.util.DecodingError>) (x -> new hydra.util.DecodingError((x))),
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> (x)),
        hydra.lexical.Lexical.stripAndDereferenceTermEither(
          (g),
          (term))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.List<T0>>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, java.util.List<T0>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, java.util.List<T0>>) ((hydra.util.Either<hydra.util.DecodingError, java.util.List<T0>>) (hydra.util.Either.<hydra.util.DecodingError, java.util.List<T0>>left(new hydra.util.DecodingError("expected list"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, java.util.List<T0>> visit(hydra.core.Term.List els) {
          return hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>) (v1 -> (((elemDecoder)).apply((g))).apply((v1))),
            ((els)).value);
        }
      })));
  }
  
  static <T0, T1> hydra.util.Either<hydra.util.DecodingError, java.util.Map<T0, T1>> decodeMap(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> keyDecoder, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T1>>> valDecoder, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<String, hydra.util.DecodingError>) (x -> new hydra.util.DecodingError((x))),
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> (x)),
        hydra.lexical.Lexical.stripAndDereferenceTermEither(
          (g),
          (term))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Map<T0, T1>>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, java.util.Map<T0, T1>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, java.util.Map<T0, T1>>) ((hydra.util.Either<hydra.util.DecodingError, java.util.Map<T0, T1>>) (hydra.util.Either.<hydra.util.DecodingError, java.util.Map<T0, T1>>left(new hydra.util.DecodingError("expected map"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, java.util.Map<T0, T1>> visit(hydra.core.Term.Map m) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<T0, T1>>, java.util.Map<T0, T1>>) ((java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<T0, T1>>, java.util.Map<T0, T1>>) ((hydra.lib.maps.FromList::apply))),
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<T0, T1>>>) (kv -> hydra.lib.eithers.Bind.apply(
                (((keyDecoder)).apply((g))).apply(hydra.lib.pairs.First.apply((kv))),
                (java.util.function.Function<T0, hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<T0, T1>>>) (k -> hydra.lib.eithers.Map.apply(
                  (java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T0, T1>>) (v -> (hydra.util.Tuple.Tuple2<T0, T1>) ((hydra.util.Tuple.Tuple2<T0, T1>) (new hydra.util.Tuple.Tuple2<T0, T1>((k), (v))))),
                  (((valDecoder)).apply((g))).apply(hydra.lib.pairs.Second.apply((kv))))))),
              hydra.lib.maps.ToList.apply(((m)).value)));
        }
      })));
  }
  
  static <T0> hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<T0>> decodeMaybe(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> elemDecoder, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<String, hydra.util.DecodingError>) (x -> new hydra.util.DecodingError((x))),
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> (x)),
        hydra.lexical.Lexical.stripAndDereferenceTermEither(
          (g),
          (term))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<T0>>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<T0>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<T0>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<T0>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.util.Maybe<T0>>left(new hydra.util.DecodingError("expected optional value"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.util.Maybe<T0>> visit(hydra.core.Term.Maybe opt) {
          return hydra.lib.eithers.MapMaybe.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>) (v1 -> (((elemDecoder)).apply((g))).apply((v1))),
            ((opt)).value);
        }
      })));
  }
  
  static <T0, T1> hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<T0, T1>> decodePair(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> firstDecoder, java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T1>>> secondDecoder, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<String, hydra.util.DecodingError>) (x -> new hydra.util.DecodingError((x))),
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> (x)),
        hydra.lexical.Lexical.stripAndDereferenceTermEither(
          (g),
          (term))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<T0, T1>>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<T0, T1>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<T0, T1>>) ((hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<T0, T1>>) (hydra.util.Either.<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<T0, T1>>left(new hydra.util.DecodingError("expected pair"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<T0, T1>> visit(hydra.core.Term.Pair p) {
          return hydra.lib.eithers.Bind.apply(
            (((firstDecoder)).apply((g))).apply(hydra.lib.pairs.First.apply(((p)).value)),
            (java.util.function.Function<T0, hydra.util.Either<hydra.util.DecodingError, hydra.util.Tuple.Tuple2<T0, T1>>>) (f -> hydra.lib.eithers.Map.apply(
              (java.util.function.Function<T1, hydra.util.Tuple.Tuple2<T0, T1>>) (s -> (hydra.util.Tuple.Tuple2<T0, T1>) ((hydra.util.Tuple.Tuple2<T0, T1>) (new hydra.util.Tuple.Tuple2<T0, T1>((f), (s))))),
              (((secondDecoder)).apply((g))).apply(hydra.lib.pairs.Second.apply(((p)).value)))));
        }
      })));
  }
  
  static <T0> hydra.util.Either<hydra.util.DecodingError, java.util.Set<T0>> decodeSet(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> elemDecoder, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<String, hydra.util.DecodingError>) (x -> new hydra.util.DecodingError((x))),
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> (x)),
        hydra.lexical.Lexical.stripAndDereferenceTermEither(
          (g),
          (term))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.util.Set<T0>>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, java.util.Set<T0>> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, java.util.Set<T0>>) ((hydra.util.Either<hydra.util.DecodingError, java.util.Set<T0>>) (hydra.util.Either.<hydra.util.DecodingError, java.util.Set<T0>>left(new hydra.util.DecodingError("expected set"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, java.util.Set<T0>> visit(hydra.core.Term.Set s) {
          return hydra.lib.eithers.Map.apply(
            (java.util.function.Function<java.util.List<T0>, java.util.Set<T0>>) ((hydra.lib.sets.FromList::apply)),
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>) (v1 -> (((elemDecoder)).apply((g))).apply((v1))),
              hydra.lib.sets.ToList.apply(((s)).value)));
        }
      })));
  }
  
  static hydra.util.Either<hydra.util.DecodingError, java.lang.Void> decodeUnit(hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<String, hydra.util.DecodingError>) (x -> new hydra.util.DecodingError((x))),
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> (x)),
        hydra.lexical.Lexical.stripAndDereferenceTermEither(
          (g),
          (term))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, java.lang.Void>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, java.lang.Void> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, java.lang.Void>) ((hydra.util.Either<hydra.util.DecodingError, java.lang.Void>) (hydra.util.Either.<hydra.util.DecodingError, java.lang.Void>left(new hydra.util.DecodingError("expected a unit value"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, java.lang.Void> visit(hydra.core.Term.Unit ignored) {
          return (hydra.util.Either<hydra.util.DecodingError, java.lang.Void>) ((hydra.util.Either<hydra.util.DecodingError, java.lang.Void>) (hydra.util.Either.<hydra.util.DecodingError, java.lang.Void>right(null)));
        }
      })));
  }
  
  static <T0> hydra.util.Either<hydra.util.DecodingError, T0> decodeWrapped(java.util.function.Function<hydra.graph.Graph, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>> bodyDecoder, hydra.graph.Graph g, hydra.core.Term term) {
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<String, hydra.util.DecodingError>) (x -> new hydra.util.DecodingError((x))),
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> (x)),
        hydra.lexical.Lexical.stripAndDereferenceTermEither(
          (g),
          (term))),
      (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.util.DecodingError, T0>>) (stripped -> ((stripped)).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.util.DecodingError, T0> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.util.DecodingError, T0>) ((hydra.util.Either<hydra.util.DecodingError, T0>) (hydra.util.Either.<hydra.util.DecodingError, T0>left(new hydra.util.DecodingError("expected wrapped value"))));
        }
        
        @Override
        public hydra.util.Either<hydra.util.DecodingError, T0> visit(hydra.core.Term.Wrap wt) {
          return (((bodyDecoder)).apply((g))).apply((((wt)).value).body);
        }
      })));
  }
  
  static <T0, T1, T2> hydra.util.Either<hydra.util.DecodingError, T2> requireField(String fieldName, java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Either<hydra.util.DecodingError, T2>>> decoder, java.util.Map<hydra.core.Name, T1> fieldMap, T0 g) {
    return hydra.lib.maybes.Maybe.apply(
      (hydra.util.Either<hydra.util.DecodingError, T2>) ((hydra.util.Either<hydra.util.DecodingError, T2>) (hydra.util.Either.<hydra.util.DecodingError, T2>left(new hydra.util.DecodingError(hydra.lib.strings.Cat.apply(java.util.List.of(
        "missing field ",
        (fieldName),
        " in record")))))),
      (java.util.function.Function<T1, hydra.util.Either<hydra.util.DecodingError, T2>>) (fieldTerm -> (((decoder)).apply((g))).apply((fieldTerm))),
      hydra.lib.maps.Lookup.apply(
        new hydra.core.Name((fieldName)),
        (fieldMap)));
  }
  
  static java.util.Map<hydra.core.Name, hydra.core.Term> toFieldMap(hydra.core.Record record) {
    return hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.core.Field, hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>>) (f -> (hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) ((hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>) (new hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term>(((f)).name, ((f)).term)))),
      ((record)).fields));
  }
}
