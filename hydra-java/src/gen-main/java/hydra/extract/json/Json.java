// Note: this is an automatically generated file. Do not edit.

package hydra.extract.json;

/**
 * Utilities for extracting values from JSON objects
 */
public interface Json {
  static hydra.util.Either<String, hydra.util.ConsList<hydra.json.model.Value>> expectArray(hydra.json.model.Value value) {
    return (value).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.util.ConsList<hydra.json.model.Value>> otherwise(hydra.json.model.Value instance) {
        return hydra.util.Either.<String, hydra.util.ConsList<hydra.json.model.Value>>left(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            "expected ",
            "JSON array"),
          hydra.lib.strings.Cat2.apply(
            " but found ",
            hydra.extract.json.Json.showValue(value))));
      }
      
      @Override
      public hydra.util.Either<String, hydra.util.ConsList<hydra.json.model.Value>> visit(hydra.json.model.Value.Array els) {
        return hydra.util.Either.<String, hydra.util.ConsList<hydra.json.model.Value>>right((els).value);
      }
    });
  }
  
  static hydra.util.Either<String, java.math.BigDecimal> expectNumber(hydra.json.model.Value value) {
    return (value).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, java.math.BigDecimal> otherwise(hydra.json.model.Value instance) {
        return hydra.util.Either.<String, java.math.BigDecimal>left(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            "expected ",
            "JSON number"),
          hydra.lib.strings.Cat2.apply(
            " but found ",
            hydra.extract.json.Json.showValue(value))));
      }
      
      @Override
      public hydra.util.Either<String, java.math.BigDecimal> visit(hydra.json.model.Value.Number_ d) {
        return hydra.util.Either.<String, java.math.BigDecimal>right((d).value);
      }
    });
  }
  
  static hydra.util.Either<String, hydra.util.PersistentMap<String, hydra.json.model.Value>> expectObject(hydra.json.model.Value value) {
    return (value).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.util.PersistentMap<String, hydra.json.model.Value>> otherwise(hydra.json.model.Value instance) {
        return hydra.util.Either.<String, hydra.util.PersistentMap<String, hydra.json.model.Value>>left(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            "expected ",
            "JSON object"),
          hydra.lib.strings.Cat2.apply(
            " but found ",
            hydra.extract.json.Json.showValue(value))));
      }
      
      @Override
      public hydra.util.Either<String, hydra.util.PersistentMap<String, hydra.json.model.Value>> visit(hydra.json.model.Value.Object_ m) {
        return hydra.util.Either.<String, hydra.util.PersistentMap<String, hydra.json.model.Value>>right((m).value);
      }
    });
  }
  
  static hydra.util.Either<String, String> expectString(hydra.json.model.Value value) {
    return (value).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, String> otherwise(hydra.json.model.Value instance) {
        return hydra.util.Either.<String, String>left(hydra.lib.strings.Cat2.apply(
          hydra.lib.strings.Cat2.apply(
            "expected ",
            "JSON string"),
          hydra.lib.strings.Cat2.apply(
            " but found ",
            hydra.extract.json.Json.showValue(value))));
      }
      
      @Override
      public hydra.util.Either<String, String> visit(hydra.json.model.Value.String_ s) {
        return hydra.util.Either.<String, String>right((s).value);
      }
    });
  }
  
  static <T0, T1> hydra.util.Maybe<T1> opt(T0 fname, hydra.util.PersistentMap<T0, T1> m) {
    return hydra.lib.maps.Lookup.apply(
      fname,
      m);
  }
  
  static <T0> hydra.util.Either<String, hydra.util.Maybe<hydra.util.ConsList<hydra.json.model.Value>>> optArray(T0 fname, hydra.util.PersistentMap<T0, hydra.json.model.Value> m) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<String, hydra.util.Maybe<hydra.util.ConsList<hydra.json.model.Value>>>right((hydra.util.Maybe<hydra.util.ConsList<hydra.json.model.Value>>) (hydra.util.Maybe.<hydra.util.ConsList<hydra.json.model.Value>>nothing())),
      (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.util.Maybe<hydra.util.ConsList<hydra.json.model.Value>>>>) (a -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<hydra.util.ConsList<hydra.json.model.Value>, hydra.util.Maybe<hydra.util.ConsList<hydra.json.model.Value>>>) (x -> hydra.util.Maybe.just(x)),
        hydra.extract.json.Json.expectArray(a))),
      hydra.extract.json.Json.opt(
        fname,
        m));
  }
  
  static <T0> hydra.util.Either<String, hydra.util.Maybe<String>> optString(T0 fname, hydra.util.PersistentMap<T0, hydra.json.model.Value> m) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<String, hydra.util.Maybe<String>>right((hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
      (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.util.Maybe<String>>>) (s -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<String, hydra.util.Maybe<String>>) (x -> hydra.util.Maybe.just(x)),
        hydra.extract.json.Json.expectString(s))),
      hydra.extract.json.Json.opt(
        fname,
        m));
  }
  
  static <T0, T1> hydra.util.Either<String, T1> require(T0 fname, hydra.util.PersistentMap<T0, T1> m) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<String, T1>left(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
        "required attribute ",
        hydra.extract.json.Json.<T0>showValue(fname),
        " not found"))),
      (java.util.function.Function<T1, hydra.util.Either<String, T1>>) (value -> hydra.util.Either.<String, T1>right(value)),
      hydra.lib.maps.Lookup.apply(
        fname,
        m));
  }
  
  static <T0> hydra.util.Either<String, hydra.util.ConsList<hydra.json.model.Value>> requireArray(T0 fname, hydra.util.PersistentMap<T0, hydra.json.model.Value> m) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.json.Json.require(
        fname,
        m),
      hydra.extract.json.Json::expectArray);
  }
  
  static <T0> hydra.util.Either<String, java.math.BigDecimal> requireNumber(T0 fname, hydra.util.PersistentMap<T0, hydra.json.model.Value> m) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.json.Json.require(
        fname,
        m),
      hydra.extract.json.Json::expectNumber);
  }
  
  static <T0> hydra.util.Either<String, String> requireString(T0 fname, hydra.util.PersistentMap<T0, hydra.json.model.Value> m) {
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.json.Json.require(
        fname,
        m),
      hydra.extract.json.Json::expectString);
  }
  
  static <T0> String showValue(T0 value) {
    return "TODO: implement showValue";
  }
}
