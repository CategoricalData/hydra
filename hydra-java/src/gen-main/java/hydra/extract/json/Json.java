// Note: this is an automatically generated file. Do not edit.

package hydra.extract.json;

/**
 * Utilities for extracting values from JSON objects
 */
public interface Json {
  static <T0> hydra.compute.Flow<T0, java.util.List<hydra.json.model.Value>> expectArray(hydra.json.model.Value value) {
    return ((value)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.json.model.Value>> otherwise(hydra.json.model.Value instance) {
        return hydra.monads.Monads.unexpected(
          "JSON array",
          hydra.extract.json.Json.showValue((value)));
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<hydra.json.model.Value>> visit(hydra.json.model.Value.Array els) {
        return hydra.lib.flows.Pure.apply(((els)).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, java.math.BigDecimal> expectNumber(hydra.json.model.Value value) {
    return ((value)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, java.math.BigDecimal> otherwise(hydra.json.model.Value instance) {
        return hydra.monads.Monads.<T0, java.math.BigDecimal>unexpected(
          "JSON number",
          hydra.extract.json.Json.showValue((value)));
      }
      
      @Override
      public hydra.compute.Flow<T0, java.math.BigDecimal> visit(hydra.json.model.Value.Number_ d) {
        return hydra.lib.flows.Pure.apply(((d)).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, java.util.Map<String, hydra.json.model.Value>> expectObject(hydra.json.model.Value value) {
    return ((value)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, java.util.Map<String, hydra.json.model.Value>> otherwise(hydra.json.model.Value instance) {
        return hydra.monads.Monads.unexpected(
          "JSON object",
          hydra.extract.json.Json.showValue((value)));
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.Map<String, hydra.json.model.Value>> visit(hydra.json.model.Value.Object_ m) {
        return hydra.lib.flows.Pure.apply(((m)).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, String> expectString(hydra.json.model.Value value) {
    return ((value)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, String> otherwise(hydra.json.model.Value instance) {
        return hydra.monads.Monads.<T0, String>unexpected(
          "JSON string",
          hydra.extract.json.Json.showValue((value)));
      }
      
      @Override
      public hydra.compute.Flow<T0, String> visit(hydra.json.model.Value.String_ s) {
        return hydra.lib.flows.Pure.apply(((s)).value);
      }
    });
  }
  
  static <T0, T1> hydra.util.Maybe<T1> opt(T0 fname, java.util.Map<T0, T1> m) {
    return hydra.lib.maps.Lookup.apply(
      (fname),
      (m));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.util.Maybe<java.util.List<hydra.json.model.Value>>> optArray(T0 fname, java.util.Map<T0, hydra.json.model.Value> m) {
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.flows.Pure.apply((hydra.util.Maybe<java.util.List<hydra.json.model.Value>>) (hydra.util.Maybe.<java.util.List<hydra.json.model.Value>>nothing())),
      (java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.util.Maybe<java.util.List<hydra.json.model.Value>>>>) (a -> hydra.lib.flows.Map.apply(
        (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.util.Maybe<java.util.List<hydra.json.model.Value>>>) ((hydra.lib.maybes.Pure::apply)),
        hydra.extract.json.Json.<T1>expectArray((a)))),
      hydra.extract.json.Json.opt(
        (fname),
        (m)));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.util.Maybe<String>> optString(T0 fname, java.util.Map<T0, hydra.json.model.Value> m) {
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.flows.Pure.apply((hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing())),
      (java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.util.Maybe<String>>>) (s -> hydra.lib.flows.Map.apply(
        (java.util.function.Function<String, hydra.util.Maybe<String>>) ((hydra.lib.maybes.Pure::apply)),
        hydra.extract.json.Json.<T1>expectString((s)))),
      hydra.extract.json.Json.opt(
        (fname),
        (m)));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T2, T1> require(T0 fname, java.util.Map<T0, T1> m) {
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
        "required attribute ",
        hydra.extract.json.Json.<T0>showValue((fname)),
        " not found"))),
      (java.util.function.Function<T1, hydra.compute.Flow<T2, T1>>) (value -> hydra.lib.flows.Pure.apply((value))),
      hydra.lib.maps.Lookup.apply(
        (fname),
        (m)));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, java.util.List<hydra.json.model.Value>> requireArray(T0 fname, java.util.Map<T0, hydra.json.model.Value> m) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.json.Json.require(
        (fname),
        (m)),
      p0 -> hydra.extract.json.Json.<T1>expectArray((p0)));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, java.math.BigDecimal> requireNumber(T0 fname, java.util.Map<T0, hydra.json.model.Value> m) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.json.Json.require(
        (fname),
        (m)),
      p0 -> hydra.extract.json.Json.<T1>expectNumber((p0)));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, String> requireString(T0 fname, java.util.Map<T0, hydra.json.model.Value> m) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.json.Json.require(
        (fname),
        (m)),
      p0 -> hydra.extract.json.Json.<T1>expectString((p0)));
  }
  
  static <T0> String showValue(T0 value) {
    return "TODO: implement showValue";
  }
}
