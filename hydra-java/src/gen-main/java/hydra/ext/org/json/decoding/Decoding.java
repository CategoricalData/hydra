// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.decoding;

/**
 * Decoding functions for JSON data
 */
public interface Decoding {
  static <T0, T1> hydra.compute.Flow<T0, java.util.List<T1>> decodeArray(java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T0, T1>> decodeElem, hydra.json.model.Value v1) {
    return ((v1)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, java.util.List<T1>> otherwise(hydra.json.model.Value instance) {
        return hydra.lib.flows.Fail.apply("expected an array");
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.List<T1>> visit(hydra.json.model.Value.Array a) {
        return hydra.lib.flows.MapList.apply(
          (decodeElem),
          ((a)).value);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, Boolean> decodeBoolean(hydra.json.model.Value v1) {
    return ((v1)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, Boolean> otherwise(hydra.json.model.Value instance) {
        return hydra.lib.flows.Fail.apply("expected a boolean");
      }
      
      @Override
      public hydra.compute.Flow<T0, Boolean> visit(hydra.json.model.Value.Boolean_ b) {
        return hydra.lib.flows.Pure.apply(((b)).value);
      }
    });
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T1, T2> decodeField(java.util.function.Function<T0, hydra.compute.Flow<T1, T2>> decodeValue, String name, java.util.Map<String, T0> m) {
    return hydra.lib.flows.Bind.apply(
      hydra.ext.org.json.decoding.Decoding.<T0, T1, T2, String>decodeOptionalField(
        (decodeValue),
        (name),
        (m)),
      (java.util.function.Function<hydra.util.Maybe<T2>, hydra.compute.Flow<T1, T2>>) (v1 -> hydra.lib.maybes.Maybe.apply(
        hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat2.apply(
          "missing field: ",
          (name))),
        (java.util.function.Function<T2, hydra.compute.Flow<T1, T2>>) (f -> hydra.lib.flows.Pure.apply((f))),
        (v1))));
  }
  
  static <T0> hydra.compute.Flow<T0, java.util.Map<String, hydra.json.model.Value>> decodeObject(hydra.json.model.Value v1) {
    return ((v1)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, java.util.Map<String, hydra.json.model.Value>> otherwise(hydra.json.model.Value instance) {
        return hydra.lib.flows.Fail.apply("expected an object");
      }
      
      @Override
      public hydra.compute.Flow<T0, java.util.Map<String, hydra.json.model.Value>> visit(hydra.json.model.Value.Object_ o) {
        return hydra.lib.flows.Pure.apply(((o)).value);
      }
    });
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T1, hydra.util.Maybe<T2>> decodeOptionalField(java.util.function.Function<T0, hydra.compute.Flow<T1, T2>> decodeValue, T3 name, java.util.Map<T3, T0> m) {
    return hydra.lib.maybes.Maybe.apply(
      hydra.lib.flows.Pure.apply((hydra.util.Maybe<T2>) (hydra.util.Maybe.<T2>nothing())),
      (java.util.function.Function<T0, hydra.compute.Flow<T1, hydra.util.Maybe<T2>>>) (v -> hydra.lib.flows.Map.apply(
        (java.util.function.Function<T2, hydra.util.Maybe<T2>>) (x -> hydra.util.Maybe.just((x))),
        ((decodeValue)).apply((v)))),
      hydra.lib.maps.Lookup.apply(
        (name),
        (m)));
  }
  
  static <T0> hydra.compute.Flow<T0, String> decodeString(hydra.json.model.Value v1) {
    return ((v1)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, String> otherwise(hydra.json.model.Value instance) {
        return hydra.lib.flows.Fail.apply("expected a string");
      }
      
      @Override
      public hydra.compute.Flow<T0, String> visit(hydra.json.model.Value.String_ s) {
        return hydra.lib.flows.Pure.apply(((s)).value);
      }
    });
  }
}
