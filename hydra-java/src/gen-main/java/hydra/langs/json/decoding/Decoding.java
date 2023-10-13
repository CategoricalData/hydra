package hydra.langs.json.decoding;

/**
 * Decoding functions for JSON data
 */
public interface Decoding {
  static <S> hydra.compute.Flow<S, String> decodeString(hydra.json.Value v1) {
    return ((v1)).accept(new hydra.json.Value.PartialVisitor<hydra.compute.Flow<S, String>>() {
      @Override
      public hydra.compute.Flow<S, String> otherwise(hydra.json.Value instance) {
        return hydra.lib.flows.Fail.apply("expected a string");
      }
      
      @Override
      public hydra.compute.Flow<S, String> visit(hydra.json.Value.String_ instance) {
        return hydra.lib.flows.Pure.apply((instance.value));
      }
    });
  }
  
  static <S> hydra.compute.Flow<S, Double> decodeNumber(hydra.json.Value v1) {
    return ((v1)).accept(new hydra.json.Value.PartialVisitor<hydra.compute.Flow<S, Double>>() {
      @Override
      public hydra.compute.Flow<S, Double> otherwise(hydra.json.Value instance) {
        return hydra.lib.flows.Fail.apply("expected a number");
      }
      
      @Override
      public hydra.compute.Flow<S, Double> visit(hydra.json.Value.Number_ instance) {
        return hydra.lib.flows.Pure.apply((instance.value));
      }
    });
  }
  
  static <S> hydra.compute.Flow<S, Boolean> decodeBoolean(hydra.json.Value v1) {
    return ((v1)).accept(new hydra.json.Value.PartialVisitor<hydra.compute.Flow<S, Boolean>>() {
      @Override
      public hydra.compute.Flow<S, Boolean> otherwise(hydra.json.Value instance) {
        return hydra.lib.flows.Fail.apply("expected a boolean");
      }
      
      @Override
      public hydra.compute.Flow<S, Boolean> visit(hydra.json.Value.Boolean_ instance) {
        return hydra.lib.flows.Pure.apply((instance.value));
      }
    });
  }
  
  static <A, S> java.util.function.Function<hydra.json.Value, hydra.compute.Flow<S, java.util.List<A>>> decodeArray(java.util.function.Function<hydra.json.Value, hydra.compute.Flow<S, A>> decodeElem) {
    return (java.util.function.Function<hydra.json.Value, hydra.compute.Flow<S, java.util.List<A>>>) (v1 -> ((v1)).accept(new hydra.json.Value.PartialVisitor<hydra.compute.Flow<S, java.util.List<A>>>() {
      @Override
      public hydra.compute.Flow<S, java.util.List<A>> otherwise(hydra.json.Value instance) {
        return hydra.lib.flows.Fail.apply("expected an array");
      }
      
      @Override
      public hydra.compute.Flow<S, java.util.List<A>> visit(hydra.json.Value.Array instance) {
        return hydra.lib.flows.MapList.apply(
          (decodeElem),
          (instance.value));
      }
    }));
  }
  
  static <S> hydra.compute.Flow<S, java.util.Map<String, hydra.json.Value>> decodeObject(hydra.json.Value v1) {
    return ((v1)).accept(new hydra.json.Value.PartialVisitor<hydra.compute.Flow<S, java.util.Map<String, hydra.json.Value>>>() {
      @Override
      public hydra.compute.Flow<S, java.util.Map<String, hydra.json.Value>> otherwise(hydra.json.Value instance) {
        return hydra.lib.flows.Fail.apply("expected an object");
      }
      
      @Override
      public hydra.compute.Flow<S, java.util.Map<String, hydra.json.Value>> visit(hydra.json.Value.Object_ instance) {
        return hydra.lib.flows.Pure.apply((instance.value));
      }
    });
  }
  
  static <A, S> java.util.function.Function<String, java.util.function.Function<java.util.Map<String, hydra.json.Value>, hydra.compute.Flow<S, A>>> decodeField(java.util.function.Function<hydra.json.Value, hydra.compute.Flow<S, A>> decodeValue) {
    return (java.util.function.Function<String, java.util.function.Function<java.util.Map<String, hydra.json.Value>, hydra.compute.Flow<S, A>>>) (name -> (java.util.function.Function<java.util.Map<String, hydra.json.Value>, hydra.compute.Flow<S, A>>) (m -> ((hydra.lib.maps.Lookup.apply(
      (name),
      (m))).map((decodeValue))).orElse(hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "unexpected field: ",
      (name)))))));
  }
}