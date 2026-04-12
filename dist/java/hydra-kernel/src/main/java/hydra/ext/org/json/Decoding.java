// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json;

/**
 * Decoding functions for JSON data
 */
public interface Decoding {
  static <T0> hydra.util.Either<String, java.util.List<T0>> decodeArray(java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, T0>> decodeElem, hydra.json.model.Value v1) {
    return (v1).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, java.util.List<T0>> otherwise(hydra.json.model.Value instance) {
        return hydra.util.Either.<String, java.util.List<T0>>left("expected an array");
      }

      @Override
      public hydra.util.Either<String, java.util.List<T0>> visit(hydra.json.model.Value.Array a) {
        return hydra.lib.eithers.MapList.apply(
          decodeElem,
          (a).value);
      }
    });
  }

  static hydra.util.Either<String, Boolean> decodeBoolean(hydra.json.model.Value v1) {
    return (v1).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, Boolean> otherwise(hydra.json.model.Value instance) {
        return hydra.util.Either.<String, Boolean>left("expected a boolean");
      }

      @Override
      public hydra.util.Either<String, Boolean> visit(hydra.json.model.Value.Boolean_ b) {
        return hydra.util.Either.<String, Boolean>right((b).value);
      }
    });
  }

  static <T0, T1> hydra.util.Either<String, T1> decodeField(java.util.function.Function<T0, hydra.util.Either<String, T1>> decodeValue, String name, java.util.Map<String, T0> m) {
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.org.json.Decoding.<T0, String, T1, String>decodeOptionalField(
        decodeValue,
        name,
        m),
      (java.util.function.Function<hydra.util.Maybe<T1>, hydra.util.Either<String, T1>>) (v1 -> hydra.lib.maybes.Maybe.applyLazy(
        () -> hydra.util.Either.<String, T1>left(hydra.lib.strings.Cat2.apply(
          "missing field: ",
          name)),
        (java.util.function.Function<T1, hydra.util.Either<String, T1>>) (f -> hydra.util.Either.<String, T1>right(f)),
        v1)));
  }

  static hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>> decodeObject(hydra.json.model.Value v1) {
    return (v1).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>> otherwise(hydra.json.model.Value instance) {
        return hydra.util.Either.<String, java.util.Map<String, hydra.json.model.Value>>left("expected an object");
      }

      @Override
      public hydra.util.Either<String, java.util.Map<String, hydra.json.model.Value>> visit(hydra.json.model.Value.Object_ o) {
        return hydra.util.Either.<String, java.util.Map<String, hydra.json.model.Value>>right((o).value);
      }
    });
  }

  static <T0, T1, T2, T3> hydra.util.Either<T1, hydra.util.Maybe<T2>> decodeOptionalField(java.util.function.Function<T0, hydra.util.Either<T1, T2>> decodeValue, T3 name, java.util.Map<T3, T0> m) {
    return hydra.lib.maybes.Maybe.applyLazy(
      () -> hydra.util.Either.<T1, hydra.util.Maybe<T2>>right((hydra.util.Maybe<T2>) (hydra.util.Maybe.<T2>nothing())),
      (java.util.function.Function<T0, hydra.util.Either<T1, hydra.util.Maybe<T2>>>) (v -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<T2, hydra.util.Maybe<T2>>) (x -> hydra.util.Maybe.just(x)),
        (decodeValue).apply(v))),
      hydra.lib.maps.Lookup.apply(
        name,
        m));
  }

  static hydra.util.Either<String, String> decodeString(hydra.json.model.Value v1) {
    return (v1).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, String> otherwise(hydra.json.model.Value instance) {
        return hydra.util.Either.<String, String>left("expected a string");
      }

      @Override
      public hydra.util.Either<String, String> visit(hydra.json.model.Value.String_ s) {
        return hydra.util.Either.<String, String>right((s).value);
      }
    });
  }
}
