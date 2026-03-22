// Note: this is an automatically generated file. Do not edit.

package hydra.json.yaml;

/**
 * YAML-to-JSON decoding. Converts YAML Nodes to JSON Values (may fail for non-JSON YAML), and YAML Nodes to Hydra Terms via JSON.
 */
public interface Decode {
  static hydra.util.Either<String, hydra.json.model.Value> yamlToJson(hydra.ext.org.yaml.model.Node node) {
    return (node).accept(new hydra.ext.org.yaml.model.Node.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.ext.org.yaml.model.Node.Mapping m) {
        java.util.function.Function<hydra.util.Pair<hydra.ext.org.yaml.model.Node, hydra.ext.org.yaml.model.Node>, hydra.util.Either<String, hydra.util.Pair<String, hydra.json.model.Value>>> convertEntry = (java.util.function.Function<hydra.util.Pair<hydra.ext.org.yaml.model.Node, hydra.ext.org.yaml.model.Node>, hydra.util.Either<String, hydra.util.Pair<String, hydra.json.model.Value>>>) (kv -> {
          hydra.util.Lazy<hydra.ext.org.yaml.model.Node> keyNode = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kv));
          hydra.util.Lazy<hydra.util.Either<String, String>> keyResult = new hydra.util.Lazy<>(() -> keyNode.get().accept(new hydra.ext.org.yaml.model.Node.PartialVisitor<>() {
            @Override
            public hydra.util.Either<String, String> otherwise(hydra.ext.org.yaml.model.Node instance) {
              return hydra.util.Either.<String, String>left("non-scalar YAML mapping key");
            }

            @Override
            public hydra.util.Either<String, String> visit(hydra.ext.org.yaml.model.Node.Scalar s) {
              return (s).value.accept(new hydra.ext.org.yaml.model.Scalar.PartialVisitor<>() {
                @Override
                public hydra.util.Either<String, String> otherwise(hydra.ext.org.yaml.model.Scalar instance) {
                  return hydra.util.Either.<String, String>left("non-string YAML mapping key");
                }

                @Override
                public hydra.util.Either<String, String> visit(hydra.ext.org.yaml.model.Scalar.Str str) {
                  return hydra.util.Either.<String, String>right((str).value);
                }
              });
            }
          }));
          hydra.util.Lazy<hydra.ext.org.yaml.model.Node> valNode = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(kv));
          return hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, hydra.util.Either<String, hydra.util.Pair<String, hydra.json.model.Value>>>) (err -> hydra.util.Either.<String, hydra.util.Pair<String, hydra.json.model.Value>>left(err)),
            (java.util.function.Function<String, hydra.util.Either<String, hydra.util.Pair<String, hydra.json.model.Value>>>) (key -> {
              hydra.util.Either<String, hydra.json.model.Value> valResult = hydra.json.yaml.Decode.yamlToJson(valNode.get());
              return hydra.lib.eithers.Map.apply(
                (java.util.function.Function<hydra.json.model.Value, hydra.util.Pair<String, hydra.json.model.Value>>) (v -> (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>(key, v)))),
                valResult);
            }),
            keyResult.get());
        });
        hydra.util.Lazy<hydra.util.Either<String, hydra.util.ConsList<hydra.util.Pair<String, hydra.json.model.Value>>>> entries = new hydra.util.Lazy<>(() -> hydra.lib.eithers.MapList.apply(
          convertEntry,
          hydra.lib.maps.ToList.apply((m).value)));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.ConsList<hydra.util.Pair<String, hydra.json.model.Value>>, hydra.json.model.Value>) (es -> new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(es))),
          entries.get());
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.ext.org.yaml.model.Node.Scalar s) {
        return (s).value.accept(new hydra.ext.org.yaml.model.Scalar.PartialVisitor<>() {
          @Override
          public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.ext.org.yaml.model.Scalar.Bool b) {
            return hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.Boolean_((b).value));
          }

          @Override
          public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.ext.org.yaml.model.Scalar.Float_ f) {
            return hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.Number_((f).value));
          }

          @Override
          public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.ext.org.yaml.model.Scalar.Int i) {
            return hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToBigfloat.apply((i).value)));
          }

          @Override
          public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.ext.org.yaml.model.Scalar.Null ignored) {
            return hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.Null());
          }

          @Override
          public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.ext.org.yaml.model.Scalar.Str str) {
            return hydra.util.Either.<String, hydra.json.model.Value>right(new hydra.json.model.Value.String_((str).value));
          }
        });
      }

      @Override
      public hydra.util.Either<String, hydra.json.model.Value> visit(hydra.ext.org.yaml.model.Node.Sequence nodes) {
        hydra.util.Lazy<hydra.util.Either<String, hydra.util.ConsList<hydra.json.model.Value>>> results = new hydra.util.Lazy<>(() -> hydra.lib.eithers.MapList.apply(
          (java.util.function.Function<hydra.ext.org.yaml.model.Node, hydra.util.Either<String, hydra.json.model.Value>>) (n -> hydra.json.yaml.Decode.yamlToJson(n)),
          (nodes).value));
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.ConsList<hydra.json.model.Value>, hydra.json.model.Value>) (vs -> new hydra.json.model.Value.Array(vs)),
          results.get());
      }
    });
  }

  static hydra.util.Either<String, hydra.core.Term> fromYaml(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Type> types, hydra.core.Name tname, hydra.core.Type typ, hydra.ext.org.yaml.model.Node node) {
    hydra.util.Either<String, hydra.json.model.Value> jsonResult = hydra.json.yaml.Decode.yamlToJson(node);
    return hydra.lib.eithers.Either.apply(
      (java.util.function.Function<String, hydra.util.Either<String, hydra.core.Term>>) (err -> hydra.util.Either.<String, hydra.core.Term>left(err)),
      (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<String, hydra.core.Term>>) (json -> hydra.json.Decode.fromJson(
        types,
        tname,
        typ,
        json)),
      jsonResult);
  }
}
