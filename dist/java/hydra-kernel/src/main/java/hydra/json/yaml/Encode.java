// Note: this is an automatically generated file. Do not edit.

package hydra.json.yaml;

/**
 * JSON-to-YAML encoding. Converts JSON Values to YAML Nodes (always succeeds), and Hydra Terms to YAML Nodes via JSON.
 */
public interface Encode {
  static hydra.yaml.model.Node jsonToYaml(hydra.json.model.Value value) {
    return (value).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.yaml.model.Node visit(hydra.json.model.Value.Array arr) {
        return new hydra.yaml.model.Node.Sequence(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.json.model.Value, hydra.yaml.model.Node>) (v -> hydra.json.yaml.Encode.jsonToYaml(v)),
          (arr).value));
      }

      @Override
      public hydra.yaml.model.Node visit(hydra.json.model.Value.Boolean_ b) {
        return new hydra.yaml.model.Node.Scalar(new hydra.yaml.model.Scalar.Bool((b).value));
      }

      @Override
      public hydra.yaml.model.Node visit(hydra.json.model.Value.Null ignored) {
        return new hydra.yaml.model.Node.Scalar(new hydra.yaml.model.Scalar.Null());
      }

      @Override
      public hydra.yaml.model.Node visit(hydra.json.model.Value.Number_ n) {
        return new hydra.yaml.model.Node.Scalar(new hydra.yaml.model.Scalar.Decimal((n).value));
      }

      @Override
      public hydra.yaml.model.Node visit(hydra.json.model.Value.Object_ obj) {
        return new hydra.yaml.model.Node.Mapping(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<String, hydra.json.model.Value>, hydra.util.Pair<hydra.yaml.model.Node, hydra.yaml.model.Node>>) (kv -> (hydra.util.Pair<hydra.yaml.model.Node, hydra.yaml.model.Node>) ((hydra.util.Pair<hydra.yaml.model.Node, hydra.yaml.model.Node>) (new hydra.util.Pair<hydra.yaml.model.Node, hydra.yaml.model.Node>(new hydra.yaml.model.Node.Scalar(new hydra.yaml.model.Scalar.Str(hydra.lib.pairs.First.apply(kv))), hydra.json.yaml.Encode.jsonToYaml(hydra.lib.pairs.Second.apply(kv)))))),
          hydra.lib.maps.ToList.apply((obj).value))));
      }

      @Override
      public hydra.yaml.model.Node visit(hydra.json.model.Value.String_ s) {
        return new hydra.yaml.model.Node.Scalar(new hydra.yaml.model.Scalar.Str((s).value));
      }
    });
  }

  static hydra.util.Either<String, hydra.yaml.model.Node> toYaml(java.util.Map<hydra.core.Name, hydra.core.Type> types, hydra.core.Name tname, hydra.core.Type typ, hydra.core.Term term) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.json.model.Value, hydra.yaml.model.Node>) (v -> hydra.json.yaml.Encode.jsonToYaml(v)),
      hydra.json.Encode.toJson(
        types,
        tname,
        typ,
        term));
  }
}
