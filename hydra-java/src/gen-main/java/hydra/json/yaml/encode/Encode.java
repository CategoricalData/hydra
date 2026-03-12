// Note: this is an automatically generated file. Do not edit.

package hydra.json.yaml.encode;

/**
 * JSON-to-YAML encoding. Converts JSON Values to YAML Nodes (always succeeds), and Hydra Terms to YAML Nodes via JSON.
 */
public interface Encode {
  static hydra.ext.org.yaml.model.Node jsonToYaml(hydra.json.model.Value value) {
    return (value).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.ext.org.yaml.model.Node visit(hydra.json.model.Value.Array arr) {
        return new hydra.ext.org.yaml.model.Node.Sequence(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.json.model.Value, hydra.ext.org.yaml.model.Node>) (v -> hydra.json.yaml.encode.Encode.jsonToYaml(v)),
          (arr).value));
      }
      
      @Override
      public hydra.ext.org.yaml.model.Node visit(hydra.json.model.Value.Boolean_ b) {
        return new hydra.ext.org.yaml.model.Node.Scalar(new hydra.ext.org.yaml.model.Scalar.Bool((b).value));
      }
      
      @Override
      public hydra.ext.org.yaml.model.Node visit(hydra.json.model.Value.Null ignored) {
        return new hydra.ext.org.yaml.model.Node.Scalar(new hydra.ext.org.yaml.model.Scalar.Null());
      }
      
      @Override
      public hydra.ext.org.yaml.model.Node visit(hydra.json.model.Value.Number_ n) {
        return new hydra.ext.org.yaml.model.Node.Scalar(new hydra.ext.org.yaml.model.Scalar.Float_((n).value));
      }
      
      @Override
      public hydra.ext.org.yaml.model.Node visit(hydra.json.model.Value.Object_ obj) {
        return new hydra.ext.org.yaml.model.Node.Mapping(hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
          (java.util.function.Function<hydra.util.Pair<String, hydra.json.model.Value>, hydra.util.Pair<hydra.ext.org.yaml.model.Node, hydra.ext.org.yaml.model.Node>>) (kv -> (hydra.util.Pair<hydra.ext.org.yaml.model.Node, hydra.ext.org.yaml.model.Node>) ((hydra.util.Pair<hydra.ext.org.yaml.model.Node, hydra.ext.org.yaml.model.Node>) (new hydra.util.Pair<hydra.ext.org.yaml.model.Node, hydra.ext.org.yaml.model.Node>(new hydra.ext.org.yaml.model.Node.Scalar(new hydra.ext.org.yaml.model.Scalar.Str(hydra.lib.pairs.First.apply(kv))), hydra.json.yaml.encode.Encode.jsonToYaml(hydra.lib.pairs.Second.apply(kv)))))),
          hydra.lib.maps.ToList.apply((obj).value))));
      }
      
      @Override
      public hydra.ext.org.yaml.model.Node visit(hydra.json.model.Value.String_ s) {
        return new hydra.ext.org.yaml.model.Node.Scalar(new hydra.ext.org.yaml.model.Scalar.Str((s).value));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.ext.org.yaml.model.Node> toYaml(hydra.core.Term term) {
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.json.model.Value, hydra.ext.org.yaml.model.Node>) (v -> hydra.json.yaml.encode.Encode.jsonToYaml(v)),
      hydra.json.encode.Encode.toJson(term));
  }
}
