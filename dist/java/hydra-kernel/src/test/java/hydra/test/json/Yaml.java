// Note: this is an automatically generated file. Do not edit.

package hydra.test.json;

/**
 * Round-trip test cases for the JSON&lt;-&gt;YAML bridge, focused on decimal precision
 */
public interface Yaml {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("JSON<->YAML bridge", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(new hydra.testing.TestGroup("decimal round-trip", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
      new hydra.testing.TestCaseWithMetadata("zero", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<String, String>) (e -> e),
        (java.util.function.Function<hydra.json.model.Value, String>) (back -> hydra.json.Writer.printJson(back)),
        hydra.json.yaml.Decode.yamlToJson(hydra.json.yaml.Encode.jsonToYaml(new hydra.json.model.Value.Number_(new java.math.BigDecimal("0.0"))))), hydra.json.Writer.printJson(new hydra.json.model.Value.Number_(new java.math.BigDecimal("0.0"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("positive whole", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<String, String>) (e -> e),
        (java.util.function.Function<hydra.json.model.Value, String>) (back -> hydra.json.Writer.printJson(back)),
        hydra.json.yaml.Decode.yamlToJson(hydra.json.yaml.Encode.jsonToYaml(new hydra.json.model.Value.Number_(new java.math.BigDecimal("42.0"))))), hydra.json.Writer.printJson(new hydra.json.model.Value.Number_(new java.math.BigDecimal("42.0"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("negative whole", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<String, String>) (e -> e),
        (java.util.function.Function<hydra.json.model.Value, String>) (back -> hydra.json.Writer.printJson(back)),
        hydra.json.yaml.Decode.yamlToJson(hydra.json.yaml.Encode.jsonToYaml(new hydra.json.model.Value.Number_(new java.math.BigDecimal("-17.0"))))), hydra.json.Writer.printJson(new hydra.json.model.Value.Number_(new java.math.BigDecimal("-17.0"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("fraction", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<String, String>) (e -> e),
        (java.util.function.Function<hydra.json.model.Value, String>) (back -> hydra.json.Writer.printJson(back)),
        hydra.json.yaml.Decode.yamlToJson(hydra.json.yaml.Encode.jsonToYaml(new hydra.json.model.Value.Number_(new java.math.BigDecimal("3.14"))))), hydra.json.Writer.printJson(new hydra.json.model.Value.Number_(new java.math.BigDecimal("3.14"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("negative fraction", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<String, String>) (e -> e),
        (java.util.function.Function<hydra.json.model.Value, String>) (back -> hydra.json.Writer.printJson(back)),
        hydra.json.yaml.Decode.yamlToJson(hydra.json.yaml.Encode.jsonToYaml(new hydra.json.model.Value.Number_(new java.math.BigDecimal("-2.5"))))), hydra.json.Writer.printJson(new hydra.json.model.Value.Number_(new java.math.BigDecimal("-2.5"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("tiny exponent", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<String, String>) (e -> e),
        (java.util.function.Function<hydra.json.model.Value, String>) (back -> hydra.json.Writer.printJson(back)),
        hydra.json.yaml.Decode.yamlToJson(hydra.json.yaml.Encode.jsonToYaml(new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0e-20"))))), hydra.json.Writer.printJson(new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0e-20"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("huge exponent", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<String, String>) (e -> e),
        (java.util.function.Function<hydra.json.model.Value, String>) (back -> hydra.json.Writer.printJson(back)),
        hydra.json.yaml.Decode.yamlToJson(hydra.json.yaml.Encode.jsonToYaml(new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0e20"))))), hydra.json.Writer.printJson(new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0e20"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("null", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<String, String>) (e -> e),
        (java.util.function.Function<hydra.json.model.Value, String>) (back -> hydra.json.Writer.printJson(back)),
        hydra.json.yaml.Decode.yamlToJson(hydra.json.yaml.Encode.jsonToYaml(new hydra.json.model.Value.Null()))), hydra.json.Writer.printJson(new hydra.json.model.Value.Null()))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("true", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<String, String>) (e -> e),
        (java.util.function.Function<hydra.json.model.Value, String>) (back -> hydra.json.Writer.printJson(back)),
        hydra.json.yaml.Decode.yamlToJson(hydra.json.yaml.Encode.jsonToYaml(new hydra.json.model.Value.Boolean_(true)))), hydra.json.Writer.printJson(new hydra.json.model.Value.Boolean_(true)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("false", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<String, String>) (e -> e),
        (java.util.function.Function<hydra.json.model.Value, String>) (back -> hydra.json.Writer.printJson(back)),
        hydra.json.yaml.Decode.yamlToJson(hydra.json.yaml.Encode.jsonToYaml(new hydra.json.model.Value.Boolean_(false)))), hydra.json.Writer.printJson(new hydra.json.model.Value.Boolean_(false)))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("simple string", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<String, String>) (e -> e),
        (java.util.function.Function<hydra.json.model.Value, String>) (back -> hydra.json.Writer.printJson(back)),
        hydra.json.yaml.Decode.yamlToJson(hydra.json.yaml.Encode.jsonToYaml(new hydra.json.model.Value.String_("hello")))), hydra.json.Writer.printJson(new hydra.json.model.Value.String_("hello")))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
      new hydra.testing.TestCaseWithMetadata("mixed array", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
        (java.util.function.Function<String, String>) (e -> e),
        (java.util.function.Function<hydra.json.model.Value, String>) (back -> hydra.json.Writer.printJson(back)),
        hydra.json.yaml.Decode.yamlToJson(hydra.json.yaml.Encode.jsonToYaml(new hydra.json.model.Value.Array(java.util.Arrays.asList(
          new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0")),
          new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0e-20")),
          new hydra.json.model.Value.String_("note")))))), hydra.json.Writer.printJson(new hydra.json.model.Value.Array(java.util.Arrays.asList(
        new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0")),
        new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0e-20")),
        new hydra.json.model.Value.String_("note")))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}
