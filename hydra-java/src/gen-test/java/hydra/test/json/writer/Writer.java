// Note: this is an automatically generated file. Do not edit.

package hydra.test.json.writer;

/**
 * Test cases for JSON serialization
 */
public interface Writer {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("JSON serialization", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.List.of(
      new hydra.testing.TestGroup("primitives", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.List.<hydra.testing.TestGroup>of()), java.util.List.of(
        new hydra.testing.TestCaseWithMetadata("null", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Null(), "null"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("true", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Boolean_(true), "true"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("false", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Boolean_(false), "false"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("zero", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Number_(new java.math.BigDecimal("0.0")), "0"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("positive integer", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Number_(new java.math.BigDecimal("42.0")), "42"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("negative integer", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Number_(new java.math.BigDecimal("-17.0")), "-17"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("large integer", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Number_(new java.math.BigDecimal("1000000.0")), "1000000"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("decimal", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Number_(new java.math.BigDecimal("3.14")), "3.14"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("negative decimal", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Number_(new java.math.BigDecimal("-2.5")), "-2.5"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("small decimal", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0e-3")), "1.0e-3"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())))),
      new hydra.testing.TestGroup("strings", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.List.<hydra.testing.TestGroup>of()), java.util.List.of(
        new hydra.testing.TestCaseWithMetadata("empty string", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.String_(""), "\"\""))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("simple string", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.String_("hello"), "\"hello\""))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("string with spaces", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.String_("hello world"), "\"hello world\""))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("string with double quote", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.String_("say \"hi\""), "\"say \\\"hi\\\"\""))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("string with backslash", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.String_("path\\to\\file"), "\"path\\\\to\\\\file\""))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("string with newline", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.String_("line1\nline2"), "\"line1\\nline2\""))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("string with carriage return", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.String_("line1\rline2"), "\"line1\\rline2\""))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("string with tab", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.String_("col1\tcol2"), "\"col1\\tcol2\""))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("string with mixed escapes", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.String_("a\"b\\c\nd"), "\"a\\\"b\\\\c\\nd\""))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())))),
      new hydra.testing.TestGroup("arrays", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.List.<hydra.testing.TestGroup>of()), java.util.List.of(
        new hydra.testing.TestCaseWithMetadata("empty array", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Array((java.util.List<hydra.json.model.Value>) (java.util.List.<hydra.json.model.Value>of())), "[]"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("single element", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Array(java.util.List.of(new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0")))), "[1]"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("multiple numbers", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Array(java.util.List.of(
          new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0")),
          new hydra.json.model.Value.Number_(new java.math.BigDecimal("2.0")),
          new hydra.json.model.Value.Number_(new java.math.BigDecimal("3.0")))), "[1, 2, 3]"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("multiple strings", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Array(java.util.List.of(
          new hydra.json.model.Value.String_("a"),
          new hydra.json.model.Value.String_("b"))), "[\"a\", \"b\"]"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("mixed types", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Array(java.util.List.of(
          new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0")),
          new hydra.json.model.Value.String_("two"),
          new hydra.json.model.Value.Boolean_(true),
          new hydra.json.model.Value.Null())), "[1, \"two\", true, null]"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())))),
      new hydra.testing.TestGroup("objects", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.List.<hydra.testing.TestGroup>of()), java.util.List.of(
        new hydra.testing.TestCaseWithMetadata("empty object", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Object_((java.util.Map<String, hydra.json.model.Value>) ((java.util.Map<String, hydra.json.model.Value>) (java.util.Map.<String, hydra.json.model.Value>ofEntries()))), "{}"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("single key-value", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Object_(java.util.Map.ofEntries(java.util.Map.entry(
          "name",
          new hydra.json.model.Value.String_("Alice")))), "{\"name\": \"Alice\"}"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("multiple keys", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Object_(java.util.Map.ofEntries(
          java.util.Map.entry(
            "a",
            new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0"))),
          java.util.Map.entry(
            "b",
            new hydra.json.model.Value.Number_(new java.math.BigDecimal("2.0"))))), "{\"a\": 1, \"b\": 2}"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("mixed value types", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Object_(java.util.Map.ofEntries(
          java.util.Map.entry(
            "active",
            new hydra.json.model.Value.Boolean_(true)),
          java.util.Map.entry(
            "count",
            new hydra.json.model.Value.Number_(new java.math.BigDecimal("42.0"))),
          java.util.Map.entry(
            "name",
            new hydra.json.model.Value.String_("test")))), "{\"active\": true, \"count\": 42, \"name\": \"test\"}"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())))),
      new hydra.testing.TestGroup("nested structures", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.List.<hydra.testing.TestGroup>of()), java.util.List.of(
        new hydra.testing.TestCaseWithMetadata("nested arrays", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Array(java.util.List.of(
          new hydra.json.model.Value.Array(java.util.List.of(
            new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0")),
            new hydra.json.model.Value.Number_(new java.math.BigDecimal("2.0")))),
          new hydra.json.model.Value.Array(java.util.List.of(
            new hydra.json.model.Value.Number_(new java.math.BigDecimal("3.0")),
            new hydra.json.model.Value.Number_(new java.math.BigDecimal("4.0")))))), "[[1, 2], [3, 4]]"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("object with array", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Object_(java.util.Map.ofEntries(java.util.Map.entry(
          "items",
          new hydra.json.model.Value.Array(java.util.List.of(
            new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0")),
            new hydra.json.model.Value.Number_(new java.math.BigDecimal("2.0"))))))), "{\"items\": [1, 2]}"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("array of objects", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Array(java.util.List.of(
          new hydra.json.model.Value.Object_(java.util.Map.ofEntries(java.util.Map.entry(
            "id",
            new hydra.json.model.Value.Number_(new java.math.BigDecimal("1.0"))))),
          new hydra.json.model.Value.Object_(java.util.Map.ofEntries(java.util.Map.entry(
            "id",
            new hydra.json.model.Value.Number_(new java.math.BigDecimal("2.0"))))))), "[{\"id\": 1}, {\"id\": 2}]"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of())),
        new hydra.testing.TestCaseWithMetadata("nested object", new hydra.testing.TestCase.JsonWriter((hydra.testing.WriterTestCase<hydra.json.model.Value>) (new hydra.testing.WriterTestCase<hydra.json.model.Value>(new hydra.json.model.Value.Object_(java.util.Map.ofEntries(java.util.Map.entry(
          "user",
          new hydra.json.model.Value.Object_(java.util.Map.ofEntries(java.util.Map.entry(
            "name",
            new hydra.json.model.Value.String_("Bob"))))))), "{\"user\": {\"name\": \"Bob\"}}"))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.List.<hydra.testing.Tag>of()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.List.<hydra.testing.TestCaseWithMetadata>of()));
  }
}
