// Note: this is an automatically generated file. Do not edit.

package hydra.test.json;

/**
 * Round-trip test cases for JSON encoding and decoding
 */
public interface Roundtrip {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("JSON round-trip", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      new hydra.testing.TestGroup("literal types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("boolean true", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()),
            new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true)))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(true))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("boolean false", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Boolean_()),
            new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false)))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(false))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("int8 positive", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int8())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int8())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int8((byte) (42)))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int8((byte) (42))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("int8 negative", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int8())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int8())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int8((byte) (-17)))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int8((byte) (-17))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("int16", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int16())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int16())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int16((short) (1000)))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int16((short) (1000))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("int32", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(100000))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(100000)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("uint8", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint8())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint8())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint8((short) (200)))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint8((short) (200))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("uint16", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint16())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint16())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint16('\uC350'))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint16('\uC350')))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("int64", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int64())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int64())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (1000000000000L)))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int64((long) (1000000000000L))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("uint32", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint32())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Uint32())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint32((long) (4000000000L)))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Uint32((long) (4000000000L))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("float32", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (1.5)))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (1.5))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("float64", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float64())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float64())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.14159))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(3.14159)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("float32 NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (Double.NaN)))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (Double.NaN))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("float32 positive infinity", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (Double.POSITIVE_INFINITY)))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (Double.POSITIVE_INFINITY))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("float32 negative infinity", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float32())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (Double.NEGATIVE_INFINITY)))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float32((float) (Double.NEGATIVE_INFINITY))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("float64 NaN", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float64())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float64())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NaN)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("float64 positive infinity", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float64())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float64())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.POSITIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("float64 negative infinity", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float64())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.Float_(new hydra.core.FloatType.Float64())),
            new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY))))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Float64(Double.NEGATIVE_INFINITY)))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string simple", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string empty", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("")))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.String_(""))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("string with spaces", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()),
            new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello world")))), hydra.show.Core.term(new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello world"))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("collection types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("list of integers", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
            new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
              new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))),
          new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("list of strings", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")),
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("b"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
            new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList())))), hydra.show.Core.term(new hydra.core.Term.List((java.util.List<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptyList()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nested list", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.List(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.List(new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))),
            new hydra.core.Term.List(java.util.Arrays.asList(
              new hydra.core.Term.List(java.util.Arrays.asList(
                new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
                new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))),
              new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3))))))))), hydra.show.Core.term(new hydra.core.Term.List(java.util.Arrays.asList(
          new hydra.core.Term.List(java.util.Arrays.asList(
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(1))),
            new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(2))))),
          new hydra.core.Term.List(java.util.Arrays.asList(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(3)))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("set of strings", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")),
              new hydra.core.Term.Literal(new hydra.core.Literal.String_("b"))))))), hydra.show.Core.term(new hydra.core.Term.Set(new java.util.TreeSet(java.util.Set.of(
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("a")),
          new hydra.core.Term.Literal(new hydra.core.Literal.String_("b")))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("empty set", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Set(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
            new hydra.core.Term.Set((java.util.Set<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptySet())))), hydra.show.Core.term(new hydra.core.Term.Set((java.util.Set<hydra.core.Term>) (java.util.Collections.<hydra.core.Term>emptySet()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("optional types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("optional string with value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello"))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("optional string nothing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
            new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))), hydra.show.Core.term(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("optional int with value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))),
            new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nested optional: nothing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Maybe(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Maybe(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
            new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))), hydra.show.Core.term(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nested optional: just nothing", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Maybe(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Maybe(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
            new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("nested optional: just just value", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Maybe(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Maybe(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
            new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello")))))))), hydra.show.Core.term(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("hello"))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())))),
      new hydra.testing.TestGroup("record types", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), java.util.Arrays.asList(
        new hydra.testing.TestCaseWithMetadata("record with required fields", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Record(java.util.Arrays.asList(
                new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
                new hydra.core.FieldType(new hydra.core.Name("age"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Record(java.util.Arrays.asList(
              new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
              new hydra.core.FieldType(new hydra.core.Name("age"), new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))),
            new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("test"), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
              new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30))))))))), hydra.show.Core.term(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("test"), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
          new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(30)))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("record with optional field present", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Record(java.util.Arrays.asList(
                new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
                new hydra.core.FieldType(new hydra.core.Name("email"), new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Record(java.util.Arrays.asList(
              new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
              new hydra.core.FieldType(new hydra.core.Name("email"), new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))),
            new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("test"), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
              new hydra.core.Field(new hydra.core.Name("email"), new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("alice@example.com")))))))))), hydra.show.Core.term(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("test"), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
          new hydra.core.Field(new hydra.core.Name("email"), new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.String_("alice@example.com"))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("record with optional field absent", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Record(java.util.Arrays.asList(
                new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
                new hydra.core.FieldType(new hydra.core.Name("email"), new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Record(java.util.Arrays.asList(
              new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
              new hydra.core.FieldType(new hydra.core.Name("email"), new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))))),
            new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("test"), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
              new hydra.core.Field(new hydra.core.Name("email"), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))))))), hydra.show.Core.term(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("test"), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Alice"))),
          new hydra.core.Field(new hydra.core.Name("email"), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("record with mixed optional fields", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Record(java.util.Arrays.asList(
                new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
                new hydra.core.FieldType(new hydra.core.Name("email"), new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
                new hydra.core.FieldType(new hydra.core.Name("age"), new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Record(java.util.Arrays.asList(
              new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
              new hydra.core.FieldType(new hydra.core.Name("email"), new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))),
              new hydra.core.FieldType(new hydra.core.Name("age"), new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32())))))),
            new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("test"), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Bob"))),
              new hydra.core.Field(new hydra.core.Name("email"), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))),
              new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(25))))))))))), hydra.show.Core.term(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("test"), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("Bob"))),
          new hydra.core.Field(new hydra.core.Name("email"), new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing()))),
          new hydra.core.Field(new hydra.core.Name("age"), new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(25)))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList())),
        new hydra.testing.TestCaseWithMetadata("record with nested optional field", new hydra.testing.TestCase.Universal(new hydra.testing.UniversalTestCase(hydra.lib.eithers.Either.apply(
          (java.util.function.Function<String, String>) (e -> e),
          (java.util.function.Function<hydra.json.model.Value, String>) (json -> hydra.lib.eithers.Either.apply(
            (java.util.function.Function<String, String>) (e -> e),
            (java.util.function.Function<hydra.core.Term, String>) (decoded -> hydra.show.Core.term(decoded)),
            hydra.json.Decode.fromJson(
              (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
              new hydra.core.Name("test"),
              new hydra.core.Type.Record(java.util.Arrays.asList(
                new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
                new hydra.core.FieldType(new hydra.core.Name("value"), new hydra.core.Type.Maybe(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))),
              json))),
          hydra.json.Encode.toJson(
            (java.util.Map<hydra.core.Name, hydra.core.Type>) ((java.util.Map<hydra.core.Name, hydra.core.Type>) (hydra.lib.maps.Empty.<hydra.core.Name, hydra.core.Type>apply())),
            new hydra.core.Name("test"),
            new hydra.core.Type.Record(java.util.Arrays.asList(
              new hydra.core.FieldType(new hydra.core.Name("name"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
              new hydra.core.FieldType(new hydra.core.Name("value"), new hydra.core.Type.Maybe(new hydra.core.Type.Maybe(new hydra.core.Type.Literal(new hydra.core.LiteralType.Integer_(new hydra.core.IntegerType.Int32()))))))),
            new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("test"), java.util.Arrays.asList(
              new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("test"))),
              new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42))))))))))))), hydra.show.Core.term(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("test"), java.util.Arrays.asList(
          new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_("test"))),
          new hydra.core.Field(new hydra.core.Name("value"), new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Maybe(hydra.util.Maybe.just(new hydra.core.Term.Literal(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Int32(42)))))))))))))), (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.Tag>) (java.util.Collections.<hydra.testing.Tag>emptyList()))))), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}
