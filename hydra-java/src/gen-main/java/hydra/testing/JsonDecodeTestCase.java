// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case for the Either-based JSON decoder. Takes a type, input JSON, and expected result (Either String Term).
 */
public class JsonDecodeTestCase implements Serializable, Comparable<JsonDecodeTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.JsonDecodeTestCase");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_JSON = new hydra.core.Name("json");
  
  public static final hydra.core.Name FIELD_NAME_EXPECTED = new hydra.core.Name("expected");
  
  /**
   * The Hydra type to decode into
   */
  public final hydra.core.Type type;
  
  /**
   * The input JSON value
   */
  public final hydra.json.model.Value json;
  
  /**
   * The expected result: Left for error, Right for decoded term
   */
  public final hydra.util.Either<String, hydra.core.Term> expected;
  
  public JsonDecodeTestCase (hydra.core.Type type, hydra.json.model.Value json, hydra.util.Either<String, hydra.core.Term> expected) {
    this.type = type;
    this.json = json;
    this.expected = expected;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof JsonDecodeTestCase)) {
      return false;
    }
    JsonDecodeTestCase o = (JsonDecodeTestCase) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.json,
      o.json) && java.util.Objects.equals(
      this.expected,
      o.expected);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(json) + 5 * java.util.Objects.hashCode(expected);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(JsonDecodeTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) type).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) json).compareTo(other.json);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      expected.hashCode(),
      other.expected.hashCode());
  }
  
  public JsonDecodeTestCase withType(hydra.core.Type type) {
    return new JsonDecodeTestCase(type, json, expected);
  }
  
  public JsonDecodeTestCase withJson(hydra.json.model.Value json) {
    return new JsonDecodeTestCase(type, json, expected);
  }
  
  public JsonDecodeTestCase withExpected(hydra.util.Either<String, hydra.core.Term> expected) {
    return new JsonDecodeTestCase(type, json, expected);
  }
}
