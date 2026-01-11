// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case for the Either-based JSON decoder. Takes a type, input JSON, and expected result (Either String Term).
 */
public class JsonDecodeTestCase implements Serializable {
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
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((json));
    java.util.Objects.requireNonNull((expected));
    this.type = type;
    this.json = json;
    this.expected = expected;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof JsonDecodeTestCase)) {
      return false;
    }
    JsonDecodeTestCase o = (JsonDecodeTestCase) (other);
    return type.equals(o.type) && json.equals(o.json) && expected.equals(o.expected);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * json.hashCode() + 5 * expected.hashCode();
  }
  
  public JsonDecodeTestCase withType(hydra.core.Type type) {
    java.util.Objects.requireNonNull((type));
    return new JsonDecodeTestCase(type, json, expected);
  }
  
  public JsonDecodeTestCase withJson(hydra.json.model.Value json) {
    java.util.Objects.requireNonNull((json));
    return new JsonDecodeTestCase(type, json, expected);
  }
  
  public JsonDecodeTestCase withExpected(hydra.util.Either<String, hydra.core.Term> expected) {
    java.util.Objects.requireNonNull((expected));
    return new JsonDecodeTestCase(type, json, expected);
  }
}
