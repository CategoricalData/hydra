// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

/**
 * A test case which parses a JSON string and compares the result with an expected JSON value
 */
public class JsonParserTestCase {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.JsonParserTestCase");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.testing.ParserTestCase<hydra.json.Value> value;
  
  public JsonParserTestCase (hydra.testing.ParserTestCase<hydra.json.Value> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof JsonParserTestCase)) {
      return false;
    }
    JsonParserTestCase o = (JsonParserTestCase) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
