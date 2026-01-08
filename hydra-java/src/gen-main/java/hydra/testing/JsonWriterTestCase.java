// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

/**
 * A test case which serializes a JSON value to a string and compares it to the expected string
 */
public class JsonWriterTestCase {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.JsonWriterTestCase");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.testing.WriterTestCase<hydra.json.model.Value> value;
  
  public JsonWriterTestCase (hydra.testing.WriterTestCase<hydra.json.model.Value> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof JsonWriterTestCase)) {
      return false;
    }
    JsonWriterTestCase o = (JsonWriterTestCase) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
