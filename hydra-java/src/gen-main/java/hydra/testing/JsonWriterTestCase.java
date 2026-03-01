// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

/**
 * A test case which serializes a JSON value to a string and compares it to the expected string.
 * Type alias for WriterTestCase&lt;hydra.json.model.Value&gt;
 */
public class JsonWriterTestCase extends hydra.testing.WriterTestCase<hydra.json.model.Value> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.testing.JsonWriterTestCase");

  public JsonWriterTestCase (hydra.json.model.Value input, String output) {
    super(input, output);
  }

  /**
   * Construct from a WriterTestCase (for type alias compatibility).
   */
  public static JsonWriterTestCase fromWriterTestCase(hydra.testing.WriterTestCase<hydra.json.model.Value> tc) {
    return new JsonWriterTestCase(tc.input, tc.output);
  }
}
