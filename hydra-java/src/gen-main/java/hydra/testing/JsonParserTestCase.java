// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

/**
 * A test case which parses a JSON string and compares the result with an expected JSON value.
 * Type alias for ParserTestCase&lt;hydra.json.model.Value&gt;
 */
public class JsonParserTestCase extends hydra.testing.ParserTestCase<hydra.json.model.Value> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.testing.JsonParserTestCase");

  public JsonParserTestCase (String input, hydra.parsing.ParseResult<hydra.json.model.Value> output) {
    super(input, output);
  }

  /**
   * Construct from a ParserTestCase (for type alias compatibility).
   */
  public static JsonParserTestCase fromParserTestCase(hydra.testing.ParserTestCase<hydra.json.model.Value> tc) {
    return new JsonParserTestCase(tc.input, tc.output);
  }
}
