// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

/**
 * A test case which parses an input string and compares the result with an expected value
 */
public class ParserTestCase<A> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.ParserTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The input string to parse
   */
  public final String input;
  
  /**
   * The expected parse result
   */
  public final hydra.parsing.ParseResult<A> output;
  
  public ParserTestCase (String input, hydra.parsing.ParseResult<A> output) {
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParserTestCase)) {
      return false;
    }
    ParserTestCase o = (ParserTestCase) (other);
    return input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * output.hashCode();
  }
  
  public ParserTestCase withInput(String input) {
    java.util.Objects.requireNonNull((input));
    return new ParserTestCase(input, output);
  }
  
  public ParserTestCase withOutput(hydra.parsing.ParseResult<A> output) {
    java.util.Objects.requireNonNull((output));
    return new ParserTestCase(input, output);
  }
}
