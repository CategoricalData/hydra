// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which parses an input string and compares the result with an expected value
 */
public class ParserTestCase<A> implements Serializable, Comparable<ParserTestCase<A>> {
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
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParserTestCase)) {
      return false;
    }
    ParserTestCase o = (ParserTestCase) other;
    return java.util.Objects.equals(
      this.input,
      o.input) && java.util.Objects.equals(
      this.output,
      o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(input) + 3 * java.util.Objects.hashCode(output);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ParserTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) input).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) output).compareTo(other.output);
  }
  
  public ParserTestCase withInput(String input) {
    return new ParserTestCase(input, output);
  }
  
  public ParserTestCase withOutput(hydra.parsing.ParseResult<A> output) {
    return new ParserTestCase(input, output);
  }
}
