// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which writes a value to a string and compares it to the expected string
 */
public class WriterTestCase<A> implements Serializable, Comparable<WriterTestCase<A>> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.WriterTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The input value to write
   */
  public final A input;
  
  /**
   * The expected string
   */
  public final String output;
  
  public WriterTestCase (A input, String output) {
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WriterTestCase)) {
      return false;
    }
    WriterTestCase o = (WriterTestCase) other;
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
  public int compareTo(WriterTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) input).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) output).compareTo(other.output);
  }
  
  public WriterTestCase withInput(A input) {
    return new WriterTestCase(input, output);
  }
  
  public WriterTestCase withOutput(String output) {
    return new WriterTestCase(input, output);
  }
}
