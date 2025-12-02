// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

/**
 * A test case which writes a value to a string and compares it to the expected string
 */
public class WriterTestCase<A> {
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
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WriterTestCase)) {
      return false;
    }
    WriterTestCase o = (WriterTestCase) (other);
    return input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * output.hashCode();
  }
  
  public WriterTestCase withInput(A input) {
    java.util.Objects.requireNonNull((input));
    return new WriterTestCase(input, output);
  }
  
  public WriterTestCase withOutput(String output) {
    java.util.Objects.requireNonNull((output));
    return new WriterTestCase(input, output);
  }
}
