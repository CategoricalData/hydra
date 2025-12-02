// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which performs term simplification (beta reduction and optimization) and compares the result with the expected term
 */
public class SimplifyTermTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.SimplifyTermTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The term to simplify
   */
  public final hydra.core.Term input;
  
  /**
   * The expected simplified term
   */
  public final hydra.core.Term output;
  
  public SimplifyTermTestCase (hydra.core.Term input, hydra.core.Term output) {
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SimplifyTermTestCase)) {
      return false;
    }
    SimplifyTermTestCase o = (SimplifyTermTestCase) (other);
    return input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * output.hashCode();
  }
  
  public SimplifyTermTestCase withInput(hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    return new SimplifyTermTestCase(input, output);
  }
  
  public SimplifyTermTestCase withOutput(hydra.core.Term output) {
    java.util.Objects.requireNonNull((output));
    return new SimplifyTermTestCase(input, output);
  }
}
