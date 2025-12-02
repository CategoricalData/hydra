// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which flattens nested let terms, lifting inner bindings to the outer let, and compares the result with the expected term
 */
public class FlattenLetTermsTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.FlattenLetTermsTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The term to flatten
   */
  public final hydra.core.Term input;
  
  /**
   * The expected flattened term
   */
  public final hydra.core.Term output;
  
  public FlattenLetTermsTestCase (hydra.core.Term input, hydra.core.Term output) {
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FlattenLetTermsTestCase)) {
      return false;
    }
    FlattenLetTermsTestCase o = (FlattenLetTermsTestCase) (other);
    return input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * output.hashCode();
  }
  
  public FlattenLetTermsTestCase withInput(hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    return new FlattenLetTermsTestCase(input, output);
  }
  
  public FlattenLetTermsTestCase withOutput(hydra.core.Term output) {
    java.util.Objects.requireNonNull((output));
    return new FlattenLetTermsTestCase(input, output);
  }
}
