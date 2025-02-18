// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which performs type inference on a given term and compares the result with an expected type scheme
 */
public class InferenceTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/testing.InferenceTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  public final hydra.core.Term input;
  
  public final hydra.core.TypeScheme output;
  
  public InferenceTestCase (hydra.core.Term input, hydra.core.TypeScheme output) {
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InferenceTestCase)) {
      return false;
    }
    InferenceTestCase o = (InferenceTestCase) (other);
    return input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * output.hashCode();
  }
  
  public InferenceTestCase withInput(hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    return new InferenceTestCase(input, output);
  }
  
  public InferenceTestCase withOutput(hydra.core.TypeScheme output) {
    java.util.Objects.requireNonNull((output));
    return new InferenceTestCase(input, output);
  }
}