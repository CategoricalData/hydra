// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which performs type inference on a given term and compares the result with an expected type scheme
 */
public class InferenceTestCase implements Serializable, Comparable<InferenceTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.InferenceTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The term to infer
   */
  public final hydra.core.Term input;
  
  /**
   * The expected type scheme
   */
  public final hydra.core.TypeScheme output;
  
  public InferenceTestCase (hydra.core.Term input, hydra.core.TypeScheme output) {
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InferenceTestCase)) {
      return false;
    }
    InferenceTestCase o = (InferenceTestCase) other;
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
  public int compareTo(InferenceTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) input).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) output).compareTo(other.output);
  }
  
  public InferenceTestCase withInput(hydra.core.Term input) {
    return new InferenceTestCase(input, output);
  }
  
  public InferenceTestCase withOutput(hydra.core.TypeScheme output) {
    return new InferenceTestCase(input, output);
  }
}
