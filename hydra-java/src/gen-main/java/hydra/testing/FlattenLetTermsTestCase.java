// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which flattens nested let terms, lifting inner bindings to the outer let, and compares the result with the expected term
 */
public class FlattenLetTermsTestCase implements Serializable, Comparable<FlattenLetTermsTestCase> {
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
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FlattenLetTermsTestCase)) {
      return false;
    }
    FlattenLetTermsTestCase o = (FlattenLetTermsTestCase) (other);
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
  public int compareTo(FlattenLetTermsTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) (input)).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (output)).compareTo(other.output);
  }
  
  public FlattenLetTermsTestCase withInput(hydra.core.Term input) {
    return new FlattenLetTermsTestCase(input, output);
  }
  
  public FlattenLetTermsTestCase withOutput(hydra.core.Term output) {
    return new FlattenLetTermsTestCase(input, output);
  }
}
