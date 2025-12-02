// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which normalizes type variables in a term (renaming them to t0, t1, t2, etc.) and compares the result with the expected term
 */
public class NormalizeTypeVariablesTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.NormalizeTypeVariablesTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The term with type annotations to normalize
   */
  public final hydra.core.Term input;
  
  /**
   * The expected term with normalized type variable names
   */
  public final hydra.core.Term output;
  
  public NormalizeTypeVariablesTestCase (hydra.core.Term input, hydra.core.Term output) {
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NormalizeTypeVariablesTestCase)) {
      return false;
    }
    NormalizeTypeVariablesTestCase o = (NormalizeTypeVariablesTestCase) (other);
    return input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * output.hashCode();
  }
  
  public NormalizeTypeVariablesTestCase withInput(hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    return new NormalizeTypeVariablesTestCase(input, output);
  }
  
  public NormalizeTypeVariablesTestCase withOutput(hydra.core.Term output) {
    java.util.Objects.requireNonNull((output));
    return new NormalizeTypeVariablesTestCase(input, output);
  }
}
