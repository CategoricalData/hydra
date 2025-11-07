// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case providing a term for which type inference is expected to fail
 */
public class InferenceFailureTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.InferenceFailureTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  /**
   * The term for which inference should fail
   */
  public final hydra.core.Term input;
  
  public InferenceFailureTestCase (hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    this.input = input;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InferenceFailureTestCase)) {
      return false;
    }
    InferenceFailureTestCase o = (InferenceFailureTestCase) (other);
    return input.equals(o.input);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode();
  }
}
