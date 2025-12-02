// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which strips all annotations from a term and compares the result with the expected term
 */
public class DeannotateTermTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.DeannotateTermTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The term to deannotate
   */
  public final hydra.core.Term input;
  
  /**
   * The expected deannotated term
   */
  public final hydra.core.Term output;
  
  public DeannotateTermTestCase (hydra.core.Term input, hydra.core.Term output) {
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DeannotateTermTestCase)) {
      return false;
    }
    DeannotateTermTestCase o = (DeannotateTermTestCase) (other);
    return input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * output.hashCode();
  }
  
  public DeannotateTermTestCase withInput(hydra.core.Term input) {
    java.util.Objects.requireNonNull((input));
    return new DeannotateTermTestCase(input, output);
  }
  
  public DeannotateTermTestCase withOutput(hydra.core.Term output) {
    java.util.Objects.requireNonNull((output));
    return new DeannotateTermTestCase(input, output);
  }
}
