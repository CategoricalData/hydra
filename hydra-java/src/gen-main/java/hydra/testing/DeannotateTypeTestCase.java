// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which strips all annotations from a type and compares the result with the expected type
 */
public class DeannotateTypeTestCase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.DeannotateTypeTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The type to deannotate
   */
  public final hydra.core.Type input;
  
  /**
   * The expected deannotated type
   */
  public final hydra.core.Type output;
  
  public DeannotateTypeTestCase (hydra.core.Type input, hydra.core.Type output) {
    java.util.Objects.requireNonNull((input));
    java.util.Objects.requireNonNull((output));
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DeannotateTypeTestCase)) {
      return false;
    }
    DeannotateTypeTestCase o = (DeannotateTypeTestCase) (other);
    return input.equals(o.input) && output.equals(o.output);
  }
  
  @Override
  public int hashCode() {
    return 2 * input.hashCode() + 3 * output.hashCode();
  }
  
  public DeannotateTypeTestCase withInput(hydra.core.Type input) {
    java.util.Objects.requireNonNull((input));
    return new DeannotateTypeTestCase(input, output);
  }
  
  public DeannotateTypeTestCase withOutput(hydra.core.Type output) {
    java.util.Objects.requireNonNull((output));
    return new DeannotateTypeTestCase(input, output);
  }
}
