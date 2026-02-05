// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which strips all annotations from a type and compares the result with the expected type
 */
public class DeannotateTypeTestCase implements Serializable, Comparable<DeannotateTypeTestCase> {
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
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DeannotateTypeTestCase)) {
      return false;
    }
    DeannotateTypeTestCase o = (DeannotateTypeTestCase) (other);
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
  public int compareTo(DeannotateTypeTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) (input)).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (output)).compareTo(other.output);
  }
  
  public DeannotateTypeTestCase withInput(hydra.core.Type input) {
    return new DeannotateTypeTestCase(input, output);
  }
  
  public DeannotateTypeTestCase withOutput(hydra.core.Type output) {
    return new DeannotateTypeTestCase(input, output);
  }
}
