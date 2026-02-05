// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which performs beta reduction on a type (reducing type applications) and compares the result with the expected type
 */
public class TypeReductionTestCase implements Serializable, Comparable<TypeReductionTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.TypeReductionTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  public static final hydra.core.Name FIELD_NAME_OUTPUT = new hydra.core.Name("output");
  
  /**
   * The type to reduce
   */
  public final hydra.core.Type input;
  
  /**
   * The expected reduced type
   */
  public final hydra.core.Type output;
  
  public TypeReductionTestCase (hydra.core.Type input, hydra.core.Type output) {
    this.input = input;
    this.output = output;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeReductionTestCase)) {
      return false;
    }
    TypeReductionTestCase o = (TypeReductionTestCase) (other);
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
  public int compareTo(TypeReductionTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) (input)).compareTo(other.input);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (output)).compareTo(other.output);
  }
  
  public TypeReductionTestCase withInput(hydra.core.Type input) {
    return new TypeReductionTestCase(input, output);
  }
  
  public TypeReductionTestCase withOutput(hydra.core.Type output) {
    return new TypeReductionTestCase(input, output);
  }
}
