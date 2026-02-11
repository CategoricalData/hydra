// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case providing a term for which type checking is expected to fail. Note: there are currently no such test cases.
 */
public class TypeCheckingFailureTestCase implements Serializable, Comparable<TypeCheckingFailureTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.TypeCheckingFailureTestCase");
  
  public static final hydra.core.Name FIELD_NAME_INPUT = new hydra.core.Name("input");
  
  /**
   * The term for which type checking should fail
   */
  public final hydra.core.Term input;
  
  public TypeCheckingFailureTestCase (hydra.core.Term input) {
    this.input = input;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeCheckingFailureTestCase)) {
      return false;
    }
    TypeCheckingFailureTestCase o = (TypeCheckingFailureTestCase) other;
    return java.util.Objects.equals(
      this.input,
      o.input);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(input);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeCheckingFailureTestCase other) {
    return ((Comparable) input).compareTo(other.input);
  }
}
