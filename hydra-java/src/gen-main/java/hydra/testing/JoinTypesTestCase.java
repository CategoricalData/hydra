// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which joins two types (producing type constraints or failing). The expected result is either Left (failure) or Right (list of constraints).
 */
public class JoinTypesTestCase implements Serializable, Comparable<JoinTypesTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.JoinTypesTestCase");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public static final hydra.core.Name FIELD_NAME_EXPECTED = new hydra.core.Name("expected");
  
  /**
   * The left type to join
   */
  public final hydra.core.Type left;
  
  /**
   * The right type to join
   */
  public final hydra.core.Type right;
  
  /**
   * The expected result: Left for failure, Right for constraints
   */
  public final hydra.util.Either<java.lang.Void, java.util.List<hydra.typing.TypeConstraint>> expected;
  
  public JoinTypesTestCase (hydra.core.Type left, hydra.core.Type right, hydra.util.Either<java.lang.Void, java.util.List<hydra.typing.TypeConstraint>> expected) {
    this.left = left;
    this.right = right;
    this.expected = expected;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof JoinTypesTestCase)) {
      return false;
    }
    JoinTypesTestCase o = (JoinTypesTestCase) (other);
    return java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.right,
      o.right) && java.util.Objects.equals(
      this.expected,
      o.expected);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(left) + 3 * java.util.Objects.hashCode(right) + 5 * java.util.Objects.hashCode(expected);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(JoinTypesTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) (left)).compareTo(other.left);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (right)).compareTo(other.right);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      expected.hashCode(),
      other.expected.hashCode());
  }
  
  public JoinTypesTestCase withLeft(hydra.core.Type left) {
    return new JoinTypesTestCase(left, right, expected);
  }
  
  public JoinTypesTestCase withRight(hydra.core.Type right) {
    return new JoinTypesTestCase(left, right, expected);
  }
  
  public JoinTypesTestCase withExpected(hydra.util.Either<java.lang.Void, java.util.List<hydra.typing.TypeConstraint>> expected) {
    return new JoinTypesTestCase(left, right, expected);
  }
}
