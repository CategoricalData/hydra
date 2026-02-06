// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which attempts to unify two types and compares the result. The expected result is either Left (failure message substring) or Right (substitution).
 */
public class UnifyTypesTestCase implements Serializable, Comparable<UnifyTypesTestCase> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.UnifyTypesTestCase");
  
  public static final hydra.core.Name FIELD_NAME_SCHEMA_TYPES = new hydra.core.Name("schemaTypes");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  public static final hydra.core.Name FIELD_NAME_EXPECTED = new hydra.core.Name("expected");
  
  /**
   * The schema types map (type variable names that should not be bound)
   */
  public final java.util.List<hydra.core.Name> schemaTypes;
  
  /**
   * The left type to unify
   */
  public final hydra.core.Type left;
  
  /**
   * The right type to unify
   */
  public final hydra.core.Type right;
  
  /**
   * The expected result: Left for failure (substring of error), Right for substitution
   */
  public final hydra.util.Either<String, hydra.typing.TypeSubst> expected;
  
  public UnifyTypesTestCase (java.util.List<hydra.core.Name> schemaTypes, hydra.core.Type left, hydra.core.Type right, hydra.util.Either<String, hydra.typing.TypeSubst> expected) {
    this.schemaTypes = schemaTypes;
    this.left = left;
    this.right = right;
    this.expected = expected;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnifyTypesTestCase)) {
      return false;
    }
    UnifyTypesTestCase o = (UnifyTypesTestCase) (other);
    return java.util.Objects.equals(
      this.schemaTypes,
      o.schemaTypes) && java.util.Objects.equals(
      this.left,
      o.left) && java.util.Objects.equals(
      this.right,
      o.right) && java.util.Objects.equals(
      this.expected,
      o.expected);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(schemaTypes) + 3 * java.util.Objects.hashCode(left) + 5 * java.util.Objects.hashCode(right) + 7 * java.util.Objects.hashCode(expected);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnifyTypesTestCase other) {
    int cmp = 0;
    cmp = Integer.compare(
      schemaTypes.hashCode(),
      other.schemaTypes.hashCode());
    if (cmp != 0) {
      return cmp;
    }
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
  
  public UnifyTypesTestCase withSchemaTypes(java.util.List<hydra.core.Name> schemaTypes) {
    return new UnifyTypesTestCase(schemaTypes, left, right, expected);
  }
  
  public UnifyTypesTestCase withLeft(hydra.core.Type left) {
    return new UnifyTypesTestCase(schemaTypes, left, right, expected);
  }
  
  public UnifyTypesTestCase withRight(hydra.core.Type right) {
    return new UnifyTypesTestCase(schemaTypes, left, right, expected);
  }
  
  public UnifyTypesTestCase withExpected(hydra.util.Either<String, hydra.typing.TypeSubst> expected) {
    return new UnifyTypesTestCase(schemaTypes, left, right, expected);
  }
}
