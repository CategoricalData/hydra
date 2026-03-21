// Note: this is an automatically generated file. Do not edit.

package hydra.error.checking;

import java.io.Serializable;

/**
 * A type mismatch between expected and actual types
 */
public class TypeMismatchError implements Serializable, Comparable<TypeMismatchError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.checking.TypeMismatchError");

  public static final hydra.core.Name EXPECTED_TYPE = new hydra.core.Name("expectedType");

  public static final hydra.core.Name ACTUAL_TYPE = new hydra.core.Name("actualType");

  /**
   * The expected type
   */
  public final hydra.core.Type expectedType;

  /**
   * The actual type encountered
   */
  public final hydra.core.Type actualType;

  public TypeMismatchError (hydra.core.Type expectedType, hydra.core.Type actualType) {
    this.expectedType = expectedType;
    this.actualType = actualType;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeMismatchError)) {
      return false;
    }
    TypeMismatchError o = (TypeMismatchError) other;
    return java.util.Objects.equals(
      this.expectedType,
      o.expectedType) && java.util.Objects.equals(
      this.actualType,
      o.actualType);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expectedType) + 3 * java.util.Objects.hashCode(actualType);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeMismatchError other) {
    int cmp = 0;
    cmp = ((Comparable) expectedType).compareTo(other.expectedType);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) actualType).compareTo(other.actualType);
  }

  public TypeMismatchError withExpectedType(hydra.core.Type expectedType) {
    return new TypeMismatchError(expectedType, actualType);
  }

  public TypeMismatchError withActualType(hydra.core.Type actualType) {
    return new TypeMismatchError(expectedType, actualType);
  }
}
