// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * An error that occurred during type unification
 */
public class UnificationError implements Serializable, Comparable<UnificationError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.UnificationError");

  public static final hydra.core.Name LEFT_TYPE = new hydra.core.Name("leftType");

  public static final hydra.core.Name RIGHT_TYPE = new hydra.core.Name("rightType");

  public static final hydra.core.Name MESSAGE = new hydra.core.Name("message");

  /**
   * The left-hand type in the unification
   */
  public final hydra.core.Type leftType;

  /**
   * The right-hand type in the unification
   */
  public final hydra.core.Type rightType;

  /**
   * A human-readable error message
   */
  public final String message;

  public UnificationError (hydra.core.Type leftType, hydra.core.Type rightType, String message) {
    this.leftType = leftType;
    this.rightType = rightType;
    this.message = message;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnificationError)) {
      return false;
    }
    UnificationError o = (UnificationError) other;
    return java.util.Objects.equals(
      this.leftType,
      o.leftType) && java.util.Objects.equals(
      this.rightType,
      o.rightType) && java.util.Objects.equals(
      this.message,
      o.message);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(leftType) + 3 * java.util.Objects.hashCode(rightType) + 5 * java.util.Objects.hashCode(message);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnificationError other) {
    int cmp = 0;
    cmp = ((Comparable) leftType).compareTo(other.leftType);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) rightType).compareTo(other.rightType);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) message).compareTo(other.message);
  }

  public UnificationError withLeftType(hydra.core.Type leftType) {
    return new UnificationError(leftType, rightType, message);
  }

  public UnificationError withRightType(hydra.core.Type rightType) {
    return new UnificationError(leftType, rightType, message);
  }

  public UnificationError withMessage(String message) {
    return new UnificationError(leftType, rightType, message);
  }
}
