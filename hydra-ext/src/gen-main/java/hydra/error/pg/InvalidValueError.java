// Note: this is an automatically generated file. Do not edit.

package hydra.error.pg;

import java.io.Serializable;

/**
 * An error indicating that a value does not match the expected type
 */
public class InvalidValueError implements Serializable, Comparable<InvalidValueError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.pg.InvalidValueError");

  public static final hydra.core.Name EXPECTED_TYPE = new hydra.core.Name("expectedType");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  /**
   * The expected type, as a string
   */
  public final String expectedType;

  /**
   * The actual value, as a string
   */
  public final String value;

  public InvalidValueError (String expectedType, String value) {
    this.expectedType = expectedType;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InvalidValueError)) {
      return false;
    }
    InvalidValueError o = (InvalidValueError) other;
    return java.util.Objects.equals(
      this.expectedType,
      o.expectedType) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expectedType) + 3 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InvalidValueError other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      expectedType,
      other.expectedType);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public InvalidValueError withExpectedType(String expectedType) {
    return new InvalidValueError(expectedType, value);
  }

  public InvalidValueError withValue(String value) {
    return new InvalidValueError(expectedType, value);
  }
}
