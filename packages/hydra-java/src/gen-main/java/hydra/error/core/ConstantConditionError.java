// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * An application of ifElse where the condition is a literal boolean, creating a dead branch (optional)
 */
public class ConstantConditionError implements Serializable, Comparable<ConstantConditionError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.ConstantConditionError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  /**
   * The path to the constant condition within the term
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The constant boolean value of the condition
   */
  public final Boolean value;

  public ConstantConditionError (hydra.paths.SubtermPath location, Boolean value) {
    this.location = location;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstantConditionError)) {
      return false;
    }
    ConstantConditionError o = (ConstantConditionError) other;
    return java.util.Objects.equals(
      this.location,
      o.location) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(location) + 3 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ConstantConditionError other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      location,
      other.location);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public ConstantConditionError withLocation(hydra.paths.SubtermPath location) {
    return new ConstantConditionError(location, value);
  }

  public ConstantConditionError withValue(Boolean value) {
    return new ConstantConditionError(location, value);
  }
}
