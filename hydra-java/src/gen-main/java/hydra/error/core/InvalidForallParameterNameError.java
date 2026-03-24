// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A forall type parameter name that violates type variable naming conventions (optional)
 */
public class InvalidForallParameterNameError implements Serializable, Comparable<InvalidForallParameterNameError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.InvalidForallParameterNameError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the forall type
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The invalid parameter name
   */
  public final hydra.core.Name name;

  public InvalidForallParameterNameError (hydra.paths.SubtermPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InvalidForallParameterNameError)) {
      return false;
    }
    InvalidForallParameterNameError o = (InvalidForallParameterNameError) other;
    return java.util.Objects.equals(
      this.location,
      o.location) && java.util.Objects.equals(
      this.name,
      o.name);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(location) + 3 * java.util.Objects.hashCode(name);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InvalidForallParameterNameError other) {
    int cmp = 0;
    cmp = ((Comparable) location).compareTo(other.location);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) name).compareTo(other.name);
  }

  public InvalidForallParameterNameError withLocation(hydra.paths.SubtermPath location) {
    return new InvalidForallParameterNameError(location, name);
  }

  public InvalidForallParameterNameError withName(hydra.core.Name name) {
    return new InvalidForallParameterNameError(location, name);
  }
}
