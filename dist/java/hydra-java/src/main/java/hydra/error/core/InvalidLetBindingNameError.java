// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A let binding name that violates naming conventions (optional)
 */
public class InvalidLetBindingNameError implements Serializable, Comparable<InvalidLetBindingNameError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.InvalidLetBindingNameError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the binding within the term
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The invalid binding name
   */
  public final hydra.core.Name name;

  public InvalidLetBindingNameError (hydra.paths.SubtermPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InvalidLetBindingNameError)) {
      return false;
    }
    InvalidLetBindingNameError o = (InvalidLetBindingNameError) other;
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
  public int compareTo(InvalidLetBindingNameError other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      location,
      other.location);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      name,
      other.name);
  }

  public InvalidLetBindingNameError withLocation(hydra.paths.SubtermPath location) {
    return new InvalidLetBindingNameError(location, name);
  }

  public InvalidLetBindingNameError withName(hydra.core.Name name) {
    return new InvalidLetBindingNameError(location, name);
  }
}
