// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A type scheme variable name that violates type variable naming conventions (optional)
 */
public class InvalidTypeSchemeVariableNameError implements Serializable, Comparable<InvalidTypeSchemeVariableNameError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the type scheme
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The invalid variable name
   */
  public final hydra.core.Name name;

  public InvalidTypeSchemeVariableNameError (hydra.paths.SubtermPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InvalidTypeSchemeVariableNameError)) {
      return false;
    }
    InvalidTypeSchemeVariableNameError o = (InvalidTypeSchemeVariableNameError) other;
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
  public int compareTo(InvalidTypeSchemeVariableNameError other) {
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

  public InvalidTypeSchemeVariableNameError withLocation(hydra.paths.SubtermPath location) {
    return new InvalidTypeSchemeVariableNameError(location, name);
  }

  public InvalidTypeSchemeVariableNameError withName(hydra.core.Name name) {
    return new InvalidTypeSchemeVariableNameError(location, name);
  }
}
